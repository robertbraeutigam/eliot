package com.vanillasource.eliot.eliotc.compiler

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.{Logging, User}
import com.vanillasource.eliot.eliotc.plugin.Configuration.namedKey
import com.vanillasource.eliot.eliotc.plugin.{CompilerPlugin, Configuration}
import com.vanillasource.eliot.eliotc.visualization.FactVisualizationTracker
import scopt.{DefaultOEffectSetup, OParser, OParserBuilder}

import java.nio.file.Path
import java.util.ServiceLoader
import scala.jdk.CollectionConverters.*

object Compiler extends Logging {
  val targetPathKey: Configuration.Key[Path]     = namedKey[Path]("targetPath")
  val visualizeFactsKey: Configuration.Key[Path] = namedKey[Path]("visualizeFacts")

  /** Run the compiler, returning whether the compilation produced any errors. The CLI ([[Main]]) maps that to a
    * non-zero exit code so callers (scripts, CI, the IntelliJ before-run build task) can gate on success. Help/parse
    * termination is not an error here (`--help` must still exit 0); a missing target plugin is.
    */
  def runCompiler(args: List[String]): IO[Boolean] =
    for {
      plugins   <- allLayers()
      // Run command line parsing with all options from all layers
      configOpt <- parseCommandLine(args, plugins.map(_.commandLineParser()))
      hadErrors <- configOpt match {
                     case None                => IO.pure(false)
                     case Some(configuration) =>
                       runWithConfiguration(configuration, plugins, args)
                   }
    } yield hadErrors

  private def runWithConfiguration(
      configuration: Configuration,
      plugins: Seq[CompilerPlugin],
      args: List[String]
  ): IO[Boolean] =
    // Select active plugins
    plugins.find(_.isSelectedBy(configuration)) match {
      case None               =>
        User.compilerGlobalError("No target plugin selected.").as(true)
      case Some(targetPlugin) =>
        val activatedPlugins = collectActivatedPlugins(targetPlugin, configuration, plugins)
        for {
          _       <- debug[IO](s"Selected target plugin: ${targetPlugin.getClass.getSimpleName}")
          _       <-
            debug[IO](s"Selected active plugins: ${activatedPlugins.map(_.getClass.getSimpleName).mkString(", ")}")
          // One-time setup: configure plugins, collect processors, seed the cache from disk
          session <- CompilationSession.create(targetPlugin, activatedPlugins, configuration, args)
          tracker <- FactVisualizationTracker.create()
          // Run the (single, for the CLI) compilation and flush the resulting cache back to disk
          _       <- debug[IO]("Compiler starting...")
          result  <- session.compileOnce(Some(tracker))
          _       <- session.persist()
          _       <- debug[IO]("Compiler exiting normally.")
          // Print the compiler errors
          _       <- result.errors.traverse_(_.print())
          // Generate visualization if requested
          _       <- tracker.generateVisualization(
                       session.effectiveConfiguration
                         .get(visualizeFactsKey)
                         .getOrElse(
                           session.effectiveConfiguration.get(targetPathKey).get.resolve("fact-visualization.html")
                         )
                     )
        } yield result.errors.nonEmpty
    }

  private def collectActivatedPlugins(
      initialPlugin: CompilerPlugin,
      configuration: Configuration,
      all: Seq[CompilerPlugin]
  ): Seq[CompilerPlugin] =
    LazyList.unfold(Seq(initialPlugin))(ps =>
      ps.headOption.map(plugin => plugin -> (ps.tail ++ resolvePlugins(plugin.pluginDependencies(configuration), all)))
    )

  private def resolvePlugins(classes: Seq[Class[? <: CompilerPlugin]], all: Seq[CompilerPlugin]): Seq[CompilerPlugin] =
    all.filter(p => classes.contains(p.getClass))

  private def allLayers(): IO[Seq[CompilerPlugin]] = IO.blocking {
    ServiceLoader
      .load(classOf[CompilerPlugin])
      .iterator()
      .asScala
      .toSeq
  }

  private def baseOptions() = {
    val cmdLineBuilder: OParserBuilder[Configuration] = OParser.builder[Configuration]
    import cmdLineBuilder.*

    OParser.sequence(
      programName("eliotc"),
      help("help").text("prints this help text"),
      opt[Path]('o', "output-dir")
        .text("the directory any output should be written")
        .action((path, config) => config.set(targetPathKey, path)),
      opt[Path]("visualize-facts")
        .text("generate an HTML visualization of fact generation flow")
        .action((path, config) => config.set(visualizeFactsKey, path))
    )
  }

  private def parseCommandLine(
      args: Seq[String],
      options: Seq[OParser[?, Configuration]]
  ): IO[Option[Configuration]] = IO.blocking {
    val (result, effects) = OParser.runParser(
      OParser.sequence(baseOptions(), options*),
      args,
      Configuration().set(targetPathKey, Path.of("target"))
    )

    var terminateState: Option[Unit] = Some(())

    OParser.runEffects(
      effects,
      new DefaultOEffectSetup {
        override def terminate(exitState: Either[String, Unit]): Unit = {
          terminateState = None
        }
      }
    )

    terminateState.flatMap(_ => result)
  }

}
