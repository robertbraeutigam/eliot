package com.vanillasource.eliot.eliotc.compiler

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.visualization.TrackedCompilerProcessor.wrapProcessor
import com.vanillasource.eliot.eliotc.feedback.{Logging, User}
import com.vanillasource.eliot.eliotc.plugin.Configuration.namedKey
import com.vanillasource.eliot.eliotc.plugin.{CompilerPlugin, Configuration}
import com.vanillasource.eliot.eliotc.processor.common.NullProcessor
import com.vanillasource.eliot.eliotc.visualization.FactVisualizationTracker
import scopt.{DefaultOEffectSetup, OParser, OParserBuilder}

import java.nio.file.Path
import java.util.ServiceLoader
import scala.jdk.CollectionConverters.*

object Compiler extends Logging {
  val targetPathKey: Configuration.Key[Path]     = namedKey[Path]("targetPath")
  val visualizeFactsKey: Configuration.Key[Path] = namedKey[Path]("visualizeFacts")

  def runCompiler(args: List[String]): IO[Unit] =
    for {
      plugins   <- allLayers()
      // Run command line parsing with all options from all layers
      configOpt <- parseCommandLine(args, plugins.map(_.commandLineParser()))
      _         <- configOpt match {
                     case None                => IO.unit
                     case Some(configuration) =>
                       runWithConfiguration(configuration, plugins)
                   }
    } yield ()

  private def runWithConfiguration(configuration: Configuration, plugins: Seq[CompilerPlugin]): IO[Unit] =
    // Select active plugins
    plugins.find(_.isSelectedBy(configuration)) match {
      case None               =>
        User.compilerGlobalError("No target plugin selected.")
      case Some(targetPlugin) =>
        for {
          _                <- debug[IO](s"Selected target plugin: ${targetPlugin.getClass.getSimpleName}")
          activatedPlugins  = collectActivatedPlugins(targetPlugin, configuration, plugins)
          _                <-
            debug[IO](s"Selected active plugins: ${activatedPlugins.map(_.getClass.getSimpleName).mkString(", ")}")
          // Give plugins a chance to configure each other
          newConfiguration <- activatedPlugins.traverse_(_.configure()).runS(configuration)
          // Collect all processors
          processor        <- activatedPlugins.traverse_(_.initialize(newConfiguration)).runS(NullProcessor())
          // Create visualization tracker if requested
          tracker          <- FactVisualizationTracker.create()
          wrappedProcessors = processor.wrapWith(wrapProcessor(_, tracker))
          // Run fact generator / compiler
          _                <- debug[IO]("Compiler starting...")
          generator        <- FactGenerator.create(wrappedProcessors)
          _                <- targetPlugin.run(newConfiguration, generator)
          _                <- debug[IO]("Compiler exiting normally.")
          // Print the compiler errors
          errors           <- generator.currentErrors()
          _                <- errors.traverse_(_.print())
          // Generate visualization if requested
          _                <- tracker.generateVisualization(
                                newConfiguration
                                  .get(visualizeFactsKey)
                                  .getOrElse(
                                    newConfiguration.get(targetPathKey).get.resolve("fact-visualization.html")
                                  )
                              )
        } yield ()
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
