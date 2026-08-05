package com.vanillasource.eliot.eliotc.compiler

import cats.effect.IO
import cats.effect.std.Console
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.{Logging, User}
import com.vanillasource.eliot.eliotc.plugin.Configuration.{diagnosticKey, namedKey}
import com.vanillasource.eliot.eliotc.plugin.{CompilerPlugin, Configuration}
import com.vanillasource.eliot.eliotc.statistics.ProcessorStatistics
import com.vanillasource.eliot.eliotc.visualization.FactVisualizationTracker
import scopt.{DefaultOEffectSetup, OParser, OParserBuilder}

import java.nio.file.Path
import java.util.ServiceLoader
import scala.jdk.CollectionConverters.*

object Compiler extends Logging {
  val targetPathKey: Configuration.Key[Path]     = namedKey[Path]("targetPath")
  // Diagnostic-only: these turn on observation (a fact-flow graph, per-processor timing) without changing any fact, so
  // they must not enter the cache identity — otherwise a `--statistics` run could never observe a warm build.
  val visualizeFactsKey: Configuration.Key[Path] = diagnosticKey[Path]("visualizeFacts")
  val statisticsKey: Configuration.Key[Unit]     = diagnosticKey[Unit]("statistics")

  /** Run the compiler, returning whether the compilation *failed* — it produced errors, or its target produced no
    * artefact. The CLI ([[Main]]) maps that to a non-zero exit code so callers (scripts, CI, the IntelliJ before-run
    * build task) can gate on success. Help/parse termination is not a failure here (`--help` must still exit 0); a
    * missing target plugin is.
    */
  def runCompiler(args: List[String]): IO[Boolean] =
    for {
      plugins   <- allLayers()
      // Run command line parsing with all options from all layers
      configOpt <- parseCommandLine(args, plugins.map(_.commandLineParser()))
      failed    <- configOpt match {
                     case None                => IO.pure(false)
                     case Some(configuration) =>
                       runWithConfiguration(configuration, plugins)
                   }
    } yield failed

  /** Build a resident [[CompilationSession]] for `args` without running it: discover plugins, parse the command line,
    * select the target plugin, and do the one-time session setup (configuring the processor graph, seeding the cache
    * from disk). Returns `None` when the arguments do not select a runnable compilation — help/parse termination, or no
    * target plugin (the latter already reported here as a global error).
    *
    * This is the seam a long-running host drives directly: a server (or the integration-test harness) calls
    * [[CompilationSession.compileOnce]] on the returned session repeatedly, reusing the warm in-memory fact cache
    * instead of paying a cold start — plugin discovery plus a full base-layer recompile — on every compilation. The CLI
    * keeps using [[runCompiler]], which drives one compile then persists the cache and prints diagnostics.
    */
  def createSession(args: List[String]): IO[Option[CompilationSession]] =
    for {
      plugins   <- allLayers()
      configOpt <- parseCommandLine(args, plugins.map(_.commandLineParser()))
      session   <- configOpt.flatTraverse(sessionFor(_, plugins))
    } yield session

  private def sessionFor(
      configuration: Configuration,
      plugins: Seq[CompilerPlugin]
  ): IO[Option[CompilationSession]] =
    // Select active plugins
    plugins.find(_.isSelectedBy(configuration)) match {
      case None               =>
        User.compilerGlobalError("No target plugin selected.").as(None)
      case Some(targetPlugin) =>
        val activatedPlugins = collectActivatedPlugins(targetPlugin, configuration, plugins)
        for {
          _       <- debug[IO](s"Selected target plugin: ${targetPlugin.getClass.getSimpleName}")
          _       <-
            debug[IO](s"Selected active plugins: ${activatedPlugins.map(_.getClass.getSimpleName).mkString(", ")}")
          // One-time setup: configure plugins, collect processors, seed the cache from disk
          session <- CompilationSession.create(targetPlugin, activatedPlugins, configuration)
        } yield Some(session)
    }

  private def runWithConfiguration(
      configuration: Configuration,
      plugins: Seq[CompilerPlugin]
  ): IO[Boolean] =
    for {
      // Start the clock before session setup so the total spans the whole lifecycle: the cache load and the
      // fingerprint digest happen inside `sessionFor` (before any compile), and `--statistics` accounts for them.
      started    <- IO.monotonic
      sessionOpt <- sessionFor(configuration, plugins)
      failed     <- sessionOpt match {
                      case None          => IO.pure(true) // no target plugin — reported by `sessionFor`, an error exit
                      case Some(session) =>
                        val visualizationPath = session.effectiveConfiguration.get(visualizeFactsKey)
                        val statisticsAsked   = session.effectiveConfiguration.contains(statisticsKey)

                        for {
                          // Both observe every processor invocation and every fact read, so neither is created unless
                          // asked for: an ordinary build should not pay for a diagnostic it discards.
                          tracker    <- visualizationPath.traverse(_ => FactVisualizationTracker.create())
                          statistics <- Option.when(statisticsAsked)(ProcessorStatistics.create()).sequence
                          // Run the (single, for the CLI) compilation and flush the resulting cache back to disk
                          _          <- debug[IO]("Compiler starting...")
                          result     <- session.compileOnce(tracker, statistics)
                          _          <- session.persist()
                          finished   <- IO.monotonic
                          _          <- debug[IO]("Compiler exiting normally.")
                          // Print the compiler errors
                          _          <- result.errors.traverse_(_.print())
                          // Generate visualization if requested
                          _          <- (tracker, visualizationPath).tupled.traverse_(_.generateVisualization(_))
                          // Print where the time went in this run, if requested — including the coarse cache phases
                          phases     <- session.phaseSnapshot
                          _          <- statistics.traverse_(_.report(finished - started, phases).flatMap(Console[IO].println))
                        } yield !result.succeeded
                    }
    } yield failed

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
        .action((path, config) => config.set(visualizeFactsKey, path)),
      opt[Unit]("statistics")
        .text("print how much time each processor took after the run")
        .action((_, config) => config.set(statisticsKey, ()))
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
