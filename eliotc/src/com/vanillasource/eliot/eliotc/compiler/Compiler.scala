package com.vanillasource.eliot.eliotc.compiler

import cats.effect.{ExitCode, IO}
import cats.effect.std.Console
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.compiler.cache.CacheFingerprint
import com.vanillasource.eliot.eliotc.feedback.{Logging, User}
import com.vanillasource.eliot.eliotc.plugin.Configuration.{diagnosticKey, namedKey}
import com.vanillasource.eliot.eliotc.plugin.{CompilerPlugin, Configuration}
import com.vanillasource.eliot.eliotc.progress.{
  ProgressDescriber,
  ProgressLineWriter,
  ProgressPhase,
  ProgressProfile,
  ProgressTracker
}
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
  val progressKey: Configuration.Key[Unit]       = diagnosticKey[Unit]("progress")

  /** Run the compiler, returning the process's exit code: an error when the compilation *failed* — it produced errors,
    * or its target produced no artefact — and otherwise whatever the selected target's
    * [[com.vanillasource.eliot.eliotc.plugin.CompilerPlugin.execute]] answers, which is success for everything that only
    * produces an artefact. The CLI ([[Main]]) exits with it so callers (scripts, CI, a build tool, the IntelliJ before-run
    * build task) can gate on it. Help/parse termination is not a failure here (`--help` must still exit 0); a missing
    * target plugin is.
    */
  def runCompiler(args: List[String]): IO[ExitCode] =
    for {
      plugins   <- allLayers()
      // Run command line parsing with all options from all layers
      configOpt <- parseCommandLine(withDefaultBackend(args, plugins), plugins.map(_.commandLineParser()))
      exitCode  <- configOpt match {
                     case None                => IO.pure(ExitCode.Success)
                     case Some(configuration) =>
                       runWithConfiguration(configuration, plugins)
                   }
    } yield exitCode

  /** The command line with the backend word filled in, when it was left out and only one backend could have been meant.
    *
    * A line that starts with a mode word (`run -m Main`, `exe-jar -m Main`) rather than a backend word names no
    * backend, and it is the one backend on the classpath accepting that mode which runs it. That is what lets a
    * platform-independent package — a test framework — say "compile with this main and run it" without naming a
    * platform, and leaves the platform to whichever backend the consumer's closure put on the classpath. Two backends
    * accepting the mode, or none, leave the line as it is, and the parser then reports it the way it reports any line
    * it does not understand; a line that does name its backend is never touched.
    */
  def withDefaultBackend(args: List[String], plugins: Seq[CompilerPlugin]): List[String] =
    args match {
      case first :: _ if !plugins.flatMap(_.backendWord).contains(first) =>
        plugins.filter(_.backendModes.contains(first)).flatMap(_.backendWord) match {
          case Seq(backend) => backend :: args
          case _            => args
        }
      case _                                                              => args
    }

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
      plugins: Seq[CompilerPlugin],
      progress: Option[ProgressTracker] = None
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
          session <- CompilationSession.create(targetPlugin, activatedPlugins, configuration, progress)
        } yield Some(session)
    }

  private def runWithConfiguration(
      configuration: Configuration,
      plugins: Seq[CompilerPlugin]
  ): IO[ExitCode] =
    for {
      // Start the clock before session setup so the total spans the whole lifecycle: the cache load and the
      // fingerprint digest happen inside `sessionFor` (before any compile), and `--statistics` accounts for them.
      started  <- IO.monotonic
      // The profile is keyed by the command line's configuration, which is known before any plugin configures a session
      profile   = Option.when(configuration.contains(progressKey))(
                    ProgressProfile.fileIn(configuration.get(targetPathKey).get, CacheFingerprint.config(configuration))
                  )
      previous <- profile.traverse(ProgressProfile.read)
      describer = ProgressDescriber.combined(plugins.flatMap(_.progressDescriber))
      progress <- previous.traverse(ProgressTracker.create(_, describer))
      writer   <- progress.traverse(ProgressLineWriter.create)
      target    = plugins.find(_.isSelectedBy(configuration)).toSeq.flatMap(_.progressTarget(configuration))
      // Progress lines are printed from session setup until the cache is persisted, and stop before the diagnostics
      compiled <- writer
                    .traverse_(_.lines(target))
                    .surround(sessionFor(configuration, plugins, progress).flatMap(_.traverse(compile(_, progress))))
      finished <- IO.monotonic
      exitCode <- compiled match {
                    case None           => IO.pure(ExitCode.Error) // no target plugin — reported by `sessionFor`
                    case Some(compiled) =>
                      import compiled.*
                      val visualizationPath = session.effectiveConfiguration.get(visualizeFactsKey)

                      for {
                        _        <- debug[IO]("Compiler exiting normally.")
                        // Print the compiler errors
                        _        <- result.errors.traverse_(_.print())
                        // Generate visualization if requested
                        _        <- (tracker, visualizationPath).tupled.traverse_(_.generateVisualization(_))
                        // Print where the time went in this run, if requested — including the coarse cache phases
                        phases   <- session.phaseSnapshot
                        _        <- statistics.traverse_(_.report(finished - started, phases).flatMap(Console[IO].println))
                        // The closing progress line comes last, so a `run` mode's program output follows a finished log
                        _        <- writer.traverse_(_.close(target, result.errors, result.targetProduced))
                        // Only a run that succeeded teaches the next one: a failed one stops short of its total
                        _        <- (profile, previous, progress).tupled.traverse_(saveProfile).whenA(result.succeeded)
                        _        <- progress.traverse_(_.enter(ProgressPhase.Running))
                        // Only a compilation that produced what it was asked for gets to do anything with it
                        exitCode <- if (result.succeeded) session.execute().map(ExitCode(_))
                                    else IO.pure(ExitCode.Error)
                      } yield exitCode
                  }
    } yield exitCode

  /** Record in the profile what this run delivered, as the total the next run is measured against, and where its time
    * went, in the history of its class.
    */
  private def saveProfile(file: Path, previous: ProgressProfile, progress: ProgressTracker): IO[Unit] =
    progress.snapshot.flatMap(snapshot =>
      snapshot.runClass.traverse_(runClass =>
        ProgressProfile.write(file, previous.including(snapshot.delivered, runClass, snapshot.history))
      )
    )

  /** A session's single compilation, with the instrumentation it ran under. */
  private case class Compiled(
      session: CompilationSession,
      result: CompilationResult,
      tracker: Option[FactVisualizationTracker],
      statistics: Option[ProcessorStatistics]
  )

  /** Run the (single, for the CLI) compilation and flush the resulting cache back to disk. */
  private def compile(session: CompilationSession, progress: Option[ProgressTracker]): IO[Compiled] = {
    val visualizationPath = session.effectiveConfiguration.get(visualizeFactsKey)
    val statisticsAsked   = session.effectiveConfiguration.contains(statisticsKey)

    for {
      // Both observe every processor invocation and every fact read, so neither is created unless
      // asked for: an ordinary build should not pay for a diagnostic it discards.
      tracker    <- visualizationPath.traverse(_ => FactVisualizationTracker.create())
      statistics <- Option.when(statisticsAsked)(ProcessorStatistics.create()).sequence
      _          <- debug[IO]("Compiler starting...")
      result     <- session.compileOnce(tracker, statistics, progress)
      _          <- session.persist()
    } yield Compiled(session, result, tracker, statistics)
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
        .action((path, config) => config.set(visualizeFactsKey, path)),
      opt[Unit]("statistics")
        .text("print how much time each processor took after the run")
        .action((_, config) => config.set(statisticsKey, ())),
      opt[Unit]("progress")
        .text("print how far along the run is while it works, to stderr")
        .action((_, config) => config.set(progressKey, ()))
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
