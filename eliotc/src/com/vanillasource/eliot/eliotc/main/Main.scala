package com.vanillasource.eliot.eliotc.main

import cats.data.OptionT
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.{Logging, User}
import com.vanillasource.eliot.eliotc.plugin.Configuration.{namedKey, stringKey}
import com.vanillasource.eliot.eliotc.plugin.{CompilerPlugin, Configuration}
import com.vanillasource.eliot.eliotc.processor.NullProcessor
import com.vanillasource.eliot.eliotc.util.CatsOps.*
import scopt.{DefaultOEffectSetup, OParser, OParserBuilder}

import java.nio.file.Path
import java.util.ServiceLoader
import scala.jdk.CollectionConverters.*

object Main extends IOApp with Logging {
  val targetPathKey: Configuration.Key[Path] = namedKey[Path]("targetPath")

  override def run(args: List[String]): IO[ExitCode] = {
    val program = for {
      plugins         <- allLayers().liftOptionT
      // Run command line parsing with all options from all layers
      configuration   <- parserCommandLine(args, plugins.map(_.commandLineParser()))
      // Select active plugins
      targetPlugin    <- plugins
                           .find(_.isSelectedBy(configuration))
                           .pure[IO]
                           .onNone(User.compilerGlobalError("No target plugin selected."))
      _               <- debug(s"Selected target plugin: ${targetPlugin.getClass.getSimpleName}").liftOptionT
      activatedPlugins = collectActivatedPlugins(targetPlugin, configuration, plugins)
      _               <- debug(
                           s"Selected active plugins: ${activatedPlugins.map(_.getClass.getSimpleName).mkString(", ")}"
                         ).liftOptionT
      // Collect all processors
      processor       <- activatedPlugins.traverse_(_.initialize(configuration)).runS(NullProcessor()).liftOptionT
      // Run fact generator / compiler
      _               <- debug("Compiler starting...").liftOptionT
      generator       <- FactGenerator(processor).liftOptionT
      _               <- targetPlugin.run(configuration, generator).liftOptionT
      _               <- debug("Compiler exiting normally.").liftOptionT
    } yield ()

    program.value.as(ExitCode.Success)
  }

  private def collectActivatedPlugins(
      initialPlugin: CompilerPlugin,
      configuration: Configuration,
      all: Seq[CompilerPlugin]
  ): Seq[CompilerPlugin] =
    LazyList.unfold(Seq(initialPlugin))(ps =>
      ps.headOption.map(plugin => plugin -> (ps.tail ++ resolvePlugins(plugin.pluginDependencies(configuration), all)))
    )

  private def resolvePlugins(classes: Seq[Class[_ <: CompilerPlugin]], all: Seq[CompilerPlugin]): Seq[CompilerPlugin] =
    all.filter(p => classes.contains(p.getClass))

  private def allLayers(): IO[Seq[CompilerPlugin]] = IO {
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
        .action((path, config) => config.set(targetPathKey, path))
    )
  }

  private def parserCommandLine(
      args: Seq[String],
      options: Seq[OParser[_, Configuration]]
  ): OptionT[IO, Configuration] = IO {
    val (result, effects) = OParser.runParser(
      OParser.sequence(baseOptions(), options: _*),
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
  }.toOptionT
}
