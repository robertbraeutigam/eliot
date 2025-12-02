package com.vanillasource.eliot.eliotc.main

import cats.data.OptionT
import cats.effect.{ExitCode, IO, IOApp}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.Init
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
      layers        <- allLayers().liftOptionT
      // Run command line parsing with all options from all layers
      configuration <- parserCommandLine(args, layers.map(_.commandLineParser()))
      // Select active plugins
      targetPlugin  <- layers
                         .find(_.isSelectedBy(configuration))
                         .pure[IO]
                         .onNone(User.compilerGlobalError("No target plugin selected."))
      // Collect all processors
      processor     <- layers.traverse_(_.initialize(configuration)).runS(NullProcessor()).liftOptionT
      // Run fact generator / compiler
      _             <- debug("Compiler starting...").liftOptionT
      generator     <- FactGenerator(processor).liftOptionT
      _             <- generator.getFact(Init.Key()).liftOptionT
      _             <- debug("Compiler exiting normally.").liftOptionT
    } yield ()

    program.value.as(ExitCode.Success)
  }

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
