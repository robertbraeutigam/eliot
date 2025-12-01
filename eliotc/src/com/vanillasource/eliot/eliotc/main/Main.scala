package com.vanillasource.eliot.eliotc.main

import cats.effect.{ExitCode, IO, IOApp, Ref}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.{CompilerProcessor, Init}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.layer.{CompilerSystem, Configuration, Layer}
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName}
import com.vanillasource.eliot.eliotc.processor.SequentialCompilerProcessors
import scopt.{DefaultOEffectSetup, OParser, OParserBuilder}

import java.io.File
import java.util.ServiceLoader
import scala.jdk.CollectionConverters.*

object Main extends IOApp with Logging {
  override def run(args: List[String]): IO[ExitCode] = {
    for {
      layers             <- allLayers()
      configurationMaybe <- parserCommandLine(args, layers.map(_.commandLineParser()))
      _                  <- configurationMaybe match
                              case Some(configuration) =>
                                for {
                                  // Assemble all processors
                                  processorsRef <- Ref[IO].of(Seq.empty[CompilerProcessor])
                                  _             <- layers.traverse_(
                                                     _.initialize(
                                                       configuration,
                                                       new CompilerSystem() {
                                                         override def registerProcessor(compilerProcessor: CompilerProcessor): IO[Unit] =
                                                           processorsRef.update(_.appended(compilerProcessor))
                                                       }
                                                     )
                                                   )
                                  _             <- info("Compiler starting...")
                                  processors    <- processorsRef.get
                                  generator     <- FactGenerator(SequentialCompilerProcessors(processors))
                                  _             <- generator.getFact(Init.Key())
                                } yield ()
                              case None                => IO.unit
    } yield ExitCode.Success
  }

  private def allLayers(): IO[Seq[Layer]] = IO.blocking {
    ServiceLoader
      .load(classOf[Layer])
      .iterator()
      .asScala
      .toSeq
  }

  private def baseOptions() = {
    val cmdLineBuilder: OParserBuilder[Configuration] = OParser.builder[Configuration]
    import cmdLineBuilder._

    OParser.sequence(
      programName("eliotc"),
      help("help").text("prints this help text")
    )
  }

  private def parserCommandLine(
      args: Seq[String],
      options: Seq[OParser[_, Configuration]]
  ): IO[Option[Configuration]] = IO {
    val (result, effects) = OParser.runParser(OParser.sequence(baseOptions(), options: _*), args, Configuration())

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
