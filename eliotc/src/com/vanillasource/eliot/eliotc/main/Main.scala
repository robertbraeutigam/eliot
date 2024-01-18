package com.vanillasource.eliot.eliotc.main

import cats.effect.{ExitCode, IO, IOApp}
import scopt.{DefaultOEffectSetup, OParser, OParserBuilder}

import java.io.File

object Main extends IOApp {
  private val cmdLineBuilder: OParserBuilder[CommandLineArguments] = OParser.builder[CommandLineArguments]
  import cmdLineBuilder._
  private val cmdLineParser                                        = OParser.sequence(
    programName("eliotc"),
    help("help").text("prints this help text"),
    arg[File]("<path>...")
      .unbounded()
      .required()
      .action((f, args) => args.copy(paths = args.paths :+ f))
      .text("paths of either directories or files to compile")
  )

  override def run(args: List[String]): IO[ExitCode] = for {
    parserResults <- IO(OParser.runParser(cmdLineParser, args, CommandLineArguments()))
    _             <- IO.blocking(
                       OParser.runEffects(
                         parserResults._2,
                         new DefaultOEffectSetup {
                           override def terminate(exitState: Either[String, Unit]): Unit = ()
                         }
                       )
                     )
    _             <- parserResults._1 match
                       case Some(cmdLineArguments) => IO.unit
                       case None                   => IO.unit
  } yield ExitCode.Success
}
