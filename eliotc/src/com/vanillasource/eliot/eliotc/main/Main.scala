package com.vanillasource.eliot.eliotc.main

import cats.effect.{ExitCode, IO, IOApp}
import com.vanillasource.eliot.eliotc.main.Compiler.runCompiler
import com.vanillasource.eliot.eliotc.feedback.Logging

object Main extends IOApp with Logging {
  override def run(args: List[String]): IO[ExitCode] = runCompiler(args).value.as(ExitCode.Success)
}
