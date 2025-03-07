package com.vanillasource.eliot.eliotc

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.main.CompilerEngine
import com.vanillasource.eliot.eliotc.source.{SourceContent, Sourced, SourcedError}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

abstract class ProcessorTest(val processors: CompilerProcessor*) extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  val file = new File("Test.els")

  def runEngineForErrors(source: String): IO[Seq[String]] =
    runEngine(source)
      .map(_.values.collect { case SourcedError(Sourced(_, _, msg)) => msg }.toSeq)

  def runEngine(source: String): IO[Map[Any, CompilerFact]] = {
    CompilerEngine(processors)
      .resolve(Seq(SourceContent(file, Option(file.getParentFile).getOrElse(File(".")), source)))
  }
}
