package com.vanillasource.eliot.eliotc

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.main.CompilerEngine
import com.vanillasource.eliot.eliotc.source.{SourceContent, Sourced, SourcedError}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

abstract class ProcessorTest(val processors: CompilerProcessor*) extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  val file                  = new File("Test.els")
  private val systemImports = Seq(SystemImport("Function", "data Function[A, B]"))

  def runEngineForErrors(source: String, imports: Seq[SystemImport] = Seq.empty): IO[Seq[String]] =
    runEngine(source, imports)
      .map(_.values.collect { case SourcedError(Sourced(_, _, msg)) => msg }.toSeq)

  def runEngine(source: String, imports: Seq[SystemImport] = Seq.empty): IO[Map[Any, CompilerFact]] = {
    CompilerEngine(processors)
      .resolve(
        imports.map(i => SourceContent(new File(s"eliot/lang/${i.module}.els"), new File("."), i.content)) ++
          Seq(SourceContent(file, Option(file.getParentFile).getOrElse(File(".")), source))
      )
  }

  def runEngineForErrorsWithImports(source: String): IO[Seq[String]] = runEngineForErrors(source, systemImports)

  case class SystemImport(module: String, content: String)
}
