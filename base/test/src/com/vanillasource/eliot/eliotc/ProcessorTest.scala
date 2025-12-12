package com.vanillasource.eliot.eliotc

import cats.effect.IO
import cats.syntax.all.*
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.main.FactGenerator
import com.vanillasource.eliot.eliotc.module.fact.ModuleName
import com.vanillasource.eliot.eliotc.processor.SequentialCompilerProcessors
import com.vanillasource.eliot.eliotc.source.content.SourceContent
import com.vanillasource.eliot.eliotc.source.pos.{PositionRange, Sourced}
import com.vanillasource.eliot.eliotc.source.error.SourcedError
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

abstract class ProcessorTest(val processors: CompilerProcessor*) extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  val file                  = new File("Test.els")
  val testModuleName        = ModuleName(Seq.empty, "Test")
  private val systemImports = Seq(SystemImport("Function", "data Function[A, B]"))

  def runGeneratorForErrors(
      source: String,
      trigger: CompilerFactKey[? <: CompilerFact],
      imports: Seq[SystemImport] = Seq.empty
  ): IO[Seq[String]] =
    runGenerator(source, trigger, imports)
      .map(_.values.collect { case SourcedError(Sourced(_, _, msg)) => msg }.toSeq)

  def runGenerator(
      source: String,
      trigger: CompilerFactKey[? <: CompilerFact],
      imports: Seq[SystemImport] = Seq.empty
  ): IO[Map[CompilerFactKey[?], CompilerFact]] =
    for {
      generator <- FactGenerator.create(SequentialCompilerProcessors(processors))
      _         <- generator.registerFact(SourceContent(file, Sourced(file, PositionRange.zero, source)))
      _         <- imports.traverse { imp =>
                     val file = new File(s"eliot/lang/${imp.module}.els")
                     generator.registerFact(SourceContent(file, Sourced(file, PositionRange.zero, imp.content)))
                   }
      _         <- generator.getFact(trigger)
      facts     <- generator.currentFacts()
    } yield facts

  def runGeneratorForErrorsWithImports(source: String, trigger: CompilerFactKey[? <: CompilerFact]): IO[Seq[String]] =
    runGeneratorForErrors(source, trigger, systemImports)

  case class SystemImport(module: String, content: String)
}
