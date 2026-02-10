package com.vanillasource.eliot.eliotc

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.compiler.FactGenerator
import com.vanillasource.eliot.eliotc.feedback.CompilerError
import com.vanillasource.eliot.eliotc.module.fact.ModuleName
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.common.SequentialCompilerProcessors
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey, CompilerProcessor}
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}
import com.vanillasource.eliot.eliotc.source.scan.PathScan
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URI
import java.nio.file.Path

abstract class ProcessorTest(val processors: CompilerProcessor*) extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  val file           = URI.create("Test.els")
  val testModuleName = ModuleName(Seq.empty, "Test")
  val sourceContent  = SourceContent(file, Sourced(file, PositionRange.zero, "test source"))
  val systemImports  = Seq(SystemImport("Function", "data Function[A, B]"))

  def sourced[T](value: T): Sourced[T] = Sourced(file, PositionRange.zero, value)

  def createGenerator(facts: Seq[CompilerFact]): IO[FactGenerator] =
    for {
      generator <- FactGenerator.create(SequentialCompilerProcessors(processors))
      _         <- generator.registerFact(sourceContent)
      _         <- facts.traverse_(generator.registerFact)
    } yield generator

  def runGeneratorWithFacts[K <: CompilerFact](
      facts: Seq[CompilerFact],
      trigger: CompilerFactKey[K]
  ): IO[(Option[K], Seq[CompilerError])] =
    for {
      generator <- createGenerator(facts)
      result    <- generator.getFact(trigger)
      errors    <- generator.currentErrors()
    } yield (result, errors)

  def runGenerator(
      source: String,
      trigger: CompilerFactKey[? <: CompilerFact],
      imports: Seq[SystemImport] = Seq.empty
  ): IO[(Seq[CompilerError], Map[CompilerFactKey[?], CompilerFact])] =
    for {
      generator <- FactGenerator.create(SequentialCompilerProcessors(processors))
      _         <- generator.registerFact(SourceContent(file, Sourced(file, PositionRange.zero, source)))
      _         <- generator.registerFact(PathScan(Path.of("Test.els"), Seq(file)))
      _         <- imports.traverse { imp =>
                     val impFile = URI.create(s"eliot/lang/${imp.module}.els")
                     generator.registerFact(PathScan(Path.of(s"eliot/lang/${imp.module}.els"), Seq(impFile))) >>
                       generator.registerFact(SourceContent(impFile, Sourced(impFile, PositionRange.zero, imp.content)))
                   }
      _         <- generator.getFact(trigger)
      facts     <- generator.currentFacts()
      errors    <- generator.currentErrors()
    } yield (errors, facts)

  case class SystemImport(module: String, content: String)
}
