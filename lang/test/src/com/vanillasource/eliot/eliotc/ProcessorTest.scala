package com.vanillasource.eliot.eliotc

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.resolve.fact.{
  QualifiedName as ResolveQualifiedName,
  Qualifier as ResolveQualifier
}
import com.vanillasource.eliot.eliotc.symbolic.fact.{
  QualifiedName as SymbolicQualifiedName,
  Qualifier as SymbolicQualifier
}
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

  def default(name: String): QualifiedName = QualifiedName(name, Qualifier.Default)

  def toResolve(qn: QualifiedName): ResolveQualifiedName =
    ResolveQualifiedName(
      qn.name,
      qn.qualifier match {
        case Qualifier.Default    => ResolveQualifier.Default
        case Qualifier.Type       => ResolveQualifier.Type
        case Qualifier.Ability(n) => ResolveQualifier.Ability(n)
        case _                    => throw IllegalArgumentException("Cannot convert AbilityImplementation in test helper")
      }
    )

  def toSymbolic(qn: QualifiedName): SymbolicQualifiedName =
    SymbolicQualifiedName(
      qn.name,
      qn.qualifier match {
        case Qualifier.Default    => SymbolicQualifier.Default
        case Qualifier.Type       => SymbolicQualifier.Type
        case Qualifier.Ability(n) => SymbolicQualifier.Ability(n)
        case _                    => throw IllegalArgumentException("Cannot convert AbilityImplementation in test helper")
      }
    )

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

  case class TestError(message: String, highlight: String)

  extension (message: String) infix def at(highlight: String): TestError = TestError(message, highlight)

  def toTestErrors(errors: Seq[CompilerError]): Seq[TestError] =
    errors.map { error =>
      val lines     = error.content.linesIterator.toSeq
      val from      = error.sourceRange.from
      val to        = error.sourceRange.to
      val highlight =
        if (from.line == to.line) lines(from.line - 1).substring(from.col - 1, to.col - 1)
        else {
          val firstLine   = lines(from.line - 1).substring(from.col - 1)
          val lastLine    = lines(to.line - 1).substring(0, to.col - 1)
          val middleLines = lines.slice(from.line, to.line - 1)
          (firstLine +: middleLines :+ lastLine).mkString("\n")
        }
      TestError(error.message, highlight)
    }
}
