package com.vanillasource.eliot.eliotc

import cats.Show
import cats.effect.IO
import cats.effect.std.Console
import cats.syntax.all.*
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.compiler.FactGenerator
import com.vanillasource.eliot.eliotc.module.fact.ModuleName
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.impl.SequentialCompilerProcessors
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey, CompilerProcessor}
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}
import com.vanillasource.eliot.eliotc.source.error.{ErrorReporter, SourcedError}
import com.vanillasource.eliot.eliotc.source.scan.PathScan
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File
import java.nio.charset.Charset
import java.nio.file.Path

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
      .map(_.values.collect { case SourcedError(Sourced(_, _, msg), _) => msg }.toSeq)

  def runGenerator(
      source: String,
      trigger: CompilerFactKey[? <: CompilerFact],
      imports: Seq[SystemImport] = Seq.empty
  ): IO[Map[CompilerFactKey[?], CompilerFact]] =
    for {
      generator <- FactGenerator.create(
                     SequentialCompilerProcessors(
                       Seq(new ErrorReporter()(using NullConsole()), SequentialCompilerProcessors(processors))
                     )
                   )
      _         <- generator.registerFact(SourceContent(file, Sourced(file, PositionRange.zero, source)))
      _         <- generator.registerFact(PathScan(Path.of("Test.els"), Seq(file)))
      _         <- imports.traverse { imp =>
                     val impFile = new File(s"eliot/lang/${imp.module}.els")
                     generator.registerFact(PathScan(Path.of(s"eliot/lang/${imp.module}.els"), Seq(impFile))) >>
                       generator.registerFact(SourceContent(impFile, Sourced(impFile, PositionRange.zero, imp.content)))
                   }
      _         <- generator.getFact(trigger)
      facts     <- generator.currentFacts()
    } yield facts

  def runGeneratorForErrorsWithImports(source: String, trigger: CompilerFactKey[? <: CompilerFact]): IO[Seq[String]] =
    runGeneratorForErrors(source, trigger, systemImports)

  case class SystemImport(module: String, content: String)

  case class NullConsole() extends Console[IO] {
    override def readLineWithCharset(charset: Charset): IO[String] = IO("")

    override def print[A](a: A)(implicit S: Show[A]): IO[Unit] = IO.unit

    override def println[A](a: A)(implicit S: Show[A]): IO[Unit] = IO.unit

    override def error[A](a: A)(implicit S: Show[A]): IO[Unit] = IO.unit

    override def errorln[A](a: A)(implicit S: Show[A]): IO[Unit] = IO.unit
  }
}
