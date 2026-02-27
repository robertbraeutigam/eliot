package com.vanillasource.eliot.eliotc.feedback

import cats.effect.IO
import cats.effect.std.Console
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.pos.{Position, PositionRange}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

class CompilerErrorTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  "compiler error" should "highlight only the referenced single character on a single line" in {
    val content = "def logic: Box[String] = Box(\"Hello\").filter(\"Expr\").as(x).map(_ -> \"Earth!\")"
    val error   = CompilerError(
      "Name not defined.",
      Seq.empty,
      "Test.els",
      content,
      PositionRange(Position(1, 57), Position(1, 58))
    )

    captureError(error).asserting(extractCarets(_) shouldBe "^")
  }

  it should "highlight a multi-character range on a single line" in {
    val content = "def a: T = undefined"
    val error   = CompilerError(
      "Name not defined.",
      Seq.empty,
      "Test.els",
      content,
      PositionRange(Position(1, 12), Position(1, 21))
    )

    captureError(error).asserting(extractCarets(_) shouldBe "^^^^^^^^^")
  }

  it should "highlight to end of line for multi-line errors" in {
    val content = "def a: T =\n  undefined"
    val error   = CompilerError(
      "Name not defined.",
      Seq.empty,
      "Test.els",
      content,
      PositionRange(Position(1, 8), Position(2, 12))
    )

    captureError(error).asserting(extractCarets(_) shouldBe "^^^...")
  }

  private def captureError(error: CompilerError): IO[String] = {
    val buffer = mutable.ArrayBuffer.empty[String]
    given console: Console[IO] = new Console[IO] {
      override def print[A](a: A)(using S: cats.Show[A]): IO[Unit]   = IO.unit
      override def println[A](a: A)(using S: cats.Show[A]): IO[Unit] = IO.unit
      override def error[A](a: A)(using S: cats.Show[A]): IO[Unit]   = IO.unit
      override def errorln[A](a: A)(using S: cats.Show[A]): IO[Unit] =
        IO.delay(buffer += S.show(a))
      override def readLineWithCharset(charset: java.nio.charset.Charset): IO[String] = IO.pure("")
    }
    error.print()(using console).map(_ => buffer.mkString("\n"))
  }

  private val ansiPattern = "\u001b\\[[0-9;]*m".r
  private val caretLine   = "\\|\\s*(\\^+\\.*)".r

  private def extractCarets(output: String): String = {
    val plain = ansiPattern.replaceAllIn(output, "")
    plain.linesIterator.toSeq
      .flatMap(line => caretLine.findFirstMatchIn(line).map(_.group(1)))
      .lastOption
      .getOrElse("")
  }
}
