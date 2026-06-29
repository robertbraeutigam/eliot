package com.vanillasource.eliot.eliotc.token

import com.vanillasource.eliot.eliotc.pos.{Position, PositionRange}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URI

class DocCommentScannerTest extends AnyFlatSpec with Matchers {
  private val file = URI.create("Test.els")

  private def scan(content: String): Seq[String] = DocCommentScanner.scan(file, content).map(_.value)

  "doc comment scanner" should "return nothing for empty content" in {
    scan("") shouldBe Seq.empty
  }

  it should "return nothing for a line comment" in {
    scan("// a doc-looking /** comment */ in a line comment") shouldBe Seq.empty
  }

  it should "return nothing for a plain block comment" in {
    scan("/* not a doc comment */") shouldBe Seq.empty
  }

  it should "return nothing for the empty block comment" in {
    scan("/**/") shouldBe Seq.empty
  }

  it should "capture the inner text of a doc comment" in {
    scan("/** Adds two numbers. */") shouldBe Seq(" Adds two numbers. ")
  }

  it should "capture an empty doc comment" in {
    scan("/***/") shouldBe Seq("")
  }

  it should "not treat a doc comment inside a string literal as documentation" in {
    scan("def s: String = \"/** not a doc */\"") shouldBe Seq.empty
  }

  it should "respect escaped quotes when skipping a string literal" in {
    scan("def s: String = \"a \\\" /** not a doc */\"") shouldBe Seq.empty
  }

  it should "capture multiple doc comments in source order" in {
    scan("/** first */ def a /** second */ def b") shouldBe Seq(" first ", " second ")
  }

  it should "capture a multi-line doc comment with its margins" in {
    scan("/** a\n * b\n */") shouldBe Seq(" a\n * b\n ")
  }

  it should "focus a doc comment on its full range" in {
    DocCommentScanner.scan(file, "/** x */") shouldBe Seq(
      Sourced(file, PositionRange(Position(1, 1), Position(1, 9)), " x ")
    )
  }

  it should "track positions across lines for a doc comment after a newline" in {
    DocCommentScanner.scan(file, "ab\n/** y */").map(_.range) shouldBe Seq(
      PositionRange(Position(2, 1), Position(2, 9))
    )
  }
}
