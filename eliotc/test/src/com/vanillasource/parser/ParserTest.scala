package com.vanillasource.parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.vanillasource.parser.Parser.*
import com.vanillasource.parser.ParserResult._
import com.vanillasource.parser.Stream.*
import cats.syntax.all._

class ParserTest extends AnyFlatSpec with Matchers {
  "end-of-input parser" should "match end-of-input" in {
    val p = endOfInput[Char]()

    p.runA(Stream.ofString("")) shouldBe SuccessWithConsuming(())
  }

  it should "return error if stream did not reach end-of-input" in {
    val p = endOfInput[Char]()

    p.runA(Stream.ofString("abc")) shouldBe FailedWithConsuming("end of input")
  }

  "literal" should "match the literal in stream" in {
    val p = literal('a')

    p.runA(Stream.ofString("abc")) shouldBe SuccessWithConsuming('a')
  }

  it should "fail on different literal" in {
    val p = literal('a')

    p.runA(Stream.ofString("bc")) shouldBe FailedWithoutConsuming("literal 'a'")
  }

  "full parsing" should "succeeds if stream ends" in {
    val p = fully(literal('a'))

    p.runA(Stream.ofString("a")) shouldBe SuccessWithConsuming('a')
  }

  it should "fail if stream does not end" in {
    val p = fully(literal('a'))

    p.runA(Stream.ofString("ab")) shouldBe FailedWithConsuming("end of input")
  }

  "optional parser" should "parse if successful" in {
    val p = option(literal('a'))

    p.runA(Stream.ofString("a")) shouldBe SuccessWithConsuming(Some('a'))
  }

  it should "return none successfully if parser fails" in {
    val p = option(literal('a'))

    p.runA(Stream.ofString("b")) shouldBe SuccessWithConsuming(None)
  }

  "sequence of parsers" should "return skip if first parser fails" in {
    val p = literal('a') >> literal('b')

    p.runA(Stream.ofString("ab")) shouldBe SuccessWithConsuming('b')
  }

  it should "indicate nothing consumed if first parser fails" in {
    val p = literal('a') >> literal('b')

    p.runA(Stream.ofString("bb")) shouldBe FailedWithoutConsuming("literal 'a'")
  }

  it should "indicate something consumed if second parser fails" in {
    val p = literal('a') >> literal('b')

    p.runA(Stream.ofString("ac")) shouldBe FailedWithConsuming("literal 'b'")
  }

}
