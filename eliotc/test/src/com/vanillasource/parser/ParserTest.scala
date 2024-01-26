package com.vanillasource.parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.vanillasource.parser.Parser.*
import com.vanillasource.parser.ParserResult._
import com.vanillasource.parser.Stream.*

class ParserTest extends AnyFlatSpec with Matchers {
  "end-of-input parser" should "match end-of-input" in {
    val p = endOfInput[Char]()

    p.runA(Stream.ofString("")) shouldBe Success(())
  }

  it should "return error if stream did not reach end-of-input" in {
    val p = endOfInput[Char]()

    p.runA(Stream.ofString("abc")) shouldBe Error("end of input")
  }

  "literal" should "match the literal in stream" in {
    val p = literal('a')

    p.runA(Stream.ofString("abc")) shouldBe Success('a')
  }

  it should "fail on different literal" in {
    val p = literal('a')

    p.runA(Stream.ofString("bc")) shouldBe Skip("a")
  }

  "full parsing" should "succeeds if stream ends" in {
    val p = fully(literal('a'))

    p.runA(Stream.ofString("a")) shouldBe Success('a')
  }

  it should "fail if stream does not end" in {
    val p = fully(literal('a'))

    p.runA(Stream.ofString("ab")) shouldBe Error("end of input")
  }

}
