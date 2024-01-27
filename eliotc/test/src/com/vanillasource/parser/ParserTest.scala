package com.vanillasource.parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.vanillasource.parser.Parser.*
import com.vanillasource.parser.ParserResult._
import cats.syntax.all._

class ParserTest extends AnyFlatSpec with Matchers {
  "end-of-input parser" should "match end-of-input" in {
    val p = endOfInput[Char]()

    p.runA("") shouldBe Success(consumed = false, ())
  }

  it should "return error if stream did not reach end-of-input" in {
    val p = endOfInput[Char]()

    p.runA("abc") shouldBe Failure(consumed = false, "end of input")
  }

  "literal" should "match the literal in stream" in {
    val p = literal('a')

    p.runA("abc") shouldBe Success(consumed = true, 'a')
  }

  it should "fail on different literal" in {
    val p = literal('a')

    p.runA("bc") shouldBe Failure(consumed = false, "literal 'a'")
  }

  "full parsing" should "succeeds if stream ends" in {
    val p = fully(literal('a'))

    p.runA("a") shouldBe Success(consumed = true, 'a')
  }

  it should "fail if stream does not end" in {
    val p = fully(literal('a'))

    p.runA("ab") shouldBe Failure(consumed = true, "end of input")
  }

  "optional parser" should "parse if successful" in {
    val p = option(literal('a'))

    p.runA("a") shouldBe Success(consumed = true, Some('a'))
  }

  it should "return none successfully if parser fails" in {
    val p = option(literal('a'))

    p.runA("b") shouldBe Success(consumed = false, None)
  }

  "sequence of parsers" should "return skip if first parser fails" in {
    val p = literal('a') >> literal('b')

    p.runA("ab") shouldBe Success(consumed = true, 'b')
  }

  it should "indicate nothing consumed if first parser fails" in {
    val p = literal('a') >> literal('b')

    p.runA("bb") shouldBe Failure(consumed = false, "literal 'a'")
  }

  it should "indicate something consumed if second parser fails" in {
    val p = literal('a') >> literal('b')

    p.runA("ac") shouldBe Failure(consumed = true, "literal 'b'")
  }

}
