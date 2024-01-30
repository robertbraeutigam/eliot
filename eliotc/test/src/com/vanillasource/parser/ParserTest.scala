package com.vanillasource.parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.vanillasource.parser.Parser.*
import com.vanillasource.parser.ParserResult.*
import com.vanillasource.parser.ParserResult.Consume.*
import cats.syntax.all.*

import scala.collection.immutable.Seq

class ParserTest extends AnyFlatSpec with Matchers {
  "end-of-input parser" should "match end-of-input" in {
    val p = endOfInput[Char]()

    p.runParser("") shouldBe Success(NotConsumed, ParserError.noError, ())
  }

  it should "return error if stream did not reach end-of-input" in {
    val p = endOfInput[Char]()

    p.runParser("abc") shouldBe Failure(NotConsumed, ParserError(0, Seq("end of input")))
  }

  "literal" should "match the literal in stream" in {
    val p = literal('a')

    p.runParser("abc") shouldBe Success(Consumed, ParserError.noError, 'a')
  }

  it should "fail on different literal" in {
    val p = literal('a')

    p.runParser("bc") shouldBe Failure(NotConsumed, ParserError(0, Seq("a")))
  }

  "full parsing" should "succeeds if stream ends" in {
    val p = literal('a').fully()

    p.runParser("a") shouldBe Success(Consumed, ParserError.noError, 'a')
  }

  it should "fail if stream does not end" in {
    val p = literal('a').fully()

    p.runParser("ab") shouldBe Failure(Consumed, ParserError(1, Seq("end of input")))
  }

  "optional parser" should "parse if successful" in {
    val p = literal('a').optional()

    p.runParser("a") shouldBe Success(Consumed, ParserError.noError, Some('a'))
  }

  it should "return none successfully if parser fails" in {
    val p = literal('a').optional()

    p.runParser("b") shouldBe Success(NotConsumed, ParserError(0, Seq("a")), None)
  }

  "sequence of parsers" should "return skip if first parser fails" in {
    val p = literal('a') >> literal('b')

    p.runParser("ab") shouldBe Success(Consumed, ParserError.noError, 'b')
  }

  it should "indicate nothing consumed if first parser fails" in {
    val p = literal('a') >> literal('b')

    p.runParser("bb") shouldBe Failure(NotConsumed, ParserError(0, Seq("a")))
  }

  it should "indicate something consumed if second parser fails" in {
    val p = literal('a') >> literal('b')

    p.runParser("ac") shouldBe Failure(Consumed, ParserError(1, Seq("b")))
  }

  "expected items" should "be listed in a many followed by a literal" in {
    val p = literal('a').anyTimes() >> literal('b')

    p.runParser("aac") shouldBe Failure(Consumed, ParserError(2, Seq("a", "b")))
  }

  "or" should "return first parser, if it matches" in {
    val p = literal('a').or(literal('b'))

    p.runParser("a") shouldBe Success(Consumed, ParserError.noError, 'a')
  }

  it should "return second parser, if the first does not match" in {
    val p = literal('a').or(literal('b'))

    p.runParser("b") shouldBe Success(Consumed, ParserError.noError, 'b')
  }

  it should "return both expected if none match" in {
    val p = literal('a').or(literal('b'))

    p.runParser("c") shouldBe Failure(NotConsumed, ParserError(0, Seq("a", "b")))
  }

  "atomic" should "should not consume any input" in {
    val a = (literal('a') >> literal('b') >> literal('c')).atomic().as(1)
    val b = (literal('a') >> literal('c')).as(2)
    val p = a or b

    p.runParser("ac") shouldBe Success(Consumed, ParserError.noError, 2)
  }

  "find" should "parse whole input to find the parser" in {
    val p = (literal('a') >> literal('b') >> literal('c')).find()

    p.runParser("..ab..abc..") shouldBe Success(Consumed, ParserError.noError, 'c')
  }

  it should "fail if the input did not contain a match and consume all of the input" in {
    val p = (literal('a') >> literal('b') >> literal('c')).find()

    p.runParser("..ab..abd..") shouldBe Failure(Consumed, ParserError(11, Seq("a")))
  }

  it should "fail with no input consumed, if atomic" in {
    val p = (literal('a') >> literal('b') >> literal('c')).find().atomic()

    p.runParser("..ab..abd..") shouldBe Failure(NotConsumed, ParserError(11, Seq("a")))
  }

  it should "collect all found matches if it can match any times" in {
    val p = (literal('a') >> literal('b') >> literal('c')).find().anyTimes().map(_.size)

    p.runParser("..abc..abd..aaabc..") shouldBe Success(Consumed, ParserError(0, Seq("a")), 2)
  }

  it should "collect all found matches, and then continue to parse after last match if combined with atomic and any times" in {
    val a = (literal('a') >> literal('b') >> literal('c')).find().atomic().anyTimes().map(_.size)
    val b = literal('c') >> literal('d').as(99)
    val p = a >> b

    p.runParser("..abc..abccd") shouldBe Success(Consumed, ParserError.noError, 99)
  }

  it should "report expected from both itself and following parser after last match" in {
    val a = (literal('a') >> literal('b') >> literal('c')).find().atomic().anyTimes().map(_.size)
    val b = literal('c') >> literal('d').as(99)
    val p = a >> b

    p.runParser("..abc..abcxcd") shouldBe Failure(Consumed, ParserError(10, Seq("a", "c")))
  }

  it should "parse success even if nothing found with anyTimes()" in {
    val p = (literal('a') >> literal('b') >> literal('c')).find().atomic().anyTimes().map(_.size)

    p.runParser("..abd..abd..aaabd..") shouldBe Success(NotConsumed, ParserError(0, Seq("a")), 0)
  }

}
