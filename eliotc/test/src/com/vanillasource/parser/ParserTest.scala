package com.vanillasource.parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.vanillasource.parser.Parser.*
import com.vanillasource.parser.ParserResult.Consume.*
import cats.syntax.all.*

import scala.collection.immutable.Seq

class ParserTest extends AnyFlatSpec with Matchers {
  "end-of-input parser" should "match end-of-input" in {
    val p = endOfInput[Char]()

    p.parse("") shouldBe ParserResult(NotConsumed, ParserError.noError, Seq.empty, Some(()))
  }

  it should "return error if stream did not reach end-of-input" in {
    val p = endOfInput[Char]()

    p.parse("abc") shouldBe ParserResult(NotConsumed, ParserError(0, Seq("end of input")), Seq.empty, None)
  }

  "literal" should "match the literal in stream" in {
    val p = literal('a')

    p.parse("abc") shouldBe ParserResult(Consumed, ParserError.noError, Seq.empty, Some('a'))
  }

  it should "fail on different literal" in {
    val p = literal('a')

    p.parse("bc") shouldBe ParserResult(NotConsumed, ParserError(0, Seq("a")), Seq.empty, None)
  }

  "full parsing" should "succeeds if stream ends" in {
    val p = literal('a').fully()

    p.parse("a") shouldBe ParserResult(Consumed, ParserError.noError, Seq.empty, Some('a'))
  }

  it should "fail if stream does not end" in {
    val p = literal('a').fully()

    p.parse("ab") shouldBe ParserResult(Consumed, ParserError(1, Seq("end of input")), Seq.empty, None)
  }

  "optional parser" should "parse if successful" in {
    val p = literal('a').optional()

    p.parse("a") shouldBe ParserResult(Consumed, ParserError.noError, Seq.empty, Some(Some('a')))
  }

  it should "return none successfully if parser fails" in {
    val p = literal('a').optional()

    p.parse("b") shouldBe ParserResult(NotConsumed, ParserError(0, Seq("a")), Seq.empty, Some(None))
  }

  "sequence of parsers" should "return skip if first parser fails" in {
    val p = literal('a') >> literal('b')

    p.parse("ab") shouldBe ParserResult(Consumed, ParserError.noError, Seq.empty, Some('b'))
  }

  it should "indicate nothing consumed if first parser fails" in {
    val p = literal('a') >> literal('b')

    p.parse("bb") shouldBe ParserResult(NotConsumed, ParserError(0, Seq("a")), Seq.empty, None)
  }

  it should "indicate something consumed if second parser fails" in {
    val p = literal('a') >> literal('b')

    p.parse("ac") shouldBe ParserResult(Consumed, ParserError(1, Seq("b")), Seq.empty, None)
  }

  "expected items" should "be listed in a many followed by a literal" in {
    val p = literal('a').anyTimes() >> literal('b')

    p.parse("aac") shouldBe ParserResult(Consumed, ParserError(2, Seq("a", "b")), Seq.empty, None)
  }

  "or" should "return first parser, if it matches" in {
    val p = literal('a').or(literal('b'))

    p.parse("a") shouldBe ParserResult(Consumed, ParserError.noError, Seq.empty, Some('a'))
  }

  it should "return second parser, if the first does not match" in {
    val p = literal('a').or(literal('b'))

    p.parse("b") shouldBe ParserResult(Consumed, ParserError.noError, Seq.empty, Some('b'))
  }

  it should "return both expected if none match" in {
    val p = literal('a').or(literal('b'))

    p.parse("c") shouldBe ParserResult(NotConsumed, ParserError(0, Seq("a", "b")), Seq.empty, None)
  }

  "atomic" should "should not consume any input" in {
    val a = (literal('a') >> literal('b') >> literal('c')).atomic().as(1)
    val b = (literal('a') >> literal('c')).as(2)
    val p = a or b

    p.parse("ac") shouldBe ParserResult(Consumed, ParserError.noError, Seq.empty, Some(2))
  }

  "find" should "parse whole input to find the parser" in {
    val p = (literal('a') >> literal('b') >> literal('c')).find()

    p.parse("..ab..abc..") shouldBe ParserResult(Consumed, ParserError.noError, Seq.empty, Some('c'))
  }

  it should "fail if the input did not contain a match and consume all of the input" in {
    val p = (literal('a') >> literal('b') >> literal('c')).find()

    p.parse("..ab..abd..") shouldBe ParserResult(Consumed, ParserError(11, Seq("a")), Seq.empty, None)
  }

  it should "fail with no input consumed, if atomic" in {
    val p = (literal('a') >> literal('b') >> literal('c')).find().atomic()

    p.parse("..ab..abd..") shouldBe ParserResult(NotConsumed, ParserError(11, Seq("a")), Seq.empty, None)
  }

  it should "collect all found matches if it can match any times" in {
    val p = (literal('a') >> literal('b') >> literal('c')).find().anyTimes().map(_.size)

    p.parse("..abc..abd..aaabc..") shouldBe ParserResult(Consumed, ParserError(0, Seq("a")), Seq.empty, Some(2))
  }

  it should "collect all found matches, and then continue to parse after last match if combined with atomic and any times" in {
    val a = (literal('a') >> literal('b') >> literal('c')).find().atomic().anyTimes().map(_.size)
    val b = literal('c') >> literal('d').as(99)
    val p = a >> b

    p.parse("..abc..abccd") shouldBe ParserResult(Consumed, ParserError.noError, Seq.empty, Some(99))
  }

  it should "report expected from both itself and following parser after last match" in {
    val a = (literal('a') >> literal('b') >> literal('c')).find().atomic().anyTimes().map(_.size)
    val b = literal('c') >> literal('d').as(99)
    val p = a >> b

    p.parse("..abc..abcxcd") shouldBe ParserResult(Consumed, ParserError(10, Seq("a", "c")), Seq.empty, None)
  }

  it should "parse success even if nothing found with anyTimes()" in {
    val p = (literal('a') >> literal('b') >> literal('c')).find().atomic().anyTimes().map(_.size)

    p.parse("..abd..abd..aaabd..") shouldBe ParserResult(NotConsumed, ParserError(0, Seq("a")), Seq.empty, Some(0))
  }

}
