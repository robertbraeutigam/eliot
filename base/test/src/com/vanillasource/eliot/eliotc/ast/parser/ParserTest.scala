package com.vanillasource.eliot.eliotc.ast.parser

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.parser.Parser.*
import com.vanillasource.eliot.eliotc.ast.parser.ParserResult.Consume.*
import com.vanillasource.eliot.eliotc.ast.parser.{ParserError, ParserResult}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.collection.immutable.Seq

class ParserTest extends AnyFlatSpec with Matchers {
  "end-of-input parser" should "match end-of-input" in {
    val p = endOfInput[Char]()

    p.parse("") shouldBe ParserResult(NotConsumed, ParserError.noError, Some(()))
  }

  it should "return error if stream did not reach end-of-input" in {
    val p = endOfInput[Char]()

    p.parse("abc") shouldBe ParserResult(NotConsumed, ParserError(0, Set("end of input")), None)
  }

  "error" should "fail without consuming input" in {
    val p = error[Char]("failed")

    p.parse("abc") shouldBe ParserResult(NotConsumed, ParserError(0, Set("failed")), None)
  }

  "literal" should "match the literal in stream" in {
    val p = literal('a')

    p.parse("abc") shouldBe ParserResult(Consumed, ParserError.noError, Some('a'))
  }

  it should "fail on different literal" in {
    val p = literal('a')

    p.parse("bc") shouldBe ParserResult(NotConsumed, ParserError(0, Set("a")), None)
  }

  "full parsing" should "succeeds if stream ends" in {
    val p = literal('a').fully()

    p.parse("a") shouldBe ParserResult(Consumed, ParserError.noError, Some('a'))
  }

  it should "fail if stream does not end" in {
    val p = literal('a').fully()

    p.parse("ab") shouldBe ParserResult(Consumed, ParserError(1, Set("end of input")), None)
  }

  "optional parser" should "parse if successful" in {
    val p = literal('a').optional()

    p.parse("a") shouldBe ParserResult(Consumed, ParserError.noError, Some(Some('a')))
  }

  it should "return none successfully if parser fails" in {
    val p = literal('a').optional()

    p.parse("b") shouldBe ParserResult(NotConsumed, ParserError(0, Set("a")), Some(None))
  }

  "sequence of parsers" should "return skip if first parser fails" in {
    val p = literal('a') >> literal('b')

    p.parse("ab") shouldBe ParserResult(Consumed, ParserError.noError, Some('b'))
  }

  it should "indicate nothing consumed if first parser fails" in {
    val p = literal('a') >> literal('b')

    p.parse("bb") shouldBe ParserResult(NotConsumed, ParserError(0, Set("a")), None)
  }

  it should "indicate something consumed if second parser fails" in {
    val p = literal('a') >> literal('b')

    p.parse("ac") shouldBe ParserResult(Consumed, ParserError(1, Set("b")), None)
  }

  "expected items" should "be listed in a many followed by a literal" in {
    val p = literal('a').anyTimes() >> literal('b')

    p.parse("aac") shouldBe ParserResult(Consumed, ParserError(2, Set("a", "b")), None)
  }

  "or" should "return first parser, if it matches" in {
    val p = literal('a').or(literal('b'))

    p.parse("a") shouldBe ParserResult(Consumed, ParserError.noError, Some('a'))
  }

  it should "return second parser, if the first does not match" in {
    val p = literal('a').or(literal('b'))

    p.parse("b") shouldBe ParserResult(Consumed, ParserError.noError, Some('b'))
  }

  it should "return both expected if none match" in {
    val p = literal('a').or(literal('b'))

    p.parse("c") shouldBe ParserResult(NotConsumed, ParserError(0, Set("a", "b")), None)
  }

  "atomic" should "should not consume any input" in {
    val a = (literal('a') >> literal('b') >> literal('c')).atomic().as(1)
    val b = (literal('a') >> literal('c')).as(2)
    val p = a or b

    p.parse("ac") shouldBe ParserResult(Consumed, ParserError.noError, Some(2))
  }

  "any" should "consume a random item" in {
    val p = any[Char]()

    p.parse("a") shouldBe ParserResult(Consumed, ParserError.noError, Some('a'))
  }

  it should "fail if there are no more tokens" in {
    val p = any[Char]()

    p.parse("") shouldBe ParserResult(NotConsumed, ParserError(0, Set("input")), None)
  }

  "skip to" should "do nothing, if parser already matches" in {
    val p = (literal('a') >> literal('b')).skipTo()

    p.parse("ab") shouldBe ParserResult(NotConsumed, ParserError.noError, Some('b'))
  }

  it should "skip through items that do not match" in {
    val p = (literal('a') >> literal('b')).skipTo()

    p.parse("..a..ac..ab..") shouldBe ParserResult(Consumed, ParserError.noError, Some('b'))
  }

  it should "let the parser itself match again after it was skipped to" in {
    val p = (literal('a') >> literal('b')).skipTo() >> (literal('a') >> literal('b'))

    p.parse("..a..ac..abcd") shouldBe ParserResult(Consumed, ParserError.noError, Some('b'))
  }

  it should "fail if pattern is nowhere to be found with consuming everything" in {
    val p = (literal('a') >> literal('b')).skipTo()

    p.parse("..a..ac..ad..") shouldBe ParserResult(Consumed, ParserError(13, Set("a", "input")), None)
  }

  "any times" should "return a successful sequence and potential expected" in {
    val p = (literal('a') >> literal('b')).anyTimes()

    p.parse("ababab") shouldBe ParserResult(Consumed, ParserError(0, Set("a")), Some(Seq('b', 'b', 'b')))
  }

  "at least once separated by" should "fail on empty input" in {
    val p = literal('a').atLeastOnceSeparatedBy(literal(','))

    p.parse("") shouldBe ParserResult(NotConsumed, ParserError(0, Set("a")), None)
  }

  it should "parser a single item" in {
    val p = literal('a').atLeastOnceSeparatedBy(literal(','))

    p.parse("a") shouldBe ParserResult(Consumed, ParserError(0, Set(",")), Some(Seq('a')))
  }

  it should "parser a two items" in {
    val p = literal('a').atLeastOnceSeparatedBy(literal(','))

    p.parse("a,a") shouldBe ParserResult(Consumed, ParserError(0, Set(",")), Some(Seq('a', 'a')))
  }

  it should "return first and last consumed items with withBounds" in {
    val p = literal('a') >> literal('b') >> literal('c')

    p.withBounds.parse("abc").value shouldBe Some(('c', 'a', 'c'))
  }

  it should "return same item for first and last when single item consumed with withBounds" in {
    val p = literal('a')

    p.withBounds.parse("a").value shouldBe Some(('a', 'a', 'a'))
  }

  "recovering" should "return right on success" in {
    val p = (literal('a') >> literal('b')).recovering(_ == 'a')

    p.parse("ab").value shouldBe Some(Right('b'))
  }

  it should "propagate failure when nothing consumed" in {
    val p = (literal('a') >> literal('b')).recovering(_ == 'a')

    p.parse("cd") shouldBe ParserResult(NotConsumed, ParserError(0, Set("a")), None)
  }

  it should "return left with error when failing after consuming" in {
    val p = (literal('a') >> literal('b')).recovering(_ == 'a')

    p.parse("ac").value shouldBe Some(Left(ParserError(1, Set("b"))))
  }

  it should "skip to next span boundary after failing" in {
    val p = (literal('a') >> literal('b')).recovering(_ == 'a') >> literal('a')

    p.parse("ac..a").value shouldBe Some('a')
  }

  it should "skip to end of input when no boundary found" in {
    val p = (literal('a') >> literal('b')).recovering(_ == 'a')

    p.fully().parse("ac..").value shouldBe Some(Left(ParserError(1, Set("b"))))
  }

  "recovering any times" should "parse all successful spans" in {
    val p = (literal('a') >> literal('b')).recoveringAnyTimes(_ == 'a')

    p.parse("ababab").value shouldBe Some((Seq.empty, Seq('b', 'b', 'b')))
  }

  it should "collect errors from failed spans and continue" in {
    val p = (literal('a') >> literal('b')).recoveringAnyTimes(_ == 'a')

    p.parse("acabab").value shouldBe Some((Seq(ParserError(1, Set("b"))), Seq('b', 'b')))
  }

  it should "stop when no span boundary is ahead" in {
    val p = (literal('a') >> literal('b')).recoveringAnyTimes(_ == 'a') >> literal('z')

    p.parse("ababz").value shouldBe Some('z')
  }

  it should "return empty results on empty input" in {
    val p = (literal('a') >> literal('b')).recoveringAnyTimes(_ == 'a')

    p.parse("").value shouldBe Some((Seq.empty, Seq.empty))
  }

  it should "collect multiple errors from consecutive failed spans" in {
    val p = (literal('a') >> literal('b')).recoveringAnyTimes(_ == 'a')

    p.parse("acac").value shouldBe Some((Seq(ParserError(1, Set("b")), ParserError(3, Set("b"))), Seq.empty))
  }

  it should "recover failed span between successful spans" in {
    val p = (literal('a') >> literal('b')).recoveringAnyTimes(_ == 'a')

    p.parse("abacab").value shouldBe Some((Seq(ParserError(3, Set("b"))), Seq('b', 'b')))
  }
}
