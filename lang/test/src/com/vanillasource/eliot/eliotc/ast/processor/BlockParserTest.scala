package com.vanillasource.eliot.eliotc.ast.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.ast.fact.{AST, Expression, SourceAST}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Tokenizer

class BlockParserTest extends ProcessorTest(new Tokenizer(), new ASTParser()) {

  "block parser" should "parse a block to a BlockExpression with one over-separated line per source line" in {
    blockOf("def f: A = {\n  val old = a\n  b\n  c\n}").asserting(_.map(_.lines.size) shouldBe Some(3))
  }

  it should "record the val binder on its line and leave statement lines binder-less" in {
    blockOf("def f: A = {\n  val old = a\n  b\n}")
      .asserting(_.map(_.lines.map(_.binder.map(_.name.value))) shouldBe Some(Seq(Some("old"), None)))
  }

  it should "over-separate a leading-dot continuation into two lines (the merge happens later)" in {
    blockOf("def f: A = {\n  foo\n  .bar\n}").asserting(_.map(_.lines.size) shouldBe Some(2))
  }

  it should "parse an empty block to a BlockExpression with no lines (emptiness is a later error)" in {
    blockOf("def f: A = {\n}").asserting(_.map(_.lines.size) shouldBe Some(0))
  }

  it should "keep a same-line atom run as a single line" in {
    blockOf("def f: A = {\n  foo bar baz\n}").asserting(_.map(_.lines.size) shouldBe Some(1))
  }

  it should "parse a typed binder" in {
    blockOf("def f: A = {\n  val x: A = a\n  x\n}")
      .asserting(_.flatMap(_.lines.head.binder.flatMap(_.typeExpression)).isDefined shouldBe true)
  }

  it should "parse a nested block (a val whose right-hand side is itself a block), keeping the outer two lines" in {
    blockOf("def f: A = {\n  val x = {\n    a\n    b\n  }\n  x\n}").asserting(_.map(_.lines.size) shouldBe Some(2))
  }

  it should "parse a block line carrying a trailing match expression" in {
    blockOf("def f: A = {\n  val r = x match { case A -> a }\n  r\n}")
      .asserting(_.flatMap(_.lines.headOption.map(_.expression.value)).collect {
        case _: Expression.MatchExpression => true
      } shouldBe Some(true))
  }

  private def blockOf(source: String): IO[Option[Expression.BlockExpression]] =
    runGenerator(source, SourceAST.Key(file)).map { case (_, facts) =>
      facts.values
        .collectFirst { case SourceAST(_, Sourced(_, _, AST(_, functions, _))) =>
          functions.flatMap(_.body).map(_.value).collectFirst {
            case b: Expression.BlockExpression                                 => b
            case Expression.FlatExpression(Seq(Sourced(_, _, b: Expression.BlockExpression))) => b
          }
        }
        .flatten
    }
}
