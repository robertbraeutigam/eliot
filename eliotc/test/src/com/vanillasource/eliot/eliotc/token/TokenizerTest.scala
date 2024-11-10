package com.vanillasource.eliot.eliotc.token

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.{CompilerFact, ProcessorTest}
import com.vanillasource.eliot.eliotc.main.CompilerEngine
import com.vanillasource.eliot.eliotc.source.*
import com.vanillasource.eliot.eliotc.token.Token.{Identifier, IntegerLiteral, Keyword}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

class TokenizerTest extends ProcessorTest(new Tokenizer()) {
  private val file = new File("test.els")

  "tokenizer" should "return nothing for empty content" in {
    parseForTokens("").asserting(_ shouldBe Seq.empty)
  }

  it should "return nothing for just a line comment" in {
    parseForTokens(" // Some comment").asserting(_ shouldBe Seq.empty)
  }

  it should "return token on new line after line comment" in {
    parseForTokens(" // Some comment\nsomething").asserting(_ shouldBe Seq(Identifier("something")))
  }

  it should "parse keywords separately from identifiers" in {
    parseForTokens("import something").asserting(_ shouldBe Seq(Keyword("import"), Identifier("something")))
  }

  it should "not return tokens in multi-line block comments" in {
    parseForTokens(" /* Some comment\nSomething else */").asserting(_ shouldBe Seq.empty)
  }

  it should "return correct positions for tokens" in {
    parseForSourcedTokens(" some\ntokens").asserting(
      _ shouldBe Seq(
        Sourced(file, PositionRange(Position(1, 2), Position(1, 6)), Identifier("some")),
        Sourced(file, PositionRange(Position(2, 1), Position(2, 7)), Identifier("tokens"))
      )
    )
  }

  it should "include comments in the position of a token" in {
    parseForSourcedTokens(" /* some comment */ token").asserting(
      _ shouldBe Seq(Sourced(file, PositionRange(Position(1, 21), Position(1, 26)), Identifier("token")))
    )
  }

  it should "fail on non-complete block comment" in {
    parseForErrors(" token /* some comment").asserting(
      _ shouldBe Seq("Parser error, unexpected end of input, expected end of comment.")
    )
  }

  it should "fail on illegal characters" in {
    parseForErrors(" token →").asserting(
      _.headOption.getOrElse("") should startWith("Parser error, unexpected \"→\"")
    )
  }

  it should "parse 1 as integer literal" in {
    parseForSourcedTokens("1").asserting(
      _ shouldBe Seq(Sourced(file, PositionRange(Position(1, 1), Position(1, 2)), IntegerLiteral(BigInt(1))))
    )
  }

  it should "parse 123 as integer literal" in {
    parseForSourcedTokens("123").asserting(
      _ shouldBe Seq(Sourced(file, PositionRange(Position(1, 1), Position(1, 4)), IntegerLiteral(BigInt(123))))
    )
  }

  private def parseForTokens(source: String): IO[Seq[Token]] =
    parseForSourcedTokens(source)
      .map(_.map(_.value))

  private def parseForSourcedTokens(source: String): IO[Seq[Sourced[Token]]] =
    runEngine(source)
      .map(_.get(SourceTokens.Key(file)).map(_.asInstanceOf[SourceTokens].tokens).getOrElse(Seq.empty))
}
