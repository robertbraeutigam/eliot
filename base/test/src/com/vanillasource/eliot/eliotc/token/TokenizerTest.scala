package com.vanillasource.eliot.eliotc.token

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.source.*
import com.vanillasource.eliot.eliotc.source.pos.{Position, PositionRange, Sourced}
import com.vanillasource.eliot.eliotc.token.Token.{Identifier, IntegerLiteral, Keyword, StringLiteral}

class TokenizerTest extends ProcessorTest(new Tokenizer()) {
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
    runEngineForErrors(" token /* some comment").asserting(
      _ shouldBe Seq("Parser error, unexpected end of input, expected end of comment.")
    )
  }

  it should "fail on illegal characters" in {
    runEngineForErrors(" token →").asserting(
      _.headOption.getOrElse("") should startWith("Parser error, unexpected \"→\"")
    )
  }

  it should "parse 1 as integer literal" in {
    parseForSourcedTokens("1").asserting(
      _ shouldBe Seq(Sourced(file, PositionRange(Position(1, 1), Position(1, 2)), IntegerLiteral("1")))
    )
  }

  it should "parse 123 as integer literal" in {
    parseForSourcedTokens("123").asserting(
      _ shouldBe Seq(Sourced(file, PositionRange(Position(1, 1), Position(1, 4)), IntegerLiteral("123")))
    )
  }

  it should "parse an empty string literal as empty string" in {
    parseForSourcedTokens("\"\"").asserting(
      _ shouldBe Seq(Sourced(file, PositionRange(Position(1, 1), Position(1, 3)), StringLiteral("")))
    )
  }

  it should "parse 'abc' string literal" in {
    parseForSourcedTokens("\"abc\"").asserting(
      _ shouldBe Seq(Sourced(file, PositionRange(Position(1, 1), Position(1, 6)), StringLiteral("abc")))
    )
  }

  it should "parse unicode in string literal" in {
    parseForSourcedTokens("\"αβγ\"").asserting(
      _ shouldBe Seq(Sourced(file, PositionRange(Position(1, 1), Position(1, 6)), StringLiteral("αβγ")))
    )
  }

  private def parseForTokens(source: String): IO[Seq[Token]] =
    parseForSourcedTokens(source)
      .map(_.map(_.value))

  private def parseForSourcedTokens(source: String): IO[Seq[Sourced[Token]]] =
    runEngine(source)
      .map(_.get(SourceTokens.Key(file)).map(_.asInstanceOf[SourceTokens].tokens).getOrElse(Seq.empty))
}
