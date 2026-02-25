package com.vanillasource.eliot.eliotc.token

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.pos.{Position, PositionRange}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token.{Identifier, IntegerLiteral, Keyword, StringLiteral}

class TokenizerTest extends ProcessorTest(new Tokenizer()) {
  "tokenizer" should "return nothing for empty content" in {
    runEngineForTokens("").asserting(_ shouldBe Seq.empty)
  }

  it should "return nothing for just a line comment" in {
    runEngineForTokens(" // Some comment").asserting(_ shouldBe Seq.empty)
  }

  it should "return token on new line after line comment" in {
    runEngineForTokens(" // Some comment\nsomething").asserting(_ shouldBe Seq(Identifier("something")))
  }

  it should "parse keywords separately from identifiers" in {
    runEngineForTokens("import something").asserting(_ shouldBe Seq(Keyword("import"), Identifier("something")))
  }

  it should "not return tokens in multi-line block comments" in {
    runEngineForTokens(" /* Some comment\nSomething else */").asserting(_ shouldBe Seq.empty)
  }

  it should "return correct positions for tokens" in {
    runEngineForSourcedTokens(" some\ntokens").asserting(
      _ shouldBe Seq(
        Sourced(file, PositionRange(Position(1, 2), Position(1, 6)), Identifier("some")),
        Sourced(file, PositionRange(Position(2, 1), Position(2, 7)), Identifier("tokens"))
      )
    )
  }

  it should "include comments in the position of a token" in {
    runEngineForSourcedTokens(" /* some comment */ token").asserting(
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
    runEngineForSourcedTokens("1").asserting(
      _ shouldBe Seq(Sourced(file, PositionRange(Position(1, 1), Position(1, 2)), IntegerLiteral("1")))
    )
  }

  it should "parse 123 as integer literal" in {
    runEngineForSourcedTokens("123").asserting(
      _ shouldBe Seq(Sourced(file, PositionRange(Position(1, 1), Position(1, 4)), IntegerLiteral("123")))
    )
  }

  it should "parse an empty string literal as empty string" in {
    runEngineForSourcedTokens("\"\"").asserting(
      _ shouldBe Seq(Sourced(file, PositionRange(Position(1, 1), Position(1, 3)), StringLiteral("")))
    )
  }

  it should "parse 'abc' string literal" in {
    runEngineForSourcedTokens("\"abc\"").asserting(
      _ shouldBe Seq(Sourced(file, PositionRange(Position(1, 1), Position(1, 6)), StringLiteral("abc")))
    )
  }

  it should "parse unicode in string literal" in {
    runEngineForSourcedTokens("\"αβγ\"").asserting(
      _ shouldBe Seq(Sourced(file, PositionRange(Position(1, 1), Position(1, 6)), StringLiteral("αβγ")))
    )
  }

  it should "tokenize :: as a standalone symbol separate from surrounding operators" in {
    runEngineForTokens("Module::+").asserting(
      _ shouldBe Seq(Identifier("Module"), Token.Symbol("::"), Token.Symbol("+"))
    )
  }

  private def runEngineForErrors(source: String): IO[Seq[String]] =
    runGenerator(source, SourceTokens.Key(file)).map(_._1.map(_.message))

  private def runEngineForTokens(source: String): IO[Seq[Token]] =
    runEngineForSourcedTokens(source)
      .map(_.map(_.value))

  private def runEngineForSourcedTokens(source: String): IO[Seq[Sourced[Token]]] =
    runGenerator(source, SourceTokens.Key(file))
      .map(_._2.get(SourceTokens.Key(file)).map(_.asInstanceOf[SourceTokens].tokens.value).getOrElse(Seq.empty))
}
