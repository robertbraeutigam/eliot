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

  it should "decode a backslash escape in a string literal" in {
    runEngineForTokens("\"a\\\\b\"").asserting(_ shouldBe Seq(StringLiteral("a\\b")))
  }

  it should "decode an escaped double quote in a string literal" in {
    runEngineForTokens("\"a\\\"b\"").asserting(_ shouldBe Seq(StringLiteral("a\"b")))
  }

  it should "decode an escaped single quote in a string literal" in {
    runEngineForTokens("\"a\\'b\"").asserting(_ shouldBe Seq(StringLiteral("a'b")))
  }

  it should "decode a newline escape in a string literal" in {
    runEngineForTokens("\"a\\nb\"").asserting(_ shouldBe Seq(StringLiteral("a\nb")))
  }

  it should "decode a tab escape in a string literal" in {
    runEngineForTokens("\"a\\tb\"").asserting(_ shouldBe Seq(StringLiteral("a\tb")))
  }

  it should "decode a carriage return escape in a string literal" in {
    runEngineForTokens("\"a\\rb\"").asserting(_ shouldBe Seq(StringLiteral("a\rb")))
  }

  it should "decode an ESC escape in a string literal" in {
    runEngineForTokens("\"a\\eb\"").asserting(_ shouldBe Seq(StringLiteral("ab")))
  }

  it should "decode all the remaining control escapes in a string literal" in {
    val expected = new String(Array(0x00, 0x07, 0x08, 0x0b, 0x0c).map(_.toChar))
    runEngineForTokens("\"\\0\\a\\b\\v\\f\"").asserting(_ shouldBe Seq(StringLiteral(expected)))
  }

  it should "decode a unicode escape in a string literal" in {
    runEngineForTokens("\"a\\u03b1b\"").asserting(_ shouldBe Seq(StringLiteral("aαb")))
  }

  it should "decode a surrogate pair of unicode escapes as an astral character" in {
    runEngineForTokens("\"\\ud83d\\ude00\"").asserting(_ shouldBe Seq(StringLiteral("😀")))
  }

  it should "fail on an unknown escape in a string literal" in {
    runEngineForErrors("\"a\\qb\"").asserting(
      _ shouldBe Seq("Parser error, unexpected \"q\", expected end of escape sequence, invalid escape sequence.")
    )
  }

  it should "fail on a too-short unicode escape in a string literal" in {
    runEngineForErrors("\"a\\u03b\"").asserting(
      _ shouldBe Seq("Parser error, numeric escape requires 4 digits, but only got 3.")
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
