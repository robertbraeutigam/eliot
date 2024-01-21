package com.vanillasource.eliot.eliotc.token

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.CompilerFact
import com.vanillasource.eliot.eliotc.main.CompilerEngine
import com.vanillasource.eliot.eliotc.source.*
import com.vanillasource.eliot.eliotc.token.Token.{Identifier, Keyword}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

class TokenizerTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
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
        Sourced(PositionRange(Position(1, 2), Position(1, 6)), Identifier("some")),
        Sourced(PositionRange(Position(2, 1), Position(2, 7)), Identifier("tokens"))
      )
    )
  }

  it should "include comments in the position of a token" in {
    parseForSourcedTokens(" /* some comment */ token").asserting(
      _ shouldBe Seq(Sourced(PositionRange(Position(1, 21), Position(1, 26)), Identifier("token")))
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

  private val file = new File("test.els")

  private def parseForTokens(source: String): IO[Seq[Token]] =
    parseForSourcedTokens(source)
      .map(_.map(_.value))

  private def parseForSourcedTokens(source: String): IO[Seq[Sourced[Token]]] =
    runTokenizer(source)
      .map(_.get(SourceTokens.Key(file)).map(_.asInstanceOf[SourceTokens].tokens).getOrElse(Seq.empty))

  private def parseForErrors(source: String): IO[Seq[String]] =
    runTokenizer(source)
      .map(_.values.collect { case SourcedError(_, Sourced(_, msg)) => msg }.toSeq)

  private def runTokenizer(source: String): IO[Map[Any, CompilerFact]] =
    CompilerEngine(Seq(new Tokenizer()))
      .resolve(Seq(SourceContent(file, source)))

}
