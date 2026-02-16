package com.vanillasource.eliot.eliotc.indent

import cats.effect.IO
import com.vanillasource.eliot.eliotc.ProcessorTest
import com.vanillasource.eliot.eliotc.indent.LayoutToken.*
import com.vanillasource.eliot.eliotc.token.Token.{Identifier, Keyword, Symbol}
import com.vanillasource.eliot.eliotc.token.Tokenizer

class LayoutGeneratorTest extends ProcessorTest(new Tokenizer(), new LayoutGenerator()) {
  "layout generator" should "return nothing for empty content" in {
    runEngineForLayoutTokens("").asserting(_ shouldBe Seq.empty)
  }

  it should "return content token and trailing newline for single token" in {
    runEngineForLayoutTokens("a").asserting(
      _ shouldBe Seq(ContentToken(Identifier("a")), Newline)
    )
  }

  it should "keep multiple tokens on same line without layout tokens between them" in {
    runEngineForLayoutTokens("a b c").asserting(
      _ shouldBe Seq(
        ContentToken(Identifier("a")),
        ContentToken(Identifier("b")),
        ContentToken(Identifier("c")),
        Newline
      )
    )
  }

  it should "insert newline between tokens on different lines at same indent" in {
    runEngineForLayoutTokens("a\nb").asserting(
      _ shouldBe Seq(
        ContentToken(Identifier("a")),
        Newline,
        ContentToken(Identifier("b")),
        Newline
      )
    )
  }

  it should "insert newlines for multiple lines at same indent" in {
    runEngineForLayoutTokens("a\nb\nc").asserting(
      _ shouldBe Seq(
        ContentToken(Identifier("a")),
        Newline,
        ContentToken(Identifier("b")),
        Newline,
        ContentToken(Identifier("c")),
        Newline
      )
    )
  }

  it should "insert indent and trailing dedent for indented block" in {
    runEngineForLayoutTokens("a\n  b").asserting(
      _ shouldBe Seq(
        ContentToken(Identifier("a")),
        Newline,
        Indent,
        ContentToken(Identifier("b")),
        Newline,
        Dedent
      )
    )
  }

  it should "insert newline without layout for same-level tokens in indented block" in {
    runEngineForLayoutTokens("a\n  b\n  c").asserting(
      _ shouldBe Seq(
        ContentToken(Identifier("a")),
        Newline,
        Indent,
        ContentToken(Identifier("b")),
        Newline,
        ContentToken(Identifier("c")),
        Newline,
        Dedent
      )
    )
  }

  it should "insert dedent when returning to base indentation" in {
    runEngineForLayoutTokens("a\n  b\nc").asserting(
      _ shouldBe Seq(
        ContentToken(Identifier("a")),
        Newline,
        Indent,
        ContentToken(Identifier("b")),
        Newline,
        Dedent,
        ContentToken(Identifier("c")),
        Newline
      )
    )
  }

  it should "handle two levels of indentation" in {
    runEngineForLayoutTokens("a\n  b\n    c").asserting(
      _ shouldBe Seq(
        ContentToken(Identifier("a")),
        Newline,
        Indent,
        ContentToken(Identifier("b")),
        Newline,
        Indent,
        ContentToken(Identifier("c")),
        Newline,
        Dedent,
        Dedent
      )
    )
  }

  it should "dedent multiple levels at once" in {
    runEngineForLayoutTokens("a\n  b\n    c\nd").asserting(
      _ shouldBe Seq(
        ContentToken(Identifier("a")),
        Newline,
        Indent,
        ContentToken(Identifier("b")),
        Newline,
        Indent,
        ContentToken(Identifier("c")),
        Newline,
        Dedent,
        Dedent,
        ContentToken(Identifier("d")),
        Newline
      )
    )
  }

  it should "dedent partially to intermediate level" in {
    runEngineForLayoutTokens("a\n  b\n    c\n  d").asserting(
      _ shouldBe Seq(
        ContentToken(Identifier("a")),
        Newline,
        Indent,
        ContentToken(Identifier("b")),
        Newline,
        Indent,
        ContentToken(Identifier("c")),
        Newline,
        Dedent,
        ContentToken(Identifier("d")),
        Newline,
        Dedent
      )
    )
  }

  it should "handle multiple tokens per line with indentation" in {
    runEngineForLayoutTokens("a b\n  c d").asserting(
      _ shouldBe Seq(
        ContentToken(Identifier("a")),
        ContentToken(Identifier("b")),
        Newline,
        Indent,
        ContentToken(Identifier("c")),
        ContentToken(Identifier("d")),
        Newline,
        Dedent
      )
    )
  }

  it should "handle multiple indent-dedent cycles" in {
    runEngineForLayoutTokens("a\n  b\nc\n  d").asserting(
      _ shouldBe Seq(
        ContentToken(Identifier("a")),
        Newline,
        Indent,
        ContentToken(Identifier("b")),
        Newline,
        Dedent,
        ContentToken(Identifier("c")),
        Newline,
        Indent,
        ContentToken(Identifier("d")),
        Newline,
        Dedent
      )
    )
  }

  it should "handle keywords as content tokens" in {
    runEngineForLayoutTokens("import a").asserting(
      _ shouldBe Seq(
        ContentToken(Keyword("import")),
        ContentToken(Identifier("a")),
        Newline
      )
    )
  }

  it should "handle symbols as content tokens" in {
    runEngineForLayoutTokens("a : b").asserting(
      _ shouldBe Seq(
        ContentToken(Identifier("a")),
        ContentToken(Symbol(":")),
        ContentToken(Identifier("b")),
        Newline
      )
    )
  }

  it should "handle three levels of indentation with full dedent" in {
    runEngineForLayoutTokens("a\n  b\n    c\n      d\ne").asserting(
      _ shouldBe Seq(
        ContentToken(Identifier("a")),
        Newline,
        Indent,
        ContentToken(Identifier("b")),
        Newline,
        Indent,
        ContentToken(Identifier("c")),
        Newline,
        Indent,
        ContentToken(Identifier("d")),
        Newline,
        Dedent,
        Dedent,
        Dedent,
        ContentToken(Identifier("e")),
        Newline
      )
    )
  }

  it should "handle indent after same-level newline" in {
    runEngineForLayoutTokens("a\nb\n  c").asserting(
      _ shouldBe Seq(
        ContentToken(Identifier("a")),
        Newline,
        ContentToken(Identifier("b")),
        Newline,
        Indent,
        ContentToken(Identifier("c")),
        Newline,
        Dedent
      )
    )
  }

  private def runEngineForLayoutTokens(source: String): IO[Seq[LayoutToken]] =
    runGenerator(source, SourceLayoutTokens.Key(file))
      .map(
        _._2
          .get(SourceLayoutTokens.Key(file))
          .map(_.asInstanceOf[SourceLayoutTokens].tokens.value.map(_.value))
          .getOrElse(Seq.empty)
      )
}
