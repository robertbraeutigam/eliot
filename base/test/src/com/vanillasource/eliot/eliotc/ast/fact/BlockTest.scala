package com.vanillasource.eliot.eliotc.ast.fact

import com.vanillasource.eliot.eliotc.indent.LayoutToken
import com.vanillasource.eliot.eliotc.indent.LayoutToken.*
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token.Identifier
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URI

class BlockTest extends AnyFlatSpec with Matchers {
  "blocks" should "return empty for empty input" in {
    Block.blocks(Seq.empty) shouldBe Seq.empty
  }

  it should "return a single block for tokens without indentation" in {
    Block.blocks(tokens(ContentToken(Identifier("a")), Newline)).map(values) shouldBe Seq(
      Seq(ContentToken(Identifier("a")), Newline)
    )
  }

  it should "return a single block for indent and dedent back to level 0" in {
    Block
      .blocks(tokens(ContentToken(Identifier("a")), Indent, ContentToken(Identifier("b")), Dedent))
      .map(values) shouldBe Seq(
      Seq(ContentToken(Identifier("a")), Indent, ContentToken(Identifier("b"))),
      Seq(Dedent)
    )
  }

  it should "keep nested indent-dedent in same block" in {
    Block.blocks(tokens(Indent, Indent, ContentToken(Identifier("a")), Dedent, Dedent)).map(values) shouldBe Seq(
      Seq(Indent, Indent, ContentToken(Identifier("a")), Dedent),
      Seq(Dedent)
    )
  }

  it should "split into multiple blocks at top level" in {
    Block
      .blocks(
        tokens(
          ContentToken(Identifier("a")),
          Indent,
          ContentToken(Identifier("b")),
          Dedent,
          ContentToken(Identifier("c")),
          Indent,
          ContentToken(Identifier("d")),
          Dedent
        )
      )
      .map(values) shouldBe Seq(
      Seq(ContentToken(Identifier("a")), Indent, ContentToken(Identifier("b"))),
      Seq(Dedent, ContentToken(Identifier("c")), Indent, ContentToken(Identifier("d"))),
      Seq(Dedent)
    )
  }

  it should "keep deeply nested tokens in one block" in {
    Block
      .blocks(
        tokens(
          Indent,
          Indent,
          Indent,
          ContentToken(Identifier("a")),
          Dedent,
          Dedent,
          Dedent
        )
      )
      .map(values) shouldBe Seq(
      Seq(Indent, Indent, Indent, ContentToken(Identifier("a")), Dedent, Dedent),
      Seq(Dedent)
    )
  }

  it should "handle tokens before and after an indented block" in {
    Block
      .blocks(
        tokens(
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
      .map(values) shouldBe Seq(
      Seq(ContentToken(Identifier("a")), Newline, Indent, ContentToken(Identifier("b")), Newline),
      Seq(Dedent, ContentToken(Identifier("c")), Newline)
    )
  }

  private val file = URI("test://test")

  private def sourced(token: LayoutToken): Sourced[LayoutToken] =
    Sourced(file, PositionRange.zero, token)

  private def tokens(ts: LayoutToken*): Seq[Sourced[LayoutToken]] =
    ts.map(sourced)

  private def values(block: Seq[Sourced[LayoutToken]]): Seq[LayoutToken] =
    block.map(_.value)
}
