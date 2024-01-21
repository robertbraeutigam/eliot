package com.vanillasource.eliot.eliotc.token

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.engine.FactEngine
import org.scalatest.flatspec.{AnyFlatSpec, AsyncFlatSpec}
import org.scalatest.matchers.should.Matchers

class TokenizerTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  "tokenizer" should "return nothing for empty content" in {
    parseForTokens("").asserting(_ shouldBe Seq.empty)
  }

  private def parseForTokens(str: String): IO[Seq[Token]] =
    FactEngine(Seq(new Tokenizer()))
}
