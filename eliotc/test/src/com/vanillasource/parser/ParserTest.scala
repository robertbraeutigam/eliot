package com.vanillasource.parser

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import com.vanillasource.parser.Parser.*
import com.vanillasource.parser.ParserResult._
import com.vanillasource.parser.Stream.*

class ParserTest extends AnyFlatSpec with Matchers {
  "end-of-input parser" should "match end-of-input" in {
    val p = endOfInput[Char]()

    p.runA(Stream.ofString("")) shouldBe Success(())
  }

  it should "return error if stream did not reach end-of-input" in {
    val p = endOfInput[Char]()

    p.runA(Stream.ofString("abc")) shouldBe Error("end of input")
  }
}
