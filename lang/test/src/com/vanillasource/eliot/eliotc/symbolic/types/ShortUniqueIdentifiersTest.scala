package com.vanillasource.eliot.eliotc.symbolic.types

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ShortUniqueIdentifiersTest extends AnyFlatSpec with Matchers {

  "generateNext" should "generate A$ as first identifier" in {
    ShortUniqueIdentifiers().generateNext()._1 shouldBe "A$"
  }

  it should "generate B$ as second identifier" in {
    val (_, ids) = ShortUniqueIdentifiers().generateNext()
    ids.generateNext()._1 shouldBe "B$"
  }

  it should "generate Z$ as 26th identifier" in {
    ShortUniqueIdentifiers(25).generateNext()._1 shouldBe "Z$"
  }

  it should "generate AA$ as 27th identifier" in {
    ShortUniqueIdentifiers(26).generateNext()._1 shouldBe "AA$"
  }

  it should "generate AZ$ as 52nd identifier" in {
    ShortUniqueIdentifiers(51).generateNext()._1 shouldBe "AZ$"
  }

  it should "generate BA$ as 53rd identifier" in {
    ShortUniqueIdentifiers(52).generateNext()._1 shouldBe "BA$"
  }

  it should "generate all unique identifiers for first 100" in {
    val ids = Iterator.iterate(ShortUniqueIdentifiers())(_.generateNext()._2).take(100).map(_.generateNext()._1).toSeq
    ids.distinct should have size 100
  }
}
