package com.vanillasource.eliot.eliotc.typesystem

import cats.Eval
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.typesystem.ShortUniqueIdentifiers.generateNextUniqueIdentifier
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ShortUniqueIdentifiersTest extends AnyFlatSpec with Matchers {
  "name generator" should "generate 'A' first" in {
    ShortUniqueIdentifiers().generateCurrentIdentifier() shouldBe "A$"
  }

  it should "generate 'B' second" in {
    ShortUniqueIdentifiers(1).generateCurrentIdentifier() shouldBe "B$"
  }

  it should "generate 'Z' as 26th" in {
    generateNextUniqueIdentifier[Eval]().replicateA(26).runA(ShortUniqueIdentifiers()).value.last shouldBe "Z$"
  }

  it should "generate 'AA' as 27th" in {
    generateNextUniqueIdentifier[Eval]().replicateA(27).runA(ShortUniqueIdentifiers()).value.last shouldBe "AA$"
  }

  it should "generate 'AB' as 28th" in {
    generateNextUniqueIdentifier[Eval]().replicateA(28).runA(ShortUniqueIdentifiers()).value.last shouldBe "AB$"
  }
}
