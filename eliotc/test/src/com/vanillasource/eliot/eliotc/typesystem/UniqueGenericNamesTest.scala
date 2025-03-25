package com.vanillasource.eliot.eliotc.typesystem

import cats.Eval
import com.vanillasource.eliot.eliotc.typesystem.UniqueGenericNames.generateNextUniqueName
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.syntax.all.*

class UniqueGenericNamesTest extends AnyFlatSpec with Matchers {
  "name generator" should "generate 'A' first" in {
    UniqueGenericNames().generateCurrentName() shouldBe "A"
  }

  it should "generate 'B' second" in {
    UniqueGenericNames(1).generateCurrentName() shouldBe "B"
  }

  it should "generate 'Z' as 26th" in {
    generateNextUniqueName[Eval]().replicateA(26).runA(UniqueGenericNames()).value.last shouldBe "Z"
  }

  it should "generate 'AA' as 27th" in {
    generateNextUniqueName[Eval]().replicateA(27).runA(UniqueGenericNames()).value.last shouldBe "AA"
  }

  it should "generate 'AB' as 28th" in {
    generateNextUniqueName[Eval]().replicateA(28).runA(UniqueGenericNames()).value.last shouldBe "AB"
  }
}
