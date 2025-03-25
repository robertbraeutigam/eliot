package com.vanillasource.eliot.eliotc.typesystem

import cats.Eval
import com.vanillasource.eliot.eliotc.typesystem.UniqueGenericNames.reserveNextName
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.syntax.all.*

class UniqueGenericNamesTest extends AnyFlatSpec with Matchers {
  "name generator" should "generate 'A' first" in {
    UniqueGenericNames().reserveNextName()._2 shouldBe "A"
  }

  it should "generate 'B' second" in {
    UniqueGenericNames().reserveNextName()._1.reserveNextName()._2 shouldBe "B"
  }

  it should "generate 'Z' as 26th" in {
    reserveNextName[Eval]().replicateA(26).runA(UniqueGenericNames()).value.last shouldBe "Z"
  }

  it should "generate 'AA' as 27th" in {
    reserveNextName[Eval]().replicateA(27).runA(UniqueGenericNames()).value.last shouldBe "AA"
  }

  it should "generate 'AB' as 28th" in {
    reserveNextName[Eval]().replicateA(28).runA(UniqueGenericNames()).value.last shouldBe "AB"
  }
}
