package com.vanillasource.eliot.eliotc.avr

import org.scalatest.flatspec.AsyncFlatSpec
import com.vanillasource.eliot.eliotc.avr.Register.*
import com.vanillasource.eliot.eliotc.avr.AVRInstruction.*
import com.vanillasource.eliot.eliotc.module.{FunctionFQN, ModuleName}
import org.scalatest.matchers.should.Matchers
import cats.syntax.all.*

class AVRInstructionTest extends AsyncFlatSpec with Matchers {
  val someLabel = FunctionFQN(ModuleName(Seq("a", "b"), "C"), "f")

  "push" should "generate right register code" in {
    push(R16).generateBytes() shouldBe Array(0x0f.toByte, 0x93.toByte)
  }

  "ret" should "generate fixed bytes" in {
    ret().generateBytes() shouldBe Array(0x08.toByte, 0x95.toByte)
  }

  "rcall" should "generate 0 offset if label follows" in {
    (rcall(someLabel) |+| label(someLabel)).generateBytes() shouldBe Array(0x00.toByte, 0xd0.toByte)
  }

  "rcall" should "generate offset 1 of label is for one instruction further" in {
    (rcall(someLabel) |+| ret() |+| label(someLabel))
      .generateBytes() shouldBe Array(0x01.toByte, 0xd0.toByte, 0x08.toByte, 0x95.toByte)
  }

  "rcall" should "generate negative 1 offset is label points to rcall instruction" in {
    (label(someLabel) |+| rcall(someLabel)).generateBytes() shouldBe Array(0xff.toByte, 0xdf.toByte)
  }
}
