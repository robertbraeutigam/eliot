package com.vanillasource.eliot.eliotc.avr

import org.scalatest.flatspec.AsyncFlatSpec
import com.vanillasource.eliot.eliotc.avr.Register.*
import com.vanillasource.eliot.eliotc.avr.AVRInstruction.*
import org.scalatest.matchers.should.Matchers

class AVRInstructionTest extends AsyncFlatSpec with Matchers {
  "push" should "generate right register code" in {
    push(R16).generateBytes() shouldBe Array(0x0f.toByte, 0xd1.toByte)
  }
}
