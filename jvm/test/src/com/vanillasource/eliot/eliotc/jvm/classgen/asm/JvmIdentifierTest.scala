package com.vanillasource.eliot.eliotc.jvm.classgen.asm

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class JvmIdentifierTest extends AnyFlatSpec with Matchers {

  "JvmIdentifier.encode" should "pass through simple alphanumeric names" in {
    JvmIdentifier.encode("hello").value shouldBe "hello"
  }

  it should "encode dot to named encoding" in {
    JvmIdentifier.encode(".").value shouldBe "_dot_"
  }

  it should "encode plus to named encoding" in {
    JvmIdentifier.encode("+").value shouldBe "_plus_"
  }

  it should "encode underscore to named encoding" in {
    JvmIdentifier.encode("_").value shouldBe "_us_"
  }

  it should "leave dollar sign unchanged" in {
    JvmIdentifier.encode("$").value shouldBe "$"
  }

  it should "encode mixed names correctly" in {
    JvmIdentifier.encode("a.b_c+d").value shouldBe "a_dot_b_us_c_plus_d"
  }

  it should "encode semicolon to hex" in {
    JvmIdentifier.encode(";").value shouldBe "_003B_"
  }

  it should "encode bracket to hex" in {
    JvmIdentifier.encode("[").value shouldBe "_005B_"
  }

  it should "encode slash to named encoding" in {
    JvmIdentifier.encode("/").value shouldBe "_slash_"
  }

  it should "pass through digits" in {
    JvmIdentifier.encode("x123").value shouldBe "x123"
  }
}
