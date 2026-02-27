package com.vanillasource.eliot.eliotc.jvm.classgen.asm

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class JvmIdentifierTest extends AnyFlatSpec with Matchers {

  "JvmIdentifier.encode" should "pass through simple alphanumeric names" in {
    JvmIdentifier.encode("hello").value shouldBe "hello"
  }

  it should "encode dot to _002E_" in {
    JvmIdentifier.encode(".").value shouldBe "_002E_"
  }

  it should "encode plus to _002B_" in {
    JvmIdentifier.encode("+").value shouldBe "_002B_"
  }

  it should "encode underscore to _005F_" in {
    JvmIdentifier.encode("_").value shouldBe "_005F_"
  }

  it should "leave dollar sign unchanged" in {
    JvmIdentifier.encode("$").value shouldBe "$"
  }

  it should "encode mixed names correctly" in {
    JvmIdentifier.encode("a.b_c+d").value shouldBe "a_002E_b_005F_c_002B_d"
  }

  it should "encode semicolon to _003B_" in {
    JvmIdentifier.encode(";").value shouldBe "_003B_"
  }

  it should "encode bracket to _005B_" in {
    JvmIdentifier.encode("[").value shouldBe "_005B_"
  }

  it should "encode slash to _002F_" in {
    JvmIdentifier.encode("/").value shouldBe "_002F_"
  }

  it should "pass through digits" in {
    JvmIdentifier.encode("x123").value shouldBe "x123"
  }
}
