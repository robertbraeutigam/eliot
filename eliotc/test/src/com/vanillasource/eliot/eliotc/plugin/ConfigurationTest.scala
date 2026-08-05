package com.vanillasource.eliot.eliotc.plugin

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ConfigurationTest extends AnyFlatSpec with Matchers {
  "identityContributions" should "include a namedKey value rendered as its toString" in {
    Configuration().set(Configuration.namedKey[Int]("n"), 42).identityContributions shouldBe Seq("n" -> "42")
  }

  it should "exclude a diagnosticKey value" in {
    Configuration().set(Configuration.diagnosticKey[Unit]("stats"), ()).identityContributions shouldBe empty
  }

  it should "exclude an opaqueKey value" in {
    Configuration().set(Configuration.opaqueKey[String]("fn"), "x").identityContributions shouldBe empty
  }

  it should "render a namedKey value through its custom serializer" in {
    Configuration()
      .set(Configuration.namedKey[String]("k")(_.toUpperCase), "abc")
      .identityContributions shouldBe Seq("k" -> "ABC")
  }
}
