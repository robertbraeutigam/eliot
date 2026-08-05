package com.vanillasource.eliot.eliotc.compiler.cache

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class CacheFingerprintTest extends AnyFlatSpec with Matchers {
  private val base = Seq("-o", "target", "--path", "stdlib/eliot", "-m", "Main")

  "config" should "be unchanged by the --statistics diagnostic flag" in {
    CacheFingerprint.config(base :+ "--statistics") shouldBe CacheFingerprint.config(base)
  }

  it should "be unchanged by --visualize-facts and its path argument" in {
    CacheFingerprint.config(base ++ Seq("--visualize-facts", "out.html")) shouldBe CacheFingerprint.config(base)
  }

  it should "be unchanged by the --visualize-facts=path form" in {
    CacheFingerprint.config(base :+ "--visualize-facts=out.html") shouldBe CacheFingerprint.config(base)
  }

  it should "still differ when a fact-affecting argument changes" in {
    CacheFingerprint.config(base) should not be CacheFingerprint.config(Seq("-o", "target", "-m", "Other"))
  }
}
