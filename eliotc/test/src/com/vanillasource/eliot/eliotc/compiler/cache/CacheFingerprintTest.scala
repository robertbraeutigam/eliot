package com.vanillasource.eliot.eliotc.compiler.cache

import com.vanillasource.eliot.eliotc.compiler.Compiler
import com.vanillasource.eliot.eliotc.plugin.Configuration
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.nio.file.Path

class CacheFingerprintTest extends AnyFlatSpec with Matchers {
  private val mainKey = Configuration.namedKey[String]("main")
  private val base    = Configuration()
    .set(Compiler.targetPathKey, Path.of("target"))
    .set(mainKey, "Main")

  "config" should "be unchanged by the --statistics diagnostic flag" in {
    CacheFingerprint.config(base.set(Compiler.statisticsKey, ())) shouldBe CacheFingerprint.config(base)
  }

  it should "be unchanged by the --visualize-facts diagnostic flag" in {
    CacheFingerprint.config(base.set(Compiler.visualizeFactsKey, Path.of("out.html"))) shouldBe
      CacheFingerprint.config(base)
  }

  it should "be unchanged by an opaque, unrepresentable configuration value" in {
    val factoryKey = Configuration.opaqueKey[Path => String]("mountFactory")
    CacheFingerprint.config(base.set(factoryKey, (p: Path) => p.toString)) shouldBe CacheFingerprint.config(base)
  }

  it should "be unchanged by a demand-scoped key (e.g. the selected main), so its caches share one file" in {
    val selectedMain = Configuration.demandScopedKey[String]("mainFunction")
    CacheFingerprint.config(base.set(selectedMain, "HelloWorld")) shouldBe
      CacheFingerprint.config(base.set(selectedMain, "Calculator"))
  }

  it should "still differ when a fact-affecting argument changes" in {
    CacheFingerprint.config(base.set(mainKey, "Other")) should not be CacheFingerprint.config(base)
  }

  it should "be independent of the order values were set in" in {
    val a = Configuration().set(Compiler.targetPathKey, Path.of("target")).set(mainKey, "Main")
    val b = Configuration().set(mainKey, "Main").set(Compiler.targetPathKey, Path.of("target"))
    CacheFingerprint.config(a) shouldBe CacheFingerprint.config(b)
  }
}
