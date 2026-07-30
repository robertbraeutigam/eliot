package com.vanillasource.eliot.eliotc.processor

import com.vanillasource.eliot.eliotc.processor.ProcessorTest.{DifferentKey, TestFactKey}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.runtime.ScalaRunTime

class CompilerFactKeyTest extends AnyFlatSpec with Matchers {

  /** The whole point of the exercise, and the part that can silently regress: if a key ever synthesizes its own case
    * class hash, it overrides the cached one and every lookup walks the key's structure again. Observed through the
    * cache field itself — it holds zero until the first hash is asked for, and the answer afterwards.
    */
  "a fact key" should "not have computed its hash before it is first asked for one" in {
    cachedHashCodeOf(TestFactKey("fresh")) shouldBe 0
  }

  it should "remember the hash it computed, so it is computed once per key" in {
    val key = TestFactKey("remembered")
    val hash = key.hashCode()

    cachedHashCodeOf(key) shouldBe hash
  }

  it should "hash exactly as the synthesized case class hash would" in {
    TestFactKey("some-key").hashCode() shouldBe ScalaRunTime._hashCode(TestFactKey("some-key"))
  }

  it should "give equal keys equal hashes" in {
    TestFactKey("same").hashCode() shouldBe TestFactKey("same").hashCode()
  }

  it should "give different keys different hashes" in {
    TestFactKey("one").hashCode() should not be TestFactKey("two").hashCode()
  }

  it should "distinguish keys of different types carrying the same value" in {
    TestFactKey("x").hashCode() should not be DifferentKey("x").hashCode()
  }

  it should "return the same hash on every call" in {
    val key = TestFactKey("repeated")

    key.hashCode() shouldBe key.hashCode()
  }

  it should "still compare equal by value" in {
    TestFactKey("value") shouldBe TestFactKey("value")
  }

  it should "still compare unequal to a different value" in {
    TestFactKey("value") should not be TestFactKey("other")
  }

  /** Reads the trait's private memo field, whose name is mangled into the implementing class. */
  private def cachedHashCodeOf(key: CompilerFactKey[?]): Int = {
    val field = key.getClass.getDeclaredField("com$vanillasource$eliot$eliotc$processor$CompilerFactKey$$cachedHashCode")
    field.setAccessible(true)

    field.getInt(key)
  }
}
