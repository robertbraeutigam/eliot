package com.vanillasource.eliot.eliotc.processor.common

import cats.data.Chain
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.ProcessorTest
import com.vanillasource.eliot.eliotc.processor.ProcessorTest.*

class SingleKeyTypeProcessorTest extends ProcessorTest {
  val processor = new TestSingleKeyTypeProcessor()
  val testKey   = TestFactKey("test")

  "single fact processor" should "generate fact when key type matches" in {
    runCompilerIO {
      for {
        _    <- processor.generate(testKey)
        fact <- getFactOrAbort(testKey)
      } yield fact
    }.asserting(_ shouldBe Right(TestFact("test")))
  }

  it should "do nothing when key type does not match" in {
    val otherKey = DifferentKey("other-value")

    runCompilerIO {
      for {
        _    <- processor.generate(otherKey)
        fact <- getFactOrAbort(otherKey)
      } yield fact
    }.asserting(_ shouldBe Left(Chain.empty))
  }

  it should "handle multiple different keys correctly" in {
    val key1 = TestFactKey("key1")
    val key2 = TestFactKey("key2")
    val key3 = DifferentKey("key3")

    runCompilerIO {
      for {
        _     <- processor.generate(key1)
        _     <- processor.generate(key2)
        _     <- processor.generate(key3)
        fact1 <- getFactOrAbort(key1)
        fact2 <- getFactOrAbort(key2)
      } yield (fact1, fact2)
    }.asserting {
      case Right((fact1, fact2)) =>
        val _ = fact1 shouldBe TestFact("key1")
        fact2 shouldBe TestFact("key2")
      case Left(_)               => fail("Expected Right but got Left")
    }
  }

  class TestSingleKeyTypeProcessor extends SingleKeyTypeProcessor[TestFactKey] {
    override protected def generateFact(key: TestFactKey): CompilerIO[Unit] =
      registerFactIfClear(TestFact(key.value))
  }
}
