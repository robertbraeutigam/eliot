package com.vanillasource.eliot.eliotc.processor.common

import cats.Monad
import cats.data.Chain
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.ProcessorTest
import com.vanillasource.eliot.eliotc.processor.ProcessorTest.*

class SingleFactProcessorTest extends ProcessorTest {
  val processor = new TestSingleFactProcessor()
  val testKey   = TestFactKey("test")

  "single fact processor" should "generate and register fact when key type matches" in {
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

  it should "not register fact when errors are present" in {
    runCompilerIO {
      for {
        _    <- registerCompilerError(error("test error"))
        _    <- processor.generate(testKey)
        fact <- getFactOrAbort(testKey)
      } yield ()
    }.asserting(_ shouldBe Left(Chain.one(error("test error"))))
  }

  it should "handle multiple fact generations independently" in {
    val key1 = TestFactKey("first")
    val key2 = TestFactKey("second")

    runCompilerIO {
      for {
        _     <- processor.generate(key1)
        _     <- processor.generate(key2)
        fact1 <- getFactOrAbort(key1)
        fact2 <- getFactOrAbort(key2)
      } yield (fact1, fact2)
    }.asserting(_ shouldBe Right((TestFact("first"), TestFact("second"))))
  }

  class TestSingleFactProcessor extends SingleFactProcessor[TestFactKey] {
    override protected def generateSingleFact(k: TestFactKey): CompilerIO[TestFact] =
      Monad[CompilerIO].pure(TestFact(k.value))
  }

  class CountingFactProcessor extends SingleFactProcessor[TestFactKey] {
    var callCount: Int = 0

    override protected def generateSingleFact(k: TestFactKey): CompilerIO[TestFact] = {
      callCount += 1
      Monad[CompilerIO].pure(TestFact(k.value))
    }
  }
}
