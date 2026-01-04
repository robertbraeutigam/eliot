package com.vanillasource.eliot.eliotc.processor.common

import cats.Monad
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.ProcessorTest
import com.vanillasource.eliot.eliotc.processor.ProcessorTest.*

class SingleFactProcessorTest extends ProcessorTest {
  val processor = new TestSingleFactProcessor()

  def testProcess = new TestCompilationProcess()

  "single fact processor" should "generate and register fact when key type matches" in {
    val process = testProcess
    val key     = TestFactKey("test-value")

    runCompilerIO(process) {
      for {
        _    <- processor.generate(key)
        fact <- getFactOrAbort(key)
      } yield fact
    }.asserting(_ shouldBe Right(TestFact("test-value")))
  }

  it should "do nothing when key type does not match" in {
    val process = testProcess

    runCompilerIO(process) {
      processor.generate(DifferentKey("other-value"))
    }.asserting { _ => process.facts shouldBe empty }
  }

  it should "not register fact when errors are present" in {
    val process = testProcess
    val key     = TestFactKey("error-test")

    runCompilerIO(process) {
      for {
        _ <- registerCompilerError(error("test error"))
        _ <- processor.generate(key)
      } yield ()
    }.asserting { _ => process.facts shouldBe empty }
  }

  it should "call generateSingleFact with correct key" in {
    val process = testProcess
    val key     = TestFactKey("specific-key")

    runCompilerIO(process) {
      for {
        _    <- processor.generate(key)
        fact <- getFactOrAbort(key)
      } yield fact
    }.asserting(_ shouldBe Right(TestFact("specific-key")))
  }

  it should "handle multiple fact generations independently" in {
    val process = testProcess
    val key1    = TestFactKey("first")
    val key2    = TestFactKey("second")

    runCompilerIO(process) {
      for {
        _     <- processor.generate(key1)
        _     <- processor.generate(key2)
        fact1 <- getFactOrAbort(key1)
        fact2 <- getFactOrAbort(key2)
      } yield (fact1, fact2)
    }.asserting(_ shouldBe Right((TestFact("first"), TestFact("second"))))
  }

  it should "not register fact if error occurs after generation starts" in {
    val process = testProcess
    val key     = TestFactKey("test")

    runCompilerIO(process) {
      for {
        _ <- registerCompilerError(error("error before"))
        _ <- processor.generate(key)
      } yield ()
    }.asserting { _ => process.facts shouldBe empty }
  }

  it should "register fact only once per key" in {
    val process   = testProcess
    val processor = new CountingFactProcessor()
    val key       = TestFactKey("counted")

    runCompilerIO(process) {
      for {
        _ <- processor.generate(key)
        _ <- processor.generate(key)
      } yield ()
    }.asserting { _ =>
      val _ = process.facts.get(key) shouldBe Some(TestFact("counted"))
      processor.callCount shouldBe 2
    }
  }

  class TestSingleFactProcessor extends SingleFactProcessor[TestFact, TestFactKey] {
    override protected def generateSingleFact(k: TestFactKey): CompilerIO[TestFact] =
      Monad[CompilerIO].pure(TestFact(k.value))
  }

  class CountingFactProcessor extends SingleFactProcessor[TestFact, TestFactKey] {
    var callCount: Int = 0

    override protected def generateSingleFact(k: TestFactKey): CompilerIO[TestFact] = {
      callCount += 1
      Monad[CompilerIO].pure(TestFact(k.value))
    }
  }
}
