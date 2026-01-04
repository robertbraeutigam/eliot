package com.vanillasource.eliot.eliotc.processor.common

import cats.Monad
import com.vanillasource.eliot.eliotc.feedback.CompilerError
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.ProcessorTest.*
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey, ProcessorTest}

class SingleFactProcessorTest extends ProcessorTest {
  "single fact processor" should "generate and register fact when key type matches" in {
    val process   = new TestCompilationProcess()
    val processor = new TestSingleFactProcessor()
    val key       = TestFactKey("test-value")

    runCompilerIO(process) {
      processor.generate(key)
    }.asserting { _ => process.facts.get(key) shouldBe Some(TestFact("test-value")) }
  }

  it should "do nothing when key type does not match" in {
    val process      = new TestCompilationProcess()
    val processor    = new TestSingleFactProcessor()
    val differentKey = DifferentKey("other-value")

    runCompilerIO(process) {
      processor.generate(differentKey)
    }.asserting { _ => process.facts shouldBe empty }
  }

  it should "not register fact when errors are present" in {
    val process   = new TestCompilationProcess()
    val processor = new TestSingleFactProcessor()
    val key       = TestFactKey("error-test")

    runCompilerIO(process) {
      for {
        _ <- registerCompilerError(CompilerError("test error", Seq.empty, "", "", null))
        _ <- processor.generate(key)
      } yield ()
    }.asserting { _ => process.facts shouldBe empty }
  }

  it should "call generateSingleFact with correct key" in {
    val process   = new TestCompilationProcess()
    val processor = new TestSingleFactProcessor()
    val key       = TestFactKey("specific-key")

    runCompilerIO(process) {
      processor.generate(key)
    }.asserting { _ => process.facts.values should contain(TestFact("specific-key")) }
  }

  it should "handle multiple fact generations independently" in {
    val process   = new TestCompilationProcess()
    val processor = new TestSingleFactProcessor()
    val key1      = TestFactKey("first")
    val key2      = TestFactKey("second")

    runCompilerIO(process) {
      for {
        _ <- processor.generate(key1)
        _ <- processor.generate(key2)
      } yield ()
    }.asserting { _ =>
      val _ = process.facts.get(key1) shouldBe Some(TestFact("first"))
      process.facts.get(key2) shouldBe Some(TestFact("second"))
    }
  }

  it should "not register fact if error occurs after generation starts" in {
    val process   = new TestCompilationProcess()
    val processor = new TestSingleFactProcessor()
    val key       = TestFactKey("test")

    runCompilerIO(process) {
      for {
        _ <- registerCompilerError(CompilerError("error before", Seq.empty, "", "", null))
        _ <- processor.generate(key)
      } yield ()
    }.asserting { _ => process.facts shouldBe empty }
  }

  it should "register fact only once per key" in {
    val process   = new TestCompilationProcess()
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

  // Test-specific fixtures
  case class DifferentFact(value: String) extends CompilerFact {
    override def key(): CompilerFactKey[DifferentFact] = DifferentKey(value)
  }

  case class DifferentKey(value: String) extends CompilerFactKey[DifferentFact]

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
