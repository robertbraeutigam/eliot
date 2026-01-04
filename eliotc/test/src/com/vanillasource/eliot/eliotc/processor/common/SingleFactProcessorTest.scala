package com.vanillasource.eliot.eliotc.processor.common

import com.vanillasource.eliot.eliotc.feedback.CompilerError
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.ProcessorTest.*
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey, ProcessorTest}

class SingleFactProcessorTest extends ProcessorTest {
  "single fact processor" should "generate fact when key type matches" in {
    val process   = new TestCompilationProcess()
    val processor = new TestSingleFactProcessor()
    val key       = TestFactKey("test-value")

    runCompilerIO(process) {
      processor.generate(key)
    }.asserting { _ => process.facts.values should contain(TestFact("test-value")) }
  }

  it should "do nothing when key type does not match" in {
    val process      = new TestCompilationProcess()
    val processor    = new TestSingleFactProcessor()
    val differentKey = DifferentKey("other-value")

    runCompilerIO(process) {
      processor.generate(differentKey)
    }.asserting { _ => process.facts shouldBe empty }
  }

  it should "provide correctly typed key to generateFact method" in {
    val process   = new TestCompilationProcess()
    val processor = new TestSingleFactProcessor()
    val key       = TestFactKey("typed-test")

    runCompilerIO(process) {
      processor.generate(key)
    }.asserting { _ => process.facts.get(key) shouldBe Some(TestFact("typed-test")) }
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

  it should "handle multiple different keys correctly" in {
    val process   = new TestCompilationProcess()
    val processor = new TestSingleFactProcessor()
    val key1      = TestFactKey("key1")
    val key2      = TestFactKey("key2")
    val key3      = DifferentKey("key3")

    runCompilerIO(process) {
      for {
        _ <- processor.generate(key1)
        _ <- processor.generate(key2)
        _ <- processor.generate(key3)
      } yield ()
    }.asserting { _ =>
      val _ = process.facts.values should contain(TestFact("key1"))
      val _ = process.facts.values should contain(TestFact("key2"))
      process.facts.values should not contain DifferentFact("key3")
    }
  }

  // Test-specific fixtures
  case class DifferentFact(value: String) extends CompilerFact {
    override def key(): CompilerFactKey[DifferentFact] = DifferentKey(value)
  }

  case class DifferentKey(value: String) extends CompilerFactKey[DifferentFact]

  class TestSingleFactProcessor extends SingleFactProcessor[TestFact, TestFactKey] {
    override protected def generateFact(key: TestFactKey): CompilerIO[Unit] =
      registerFactIfClear(TestFact(key.value))
  }
}
