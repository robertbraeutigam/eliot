package com.vanillasource.eliot.eliotc.processor.common

import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.ProcessorTest
import com.vanillasource.eliot.eliotc.processor.ProcessorTest.*

class SingleFactTypeProcessorTest extends ProcessorTest {
  val processor = new TestSingleFactTypeProcessor()

  "single fact processor" should "generate fact when key type matches" in {
    val key = TestFactKey("test-value")

    runCompilerIO {
      for {
        _    <- processor.generate(key)
        fact <- getFactOrAbort(key)
      } yield fact
    }.asserting(_ shouldBe Right(TestFact("test-value")))
  }

  it should "do nothing when key type does not match" in {
    given process: TestCompilationProcess = new TestCompilationProcess()

    runCompilerIO {
      processor.generate(DifferentKey("other-value"))
    }.asserting { _ => process.facts shouldBe empty }
  }

  it should "provide correctly typed key to generateFact method" in {
    val typedKey = TestFactKey("typed-test")

    runCompilerIO {
      for {
        _    <- processor.generate(typedKey)
        fact <- getFactOrAbort(typedKey)
      } yield fact
    }.asserting(_ shouldBe Right(TestFact("typed-test")))
  }

  it should "not register fact when errors are present" in {
    given process: TestCompilationProcess = new TestCompilationProcess()
    val key = TestFactKey("error-test")

    runCompilerIO {
      for {
        _ <- registerCompilerError(error("test error"))
        _ <- processor.generate(key)
      } yield ()
    }.asserting { _ => process.facts shouldBe empty }
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
      case Left(_) => fail("Expected Right but got Left")
    }
  }

  class TestSingleFactTypeProcessor extends SingleFactTypeProcessor[TestFact, TestFactKey] {
    override protected def generateFact(key: TestFactKey): CompilerIO[Unit] =
      registerFactIfClear(TestFact(key.value))
  }
}
