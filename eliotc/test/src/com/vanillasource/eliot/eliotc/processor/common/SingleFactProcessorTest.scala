package com.vanillasource.eliot.eliotc.processor.common

import cats.Monad
import cats.data.Chain
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.feedback.CompilerError
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.{CompilationProcess, CompilerFact, CompilerFactKey}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

class SingleFactProcessorTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  "SingleFactProcessor" should "generate fact when key type matches" in {
    val process   = new TestCompilationProcess()
    val processor = new TestSingleFactProcessor()
    val key       = TestKey("test-value")

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
    val key       = TestKey("typed-test")

    runCompilerIO(process) {
      processor.generate(key)
    }.asserting { _ =>
      val fact = process.facts.get(key)
      fact shouldBe defined
      fact.get shouldBe TestFact("typed-test")
    }
  }

  it should "not register fact when errors are present" in {
    val process   = new TestCompilationProcess()
    val processor = new TestSingleFactProcessor()
    val key       = TestKey("error-test")

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
    val key1      = TestKey("key1")
    val key2      = TestKey("key2")
    val key3      = DifferentKey("key3")

    runCompilerIO(process) {
      for {
        _ <- processor.generate(key1)
        _ <- processor.generate(key2)
        _ <- processor.generate(key3)
      } yield ()
    }.asserting { _ =>
      process.facts.values should contain(TestFact("key1"))
      process.facts.values should contain(TestFact("key2"))
      process.facts.values should not contain DifferentFact("key3")
    }
  }

  private def runCompilerIO[T](
      process: CompilationProcess
  )(value: CompilerIO[T]): IO[Either[Chain[CompilerError], T]] =
    value.run(process).run(Chain.empty).value.map {
      case Left(errors)  => Left(errors)
      case Right((_, t)) => Right(t)
    }

  // Test fixtures
  case class TestFact(value: String) extends CompilerFact {
    override def key(): CompilerFactKey[TestFact] = TestKey(value)
  }

  case class TestKey(value: String) extends CompilerFactKey[TestFact]

  case class DifferentFact(value: String) extends CompilerFact {
    override def key(): CompilerFactKey[DifferentFact] = DifferentKey(value)
  }

  case class DifferentKey(value: String) extends CompilerFactKey[DifferentFact]

  class TestSingleFactProcessor extends SingleFactProcessor[TestFact, TestKey] {
    override protected def generateFact(key: TestKey): CompilerIO[Unit] =
      registerFactIfClear(TestFact(key.value))
  }

  class TestCompilationProcess extends CompilationProcess {
    var facts: Map[CompilerFactKey[?], CompilerFact] = Map.empty

    def registerFactSync(fact: CompilerFact): Unit = {
      facts = facts.updated(fact.key(), fact)
    }

    override def getFact[V <: CompilerFact, K <: CompilerFactKey[V]](key: K): IO[Option[V]] =
      IO.pure(facts.get(key).map(_.asInstanceOf[V]))

    override def registerFact(value: CompilerFact): IO[Unit] =
      IO { registerFactSync(value) }
  }
}
