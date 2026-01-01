package com.vanillasource.eliot.eliotc.processor

import cats.data.Chain
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.pos.{PositionRange, Sourced}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

class CompilerIOTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  private val testFile                 = new File("test.el")
  private val testRange                = PositionRange.zero
  private def testSourced(msg: String) = Sourced(testFile, testRange, msg)

  "context" should "be clear if nothing yet happened" in {
    runCompilerIO {
      isClear
    }.asserting(_ shouldBe Some(true))
  }

  it should "not be clear after an error is registered" in {
    runCompilerIO {
      for {
        _     <- compilerError(testSourced("error"))
        clear <- isClear
      } yield clear
    }.asserting(_ shouldBe Some(false))
  }

  "current errors" should "return the only error registered" in {
    runCompilerIO {
      for {
        _      <- compilerError(testSourced("test error"))
        errors <- currentErrors
      } yield errors
    }.asserting(_.map(_.toList.map(_.message.value)) shouldBe Some(Seq("test error")))
  }

  it should "return empty chain when context is clear" in {
    runCompilerIO {
      currentErrors
    }.asserting(_ shouldBe Some(Chain.empty))
  }

  it should "contain all errors registered previously" in {
    runCompilerIO {
      for {
        _      <- compilerError(testSourced("error 1"))
        _      <- compilerError(testSourced("error 2"))
        _      <- compilerError(testSourced("error 3"))
        errors <- currentErrors
      } yield errors
    }.asserting(_.map(_.toList.map(_.message.value)) shouldBe Some(Seq("error 1", "error 2", "error 3")))
  }

  "getting a fact" should "return a fact when available" in {
    val process  = new TestCompilationProcess()
    val testFact = TestFact("test")
    process.registerFactSync(testFact)

    runCompilerIOWithProcess(process) {
      getFactOrAbort(TestFactKey("test"))
    }.asserting(_ shouldBe Some(testFact))
  }

  it should "short circuit when fact is not available" in {
    val process = new TestCompilationProcess()

    runCompilerIOWithProcess(process) {
      getFactOrAbort(TestFactKey("test"))
    }.asserting(_ shouldBe None)
  }

  "registering a fact when clear" should "register fact when there are no errors" in {
    val process  = new TestCompilationProcess()
    val testFact = TestFact("test")

    runCompilerIOWithProcess(process) {
      registerFactIfClear(testFact)
    }.asserting { _ => process.facts should contain(testFact) }
  }

  it should "not register fact when there are errors" in {
    val process  = new TestCompilationProcess()
    val testFact = TestFact("test")

    runCompilerIOWithProcess(process) {
      for {
        _ <- compilerError(testSourced("error"))
        _ <- registerFactIfClear(testFact)
      } yield ()
    }.asserting { _ => process.facts should not contain testFact }
  }

  private def runCompilerIO[T](value: CompilerIO[T]): IO[Option[T]] =
    runCompilerIOWithProcess(null)(value)

  private def runCompilerIOWithProcess[T](process: CompilationProcess)(value: CompilerIO[T]): IO[Option[T]] =
    value.run(process).run(Chain.empty).value.map {
      case Left(_)       => None
      case Right((_, t)) => Some(t)
    }

  // Test fixtures
  case class TestFact(value: String) extends CompilerFact {
    override def key(): CompilerFactKey[TestFact] = TestFactKey(value)
  }

  case class TestFactKey(value: String) extends CompilerFactKey[TestFact]

  class TestCompilationProcess extends CompilationProcess {
    var facts: List[CompilerFact] = List.empty

    def registerFactSync(fact: CompilerFact): Unit = {
      facts = facts :+ fact
    }

    override def getFact[V <: CompilerFact, K <: CompilerFactKey[V]](key: K): IO[Option[V]] =
      IO.pure(facts.collectFirst { case f: V => f })

    override def registerFact(value: CompilerFact): IO[Unit] =
      IO { registerFactSync(value) }
  }
}
