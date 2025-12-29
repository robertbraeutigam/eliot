package com.vanillasource.eliot.eliotc.processor

import cats.data.Chain
import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.pos.{PositionRange, Sourced}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import java.io.File

// FIXME: fix all of these tests
class CompilerIOTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  "isClear" should "be true if nothing yet happened" in {
    runCompilerIO {
      isClear
    }.asserting(_ shouldBe Some(true))
  }

  it should "be false after an error is registered" in {
    runCompilerIO {
      for {
        _     <- compilerError(testSourced("error"))
        clear <- isClear
      } yield clear
    }.asserting(_ shouldBe Some(false))
  }

  "compilerError" should "register an error with just a message" in {
    runCompilerIO {
      for {
        _      <- compilerError(testSourced("test error"))
        errors <- currentErrors
      } yield errors
    }.asserting { result =>
      result shouldBe defined
      result.get.size shouldBe 1
      result.head.headOption.get.message.value shouldBe "test error"
      result.head.headOption.get.description shouldBe Seq.empty
    }
  }

  it should "register an error with a message and description" in {
    runCompilerIO {
      for {
        _      <- compilerError(testSourced("test error"), Seq("line 1", "line 2"))
        errors <- currentErrors
      } yield errors
    }.asserting { result =>
      result shouldBe defined
      result.get.size shouldBe 1
      result.head.headOption.get.message.value shouldBe "test error"
      result.head.headOption.get.description shouldBe Seq("line 1", "line 2")
    }
  }

  "currentErrors" should "return empty chain when no errors" in {
    runCompilerIO {
      currentErrors
    }.asserting { result =>
      result shouldBe Some(Chain.empty)
    }
  }

  it should "accumulate multiple errors" in {
    runCompilerIO {
      for {
        _      <- compilerError(testSourced("error 1"))
        _      <- compilerError(testSourced("error 2"))
        _      <- compilerError(testSourced("error 3"))
        errors <- currentErrors
      } yield errors
    }.asserting { result =>
      result shouldBe defined
      result.get.size shouldBe 3
      result.get.toList.map(_.message.value) shouldBe List("error 1", "error 2", "error 3")
    }
  }

  "getFact" should "return fact when available" in {
    val process  = new TestCompilationProcess()
    val testFact = TestFact("test")
    process.registerFactSync(testFact)

    runCompilerIOWithProcess(process) {
      getFactOrAbort(TestFactKey)
    }.asserting(_ shouldBe Some(testFact))
  }

  it should "short circuit when fact is not available" in {
    val process = new TestCompilationProcess()

    runCompilerIOWithProcess(process) {
      getFactOrAbort(TestFactKey)
    }.asserting(_ shouldBe None)
  }

  it should "short circuit when there are accumulated errors" in {
    val process = new TestCompilationProcess()

    runCompilerIOWithProcess(process) {
      for {
        _    <- compilerError(testSourced("error"))
        fact <- getFactOrAbort(TestFactKey)
      } yield fact
    }.asserting(_ shouldBe None)
  }

  "registerFact" should "register fact when no errors" in {
    val process  = new TestCompilationProcess()
    val testFact = TestFact("test")

    runCompilerIOWithProcess(process) {
      registerFactIfClear(testFact)
    }.asserting { result =>
      result shouldBe Some(())
      process.facts should contain(testFact)
    }
  }

  it should "not register fact when there are errors" in {
    val process  = new TestCompilationProcess()
    val testFact = TestFact("test")

    runCompilerIOWithProcess(process) {
      for {
        _ <- compilerError(testSourced("error"))
        _ <- registerFactIfClear(testFact)
      } yield ()
    }.asserting { result =>
      result shouldBe Some(())
      process.facts should not contain testFact
    }
  }

  private val testFile  = new File("test.el")
  private val testRange = PositionRange.zero

  private def testSourced(msg: String) = Sourced(testFile, testRange, msg)

  private def runCompilerIO[T](value: CompilerIO[T]): IO[Option[T]] =
    value.run(null).run.fold(_ => None, t => Some(t._2))

  private def runCompilerIOWithProcess[T](process: CompilationProcess)(value: CompilerIO[T]): IO[Option[T]] =
    value.run(process).run.fold(_ => None, t => Some(t._2))

  // Test fixtures
  case class TestFact(value: String) extends CompilerFact {
    override def key(): CompilerFactKey[TestFact] = TestFactKey
  }

  case object TestFactKey extends CompilerFactKey[TestFact]

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
