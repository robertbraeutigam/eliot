package com.vanillasource.eliot.eliotc.processor

import cats.data.Chain
import com.vanillasource.eliot.eliotc.feedback.CompilerError
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*

class CompilerIOTest extends CompilerIOTestBase {
  "context" should "be clear if nothing yet happened" in {
    runCompilerIO() {
      isClear
    }.asserting(_ shouldBe Right(true))
  }

  it should "not be clear after an error is registered" in {
    runCompilerIO() {
      for {
        _     <- registerCompilerError(error("error"))
        clear <- isClear
      } yield clear
    }.asserting(_ shouldBe Right(false))
  }

  "current errors" should "return the only error registered" in {
    runCompilerIO() {
      for {
        _      <- registerCompilerError(error("test error"))
        errors <- currentErrors
      } yield errors
    }.asserting(_.map(_.toList.map(_.message)) shouldBe Right(Seq("test error")))
  }

  it should "return empty chain when context is clear" in {
    runCompilerIO() {
      currentErrors
    }.asserting(_ shouldBe Right(Chain.empty))
  }

  it should "contain all errors registered previously" in {
    runCompilerIO() {
      for {
        _      <- registerCompilerError(error("error 1"))
        _      <- registerCompilerError(error("error 2"))
        _      <- registerCompilerError(error("error 3"))
        errors <- currentErrors
      } yield errors
    }.asserting(_.map(_.toList.map(_.message)) shouldBe Right(Seq("error 1", "error 2", "error 3")))
  }

  "getting a fact" should "return a fact when available" in {
    val process  = new TestCompilationProcess()
    val testFact = TestFact("test")
    process.registerFactSync(testFact)

    runCompilerIO(process) {
      getFactOrAbort(TestFactKey("test"))
    }.asserting(_ shouldBe Right(testFact))
  }

  it should "short circuit when fact is not available" in {
    val process = new TestCompilationProcess()

    runCompilerIO(process) {
      getFactOrAbort(TestFactKey("test"))
    }.asserting(_.isLeft shouldBe true)
  }

  "registering a fact when clear" should "register fact when there are no errors" in {
    val process  = new TestCompilationProcess()
    val testFact = TestFact("test")

    runCompilerIO(process) {
      registerFactIfClear(testFact)
    }.asserting { _ => process.facts.values should contain(testFact) }
  }

  it should "not register fact when there are errors" in {
    val process  = new TestCompilationProcess()
    val testFact = TestFact("test")

    runCompilerIO(process) {
      for {
        _ <- registerCompilerError(error("error"))
        _ <- registerFactIfClear(testFact)
      } yield ()
    }.asserting { _ => process.facts should not contain testFact }
  }

  "abort" should "short circuit with accumulated errors" in {
    runCompilerIO() {
      for {
        _      <- registerCompilerError(error("error 1"))
        _      <- registerCompilerError(error("error 2"))
        result <- abort[String]
      } yield result
    }.asserting(_.isLeft shouldBe true)
  }

  it should "move errors from state to Either left" in {
    runCompilerIO() {
      for {
        _ <- registerCompilerError(error("error 1"))
        _ <- registerCompilerError(error("error 2"))
        _ <- abort[Unit]
      } yield ()
    }.asserting {
      case Left(errors) => errors.toList.map(_.message) shouldBe Seq("error 1", "error 2")
      case Right(_)     => fail("Expected Left but got Right")
    }
  }

  it should "short circuit even when no errors present" in {
    runCompilerIO() {
      for {
        result <- abort[String]
      } yield result
    }.asserting(_.isLeft shouldBe true)
  }

  private def error(msg: String) = CompilerError(msg, Seq.empty, "", "", PositionRange.zero)
}
