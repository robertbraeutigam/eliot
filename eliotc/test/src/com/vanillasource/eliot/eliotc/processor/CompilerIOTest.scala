package com.vanillasource.eliot.eliotc.processor

import cats.data.Chain
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.ProcessorTest.*

class CompilerIOTest extends ProcessorTest {
  val testKey  = TestFactKey("test")
  val testFact = TestFact("test")

  "context" should "be clear if nothing yet happened" in {
    runCompilerIO {
      isClear
    }.asserting(_ shouldBe Right(true))
  }

  it should "not be clear after an error is registered" in {
    runCompilerIO {
      for {
        _     <- registerCompilerError(error("error"))
        clear <- isClear
      } yield clear
    }.asserting(_ shouldBe Right(false))
  }

  "current errors" should "return the only error registered" in {
    runCompilerIO {
      for {
        _      <- registerCompilerError(error("test error"))
        errors <- currentErrors
      } yield errors
    }.asserting(_.map(_.toList.map(_.message)) shouldBe Right(Seq("test error")))
  }

  it should "return empty chain when context is clear" in {
    runCompilerIO {
      currentErrors
    }.asserting(_ shouldBe Right(Chain.empty))
  }

  it should "contain all errors registered previously" in {
    runCompilerIO {
      for {
        _      <- registerCompilerError(error("error 1"))
        _      <- registerCompilerError(error("error 2"))
        _      <- registerCompilerError(error("error 3"))
        errors <- currentErrors
      } yield errors
    }.asserting(_.map(_.toList.map(_.message)) shouldBe Right(Seq("error 1", "error 2", "error 3")))
  }

  "getting a fact" should "return a fact when available" in {
    runCompilerIO {
      for {
        _    <- registerFactIfClear(testFact)
        fact <- getFactOrAbort(testKey)
      } yield fact
    }.asserting(_ shouldBe Right(testFact))
  }

  it should "short circuit when fact is not available" in {
    runCompilerIO {
      getFactOrAbort(TestFactKey("test"))
    }.asserting(_.isLeft shouldBe true)
  }

  "abort" should "short circuit with accumulated errors" in {
    runCompilerIO {
      for {
        _      <- registerCompilerError(error("error 1"))
        _      <- registerCompilerError(error("error 2"))
        result <- abort[String]
      } yield result
    }.asserting(_.isLeft shouldBe true)
  }

  it should "move errors from state to Either left" in {
    runCompilerIO {
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
    runCompilerIO {
      for {
        result <- abort[String]
      } yield result
    }.asserting(_.isLeft shouldBe true)
  }

  "recover" should "return the computation's result when it succeeds without errors" in {
    runCompilerIO {
      recover("success".pure[CompilerIO])("default")
    }.asserting(_ shouldBe Right("success"))
  }

  it should "return the computation's result when it succeeds with errors" in {
    runCompilerIO {
      for {
        result <- recover {
                    for {
                      _ <- registerCompilerError(error("error 1"))
                    } yield "success"
                  }("default")
      } yield result
    }.asserting(_ shouldBe Right("success"))
  }

  it should "return the default value when the computation aborts" in {
    runCompilerIO {
      recover {
        for {
          _ <- registerCompilerError(error("error 1"))
          _ <- abort[String]
        } yield "success"
      }("default")
    }.asserting(_ shouldBe Right("default"))
  }

  it should "collect errors from successful computations into parent state" in {
    runCompilerIO {
      for {
        _      <- recover {
                    registerCompilerError(error("error 1"))
                  }(())
        errors <- currentErrors
      } yield errors
    }.asserting(_.map(_.toList.map(_.message)) shouldBe Right(Seq("error 1")))
  }

  it should "collect errors from aborted computations into parent state" in {
    runCompilerIO {
      for {
        _      <- recover {
                    for {
                      _ <- registerCompilerError(error("error 1"))
                      _ <- registerCompilerError(error("error 2"))
                      _ <- abort[Unit]
                    } yield ()
                  }(())
        errors <- currentErrors
      } yield errors
    }.asserting(_.map(_.toList.map(_.message)) shouldBe Right(Seq("error 1", "error 2")))
  }

  it should "accumulate errors from multiple recover calls" in {
    runCompilerIO {
      for {
        _ <- recover(registerCompilerError(error("error 1")))(())
        _ <- recover(registerCompilerError(error("error 2")))(())
        _ <- recover {
               for {
                 _ <- registerCompilerError(error("error 3"))
                 _ <- abort[Unit]
               } yield ()
             }(())
        errors <- currentErrors
      } yield errors
    }.asserting(_.map(_.toList.map(_.message)) shouldBe Right(Seq("error 1", "error 2", "error 3")))
  }

  it should "preserve parent errors when child computation succeeds" in {
    runCompilerIO {
      for {
        _      <- registerCompilerError(error("parent error"))
        _      <- recover {
                    registerCompilerError(error("child error"))
                  }(())
        errors <- currentErrors
      } yield errors
    }.asserting(_.map(_.toList.map(_.message)) shouldBe Right(Seq("parent error", "child error")))
  }

  it should "preserve parent errors when child computation aborts" in {
    runCompilerIO {
      for {
        _      <- registerCompilerError(error("parent error"))
        _      <- recover {
                    for {
                      _ <- registerCompilerError(error("child error"))
                      _ <- abort[Unit]
                    } yield ()
                  }(())
        errors <- currentErrors
      } yield errors
    }.asserting(_.map(_.toList.map(_.message)) shouldBe Right(Seq("parent error", "child error")))
  }

  it should "handle nested recover calls correctly" in {
    runCompilerIO {
      for {
        result <- recover {
                    for {
                      _ <- registerCompilerError(error("outer error"))
                      innerResult <- recover {
                                       for {
                                         _ <- registerCompilerError(error("inner error"))
                                         _ <- abort[String]
                                       } yield "inner success"
                                     }("inner default")
                    } yield innerResult
                  }("outer default")
        errors <- currentErrors
      } yield (result, errors)
    }.asserting {
      case Right((result, errors)) =>
        val _ = result shouldBe "inner default"
        errors.toList.map(_.message) shouldBe Seq("outer error", "inner error")
      case Left(_) => fail("Expected Right but got Left")
    }
  }

  it should "not affect parent when child has no errors" in {
    runCompilerIO {
      for {
        _      <- recover("success".pure[CompilerIO])("default")
        errors <- currentErrors
      } yield errors
    }.asserting(_ shouldBe Right(Chain.empty))
  }
}
