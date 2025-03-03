package com.vanillasource.eliot.eliotc.source

import cats.effect.Ref
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.eliot.eliotc.source.CompilationIO.*
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

class CompilationIOTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  "compilation io" should "support normal monadic syntax" in {
    val program = for {
      variable <- Ref[CompilationIO].of(0)
      _        <- variable.update(_ + 1)
      _        <- variable.update(_ + 1)
      _        <- variable.get.asserting(_ should be(2))
    } yield ()

    program.runCompilation_()
  }

  it should "not execute side effects after an abort" in {
    val program = for {
      variable <- Ref[CompilationIO].of(0)
      _        <- variable.update(_ + 1)
      _        <- compilationAbort
      _        <- variable.update(_ + 1)
      _        <- variable.get.asserting(_ should be(1))
    } yield ()

    program.runCompilation_()
  }

  it should "execute side effects under condition if no errors were registered" in {
    val program = for {
      variable <- Ref[CompilationIO].of(0)
      _        <- variable.update(_ + 1)
      _        <- variable.update(_ + 1).ifNoErrors
      _        <- variable.get.asserting(_ should be(2))
    } yield ()

    program.runCompilation_()
  }

  it should "not execute side effects under condition if there were errors registered" in {
    val program = for {
      variable <- Ref[CompilationIO].of(0)
      _        <- variable.update(_ + 1)
      _        <- compilationError
      _        <- variable.update(_ + 1).ifNoErrors
      _        <- variable.get.asserting(_ should be(1))
    } yield ()

    program.runCompilation_()
  }

  it should "execute side effects without condition even if there were errors registered" in {
    val program = for {
      variable <- Ref[CompilationIO].of(0)
      _        <- variable.update(_ + 1)
      _        <- compilationError
      _        <- variable.update(_ + 1)
      _        <- variable.get.asserting(_ should be(2))
    } yield ()

    program.runCompilation_()
  }

}
