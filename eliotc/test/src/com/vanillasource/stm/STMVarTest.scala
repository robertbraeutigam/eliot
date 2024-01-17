package com.vanillasource.stm

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import com.vanillasource.stm.STM.*
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

class STMVarTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  "var" should "return initial value" in {
    val program = for {
      stmRuntime <- createRuntime()
      v          <- createSTMVar(100).commit(using stmRuntime)
      result     <- v.get().commit(using stmRuntime)
    } yield result

    program.asserting(_ shouldBe 100)
  }

  it should "return set value" in {
    val program = for {
      stmRuntime <- createRuntime()
      v          <- createSTMVar(100).commit(using stmRuntime)
      _          <- v.set(999).commit(using stmRuntime)
      result     <- v.get().commit(using stmRuntime)
    } yield result

    program.asserting(_ shouldBe 999)
  }

  it should "atomically update" in {
    val program = for {
      stmRuntime <- createRuntime()
      v          <- createSTMVar(100).commit(using stmRuntime)
      _          <- v.update(_ + 1).commit(using stmRuntime).parReplicateA_(100)
      result     <- v.get().commit(using stmRuntime)
    } yield result

    program.asserting(_ shouldBe 200)
  }
}
