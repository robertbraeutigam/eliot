package com.vanillasource.stm

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import com.vanillasource.stm.STM.*
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

class STMMapTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  "map" should "find value inserted" in {
    val program = for {
      stmRuntime <- createRuntime()
      map        <- STMMap.empty[Int, Int]().commit(using stmRuntime)
      _          <- map.insert(1, 100).commit(using stmRuntime)
      result     <- map.lookup(1).commit(using stmRuntime)
    } yield result

    program.asserting(_ shouldBe Some(100))
  }

  it should "return none on an empty map" in {
    val program = for {
      stmRuntime <- createRuntime()
      map        <- STMMap.empty[Int, Int]().commit(using stmRuntime)
      result     <- map.lookup(1).commit(using stmRuntime)
    } yield result

    program.asserting(_ shouldBe None)
  }
}
