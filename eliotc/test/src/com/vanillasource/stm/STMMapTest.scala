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
      result     <- echo(map, 1, 2).commit(using stmRuntime)
    } yield result

    program.asserting(_ shouldBe 100)
  }

  it should "handle dependencies among many echo" in {
    val program = for {
      stmRuntime <- createRuntime()
      map        <- STMMap.empty[Int, Int]().commit(using stmRuntime)
      fibers     <- (1 to 10000).map(i => echo(map, i, i + 1).commit(using stmRuntime)).map(_.start).toList.sequence
      _          <- map.insert(1, 1).commit(using stmRuntime)
      results    <- fibers.map(_.joinWithNever).sequence
    } yield results

    program.asserting(_ shouldBe (1 to 10000).toList)
  }

  private def echo(map: STMMap[Int, Int], waitFor: Int, produce: Int): STM[Int] = for {
    value <- map.lookup(waitFor)
    result <- value match
      case Some(value) => map.insert(produce, produce) >> value.pure[STM]
      case None        => retry()
  } yield result
}
