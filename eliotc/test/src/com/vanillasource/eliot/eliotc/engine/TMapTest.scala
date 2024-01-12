package com.vanillasource.eliot.eliotc.engine

import cats.effect.IO
import org.scalatest.flatspec.AsyncFlatSpec
import cats.effect.testing.scalatest.AsyncIOSpec
import io.github.timwspence.cats.stm.STM
import org.scalatest.matchers.should.Matchers

class TMapTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  "map" should "find value inserted" in {
    val program = for {
      stm    <- STM.runtime[IO]
      map    <- TMap.create[Int, Int](stm)
      _      <- map.stm.commit(map.insert(1, 100))
      result <- echo(map, 1, 2)
    } yield result

    program.asserting(_ shouldBe 100)
  }

  private def echo(map: TMap[Int, Int], waitFor: Int, produce: Int) = map.stm.commit(for {
    value <- map.lookup(waitFor)
    result <- value match
      case Some(value) => map.insert(produce, produce) >> map.stm.pure(value)
      case None        => map.stm.retry
  } yield result)
}
