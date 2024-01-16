package com.vanillasource.stm

import cats.data.ReaderT
import cats.effect.IO
import cats.syntax.all.*
import cats.effect.testing.scalatest.AsyncIOSpec
import com.vanillasource.stm.STM.*
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import io.github.timwspence.cats.stm.STM as CatsSTM

class STMMapTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  "map" should "find value inserted" in {
    val program = for {
      map    <- STMMap.empty[Int, Int]().commit()
      _      <- map.insert(1, 100).commit()
      result <- echo(map, 1, 2).commit()
    } yield result

    execute(program).asserting(_ shouldBe 100)
  }

  private def echo(map: STMMap[Int, Int], waitFor: Int, produce: Int): STM[Int] = for {
    value <- map.lookup(waitFor)
    result <- value match
      case Some(value) => map.insert(produce, produce) >> value.pure[STM]
      case None        => retry()
  } yield result

  private def execute[A](program: ReaderT[IO, CatsSTM[IO], A]): IO[A] =
    CatsSTM.runtime[IO].flatMap(program.apply)
}
