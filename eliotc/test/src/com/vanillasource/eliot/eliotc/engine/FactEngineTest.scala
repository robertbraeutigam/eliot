package com.vanillasource.eliot.eliotc.engine

import cats.effect.IO
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

class FactEngineTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  "fact engine" should "terminate with empty map if there are no initial facts" in {
    FactEngine(Seq.empty)
      .resolve(Map.empty[Int, Int])
      .asserting(_ shouldBe Map.empty)
  }

  it should "terminate with initial facts if no processors are supplied" in {
    FactEngine(Seq.empty)
      .resolve(Map((1, 1), (2, 2)))
      .asserting(_ shouldBe Map((1, 1), (2, 2)))
  }

  it should "execute relevant processor on initial facts" in {
    FactEngine(Seq(Echo(1, 2)))
      .resolve(Map((1, 1)))
      .asserting(_ shouldBe Map((1, 1), (2, 2)))
  }

  it should "fail if a processor fails" in {
    FactEngine(Seq(Fail(1)))
      .resolve(Map((1, 1)))
      .assertThrowsWithMessage[Exception]("failing processor")
  }

  it should "return initial facts if processors don't react to initial facts" in {
    FactEngine(Seq(Echo(2, 3), Echo(3, 4)))
      .resolve(Map((1, 1)))
      .asserting(_ shouldBe Map((1, 1)))
  }

  it should "execute a dependency chain" in {
    FactEngine(Seq(Echo(1, 2), Echo(2, 3), Echo(3, 4)))
      .resolve(Map((1, 1)))
      .asserting(_ shouldBe Map((1, 1), (2, 2), (3, 3), (4, 4)))
  }

  it should "execute a processor that requires additional dependencies" in {
    FactEngine(Seq(Echo(1, 2), Echo(2, 3), Echo(3, 4, Seq(1, 2))))
      .resolve(Map((1, 1)))
      .asserting(_ shouldBe Map((1, 1), (2, 2), (3, 3), (4, 4)))
  }

  it should "return none when getting a fact that will never be produced" in {
    FactEngine(Seq(Echo(1, 2), EchoOnMissing(2, 3, 4)))
      .resolve(Map((1, 1)))
      .asserting(_ shouldBe Map((1, 1), (2, 2), (3, 3)))

  }

  private case class Echo(dependsOn: Int, produces: Int, additionalDependencies: Seq[Int] = Seq.empty)
      extends FactProcessor[Int, Int] {
    override def process(value: Int)(using runningFactEngine: RunningFactEngine[Int, Int]): IO[Unit] = value match {
      case v if v === dependsOn =>
        additionalDependencies
          .map(dep => runningFactEngine.getFact(dep))
          .sequence
          .map(_.flatten)
          .asserting(_.size shouldBe additionalDependencies.size) >>
          runningFactEngine.registerFact(produces, produces)
      case _                    => IO.unit
    }
  }

  private case class Fail(dependsOn: Int) extends FactProcessor[Int, Int] {
    override def process(value: Int)(using runningFactEngine: RunningFactEngine[Int, Int]): IO[Unit] = value match {
      case v if v === dependsOn => IO.raiseError(new Exception("failing processor"))
      case _                    => IO.unit
    }
  }

  private case class EchoOnMissing(dependsOn: Int, produces: Int, missingDependency: Int)
      extends FactProcessor[Int, Int] {
    override def process(value: Int)(using runningFactEngine: RunningFactEngine[Int, Int]): IO[Unit] = value match {
      case v if v === dependsOn =>
        for {
          depResult <- runningFactEngine.getFact(missingDependency)
          _         <- depResult match
                         case Some(value) => IO.unit
                         case None        => runningFactEngine.registerFact(produces, produces)
        } yield ()
      case _                    => IO.unit
    }
  }
}
