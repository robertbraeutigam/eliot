package com.vanillasource.eliot.eliotc.compiler

import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.{Deferred, IO, Ref}
import cats.effect.syntax.all.*
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.common.NullProcessor
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration.*

/** Behavioral tests for the cancel-restart server loop. Each test drives [[CompilationServer.startWith]] with a
  * synthetic compile that records when it *starts* and when it *finishes* (after an optional gate), so we can assert on
  * which runs actually completed — i.e. that stale runs were cancelled and the latest request won.
  */
class CompilationServerTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  import CompilationServerTest.*

  "a compilation server" should "run one compilation per request and expose its result" in {
    val test = for {
      started  <- Ref.of[IO, Int](0)
      finished <- Ref.of[IO, List[Int]](Nil)
      gen      <- IncrementalFactGenerator.create(NullProcessor(), None)
      defined  <- CompilationServer
                    .startWith(fakeCompile(started, finished, IO.unit, gen), IO.unit, _ => IO.unit)
                    .use(server => server.requestCompile *> eventually(server.latestResult)(_.isDefined))
                    .map(_.isDefined)
      fin      <- finished.get
    } yield (fin, defined)
    test.timeout(testTimeout).asserting(_ shouldBe (List(1), true))
  }

  it should "cancel an in-flight compilation when a newer request arrives, completing only the latest" in {
    val test = for {
      started  <- Ref.of[IO, Int](0)
      finished <- Ref.of[IO, List[Int]](Nil)
      gate     <- Deferred[IO, Unit]
      gen      <- IncrementalFactGenerator.create(NullProcessor(), None)
      fin      <- CompilationServer.startWith(fakeCompile(started, finished, gate.get, gen), IO.unit, _ => IO.unit).use {
                    server =>
                      for {
                        _ <- server.requestCompile
                        _ <- eventually(started.get)(_ == 1) // first compile in flight, blocked on the gate
                        _ <- server.requestCompile           // supersede it
                        _ <- eventually(started.get)(_ == 2) // second compile in flight (first was cancelled)
                        _ <- gate.complete(())               // release; only the second can reach completion
                        f <- eventually(finished.get)(_.nonEmpty)
                      } yield f
                  }
    } yield fin
    test.timeout(testTimeout).asserting(_ shouldBe List(2))
  }

  it should "push each completed result to the onResult callback" in {
    val test = for {
      started  <- Ref.of[IO, Int](0)
      finished <- Ref.of[IO, List[Int]](Nil)
      pushed   <- Ref.of[IO, Int](0)
      gen      <- IncrementalFactGenerator.create(NullProcessor(), None)
      count    <- CompilationServer
                    .startWith(fakeCompile(started, finished, IO.unit, gen), IO.unit, _ => pushed.update(_ + 1))
                    .use(server => server.requestCompile *> eventually(pushed.get)(_ == 1))
    } yield count
    test.timeout(testTimeout).asserting(_ shouldBe 1)
  }

  it should "flush the session cache when the server shuts down" in {
    val test = for {
      persisted <- Ref.of[IO, Boolean](false)
      started   <- Ref.of[IO, Int](0)
      finished  <- Ref.of[IO, List[Int]](Nil)
      gen       <- IncrementalFactGenerator.create(NullProcessor(), None)
      _         <- CompilationServer
                     .startWith(fakeCompile(started, finished, IO.unit, gen), persisted.set(true), _ => IO.unit)
                     .use(server => server.requestCompile *> eventually(finished.get)(_.nonEmpty))
      flushed   <- persisted.get
    } yield flushed
    test.timeout(testTimeout).asserting(_ shouldBe true)
  }

  it should "cancel an in-flight compilation when it shuts down" in {
    val test = for {
      started  <- Ref.of[IO, Int](0)
      finished <- Ref.of[IO, List[Int]](Nil)
      gate     <- Deferred[IO, Unit] // never completed: the compile blocks until the server is shut down
      gen      <- IncrementalFactGenerator.create(NullProcessor(), None)
      _        <- CompilationServer
                    .startWith(fakeCompile(started, finished, gate.get, gen), IO.unit, _ => IO.unit)
                    .use(server => server.requestCompile *> eventually(started.get)(_ == 1))
      fin      <- finished.get
    } yield fin
    test.timeout(testTimeout).asserting(_ shouldBe Nil)
  }
}

object CompilationServerTest {
  private val testTimeout: FiniteDuration = 10.seconds

  /** A synthetic compilation: record the run's ordinal as it starts, await `gate`, then record it as finished and return
    * an (empty) result. Cancelled before the gate opens ⇒ it never reaches `finished`, which is how the tests observe
    * that a stale run was cancelled.
    */
  private def fakeCompile(
      started: Ref[IO, Int],
      finished: Ref[IO, List[Int]],
      gate: IO[Unit],
      generator: IncrementalFactGenerator
  ): IO[CompilationResult] =
    for {
      id <- started.updateAndGet(_ + 1)
      _  <- gate
      _  <- finished.update(_ :+ id)
    } yield CompilationResult(generator, Seq.empty)

  /** Poll `fa` until it satisfies `p`, yielding the satisfying value. Bounded by each test's overall timeout. */
  private def eventually[A](fa: IO[A])(p: A => Boolean): IO[A] =
    fa.flatMap(a => if (p(a)) IO.pure(a) else IO.sleep(2.millis) *> eventually(fa)(p))
}
