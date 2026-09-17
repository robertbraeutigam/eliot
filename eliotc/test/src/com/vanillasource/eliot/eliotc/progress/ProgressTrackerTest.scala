package com.vanillasource.eliot.eliotc.progress

import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.{IO, Ref}
import com.vanillasource.eliot.eliotc.compiler.IncrementalFactGenerator
import com.vanillasource.eliot.eliotc.compiler.cache.FactCacheData
import com.vanillasource.eliot.eliotc.compiler.cache.IncrementalFactGeneratorTest.*
import com.vanillasource.eliot.eliotc.processor.CompilerProcessor
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

class ProgressTrackerTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {

  "the progress tracker" should "count every demanded fact of a cold build as generated" in {
    chain(10).flatMap(proc => countBuild(proc, None)).asserting(_._1 shouldBe (2L, 0L))
  }

  it should "count the same facts on an unchanged rerun, the derived ones from the cache" in {
    val test = for {
      proc <- chain(10)
      cold <- countBuild(proc, None)
      warm <- countBuild(proc, Some(cold._2))
    } yield warm._1
    // the leaf is re-read from the world, the derived fact is accepted
    test.asserting(_ shouldBe (2L, 1L))
  }

  it should "count a fact proven unchanged by the drill as from the cache" in {
    val test = for {
      src  <- Ref.of[IO, Int](10)
      proc  = graph(
                Map("src" -> Leaf(src), "mid" -> Derived("src", _ + 1), "top" -> Derived("mid", _ * 2)),
                Map.empty
              )
      cold <- countBuild(proc, None, "top")
      warm <- countBuild(proc, Some(cold._2), "top")
    } yield warm._1
    // `mid` is never materialised on the rerun: the drill proves it unchanged on the way to accepting `top`
    test.asserting(_ shouldBe (3L, 2L))
  }

  it should "not count a pushed fact that nothing asked for" in {
    val test = for {
      src  <- Ref.of[IO, Int](10)
      proc  = graph(Map("src" -> Leaf(src), "owner" -> Pushing("src", "sibling", _ + 1)), Map.empty)
      run  <- countBuild(proc, None, "owner")
    } yield run._1
    test.asserting(_ shouldBe (2L, 0L))
  }

  it should "count a fact once however often it is asked for" in {
    val test = for {
      src <- Ref.of[IO, Int](10)
      proc = graph(
               Map(
                 "a" -> Leaf(src),
                 "b" -> Derived("a", _ + 1),
                 "c" -> Derived("a", _ + 2),
                 "d" -> Derived2("b", "c", _ + _)
               ),
               Map.empty
             )
      run <- countBuild(proc, None, "d")
    } yield run._1
    test.asserting(_ shouldBe (4L, 0L))
  }

  it should "report the phase it was last told" in {
    val test = for {
      tracker  <- ProgressTracker.create()
      _        <- tracker.enter(ProgressPhase.LoadingCache)
      _        <- tracker.enter(ProgressPhase.Working)
      snapshot <- tracker.snapshot
    } yield snapshot.phase
    test.asserting(_ shouldBe ProgressPhase.Working)
  }

  private def chain(value: Int): IO[CompilerProcessor] =
    Ref.of[IO, Int](value).map(src => graph(Map("leaf" -> Leaf(src), "derived" -> Derived("leaf", _ * 2)), Map.empty))

  /** One run demanding `root`, answering the (delivered, from cache) counts and the cache for the next run. */
  private def countBuild(
      processor: CompilerProcessor,
      prior: Option[FactCacheData],
      root: String = "derived"
  ): IO[((Long, Long), FactCacheData)] =
    for {
      tracker   <- ProgressTracker.create()
      generator <- IncrementalFactGenerator.create(processor, prior, strictAccounting = true, Some(tracker))
      _         <- generator.getFact(NumberKey(root))
      cache     <- generator.buildCacheData()
      snapshot  <- tracker.snapshot
    } yield ((snapshot.delivered, snapshot.fromCache), cache)
}
