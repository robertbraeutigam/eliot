package com.vanillasource.eliot.eliotc.compiler.cache

import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.compiler.IncrementalFactGenerator
import com.vanillasource.eliot.eliotc.feedback.CompilerError
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey, CompilerProcessor}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

/** Exhaustive behavioral tests for the incremental, backward-pull fact generator. Each test drives one or more
  * compilation "runs" — a run is a fresh [[IncrementalFactGenerator]] seeded with the previous run's persisted cache —
  * over a small graph of synthetic facts, asserting on generation counts (how many times a processor actually ran),
  * returned values, and the persisted cache.
  */
class IncrementalFactGeneratorTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  import IncrementalFactGeneratorTest.*

  "incremental generation" should "generate every fact once on a cold start" in {
    val test = for {
      src      <- Ref.of[IO, Int](10)
      counts   <- counters("leaf", "derived")
      proc      = graph(Map("leaf" -> Leaf(src), "derived" -> Derived("leaf", _ * 2)), counts)
      r        <- runBuild(proc, None)(_.getFact(NumberKey("derived")))
      leafN    <- counts("leaf").get
      derivedN <- counts("derived").get
    } yield (r._1, leafN, derivedN)
    test.asserting(_ shouldBe (Some(NumberFact("derived", 20)), 1, 1))
  }

  it should "re-run leaves but accept derived facts on an unchanged rerun" in {
    val test = for {
      src      <- Ref.of[IO, Int](10)
      counts   <- counters("leaf", "derived")
      proc      = graph(Map("leaf" -> Leaf(src), "derived" -> Derived("leaf", _ * 2)), counts)
      run1     <- runBuild(proc, None)(_.getFact(NumberKey("derived")))
      run2     <- runBuild(proc, Some(run1._2))(_.getFact(NumberKey("derived")))
      leafN    <- counts("leaf").get
      derivedN <- counts("derived").get
    } yield (run2._1, leafN, derivedN)
    // leaf re-runs each run (it is a starting point); derived is served from cache the second time
    test.asserting(_ shouldBe (Some(NumberFact("derived", 20)), 2, 1))
  }

  it should "regenerate a derived fact when its leaf value changes" in {
    val test = for {
      src      <- Ref.of[IO, Int](10)
      counts   <- counters("leaf", "derived")
      proc      = graph(Map("leaf" -> Leaf(src), "derived" -> Derived("leaf", _ * 2)), counts)
      run1     <- runBuild(proc, None)(_.getFact(NumberKey("derived")))
      _        <- src.set(15)
      run2     <- runBuild(proc, Some(run1._2))(_.getFact(NumberKey("derived")))
      leafN    <- counts("leaf").get
      derivedN <- counts("derived").get
    } yield (run2._1, leafN, derivedN)
    test.asserting(_ shouldBe (Some(NumberFact("derived", 30)), 2, 2))
  }

  it should "cut off propagation when a changed leaf does not change a derived value" in {
    // src -> sig (= src % 10) -> compiled (= sig * 100). Changing src 5 -> 15 leaves sig unchanged (both 5).
    val test = for {
      src         <- Ref.of[IO, Int](5)
      counts      <- counters("src", "sig", "compiled")
      proc         = graph(
                       Map(
                         "src"      -> Leaf(src),
                         "sig"      -> Derived("src", _ % 10),
                         "compiled" -> Derived("sig", _ * 100)
                       ),
                       counts
                     )
      run1        <- runBuild(proc, None)(_.getFact(NumberKey("compiled")))
      _           <- src.set(15)
      run2        <- runBuild(proc, Some(run1._2))(_.getFact(NumberKey("compiled")))
      srcN        <- counts("src").get
      sigN        <- counts("sig").get
      compiledN   <- counts("compiled").get
    } yield (run2._1, srcN, sigN, compiledN)
    // src + sig recompute (2 each), but compiled is cut off (still 1) because sig's value is unchanged
    test.asserting(_ shouldBe (Some(NumberFact("compiled", 500)), 2, 2, 1))
  }

  it should "propagate through the cutoff point when the derived value does change" in {
    val test = for {
      src       <- Ref.of[IO, Int](5)
      counts    <- counters("src", "sig", "compiled")
      proc       = graph(
                     Map("src" -> Leaf(src), "sig" -> Derived("src", _ % 10), "compiled" -> Derived("sig", _ * 100)),
                     counts
                   )
      run1      <- runBuild(proc, None)(_.getFact(NumberKey("compiled")))
      _         <- src.set(6) // 6 % 10 = 6, so sig changes
      run2      <- runBuild(proc, Some(run1._2))(_.getFact(NumberKey("compiled")))
      compiledN <- counts("compiled").get
    } yield (run2._1, compiledN)
    test.asserting(_ shouldBe (Some(NumberFact("compiled", 600)), 2))
  }

  it should "compute a shared dependency only once within a run (diamond)" in {
    // a -> {b, c} -> d. 'a' must be computed exactly once despite two paths into it.
    val test = for {
      src   <- Ref.of[IO, Int](10)
      counts <- counters("a", "b", "c", "d")
      proc   = graph(
                 Map(
                   "a" -> Leaf(src),
                   "b" -> Derived("a", _ + 1),
                   "c" -> Derived("a", _ + 2),
                   "d" -> Derived2("b", "c", _ + _)
                 ),
                 counts
               )
      run   <- runBuild(proc, None)(_.getFact(NumberKey("d")))
      aN    <- counts("a").get
    } yield (run._1, aN)
    test.asserting(_ shouldBe (Some(NumberFact("d", 23)), 1)) // b=11, c=12, d=23; a computed once
  }

  it should "record direct dependencies in the built cache" in {
    val test = for {
      src   <- Ref.of[IO, Int](10)
      counts <- counters("leaf", "derived")
      proc   = graph(Map("leaf" -> Leaf(src), "derived" -> Derived("leaf", _ * 2)), counts)
      run   <- runBuild(proc, None)(_.getFact(NumberKey("derived")))
    } yield run._2.entries
    test.asserting { entries =>
      entries(NumberKey("derived")) shouldBe CacheEntry(NumberFact("derived", 20), Set(NumberKey("leaf")))
    }
  }

  it should "mark a leaf with an empty dependency set in the built cache" in {
    val test = for {
      src   <- Ref.of[IO, Int](10)
      counts <- counters("leaf", "derived")
      proc   = graph(Map("leaf" -> Leaf(src), "derived" -> Derived("leaf", _ * 2)), counts)
      run   <- runBuild(proc, None)(_.getFact(NumberKey("derived")))
    } yield run._2.entries(NumberKey("leaf"))
    test.asserting(_ shouldBe CacheEntry(NumberFact("leaf", 10), Set.empty))
  }

  it should "keep accepting a derived fact across many unchanged runs (deps carried forward)" in {
    val test = for {
      src      <- Ref.of[IO, Int](10)
      counts   <- counters("leaf", "derived")
      proc      = graph(Map("leaf" -> Leaf(src), "derived" -> Derived("leaf", _ * 2)), counts)
      run1     <- runBuild(proc, None)(_.getFact(NumberKey("derived")))
      run2     <- runBuild(proc, Some(run1._2))(_.getFact(NumberKey("derived")))
      run3     <- runBuild(proc, Some(run2._2))(_.getFact(NumberKey("derived")))
      derivedN <- counts("derived").get
    } yield derivedN
    // accepted in run2 and run3 ⇒ generated exactly once across three runs
    test.asserting(_ shouldBe 1)
  }

  it should "always regenerate a fact whose cached entry has no recorded dependencies" in {
    // A hand-built prior in which 'derived' has empty deps must be treated as a leaf (always regenerated),
    // never accepted — this is the conservative safety rule for side-effect-registered facts.
    val staleEntry = CacheEntry(NumberFact("derived", 999), Set.empty)
    val test = for {
      src      <- Ref.of[IO, Int](10)
      counts   <- counters("leaf", "derived")
      proc      = graph(Map("leaf" -> Leaf(src), "derived" -> Derived("leaf", _ * 2)), counts)
      prior     = FactCacheData(FactCache.CACHE_VERSION, Map(NumberKey("derived") -> staleEntry))
      run      <- runBuild(proc, Some(prior))(_.getFact(NumberKey("derived")))
      derivedN <- counts("derived").get
    } yield (run._1, derivedN)
    test.asserting(_ shouldBe (Some(NumberFact("derived", 20)), 1)) // recomputed fresh, not the stale 999
  }

  it should "accept an injected fact from cache rather than trying to regenerate it" in {
    // 'injected' has no processor that can produce it; treating it as a leaf and regenerating would abort. It must be
    // served from the cache. 'derived' reads it, so a successful result proves the injected fact was accepted.
    val priorInjected = FactCacheData(
      FactCache.CACHE_VERSION,
      Map(NumberKey("injected") -> CacheEntry(NumberFact("injected", 42), Set.empty, injected = true))
    )
    val test = for {
      counts <- counters("derived")
      proc    = graph(Map("derived" -> Derived("injected", _ + 1)), counts)
      run    <- runBuild(proc, Some(priorInjected))(_.getFact(NumberKey("derived")))
    } yield run._1
    test.asserting(_ shouldBe Some(NumberFact("derived", 43)))
  }

  it should "re-persist an accepted injected fact as injected" in {
    val injectedEntry = CacheEntry(NumberFact("injected", 42), Set.empty, injected = true)
    val priorInjected = FactCacheData(FactCache.CACHE_VERSION, Map(NumberKey("injected") -> injectedEntry))
    val test = for {
      counts <- counters("derived")
      proc    = graph(Map("derived" -> Derived("injected", _ + 1)), counts)
      run    <- runBuild(proc, Some(priorInjected))(_.getFact(NumberKey("derived")))
    } yield run._2.entries.get(NumberKey("injected"))
    test.asserting(_ shouldBe Some(injectedEntry))
  }

  it should "not cache a failed fact and re-run it (re-emitting its error) on the next run" in {
    val test = for {
      counts  <- counters("fail")
      proc     = graph(Map("fail" -> Failing("boom")), counts)
      run1    <- runBuild(proc, None) { g => g.getFact(NumberKey("fail")) *> g.currentErrors() }
      run2    <- runBuild(proc, Some(run1._2)) { g => g.getFact(NumberKey("fail")) *> g.currentErrors() }
      failN   <- counts("fail").get
    } yield (run1._1.map(_.message), run2._1.map(_.message), run1._2.entries.contains(NumberKey("fail")), failN)
    test.asserting { case (errors1, errors2, cachedFailure, failN) =>
      errors1 shouldBe Seq("boom")
      errors2 shouldBe Seq("boom") // error re-surfaces on the unchanged rerun
      cachedFailure shouldBe false // failure is never persisted
      failN shouldBe 2             // processor ran on both runs
    }
  }

  it should "regenerate (and re-error) a fact whose failing dependency is not cached" in {
    val test = for {
      counts    <- counters("fail", "consumer")
      proc       = graph(Map("fail" -> Failing("boom"), "consumer" -> Derived("fail", _ + 1)), counts)
      run1      <- runBuild(proc, None)(_.getFact(NumberKey("consumer")))
      run2      <- runBuild(proc, Some(run1._2))(_.getFact(NumberKey("consumer")))
      consumerN <- counts("consumer").get
    } yield (run1._1, run2._1, run1._2.entries.keySet, consumerN)
    test.asserting { case (r1, r2, cachedKeys, consumerN) =>
      r1 shouldBe None
      r2 shouldBe None
      cachedKeys shouldBe Set.empty // neither the failure nor its dependent is cached
      consumerN shouldBe 2          // dependent re-ran on the second run
    }
  }

  it should "prune facts that are no longer reachable from the requested fact" in {
    val test = for {
      srcA  <- Ref.of[IO, Int](1)
      srcB  <- Ref.of[IO, Int](2)
      counts <- counters("a", "b", "da", "db")
      proc   = graph(
                 Map(
                   "a"  -> Leaf(srcA),
                   "b"  -> Leaf(srcB),
                   "da" -> Derived("a", _ * 10),
                   "db" -> Derived("b", _ * 10)
                 ),
                 counts
               )
      run1  <- runBuild(proc, None)(g => g.getFact(NumberKey("da")) *> g.getFact(NumberKey("db")))
      run2  <- runBuild(proc, Some(run1._2))(_.getFact(NumberKey("da"))) // only 'da' requested now
    } yield (run1._2.entries.keySet, run2._2.entries.keySet)
    test.asserting { case (keys1, keys2) =>
      keys1 shouldBe Set(NumberKey("a"), NumberKey("b"), NumberKey("da"), NumberKey("db"))
      keys2 shouldBe Set(NumberKey("a"), NumberKey("da")) // b and db pruned
    }
  }

  it should "generate a newly requested fact fresh against an existing cache" in {
    val test = for {
      srcA  <- Ref.of[IO, Int](1)
      srcB  <- Ref.of[IO, Int](2)
      counts <- counters("a", "b", "da", "db")
      proc   = graph(
                 Map("a" -> Leaf(srcA), "b" -> Leaf(srcB), "da" -> Derived("a", _ * 10), "db" -> Derived("b", _ * 10)),
                 counts
               )
      run1  <- runBuild(proc, None)(_.getFact(NumberKey("da")))
      run2  <- runBuild(proc, Some(run1._2))(_.getFact(NumberKey("db"))) // new fact this run
      dbN   <- counts("db").get
    } yield (run2._1, dbN)
    test.asserting(_ shouldBe (Some(NumberFact("db", 20)), 1))
  }

  it should "maintain the ancestor request chain in activeFactKeys during generation" in {
    // chain d -> b -> a; when generating 'a' the chain is [a, b, d] (innermost first).
    val test = for {
      src     <- Ref.of[IO, Int](10)
      counts  <- counters("a", "b", "d")
      capture <- Ref.of[IO, Map[String, List[CompilerFactKey[?]]]](Map.empty)
      proc     = graph(
                   Map("a" -> Leaf(src), "b" -> Derived("a", _ + 1), "d" -> Derived("b", _ + 1)),
                   counts,
                   Some(capture)
                 )
      _       <- runBuild(proc, None)(_.getFact(NumberKey("d")))
      chains  <- capture.get
    } yield chains
    test.asserting { chains =>
      chains("a") shouldBe List(NumberKey("a"), NumberKey("b"), NumberKey("d"))
      chains("d") shouldBe List(NumberKey("d"))
    }
  }

  it should "rebuild a deleted output and then stabilize (presence-leaf behavior)" in {
    // Models OutputFileStat: 'present' leaf reads a 0/1 presence flag; 'jar' depends on it and 'writes'
    // (sets the flag to 1 and bumps the write counter). Deleting the output (flag -> 0) forces a rebuild.
    val test = for {
      present <- Ref.of[IO, Int](0) // 0 = output missing
      writes  <- Ref.of[IO, Int](0)
      counts  <- counters("present", "jar")
      proc     = graph(
                   Map("present" -> Leaf(present), "jar" -> Writer("present", present.set(1) >> writes.update(_ + 1))),
                   counts
                 )
      run1    <- runBuild(proc, None)(_.getFact(NumberKey("jar")))
      w1      <- writes.get
      run2    <- runBuild(proc, Some(run1._2))(_.getFact(NumberKey("jar")))
      w2      <- writes.get // echo rebuild: presence flipped 0 -> 1 across the first write
      run3    <- runBuild(proc, Some(run2._2))(_.getFact(NumberKey("jar")))
      w3      <- writes.get // stable now
      _       <- present.set(0) // delete the output
      run4    <- runBuild(proc, Some(run3._2))(_.getFact(NumberKey("jar")))
      w4      <- writes.get // deletion forces a rebuild
    } yield (w1, w2, w3, w4)
    test.asserting(_ shouldBe (1, 2, 2, 3))
  }
}

object IncrementalFactGeneratorTest {
  case class NumberFact(name: String, value: Int) extends CompilerFact {
    override def key(): CompilerFactKey[NumberFact] = NumberKey(name)
  }

  case class NumberKey(name: String) extends CompilerFactKey[NumberFact]

  sealed trait Node
  case class Leaf(source: Ref[IO, Int])                                extends Node
  case class Derived(dep: String, f: Int => Int)                       extends Node
  case class Derived2(dep1: String, dep2: String, f: (Int, Int) => Int) extends Node
  case class Writer(dep: String, sideEffect: IO[Unit])                 extends Node
  case class Failing(message: String)                                  extends Node

  private def err(message: String): CompilerError =
    CompilerError(message, Seq.empty, "", "", PositionRange.zero)

  def counters(names: String*): IO[Map[String, Ref[IO, Int]]] =
    names.toList.traverse(name => Ref.of[IO, Int](0).map(name -> _)).map(_.toMap)

  def graph(
      defs: Map[String, Node],
      counts: Map[String, Ref[IO, Int]],
      capture: Option[Ref[IO, Map[String, List[CompilerFactKey[?]]]]] = None
  ): CompilerProcessor = new GraphProcessor(defs, counts, capture)

  def runBuild[A](processor: CompilerProcessor, prior: Option[FactCacheData])(
      body: IncrementalFactGenerator => IO[A]
  ): IO[(A, FactCacheData)] =
    for {
      generator <- IncrementalFactGenerator.create(processor, prior)
      a         <- body(generator)
      cache     <- generator.buildCacheData()
    } yield (a, cache)

  class GraphProcessor(
      defs: Map[String, Node],
      counts: Map[String, Ref[IO, Int]],
      capture: Option[Ref[IO, Map[String, List[CompilerFactKey[?]]]]]
  ) extends SingleFactProcessor[NumberKey] {
    override protected def generateSingleFact(key: NumberKey): CompilerIO[NumberFact] = {
      val tick = counts.get(key.name).fold(().pure[CompilerIO])(_.update(_ + 1).to[CompilerIO])
      val rec  = capture.fold(().pure[CompilerIO])(ref =>
        activeFactKeys.flatMap(keys => ref.update(_ + (key.name -> keys)).to[CompilerIO])
      )
      tick >> rec >> produce(key)
    }

    private def produce(key: NumberKey): CompilerIO[NumberFact] =
      defs.get(key.name) match {
        case Some(Leaf(src))           => src.get.to[CompilerIO].map(NumberFact(key.name, _))
        case Some(Derived(dep, f))     => getFactOrAbort(NumberKey(dep)).map(d => NumberFact(key.name, f(d.value)))
        case Some(Derived2(d1, d2, f)) =>
          for {
            a <- getFactOrAbort(NumberKey(d1))
            b <- getFactOrAbort(NumberKey(d2))
          } yield NumberFact(key.name, f(a.value, b.value))
        case Some(Writer(dep, eff))    => getFactOrAbort(NumberKey(dep)) >> eff.to[CompilerIO].as(NumberFact(key.name, 1))
        case Some(Failing(message))    => registerCompilerError(err(message)) >> abort[NumberFact]
        case None                      => abort[NumberFact]
      }
  }
}
