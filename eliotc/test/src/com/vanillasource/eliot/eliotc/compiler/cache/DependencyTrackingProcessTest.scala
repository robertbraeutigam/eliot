package com.vanillasource.eliot.eliotc.compiler.cache

import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.{IO, Ref}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.{CompilationProcess, CompilerFact, CompilerFactKey}
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers

class DependencyTrackingProcessTest extends AsyncFlatSpec with AsyncIOSpec with Matchers {
  import DependencyTrackingProcessTest.*

  private val owner = Key("owner")

  "DependencyTrackingProcess" should "return the underlying fact for a hit" in {
    tracked(Map(Key("a") -> Fact("a"))) { (process, _, _) =>
      process.getFact(Key("a"))
    }.asserting(_ shouldBe Some(Fact("a")))
  }

  it should "record a read key as the owner's dependency" in {
    tracked(Map(Key("a") -> Fact("a"))) { (process, deps, _) =>
      process.getFact(Key("a")) >> deps.get
    }.asserting(_ shouldBe Map(owner -> Set(Key("a"))))
  }

  it should "record the dependency even when the read is a miss" in {
    tracked(Map.empty) { (process, deps, _) =>
      process.getFact(Key("missing")) >> deps.get
    }.asserting(_ shouldBe Map(owner -> Set(Key("missing"))))
  }

  it should "accumulate every read key under the owner" in {
    tracked(Map(Key("a") -> Fact("a"), Key("b") -> Fact("b"))) { (process, deps, _) =>
      process.getFact(Key("a")) >> process.getFact(Key("b")) >> deps.get
    }.asserting(_ shouldBe Map(owner -> Set(Key("a"), Key("b"))))
  }

  it should "delegate registerFact to the underlying process without recording a dependency" in {
    tracked(Map.empty) { (process, deps, registered) =>
      process.registerFact(Fact("x")) >> (deps.get, registered.get).tupled
    }.asserting(_ shouldBe (Map.empty, List(Fact("x"))))
  }

  it should "expose its own key prepended to the ancestor chain as the active fact keys" in {
    tracked(Map.empty, ancestors = List(Key("ancestor"))) { (process, _, _) =>
      process.activeFactKeys
    }.asserting(_ shouldBe List(owner, Key("ancestor")))
  }

  it should "forward its own key and ancestor chain as the ancestors of reads on the underlying process" in {
    for {
      seen      <- Ref.of[IO, List[CompilerFactKey[?]]](Nil)
      underlying = new CompilationProcess {
                     override def getFact[V <: CompilerFact, K <: CompilerFactKey[V]](
                         key: K,
                         ancestors: List[CompilerFactKey[?]]
                     ): IO[Option[V]]                                          = seen.set(ancestors).as(None)
                     override def registerFact(value: CompilerFact): IO[Unit] = IO.unit
                   }
      deps      <- Ref.of[IO, Map[CompilerFactKey[?], Set[CompilerFactKey[?]]]](Map.empty)
      process    = new DependencyTrackingProcess(underlying, owner, deps, List(Key("ancestor")))
      _         <- process.getFact(Key("a"))
      result    <- seen.get
    } yield result
  }.asserting(_ shouldBe List(owner, Key("ancestor")))

  /** Build a [[DependencyTrackingProcess]] for `owner` (requested under `ancestors`) over a stub process with the given
    * facts, exposing the dependency map and a record of facts registered through it.
    */
  private def tracked[A](facts: Map[CompilerFactKey[?], CompilerFact], ancestors: List[CompilerFactKey[?]] = Nil)(
      body: (
          DependencyTrackingProcess,
          Ref[IO, Map[CompilerFactKey[?], Set[CompilerFactKey[?]]]],
          Ref[IO, List[CompilerFact]]
      ) => IO[A]
  ): IO[A] =
    for {
      deps       <- Ref.of[IO, Map[CompilerFactKey[?], Set[CompilerFactKey[?]]]](Map.empty)
      registered <- Ref.of[IO, List[CompilerFact]](List.empty)
      underlying  = new CompilationProcess {
                      override def getFact[V <: CompilerFact, K <: CompilerFactKey[V]](
                          key: K,
                          ancestors: List[CompilerFactKey[?]]
                      ): IO[Option[V]]                                          =
                        IO.pure(facts.get(key).map(_.asInstanceOf[V]))
                      override def registerFact(value: CompilerFact): IO[Unit] = registered.update(_ :+ value)
                    }
      result     <- body(new DependencyTrackingProcess(underlying, owner, deps, ancestors), deps, registered)
    } yield result
}

object DependencyTrackingProcessTest {
  case class Fact(name: String) extends CompilerFact {
    override def key(): CompilerFactKey[Fact] = Key(name)
  }
  case class Key(name: String) extends CompilerFactKey[Fact]
}
