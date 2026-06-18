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

  it should "delegate activeFactKeys to the underlying process" in {
    tracked(Map.empty, active = List(Key("ancestor"))) { (process, _, _) =>
      process.activeFactKeys
    }.asserting(_ shouldBe List(Key("ancestor")))
  }

  /** Build a [[DependencyTrackingProcess]] for `owner` over a stub process with the given facts, exposing the
    * dependency map and a record of facts registered through it.
    */
  private def tracked[A](facts: Map[CompilerFactKey[?], CompilerFact], active: List[CompilerFactKey[?]] = Nil)(
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
                      override def getFact[V <: CompilerFact, K <: CompilerFactKey[V]](key: K): IO[Option[V]] =
                        IO.pure(facts.get(key).map(_.asInstanceOf[V]))
                      override def registerFact(value: CompilerFact): IO[Unit] = registered.update(_ :+ value)
                      override def activeFactKeys: IO[List[CompilerFactKey[?]]] = IO.pure(active)
                    }
      result     <- body(new DependencyTrackingProcess(underlying, owner, deps), deps, registered)
    } yield result
}

object DependencyTrackingProcessTest {
  case class Fact(name: String) extends CompilerFact {
    override def key(): CompilerFactKey[Fact] = Key(name)
  }
  case class Key(name: String) extends CompilerFactKey[Fact]
}
