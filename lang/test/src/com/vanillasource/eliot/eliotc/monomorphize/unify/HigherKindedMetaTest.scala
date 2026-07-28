package com.vanillasource.eliot.eliotc.monomorphize.unify

import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.source.content.Sourced
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URI

/** The per-meta *kind* bookkeeping ([[Unifier.higherKindedMetas]]): which instantiation metas were peeled from a
  * higher-kinded (`[F[_]]`) binder, and to what kind. Post-drain kind verification reads the kind; the compiler
  * track's two carrier readers key on the membership itself.
  */
class HigherKindedMetaTest extends AnyFlatSpec with Matchers {
  private val ctx: Sourced[String] = Sourced(URI.create("Test.els"), PositionRange.zero, "ctx")

  /** Allocate `n` fresh metavariables, returning their ids and a fresh unifier over them. */
  private def withMetas(n: Int): (Vector[SemValue.MetaId], Unifier) = {
    val (ids, store) = (0 until n).foldLeft((Vector.empty[SemValue.MetaId], MetaStore.empty)) { case ((acc, s), _) =>
      val (id, next) = s.fresh
      (acc :+ id, next)
    }
    (ids, Unifier.create(store, 0))
  }

  "a fresh metavariable" should "be ordinary — no kind bookkeeping" in {
    val (ids, u) = withMetas(1)
    u.isHigherKindedMeta(ids.head.value) shouldBe false
  }

  "recordHigherKindedMeta" should "expose the meta and its kind via carrierMetas" in {
    val (ids, u) = withMetas(1)
    val id       = ids.head
    u.recordHigherKindedMeta(id, VType, ctx).carrierMetas shouldBe List((id.value, (VType, ctx)))
  }

  it should "make the meta a higher-kinded one (what a carrier head is recognized by)" in {
    val (ids, u) = withMetas(1)
    u.recordHigherKindedMeta(ids.head, VType, ctx).isHigherKindedMeta(ids.head.value) shouldBe true
  }

  it should "list the recorded id (the compiler track's only handle on an inline guard's carrier)" in {
    val (ids, u) = withMetas(2)
    u.recordHigherKindedMeta(ids.head, VType, ctx).higherKindedMetaIds shouldBe List(ids.head.value)
  }
}
