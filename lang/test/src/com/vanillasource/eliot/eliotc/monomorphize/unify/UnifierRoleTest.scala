package com.vanillasource.eliot.eliotc.monomorphize.unify

import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.source.content.Sourced
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URI

/** D2: the single `metaRoles` map replaces the former six side-sets. These tests pin the role lifecycle and the
  * finalization-relevant queries the post-check pipeline reads — in particular that the protected set is *exactly* the
  * [[MetaRole.AbstractAssoc]] metas, which is what makes `defaultUnsolvedMetas`'s total role match default everything
  * else to `Type` while leaving only abstract-associated types abstract (the F2 catch-all is gone).
  */
class UnifierRoleTest extends AnyFlatSpec with Matchers {
  private val ctx: Sourced[String] = Sourced(URI.create("Test.els"), PositionRange.zero, "ctx")

  private def fqn(name: String, qualifier: Qualifier): ValueFQN =
    ValueFQN(ModuleName(Seq("test"), "M"), QualifiedName(name, qualifier))

  private def topDef(name: String): SemValue = VTopDef(fqn(name, Qualifier.Default), None, Spine.SNil)

  /** Allocate `n` fresh metavariables, returning their ids and a fresh unifier over them. */
  private def withMetas(n: Int): (Vector[SemValue.MetaId], Unifier) = {
    val (ids, store) = (0 until n).foldLeft((Vector.empty[SemValue.MetaId], MetaStore.empty)) { case ((acc, s), _) =>
      val (id, next) = s.fresh
      (acc :+ id, next)
    }
    (ids, Unifier.create(store, 0))
  }

  "a fresh metavariable" should "have role Plain" in {
    val (ids, u) = withMetas(1)
    u.roleOf(ids.head.value) shouldBe MetaRole.Plain
  }

  "markCombinable" should "promote a Plain meta to a combinable Instantiation" in {
    val (ids, u) = withMetas(1)
    u.markCombinable(ids.head).roleOf(ids.head.value) shouldBe MetaRole.Instantiation()
  }

  "two distinct contributions to a combinable meta" should "accumulate as candidates without erroring" in {
    val (ids, u) = withMetas(1)
    val id       = ids.head
    val result   = u.markCombinable(id).unify(topDef("A"), VMeta(id, Spine.SNil), ctx).unify(topDef("B"), VMeta(id, Spine.SNil), ctx)
    result.candidatesOf(id.value).map(_._1) shouldBe List(topDef("A"), topDef("B"))
  }

  it should "leave the unifier free of errors (the second contribution is deferred, not rejected)" in {
    val (ids, u) = withMetas(1)
    val id       = ids.head
    val result   = u.markCombinable(id).unify(topDef("A"), VMeta(id, Spine.SNil), ctx).unify(topDef("B"), VMeta(id, Spine.SNil), ctx)
    result.errors shouldBe empty
  }

  it should "surface the meta as an unresolved candidate target" in {
    val (ids, u) = withMetas(1)
    val id       = ids.head
    val result   = u.markCombinable(id).unify(topDef("A"), VMeta(id, Spine.SNil), ctx).unify(topDef("B"), VMeta(id, Spine.SNil), ctx)
    result.unresolvedCandidateMetas.map(_._1) shouldBe List(id.value)
  }

  "tainting a combinable meta (a use in a VPi domain)" should "clear combinable but preserve its candidates" in {
    val (ids, u) = withMetas(1)
    val id        = ids.head
    val withCands = u.markCombinable(id).unify(topDef("A"), VMeta(id, Spine.SNil), ctx).unify(topDef("B"), VMeta(id, Spine.SNil), ctx)
    val tainted   = withCands.unify(VPi(VMeta(id, Spine.SNil), _ => VType), VPi(VMeta(id, Spine.SNil), _ => VType), ctx)
    (tainted.isCombinable(id.value), tainted.candidatesOf(id.value).map(_._1)) shouldBe (false, List(topDef("A"), topDef("B")))
  }

  "recordCombineResolved" should "remove a meta from the unresolved candidate set" in {
    val (ids, u) = withMetas(1)
    val id       = ids.head
    val resolved = u.markCombinable(id).unify(topDef("A"), VMeta(id, Spine.SNil), ctx).unify(topDef("B"), VMeta(id, Spine.SNil), ctx).recordCombineResolved(id)
    resolved.unresolvedCandidateMetas shouldBe empty
  }

  "recordCarrierKind" should "expose the meta and its kind via carrierMetas" in {
    val (ids, u) = withMetas(1)
    val id       = ids.head
    u.recordCarrierKind(id, VType, ctx).carrierMetas shouldBe List((id.value, (VType, ctx)))
  }

  "recordUpperBound" should "surface the obligation via pendingUpperBounds" in {
    val (ids, u) = withMetas(1)
    val id       = ids.head
    u.markCombinable(id).recordUpperBound(id, topDef("E"), ctx).pendingUpperBounds shouldBe List((id, topDef("E"), ctx))
  }

  "the finalizer's protected set" should "be exactly the AbstractAssoc metas, not the Plain/combinable/carrier ones" in {
    val (ids, u)                = withMetas(4)
    val Vector(plain, comb, carrier, assoc) = ids
    val classified              = u
      .markCombinable(comb)
      .recordCarrierKind(carrier, VType, ctx)
      .recordAbstractAssoc(assoc, fqn("X", Qualifier.Ability("Ability")))
    classified.abstractAssocMetaIds shouldBe Set(assoc.value)
  }
}
