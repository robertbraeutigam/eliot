package com.vanillasource.eliot.eliotc.monomorphize.unify

import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.source.content.Sourced
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URI

/** D2: the single `metaRoles` map replaces the former six side-sets. These tests pin the role lifecycle (a fresh meta is
  * [[MetaRole.Plain]]) and the carrier-kind / effect-carrier queries the post-check pipeline reads.
  *
  * (The protected-set / abstract-associated-type machinery this doc once described — and the `Combine`-branch-join roles
  * `combinable`/`candidates`/`combineResolved`/upper-bounds it once exercised — were removed with the associated-types
  * lane and the refinement channel's flag day; see `docs/bounds-as-refinements.md` Steps 7b/7c. `defaultUnsolvedMetas`
  * now defaults every unsolved meta to `Type`, both roles alike.)
  */
class UnifierRoleTest extends AnyFlatSpec with Matchers {
  private val ctx: Sourced[String] = Sourced(URI.create("Test.els"), PositionRange.zero, "ctx")

  private def fqn(name: String, qualifier: Qualifier): ValueFQN =
    ValueFQN(ModuleName(Seq("test"), "M"), QualifiedName(name, qualifier))

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

  "recordCarrierKind" should "expose the meta and its kind via carrierMetas" in {
    val (ids, u) = withMetas(1)
    val id       = ids.head
    u.recordCarrierKind(id, VType, ctx).carrierMetas shouldBe List((id.value, (VType, ctx)))
  }

  "recordEffectCarrier" should "flag the meta as an effect carrier" in {
    val (ids, u) = withMetas(1)
    u.recordEffectCarrier(ids.head).isEffectCarrier(ids.head.value) shouldBe true
  }

  it should "preserve an already-recorded carrier kind" in {
    val (ids, u) = withMetas(1)
    val id       = ids.head
    u.recordCarrierKind(id, VType, ctx).recordEffectCarrier(id).carrierMetas shouldBe List((id.value, (VType, ctx)))
  }

  "a carrier-kinded meta without recordEffectCarrier" should "not be an effect carrier (a bare HKT binder stays unliftable)" in {
    val (ids, u) = withMetas(1)
    u.recordCarrierKind(ids.head, VType, ctx).isEffectCarrier(ids.head.value) shouldBe false
  }
}
