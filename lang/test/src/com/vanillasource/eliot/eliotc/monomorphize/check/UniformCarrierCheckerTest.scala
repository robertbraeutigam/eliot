package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.data.Chain
import cats.effect.unsafe.implicits.global
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.carrier.{Carrier, UniformLadder}
import com.vanillasource.eliot.eliotc.monomorphize.carrier.UniformLadder.*
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.unify.Unifier
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.source.content.Sourced
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URI

/** The checker-side uniform-carrier bridge ([[UniformCarrierChecker]]), over directly constructed [[SemValue]]s and
  * [[CheckState]]s — no pipeline (the harness mirrors [[EffectLifterTest]]). Pins the §12-Q1 check-time carrier-wrapping
  * ([[UniformCarrierChecker.intoCarrierHeaded]]), the expected-slot classification reading the value's real carrier
  * bookkeeping, and the CheckIO-threaded ladder + boundary finalize/materialize, so the spine-loop flip (U3a-2) can rely
  * on it.
  */
class UniformCarrierCheckerTest extends AnyFlatSpec with Matchers {

  private val ctx: Sourced[String] = Sourced(URI.create("Test.els"), PositionRange.zero, "ctx")

  private def fqn(name: String): ValueFQN =
    ValueFQN(ModuleName(Seq("test"), "M"), QualifiedName(name, Qualifier.Type))

  private val string = VTopDef(fqn("String"), None, Spine.SNil)
  private val unit   = VTopDef(fqn("Unit"), None, Spine.SNil)
  private val io     = VTopDef(fqn("IO"), None, Spine.SNil)
  private val ioFQN  = fqn("IO")

  private def applied(head: SemValue, args: SemValue*): SemValue = args.foldLeft(head)(Evaluator.applyValue)
  private def id(p: SemValue): SemValue                          = VTopDef(WellKnownTypes.idFQN, None, Spine.SNil :+ p)
  private def list(p: SemValue): SemValue                        = VTopDef(fqn("List"), None, Spine.SNil :+ p)

  private val lifter = new EffectLifter(
    sv => inspect(s => Evaluator.force(sv, s.unifier.metaStore)),
    (l, r, c) => modify(s => s.withUnifier(s.unifier.unify(l, r, c)))
  )

  private val checker = new UniformCarrierChecker(
    sv => inspect(s => Evaluator.force(sv, s.unifier.metaStore)),
    lifter.effectCarrierSplit
  )

  private def stateWithMetas(n: Int): (Vector[SemValue.MetaId], CheckState) = {
    val (ids, store) = (0 until n).foldLeft((Vector.empty[SemValue.MetaId], MetaStore.empty)) { case ((acc, s), _) =>
      val (id, next) = s.fresh
      (acc :+ id, next)
    }
    (ids, CheckState.initial.withUnifier(Unifier.create(store, 0)))
  }

  private val ambientIoState: CheckState =
    CheckState.initial.recordAmbientCarriers(Set(CheckState.CarrierHead.TopDef(ioFQN)))

  private def run[A](state: CheckState, io: CheckIO[A]): A = runWithState(state, io)._2

  private def runWithState[A](state: CheckState, io: CheckIO[A]): (CheckState, A) =
    io.run(state).run(null).run(Chain.empty).value.unsafeRunSync() match {
      case Right((_, stateAndResult)) => stateAndResult
      case Left(errors)               => fail(s"computation aborted: $errors")
    }

  // --- intoCarrierHeaded (the §12-Q1 check-time wrapping) ---

  "intoCarrierHeaded" should "wrap a pure type in Id" in {
    run(CheckState.initial, checker.intoCarrierHeaded(string)) shouldBe id(string)
  }

  it should "leave an already Id-headed type untouched" in {
    run(CheckState.initial, checker.intoCarrierHeaded(id(string))) shouldBe id(string)
  }

  it should "leave an ambient concrete carrier (IO[String]) untouched" in {
    run(ambientIoState, checker.intoCarrierHeaded(applied(io, string))) shouldBe applied(io, string)
  }

  it should "leave an ambient carrier-meta application untouched" in {
    val (ids, st) = stateWithMetas(1)
    val carried   = applied(VMeta(ids.head, Spine.SNil), string)
    run(st.recordEffectCarrier(ids.head), checker.intoCarrierHeaded(carried)) shouldBe carried
  }

  it should "never wrap a type-level judgment (VType — the §8 compile-time boundary)" in {
    run(CheckState.initial, checker.intoCarrierHeaded(VType)) shouldBe VType
  }

  it should "wrap a pure data container (List[String]) in Id, not treat List as a carrier" in {
    run(CheckState.initial, checker.intoCarrierHeaded(list(string))) shouldBe id(list(string))
  }

  // --- classifyExpectedSlot (the surviving positional recognition, on the expected side) ---

  "classifyExpectedSlot" should "classify a bare flex meta as Generic" in {
    val (ids, st) = stateWithMetas(1)
    run(st, checker.classifyExpectedSlot(VMeta(ids.head, Spine.SNil))) shouldBe ExpectedSlot.Generic(ids.head)
  }

  it should "classify an ambient carrier-meta application as a CarrierSlot" in {
    val (ids, st) = stateWithMetas(1)
    run(st.recordEffectCarrier(ids.head), checker.classifyExpectedSlot(applied(VMeta(ids.head, Spine.SNil), string)))
      .shouldBe(ExpectedSlot.CarrierSlot(Carrier.Var(ids.head), string))
  }

  it should "classify a plain data container (List[String]) as a PayloadSlot (told apart only by the tag)" in {
    run(CheckState.initial, checker.classifyExpectedSlot(list(string))) shouldBe ExpectedSlot.PayloadSlot(list(string))
  }

  // --- resolveSlot (the CheckIO-threaded ladder) ---

  "resolveSlot on an effect-carrier slot receiving an IO actual" should "join the ambient meta to IO in the state" in {
    val (ids, st) = stateWithMetas(2) // ids(0) = ambient carrier meta, ids(1) = slot payload meta
    val flagged   = st.recordEffectCarrier(ids.head)
    val slot      = ExpectedSlot.CarrierSlot(Carrier.Var(ids.head), VMeta(ids(1), Spine.SNil))
    val (endState, outcome) = runWithState(flagged, checker.resolveSlot(applied(io, unit), slot, ctx))
    Evaluator.force(VMeta(ids.head, Spine.SNil), endState.unifier.metaStore) shouldBe io
    outcome shouldBe Outcome.PassJoin(None) // an effectful actual leaves no deferred pure
  }

  "resolveSlot on an effect-carrier slot receiving a pure actual" should "record a deferred pure and join nothing" in {
    val (ids, st) = stateWithMetas(2)
    val flagged   = st.recordEffectCarrier(ids.head)
    val slot      = ExpectedSlot.CarrierSlot(Carrier.Var(ids.head), VMeta(ids(1), Spine.SNil))
    run(flagged, checker.resolveSlot(id(unit), slot, ctx)) shouldBe Outcome.PassJoin(Some(DeferredLift.LiftPure(Carrier.Var(ids.head))))
  }

  // --- finalizeAndMaterialize (boundary defaulting + decision-free materialization) ---

  "finalizeAndMaterialize" should "erase a pure conditional's lift by defaulting its untouched ambient meta to Id" in {
    val (ids, st) = stateWithMetas(1)
    val flagged   = st.recordEffectCarrier(ids.head) // ambient carrier meta, never joined by any effectful arm
    val lift      = DeferredLift.LiftPure(Carrier.Var(ids.head))
    run(flagged, checker.finalizeAndMaterialize(List(lift))).map(_.erased) shouldBe List(true)
  }

  it should "materialize a pure arm's lift at the joined non-Id carrier" in {
    val (ids, st) = stateWithMetas(2)
    val flagged   = st.recordEffectCarrier(ids.head)
    // An effectful sibling joined the ambient meta to IO, then the pure arm recorded its deferred lift.
    val program   = for {
      _        <- checker.resolveSlot(applied(io, string), ExpectedSlot.CarrierSlot(Carrier.Var(ids.head), VMeta(ids(1), Spine.SNil)), ctx)
      pureArm  <- checker.resolveSlot(id(string), ExpectedSlot.CarrierSlot(Carrier.Var(ids.head), VMeta(ids(1), Spine.SNil)), ctx)
      lifts     = pureArm match { case Outcome.PassJoin(Some(l)) => List(l); case _ => Nil }
      result   <- checker.finalizeAndMaterialize(lifts)
    } yield result
    run(flagged, program) shouldBe List(MaterializedLift(LiftKind.Pure, Carrier.Con(ioFQN, Nil)))
  }
}
