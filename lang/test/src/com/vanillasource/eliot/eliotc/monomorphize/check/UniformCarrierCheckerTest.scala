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
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression

import scala.annotation.tailrec
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

  private val uri                                         = URI.create("Test.els")
  private val anchor: Sourced[OperatorResolvedExpression] =
    Sourced(uri, PositionRange.zero, OperatorResolvedExpression.StringLiteral(Sourced(uri, PositionRange.zero, "x")))
  private def exprOf(tpe: SemValue): SemExpression        =
    SemExpression(tpe, SemExpression.StringLiteral(Sourced(uri, PositionRange.zero, "x")))

  @tailrec
  private def headRef(se: SemExpression): SemExpression.ValueReference = se.expression match {
    case SemExpression.FunctionApplication(target, _) => headRef(target.value)
    case ref: SemExpression.ValueReference            => ref
    case other                                        => fail(s"no head value reference in: $other")
  }

  /** The immediate argument expression of an application node (for peeking at a wrapped inner node). */
  private def argOf(se: SemExpression): SemExpression = se.expression match {
    case SemExpression.FunctionApplication(_, argument) => argument.value
    case other                                          => fail(s"not an application: $other")
  }

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

  // --- carrierSlotLift (the pure-actual re-carry node, reusing EffectLifter mechanics) ---

  "carrierSlotLift" should "wrap the actual in pure at the expected carrier, over a runId unwrap" in {
    val node = UniformCarrierChecker.carrierSlotLift(io, string, exprOf(id(string)), anchor)
    headRef(node).valueName.value shouldBe WellKnownTypes.effectPureFQN
  }

  it should "carry the [carrier, payload] type arguments on the pure reference" in {
    headRef(UniformCarrierChecker.carrierSlotLift(io, string, exprOf(id(string)), anchor)).typeArguments shouldBe Seq(io, string)
  }

  it should "type the whole node at carrier[payload]" in {
    UniformCarrierChecker.carrierSlotLift(io, string, exprOf(id(string)), anchor).expressionType shouldBe applied(io, string)
  }

  it should "unwrap the Id-carried actual with runId before re-wrapping" in {
    headRef(argOf(UniformCarrierChecker.carrierSlotLift(io, string, exprOf(id(string)), anchor))).valueName.value shouldBe WellKnownTypes.runIdFQN
  }

  // --- resolveArgumentSlot (the node-producing slot resolution) ---

  "resolveArgumentSlot at a Generic slot" should "pass the whole carrier-headed action through unchanged" in {
    val (ids, st)  = stateWithMetas(1)
    val outcome    = run(st, checker.resolveArgumentSlot(anchor, exprOf(applied(io, string)), applied(io, string), VMeta(ids.head, Spine.SNil)))
    outcome shouldBe UniformCarrierChecker.UniformSlotOutcome.Passed(exprOf(applied(io, string)))
  }

  "resolveArgumentSlot at a carrier slot receiving an effectful actual" should "pass it through and join the carrier" in {
    val slotType             = applied(io, string) // IO[String], an IO-ambient CarrierSlot
    val (endState, outcome)  = runWithState(ambientIoState, checker.resolveArgumentSlot(anchor, exprOf(applied(io, string)), applied(io, string), slotType))
    outcome shouldBe UniformCarrierChecker.UniformSlotOutcome.Passed(exprOf(applied(io, string)))
    endState.unifier.errors shouldBe empty
  }

  "resolveArgumentSlot at a carrier slot receiving a pure actual" should "re-carry it with a pure lift at the ambient meta" in {
    val (ids, st) = stateWithMetas(1)
    val flagged   = st.recordEffectCarrier(ids.head)
    val slotType  = applied(VMeta(ids.head, Spine.SNil), string) // ?G[String], an effect-carrier CarrierSlot
    val outcome   = run(flagged, checker.resolveArgumentSlot(anchor, exprOf(id(string)), id(string), slotType))
    headRef(outcome.slotExpr).valueName.value shouldBe WellKnownTypes.effectPureFQN
  }

  "resolveArgumentSlot at a payload slot receiving an effectful actual" should "bind at the call site" in {
    val outcome = run(CheckState.initial, checker.resolveArgumentSlot(anchor, exprOf(applied(io, string)), applied(io, string), string))
    outcome match {
      case UniformCarrierChecker.UniformSlotOutcome.Bound(slotExpr, bind) =>
        (slotExpr.expressionType, bind.carrier, bind.payload, bind.name) shouldBe (string, io, string, "$eff$0")
      case other                                                          => fail(s"expected a Bound outcome, got $other")
    }
  }

  "resolveArgumentSlot at a payload slot receiving a pure actual" should "pass its payload directly via runId, not bind (a bind would strip an effectful core's carrier)" in {
    run(CheckState.initial, checker.resolveArgumentSlot(anchor, exprOf(id(string)), id(string), string)) match {
      case UniformCarrierChecker.UniformSlotOutcome.Passed(slotExpr) =>
        (headRef(slotExpr).valueName.value, headRef(slotExpr).typeArguments, slotExpr.expressionType) shouldBe
          (WellKnownTypes.runIdFQN, Seq(string), string)
      case other                                                     => fail(s"expected a Passed(runId ...) outcome, got $other")
    }
  }

  // --- intoCarrierHeadedTerm (the eager term-level pure carrier-wrap) ---

  "intoCarrierHeadedTerm" should "wrap a pure term's value in pure@Effect[Id]" in {
    val node = run(CheckState.initial, checker.intoCarrierHeadedTerm(exprOf(string), anchor))
    (headRef(node).valueName.value, headRef(node).typeArguments, node.expressionType) shouldBe
      (WellKnownTypes.effectPureFQN, Seq(EffectLifter.idCarrier, string), id(string))
  }

  it should "leave an already Id-headed term unchanged" in {
    run(CheckState.initial, checker.intoCarrierHeadedTerm(exprOf(id(string)), anchor)) shouldBe exprOf(id(string))
  }

  it should "leave an already effectful (ambient IO) term unchanged" in {
    run(ambientIoState, checker.intoCarrierHeadedTerm(exprOf(applied(io, string)), anchor)) shouldBe exprOf(applied(io, string))
  }

  it should "never wrap a type-level term (VType — the §8 boundary)" in {
    run(CheckState.initial, checker.intoCarrierHeadedTerm(exprOf(VType), anchor)) shouldBe exprOf(VType)
  }

  // --- checkReturnBoundary (the uniform return-boundary resolver) ---

  "checkReturnBoundary of a pure body against a pure return" should "re-carry via a (downstream-erased) pure@Id lift" in {
    val node = run(CheckState.initial, checker.checkReturnBoundary(exprOf(id(string)), id(string), string, anchor))
    (headRef(node).valueName.value, headRef(node).typeArguments) shouldBe (WellKnownTypes.effectPureFQN, Seq(EffectLifter.idCarrier, string))
  }

  "checkReturnBoundary of a pure body against an ambient IO return" should "lift the body into IO with pure@Effect[IO]" in {
    val node = run(ambientIoState, checker.checkReturnBoundary(exprOf(id(string)), id(string), applied(io, string), anchor))
    (headRef(node).valueName.value, headRef(node).typeArguments) shouldBe (WellKnownTypes.effectPureFQN, Seq(io, string))
  }

  "checkReturnBoundary of an effectful body against an ambient IO return" should "pass it through unchanged, no error" in {
    val (endState, node) = runWithState(ambientIoState, checker.checkReturnBoundary(exprOf(applied(io, string)), applied(io, string), applied(io, string), anchor))
    (node, endState.unifier.errors.isEmpty) shouldBe (exprOf(applied(io, string)), true)
  }

  "checkReturnBoundary of an effectful body against a pure return" should "leave the carrier to default to Id (caught downstream)" in {
    // A pure-declared value with an effectful body: the join defaults the body's carrier to Id rather than erroring at
    // the boundary; the effect operation's Id instance then fails to resolve (the loud fail-safe, as on the default path).
    val (ids, st) = stateWithMetas(1)
    val flagged   = st.recordEffectCarrier(ids.head)
    val (endState, _) = runWithState(flagged, checker.checkReturnBoundary(exprOf(applied(VMeta(ids.head, Spine.SNil), string)), applied(VMeta(ids.head, Spine.SNil), string), string, anchor))
    endState.unifier.errors shouldBe empty
  }
}
