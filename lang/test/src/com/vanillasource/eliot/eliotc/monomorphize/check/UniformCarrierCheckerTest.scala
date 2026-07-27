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
  * [[CheckState]]s — no pipeline (the harness mirrors [[EffectLifterTest]]). Pins the actual-form classification
  * ([[UniformCarrierChecker.actualForm]] — the recognition that replaced the manufactured `Id` head, A.8.10), the
  * expected-slot classification reading the value's real carrier bookkeeping, the node-producing argument-slot
  * resolution and the return boundary — the parts the live spine still routes through.
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

  // --- actualForm (the positional recognition that replaced the manufactured Id head) ---

  "actualForm" should "classify a plain type as Pure — never manufacturing an Id head for it" in {
    run(CheckState.initial, checker.actualForm(string)) shouldBe ActualForm.Pure(string)
  }

  it should "classify a genuine Id-headed type as IdCarried, keeping its payload to project" in {
    run(CheckState.initial, checker.actualForm(id(string))) shouldBe ActualForm.IdCarried(id(string), string)
  }

  it should "classify an ambient concrete carrier (IO[String]) as Carried" in {
    run(ambientIoState, checker.actualForm(applied(io, string)))
      .shouldBe(ActualForm.Carried(applied(io, string), Carrier.Con(ioFQN, Nil), string))
  }

  it should "classify an ambient carrier-meta application as Carried" in {
    val (ids, st) = stateWithMetas(1)
    val carried   = applied(VMeta(ids.head, Spine.SNil), string)
    run(st.recordEffectCarrier(ids.head), checker.actualForm(carried))
      .shouldBe(ActualForm.Carried(carried, Carrier.Var(ids.head), string))
  }

  it should "classify a type-level judgment (VType — the §8 compile-time boundary) as Pure" in {
    run(CheckState.initial, checker.actualForm(VType)) shouldBe ActualForm.Pure(VType)
  }

  it should "classify a pure data container (List[String]) as Pure, not treat List as a carrier" in {
    run(CheckState.initial, checker.actualForm(list(string))) shouldBe ActualForm.Pure(list(string))
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

  // --- pureLift (the pure-actual re-carry node, reusing EffectLifter mechanics) ---

  "pureLift" should "wrap the actual in pure at the expected carrier" in {
    val node = UniformCarrierChecker.pureLift(io, string, idCarried = false, exprOf(string), anchor)
    headRef(node).valueName.value shouldBe WellKnownTypes.effectPureFQN
  }

  it should "carry the [carrier, payload] type arguments on the pure reference" in {
    headRef(UniformCarrierChecker.pureLift(io, string, idCarried = false, exprOf(string), anchor)).typeArguments shouldBe Seq(io, string)
  }

  it should "type the whole node at carrier[payload]" in {
    UniformCarrierChecker.pureLift(io, string, idCarried = false, exprOf(string), anchor).expressionType shouldBe applied(io, string)
  }

  it should "wrap a plain actual directly, with no runId round trip" in {
    argOf(UniformCarrierChecker.pureLift(io, string, idCarried = false, exprOf(string), anchor)) shouldBe exprOf(string)
  }

  it should "project a genuine Id-carried actual with runId before re-wrapping" in {
    headRef(argOf(UniformCarrierChecker.pureLift(io, string, idCarried = true, exprOf(id(string)), anchor))).valueName.value shouldBe WellKnownTypes.runIdFQN
  }

  // --- resolveArgumentSlot (the node-producing slot resolution) ---

  "resolveArgumentSlot at a Generic slot" should "pass the whole carrier-headed action through unchanged" in {
    val (ids, st)  = stateWithMetas(1)
    val outcome    = run(st, checker.resolveArgumentSlot(anchor, exprOf(applied(io, string)), applied(io, string), VMeta(ids.head, Spine.SNil)))
    outcome shouldBe exprOf(applied(io, string))
  }

  "resolveArgumentSlot at a carrier slot receiving an effectful actual" should "pass it through and join the carrier" in {
    val slotType             = applied(io, string) // IO[String], an IO-ambient CarrierSlot
    val (endState, outcome)  = runWithState(ambientIoState, checker.resolveArgumentSlot(anchor, exprOf(applied(io, string)), applied(io, string), slotType))
    outcome shouldBe exprOf(applied(io, string))
    endState.unifier.errors shouldBe empty
  }

  "resolveArgumentSlot at a carrier slot receiving a pure actual" should "re-carry it with a pure lift at the ambient meta" in {
    val (ids, st) = stateWithMetas(1)
    val flagged   = st.recordEffectCarrier(ids.head)
    val slotType  = applied(VMeta(ids.head, Spine.SNil), string) // ?G[String], an effect-carrier CarrierSlot
    val outcome   = run(flagged, checker.resolveArgumentSlot(anchor, exprOf(string), string, slotType))
    headRef(outcome).valueName.value shouldBe WellKnownTypes.effectPureFQN
  }

  "resolveArgumentSlot at a payload slot receiving an effectful actual" should "be rejected as a compiler bug — hoisting is the desugar's rewrite, so the slot suspends instead of reaching here" in {
    an[IllegalStateException] should be thrownBy
      run(ambientIoState, checker.resolveArgumentSlot(anchor, exprOf(applied(io, string)), applied(io, string), string))
  }

  "resolveArgumentSlot at a payload slot receiving a pure actual" should "pass the term through untouched — it already IS its payload" in {
    run(CheckState.initial, checker.resolveArgumentSlot(anchor, exprOf(string), string, string)) shouldBe exprOf(string)
  }

  "resolveArgumentSlot at a payload slot receiving a genuine Id-carried actual" should "project its payload with runId" in {
    val outcome = run(CheckState.initial, checker.resolveArgumentSlot(anchor, exprOf(id(string)), id(string), string))
    (headRef(outcome).valueName.value, headRef(outcome).typeArguments, outcome.expressionType) shouldBe
      (WellKnownTypes.runIdFQN, Seq(string), string)
  }

  // --- checkReturnBoundary (the uniform return-boundary resolver) ---

  "checkReturnBoundary of a pure body against a pure return" should "emit no node at all — there is no carrier to lift into" in {
    run(CheckState.initial, checker.checkReturnBoundary(exprOf(string), string, string, anchor)) shouldBe exprOf(string)
  }

  "checkReturnBoundary of a pure body against an ambient IO return" should "lift the body into IO with pure@Effect[IO]" in {
    val node = run(ambientIoState, checker.checkReturnBoundary(exprOf(string), string, applied(io, string), anchor))
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
