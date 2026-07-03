package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.data.Chain
import cats.effect.unsafe.implicits.global
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.unify.Unifier
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.source.content.Sourced
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URI
import scala.annotation.tailrec

/** The effect lifter's decision tables and wrap shapes (docs/effect-lift-in-checker.md, Step 3), over directly
  * constructed [[SemValue]]s / [[SemExpression]]s — no pipeline. The lifter's arms are consulted only after
  * unify-and-coerce failed, so these tests pin exactly what fires (a carrier-headed argument whose payload fits) and
  * what declines (a plain constructor, a payload mismatch, an effectful term against a carrier expectation).
  */
class EffectLifterTest extends AnyFlatSpec with Matchers {

  private val uri                                     = URI.create("Test.els")
  private val anchor: Sourced[OperatorResolvedExpression] =
    Sourced(uri, PositionRange.zero, OperatorResolvedExpression.StringLiteral(Sourced(uri, PositionRange.zero, "x")))

  private def fqn(name: String): ValueFQN =
    ValueFQN(ModuleName(Seq("test"), "M"), QualifiedName(name, Qualifier.Type))

  private val io     = VTopDef(fqn("IO"), None, Spine.SNil)
  private val box    = VTopDef(fqn("Box"), None, Spine.SNil)
  private val string = VTopDef(fqn("String"), None, Spine.SNil)
  private val unit   = VTopDef(fqn("Unit"), None, Spine.SNil)

  private def applied(head: SemValue, args: SemValue*): SemValue = args.foldLeft(head)(Evaluator.applyValue)

  private def exprOf(tpe: SemValue): SemExpression =
    SemExpression(tpe, SemExpression.StringLiteral(Sourced(uri, PositionRange.zero, "x")))

  private val lifter = new EffectLifter(
    sv => inspect(s => Evaluator.force(sv, s.unifier.metaStore)),
    (l, r, ctx) => modify(s => s.withUnifier(s.unifier.unify(l, r, ctx)))
  )

  private def stateWithMetas(n: Int): (Vector[SemValue.MetaId], CheckState) = {
    val (ids, store) = (0 until n).foldLeft((Vector.empty[SemValue.MetaId], MetaStore.empty)) { case ((acc, s), _) =>
      val (id, next) = s.fresh
      (acc :+ id, next)
    }
    (ids, CheckState.initial.withUnifier(Unifier.create(store, 0)))
  }

  private val ambientIoState: CheckState =
    CheckState.initial.recordAmbientCarriers(Set(CheckState.CarrierHead.TopDef(fqn("IO"))))

  private def run[A](state: CheckState, io: CheckIO[A]): A = runWithState(state, io)._2

  private def runWithState[A](state: CheckState, io: CheckIO[A]): (CheckState, A) =
    io.run(state).run(null).run(Chain.empty).value.unsafeRunSync() match {
      case Right((_, stateAndResult)) => stateAndResult
      case Left(errors)               => fail(s"computation aborted: $errors")
    }

  @tailrec
  private def headRef(se: SemExpression): SemExpression.ValueReference = se.expression match {
    case SemExpression.FunctionApplication(target, _) => headRef(target.value)
    case ref: SemExpression.ValueReference            => ref
    case other                                        => fail(s"no head value reference in: $other")
  }

  // --- effectCarrierSplit (the isEffectCarrierHeaded read) ---

  "effectCarrierSplit" should "split a role-flagged carrier meta application into carrier and payload" in {
    val (ids, st) = stateWithMetas(1)
    run(st.recordEffectCarrier(ids.head), lifter.effectCarrierSplit(applied(VMeta(ids.head, Spine.SNil), string)))
      .shouldBe(Some((VMeta(ids.head, Spine.SNil), string)))
  }

  it should "not split a plain constructor application (Box[String] is not a carrier)" in {
    run(CheckState.initial, lifter.effectCarrierSplit(applied(box, string))) shouldBe None
  }

  it should "not split a bare (unapplied) carrier meta — a carrier type needs a payload" in {
    val (ids, st) = stateWithMetas(1)
    run(st.recordEffectCarrier(ids.head), lifter.effectCarrierSplit(VMeta(ids.head, Spine.SNil))) shouldBe None
  }

  it should "split an ambient concrete carrier application (IO[String] with IO ambient)" in {
    run(ambientIoState, lifter.effectCarrierSplit(applied(io, string))) shouldBe Some((io, string))
  }

  it should "recognize an ambient carrier meta through its solution" in {
    val (ids, st) = stateWithMetas(1)
    val solved    = st
      .recordAmbientCarriers(Set(CheckState.CarrierHead.Meta(ids.head.value)))
      .withUnifier(st.unifier.copy(metaStore = st.unifier.metaStore.solve(ids.head, io)))
    run(solved, lifter.effectCarrierSplit(applied(io, string))) shouldBe Some((io, string))
  }

  // --- mustLiftBeforeUnify / mustPureWrapBeforeUnify (the doomed-postponement pre-arms) ---

  "mustLiftBeforeUnify" should "fire for a carrier-meta application against an under-applied rigid head" in {
    val (ids, st) = stateWithMetas(1)
    val actual    = applied(VMeta(ids.head, Spine.SNil), string)
    run(st.recordEffectCarrier(ids.head), lifter.mustLiftBeforeUnify(actual, string)) shouldBe true
  }

  it should "not fire against an equal-arity rigid head (injectivity decomposes it)" in {
    val (ids, st) = stateWithMetas(1)
    val actual    = applied(VMeta(ids.head, Spine.SNil), string)
    run(st.recordEffectCarrier(ids.head), lifter.mustLiftBeforeUnify(actual, applied(io, string))) shouldBe false
  }

  it should "not fire against another meta application (a storage-slot postponement stays with unification)" in {
    val (ids, st) = stateWithMetas(2)
    val actual    = applied(VMeta(ids.head, Spine.SNil), string)
    run(st.recordEffectCarrier(ids.head), lifter.mustLiftBeforeUnify(actual, applied(VMeta(ids(1), Spine.SNil), string)))
      .shouldBe(false)
  }

  it should "not fire for a concrete carrier head (which mismatches properly and takes the failure path)" in {
    run(ambientIoState, lifter.mustLiftBeforeUnify(applied(io, string), string)) shouldBe false
  }

  "mustPureWrapBeforeUnify" should "fire for a pure rigid term against an ambient carrier-meta application" in {
    val (ids, st) = stateWithMetas(1)
    val ambient   = st.recordAmbientCarriers(Set(CheckState.CarrierHead.Meta(ids.head.value)))
    run(ambient, lifter.mustPureWrapBeforeUnify(string, applied(VMeta(ids.head, Spine.SNil), unit))) shouldBe true
  }

  it should "not fire when the expected carrier is concrete (which mismatches properly)" in {
    run(ambientIoState, lifter.mustPureWrapBeforeUnify(string, applied(io, unit))) shouldBe false
  }

  // --- tryBindLift (ladder arm 3) ---

  "tryBindLift on a rigid slot receiving a carrier-headed argument" should "record a bind at the payload type" in {
    val (ids, st) = stateWithMetas(1)
    val actual    = applied(VMeta(ids.head, Spine.SNil), string)
    run(st.recordEffectCarrier(ids.head), lifter.tryBindLift(anchor, exprOf(actual), actual, string))
      .map(_._2.payload) shouldBe Some(string)
  }

  it should "hand the slot a fresh parameter reference typed at the payload" in {
    val (ids, st) = stateWithMetas(1)
    val actual    = applied(VMeta(ids.head, Spine.SNil), string)
    run(st.recordEffectCarrier(ids.head), lifter.tryBindLift(anchor, exprOf(actual), actual, string))
      .map(_._1.expressionType) shouldBe Some(string)
  }

  it should "thread fresh $eff$N binder names across successive lifts" in {
    val (ids, st) = stateWithMetas(1)
    val actual    = applied(VMeta(ids.head, Spine.SNil), string)
    val both      = for {
      first  <- lifter.tryBindLift(anchor, exprOf(actual), actual, string)
      second <- lifter.tryBindLift(anchor, exprOf(actual), actual, string)
    } yield Seq(first, second).flatten.map(_._2.name)
    run(st.recordEffectCarrier(ids.head), both) shouldBe Seq("$eff$0", "$eff$1")
  }

  it should "not lift a plain constructor mismatch (Box[String] into a String slot)" in {
    run(CheckState.initial, lifter.tryBindLift(anchor, exprOf(applied(box, string)), applied(box, string), string))
      .shouldBe(None)
  }

  it should "not lift when the payload does not unify with the slot (C[Unit] into a String slot)" in {
    val (ids, st) = stateWithMetas(1)
    val actual    = applied(VMeta(ids.head, Spine.SNil), unit)
    run(st.recordEffectCarrier(ids.head), lifter.tryBindLift(anchor, exprOf(actual), actual, string)) shouldBe None
  }

  it should "lift an ambient-carrier-headed argument (IO[String] into a String slot)" in {
    run(ambientIoState, lifter.tryBindLift(anchor, exprOf(applied(io, string)), applied(io, string), string))
      .map(_._2.carrier) shouldBe Some(io)
  }

  // --- tryPureWrap (ladder arm 4) ---

  "tryPureWrap against an ambient-carrier-typed expectation" should "wrap with Effect.pure" in {
    run(ambientIoState, lifter.tryPureWrap(anchor, exprOf(string), string, applied(io, string)))
      .map(w => headRef(w).valueName.value) shouldBe Some(WellKnownTypes.effectPureFQN)
  }

  it should "carry the [C, T] type arguments on the pure reference" in {
    run(ambientIoState, lifter.tryPureWrap(anchor, exprOf(string), string, applied(io, string)))
      .map(w => headRef(w).typeArguments) shouldBe Some(Seq(io, string))
  }

  it should "type the wrapped node at the expected carrier type" in {
    run(ambientIoState, lifter.tryPureWrap(anchor, exprOf(string), string, applied(io, string)))
      .map(_.expressionType) shouldBe Some(applied(io, string))
  }

  it should "not wrap when the expected head is not an ambient carrier (Box[String])" in {
    run(ambientIoState, lifter.tryPureWrap(anchor, exprOf(string), string, applied(box, string))) shouldBe None
  }

  it should "not wrap an already carrier-headed term (never double-wrap or strip)" in {
    run(ambientIoState, lifter.tryPureWrap(anchor, exprOf(applied(io, string)), applied(io, string), applied(io, string)))
      .shouldBe(None)
  }

  it should "not wrap when the pure type does not unify with the payload (Unit into IO[String])" in {
    run(ambientIoState, lifter.tryPureWrap(anchor, exprOf(unit), unit, applied(io, string))) shouldBe None
  }

  // --- wrapBinds / bindWrap (the machinery-node assembly) ---

  private def bindOf(name: String = "$eff$0"): EffectLifter.Bind =
    EffectLifter.Bind(name, anchor, exprOf(applied(io, string)), applied(io, string), io, string)

  "wrapBinds over a pure core" should "select map innermost" in {
    headRef(run(ambientIoState, lifter.wrapBinds(exprOf(unit), unit, Seq(bindOf())))._1)
      .valueName.value shouldBe WellKnownTypes.effectMapFQN
  }

  it should "carry the [C, T', R] type arguments on the combinator" in {
    headRef(run(ambientIoState, lifter.wrapBinds(exprOf(unit), unit, Seq(bindOf())))._1)
      .typeArguments shouldBe Seq(io, string, unit)
  }

  it should "type the wrap at the carrier applied to the core type" in {
    run(ambientIoState, lifter.wrapBinds(exprOf(unit), unit, Seq(bindOf())))._2 shouldBe applied(io, unit)
  }

  "wrapBinds over a carrier-headed core" should "select flatMap" in {
    headRef(run(ambientIoState, lifter.wrapBinds(exprOf(applied(io, unit)), applied(io, unit), Seq(bindOf())))._1)
      .valueName.value shouldBe WellKnownTypes.effectFlatMapFQN
  }

  it should "carry the core's payload as the [C, T', R] result argument" in {
    headRef(run(ambientIoState, lifter.wrapBinds(exprOf(applied(io, unit)), applied(io, unit), Seq(bindOf())))._1)
      .typeArguments shouldBe Seq(io, string, unit)
  }

  it should "keep the core's carrier-headed type as the wrap type" in {
    run(ambientIoState, lifter.wrapBinds(exprOf(applied(io, unit)), applied(io, unit), Seq(bindOf())))
      ._2 shouldBe applied(io, unit)
  }

  "bindWrap over a carrier-headed core" should "unify the bind's carrier with the core's carrier" in {
    val (ids, st) = stateWithMetas(1)
    val ambient   = st
      .recordEffectCarrier(ids.head)
      .recordAmbientCarriers(Set(CheckState.CarrierHead.TopDef(fqn("IO"))))
    val bind      = EffectLifter.Bind(
      "$eff$0",
      anchor,
      exprOf(applied(VMeta(ids.head, Spine.SNil), string)),
      applied(VMeta(ids.head, Spine.SNil), string),
      VMeta(ids.head, Spine.SNil),
      string
    )
    val (endState, _) = runWithState(ambient, lifter.bindWrap(bind, exprOf(applied(io, unit)), applied(io, unit)))
    Evaluator.force(VMeta(ids.head, Spine.SNil), endState.unifier.metaStore) shouldBe io
  }

  "wrapBinds with two binds" should "use flatMap for the outer bind (its continuation is the inner wrap)" in {
    headRef(run(ambientIoState, lifter.wrapBinds(exprOf(unit), unit, Seq(bindOf("$eff$0"), bindOf("$eff$1"))))._1)
      .valueName.value shouldBe WellKnownTypes.effectFlatMapFQN
  }

  "wrapBinds over a still-flex core type" should "default to map (a wrong default errors loudly downstream)" in {
    val (ids, st) = stateWithMetas(2)
    val flagged   = st.recordEffectCarrier(ids.head)
    val bind      = EffectLifter.Bind(
      "$eff$0",
      anchor,
      exprOf(applied(VMeta(ids.head, Spine.SNil), string)),
      applied(VMeta(ids.head, Spine.SNil), string),
      VMeta(ids.head, Spine.SNil),
      string
    )
    headRef(run(flagged, lifter.wrapBinds(exprOf(VMeta(ids(1), Spine.SNil)), VMeta(ids(1), Spine.SNil), Seq(bind)))._1)
      .valueName.value shouldBe WellKnownTypes.effectMapFQN
  }
}
