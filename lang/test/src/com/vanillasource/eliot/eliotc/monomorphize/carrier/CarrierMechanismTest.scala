package com.vanillasource.eliot.eliotc.monomorphize.carrier

import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.carrier.UniformLadder.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.unify.{SemValuePrinter, Unifier}
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.source.content.Sourced
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import java.net.URI

/** U3a acceptance suite, ported from the U2 foundation spike (since deleted) onto the **real** [[SemValue]] /
  * [[Unifier]] domain. It exercises the productionised carrier mechanism ([[Carrier]] / [[CarrierJoin]] /
  * [[UniformLadder]]) that the checker-wiring slice consumes — none of it is wired into the checker yet, so the default
  * compiler path is byte-identical.
  *
  * The four historical [[com.vanillasource.eliot.eliotc.monomorphize.check.EffectLifter]] failure cases dissolve under
  * one guard-free rule set, and — the sharpest gain of porting to real types — the theft the pre-uniform equal-arity
  * guard existed to block is demonstrated as an *actual* [[Unifier]] injectivity decomposition (the naive-contrast
  * cases run the real `unify`), against which splitting the carrier head off first is the fix.
  */
class CarrierMechanismTest extends AnyFlatSpec with Matchers {
  private val ctx: Sourced[String] = Sourced(URI.create("Test.els"), PositionRange.zero, "ctx")

  // --- constructing real SemValue judgments --------------------------------------------------------------------------

  private def con(name: String, args: SemValue*): SemValue =
    VTopDef(ValueFQN(ModuleName(ModuleName.defaultSystemPackage, name), QualifiedName(name, Qualifier.Type)), None,
      args.foldLeft(Spine.SNil: Spine)(_ :+ _))

  private def meta(id: Int, args: SemValue*): SemValue =
    VMeta(MetaId(id), args.foldLeft(Spine.SNil: Spine)(_ :+ _))

  private val ioFQN: ValueFQN     = ValueFQN(ModuleName(ModuleName.defaultSystemPackage, "IO"), QualifiedName("IO", Qualifier.Type))
  private val stateFQN: ValueFQN  = ValueFQN(ModuleName(ModuleName.defaultSystemPackage, "StateCarrier"), QualifiedName("StateCarrier", Qualifier.Type))

  private def io(p: SemValue): SemValue  = VTopDef(ioFQN, None, Spine.SNil :+ p)
  private def id(p: SemValue): SemValue  = VTopDef(WellKnownTypes.idFQN, None, Spine.SNil :+ p)
  private def list(p: SemValue): SemValue = con("List", p)
  private def opt(p: SemValue): SemValue  = con("Option", p)
  private val string: SemValue = con("String")
  private val intT: SemValue   = con("Int")
  private val unit: SemValue   = con("Unit")

  private val ioCarrier: Carrier = Carrier.Con(ioFQN, Nil)

  private def fresh: Unifier = Unifier.create(MetaStore.empty, 0)

  /** Force a payload meta to its solution for assertions. */
  private def solved(u: Unifier, id: Int): SemValue = Evaluator.force(meta(id), u.metaStore)

  /** A **carried** actual — a judgment the checker's positional recognition tagged as sitting on an effect carrier. */
  private def carried(judgment: SemValue): ActualForm = {
    val (carrier, payload) = Carrier.split(judgment)
    ActualForm.Carried(judgment, carrier, payload)
  }

  /** A genuine `Id[T]` actual — the identity carrier as ordinary data, whose payload is projected with `runId`. */
  private def idCarried(payload: SemValue): ActualForm = ActualForm.IdCarried(id(payload), payload)

  /** Run a spine of `(actual, expectedSlot)` pairs left-to-right, then default the named carrier metas. Returns the
    * finalized unifier, the slots the caller re-carries with a `pure` lift ([[Outcome.PureLift]]), and every outcome
    * in order.
    */
  private def runSpine(
      slots: List[(ActualForm, ExpectedSlot)],
      carrierMetaIds: List[Int]
  ): (Unifier, List[Outcome], List[Outcome]) = {
    val (u, outs) = slots.foldLeft((fresh, List.empty[Outcome])) { case ((acc, os), (actual, expected)) =>
      val (acc2, out) = UniformLadder.resolveSlot(acc, actual, expected, ctx)
      (acc2, os :+ out)
    }
    (CarrierJoin.finalize(u, carrierMetaIds.map(MetaId(_))), outs.filter { case _: Outcome.PureLift => true; case _ => false }, outs)
  }

  // =================================================================================================================
  // The four historical failure cases, each dissolving with no guards.
  // =================================================================================================================

  // --- Case 1: ?F[List[String]] ~ List[A]  (compound-state / eliot.file class, CONCRETE payload) -------------------

  "case 1 (?F[List[String]] ~ List[A])" should "unify payload-with-payload, solving A := String, carrier untouched" in {
    val (u, _, out) = runSpine(List(carried(meta(1, list(string))) -> ExpectedSlot.PayloadSlot(list(meta(2)))), List(1))
    solved(u, 2) shouldBe string
    out.head shouldBe Outcome.PayloadBound
  }

  it should "never let the container steal the carrier meta (A is String, not List[String])" in {
    val (u, _, _) = runSpine(List(carried(meta(1, list(string))) -> ExpectedSlot.PayloadSlot(list(meta(2)))), List(1))
    solved(u, 2) should not be list(string)
  }

  it should "match what the real unifier's injectivity decomposition STEALS without a split (regression contrast)" in {
    // Whole-unify `?1[List[String]] ~ List[?2]` on the REAL unifier injectivity-decomposes to ?1 := List, then
    // List[String] ~ ?2 ⇒ ?2 := List[String] — the theft the equal-arity guard existed to block. Splitting the carrier
    // off first (case 1 above) is exactly what avoids it.
    val naive = fresh.unify(meta(1, list(string)), list(meta(2)), ctx)
    solved(naive, 2) shouldBe list(string)
  }

  // --- Case 4: ?F[?S] ~ List[X]  (compound-state equal-arity, FLEX payload) ----------------------------------------

  "case 4 (?F[?S] ~ List[X], equal arity)" should "bind the payload (?S := List[X]), not steal the carrier" in {
    val (u, _, out) = runSpine(List(carried(meta(1, meta(2))) -> ExpectedSlot.PayloadSlot(list(con("X")))), List(1))
    solved(u, 2) shouldBe list(con("X"))
    out.head shouldBe Outcome.PayloadBound
  }

  it should "differ from the real unifier's naive theft (?F := List, ?S := X)" in {
    val naive = fresh.unify(meta(1, meta(2)), list(con("X")), ctx)
    solved(naive, 2) shouldBe con("X")
  }

  // --- Case 2: catch-handler default (ordering fragility of tryIdDefault) ------------------------------------------

  "case 2 (catch-handler default)" should "join to IO whether the pure boundary or the handler is seen first" in {
    val pureThenHandler = CarrierJoin.join(CarrierJoin.join(fresh, MetaId(7), Carrier.Bottom, ctx), MetaId(7), ioCarrier, ctx)
    val handlerThenPure = CarrierJoin.join(CarrierJoin.join(fresh, MetaId(7), ioCarrier, ctx), MetaId(7), Carrier.Bottom, ctx)
    CarrierJoin.resolve(pureThenHandler, Carrier.Var(MetaId(7))) shouldBe ioCarrier
    CarrierJoin.resolve(handlerThenPure, Carrier.Var(MetaId(7))) shouldBe ioCarrier
  }

  it should "not prematurely commit to Id the way first-contact solving did" in {
    // OLD first-contact (carrier as an ordinary meta): the pure boundary solves ?7 := Id[Unit], so the effectful
    // sibling's IO[Unit] then mismatches (the historical tryIdDefault ordering bug) — shown on the real unifier.
    val oldPureFirst = fresh.unify(meta(7), id(unit), ctx).unify(meta(7), io(unit), ctx)
    oldPureFirst.errors should not be empty
    // NEW join solver: Id is the lattice bottom, never a commitment, so IO wins regardless of order.
    CarrierJoin.join(CarrierJoin.join(fresh, MetaId(7), Carrier.Bottom, ctx), MetaId(7), ioCarrier, ctx).errors shouldBe empty
  }

  // --- Case 3: if(c, None) else Some(x)  (element type committed before the sibling constrains it) -----------------

  "case 3 (if(c, None) else Some(x))" should "solve the element to Int, carrier defaulting to Id, either arm order" in {
    val arms = List(
      idCarried(opt(meta(11))) -> ExpectedSlot.CarrierSlot(Carrier.Var(MetaId(9)), opt(meta(10))), // None
      idCarried(opt(intT))     -> ExpectedSlot.CarrierSlot(Carrier.Var(MetaId(9)), opt(meta(10)))  // Some(x)
    )
    val (u, lifts, _) = runSpine(arms, List(9))
    solved(u, 10) shouldBe intT
    CarrierJoin.resolve(u, Carrier.Var(MetaId(9))) shouldBe Carrier.Bottom
    lifts shouldBe List(Outcome.PureLift(projectPayload = true), Outcome.PureLift(projectPayload = true)) // both arms pure, so both lift at the Id-defaulted carrier — no machinery ships
  }

  it should "give the same result with the arms swapped (Some first)" in {
    val arms = List(
      idCarried(opt(intT))     -> ExpectedSlot.CarrierSlot(Carrier.Var(MetaId(9)), opt(meta(10))),
      idCarried(opt(meta(11))) -> ExpectedSlot.CarrierSlot(Carrier.Var(MetaId(9)), opt(meta(10)))
    )
    solved(runSpine(arms, List(9))._1, 10) shouldBe intT
  }

  // =================================================================================================================
  // The flagship: effectful and MIXED conditionals with NO fold/if FQN hardcode.
  // =================================================================================================================

  "an all-effectful conditional if(c, printLine(a)) else printLine(b)" should "resolve the ambient carrier to IO" in {
    val arms = List(
      carried(io(unit)) -> ExpectedSlot.CarrierSlot(Carrier.Var(MetaId(3)), meta(4)),
      carried(io(unit)) -> ExpectedSlot.CarrierSlot(Carrier.Var(MetaId(3)), meta(4))
    )
    val (u, lifts, _) = runSpine(arms, List(3))
    CarrierJoin.resolve(u, Carrier.Var(MetaId(3))) shouldBe ioCarrier
    solved(u, 4) shouldBe unit
    lifts shouldBe Nil // both arms already at the carrier — nothing to lift
  }

  "a mixed conditional if(c, readLine) else \"default\"" should "join to IO and lift ONLY the pure arm, order-independently" in {
    val arms = List(
      carried(io(string)) -> ExpectedSlot.CarrierSlot(Carrier.Var(MetaId(5)), meta(6)),
      idCarried(string) -> ExpectedSlot.CarrierSlot(Carrier.Var(MetaId(5)), meta(6))
    )
    val (u, lifts, _) = runSpine(arms, List(5))
    CarrierJoin.resolve(u, Carrier.Var(MetaId(5))) shouldBe ioCarrier
    solved(u, 6) shouldBe string
    lifts shouldBe List(Outcome.PureLift(projectPayload = true)) // only the pure arm is flagged, and it lifts at the joined carrier
  }

  it should "still lift the pure arm when it is seen FIRST (no premature Id commitment)" in {
    val arms = List(
      idCarried(string) -> ExpectedSlot.CarrierSlot(Carrier.Var(MetaId(5)), meta(6)), // pure arm first
      carried(io(string)) -> ExpectedSlot.CarrierSlot(Carrier.Var(MetaId(5)), meta(6))
    )
    val (u, lifts, _) = runSpine(arms, List(5))
    CarrierJoin.resolve(u, Carrier.Var(MetaId(5))) shouldBe ioCarrier
    lifts shouldBe List(Outcome.PureLift(projectPayload = true))
  }

  "a generic slot (fold's arm B)" should "receive the whole carrier-headed action by pass-through" in {
    val (u, lifts, out) = runSpine(List(carried(io(unit)) -> ExpectedSlot.Generic(MetaId(20))), Nil)
    solved(u, 20) shouldBe io(unit) // the suspended action IS the value of B
    out.head shouldBe Outcome.PassWhole
    lifts shouldBe Nil
  }

  "runMain(main) with main : ?G[Unit]" should "bind the ambient carrier to the platform's IO, no config key" in {
    val (u, _, out) = runSpine(List(carried(meta(30, unit)) -> ExpectedSlot.CarrierSlot(ioCarrier, meta(31))), List(30))
    CarrierJoin.resolve(u, Carrier.Var(MetaId(30))) shouldBe ioCarrier
    solved(u, 31) shouldBe unit
    out.head shouldBe Outcome.PassJoin
  }

  // =================================================================================================================
  // The join lattice: bottom, single winner, conflict, default, multi-layer stack.
  // =================================================================================================================

  "the join lattice" should "treat Id as bottom (never a solution)" in {
    CarrierJoin.resolve(CarrierJoin.join(fresh, MetaId(0), Carrier.Bottom, ctx), Carrier.Var(MetaId(0))) shouldBe Carrier.Var(MetaId(0))
  }

  it should "solve to the single non-Id contributor" in {
    CarrierJoin.resolve(CarrierJoin.join(CarrierJoin.join(fresh, MetaId(0), Carrier.Bottom, ctx), MetaId(0), ioCarrier, ctx), Carrier.Var(MetaId(0))) shouldBe ioCarrier
  }

  it should "reject two different non-Id carriers as a genuine mismatch" in {
    CarrierJoin.join(CarrierJoin.join(fresh, MetaId(0), ioCarrier, ctx), MetaId(0), Carrier.Con(stateFQN, Nil), ctx).errors should not be empty
  }

  it should "default an untouched carrier meta to Id at the boundary" in {
    CarrierJoin.resolve(CarrierJoin.finalize(fresh, List(MetaId(0))), Carrier.Var(MetaId(0))) shouldBe Carrier.Bottom
  }

  it should "treat a carrier meta joined toward ITSELF as a no-op (no self-referential cycle)" in {
    // `?F` against `?F` — a value's own ambient carrier meeting its declared return. Solving `?F := ?F` would loop
    // `resolve` forever; the self-join is a no-op, leaving the meta unsolved.
    CarrierJoin.resolve(CarrierJoin.join(fresh, MetaId(0), Carrier.Var(MetaId(0)), ctx), Carrier.Var(MetaId(0))) shouldBe Carrier.Var(MetaId(0))
  }

  it should "preserve a multi-layer stack prefix when joining it into a meta" in {
    // A StateCarrier[S, G] stack joined into a bare ambient meta keeps its [S, G] prefix (not dropped to Nil).
    val stack = Carrier.Con(stateFQN, List(con("S"), con("G")))
    CarrierJoin.resolve(CarrierJoin.joinToward(fresh, stack, Carrier.Var(MetaId(0)), ctx), Carrier.Var(MetaId(0))) shouldBe stack
  }

  // =================================================================================================================
  // The compile-time boundary (§8): the type language is NEVER carrier-wrapped.
  // =================================================================================================================

  "split" should "refuse a bare type-of-types judgment (a type-level term is not carrier-headed)" in {
    an[IllegalArgumentException] should be thrownBy Carrier.split(VType)
  }

  it should "refuse a bare nullary head (never a runtime judgment)" in {
    an[IllegalArgumentException] should be thrownBy Carrier.split(string)
  }

  "a type-level payload unification" should "introduce no carrier metas" in {
    // Int[0,255] ~ Int[0,255] at the TYPE level — the NbE/signature path, never split, never joined.
    val bounded = con("Int", con("0"), con("255"))
    fresh.unify(bounded, bounded, ctx).errors shouldBe empty
  }

  // =================================================================================================================
  // The classifier: the surviving positional recognition (effect-carrier tag on the EXPECTED binder).
  // =================================================================================================================

  "classifyExpected" should "classify a bare flex meta as Generic" in {
    UniformLadder.classifyExpected(meta(1), _ => false) shouldBe ExpectedSlot.Generic(MetaId(1))
  }

  it should "classify a tagged effect-carrier form as a CarrierSlot (split into carrier + payload)" in {
    UniformLadder.classifyExpected(meta(1, string), _ => true) shouldBe ExpectedSlot.CarrierSlot(Carrier.Var(MetaId(1)), string)
  }

  it should "classify an UNtagged Head[arg] as a PayloadSlot (a Functor/data slot, told apart only by the tag)" in {
    UniformLadder.classifyExpected(list(meta(1)), _ => false) shouldBe ExpectedSlot.PayloadSlot(list(meta(1)))
  }

  // =================================================================================================================
  // Rendering: never leak a carrier / Id / a metavariable — payload vocabulary only.
  // =================================================================================================================

  "payload rendering" should "show only the payload for a pure Id-carried type" in {
    SemValuePrinter.show(Carrier.split(id(string))._2, MetaStore.empty) shouldBe "String"
  }

  it should "show only the payload for an IO-carried type" in {
    SemValuePrinter.show(Carrier.split(io(list(string)))._2, MetaStore.empty) shouldBe "List(String)"
  }
}
