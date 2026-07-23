package com.vanillasource.eliot.eliotc.monomorphize.spike

import com.vanillasource.eliot.eliotc.monomorphize.spike.UniformCarrierSpike.*
import com.vanillasource.eliot.eliotc.monomorphize.spike.UniformCarrierSpike.SType.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** U2 foundation spike harness (docs/effects-as-channel.md §10, U2c) — the executable regression suite that de-risks
  * the U3 uniform-checker refactor. It demonstrates, over [[UniformCarrierSpike]]'s minimal faithful model:
  *
  *   - the **four historical [[com.vanillasource.eliot.eliotc.monomorphize.check.EffectLifter]] failure cases** all
  *     dissolve under one guard-free rule set (`?F[List[String]] ~ List[A]`, the compound-state equal-arity theft, the
  *     `catch`-handler default, `if(c, None) else Some(x)`);
  *   - the **join solver** is order-independent (the premature-`Id`-commitment class cannot arise);
  *   - the flagship **effectful / mixed conditional** works with no `fold`/`if` FQN hardcode;
  *   - the **compile-time boundary** (§8) holds — the type language is never carrier-wrapped;
  *   - **error rendering** never leaks `Id` / a carrier / a metavariable;
  *   - the mechanism is **constant-factor** — no new fixpoint.
  *
  * When U3 grows the real uniform checker, these become its acceptance cases (the doc's "the U2 spike and the four-case
  * regression suite bound it").
  */
class UniformCarrierSpikeTest extends AnyFlatSpec with Matchers {

  // Convenient constructors mirroring the paper examples.
  private def io(p: SType): SType    = TCon("IO", List(p))
  private def id(p: SType): SType    = TCon("Id", List(p))
  private def list(p: SType): SType  = TCon("List", List(p))
  private def opt(p: SType): SType   = TCon("Option", List(p))
  private val string: SType          = TCon("String")
  private val int: SType             = TCon("Int")
  private val unit: SType            = TCon("Unit")

  /** Run a spine of `(actual, expectedSlot)` pairs left-to-right, then default carriers and materialize lifts. */
  private def runSpine(
      slots: List[(SType, ExpectedSlot)],
      carrierMetaIds: List[Int]
  ): (Solver, List[String], List[Outcome]) = {
    val (s, lifts, outs) = slots.foldLeft((Solver(), List.empty[DeferredLift], List.empty[Outcome])) {
      case ((acc, ls, os), (actual, expected)) =>
        val (acc2, out) = resolveSlot(acc, actual, expected, "slot")
        val added       = out match {
          case PassJoin(Some(l)) => List(l)
          case Bound(c)          => List[DeferredLift](LiftBind(c))
          case _                 => Nil
        }
        (acc2, ls ++ added, os :+ out)
    }
    val finalized = s.finalizeCarriers(carrierMetaIds)
    (finalized, materialize(finalized, lifts), outs)
  }

  // =================================================================================================================
  // U2c — the four historical failure cases, each dissolving with no guards.
  // =================================================================================================================

  // --- Case 1: ?F[List[String]] ~ List[A]  (compound-state / eliot.file class, CONCRETE payload) -------------------

  "case 1 (?F[List[String]] ~ List[A])" should "unify payload-with-payload, solving A := String" in {
    // actual: an effectful list-of-strings  ?1[List[String]]  (carrier meta ?1, payload List[String])
    // expected: a pure `List[A]` slot         List[?2]         (data container ⇒ PayloadSlot, A = ?2)
    val (s, _, out) = runSpine(List(TMeta(1, List(list(string))) -> PayloadSlot(list(TMeta(2)))), List(1))
    s.ok shouldBe true
    s.resolvePayload(TMeta(2)) shouldBe string
    out.head shouldBe Bound(CVar(1))
  }

  it should "never let the container steal the carrier meta (A is String, not List[String])" in {
    val (s, _, _) = runSpine(List(TMeta(1, List(list(string))) -> PayloadSlot(list(TMeta(2)))), List(1))
    s.resolvePayload(TMeta(2)) should not be list(string)
  }

  it should "match what the pre-uniform injectivity decomposition would STEAL (regression contrast)" in {
    // The old world kept ?F in the same meta store, so `?F[List[String]] ~ List[A]` injectivity-decomposed to
    // ?F := List, then List[String] ~ A ⇒ A := List[String]  — the theft the equal-arity guard existed to block.
    val naive = Solver()
      .copy(payloadStore = Map(1 -> TCon("List")))       // ?F := List   (carrier stolen by the container)
      .unifyPayload(list(string), TMeta(2), "naive")     // List[String] ~ A  ⇒  A := List[String]
    naive.resolvePayload(TMeta(2)) shouldBe list(string) // theft: A got the whole list, effect dropped
  }

  // --- Case 4: ?F[?S] ~ List[X]  (compound-state equal-arity, FLEX payload) ----------------------------------------

  "case 4 (?F[?S] ~ List[X], equal arity)" should "bind the payload (?S := List[X]), not steal the carrier" in {
    // actual: State's  state : ?1[?2]   (carrier ?1, flex payload ?2);  expected: an  S = List[X]  slot.
    val (s, _, out) = runSpine(List(TMeta(1, List(TMeta(2))) -> PayloadSlot(list(TCon("X")))), List(1))
    s.ok shouldBe true
    s.resolvePayload(TMeta(2)) shouldBe list(TCon("X"))  // State resolves at [List[X], StateCarrier..], not [X, List]
    out.head shouldBe Bound(CVar(1))
  }

  it should "differ from the naive theft (?F := List, ?S := X)" in {
    val naive = Solver()
      .copy(payloadStore = Map(1 -> TCon("List")))       // ?F := List
      .unifyPayload(TMeta(2), TCon("X"), "naive")        // ?S := X
    naive.resolvePayload(TMeta(2)) shouldBe TCon("X")    // the wrong element type the guard blocked
  }

  // --- Case 2: catch-handler default (ordering fragility of tryIdDefault) ------------------------------------------

  "case 2 (catch-handler default)" should "join to IO whether the pure boundary or the handler is seen first" in {
    // ?G is the result carrier.  The pure boundary contributes Id (nothing); the handler contributes IO (Suspend).
    val pureThenHandler = Solver().joinCarrier(7, CId).joinCarrier(7, CCon("IO"))
    val handlerThenPure = Solver().joinCarrier(7, CCon("IO")).joinCarrier(7, CId)
    pureThenHandler.resolveCarrier(CVar(7)) shouldBe CCon("IO")
    handlerThenPure.resolveCarrier(CVar(7)) shouldBe CCon("IO")
  }

  it should "not prematurely commit to Id the way first-contact solving did" in {
    // OLD first-contact (carrier as an ordinary meta): the pure boundary solves ?G := Id[Unit] — a real commitment —
    // so the effectful sibling's IO ~ Id then mismatches (the historical tryIdDefault ordering bug).
    val oldPureFirst = Solver().unifyPayload(TMeta(7), id(unit), "old pure boundary")
    oldPureFirst.unifyPayload(TMeta(7), io(unit), "old sibling").ok shouldBe false
    // NEW join solver: Id is the lattice bottom, never a commitment, so IO wins regardless of order.
    Solver().joinCarrier(7, CId).joinCarrier(7, CCon("IO")).ok shouldBe true
  }

  // --- Case 3: if(c, None) else Some(x)  (element type committed before the sibling constrains it) -----------------

  "case 3 (if(c, None) else Some(x))" should "solve the element to Int with the carrier defaulting to Id, either arm order" in {
    // if : (Bool, ?G[A], ?G[A]) -> ?G[A].  Arms share ambient carrier meta ?9, payload A = ?10.
    // None : Id[Option[?11]]     Some(x:Int) : Id[Option[Int]]
    val arms = List(
      id(opt(TMeta(11))) -> CarrierSlot(CVar(9), opt(TMeta(10))), // None
      id(opt(int))       -> CarrierSlot(CVar(9), opt(TMeta(10)))  // Some(x)
    )
    val (s, lifts, _) = runSpine(arms, List(9))
    s.ok shouldBe true
    s.resolvePayload(TMeta(10)) shouldBe int          // element correctly Int, not a prematurely-defaulted hole
    s.resolveCarrier(CVar(9)) shouldBe CId            // both arms pure ⇒ ambient defaults to Id
    lifts shouldBe List("pure erased (Id)", "pure erased (Id)")  // no machinery ships for the pure conditional
  }

  it should "give the same result with the arms swapped (Some first)" in {
    val arms = List(
      id(opt(int))       -> CarrierSlot(CVar(9), opt(TMeta(10))),
      id(opt(TMeta(11))) -> CarrierSlot(CVar(9), opt(TMeta(10)))
    )
    val (s, _, _) = runSpine(arms, List(9))
    s.resolvePayload(TMeta(10)) shouldBe int
  }

  // =================================================================================================================
  // The flagship: effectful and MIXED conditionals with NO fold/if FQN hardcode (the slice that opened the §13 fork).
  // =================================================================================================================

  "an all-effectful conditional if(c, printLine(a)) else printLine(b)" should "resolve the ambient carrier to IO" in {
    // arms: IO[Unit] into CarrierSlot(?G, ?a).  No compiler knowledge of `if` is used — purely the ladder.
    val arms = List(
      io(unit) -> CarrierSlot(CVar(3), TMeta(4)),
      io(unit) -> CarrierSlot(CVar(3), TMeta(4))
    )
    val (s, lifts, _) = runSpine(arms, List(3))
    s.resolveCarrier(CVar(3)) shouldBe CCon("IO")
    s.resolvePayload(TMeta(4)) shouldBe unit
    lifts shouldBe Nil // both arms already at the carrier — nothing to lift
  }

  "a mixed conditional if(c, readLine) else \"default\"" should "join to IO and lift ONLY the pure arm, order-independently" in {
    // arm1 readLine : IO[String]   arm2 "default" : Id[String]   into CarrierSlot(?G, ?a).
    val arms = List(
      io(string) -> CarrierSlot(CVar(5), TMeta(6)),
      id(string) -> CarrierSlot(CVar(5), TMeta(6))
    )
    val (s, lifts, _) = runSpine(arms, List(5))
    s.resolveCarrier(CVar(5)) shouldBe CCon("IO")
    s.resolvePayload(TMeta(6)) shouldBe string
    lifts shouldBe List("pure@Effect[IO]") // the pure arm's deferred lift materializes at the joined carrier
  }

  it should "still lift the pure arm when it is seen FIRST (no premature Id commitment)" in {
    val arms = List(
      id(string) -> CarrierSlot(CVar(5), TMeta(6)), // pure arm first
      io(string) -> CarrierSlot(CVar(5), TMeta(6))
    )
    val (s, lifts, _) = runSpine(arms, List(5))
    s.resolveCarrier(CVar(5)) shouldBe CCon("IO")
    lifts shouldBe List("pure@Effect[IO]")
  }

  // A generic (lazy) combinator: `fold`'s bare-generic arm receives the whole suspended action, no lift.
  "a generic slot (fold's arm B)" should "receive the whole carrier-headed action by pass-through" in {
    val (s, lifts, out) = runSpine(List(io(unit) -> Generic(20)), Nil)
    s.resolvePayload(TMeta(20)) shouldBe io(unit) // the suspended action IS the value of B
    out.head shouldBe PassWhole
    lifts shouldBe Nil
  }

  // runMain(io: IO[A]) — a concrete carrier slot binds the caller's ambient by ordinary pass-join, no config key.
  "runMain(main) with main : ?G[Unit]" should "bind the ambient carrier to the platform's IO" in {
    val (s, _, out) = runSpine(List(TMeta(30, List(unit)) -> CarrierSlot(CCon("IO"), TMeta(31))), List(30))
    s.resolveCarrier(CVar(30)) shouldBe CCon("IO")
    s.resolvePayload(TMeta(31)) shouldBe unit
    out.head shouldBe PassJoin(None)
  }

  // =================================================================================================================
  // U2b — the join lattice: bottom, single winner, conflict.
  // =================================================================================================================

  "the join lattice" should "treat Id as bottom (never a solution)" in {
    Solver().joinCarrier(0, CId).resolveCarrier(CVar(0)) shouldBe CVar(0)
  }

  it should "solve to the single non-Id contributor" in {
    Solver().joinCarrier(0, CId).joinCarrier(0, CCon("IO")).resolveCarrier(CVar(0)) shouldBe CCon("IO")
  }

  it should "reject two different non-Id carriers as a genuine mismatch" in {
    Solver().joinCarrier(0, CCon("IO")).joinCarrier(0, CCon("StateCarrier")).ok shouldBe false
  }

  it should "default an untouched carrier meta to Id at the boundary" in {
    Solver().finalizeCarriers(List(0)).resolveCarrier(CVar(0)) shouldBe CId
  }

  // =================================================================================================================
  // U2d — the compile-time boundary (§8): the type language is NEVER carrier-wrapped.
  // =================================================================================================================

  "a type-level judgment" should "unify with plain unification and introduce no carrier metas" in {
    // Int[0,255] ~ Int[0,255] at the TYPE level — the NbE/signature path, never a runtime term.
    val bounded  = TCon("Int", List(TCon("0"), TCon("255")))
    val afterType = Solver().unifyPayload(bounded, bounded, "type-level")
    afterType.ok shouldBe true
    afterType.carrierStore shouldBe empty // no carrier ever enters type-level evaluation
  }

  it should "not be carrier-headed (split refuses a type-of-types judgment)" in {
    an[IllegalArgumentException] should be thrownBy split(TType)
  }

  "the §8 Either mini-weave carrier" should "be a data type, not a runtime carrier" in {
    // The compiler track discharges {Throw[String]} onto Either[String, _] by TYPE-level Either folding — Either is a
    // data container here, never joined as a runtime carrier.
    isCarrierCon("Either") shouldBe false
  }

  // =================================================================================================================
  // U2e — error rendering: never leak Id / a carrier / a metavariable.
  // =================================================================================================================

  "the renderer" should "show only the payload for a pure Id-carried type" in {
    render(id(string)) shouldBe "String"
  }

  it should "show only the payload for an IO-carried type" in {
    render(io(list(string))) shouldBe "List[String]"
  }

  it should "never emit Id, a carrier name, or a metavariable" in {
    val samples = List(id(string), io(unit), TMeta(0, List(opt(int))), list(string))
    samples.map(render).mkString(" ") should not include regex("Id|IO|StateCarrier|\\?")
  }

  it should "render the declared row from the channel, not from carrier residue" in {
    (renderRow(List("Console")) + render(io(unit))) shouldBe "{Console} Unit"
  }

  // =================================================================================================================
  // U2f — perf constant: the ladder is linear in slots, the join solver adds no fixpoint.
  // =================================================================================================================

  "resolving a wide spine" should "cost unify calls linear in the number of slots (no blow-up)" in {
    val slots = (0 until 50).toList.map(i => io(unit) -> CarrierSlot(CVar(100), TMeta(200 + i)))
    val (s, _, _) = runSpine(slots, List(100))
    s.unifyCalls should be <= (slots.length * 2) // one payload unify per slot, constant factor
  }

  it should "solve carriers in one pass with no iteration" in {
    // 100 arms all joining the same ambient — join is O(1) per contribution, no fixpoint over the set.
    val s = (0 until 100).foldLeft(Solver())((acc, _) => acc.joinCarrier(0, CCon("IO")))
    s.resolveCarrier(CVar(0)) shouldBe CCon("IO")
    s.ok shouldBe true
  }
}
