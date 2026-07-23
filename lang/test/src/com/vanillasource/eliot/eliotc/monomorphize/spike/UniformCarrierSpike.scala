package com.vanillasource.eliot.eliotc.monomorphize.spike

/** U2 foundation spike (docs/effects-as-channel.md §10) — a self-contained, throwaway prototype of the committed
  * **uniform-carrier (Id-uniform)** foundation, validating the mechanism before the wide U3 checker refactor leans on
  * it. It is deliberately **not wired into the compiler**: the default path stays byte-identical (the hard requirement
  * until U4), and this object never runs during a real compile. It lives in the test tree so its four historical
  * failure cases persist as regression cases (U2c) that U3 inherits as acceptance cases.
  *
  * =Correspondence to the real domain=
  *
  * The spike models a *minimal* type language rather than reusing [[com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue]]
  * directly, because the mechanism the spike validates is representation-agnostic and far more legible in the small.
  * The mapping is 1:1 and load-bearing (U2a confirms the representation is "carrier as an ordinary outermost type
  * application", not a parallel judgment pair):
  *
  *   - [[SType.TCon]]`(name, args)` ↔ `VTopDef(fqn, None, spine)` / a `VConst` structure — a rigid constructor applied
  *     to a spine. A *carrier* constructor (`Id`, `IO`, `StateCarrier`, …) and a *data* constructor (`List`, `Option`,
  *     `Pair`, `String`) are both `TCon`; they are distinguished the same way the real system distinguishes them — by a
  *     resolvable semantic fact ([[isCarrierCon]] ↔ "has an `Effect`/`Suspend` instance"), never by shape.
  *   - [[SType.TMeta]]`(id, args)` ↔ `VMeta(id, spine)`.
  *   - [[SType.TType]] ↔ `VType` — the type of types, used only to pin the compile-time boundary (§8).
  *
  * The one deliberate divergence, made to render the central invariant *structural*: the real
  * [[com.vanillasource.eliot.eliotc.monomorphize.unify.Unifier]] keeps carrier and payload metas in **one**
  * `MetaStore`, distinguished by membership in `carrierRoles`. The spike splits them into two maps
  * ([[Solver.carrierStore]] vs [[Solver.payloadStore]]) so that "a carrier meta can never be stolen by a container"
  * holds *by construction* — a container unification only ever touches [[Solver.payloadStore]]. In U3 the same
  * guarantee is recovered by routing carrier metas exclusively through the join channel; the two-store split is the
  * spike's way of making that discipline impossible to violate.
  *
  * =The result=
  *
  * All four historical [[com.vanillasource.eliot.eliotc.monomorphize.check.EffectLifter]] failure cases dissolve under
  * **one** rule set, with **none** of the lifter's guards (`mustLiftBeforeUnify`, the equal-arity arm and its three
  * sub-guards, `underApplied`, `tryIdDefault`-as-an-arm, `ambientCarriers` shape recognition of the *actual*):
  *
  *   1. Every runtime term judgment is carrier-headed by construction ([[split]] is total, positional — never a
  *      "is this type a carrier?" query on the actual).
  *   2. Carrier metavariables solve by **join** with `Id` as the lattice bottom ([[Solver.joinCarrier]]) — never by
  *      first-contact unification, so no premature `Id` commitment exists to mis-order.
  *   3. The one ladder ([[resolveSlot]]) reads the *expected* slot's shape to pick pass-through / pass-join / bind; the
  *      only recognition that survives is the effect-carrier **tag** on the expected binder (positional, set at
  *      elaboration), which cleanly separates an ambient/effect-carrier slot (join) from a Functor/data/concrete slot
  *      (bind).
  *   4. Lift materialization is **deferred and decision-free** ([[materialize]]): the bind-vs-pass *decision* is made
  *      eagerly by the ladder; only the carrier *identity* for the inserted `pure`/`flatMap` waits for the join to
  *      resolve (replacing the Checker's Phase A/B decision-deferral).
  */
object UniformCarrierSpike {

  // ---------------------------------------------------------------------------------------------------------------
  // The minimal type language (↔ SemValue).
  // ---------------------------------------------------------------------------------------------------------------

  sealed trait SType
  object SType {

    /** The type of types (↔ `VType`). Appears only at the compile-time boundary (§8): a judgment whose type is
      * [[TType]] is type-level and is **never** carrier-wrapped.
      */
    case object TType extends SType

    /** A rigid constructor applied to a spine (↔ `VTopDef(fqn, None, spine)` / `VConst`). */
    case class TCon(name: String, args: List[SType] = Nil) extends SType

    /** A metavariable applied to a spine (↔ `VMeta(id, spine)`). Whether it is a *carrier* meta or a *payload* meta is
      * a property of the [[Solver]] (which store owns it), exactly as `carrierRoles` membership decides in the real
      * unifier — never a property of the shape.
      */
    case class TMeta(id: Int, args: List[SType] = Nil) extends SType
  }

  import SType.*

  /** The carrier constructors — those with an `Effect`/`Suspend` instance in the real system. This is a resolvable
    * semantic fact per program, **not** a shape/arity guess: `IO` and `List` are both `TCon`, and the whole point of
    * the uniform foundation is that we never ask "is this `C[T]` a carrier by its shape?". `Id` is the pure carrier
    * (compiler-owned machinery, `WellKnownTypes.idFQN`).
    */
  val carrierCons: Set[String] = Set("Id", "IO", "StateCarrier", "AbortCarrier", "ThrowCarrier")

  def isCarrierCon(name: String): Boolean = carrierCons.contains(name)

  // ---------------------------------------------------------------------------------------------------------------
  // Carriers and the join lattice.  Id ⊑ everything;  two distinct non-Id = ⊤ (a genuine mismatch).
  // ---------------------------------------------------------------------------------------------------------------

  /** A carrier value: the pure bottom [[CId]], a concrete carrier constructor [[CCon]] (with its leading stack prefix —
    * `StateCarrier[S, G]` keeps `[S, G]`), or an unsolved carrier metavariable [[CVar]] (still bottom until joined).
    */
  sealed trait Carrier
  case object CId                                            extends Carrier
  case class CCon(name: String, prefix: List[SType] = Nil) extends Carrier
  case class CVar(id: Int)                                   extends Carrier

  // ---------------------------------------------------------------------------------------------------------------
  // The solver:  two separate stores (see the object doc) + accumulated errors in *payload vocabulary*.
  // ---------------------------------------------------------------------------------------------------------------

  case class Solver(
      payloadStore: Map[Int, SType] = Map.empty,
      carrierStore: Map[Int, Carrier] = Map.empty,
      errors: List[String] = Nil,
      unifyCalls: Int = 0
  ) {

    def error(msg: String): Solver = copy(errors = msg :: errors)

    def ok: Boolean = errors.isEmpty

    // --- carrier solving: join, with Id as the lattice bottom -----------------------------------------------------

    /** Follow a carrier through the store to its representative: `CId`, a `CCon`, or a still-unsolved `CVar` (bottom).
      * A carrier constructor named `Id` normalizes to the bottom [[CId]] here — `Id` is the lattice bottom *everywhere*,
      * never a concrete carrier. (Collapsing this distinction is exactly what makes the pure arm of a conditional never
      * commit its ambient: the spike's own bring-up rediscovered the premature-commitment bug when a pure `Id[String]`
      * actual split to `CCon("Id")` and was treated as a real contribution.)
      */
    def resolveCarrier(c: Carrier): Carrier = c match {
      case CVar(id)      => carrierStore.get(id).map(resolveCarrier).getOrElse(CVar(id))
      case CCon("Id", _) => CId
      case other         => other
    }

    /** Join `contribution` into the carrier meta `id` (the load-bearing rule, §3):
      *   - `Id` never solves a carrier meta — it is the lattice bottom, so a pure term meeting a flex carrier records
      *     nothing and the meta stays liftable;
      *   - a meta touched by exactly one non-`Id` carrier solves to it;
      *   - two *different* non-`Id` carriers meeting one meta are a genuine mismatch;
      *   - a meta untouched by any non-`Id` carrier defaults to `Id` at the boundary ([[finalizeCarriers]]).
      *
      * Crucially this is *not* first-contact unification: the pure arm of `if(c, "a") else readLine` records `Id`
      * (nothing) and never commits the meta before the effectful sibling contributes `IO`. `tryIdDefault` is thereby
      * promoted from a fragile ladder arm into the solving rule itself.
      */
    def joinCarrier(id: Int, contribution: Carrier): Solver = resolveCarrier(CVar(id)) match {
      case CVar(rep) =>
        resolveCarrier(contribution) match {
          case CId       => this                                             // bottom contributes nothing
          case resolved  => copy(carrierStore = carrierStore.updated(rep, resolved))
        }
      case CCon(a, _) =>
        resolveCarrier(contribution) match {
          case CId | CCon(`a`, _) => this                                    // same carrier or Id — ok
          case CCon(b, _)         => error(s"conflicting effect carriers: '$a' and '$b'")
          case CVar(vid)          => copy(carrierStore = carrierStore.updated(vid, CCon(a, Nil)))
        }
      case CId => this
    }

    /** Default every carrier meta untouched by a non-`Id` contribution to `Id` (the boundary rule). */
    def finalizeCarriers(ids: Iterable[Int]): Solver =
      ids.foldLeft(this) { (s, id) =>
        s.resolveCarrier(CVar(id)) match {
          case CVar(rep) => s.copy(carrierStore = s.carrierStore.updated(rep, CId))
          case _         => s
        }
      }

    // --- payload solving: ordinary first-contact unification (occurs-checked) -------------------------------------

    def resolvePayload(t: SType): SType = t match {
      case TMeta(id, Nil) => payloadStore.get(id).map(resolvePayload).getOrElse(t)
      case _              => t
    }

    private def occurs(id: Int, t: SType): Boolean = resolvePayload(t) match {
      case TMeta(other, args) => other == id || args.exists(occurs(id, _))
      case TCon(_, args)      => args.exists(occurs(id, _))
      case TType              => false
    }

    /** Structural unification over **payload** metas only — carrier metas live in the other store and are never
      * touched here. This is what makes "a carrier meta can never be stolen by a container" structural in the spike:
      * unifying `?F[List[String]] ~ List[A]` at a *payload* position only ever aligns `List` with `List`, never `?F`
      * with `List`.
      */
    def unifyPayload(l: SType, r: SType, ctx: String): Solver = {
      val s = copy(unifyCalls = unifyCalls + 1)
      (s.resolvePayload(l), s.resolvePayload(r)) match {
        case (TType, TType)                     => s
        case (TMeta(id, Nil), rhs)              =>
          if (occurs(id, rhs)) s.error(s"cannot construct infinite type ($ctx)")
          else s.copy(payloadStore = s.payloadStore.updated(id, rhs))
        case (lhs, TMeta(id, Nil))              =>
          if (occurs(id, lhs)) s.error(s"cannot construct infinite type ($ctx)")
          else s.copy(payloadStore = s.payloadStore.updated(id, lhs))
        case (TCon(n1, a1), TCon(n2, a2)) if n1 == n2 && a1.length == a2.length =>
          a1.zip(a2).foldLeft(s) { case (acc, (x, y)) => acc.unifyPayload(x, y, ctx) }
        case (lhs, rhs)                         =>
          s.error(s"type mismatch: ${render(lhs)} vs ${render(rhs)} ($ctx)")
      }
    }
  }

  // ---------------------------------------------------------------------------------------------------------------
  // Splitting a runtime judgment into (carrier, payload).  Positional, total, no recognition of the *actual*.
  // ---------------------------------------------------------------------------------------------------------------

  /** Split a carrier-headed runtime judgment `Ca[Ta]` into its carrier and payload — **unconditionally**, because
    * every runtime term is carrier-headed by construction (the elaboration invariant). A multi-argument carrier stack
    * (`StateCarrier[S, G, A]`) keeps the leading prefix as the carrier and the *last* argument as the payload, exactly
    * as [[com.vanillasource.eliot.eliotc.monomorphize.check.EffectLifter.effectCarrierSplit]] does today.
    *
    * The `require`s encode the invariant: a runtime judgment is never a bare payload or a nullary head — if it is, the
    * elaboration failed to maintain carrier-headedness, which under uniformity is a bug, not a shape to recognize.
    */
  def split(t: SType): (Carrier, SType) = t match {
    case TCon(name, args) if isCarrierCon(name) && args.nonEmpty =>
      val carrier = if (name == "Id") CId else CCon(name, args.init) // Id is the lattice bottom, not a concrete carrier
      (carrier, args.last)
    case TMeta(id, args) if args.nonEmpty                        =>
      (CVar(id), args.last)
    case other                                                   =>
      throw new IllegalArgumentException(s"not a carrier-headed runtime judgment: ${debug(other)}")
  }

  // ---------------------------------------------------------------------------------------------------------------
  // The one ladder.
  // ---------------------------------------------------------------------------------------------------------------

  /** The callee's declared parameter slot, as classified off its **elaborated** signature. This classification is the
    * only "recognition" the uniform foundation keeps, and it is a legitimate *positional tag* set at elaboration, not a
    * shape guess on an arbitrary type:
    *
    *   - [[Generic]] — a bare flex binder `?A` (`fold`'s arm `B`): the slot receives the whole *suspended action*
    *     (carrier included). This is the extensible-conditionals mechanism — a lazy combinator's arms pass through by
    *     ordinary parametric instantiation, with zero compiler knowledge of its name.
    *   - [[CarrierSlot]] — an **effect-carrier** form: headed either by an effect-carrier-tagged binder `?G` (`if`'s
    *     ambient arms) or by a known carrier constructor / pinned stack (`runMain`'s `io: IO[A]`, a discharger's
    *     `{Throw[E] | G} A` input). The carrier *joins*; the payload unifies; a pure actual records a deferred `pure`.
    *   - [[PayloadSlot]] — a concrete / data / **Functor** binder slot (`printLine`'s `String`, `map`'s `xs: F[A]`,
    *     `List[A]`): the effect must *run here* (bind), so the payload unifies against the slot and the carrier is
    *     sequenced.
    *
    * The load-bearing distinction is [[CarrierSlot]] vs [[PayloadSlot]] when both are `Head[arg]`: `if`'s `?G[A]` and
    * `map`'s `F[A]` are structurally identical, and are told apart *only* by whether the head binder was tagged an
    * effect carrier at elaboration (ability-constrained to the effect machinery / the value's ambient). That tag is
    * what dies as *shape recognition of the actual* and survives as a *positional fact about the expected*.
    */
  sealed trait ExpectedSlot
  case class Generic(metaId: Int)                     extends ExpectedSlot
  case class CarrierSlot(carrier: Carrier, payload: SType) extends ExpectedSlot
  case class PayloadSlot(shape: SType)                extends ExpectedSlot

  /** What the ladder decided at a slot, plus any deferred, decision-free lift to materialize once carriers solve. */
  sealed trait Outcome
  case object PassWhole                       extends Outcome
  case class PassJoin(lift: Option[DeferredLift]) extends Outcome
  case class Bound(carrier: Carrier)          extends Outcome
  case object Mismatch                         extends Outcome

  /** A recorded lift whose *insertion* is fixed but whose *carrier identity* is not yet known. Materialized in
    * [[materialize]] once the join resolves the carrier: `Id` ⇒ nothing; a non-`Id` carrier ⇒ `pure`/`flatMap`.
    */
  sealed trait DeferredLift
  case class LiftPure(carrier: Carrier)    extends DeferredLift
  case class LiftBind(carrier: Carrier)    extends DeferredLift

  /** Resolve one application slot: `actual` is the argument's (carrier-headed) type, `expected` the callee's declared
    * slot. Returns the updated solver, the ladder outcome, and any deferred lift. **No guards, no pre-arms** — the
    * expected slot's classification picks the arm directly.
    */
  def resolveSlot(solver: Solver, actual: SType, expected: ExpectedSlot, ctx: String): (Solver, Outcome) = {
    val (ca, ta) = split(actual)
    expected match {
      // Generic slot ⇒ the whole carrier-headed action is the value of ?A; runs where the consumer sequences it.
      case Generic(m) =>
        (solver.unifyPayload(TMeta(m), actual, ctx), PassWhole)

      // Effect-carrier slot ⇒ carrier joins (Id no-op), payload unifies; ONLY a pure (Id-side) actual records a
      // deferred `pure`, keyed on the *result* carrier — materialized to `pure@Effect[C]` once the join resolves it,
      // or erased if it defaults to Id.
      case CarrierSlot(cExpected, pExpected) =>
        val joined   = joinToward(solver, ca, cExpected).unifyPayload(ta, pExpected, ctx)
        val deferred = if (ca == CId) Some(LiftPure(cExpected)) else None
        (joined, PassJoin(deferred))

      // Payload slot ⇒ bind: the effect runs at the call site; the payload fills the slot.
      case PayloadSlot(shape) =>
        (solver.unifyPayload(ta, shape, ctx), Bound(ca))
    }
  }

  /** Feed a carrier contribution `ca` toward an expected carrier `cExpected`: when the expected is itself a carrier
    * meta `?G`, `ca` joins into it; when the expected is a concrete carrier (`IO`), the *actual*'s carrier meta (if
    * any) joins toward it; two concretes are compared. This is the carrier half of pass-join and is pure join — never
    * first-contact.
    */
  private def joinToward(solver: Solver, ca: Carrier, cExpected: Carrier): Solver = (ca, cExpected) match {
    case (_, CVar(gid))          => solver.joinCarrier(gid, ca)
    case (CVar(aid), concrete)   => solver.joinCarrier(aid, concrete)
    case (CId, _)                => solver
    case (CCon(a, _), CCon(b, _)) if a == b => solver
    case (CCon(a, _), CCon(b, _)) => solver.error(s"conflicting effect carriers: '$a' and '$b'")
    case (_, CId)                => solver
  }

  /** Materialize every deferred lift now that carriers have joined and defaulted — **decision-free**: the rule is
    * fixed, only the carrier was unknown. Returns a human description of each inserted node (or that it erased).
    */
  def materialize(solver: Solver, lifts: List[DeferredLift]): List[String] =
    lifts.map {
      case LiftPure(c) => solver.resolveCarrier(c) match {
          case CId          => "pure erased (Id)"
          case CCon(n, _)   => s"pure@Effect[$n]"
          case CVar(_)      => "pure erased (Id)" // unsolved ⇒ defaulted to Id
        }
      case LiftBind(c) => solver.resolveCarrier(c) match {
          case CId          => "flatMap erased (Id)"
          case CCon(n, _)   => s"flatMap@Effect[$n]"
          case CVar(_)      => "flatMap erased (Id)"
        }
    }

  // ---------------------------------------------------------------------------------------------------------------
  // Error rendering (U2e):  never leak Id / a carrier / a meta to the user — payload vocabulary only.
  // ---------------------------------------------------------------------------------------------------------------

  /** Render a type for a user-facing message. A runtime judgment is carrier-headed; the renderer shows **only the
    * payload**, so `Id[String]`, `IO[String]`, and a pure `String` all render as `String`. Carrier names, `Id`, and
    * metavariables never appear. (Declared/derived effect *rows* are rendered separately from the channel — modelled
    * by [[renderRow]] — never from carrier residue.)
    */
  def render(t: SType): String = t match {
    case TCon(name, args) if isCarrierCon(name) && args.nonEmpty => render(args.last)  // strip the carrier
    case TMeta(_, args) if args.nonEmpty                          => render(args.last)  // strip a carrier meta
    case TCon(name, Nil)                                          => name
    case TCon(name, args)                                         => s"$name[${args.map(render).mkString(", ")}]"
    case TMeta(_, _)                                              => "_"                // an inferred payload hole
    case TType                                                    => "Type"
  }

  /** The declared effect row rendered from the *channel*, never from carriers — the LSP/diagnostic vocabulary. */
  def renderRow(effects: List[String]): String =
    if (effects.isEmpty) "" else s"{${effects.mkString(", ")}} "

  /** A debug rendering that *does* show carriers — for spike diagnostics only, never user-facing. */
  def debug(t: SType): String = t match {
    case TType            => "Type"
    case TCon(name, Nil)  => name
    case TCon(name, args) => s"$name[${args.map(debug).mkString(", ")}]"
    case TMeta(id, Nil)   => s"?$id"
    case TMeta(id, args)  => s"?$id[${args.map(debug).mkString(", ")}]"
  }
}
