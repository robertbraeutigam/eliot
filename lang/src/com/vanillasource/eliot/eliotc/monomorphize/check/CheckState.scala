package com.vanillasource.eliot.eliotc.monomorphize.check

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.monomorphize.unify.Unifier
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Immutable state for the bidirectional type checker.
  *
  * The checker keeps the **typing context Γ** and the **evaluation environment ρ** separate (the textbook NbE-checker
  * shape), instead of overloading one `Env` for both:
  *   - [[gamma]] (Γ) maps a parameter name to its **type** — read by [[Checker.infer]]'s `ParameterReference`.
  *   - [[rho]] (ρ) maps a parameter name to its **value** for the evaluator: an erased type parameter to its
  *     concrete value, a runtime value parameter to a **fresh neutral** standing for its not-yet-known runtime value
  *     (so a dependent type stays abstract in that parameter — genuine dependent Π), a peeled instantiation meta to the
  *     meta itself.
  *
  * The two grow in lockstep (every `bind*` extends both), so their de Bruijn levels stay in sync.
  *
  * Per-metavariable carrier bookkeeping (carrier kinds / effect-carrier flags) is **not** held here as separate
  * side-tables: it lives in a single
  * [[com.vanillasource.eliot.eliotc.monomorphize.unify.Unifier.CarrierRole]] map on the [[unifier]], and the `record*`
  * methods below delegate into it.
  *
  * @param gamma
  *   Γ: the typing context — parameter name → its type (de Bruijn level environment).
  * @param rho
  *   ρ: the evaluation environment — parameter name → its value (de Bruijn level environment); consumed by the
  *   [[Evaluator]].
  * @param unifier
  *   The unifier (carries meta store, depth, postponed, errors, and the per-meta role map)
  * @param bindingCache
  *   Cache of fetched NativeBinding SemValues, keyed by ValueFQN.
  * @param abilityResolutions
  *   Map from each ability-qualified value reference (by its source-positioned FQN) to its resolved concrete impl.
  *   Filled by the drain-resolution loop; absence means the ref stays abstract (constraint-covered) at quoting time.
  * @param ambientCarriers
  *   The value-under-check's own *ambient* effect-carrier heads: for each of its higher-kinded, ability-constrained
  *   signature binders (the M1 `{E...}` carrier, `[F[_] ~ E...]`), the forced head of the binder's value in ρ after
  *   type-argument application and instantiation — a [[CheckState.CarrierHead.TopDef]] for a concrete instantiation
  *   (`IO`), a [[CheckState.CarrierHead.Meta]] for a peeled one. Recorded once by [[TypeStackLoop]]; read by the
  *   checker-side effect lift (`isEffectCarrierHeaded`, the pure-wrap arm).
  * @param metaConstraints
  *   The ability constraints a callee reference declared on each of its freshly-peeled instantiation metas — keyed by
  *   the meta's raw id (docs/effects-as-channel.md §10 U4-f). Recorded at instantiation ([[recordMetaConstraints]],
  *   from [[CarrierKindChecker.recordCarrierMetas]]); read by the row-argument type-pinning rule when a constrained
  *   carrier meta is captured whole into a pinned-row parameter. The same table pinned finding 4 (the `CarrierJoin`
  *   Id-default guard) needs — built once here.
  * @param modeObligations
  *   The suspended slot-mode obligations (docs/effects-as-rows.md A.8.7): one per runtime-track computation that met a
  *   bare-generic argument slot during checking. Recorded by the spine loop instead of deciding the slot mid-spine
  *   (first-contact unification *is* a mode decision); classified against the solved meta store at post-drain
  *   quiescence by [[ModeResolver]].
  * @param letObligations
  *   The deferred `val`/statement bindings (docs/effects-as-rows.md A.8.7): a runtime-track `let` whose bound type was
  *   a bare metavariable at build time — the binding's bind-vs-plain mode is the bound instantiation's to decide, so
  *   the `let` is built plain and re-decided at quiescence: a bound type resolved carrier-headed gets the desugar's
  *   binding rewrite spliced ([[com.vanillasource.eliot.eliotc.row.RowElaborator.spliceResolvedModes]]).
  */
case class CheckState(
    gamma: Env,
    rho: Env,
    unifier: Unifier,
    bindingCache: Map[ValueFQN, Option[SemValue]],
    abilityResolutions: Map[Sourced[ValueFQN], (ValueFQN, Seq[GroundValue])],
    ambientCarriers: Set[CheckState.CarrierHead] = Set.empty,
    metaConstraints: Map[Int, Seq[CheckState.MetaConstraint]] = Map.empty,
    modeObligations: Vector[CheckState.ModeObligation] = Vector.empty,
    letObligations: Vector[CheckState.LetObligation] = Vector.empty
) {

  /** Record a suspended slot-mode obligation (A.8.7). See [[modeObligations]]. */
  def recordModeObligation(obligation: CheckState.ModeObligation): CheckState =
    copy(modeObligations = modeObligations :+ obligation)

  /** Record a deferred `let` binding whose mode the bound instantiation decides (A.8.7). See [[letObligations]]. */
  def recordLetObligation(obligation: CheckState.LetObligation): CheckState =
    copy(letObligations = letObligations :+ obligation)

  /** Record a higher-kinded type-parameter instantiation meta with its expected kind, for post-drain verification. */
  def recordCarrierKind(id: MetaId, expectedKind: SemValue, context: Sourced[String]): CheckState =
    withUnifier(unifier.recordCarrierKind(id, expectedKind, context))

  /** Mark an instantiation meta as standing for an *effect* carrier (an ability-constrained higher-kinded binder). */
  def recordEffectCarrier(id: MetaId): CheckState =
    withUnifier(unifier.recordEffectCarrier(id))

  /** Record the value-under-check's ambient effect-carrier heads. See [[ambientCarriers]]. */
  def recordAmbientCarriers(heads: Set[CheckState.CarrierHead]): CheckState =
    copy(ambientCarriers = ambientCarriers ++ heads)

  /** Record the ability constraints a callee reference declared on one of its instantiation metas. See
    * [[metaConstraints]].
    */
  def recordMetaConstraints(id: MetaId, constraints: Seq[CheckState.MetaConstraint]): CheckState =
    if (constraints.isEmpty) this
    else copy(metaConstraints = metaConstraints.updated(id.value, constraints))

  /** The neutral a runtime value parameter binds to in ρ: a fresh rigid variable at the current ρ level, standing for
    * the parameter's not-yet-known runtime value. Read *before* [[bindValueParam]] so the checker can substitute it into
    * a dependent codomain (`codomain(neutral)`, genuine dependent Π).
    */
  def paramNeutral(name: String): SemValue =
    VNeutral(NeutralHead.Param(rho.level, name), Spine.SNil)

  /** Bind a runtime value parameter: its declared type in Γ and a fresh neutral standing for its runtime value in ρ. A
    * value-position reference reads the type from Γ; the evaluator reads the neutral from ρ, so a dependent type stays
    * abstract in the parameter.
    */
  def bindValueParam(name: String, tpe: SemValue): CheckState =
    copy(gamma = gamma.bind(name, tpe), rho = rho.bind(name, paramNeutral(name)))

  /** Bind an erased type parameter: its type in Γ and its evaluable value in ρ. Both are computed by the caller
    * from the ground argument ([[TypeStackLoop.establishSignature]]) — Γ from the argument's declared type, ρ from its
    * `groundToSem` form (so the reification gate and type-level code see a data value as its constructor `VTopDef`).
    */
  def bindTypeStackParam(name: String, tpe: SemValue, value: SemValue): CheckState =
    copy(gamma = gamma.bind(name, tpe), rho = rho.bind(name, value))

  /** Bind a peeled instantiation meta (a leftover type parameter): the meta in both Γ and ρ — its value is the meta
    * (the evaluator keeps it abstract until unification solves it) and, referenced in value position, its type slot is
    * the meta too (it resolves through the metastore).
    */
  def bindTypeParam(name: String, meta: SemValue): CheckState =
    copy(gamma = gamma.bind(name, meta), rho = rho.bind(name, meta))

  def withUnifier(u: Unifier): CheckState = copy(unifier = u)

  def cacheBinding(vfqn: ValueFQN, value: Option[SemValue]): CheckState =
    copy(bindingCache = bindingCache + (vfqn -> value))

  def recordAbilityResolution(
      ref: Sourced[ValueFQN],
      impl: (ValueFQN, Seq[GroundValue])
  ): CheckState =
    copy(abilityResolutions = abilityResolutions + (ref -> impl))

  /** Build an [[Evaluator]] from this state. Pure — only reads `bindingCache`. */
  def makeEvaluator: Evaluator =
    new Evaluator(vfqn => bindingCache.getOrElse(vfqn, None))

  /** Look up the first in-scope parameter constraint that targets the given ability name and return its type arguments
    * evaluated against ρ.
    *
    * Used by the ability-resolution loop for refs covered by a constraint: the constraint's type arguments are the
    * caller's already-monomorphized values, so they're directly groundable — the reference's own implicit metas aren't,
    * until unification has connected them back to the ambient parameter.
    */
  def findConstraintTypeArgs(
      paramConstraints: Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]],
      abilityName: String
  ): Option[Seq[SemValue]] =
    paramConstraints.collectFirst(Function.unlift { (_, constraints) =>
      constraints.find(_.abilityFQN.abilityName == abilityName).map { c =>
        c.typeArgs.map(arg => makeEvaluator.eval(rho, arg))
      }
    })
}

object CheckState {
  def initial: CheckState = CheckState(
    Env.empty,
    Env.empty,
    Unifier.create(MetaStore.empty, 0),
    Map.empty,
    Map.empty
  )

  /** The forced head identity of an ambient effect carrier ([[CheckState.ambientCarriers]]): the two shapes a carrier
    * binder's ρ value can take after type-argument application and instantiation. Identity-comparable (no closures),
    * unlike the [[SemValue]]s themselves.
    */
  sealed trait CarrierHead

  object CarrierHead {

    /** A concrete carrier instantiation — the binder was applied to a type constructor (`F := IO`). */
    case class TopDef(fqn: ValueFQN) extends CarrierHead

    /** A still-open carrier — the binder was peeled to an instantiation metavariable. */
    case class Meta(id: Int) extends CarrierHead
  }

  /** One ability constraint a callee reference declared on an instantiation meta, its type arguments already evaluated
    * against the callee's binder→instantiation-meta substitution ([[metaConstraints]]). The carrier binder itself is
    * the constraint's *last* argument (as [[com.vanillasource.eliot.eliotc.core.processor.EffectSugarDesugarer]]
    * appends it); the non-carrier ability arguments — the ones the row-pinning rule pins into the carrier's leading
    * slots — are `args.dropRight(1)`.
    */
  case class MetaConstraint(abilityFQN: AbilityFQN, args: Seq[SemValue])

  /** One suspended slot-mode obligation (docs/effects-as-rows.md A.8.7): a carrier-headed computation met a
    * bare-generic argument slot, whose mode only the instantiation decides. Held open — the argument's type is *not*
    * unified into the slot — until post-drain quiescence, where [[ModeResolver]] classifies the solved `domain`:
    * payload ⟹ the desugar's strict-hoist rewrite (splice + restart), carrier-headed / pinned ⟹ pass the computation
    * through (unify), still-unsolved ⟹ the v2 default (adopt when the meta rides into `retType`, hoist otherwise).
    *
    * @param argNode
    *   The argument's node in the checked body — the splice anchor (compared by reference identity, since the checker
    *   threads the body's own [[Sourced]] nodes).
    * @param argType
    *   The argument's instantiated, effect-carrier-headed type.
    * @param domain
    *   The slot's domain — a bare metavariable at suspension time; forced at classification.
    * @param retType
    *   The application node's result type, for the ride-up occurs-check of the unsolved default.
    * @param spineType
    *   The whole spine's result type — read at splice time to apply the desugar's core rule with the mode known: a
    *   hoisted core whose solved spine result is a rigid non-carrier is a *payload* and is `pure`-wrapped as the
    *   chain's innermost continuation (the re-check must never meet a bare-generic continuation tail against the
    *   machinery's carrier codomain, which first-contact unification would steal).
    */
  case class ModeObligation(
      argNode: Sourced[OperatorResolvedExpression],
      argType: SemValue,
      domain: SemValue,
      retType: SemValue,
      spineType: SemValue,
      status: ModeObligation.Status = ModeObligation.Status.Pending
  )

  object ModeObligation {

    /** The lifecycle of a suspended obligation within the post-drain fixpoint. */
    sealed trait Status

    object Status {

      /** Not yet classifiable — the slot's domain is still a bare metavariable. */
      case object Pending extends Status

      /** Classified as pass-through/capture: the computation's type was unified into the slot. */
      case object Passed extends Status

      /** Classified as payload: the argument must be hoisted — the desugar's rewrite is spliced and the mono
        * restarts.
        */
      case object Hoist extends Status
    }
  }

  /** One deferred `let` binding (docs/effects-as-rows.md A.8.7): the bound expression's type was a bare metavariable
    * when the `let` was built, so bind-vs-plain could not be decided and the `let` was built plain. At quiescence a
    * bound type resolved carrier-headed means the binding must sequence: the desugar's binding rewrite is spliced
    * (`flatMap(x -> rest, bound)`) and the mono restarts. `argNode` is the bound expression's node in the checked
    * body (the splice anchor, by reference identity).
    */
  case class LetObligation(argNode: Sourced[OperatorResolvedExpression], argType: SemValue)
}
