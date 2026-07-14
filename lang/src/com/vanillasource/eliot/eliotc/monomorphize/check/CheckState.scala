package com.vanillasource.eliot.eliotc.monomorphize.check

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.monomorphize.unify.Unifier
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
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
  * @param sawGuardReturn
  *   Whether the kind check accepted a `{Throw[String]}`-carrier return as a *guarded type* (effectful-signatures W2b).
  *   Set when [[Checker.check]] accepts an `Either[..]`-valued term where a `Type` kind is expected; read by
  *   [[TypeStackLoop]] so that a guard whose bounds are abstract (stuck, not reducible to `Right`/`Left`) is *deferred*
  *   to the body — its return position becomes a metavariable the body solves — instead of the body hard-erroring
  *   against the undischarged carrier. Use-Site Verification: the guard is still enforced at every concrete instance.
  * @param ambientCarriers
  *   The value-under-check's own *ambient* effect-carrier heads: for each of its higher-kinded, ability-constrained
  *   signature binders (the M1 `{E...}` carrier, `[F[_] ~ E...]`), the forced head of the binder's value in ρ after
  *   type-argument application and instantiation — a [[CheckState.CarrierHead.TopDef]] for a concrete instantiation
  *   (`IO`), a [[CheckState.CarrierHead.Meta]] for a peeled one. Recorded once by [[TypeStackLoop]]; read by the
  *   checker-side effect lift (`isEffectCarrierHeaded`, the pure-wrap arm).
  * @param liftCounter
  *   The fresh-binder counter for effect-lift-inserted bindings (the established `$eff$N` naming convention; `$` is
  *   not a user identifier character). Threaded by the effect lifter so synthesized binders are unique within a body.
  */
case class CheckState(
    gamma: Env,
    rho: Env,
    unifier: Unifier,
    bindingCache: Map[ValueFQN, Option[SemValue]],
    abilityResolutions: Map[Sourced[ValueFQN], (ValueFQN, Seq[GroundValue])],
    sawGuardReturn: Boolean = false,
    ambientCarriers: Set[CheckState.CarrierHead] = Set.empty,
    liftCounter: Int = 0
) {

  /** Record that the kind check accepted a guard-carrier return (effectful-signatures W2b). See [[sawGuardReturn]]. */
  def recordGuardReturn: CheckState = copy(sawGuardReturn = true)

  /** Record a higher-kinded type-parameter instantiation meta with its expected kind, for post-drain verification. */
  def recordCarrierKind(id: MetaId, expectedKind: SemValue, context: Sourced[String]): CheckState =
    withUnifier(unifier.recordCarrierKind(id, expectedKind, context))

  /** Mark an instantiation meta as standing for an *effect* carrier (an ability-constrained higher-kinded binder). */
  def recordEffectCarrier(id: MetaId): CheckState =
    withUnifier(unifier.recordEffectCarrier(id))

  /** Record the value-under-check's ambient effect-carrier heads. See [[ambientCarriers]]. */
  def recordAmbientCarriers(heads: Set[CheckState.CarrierHead]): CheckState =
    copy(ambientCarriers = ambientCarriers ++ heads)

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
    * from the ground argument ([[TypeStackLoop.applyTypeArgs]]) — Γ from the argument's declared type, ρ from its
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
}
