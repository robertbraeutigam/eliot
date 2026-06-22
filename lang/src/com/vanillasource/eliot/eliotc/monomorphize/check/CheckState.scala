package com.vanillasource.eliot.eliotc.monomorphize.check

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.MetaId
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.monomorphize.unify.Unifier
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Immutable state for the bidirectional type checker.
  *
  * Per-metavariable metadata (combinable / candidates / combine-resolved / upper-bounds / carrier-kinds /
  * abstract-associated-type placeholders) is **not** held here as separate side-tables (D2): it lives in a single
  * [[com.vanillasource.eliot.eliotc.monomorphize.domain.MetaRole]] map on the [[unifier]], and the `record*` methods
  * below delegate into it. This leaves `CheckState` with just the environment, the unifier, the binding cache, the
  * ability resolutions, and the type-stack-value-param name set.
  *
  * @param env
  *   The current de Bruijn level environment (bindings + names)
  * @param unifier
  *   The unifier (carries meta store, depth, postponed, errors, and the per-meta role map)
  * @param bindingCache
  *   Cache of fetched NativeBinding SemValues, keyed by ValueFQN.
  * @param abilityResolutions
  *   Map from each ability-qualified value reference (by its source-positioned FQN) to its resolved concrete impl.
  *   Filled by the drain-resolution loop; absence means the ref stays abstract (constraint-covered) at quoting time.
  */
case class CheckState(
    env: Env,
    unifier: Unifier,
    bindingCache: Map[ValueFQN, Option[SemValue]],
    abilityResolutions: Map[Sourced[ValueFQN], (ValueFQN, Seq[GroundValue])],
    typeStackValueParams: Set[String]
) {

  /** Record a higher-kinded type-parameter instantiation meta with its expected kind, for post-drain verification. */
  def recordCarrierKind(id: MetaId, expectedKind: SemValue, context: Sourced[String]): CheckState =
    withUnifier(unifier.recordCarrierKind(id, expectedKind, context))

  def recordCombineResolved(id: MetaId): CheckState =
    withUnifier(unifier.recordCombineResolved(id))

  def recordUpperBound(id: MetaId, expected: SemValue, context: Sourced[String]): CheckState =
    withUnifier(unifier.recordUpperBound(id, expected, context))

  /** Bind a parameter with the given name and type, extending the env. */
  def bind(name: String, value: SemValue): CheckState =
    copy(env = env.bind(name, value))

  /** Bind an erased type-stack parameter to its concrete value and record its name. Unlike a runtime value parameter
    * (whose env binding is its *type*), a type-stack parameter's env binding is its *value* — so when it is referenced
    * in value position the checker must recover its type from the value, not return the value as a type. The recorded
    * set distinguishes the two; see [[Checker.infer]]'s `ParameterReference` case.
    */
  def bindTypeStackParam(name: String, value: SemValue): CheckState =
    copy(env = env.bind(name, value), typeStackValueParams = typeStackValueParams + name)

  def withUnifier(u: Unifier): CheckState = copy(unifier = u)

  def cacheBinding(vfqn: ValueFQN, value: Option[SemValue]): CheckState =
    copy(bindingCache = bindingCache + (vfqn -> value))

  def recordAbstractTypeMeta(vfqn: ValueFQN, metaId: MetaId): CheckState =
    withUnifier(unifier.recordAbstractAssoc(metaId, vfqn))

  def recordAbilityResolution(
      ref: Sourced[ValueFQN],
      impl: (ValueFQN, Seq[GroundValue])
  ): CheckState =
    copy(abilityResolutions = abilityResolutions + (ref -> impl))

  /** Build an [[Evaluator]] from this state. Pure — only reads `bindingCache`. */
  def makeEvaluator: Evaluator =
    new Evaluator(vfqn => bindingCache.getOrElse(vfqn, None))

  /** Look up the first in-scope parameter constraint that targets the given ability name and return its type arguments
    * evaluated against this state's env.
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
        c.typeArgs.map(arg => makeEvaluator.eval(env, arg))
      }
    })
}

object CheckState {
  def initial: CheckState = CheckState(
    Env.empty,
    Unifier.create(MetaStore.empty, 0),
    Map.empty,
    Map.empty,
    Set.empty
  )
}
