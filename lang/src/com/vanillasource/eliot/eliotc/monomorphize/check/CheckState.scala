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
  * @param env
  *   The current de Bruijn level environment (bindings + names)
  * @param unifier
  *   The unifier (carries meta store, depth, postponed, errors)
  * @param bindingCache
  *   Cache of fetched NativeBinding SemValues, keyed by ValueFQN. References to abstract associated ability types
  *   (`type X` inside `ability ...`, no body) are rewritten to a fresh [[SemValue.VMeta]] on first access and cached in
  *   that form; the meta is solved post-drain by unifying against the concrete impl's associated-type value.
  * @param abilityResolutions
  *   Map from each ability-qualified value reference (by its source-positioned FQN) to its resolved concrete impl.
  *   Filled by the drain-resolution loop; absence means the ref stays abstract (constraint-covered) at quoting time.
  * @param abilityRefs
  *   Pending ability-qualified value references collected during inference, keyed by source-positioned FQN with their
  *   current type arguments. Built by [[Checker]] as it walks the ORE; consumed by the drain-and-resolve loop instead
  *   of re-walking the checker's output tree.
  */
case class CheckState(
    env: Env,
    unifier: Unifier,
    bindingCache: Map[ValueFQN, Option[SemValue]],
    abilityResolutions: Map[Sourced[ValueFQN], (ValueFQN, Seq[GroundValue])],
    abilityRefs: Map[Sourced[ValueFQN], Seq[SemValue]]
) {

  /** Bind a parameter with the given name and type, extending the env. */
  def bind(name: String, value: SemValue): CheckState =
    copy(env = env.bind(name, value))

  def withUnifier(u: Unifier): CheckState = copy(unifier = u)

  def cacheBinding(vfqn: ValueFQN, value: Option[SemValue]): CheckState =
    copy(bindingCache = bindingCache + (vfqn -> value))

  def recordAbilityResolution(
      ref: Sourced[ValueFQN],
      impl: (ValueFQN, Seq[GroundValue])
  ): CheckState =
    copy(abilityResolutions = abilityResolutions + (ref -> impl))

  /** Record (or overwrite) the type arguments for an ability-qualified value reference. Called on initial inference and
    * again when implicit metas are appended during polytype instantiation.
    */
  def recordAbilityRef(ref: Sourced[ValueFQN], typeArgs: Seq[SemValue]): CheckState =
    copy(abilityRefs = abilityRefs + (ref -> typeArgs))

  /** Enumerate every abstract associated-type reference (abstract-ability-type vfqn → its standing meta id) that was
    * rewritten into a [[SemValue.VMeta]] during binding fetch. Derived from [[bindingCache]] — no separate index is
    * maintained. Used by the drain-resolve loop to inject concrete impl values into the standing metas once an ability
    * is resolved.
    */
  def associatedTypeMetas: Iterator[(ValueFQN, MetaId)] =
    bindingCache.iterator.collect { case (absFqn, Some(SemValue.VMeta(id, SemValue.Spine.SNil))) =>
      (absFqn, id)
    }

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
    Map.empty
  )
}
