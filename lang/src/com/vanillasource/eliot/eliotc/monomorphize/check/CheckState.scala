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
  *   Cache of fetched NativeBinding SemValues, keyed by ValueFQN.
  * @param abstractTypeMetas
  *   For each abstract associated-ability-type (`type X` inside `ability ...`, no body) that has been referenced in
  *   this session, the fresh [[SemValue.MetaId]] used as its standing placeholder. Looked up at [[bindingCache]] hits;
  *   solved post-drain by unifying against the concrete impl's associated-type value.
  * @param abilityResolutions
  *   Map from each ability-qualified value reference (by its source-positioned FQN) to its resolved concrete impl.
  *   Filled by the drain-resolution loop; absence means the ref stays abstract (constraint-covered) at quoting time.
  * @param abilityRefs
  *   Pending ability-qualified value references collected during inference, keyed by source-positioned FQN with their
  *   current type arguments. Built by [[Checker]] as it walks the ORE; consumed by the drain-and-resolve loop instead
  *   of re-walking the checker's output tree.
  * @param phantomMetas
  *   Metas allocated while peeling VLam closures from polytype signatures (implicit type arguments + phantom type
  *   parameters). If still unsolved after drain-and-resolve, they are defaulted to [[SemValue.VType]]: they either
  *   never get constrained (true phantoms) or they cover a polymorphic reference whose caller didn't need to pin them
  *   down (e.g. a generic value referenced inside a match arm). Defaulting preserves the pre-NbE behaviour.
  */
case class CheckState(
    env: Env,
    unifier: Unifier,
    bindingCache: Map[ValueFQN, Option[SemValue]],
    abstractTypeMetas: Map[ValueFQN, MetaId],
    abilityResolutions: Map[Sourced[ValueFQN], (ValueFQN, Seq[GroundValue])],
    abilityRefs: Map[Sourced[ValueFQN], Seq[SemValue]],
    phantomMetas: Set[MetaId]
) {

  /** Bind a parameter with the given name and type, extending the env. */
  def bind(name: String, value: SemValue): CheckState =
    copy(env = env.bind(name, value))

  def withUnifier(u: Unifier): CheckState = copy(unifier = u)

  def cacheBinding(vfqn: ValueFQN, value: Option[SemValue]): CheckState =
    copy(bindingCache = bindingCache + (vfqn -> value))

  def recordAbstractTypeMeta(vfqn: ValueFQN, metaId: MetaId): CheckState =
    copy(abstractTypeMetas = abstractTypeMetas + (vfqn -> metaId))

  def recordPhantomMeta(metaId: MetaId): CheckState =
    copy(phantomMetas = phantomMetas + metaId)

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
    Map.empty,
    Map.empty,
    Set.empty
  )
}
