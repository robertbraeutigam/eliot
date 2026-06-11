package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementation
import com.vanillasource.eliot.eliotc.ability.util.AbilityMatcher
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, UnifiedModuleNames, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.check.TypeStackLoop
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicValue, NativeBinding}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor

/** Entry point for NbE-based type checking (monomorphize). Delegates to TypeStackLoop for the actual type checking
  * work.
  */
class MonomorphicTypeCheckProcessor
    extends TransformationProcessor[OperatorResolvedValue.Key, MonomorphicValue.Key](key =>
      OperatorResolvedValue.Key(key.vfqn)
    ) {

  private def fetchBinding(vfqn: ValueFQN): CompilerIO[Option[SemValue]] =
    getFact(NativeBinding.Key(vfqn)).map(_.map(_.semValue))

  override protected def generateFromKeyAndFact(
      key: MonomorphicValue.Key,
      resolvedValue: OperatorResolvedValue
  ): CompilerIO[MonomorphicValue] =
    for {
      refinements <- buildRefinements(resolvedValue)
      result      <- TypeStackLoop.process(
                       key,
                       resolvedValue,
                       fetchBinding = fetchBinding,
                       resolveAbility = resolveAbilityImpl,
                       refinements = refinements
                     )
    } yield result

  private def resolveAbilityImpl(
      vfqn: ValueFQN,
      typeArgs: Seq[GroundValue]
  ): CompilerIO[Option[(ValueFQN, Seq[GroundValue])]] =
    getFact(AbilityImplementation.Key(vfqn, typeArgs)).map(
      _.map(impl => (impl.implementationFQN, impl.implementationTypeArgs))
    )

  /** Build the refinement map for this value: for each type constructor referenced in its type stack (and runtime
    * body) that has a *custom* `TypeRefinement` implementation, map the constructor FQN to that implementation's
    * `assignableFrom` semantic value. Constructors without a custom implementation are omitted — the unifier then
    * compares them structurally, which is exactly the default (structural-equality) behaviour.
    *
    * Custom implementations are detected by the same lookup the ability resolver uses — scanning the relevant modules'
    * names for an `assignableFrom` implementation marker and matching it against the constructor — but via plain
    * `getFact` (never demanding the erroring `AbilityImplementation`/`AbilityImplementationCheck` facts), so a
    * constructor with no implementation is simply skipped rather than reported as "does not implement".
    */
  private def buildRefinements(resolvedValue: OperatorResolvedValue): CompilerIO[Map[ValueFQN, SemValue]] = {
    val exprs = resolvedValue.typeStack.value.levels.toList ++ resolvedValue.runtime.map(_.value).toList
    for {
      ctors   <- exprs.foldLeftM(Set.empty[ValueFQN]) { (acc, e) =>
                   OperatorResolvedExpression.foldValueReferences[CompilerIO, Set[ValueFQN]](e, acc) { (s, vfqn) =>
                     (if (vfqn.value.name.qualifier === Qualifier.Type) s + vfqn.value else s).pure[CompilerIO]
                   }
                 }
      entries <- ctors.toList.traverseFilter(resolveRefinement)
    } yield entries.toMap
  }

  private def resolveRefinement(ctor: ValueFQN): CompilerIO[Option[(ValueFQN, SemValue)]] = {
    val query            = Seq(GroundValue.Structure(ctor, Seq.empty, GroundValue.Type))
    val candidateModules = List(ctor.moduleName, WellKnownTypes.typeRefinementAssignableFromFQN.moduleName).distinct
    for {
      implFQNs <- candidateModules.flatTraverse(assignableFromImpls)
      matching <- implFQNs.filterA(markerMatches(_, query))
      result   <- matching.headOption.flatTraverse(implFQN => fetchBinding(implFQN).map(_.map(sem => (ctor, sem))))
    } yield result
  }

  /** The `assignableFrom` `TypeRefinement` implementation-method FQNs declared in a module (non-erroring). */
  private def assignableFromImpls(module: ModuleName): CompilerIO[List[ValueFQN]] =
    getFact(UnifiedModuleNames.Key(module)).map {
      case None        => Nil
      case Some(names) =>
        names.names.keys.toList.collect {
          case qn @ QualifiedName("assignableFrom", Qualifier.AbilityImplementation(ability, _))
              if ability.value == "TypeRefinement" =>
            ValueFQN(module, qn)
        }
    }

  /** Whether the implementation's marker pattern matches the queried constructor (reusing [[AbilityMatcher.matchImpl]],
    * the resolver's own matcher). The marker shares the implementation method's ability-implementation qualifier.
    */
  private def markerMatches(implFQN: ValueFQN, query: Seq[GroundValue]): CompilerIO[Boolean] = {
    val markerFQN = ValueFQN(implFQN.moduleName, QualifiedName("TypeRefinement", implFQN.name.qualifier))
    getFact(OperatorResolvedValue.Key(markerFQN)).flatMap {
      case None      => false.pure[CompilerIO]
      case Some(orv) =>
        AbilityMatcher.matchImpl(orv.typeStack.as(orv.typeStack.value.signature), query).map(_.isDefined)
    }
  }
}
