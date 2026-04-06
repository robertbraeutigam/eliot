package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{Qualifier as CoreQualifier}
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.implementation.fact.AbilityImplementation
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicExpression
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Resolves ability method references to their concrete implementations during monomorphization. */
object MonomorphicAbilityResolver {

  /** Check if a ValueFQN refers to an ability method. */
  def isAbilityRef(vfqn: ValueFQN): Boolean =
    vfqn.name.qualifier.isInstanceOf[CoreQualifier.Ability]

  /** Resolve an ability method call to its concrete implementation. */
  def resolve(
      vfqn: Sourced[ValueFQN],
      typeArgs: Seq[Value],
      concreteType: Value,
      source: Sourced[?]
  ): CompilerIO[MonomorphicExpression] =
    for {
      abilityTypeParamCount <- countAbilityTypeParams(vfqn.value)
      abilityTypeArgs        = typeArgs.take(abilityTypeParamCount)
      impl                  <- getFactOrAbort(AbilityImplementation.Key(vfqn.value, abilityTypeArgs))
    } yield MonomorphicExpression(
      concreteType,
      MonomorphicExpression.MonomorphicValueReference(vfqn.as(impl.implementationFQN), impl.implementationTypeArgs)
    )

  private def countAbilityTypeParams(vfqn: ValueFQN): CompilerIO[Int] = {
    val abilityName = vfqn.name.qualifier.asInstanceOf[CoreQualifier.Ability].name
    val markerVFQN  =
      ValueFQN(
        vfqn.moduleName,
        com.vanillasource.eliot.eliotc.core.fact.QualifiedName(abilityName, CoreQualifier.Ability(abilityName))
      )
    ValueReferenceResolver
      .evaluateValueType(markerVFQN)
      .map(ev => TypeParameterAnalysis.fromEvaluatedType(ev).allTypeParams.size)
  }
}
