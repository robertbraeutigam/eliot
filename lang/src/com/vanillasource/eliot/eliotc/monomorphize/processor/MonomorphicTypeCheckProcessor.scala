package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementation
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.check.{Track, TypeStackLoop}
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicValue, NativeBinding}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue

/** Entry point for NbE-based type checking (monomorphize). Delegates to TypeStackLoop for the actual type checking
  * work. Reads the [[SaturatedValue]] (the operator-resolved value with parameter-position bare omittable references
  * generalized into explicit generic binders), not the raw `OperatorResolvedValue`.
  */
class MonomorphicTypeCheckProcessor
    extends TransformationProcessor[SaturatedValue.Key, MonomorphicValue.Key](key => SaturatedValue.Key(key.vfqn)) {

  private def fetchBinding(vfqn: ValueFQN): CompilerIO[Option[SemValue]] =
    getFactIfProduced(NativeBinding.Key(vfqn, Platform.Runtime)).map(_.map(_.semValue))

  override protected def generateFromKeyAndFact(
      key: MonomorphicValue.Key,
      saturatedValue: SaturatedValue
  ): CompilerIO[MonomorphicValue] =
    TypeStackLoop
      .process(
        key.typeArguments,
        saturatedValue.value,
        fetchBinding = fetchBinding,
        resolveAbility = resolveAbilityImpl,
        track = Track.Runtime
      )
      .map(result =>
        MonomorphicValue(
          key.vfqn,
          key.typeArguments,
          saturatedValue.value.typeStack.as(key.vfqn.name),
          result.signature,
          result.body
        )
      )

  private def resolveAbilityImpl(
      vfqn: ValueFQN,
      typeArgs: Seq[GroundValue]
  ): CompilerIO[Option[(ValueFQN, Seq[GroundValue])]] =
    getFactIfProduced(AbilityImplementation.Key(vfqn, typeArgs, Platform.Runtime)).map(
      _.map(impl => (impl.implementationFQN, impl.implementationTypeArgs))
    )
}
