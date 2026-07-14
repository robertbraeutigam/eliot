package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementation
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.check.{MarkerGuardSignature, Track, TypeStackLoop}
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
  ): CompilerIO[MonomorphicValue] = {
    // An ability-implementation marker is monomorphized only to discharge its `where` guard (ability-guards §2.3); its
    // pattern-argument types are not real value parameters, so they are stripped to leave binders + guard return.
    val value = MarkerGuardSignature.strippedForGuard(saturatedValue.value)
    TypeStackLoop
      .process(
        key.typeArguments,
        value,
        fetchBinding = fetchBinding,
        resolveAbility = resolveAbilityImpl,
        track = Track.Runtime,
        reduceInstance = ReducedBindingClosure.reduceInstance
      )
      .map(result =>
        MonomorphicValue(
          key.vfqn,
          key.typeArguments,
          value.signature.as(key.vfqn.name),
          result.signature,
          result.body
        )
      )
  }

  private def resolveAbilityImpl(
      vfqn: ValueFQN,
      typeArgs: Seq[GroundValue]
  ): CompilerIO[Option[(ValueFQN, Seq[GroundValue])]] =
    getFactIfProduced(AbilityImplementation.Key(vfqn, typeArgs, Platform.Runtime)).map(
      _.flatMap(_.resolution.resolved)
    )
}
