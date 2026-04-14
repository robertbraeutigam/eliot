package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementation
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.check.TypeStackLoop
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicValue, NativeBinding}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
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
    TypeStackLoop.process(
      key,
      resolvedValue,
      fetchBinding = fetchBinding,
      resolveAbility = resolveAbilityImpl
    )

  private def resolveAbilityImpl(
      vfqn: ValueFQN,
      typeArgs: Seq[GroundValue]
  ): CompilerIO[Option[(ValueFQN, Seq[GroundValue])]] =
    getFact(AbilityImplementation.Key(vfqn, typeArgs)).map(
      _.map(impl => (impl.implementationFQN, impl.implementationTypeArgs))
    )
}
