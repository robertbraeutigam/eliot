package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementation
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.check.TypeStackLoop
import com.vanillasource.eliot.eliotc.monomorphize.domain.{Env, SemValue}
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
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
      fetchValueType = vfqn => fetchEvaluatedSignature(vfqn),
      resolveAbility = resolveAbilityImpl
    )

  private def resolveAbilityImpl(
      vfqn: ValueFQN,
      typeArgs: Seq[GroundValue]
  ): CompilerIO[Option[(ValueFQN, Seq[GroundValue])]] =
    getFact(AbilityImplementation.Key(vfqn, typeArgs)).map(
      _.map(impl => (impl.implementationFQN, impl.implementationTypeArgs))
    )

  /** Fetch a value's type stack signature, evaluate it to a SemValue. Uses NativeBinding lookups for proper resolution.
    */
  private def fetchEvaluatedSignature(vfqn: ValueFQN): CompilerIO[Option[SemValue]] =
    getFact(OperatorResolvedValue.Key(vfqn)).flatMap {
      case Some(orv) =>
        val levels = orv.typeStack.value.levels.toSeq
        for {
          bindings <- collectBindings(levels)
        } yield {
          val evaluator = new Evaluator(v => bindings.get(v), Map.empty)
          val reversed  = levels.reverse
          val result    = reversed.foldLeft(SemValue.VType.asInstanceOf[SemValue]) { (_, level) =>
            evaluator.eval(Env.empty, level)
          }
          Some(result)
        }
      case None      => None.pure[CompilerIO]
    }

  /** Recursively collect all NativeBindings referenced in ORE expressions. */
  private def collectBindings(
      levels: Seq[com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression]
  ): CompilerIO[Map[ValueFQN, SemValue]] =
    levels.foldLeft(Map.empty[ValueFQN, SemValue].pure[CompilerIO]) { (acc, level) =>
      acc.flatMap(m =>
        com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
          .foldValueReferences(level, m) { (map, name) =>
            if (map.contains(name.value)) map.pure[CompilerIO]
            else fetchBinding(name.value).map(_.fold(map)(sem => map + (name.value -> sem)))
          }
      )
    }
}
