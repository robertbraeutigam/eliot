package com.vanillasource.eliot.eliotc.monomorphize.fact

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.core.fact.QualifiedName
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** A monomorphized (specialized) value with all type parameters instantiated to concrete types via NbE.
  *
  * @param vfqn
  *   The fully qualified name of the original value
  * @param specifiedTypeArguments
  *   The type arguments as specified in the source
  * @param name
  *   The sourced name of the value
  * @param signature
  *   The concrete ground type of this specialized instance
  * @param runtime
  *   The optional monomorphized runtime body
  */
case class MonomorphicValue(
    vfqn: ValueFQN,
    specifiedTypeArguments: Seq[Sourced[OperatorResolvedExpression]],
    name: Sourced[QualifiedName],
    signature: GroundValue,
    runtime: Option[Sourced[MonomorphicExpression.Expression]]
) extends CompilerFact {
  override def key(): CompilerFactKey[MonomorphicValue] =
    MonomorphicValue.Key(vfqn, specifiedTypeArguments)
}

object MonomorphicValue {

  /** Composite key that uniquely identifies a monomorphic specialization.
    */
  case class Key(vfqn: ValueFQN, specifiedTypeArguments: Seq[Sourced[OperatorResolvedExpression]])
      extends CompilerFactKey[MonomorphicValue]
}
