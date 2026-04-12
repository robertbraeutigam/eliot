package com.vanillasource.eliot.eliotc.monomorphize3.fact

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** A monomorphized (specialized) value with all type parameters instantiated to concrete types via NbE. Unlike
  * monomorphize2's MonomorphicValue, this drops `calculatedTypeArguments` because NbE folds concrete type args into the
  * signature directly.
  *
  * @param vfqn
  *   The fully qualified name of the original value
  * @param specifiedTypeArguments
  *   The type arguments as specified in the source
  * @param signature
  *   The concrete ground type of this specialized instance
  * @param runtime
  *   The optional monomorphized runtime body
  */
case class Monomorphic3Value(
    vfqn: ValueFQN,
    specifiedTypeArguments: Seq[Sourced[OperatorResolvedExpression]],
    signature: GroundValue,
    runtime: Option[Sourced[Monomorphic3Expression.Expression]]
) extends CompilerFact {
  override def key(): CompilerFactKey[Monomorphic3Value] =
    Monomorphic3Value.Key(vfqn, specifiedTypeArguments)
}

object Monomorphic3Value {

  /** Composite key that uniquely identifies a monomorphic specialization.
    */
  case class Key(vfqn: ValueFQN, specifiedTypeArguments: Seq[Sourced[OperatorResolvedExpression]])
      extends CompilerFactKey[Monomorphic3Value]
}
