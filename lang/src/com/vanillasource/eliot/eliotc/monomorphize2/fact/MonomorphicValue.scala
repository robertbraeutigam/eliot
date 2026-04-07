package com.vanillasource.eliot.eliotc.monomorphize2.fact

import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.resolve.fact.QualifiedName
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** A monomorphized (specialized) value with all type parameters instantiated to concrete types.
  *
  * @param vfqn
  *   The fully qualified name of the original generic value
  * @param specifiedTypeArguments
  *   The type arguments as specified in the source.
  * @param calculatedTypeArguments
  *   The concrete type arguments used for specialization (each is a fully evaluated Value)
  * @param name
  *   The sourced name of the value
  * @param signature
  *   The concrete (ground) type of this specialized instance (a fully evaluated Value)
  * @param runtime
  *   The optional monomorphized runtime body
  */
case class MonomorphicValue(
    vfqn: ValueFQN,
    specifiedTypeArguments: Seq[Sourced[OperatorResolvedExpression]],
    calculatedTypeArguments: Seq[Value],
    signature: Value,
    runtime: Option[Sourced[MonomorphicExpression.Expression]]
) extends CompilerFact {
  override def key(): CompilerFactKey[MonomorphicValue] =
    MonomorphicValue.Key(vfqn, specifiedTypeArguments)
}

object MonomorphicValue {

  /** Composite key that uniquely identifies a monomorphic specialization. The same generic function with different type
    * arguments produces different keys.
    */
  case class Key(vfqn: ValueFQN, specifiedTypeArguments: Seq[Sourced[OperatorResolvedExpression]])
      extends CompilerFactKey[MonomorphicValue]
}
