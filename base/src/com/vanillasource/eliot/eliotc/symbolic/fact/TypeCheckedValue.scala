package com.vanillasource.eliot.eliotc.symbolic.fact

import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** A type-checked value with its signature type and optional runtime body.
  *
  * @param vfqn
  *   The fully qualified name of this value
  * @param name
  *   The sourced name
  * @param signature
  *   The type of this value (result of type checking)
  * @param runtime
  *   The optional typed runtime body expression (the signature is its type)
  */
case class TypeCheckedValue(
    vfqn: ValueFQN,
    name: Sourced[String],
    signature: ExpressionValue,
    runtime: Option[Sourced[TypedExpression.Expression]]
) extends CompilerFact {
  override def key(): CompilerFactKey[TypeCheckedValue] = TypeCheckedValue.Key(vfqn)
}

object TypeCheckedValue {
  case class Key(vfqn: ValueFQN) extends CompilerFactKey[TypeCheckedValue]
}
