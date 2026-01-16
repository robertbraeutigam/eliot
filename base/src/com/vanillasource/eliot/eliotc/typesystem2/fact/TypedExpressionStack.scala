package com.vanillasource.eliot.eliotc.typesystem2.fact

/** An expression stack where each expression has been type-checked. The stack preserves the layered structure where
  * each layer types the one below.
  */
case class TypedExpressionStack(expressions: Seq[TypedExpression])

object TypedExpressionStack {
  def of(expression: TypedExpression) = TypedExpressionStack(Seq(expression))

  def empty = TypedExpressionStack(Seq.empty)
}
