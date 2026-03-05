package com.vanillasource.eliot.eliotc.symbolic.processor

import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.symbolic.fact.TypedExpression
import com.vanillasource.eliot.eliotc.symbolic.types.TypeCheckState.TypeGraphIO

object SymbolicEvaluator2 {

  /** Typecheck the given expression stack and return the typed expression, i.e. the result type and the expression when
    * run producing the result.
    */
  def typeCheck(
      expressions: Seq[Sourced[OperatorResolvedExpression]]
  ): TypeGraphIO[TypedExpression] =
    ???

  /** Evaluate the given expression to its symbolic normal form. The normal form is where all referenced values are
    * substituted, "native" functions are "skipped", all other functions a beta-reduced, and constructors are left as
    * structure.
    */
  def evaluateToNormalForm(expression: Sourced[OperatorResolvedExpression]): ExpressionValue =
    ???

}
