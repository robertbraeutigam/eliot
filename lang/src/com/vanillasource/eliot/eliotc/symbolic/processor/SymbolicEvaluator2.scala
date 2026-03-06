package com.vanillasource.eliot.eliotc.symbolic.processor

import cats.data.{NonEmptySeq, StateT}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.ConcreteValue
import com.vanillasource.eliot.eliotc.eval.fact.Value.Type
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.symbolic.fact.TypedExpression
import com.vanillasource.eliot.eliotc.symbolic.types.TypeCheckState.TypeGraphIO

object SymbolicEvaluator2 {

  /** Typecheck the given expression stack and return the typed expression, i.e. the result type and the expression when
    * run producing the result.
    */
  def typeCheck(expressions: NonEmptySeq[Sourced[OperatorResolvedExpression]]): TypeGraphIO[TypedExpression] =
    for {
      // The topmost expression needs to be of type "Type" and there is a topmost expression always
      top    <- typeCheck(ConcreteValue(Type), expressions.head)
      // Iterate the rest, every level is the type of the next level
      result <- expressions.tail.foldLeftM((top, expressions.head)) { (acc, expression) =>
                  for {
                    previousLevel   <- StateT.liftF(evaluateToNormalForm(acc._2))
                    typedExpression <- typeCheck(previousLevel, expression)
                  } yield (typedExpression, expression)
                }
    } yield result._1

  /** Evaluate the given expression to its symbolic normal form. The normal form is where all referenced values are
    * substituted, "native" functions are "skipped", all other functions a beta-reduced, and constructors are left as
    * structure. TODO: this should be provided by Evaluator.
    */
  def evaluateToNormalForm(expression: Sourced[OperatorResolvedExpression]): CompilerIO[ExpressionValue] =
    ???

  private def typeCheck(
      resultType: ExpressionValue,
      expression: Sourced[OperatorResolvedExpression]
  ): TypeGraphIO[TypedExpression] =
    ???
}
