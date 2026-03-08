package com.vanillasource.eliot.eliotc.symbolic.processor

import cats.data.{NonEmptySeq, StateT}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.ConcreteValue
import com.vanillasource.eliot.eliotc.eval.fact.Value.Type
import com.vanillasource.eliot.eliotc.eval.util.Evaluator
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.symbolic.fact.TypedExpression
import com.vanillasource.eliot.eliotc.symbolic.types.TypeCheckState.TypeGraphIO

/** Symbolically evaluates and type checks an expression stack.
  *
  * These are the forms of data in this class:
  *   - OperatorResolvedExpression: These are the "raw" expressions from the previous processing step. These are
  *     organized in stacks, where every level defines the type of the next level. Also all levels may have smaller
  *     stacks as parts, like lambda bodies, parameter types, function calls, etc. It's a sort-of fractal data
  *     structure.
  *   - ExpressionValue: In this class this always refers to a "normal form", i.e. a symbolic evaluation of an
  *     expression. This means it will inline and reduce all referenced functions except constructors which will stay as
  *     structural elements to unify later.
  *   - TypedExpression: A pair of an ExpressionValue describing type and TypedExpression.Expression, which is the same
  *     as an OperatorResolvedExpression, except it is not stacked anymore. All type information if "flattened" to a
  *     single ExpressionValue.
  */
object SymbolicEvaluator2 {

  /** Typecheck the given expression stack and return the typed expression, i.e. the result type and the expression when
    * run producing the result.
    */
  def typeCheck(expressions: NonEmptySeq[Sourced[OperatorResolvedExpression]]): TypeGraphIO[TypedExpression] =
    for {
      // The topmost expression needs to be of type "Type" and there is always a topmost expression (non-empty seq)
      top    <- typeCheck(ConcreteValue(Type), expressions.head)
      // Iterate the rest, every level is the type of the next level
      result <- expressions.tail.foldLeftM((top, expressions.head)) { (acc, expression) =>
                  for {
                    // Convert previous level to normalized type expression
                    previousLevel   <- StateT.liftF(Evaluator.toNormalFormExpressionValue(acc._2))
                    // Create constraints of this level against the assumed type
                    typedExpression <- typeCheck(previousLevel, expression)
                  } yield (typedExpression, expression)
                }
    } yield result._1

  private def typeCheck(
      resultType: ExpressionValue,
      expression: Sourced[OperatorResolvedExpression]
  ): TypeGraphIO[TypedExpression] =
    ???
}
