package com.vanillasource.eliot.eliotc.symbolic.processor

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.resolve2.fact.Expression
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import com.vanillasource.eliot.eliotc.symbolic.fact.TypedExpression
import com.vanillasource.eliot.eliotc.symbolic.types.TypeCheckState.*

/** Processes type stacks by validating type levels top-down. Each level's value must have valueType matching the level
  * above (with implicit Type at top).
  */
object TypeStackBuilder {

  /** Process a type stack by processing all type levels from top to bottom.
    *
    * @return
    *   Tuple of (signatureType, typedStack)
    */
  def processStack(
      stack: Sourced[TypeStack[Expression]]
  ): TypeGraphIO[(ExpressionValue, Sourced[TypeStack[TypedExpression]])] =
    for {
      (signatureType, typedLevels) <- processLevels(stack.value.levels.toList.reverse, Value.Type, stack)
    } yield (signatureType, stack.as(TypeStack(typedLevels.reverse)))

  /** Recursively process type levels from top (highest) to bottom (signature).
    *
    * @param levels
    *   Remaining levels to process (from top to bottom)
    * @param expectedType
    *   The expected type for the current level's value
    * @param source
    *   Source location for error messages
    * @return
    *   Tuple of (signatureType, typedLevels in reverse order)
    */
  private def processLevels(
      levels: List[Expression],
      expectedType: Value,
      source: Sourced[?]
  ): TypeGraphIO[(ExpressionValue, Seq[TypedExpression])] =
    levels match {
      case Nil          =>
        generateUnificationVar(source).map((_, Seq.empty))

      case head :: Nil  =>
        TypeExpressionBuilder.build(head).map(typeResult => (typeResult.expressionType, Seq(typeResult)))

      case expr :: rest =>
        for {
          typeResult                       <- TypeExpressionBuilder.build(expr)
          evaluatedValue                   <- extractConcreteValue(typeResult, expectedType, source)
          (signatureType, restTypedLevels) <- processLevels(rest, evaluatedValue, source)
        } yield (signatureType, typeResult +: restTypedLevels)
    }

  private def extractConcreteValue(
      typeResult: TypedExpression,
      expectedType: Value,
      source: Sourced[?]
  ): TypeGraphIO[Value] =
    typeResult.expressionType match {
      case ConcreteValue(v) if v.valueType == expectedType =>
        v.pure[TypeGraphIO]
      case ConcreteValue(v)                                =>
        StateT.liftF(
          compilerError(source.as(s"Type level mismatch: expected $expectedType, but got ${v.valueType}"))
        ) *> v.pure[TypeGraphIO]
      case _                                               =>
        StateT.liftF(
          compilerError(source.as("Higher level type annotation must evaluate to a concrete type."))
        ) *> Value.Type.pure[TypeGraphIO]
    }
}

