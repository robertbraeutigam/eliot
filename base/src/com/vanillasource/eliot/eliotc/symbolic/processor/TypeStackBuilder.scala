package com.vanillasource.eliot.eliotc.symbolic.processor

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.ExpressionStack
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.resolve2.fact.Expression
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import com.vanillasource.eliot.eliotc.symbolic.fact.TypedExpression
import com.vanillasource.eliot.eliotc.symbolic.types.TypeCheckState
import com.vanillasource.eliot.eliotc.symbolic.types.TypeCheckState.*

/** Processes expression stacks by validating type levels top-down. Each level's value must have valueType matching the
  * level above (with implicit Type at top).
  */
object TypeStackBuilder {

  /** Process a stack by processing all type levels from top to bottom.
    *
    * Note: This only processes TYPE levels. If the stack has a runtime expression (hasRuntime=true), it is excluded
    * from processing. The runtime expression must be handled separately by the body inference.
    *
    * @return
    *   Tuple of (signatureType, typedStack with only type levels)
    */
  def processStack(
      stack: Sourced[ExpressionStack[Expression]]
  ): TypeGraphIO[(ExpressionValue, Sourced[ExpressionStack[TypedExpression]])] = {
    val expressions = stack.value.expressions
    val hasRuntime  = stack.value.hasRuntime

    // Extract type levels only (excluding runtime if present)
    // If hasRuntime=true and length > 1: first is runtime, rest are type levels
    // If hasRuntime=true and length = 1: single expression serves as type (common for nested stacks in type positions)
    // If hasRuntime=false: all expressions are type levels
    val typeLevelExprs =
      if (hasRuntime && expressions.length > 1) expressions.drop(1)
      else expressions

    // Process from top (last) to bottom (first/signature)
    typeLevelExprs.toList.reverse match {
      case Nil    =>
        // No explicit type levels - generate unification variable for signature
        for {
          uvar <- generateUnificationVar(stack)
        } yield (uvar, stack.as(ExpressionStack[TypedExpression](Seq.empty, hasRuntime)))
      case levels =>
        // Process from top to bottom, starting with Type as expected
        for {
          (signatureType, typedLevels) <- processLevelsRecursive(levels, Value.Type, stack)
        } yield (signatureType, stack.as(ExpressionStack(typedLevels.reverse, hasRuntime)))
    }
  }

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
  private def processLevelsRecursive(
      levels: List[Expression],
      expectedType: Value,
      source: Sourced[?]
  ): TypeGraphIO[(ExpressionValue, Seq[TypedExpression])] =
    levels match {
      case Nil =>
        // Base case: no more levels
        (ConcreteValue(expectedType), Seq.empty[TypedExpression]).pure[TypeGraphIO]

      case expr :: rest if rest.isEmpty =>
        // Signature level - build type expression
        for {
          typeResult <- TypeExpressionBuilder.build(expr)
        } yield (typeResult.expressionType, Seq(typeResult))

      case expr :: rest =>
        // Higher level (above signature) - must evaluate to ConcreteValue
        for {
          // 1. Build the type expression
          typeResult <- TypeExpressionBuilder.build(expr)

          // 2. Extract the concrete value for use as expected type in next level
          evaluatedValue                   <- typeResult.expressionType match {
                                                case ConcreteValue(v) =>
                                                  // 3. Check the value's type matches expected
                                                  if (v.valueType == expectedType) {
                                                    v.pure[TypeGraphIO]
                                                  } else {
                                                    StateT.liftF(
                                                      compilerError(
                                                        source.as(
                                                          s"Type level mismatch: expected ${expectedType}, but got ${v.valueType}"
                                                        )
                                                      )
                                                    ) *> v.pure[TypeGraphIO]
                                                  }
                                                case other            =>
                                                  StateT.liftF(
                                                    compilerError(
                                                      source.as("Higher level type annotation must evaluate to a concrete type.")
                                                    )
                                                  ) *> Value.Type.pure[TypeGraphIO]
                                              }

          // 4. Process remaining levels with evaluated value as expected type
          (signatureType, restTypedLevels) <- processLevelsRecursive(rest, evaluatedValue, source)
        } yield (signatureType, typeResult +: restTypedLevels)
    }
}
