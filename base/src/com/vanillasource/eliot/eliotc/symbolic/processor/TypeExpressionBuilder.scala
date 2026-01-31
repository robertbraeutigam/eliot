package com.vanillasource.eliot.eliotc.symbolic.processor

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.ExpressionStack
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Value}
import com.vanillasource.eliot.eliotc.eval.util.Types
import com.vanillasource.eliot.eliotc.module2.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.resolve2.fact.Expression
import com.vanillasource.eliot.eliotc.resolve2.fact.Expression as Expr
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import com.vanillasource.eliot.eliotc.symbolic.fact.TypedExpression
import com.vanillasource.eliot.eliotc.symbolic.types.TypeCheckState
import com.vanillasource.eliot.eliotc.symbolic.types.TypeCheckState.*

/** Evaluates type expressions to TypedExpressions. Tracks universal variables for generic type parameters. Processes
  * expression stacks top-down, validating each level against the expected type from above.
  */
object TypeExpressionBuilder {

  /** Build type constraints from a single type expression. Returns the typed AST with inferred type. */
  def build(expression: Expression): TypeGraphIO[TypedExpression] =
    expression match {
      case Expr.FunctionLiteral(paramName, paramType, body) if paramType.value.expressions.isEmpty =>
        evaluateUniversalIntro(paramName, paramType, body)

      case Expr.FunctionLiteral(paramName, paramType, body) =>
        evaluateFunctionType(paramName, paramType, body)

      case Expr.ValueReference(vfqn) =>
        evaluateValueReference(vfqn)

      case Expr.ParameterReference(name) =>
        lookupParameter(name.value).map { maybeType =>
          val exprValue = maybeType.getOrElse(ParameterReference(name.value, Value.Type))
          TypedExpression(exprValue, TypedExpression.ParameterReference(name))
        }

      case Expr.FunctionApplication(target, arg) =>
        evaluateTypeApplication(target, arg)

      case Expr.IntegerLiteral(value) =>
        val exprValue = ConcreteValue(Value.Direct(value.value, Types.bigIntType))
        TypedExpression(exprValue, TypedExpression.IntegerLiteral(value)).pure[TypeGraphIO]

      case Expr.StringLiteral(value) =>
        val exprValue = ConcreteValue(Value.Direct(value.value, Types.stringType))
        TypedExpression(exprValue, TypedExpression.StringLiteral(value)).pure[TypeGraphIO]
    }

  /** Universal variable introduction: A -> ... where A has empty type */
  private def evaluateUniversalIntro(
      paramName: Sourced[String],
      paramType: Sourced[ExpressionStack[Expression]],
      body: Sourced[ExpressionStack[Expression]]
  ): TypeGraphIO[TypedExpression] =
    for {
      _                              <- addUniversalVar(paramName.value)
      (bodyTypeValue, typedBodyStack) <- processStack(body)
      typedParamType                  = paramType.as(ExpressionStack[TypedExpression](Seq.empty, paramType.value.hasRuntime))
    } yield TypedExpression(
      bodyTypeValue,
      TypedExpression.FunctionLiteral(paramName, typedParamType, typedBodyStack)
    )

  /** Regular function literal (lambda type): (a: A) -> B becomes FunctionType */
  private def evaluateFunctionType(
      paramName: Sourced[String],
      paramType: Sourced[ExpressionStack[Expression]],
      body: Sourced[ExpressionStack[Expression]]
  ): TypeGraphIO[TypedExpression] =
    for {
      (paramTypeValue, typedParamStack) <- processStack(paramType)
      _                                 <- bindParameter(paramName.value, paramTypeValue)
      (bodyTypeValue, typedBodyStack)   <- processStack(body)
      funcType                           = functionType(paramTypeValue, bodyTypeValue)
    } yield TypedExpression(funcType, TypedExpression.FunctionLiteral(paramName, typedParamStack, typedBodyStack))

  /** Process a nested stack by processing all type levels from top to bottom. Each level's value must have valueType
    * matching the level above (with implicit Type at top).
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
          typeResult <- build(expr)
        } yield (typeResult.expressionType, Seq(typeResult))

      case expr :: rest =>
        // Higher level (above signature) - must evaluate to ConcreteValue
        for {
          // 1. Build the type expression
          typeResult <- build(expr)

          // 2. Extract the concrete value for use as expected type in next level
          evaluatedValue <- typeResult.expressionType match {
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

  /** Value reference - could be a type like Int, String, or a universal var */
  private def evaluateValueReference(
      vfqn: Sourced[ValueFQN]
  ): TypeGraphIO[TypedExpression] =
    isUniversalVar(vfqn.value.name).map { isUniv =>
      val exprValue =
        if (isUniv) ParameterReference(vfqn.value.name, Value.Type)
        else ConcreteValue(Types.dataType(vfqn.value))
      TypedExpression(exprValue, TypedExpression.ValueReference(vfqn))
    }

  /** Function application in type position: A(B) means A parameterized by B */
  private def evaluateTypeApplication(
      target: Sourced[ExpressionStack[Expression]],
      arg: Sourced[ExpressionStack[Expression]]
  ): TypeGraphIO[TypedExpression] =
    for {
      (targetTypeValue, typedTargetStack) <- processStack(target)
      (argTypeValue, typedArgStack)       <- processStack(arg)
      resultType                           = applyTypeApplication(targetTypeValue, argTypeValue)
    } yield TypedExpression(resultType, TypedExpression.FunctionApplication(typedTargetStack, typedArgStack))

  /** Apply a type constructor to an argument. Handles Function type specially. */
  private def applyTypeApplication(
      target: ExpressionValue,
      arg: ExpressionValue
  ): ExpressionValue =
    target match {
      case FunctionApplication(ConcreteValue(v), paramType) if isFunctionType(v) =>
        functionType(paramType, arg)
      case _                                                                     =>
        FunctionApplication(target, arg)
    }

  private def isFunctionType(v: Value): Boolean =
    v match {
      case Value.Structure(fields, _) =>
        fields.get("$typeName") match {
          case Some(Value.Direct(vfqn: ValueFQN, _)) =>
            vfqn.moduleName === ModuleName.systemFunctionModuleName && vfqn.name === "Function$DataType"
          case _                                     => false
        }
      case _                          => false
    }
}
