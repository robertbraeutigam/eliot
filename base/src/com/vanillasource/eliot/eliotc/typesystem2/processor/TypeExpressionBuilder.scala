package com.vanillasource.eliot.eliotc.typesystem2.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.ExpressionStack
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.eval.util.Types
import com.vanillasource.eliot.eliotc.module2.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.resolve2.fact.Expression
import com.vanillasource.eliot.eliotc.resolve2.fact.Expression as Expr
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.typesystem2.fact.TypedExpression
import com.vanillasource.eliot.eliotc.typesystem2.types.*
import com.vanillasource.eliot.eliotc.typesystem2.types.TypeCheckState.*

/** Evaluates type expressions to ExpressionValues and TypedExpressions. Tracks universal variables for generic type
  * parameters. Works on single expressions; stack-level processing is handled by the processor.
  */
object TypeExpressionBuilder {

  /** Build type constraints from a single type expression. Returns the inferred type as exprValue and the typed AST. */
  def build(
      expr: Expression,
      source: Sourced[?]
  ): TypeGraphIO[TypeWithTyped] =
    expr match {
      case Expr.FunctionLiteral(paramName, paramType, body) if paramType.value.expressions.isEmpty =>
        evaluateUniversalIntro(paramName, paramType, body)

      case Expr.FunctionLiteral(paramName, paramType, body) =>
        evaluateFunctionType(paramName, paramType, body)

      case Expr.ValueReference(vfqn) =>
        evaluateValueReference(vfqn)

      case Expr.ParameterReference(name) =>
        resolveParameterRef(name)

      case Expr.FunctionApplication(target, arg) =>
        evaluateTypeApplication(target, arg)

      case Expr.IntegerLiteral(value) =>
        val exprValue = ConcreteValue(Value.Direct(value.value, Types.bigIntType))
        TypeWithTyped(exprValue, TypedExpression(exprValue, TypedExpression.IntegerLiteral(value)))
          .pure[TypeGraphIO]

      case Expr.StringLiteral(value) =>
        val exprValue = ConcreteValue(Value.Direct(value.value, Types.stringType))
        TypeWithTyped(exprValue, TypedExpression(exprValue, TypedExpression.StringLiteral(value)))
          .pure[TypeGraphIO]
    }

  /** Universal variable introduction: A -> ... where A has empty type */
  private def evaluateUniversalIntro(
      paramName: Sourced[String],
      paramType: Sourced[ExpressionStack[Expression]],
      body: Sourced[ExpressionStack[Expression]]
  ): TypeGraphIO[TypeWithTyped] =
    for {
      _             <- addUniversalVar(paramName.value)
      _             <- tellUniversalVar(paramName.value)
      inner         <- build(body.value.expressions.head, body)
      typedParamType = paramType.as(ExpressionStack[TypedExpression](Seq.empty, paramType.value.hasRuntime))
      typedBody      = body.as(ExpressionStack[TypedExpression](Seq(inner.typed), body.value.hasRuntime))
    } yield TypeWithTyped(
      inner.exprValue,
      TypedExpression(inner.exprValue, TypedExpression.FunctionLiteral(paramName, typedParamType, typedBody))
    )

  /** Regular function literal (lambda type): (a: A) -> B becomes FunctionType */
  private def evaluateFunctionType(
      paramName: Sourced[String],
      paramType: Sourced[ExpressionStack[Expression]],
      body: Sourced[ExpressionStack[Expression]]
  ): TypeGraphIO[TypeWithTyped] =
    for {
      paramResult <- buildFromStack(paramType)
      _           <- bindParameter(paramName.value, paramResult.exprValue)
      bodyResult  <- buildFromStack(body)
      funcType     = functionType(paramResult.exprValue, bodyResult.exprValue)
      paramNested  = toNestedStack(paramType, paramResult)
      bodyNested   = toNestedStack(body, bodyResult)
    } yield TypeWithTyped(
      funcType,
      TypedExpression(funcType, TypedExpression.FunctionLiteral(paramName, paramNested, bodyNested))
    )

  /** Build from a nested stack by extracting the signature expression. For nested stacks, we only need the signature
    * level for type construction. Returns both the type and typed expression for the signature.
    */
  private def buildFromStack(
      stack: Sourced[ExpressionStack[Expression]]
  ): TypeGraphIO[TypeWithTyped] =
    stack.value.expressions.headOption match {
      case Some(expr) => build(expr, stack)
      case None       =>
        for {
          uvar <- generateUnificationVar(stack)
        } yield TypeWithTyped(uvar, TypedExpression(uvar, TypedExpression.ParameterReference(stack.as(uvar.parameterName))))
    }

  /** Convert a build result to a nested stack for use in FunctionLiteral/FunctionApplication AST nodes. */
  private def toNestedStack(
      source: Sourced[ExpressionStack[Expression]],
      result: TypeWithTyped
  ): Sourced[ExpressionStack[TypedExpression]] =
    source.as(ExpressionStack(Seq(result.typed), source.value.hasRuntime))

  /** Value reference - could be a type like Int, String, or a universal var */
  private def evaluateValueReference(
      vfqn: Sourced[ValueFQN]
  ): TypeGraphIO[TypeWithTyped] =
    isUniversalVar(vfqn.value.name).map { isUniv =>
      val exprValue =
        if (isUniv) ParameterReference(vfqn.value.name, Value.TypeType)
        else ConcreteValue(Types.dataType(vfqn.value))
      TypeWithTyped(exprValue, TypedExpression(exprValue, TypedExpression.ValueReference(vfqn)))
    }

  /** Function application in type position: A(B) means A parameterized by B */
  private def evaluateTypeApplication(
      target: Sourced[ExpressionStack[Expression]],
      arg: Sourced[ExpressionStack[Expression]]
  ): TypeGraphIO[TypeWithTyped] =
    for {
      targetResult <- buildFromStack(target)
      argResult    <- buildFromStack(arg)
      resultType    = applyTypeApplication(targetResult.exprValue, argResult.exprValue)
      targetNested  = toNestedStack(target, targetResult)
      argNested     = toNestedStack(arg, argResult)
    } yield TypeWithTyped(
      resultType,
      TypedExpression(resultType, TypedExpression.FunctionApplication(targetNested, argNested))
    )

  /** Apply a type constructor to an argument. Handles Function type specially. */
  private def applyTypeApplication(
      target: ExpressionValue,
      arg: ExpressionValue
  ): ExpressionValue =
    target match {
      case FunctionApplication(ConcreteValue(v), paramType) if isFunctionType(v) =>
        functionType(paramType, arg)
      case ConcreteValue(v) if isFunctionType(v) =>
        FunctionApplication(target, arg)
      case _ =>
        FunctionApplication(target, arg)
    }

  private def isFunctionType(v: Value): Boolean =
    v match {
      case Value.Structure(fields, _) =>
        fields.get("$typeName") match {
          case Some(Value.Direct(vfqn: ValueFQN, _)) =>
            vfqn.moduleName === ModuleName.systemFunctionModuleName && vfqn.name === "Function$DataType"
          case _ => false
        }
      case _ => false
    }
}
