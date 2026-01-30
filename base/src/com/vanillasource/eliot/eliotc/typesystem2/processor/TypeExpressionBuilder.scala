package com.vanillasource.eliot.eliotc.typesystem2.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.ExpressionStack
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Value}
import com.vanillasource.eliot.eliotc.eval.util.Types
import com.vanillasource.eliot.eliotc.module2.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.resolve2.fact.Expression
import com.vanillasource.eliot.eliotc.resolve2.fact.Expression as Expr
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.typesystem2.fact.TypedExpression
import com.vanillasource.eliot.eliotc.typesystem2.types.TypeCheckState.*

/** Evaluates type expressions to TypedExpressions. Tracks universal variables for generic type parameters. Works on
  * single expressions; stack-level processing is handled by the processor.
  */
object TypeExpressionBuilder {

  /** Build type constraints from a single type expression. Returns the typed AST with inferred type. */
  def build(
      expr: Expression,
      source: Sourced[?]
  ): TypeGraphIO[TypedExpression] =
    expr match {
      case Expr.FunctionLiteral(paramName, paramType, body) if paramType.value.expressions.isEmpty =>
        evaluateUniversalIntro(paramName, paramType, body)

      case Expr.FunctionLiteral(paramName, paramType, body) =>
        evaluateFunctionType(paramName, paramType, body)

      case Expr.ValueReference(vfqn) =>
        evaluateValueReference(vfqn)

      case Expr.ParameterReference(name) =>
        lookupParameter(name.value).map { maybeType =>
          val exprValue = maybeType.getOrElse(ParameterReference(name.value, Value.TypeType))
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
      _             <- addUniversalVar(paramName.value)
      _             <- tellUniversalVar(paramName.value)
      inner         <- build(body.value.expressions.head, body)
      typedParamType = paramType.as(ExpressionStack[TypedExpression](Seq.empty, paramType.value.hasRuntime))
      typedBody      = body.as(ExpressionStack[TypedExpression](Seq(inner), body.value.hasRuntime))
    } yield TypedExpression(
      inner.expressionType,
      TypedExpression.FunctionLiteral(paramName, typedParamType, typedBody)
    )

  /** Regular function literal (lambda type): (a: A) -> B becomes FunctionType */
  private def evaluateFunctionType(
      paramName: Sourced[String],
      paramType: Sourced[ExpressionStack[Expression]],
      body: Sourced[ExpressionStack[Expression]]
  ): TypeGraphIO[TypedExpression] =
    for {
      paramResult <- buildFromStack(paramType)
      _           <- bindParameter(paramName.value, paramResult.expressionType)
      bodyResult  <- buildFromStack(body)
      funcType     = functionType(paramResult.expressionType, bodyResult.expressionType)
      paramNested  = toNestedStack(paramType, paramResult)
      bodyNested   = toNestedStack(body, bodyResult)
    } yield TypedExpression(funcType, TypedExpression.FunctionLiteral(paramName, paramNested, bodyNested))

  /** Build from a nested stack by extracting the signature expression. For nested stacks, we only need the signature
    * level for type construction.
    */
  private def buildFromStack(
      stack: Sourced[ExpressionStack[Expression]]
  ): TypeGraphIO[TypedExpression] =
    stack.value.expressions.headOption match {
      case Some(expr) => build(expr, stack)
      case None       =>
        for {
          uvar <- generateUnificationVar(stack)
        } yield TypedExpression(uvar, TypedExpression.ParameterReference(stack.as(uvar.parameterName)))
    }

  /** Convert a build result to a nested stack for use in FunctionLiteral/FunctionApplication AST nodes. */
  private def toNestedStack(
      source: Sourced[ExpressionStack[Expression]],
      result: TypedExpression
  ): Sourced[ExpressionStack[TypedExpression]] =
    source.as(ExpressionStack(Seq(result), source.value.hasRuntime))

  /** Value reference - could be a type like Int, String, or a universal var */
  private def evaluateValueReference(
      vfqn: Sourced[ValueFQN]
  ): TypeGraphIO[TypedExpression] =
    isUniversalVar(vfqn.value.name).map { isUniv =>
      val exprValue =
        if (isUniv) ParameterReference(vfqn.value.name, Value.TypeType)
        else ConcreteValue(Types.dataType(vfqn.value))
      TypedExpression(exprValue, TypedExpression.ValueReference(vfqn))
    }

  /** Function application in type position: A(B) means A parameterized by B */
  private def evaluateTypeApplication(
      target: Sourced[ExpressionStack[Expression]],
      arg: Sourced[ExpressionStack[Expression]]
  ): TypeGraphIO[TypedExpression] =
    for {
      targetResult <- buildFromStack(target)
      argResult    <- buildFromStack(arg)
      resultType    = applyTypeApplication(targetResult.expressionType, argResult.expressionType)
      targetNested  = toNestedStack(target, targetResult)
      argNested     = toNestedStack(arg, argResult)
    } yield TypedExpression(resultType, TypedExpression.FunctionApplication(targetNested, argNested))

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
