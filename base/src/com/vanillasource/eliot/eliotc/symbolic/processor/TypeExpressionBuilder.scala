package com.vanillasource.eliot.eliotc.symbolic.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.ExpressionStack
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Value}
import com.vanillasource.eliot.eliotc.eval.util.Types
import com.vanillasource.eliot.eliotc.module2.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.resolve2.fact.Expression
import com.vanillasource.eliot.eliotc.resolve2.fact.Expression as Expr
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.symbolic.fact.TypedExpression
import com.vanillasource.eliot.eliotc.symbolic.types.TypeCheckState.*

/** Builds typed expressions from type-level expressions. Handles pattern matching on expression types and constructs
  * the corresponding typed AST nodes.
  */
object TypeExpressionBuilder {

  /** Build a typed expression from a single type expression. */
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
      _                               <- addUniversalVar(paramName.value)
      (bodyTypeValue, typedBodyStack) <- TypeStackBuilder.processStack(body)
      typedParamType                   = paramType.as(ExpressionStack[TypedExpression](Seq.empty, paramType.value.hasRuntime))
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
      (paramTypeValue, typedParamStack) <- TypeStackBuilder.processStack(paramType)
      _                                 <- bindParameter(paramName.value, paramTypeValue)
      (bodyTypeValue, typedBodyStack)   <- TypeStackBuilder.processStack(body)
      funcType                           = functionType(paramTypeValue, bodyTypeValue)
    } yield TypedExpression(funcType, TypedExpression.FunctionLiteral(paramName, typedParamStack, typedBodyStack))

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
      (targetTypeValue, typedTargetStack) <- TypeStackBuilder.processStack(target)
      (argTypeValue, typedArgStack)       <- TypeStackBuilder.processStack(arg)
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
