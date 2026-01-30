package com.vanillasource.eliot.eliotc.symbolic.processor

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.ExpressionStack
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.eval.util.Types
import com.vanillasource.eliot.eliotc.module2.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.resolve2.fact.Expression as Expr
import com.vanillasource.eliot.eliotc.resolve2.fact.{Expression, ResolvedValue}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.symbolic.fact.TypedExpression
import com.vanillasource.eliot.eliotc.symbolic.types.*
import com.vanillasource.eliot.eliotc.symbolic.types.TypeCheckState.*

/** Infers types for body expressions. Generates unification variables for unknowns and emits type constraints. Uses
  * TypeExpressionBuilder for parameter types in lambdas.
  */
object BodyInferenceBuilder {

  /** Build constraints from a body expression, inferring types. */
  def build(
      body: Sourced[Expression]
  ): TypeGraphIO[TypedExpression] =
    body.value match {
      case Expr.IntegerLiteral(value) =>
        inferLiteral(value, "Number", "Byte", TypedExpression.IntegerLiteral(value))

      case Expr.StringLiteral(value) =>
        inferLiteral(value, "String", "String", TypedExpression.StringLiteral(value))

      case Expr.ParameterReference(name) =>
        lookupParameter(name.value).map { maybeType =>
          val exprValue = maybeType.getOrElse(ParameterReference(name.value, Value.TypeType))
          TypedExpression(exprValue, TypedExpression.ParameterReference(name))
        }

      case Expr.ValueReference(vfqn) =>
        inferValueReference(vfqn)

      case Expr.FunctionApplication(target, arg) =>
        inferFunctionApplication(body, target, arg)

      case Expr.FunctionLiteral(paramName, paramType, bodyStack) =>
        inferFunctionLiteral(paramName, paramType, bodyStack)
    }

  private def inferLiteral[T](
      value: Sourced[T],
      moduleName: String,
      typeName: String,
      typedExpr: TypedExpression.Expression
  ): TypeGraphIO[TypedExpression] =
    TypedExpression(
      ConcreteValue(Types.dataType(ValueFQN(ModuleName(Seq("eliot", "lang"), moduleName), typeName))),
      typedExpr
    ).pure[TypeGraphIO]

  private def inferValueReference(
      vfqn: Sourced[ValueFQN]
  ): TypeGraphIO[TypedExpression] =
    for {
      maybeResolved <- StateT.liftF(getFactOrAbort(ResolvedValue.Key(vfqn.value)).attempt.map(_.toOption))
      result        <- maybeResolved match {
                         case Some(resolved) =>
                           buildFromStack(resolved.value).map { typeResult =>
                             TypedExpression(typeResult.expressionType, TypedExpression.ValueReference(vfqn))
                           }
                         case None           =>
                           val exprValue = ConcreteValue(Types.dataType(vfqn.value))
                           TypedExpression(exprValue, TypedExpression.ValueReference(vfqn)).pure[TypeGraphIO]
                       }
    } yield result

  private def inferFunctionApplication(
      body: Sourced[Expression],
      target: Sourced[ExpressionStack[Expression]],
      arg: Sourced[ExpressionStack[Expression]]
  ): TypeGraphIO[TypedExpression] =
    for {
      argTypeVar      <- generateUnificationVar(arg)
      retTypeVar      <- generateUnificationVar(body)
      targetResult    <- build(target.map(_.expressions.head))
      argResult       <- build(arg.map(_.expressions.head))
      expectedFuncType = functionType(argTypeVar, retTypeVar)
      _               <- tellConstraint(
                           SymbolicUnification.constraint(
                             expectedFuncType,
                             target.as(targetResult.expressionType),
                             "Target of function application is not a Function. Possibly too many arguments."
                           )
                         )
      _               <- tellConstraint(
                           SymbolicUnification.constraint(argTypeVar, arg.as(argResult.expressionType), "Argument type mismatch.")
                         )
      typedTarget      = target.as(ExpressionStack[TypedExpression](Seq(targetResult), target.value.hasRuntime))
      typedArg         = arg.as(ExpressionStack[TypedExpression](Seq(argResult), arg.value.hasRuntime))
    } yield TypedExpression(retTypeVar, TypedExpression.FunctionApplication(typedTarget, typedArg))

  private def inferFunctionLiteral(
      paramName: Sourced[String],
      paramType: Sourced[ExpressionStack[Expression]],
      bodyStack: Sourced[ExpressionStack[Expression]]
  ): TypeGraphIO[TypedExpression] =
    for {
      paramResult   <- buildFromStack(paramType)
      _             <- bindParameter(paramName.value, paramResult.expressionType)
      bodyResult    <- build(bodyStack.map(_.expressions.head))
      funcType       = functionType(paramResult.expressionType, bodyResult.expressionType)
      typedParamType = paramType.as(ExpressionStack[TypedExpression](Seq(paramResult), paramType.value.hasRuntime))
      typedBodyStack = bodyStack.as(ExpressionStack[TypedExpression](Seq(bodyResult), bodyStack.value.hasRuntime))
    } yield TypedExpression(funcType, TypedExpression.FunctionLiteral(paramName, typedParamType, typedBodyStack))

  /** Build from a nested stack by extracting the signature expression. */
  private def buildFromStack(
      stack: Sourced[ExpressionStack[Expression]]
  ): TypeGraphIO[TypedExpression] =
    stack.value.expressions.headOption match {
      case Some(expr) => TypeExpressionBuilder.build(expr)
      case None       =>
        for {
          uvar <- generateUnificationVar(stack)
        } yield TypedExpression(uvar, TypedExpression.ParameterReference(stack.as(uvar.parameterName)))
    }

}
