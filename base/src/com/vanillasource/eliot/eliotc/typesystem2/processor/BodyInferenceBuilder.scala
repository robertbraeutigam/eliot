package com.vanillasource.eliot.eliotc.typesystem2.processor

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.ExpressionStack
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.util.Types
import com.vanillasource.eliot.eliotc.module2.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.resolve2.fact.{Expression, ResolvedValue}
import com.vanillasource.eliot.eliotc.resolve2.fact.Expression as Expr
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.typesystem2.fact.TypedExpression
import com.vanillasource.eliot.eliotc.typesystem2.types.*
import com.vanillasource.eliot.eliotc.typesystem2.types.TypeCheckState.*

/** Infers types for body expressions. Generates unification variables for unknowns and emits type constraints. Uses
  * TypeExpressionBuilder for parameter types in lambdas.
  */
object BodyInferenceBuilder {

  /** Build constraints from a body expression, inferring types. */
  def build(
      body: Sourced[Expression]
  ): TypeGraphIO[TypeWithTyped] =
    body.value match {
      case Expr.IntegerLiteral(value) =>
        inferLiteral(value, "Number", "Byte", TypedExpression.IntegerLiteral(value))

      case Expr.StringLiteral(value) =>
        inferLiteral(value, "String", "String", TypedExpression.StringLiteral(value))

      case Expr.ParameterReference(name) =>
        resolveParameterRef(name)

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
  ): TypeGraphIO[TypeWithTyped] = {
    val inferredType = primitiveType(moduleName, typeName)
    TypeWithTyped(inferredType, TypedExpression(inferredType, typedExpr))
      .pure[TypeGraphIO]
  }

  private def inferValueReference(
      vfqn: Sourced[ValueFQN]
  ): TypeGraphIO[TypeWithTyped] =
    for {
      maybeResolved <- StateT.liftF(getFactOrAbort(ResolvedValue.Key(vfqn.value)).attempt.map(_.toOption))
      result        <- maybeResolved match {
                         case Some(resolved) =>
                           TypeExpressionBuilder.build(resolved.value).map { typeResult =>
                             TypeWithTyped(
                               typeResult.exprValue,
                               TypedExpression(typeResult.exprValue, TypedExpression.ValueReference(vfqn))
                             )
                           }
                         case None           =>
                           val exprValue = ConcreteValue(Types.dataType(vfqn.value))
                           TypeWithTyped(
                             exprValue,
                             TypedExpression(exprValue, TypedExpression.ValueReference(vfqn))
                           ).pure[TypeGraphIO]
                       }
    } yield result

  private def inferFunctionApplication(
      body: Sourced[Expression],
      target: Sourced[ExpressionStack[Expression]],
      arg: Sourced[ExpressionStack[Expression]]
  ): TypeGraphIO[TypeWithTyped] =
    for {
      argTypeVar   <- generateUnificationVar(arg)
      retTypeVar   <- generateUnificationVar(body)
      targetResult <- build(target.map(_.expressions.head))
      argResult    <- build(arg.map(_.expressions.head))
      expectedFuncType = functionType(argTypeVar, retTypeVar)
      _            <- tellConstraint(
                        SymbolicUnification.constraint(
                          expectedFuncType,
                          target.as(targetResult.exprValue),
                          "Target of function application is not a Function. Possibly too many arguments."
                        )
                      )
      _            <- tellConstraint(
                        SymbolicUnification.constraint(argTypeVar, arg.as(argResult.exprValue), "Argument type mismatch.")
                      )
      typedTarget   = target.as(ExpressionStack[TypedExpression](Seq(targetResult.typed), target.value.hasRuntime))
      typedArg      = arg.as(ExpressionStack[TypedExpression](Seq(argResult.typed), arg.value.hasRuntime))
    } yield TypeWithTyped(
      retTypeVar,
      TypedExpression(retTypeVar, TypedExpression.FunctionApplication(typedTarget, typedArg))
    )

  private def inferFunctionLiteral(
      paramName: Sourced[String],
      paramType: Sourced[ExpressionStack[Expression]],
      bodyStack: Sourced[ExpressionStack[Expression]]
  ): TypeGraphIO[TypeWithTyped] =
    for {
      paramResult   <- TypeExpressionBuilder.build(paramType)
      _             <- bindParameter(paramName.value, paramResult.exprValue)
      bodyResult    <- build(bodyStack.map(_.expressions.head))
      funcType       = functionType(paramResult.exprValue, bodyResult.exprValue)
      typedBodyStack =
        bodyStack.as(ExpressionStack[TypedExpression](Seq(bodyResult.typed), bodyStack.value.hasRuntime))
    } yield TypeWithTyped(
      funcType,
      TypedExpression(funcType, TypedExpression.FunctionLiteral(paramName, paramResult.typed, typedBodyStack))
    )

  private def primitiveType(moduleName: String, typeName: String): ExpressionValue =
    ConcreteValue(Types.dataType(ValueFQN(ModuleName(Seq("eliot", "lang"), moduleName), typeName)))
}
