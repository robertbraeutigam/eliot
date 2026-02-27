package com.vanillasource.eliot.eliotc.monomorphize.fact

import cats.Applicative
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** A runtime expression annotated with concrete (ground) types. All types are fully evaluated Values with no free type
  * variables.
  */
case class MonomorphicExpression(
    expressionType: Value,
    expression: MonomorphicExpression.Expression
)

object MonomorphicExpression {
  def mapChildrenM[F[_]: Applicative](
      f: Sourced[MonomorphicExpression] => F[Sourced[MonomorphicExpression]]
  )(expr: Expression): F[Expression] =
    expr match {
      case FunctionApplication(target, arg)        => (f(target), f(arg)).mapN(FunctionApplication.apply)
      case FunctionLiteral(paramName, paramType, body) => f(body).map(FunctionLiteral(paramName, paramType, _))
      case _: IntegerLiteral | _: StringLiteral | _: ParameterReference | _: MonomorphicValueReference => expr.pure[F]
    }

  sealed trait Expression

  case class FunctionApplication(
      target: Sourced[MonomorphicExpression],
      argument: Sourced[MonomorphicExpression]
  ) extends Expression

  case class FunctionLiteral(
      parameterName: Sourced[String],
      parameterType: Value,
      body: Sourced[MonomorphicExpression]
  ) extends Expression

  case class IntegerLiteral(value: Sourced[BigInt]) extends Expression

  case class StringLiteral(value: Sourced[String]) extends Expression

  case class ParameterReference(parameterName: Sourced[String]) extends Expression

  /** Reference to a specialized (monomorphic) function instance.
    */
  case class MonomorphicValueReference(
      valueName: Sourced[ValueFQN],
      typeArguments: Seq[Value]
  ) extends Expression
}
