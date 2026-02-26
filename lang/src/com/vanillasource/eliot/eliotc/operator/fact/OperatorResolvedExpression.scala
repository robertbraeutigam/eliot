package com.vanillasource.eliot.eliotc.operator.fact

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.resolve.fact.Expression
import com.vanillasource.eliot.eliotc.source.content.Sourced

sealed trait OperatorResolvedExpression

object OperatorResolvedExpression {
  case class FunctionApplication(
      target: Sourced[TypeStack[OperatorResolvedExpression]],
      argument: Sourced[TypeStack[OperatorResolvedExpression]]
  ) extends OperatorResolvedExpression
  case class IntegerLiteral(integerLiteral: Sourced[BigInt])    extends OperatorResolvedExpression
  case class StringLiteral(stringLiteral: Sourced[String])      extends OperatorResolvedExpression
  case class ParameterReference(parameterName: Sourced[String]) extends OperatorResolvedExpression
  case class ValueReference(
      valueName: Sourced[ValueFQN],
      typeArgs: Seq[Sourced[OperatorResolvedExpression]] = Seq.empty
  ) extends OperatorResolvedExpression
  case class FunctionLiteral(
      parameterName: Sourced[String],
      parameterType: Option[Sourced[TypeStack[OperatorResolvedExpression]]],
      body: Sourced[TypeStack[OperatorResolvedExpression]]
  ) extends OperatorResolvedExpression

  def fromExpression(expr: Expression): OperatorResolvedExpression = expr match {
    case Expression.FunctionApplication(target, arg)            =>
      FunctionApplication(convertTypeStack(target), convertTypeStack(arg))
    case Expression.IntegerLiteral(v)                           => IntegerLiteral(v)
    case Expression.StringLiteral(v)                            => StringLiteral(v)
    case Expression.ParameterReference(v)                       => ParameterReference(v)
    case Expression.ValueReference(name, typeArgs)              =>
      ValueReference(name, typeArgs.map(ta => ta.map(fromExpression)))
    case Expression.FunctionLiteral(paramName, paramType, body) =>
      FunctionLiteral(paramName, paramType.map(convertTypeStack), convertTypeStack(body))
    case Expression.FlatExpression(_)                           =>
      throw IllegalStateException("FlatExpression should not exist after operator resolution")
    case Expression.MatchExpression(_, _)                      =>
      throw IllegalStateException("MatchExpression should not exist after match desugaring")
  }

  private def convertTypeStack(
      stack: Sourced[TypeStack[Expression]]
  ): Sourced[TypeStack[OperatorResolvedExpression]] =
    stack.map(ts => TypeStack(ts.levels.map(fromExpression)))

  given Show[OperatorResolvedExpression] = {
    case IntegerLiteral(Sourced(_, _, value))                                          => value.toString()
    case StringLiteral(Sourced(_, _, value))                                           => s"\"$value\""
    case FunctionApplication(Sourced(_, _, targetValue), Sourced(_, _, argumentValue)) =>
      s"${targetValue.show}(${argumentValue.show})"
    case FunctionLiteral(param, paramType, body)                                       =>
      s"(${paramType.map(_.value.show).getOrElse("<n/a>")} :: ${param.value}) -> ${body.value.show}"
    case ParameterReference(name)                                                      => name.value
    case ValueReference(name, typeArgs)                                                =>
      name.value.show +
        (if (typeArgs.isEmpty) "" else typeArgs.map(ta => ta.value.show).mkString("[", ", ", "]"))
  }
}
