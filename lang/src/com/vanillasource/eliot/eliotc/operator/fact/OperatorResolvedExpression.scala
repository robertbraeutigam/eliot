package com.vanillasource.eliot.eliotc.operator.fact

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.matchdesugar.fact.MatchDesugaredExpression
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
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

  def fromExpression(expr: MatchDesugaredExpression): OperatorResolvedExpression = expr match {
    case MatchDesugaredExpression.FunctionApplication(target, arg)            =>
      FunctionApplication(convertTypeStack(target), convertTypeStack(arg))
    case MatchDesugaredExpression.IntegerLiteral(v)                           => IntegerLiteral(v)
    case MatchDesugaredExpression.StringLiteral(v)                            => StringLiteral(v)
    case MatchDesugaredExpression.ParameterReference(v)                       => ParameterReference(v)
    case MatchDesugaredExpression.ValueReference(name, typeArgs)              =>
      ValueReference(name, typeArgs.map(ta => ta.map(fromExpression)))
    case MatchDesugaredExpression.FunctionLiteral(paramName, paramType, body) =>
      FunctionLiteral(paramName, paramType.map(convertTypeStack), convertTypeStack(body))
    case MatchDesugaredExpression.FlatExpression(_)                           =>
      throw IllegalStateException("FlatExpression should not exist after operator resolution")
  }

  private def convertTypeStack(
      stack: Sourced[TypeStack[MatchDesugaredExpression]]
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
