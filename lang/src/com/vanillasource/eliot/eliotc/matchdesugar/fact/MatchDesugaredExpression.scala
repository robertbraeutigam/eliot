package com.vanillasource.eliot.eliotc.matchdesugar.fact

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.resolve.fact.Expression
import com.vanillasource.eliot.eliotc.source.content.Sourced

sealed trait MatchDesugaredExpression

object MatchDesugaredExpression {
  case class FunctionApplication(
      target: Sourced[TypeStack[MatchDesugaredExpression]],
      argument: Sourced[TypeStack[MatchDesugaredExpression]]
  ) extends MatchDesugaredExpression
  case class IntegerLiteral(integerLiteral: Sourced[BigInt])    extends MatchDesugaredExpression
  case class StringLiteral(stringLiteral: Sourced[String])      extends MatchDesugaredExpression
  case class ParameterReference(parameterName: Sourced[String]) extends MatchDesugaredExpression
  case class ValueReference(
      valueName: Sourced[ValueFQN],
      typeArgs: Seq[Sourced[MatchDesugaredExpression]] = Seq.empty
  ) extends MatchDesugaredExpression
  case class FunctionLiteral(
      parameterName: Sourced[String],
      parameterType: Option[Sourced[TypeStack[MatchDesugaredExpression]]],
      body: Sourced[TypeStack[MatchDesugaredExpression]]
  ) extends MatchDesugaredExpression
  case class FlatExpression(parts: Seq[Sourced[TypeStack[MatchDesugaredExpression]]])
      extends MatchDesugaredExpression

  def fromExpression(expr: Expression): MatchDesugaredExpression = expr match {
    case Expression.FunctionApplication(target, arg)            =>
      FunctionApplication(convertTypeStack(target), convertTypeStack(arg))
    case Expression.IntegerLiteral(v)                           => IntegerLiteral(v)
    case Expression.StringLiteral(v)                            => StringLiteral(v)
    case Expression.ParameterReference(v)                       => ParameterReference(v)
    case Expression.ValueReference(name, typeArgs)              =>
      ValueReference(name, typeArgs.map(ta => ta.map(fromExpression)))
    case Expression.FunctionLiteral(paramName, paramType, body) =>
      FunctionLiteral(paramName, paramType.map(convertTypeStack), convertTypeStack(body))
    case Expression.FlatExpression(parts)                       =>
      FlatExpression(parts.map(convertTypeStack))
    case Expression.MatchExpression(_, _)                       =>
      throw IllegalStateException("MatchExpression should not exist after match desugaring")
  }

  private def convertTypeStack(
      stack: Sourced[TypeStack[Expression]]
  ): Sourced[TypeStack[MatchDesugaredExpression]] =
    stack.map(ts => TypeStack(ts.levels.map(fromExpression)))

  given Show[MatchDesugaredExpression] = {
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
    case FlatExpression(parts) => parts.map(_.value.show).mkString(" ")
  }
}
