package com.vanillasource.eliot.eliotc.resolve.fact

import cats.Applicative
import cats.syntax.all.*
import cats.Show
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced

sealed trait Expression

object Expression {
  case class FunctionApplication(
      target: Sourced[TypeStack[Expression]],
      argument: Sourced[TypeStack[Expression]]
  ) extends Expression
  case class IntegerLiteral(integerLiteral: Sourced[BigInt])    extends Expression
  case class StringLiteral(stringLiteral: Sourced[String])      extends Expression
  case class ParameterReference(parameterName: Sourced[String]) extends Expression
  case class ValueReference(valueName: Sourced[ValueFQN], typeArgs: Seq[Sourced[Expression]] = Seq.empty)
      extends Expression
  case class FunctionLiteral(
      parameterName: Sourced[String],
      parameterType: Option[Sourced[TypeStack[Expression]]],
      body: Sourced[TypeStack[Expression]]
  ) extends Expression
  case class FlatExpression(parts: Seq[Sourced[TypeStack[Expression]]]) extends Expression
  case class MatchExpression(
      scrutinee: Sourced[TypeStack[Expression]],
      cases: Seq[MatchCase]
  ) extends Expression

  case class MatchCase(
      pattern: Sourced[Pattern],
      body: Sourced[TypeStack[Expression]]
  )

  def mapChildrenM[F[_]: Applicative](f: Expression => F[Expression])(expr: Expression): F[Expression] = {
    def traverseStack(stack: Sourced[TypeStack[Expression]]): F[Sourced[TypeStack[Expression]]] =
      stack.value.levels.traverse(f).map(levels => stack.as(TypeStack(levels)))

    expr match {
      case FunctionApplication(target, arg)                =>
        (traverseStack(target), traverseStack(arg)).mapN(FunctionApplication.apply)
      case FunctionLiteral(paramName, paramType, body)     =>
        (paramType.traverse(traverseStack), traverseStack(body)).mapN(FunctionLiteral(paramName, _, _))
      case FlatExpression(parts)                           =>
        parts.traverse(traverseStack).map(FlatExpression.apply)
      case MatchExpression(scrutinee, cases)               =>
        (
          traverseStack(scrutinee),
          cases.traverse(mc => traverseStack(mc.body).map(MatchCase(mc.pattern, _)))
        ).mapN(MatchExpression.apply)
      case ValueReference(name, typeArgs)                  =>
        typeArgs.traverse(ta => f(ta.value).map(ta.as)).map(ValueReference(name, _))
      case _: IntegerLiteral | _: StringLiteral | _: ParameterReference => expr.pure[F]
    }
  }

  given Show[Expression] = {
    case IntegerLiteral(Sourced(_, _, value))                                          => value.toString()
    case StringLiteral(Sourced(_, _, value))                                           => s"\"$value\""
    case FunctionApplication(Sourced(_, _, targetValue), Sourced(_, _, argumentValue)) =>
      s"${targetValue.show}(${argumentValue.show})"
    case FunctionLiteral(param, paramType, body)                                       =>
      s"(${paramType.map(_.value.show).getOrElse("<n/a>")} :: ${param.value}) -> ${body.value.show}"
    case ParameterReference(name)                => name.value
    case ValueReference(name, typeArgs)          =>
      name.value.show +
        (if (typeArgs.isEmpty) "" else typeArgs.map(ta => ta.value.show).mkString("[", ", ", "]"))
    case FlatExpression(parts)                   => parts.map(_.value.show).mkString(" ")
    case MatchExpression(scrutinee, cases)        =>
      s"${scrutinee.value.show} match { ${cases.map(c => s"case ${c.pattern.value.show} -> ${c.body.value.show}").mkString(" ")} }"
  }
}
