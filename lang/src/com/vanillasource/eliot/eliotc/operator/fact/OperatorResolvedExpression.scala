package com.vanillasource.eliot.eliotc.operator.fact

import cats.{Applicative, Monad, Show}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.matchdesugar.fact.MatchDesugaredExpression
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced

sealed trait OperatorResolvedExpression

object OperatorResolvedExpression {
  case class FunctionApplication(
      target: Sourced[OperatorResolvedExpression],
      argument: Sourced[OperatorResolvedExpression]
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
      body: Sourced[OperatorResolvedExpression]
  ) extends OperatorResolvedExpression

  /** Replace every free occurrence of the parameter named `paramName` with `replacement`. Respects shadowing by
    * `FunctionLiteral` parameters of the same name.
    */
  def substitute(
      expr: OperatorResolvedExpression,
      paramName: String,
      replacement: OperatorResolvedExpression
  ): OperatorResolvedExpression = expr match {
    case ParameterReference(name) if name.value == paramName =>
      replacement
    case _: ParameterReference                               =>
      expr
    case FunctionApplication(target, argument)               =>
      FunctionApplication(
        target.map(substitute(_, paramName, replacement)),
        argument.map(substitute(_, paramName, replacement))
      )
    case ValueReference(name, typeArgs)                      =>
      ValueReference(name, typeArgs.map(_.map(substitute(_, paramName, replacement))))
    case FunctionLiteral(pn, paramType, body)                =>
      val newParamType = paramType.map(_.map(stack => TypeStack(stack.levels.map(substitute(_, paramName, replacement)))))
      val newBody      =
        if (pn.value == paramName) body
        else body.map(substitute(_, paramName, replacement))
      FunctionLiteral(pn, newParamType, newBody)
    case _: IntegerLiteral | _: StringLiteral                =>
      expr
  }

  /** Returns true if `expr` contains a free `ParameterReference` named `varName`. Used for occurs check during
    * unification.
    */
  def containsVar(expr: OperatorResolvedExpression, varName: String): Boolean = expr match {
    case ParameterReference(name)              => name.value == varName
    case FunctionApplication(target, argument) =>
      containsVar(target.value, varName) || containsVar(argument.value, varName)
    case ValueReference(_, typeArgs)           =>
      typeArgs.exists(ta => containsVar(ta.value, varName))
    case FunctionLiteral(pn, paramType, body)  =>
      val inParamType = paramType.exists(_.value.levels.exists(containsVar(_, varName)))
      val inBody      = pn.value != varName && containsVar(body.value, varName)
      inParamType || inBody
    case _: IntegerLiteral | _: StringLiteral  => false
  }

  /** Monadically fold over every [[ValueReference]] in the expression, including references nested in
    * type-argument lists and in `FunctionLiteral` parameter type annotations.
    *
    * The callback receives the running state plus each [[Sourced]] reference as it is visited; the walk itself
    * does not deduplicate — callers that want to avoid visiting the same vfqn twice should short-circuit inside
    * the callback by inspecting the state they thread.
    */
  def foldValueReferences[F[_]: Monad, S](
      expr: OperatorResolvedExpression,
      initial: S
  )(
      onVfqn: (S, Sourced[ValueFQN]) => F[S]
  ): F[S] = {
    def go(e: OperatorResolvedExpression, s: S): F[S] = e match {
      case ValueReference(name, typeArgs)      =>
        onVfqn(s, name).flatMap(s1 => typeArgs.foldLeft(s1.pure[F])((a, ta) => a.flatMap(go(ta.value, _))))
      case FunctionApplication(t, a)           =>
        go(t.value, s).flatMap(go(a.value, _))
      case FunctionLiteral(_, paramType, body) =>
        val withParam: F[S] = paramType match {
          case Some(pt) => pt.value.levels.toSeq.foldLeft(s.pure[F])((acc, l) => acc.flatMap(go(l, _)))
          case None     => s.pure[F]
        }
        withParam.flatMap(go(body.value, _))
      case _: IntegerLiteral | _: StringLiteral | _: ParameterReference =>
        s.pure[F]
    }
    go(expr, initial)
  }

  def mapChildrenM[F[_]: Applicative](f: OperatorResolvedExpression => F[OperatorResolvedExpression])(
      expr: OperatorResolvedExpression
  ): F[OperatorResolvedExpression] = {
    def traverseStack(
        stack: Sourced[TypeStack[OperatorResolvedExpression]]
    ): F[Sourced[TypeStack[OperatorResolvedExpression]]] =
      stack.value.levels.traverse(f).map(levels => stack.as(TypeStack(levels)))

    expr match {
      case FunctionApplication(target, arg)                             =>
        (f(target.value).map(target.as), f(arg.value).map(arg.as)).mapN(FunctionApplication.apply)
      case FunctionLiteral(paramName, paramType, body)                  =>
        (paramType.traverse(traverseStack), f(body.value).map(body.as)).mapN(FunctionLiteral(paramName, _, _))
      case ValueReference(name, typeArgs)                               =>
        typeArgs.traverse(ta => f(ta.value).map(ta.as)).map(ValueReference(name, _))
      case _: IntegerLiteral | _: StringLiteral | _: ParameterReference => expr.pure[F]
    }
  }

  def fromExpression(expr: MatchDesugaredExpression): OperatorResolvedExpression = expr match {
    case MatchDesugaredExpression.FunctionApplication(target, arg)            =>
      FunctionApplication(target.map(fromExpression), arg.map(fromExpression))
    case MatchDesugaredExpression.IntegerLiteral(v)                           => IntegerLiteral(v)
    case MatchDesugaredExpression.StringLiteral(v)                            => StringLiteral(v)
    case MatchDesugaredExpression.ParameterReference(v)                       => ParameterReference(v)
    case MatchDesugaredExpression.ValueReference(name, typeArgs)              =>
      ValueReference(name, typeArgs.map(ta => ta.map(fromExpression)))
    case MatchDesugaredExpression.FunctionLiteral(paramName, paramType, body) =>
      FunctionLiteral(paramName, paramType.map(convertTypeStack), body.map(ts => fromExpression(ts.signature)))
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
    case FunctionApplication(Sourced(_, _, target), Sourced(_, _, argument)) =>
      s"${target.show}(${argument.show})"
    case FunctionLiteral(param, paramType, body)                                       =>
      s"(${paramType.map(_.value.show).getOrElse("<n/a>")} :: ${param.value}) -> ${body.value.show}"
    case ParameterReference(name)                                                      => name.value
    case ValueReference(name, typeArgs)                                                =>
      name.value.show +
        (if (typeArgs.isEmpty) "" else typeArgs.map(ta => ta.value.show).mkString("[", ", ", "]"))
  }
}
