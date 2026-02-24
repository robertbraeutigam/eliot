package com.vanillasource.eliot.eliotc.symbolic.fact

import cats.{Monad, Show}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.symbolic.fact.TypedExpression.Expression

/** An expression annotated with its type (as an ExpressionValue). */
case class TypedExpression(
    expressionType: ExpressionValue,
    expression: Expression
) {

  /** Apply a transformation function to all ExpressionValue fields recursively. */
  def transformTypes(f: ExpressionValue => ExpressionValue): TypedExpression =
    TypedExpression(
      f(expressionType),
      expression match {
        case TypedExpression.FunctionApplication(target, arg)            =>
          TypedExpression.FunctionApplication(
            target.map(_.transformTypes(f)),
            arg.map(_.transformTypes(f))
          )
        case TypedExpression.FunctionLiteral(paramName, paramType, body) =>
          TypedExpression.FunctionLiteral(
            paramName,
            paramType.map(f),
            body.map(_.transformTypes(f))
          )
        case other                                                       => other
      }
    )
}

object TypedExpression {

  /** Dispatch over the six expression cases. Each callback receives the raw children (not pre-folded), allowing callers
    * to control recursion themselves. Use cats.Id as F for pure callers.
    */
  def foldExpression[F[_]: Monad, A](
      onIntLit: Sourced[BigInt] => F[A],
      onStrLit: Sourced[String] => F[A],
      onParamRef: Sourced[String] => F[A],
      onValRef: Sourced[ValueFQN] => F[A],
      onFunApp: (Sourced[TypedExpression], Sourced[TypedExpression]) => F[A],
      onFunLit: (Sourced[String], Sourced[ExpressionValue], Sourced[TypedExpression]) => F[A]
  )(self: Expression): F[A] =
    self match {
      case IntegerLiteral(v)                       => onIntLit(v)
      case StringLiteral(v)                        => onStrLit(v)
      case ParameterReference(name)                => onParamRef(name)
      case ValueReference(vfqn)                    => onValRef(vfqn)
      case FunctionApplication(target, arg)        => onFunApp(target, arg)
      case FunctionLiteral(paramName, paramType, body) => onFunLit(paramName, paramType, body)
    }

  sealed trait Expression

  case class FunctionApplication(target: Sourced[TypedExpression], argument: Sourced[TypedExpression])
      extends Expression

  case class IntegerLiteral(integerLiteral: Sourced[BigInt]) extends Expression

  case class StringLiteral(stringLiteral: Sourced[String]) extends Expression

  case class ParameterReference(parameterName: Sourced[String]) extends Expression

  case class ValueReference(valueName: Sourced[ValueFQN]) extends Expression

  case class FunctionLiteral(
      parameterName: Sourced[String],
      parameterType: Sourced[ExpressionValue],
      body: Sourced[TypedExpression]
  ) extends Expression

  given Show[TypedExpression] with {
    override def show(expression: TypedExpression): String =
      s"L0: ${showTyped(expression)}, L1: ${showLevel1(expression.expression)}"
  }

  given Show[Expression] with {
    def show(expression: Expression): String =
      s"L0: ${showLevel0(expression)}, L1: ${showLevel1(expression).mkString(", ")}"
  }

  private def showLevel1(expression: Expression): Seq[String] =
    expression match {
      case FunctionLiteral(name, parameterType, body) =>
        Seq(s"${name.value}: ${parameterType.value.show}", showTyped(body.value)) ++ showLevel1(body.value.expression)
      case FunctionApplication(target, argument)      =>
        Seq(showTyped(target.value), showTyped(argument.value)) ++
          showLevel1(target.value.expression) ++ showLevel1(argument.value.expression)
      case _                                          => Seq.empty
    }

  private def showTyped(typedExpression: TypedExpression): String =
    s"${showLevel0(typedExpression.expression)}: ${typedExpression.expressionType.show}"

  private def showLevel0(expression: Expression): String =
    expression match {
      case IntegerLiteral(integerLiteral)        => integerLiteral.value.toString()
      case StringLiteral(stringLiteral)          => stringLiteral.value
      case FunctionLiteral(name, _, body)        => s"${name.value} -> ${showLevel0(body.value.expression)}"
      case ParameterReference(parameterName)     => parameterName.value
      case FunctionApplication(target, argument) =>
        s"${showLevel0(target.value.expression)}(${showLevel0(argument.value.expression)})"
      case ValueReference(valueFQN)              => valueFQN.value.show
    }
}
