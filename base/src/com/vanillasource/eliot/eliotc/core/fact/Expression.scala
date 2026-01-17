package com.vanillasource.eliot.eliotc.core.fact

import cats.syntax.all.*
import cats.{Eq, Show}
import com.vanillasource.eliot.eliotc.core.fact
import com.vanillasource.eliot.eliotc.core.fact.ExpressionStack
import com.vanillasource.eliot.eliotc.source.content.Sourced

trait Expression

object Expression {

  // Reference to a named value
  case class NamedValueReference(valueName: Sourced[String], qualifier: Option[Sourced[String]] = None)
      extends Expression
  // Apply an argument to an expression (assumed to be a function)
  case class FunctionApplication(
      target: Sourced[ExpressionStack[Expression]],
      argument: Sourced[ExpressionStack[Expression]]
  ) extends Expression
  // Function literal, i.e. a lambda expression, i.e. an ad-hoc function, i.e. an unnamed function
  case class FunctionLiteral(
      parameterName: Sourced[String],
      parameterType: ExpressionStack[Expression],
      body: Sourced[ExpressionStack[Expression]]
  ) extends Expression
  // Integer literal
  case class IntegerLiteral(integerLiteral: Sourced[String]) extends Expression
  // String literal
  case class StringLiteral(stringLiteral: Sourced[String])   extends Expression

  given expressionEquality: Eq[Expression] = (x: Expression, y: Expression) =>
    (x, y) match {
      case (NamedValueReference(n1, q1), NamedValueReference(n2, q2))   =>
        n1.value == n2.value && q1.map(_.value) == q2.map(_.value)
      case (FunctionApplication(t1, a1), FunctionApplication(t2, a2))   =>
        ExpressionStack.expressionStackEquality
          .eqv(t1.value, t2.value) && ExpressionStack.expressionStackEquality.eqv(a1.value, a2.value)
      case (FunctionLiteral(p1, pt1, b1), FunctionLiteral(p2, pt2, b2)) =>
        p1.value == p2.value && ExpressionStack.expressionStackEquality.eqv(
          pt1,
          pt2
        ) && ExpressionStack.expressionStackEquality.eqv(
          b1.value,
          b2.value
        )
      case (IntegerLiteral(i1), IntegerLiteral(i2))                     => i1.value == i2.value
      case (StringLiteral(s1), StringLiteral(s2))                       => s1.value == s2.value
      case _                                                            => false
    }

  given Show[Expression] = {
    case IntegerLiteral(Sourced(_, _, value))                                          => value
    case StringLiteral(Sourced(_, _, value))                                           => value
    case FunctionApplication(Sourced(_, _, targetValue), Sourced(_, _, argumentValue)) =>
      s"${targetValue.show}(${argumentValue.show})"
    case FunctionLiteral(param, _, body)                                               => param.value + " -> " + body.value.show
    case NamedValueReference(valueName, qualifier)                                     => valueName.value
  }

  given TreeDisplay[Expression] with {
    def render(expr: Expression): fact.TreeDisplay.Node[Expression] = expr match {
      case NamedValueReference(valueName, qualifier) =>
        val qualStr = qualifier.map(q => s"${q.value}::").getOrElse("")
        TreeDisplay.Node(s"NamedValueReference($qualStr${valueName.value})", Seq.empty)
      case IntegerLiteral(sourced)                   =>
        TreeDisplay.Node(s"IntegerLiteral(${sourced.value})", Seq.empty)
      case StringLiteral(sourced)                    =>
        TreeDisplay.Node(s"StringLiteral(\"${sourced.value}\")", Seq.empty)
      case FunctionApplication(target, argument)     =>
        TreeDisplay.Node("FunctionApplication", Seq("target" -> target.value, "argument" -> argument.value))
      case FunctionLiteral(param, paramType, body)   =>
        TreeDisplay.Node(s"FunctionLiteral(${param.value})", Seq("parameterType" -> paramType, "body" -> body.value))
    }
  }
}
