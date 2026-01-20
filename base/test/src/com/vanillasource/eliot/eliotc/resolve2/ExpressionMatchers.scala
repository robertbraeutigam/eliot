package com.vanillasource.eliot.eliotc.resolve2

import com.vanillasource.eliot.eliotc.core.fact.ExpressionStack
import com.vanillasource.eliot.eliotc.module2.fact.ValueFQN
import com.vanillasource.eliot.eliotc.resolve2.fact.Expression
import com.vanillasource.eliot.eliotc.resolve2.fact.Expression.*
import com.vanillasource.eliot.eliotc.source.content.Sourced

object ExpressionMatchers {

  object FunLit {
    def unapply(expr: Expression): Option[(String, Expression)] =
      expr match {
        case FunctionLiteral(Sourced(_, _, paramName), _, Sourced(_, _, ExpressionStack(Seq(body), _))) =>
          Some((paramName, body))
        case _                                                                                          => None
      }
  }

  object ParamRef {
    def unapply(expr: Expression): Option[String] =
      expr match {
        case ParameterReference(Sourced(_, _, name)) => Some(name)
        case _                                       => None
      }
  }

  object ValRef {
    def unapply(expr: Expression): Option[ValueFQN] =
      expr match {
        case ValueReference(Sourced(_, _, vfqn)) => Some(vfqn)
        case _                                   => None
      }
  }

  object IntLit {
    def unapply(expr: Expression): Option[BigInt] =
      expr match {
        case IntegerLiteral(Sourced(_, _, value)) => Some(value)
        case _                                    => None
      }
  }

  object StrLit {
    def unapply(expr: Expression): Option[String] =
      expr match {
        case StringLiteral(Sourced(_, _, value)) => Some(value)
        case _                                   => None
      }
  }

  object FunApp {
    def unapply(expr: Expression): Option[(Expression, Expression)] =
      expr match {
        case FunctionApplication(
              Sourced(_, _, ExpressionStack(Seq(target), _)),
              Sourced(_, _, ExpressionStack(Seq(arg), _))
            ) =>
          Some((target, arg))
        case _ => None
      }
  }
}
