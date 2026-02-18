package com.vanillasource.eliot.eliotc.resolve

import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.resolve.fact.Expression
import com.vanillasource.eliot.eliotc.resolve.fact.Expression.*
import com.vanillasource.eliot.eliotc.source.content.Sourced

object ExpressionMatchers {

  object FunLit {
    def unapply(expr: Expression): Option[(String, Expression)] =
      expr match {
        case FunctionLiteral(Sourced(_, _, paramName), _, Sourced(_, _, body)) =>
          Some((paramName, body.signature))
        case _                                                                 => None
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
              Sourced(_, _, target),
              Sourced(_, _, arg)
            ) =>
          Some((target.signature, arg.signature))
        case _ => None
      }
  }
}
