package com.vanillasource.eliot.eliotc.operator

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.OperatorResolvedExpression.*
import com.vanillasource.eliot.eliotc.source.content.Sourced

object OperatorResolvedExpressionMatchers {

  object FunLit {
    def unapply(expr: OperatorResolvedExpression): Option[(String, OperatorResolvedExpression)] =
      expr match {
        case FunctionLiteral(Sourced(_, _, paramName), _, Sourced(_, _, body)) =>
          Some((paramName, body.signature))
        case _                                                                 => None
      }
  }

  object ParamRef {
    def unapply(expr: OperatorResolvedExpression): Option[String] =
      expr match {
        case ParameterReference(Sourced(_, _, name)) => Some(name)
        case _                                       => None
      }
  }

  object ValRef {
    def unapply(expr: OperatorResolvedExpression): Option[ValueFQN] =
      expr match {
        case ValueReference(Sourced(_, _, vfqn), _) => Some(vfqn)
        case _                                      => None
      }
  }

  object IntLit {
    def unapply(expr: OperatorResolvedExpression): Option[BigInt] =
      expr match {
        case IntegerLiteral(Sourced(_, _, value)) => Some(value)
        case _                                    => None
      }
  }

  object StrLit {
    def unapply(expr: OperatorResolvedExpression): Option[String] =
      expr match {
        case StringLiteral(Sourced(_, _, value)) => Some(value)
        case _                                   => None
      }
  }

  object FunApp {
    def unapply(expr: OperatorResolvedExpression): Option[(OperatorResolvedExpression, OperatorResolvedExpression)] =
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
