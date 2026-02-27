package com.vanillasource.eliot.eliotc.matchdesugar.processor

import com.vanillasource.eliot.eliotc.matchdesugar.fact.MatchDesugaredExpression
import com.vanillasource.eliot.eliotc.matchdesugar.fact.MatchDesugaredExpression.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced

object MatchDesugaredExpressionMatchers {

  object FunLit {
    def unapply(expr: MatchDesugaredExpression): Option[(String, MatchDesugaredExpression)] =
      expr match {
        case FunctionLiteral(Sourced(_, _, paramName), _, Sourced(_, _, body)) =>
          Some((paramName, body.signature))
        case _                                                                 => None
      }
  }

  object ParamRef {
    def unapply(expr: MatchDesugaredExpression): Option[String] =
      expr match {
        case ParameterReference(Sourced(_, _, name)) => Some(name)
        case _                                       => None
      }
  }

  object ValRef {
    def unapply(expr: MatchDesugaredExpression): Option[ValueFQN] =
      expr match {
        case ValueReference(Sourced(_, _, vfqn), _) => Some(vfqn)
        case _                                      => None
      }
  }

  object IntLit {
    def unapply(expr: MatchDesugaredExpression): Option[BigInt] =
      expr match {
        case IntegerLiteral(Sourced(_, _, value)) => Some(value)
        case _                                    => None
      }
  }

  object StrLit {
    def unapply(expr: MatchDesugaredExpression): Option[String] =
      expr match {
        case StringLiteral(Sourced(_, _, value)) => Some(value)
        case _                                   => None
      }
  }

  object FunApp {
    def unapply(expr: MatchDesugaredExpression): Option[(MatchDesugaredExpression, MatchDesugaredExpression)] =
      expr match {
        case FunctionApplication(Sourced(_, _, target), Sourced(_, _, arg)) =>
          Some((target.signature, arg.signature))
        case _                                                              => None
      }
  }
}
