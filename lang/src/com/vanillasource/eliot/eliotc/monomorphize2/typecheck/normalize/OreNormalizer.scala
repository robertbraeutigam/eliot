package com.vanillasource.eliot.eliotc.monomorphize2.typecheck.normalize

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.*
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Reduces an ORE expression to weak head normal form (WHNF) by repeatedly reducing top-level beta-redexes.
  *
  * A beta-redex is a [[FunctionApplication]] whose target is (or itself reduces to) a [[FunctionLiteral]]. For
  * example, `((A :: Type) -> A)(Int)` reduces to `Int`.
  *
  * Nested beta-redexes in the target position are reduced recursively, so
  * `((A :: Type) -> (B :: Type) -> F(A)(B))(Int)(String)` correctly reduces to `F(Int)(String)`.
  *
  * Only the head position is normalized — arguments and sub-expressions in non-head positions are left as-is, to be
  * normalized later when they appear as heads of their own constraints.
  */
object OreNormalizer {
  def normalize(expr: OperatorResolvedExpression): OperatorResolvedExpression =
    expr match {
      case FunctionApplication(target, arg) =>
        normalize(target.value) match {
          case FunctionLiteral(paramName, _, body) =>
            normalize(OperatorResolvedExpression.substitute(body.value, paramName.value, arg.value))
          case normalizedTarget                    =>
            FunctionApplication(target.as(normalizedTarget), arg)
        }
      case _                                => expr
    }
}
