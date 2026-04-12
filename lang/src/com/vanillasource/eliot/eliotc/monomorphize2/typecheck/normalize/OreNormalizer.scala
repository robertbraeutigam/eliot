package com.vanillasource.eliot.eliotc.monomorphize2.typecheck.normalize

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.*
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Reduces an ORE expression to weak head normal form (WHNF) by repeatedly reducing top-level beta-redexes.
  *
  * A beta-redex is a [[FunctionApplication]] whose target is (or itself reduces to) a [[FunctionLiteral]]. For example,
  * `((A :: Type) -> A)(Int)` reduces to `Int`.
  *
  * Nested beta-redexes in the target position are reduced recursively, so `((A :: Type) -> (B :: Type) ->
  * F(A)(B))(Int)(String)` correctly reduces to `F(Int)(String)`.
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

  /** Replace all inner [[Sourced]] wrappers in an ORE tree with the given source position. Used to re-source
    * definition-site positions to a call-site position after polytype instantiation, so that error positions point to
    * the call site rather than the definition.
    */
  def reSource(expr: OperatorResolvedExpression, position: Sourced[?]): OperatorResolvedExpression =
    expr match {
      case FunctionApplication(target, arg)     =>
        FunctionApplication(
          position.as(reSource(target.value, position)),
          position.as(reSource(arg.value, position))
        )
      case ValueReference(name, typeArgs)       =>
        ValueReference(position.as(name.value), typeArgs.map(ta => position.as(reSource(ta.value, position))))
      case ParameterReference(name)             =>
        ParameterReference(position.as(name.value))
      case FunctionLiteral(pn, paramType, body) =>
        FunctionLiteral(
          position.as(pn.value),
          paramType.map(pt => position.as(TypeStack(pt.value.levels.map(reSource(_, position))))),
          position.as(reSource(body.value, position))
        )
      case _: IntegerLiteral | _: StringLiteral => expr
    }
}
