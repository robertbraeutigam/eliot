package com.vanillasource.eliot.eliotc.reconcile.fact

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.uncurry.fact.MonomorphicParameterDefinition

/** An uncurried monomorphic expression enriched by the refinement-reconcile pass
  * ([[com.vanillasource.eliot.eliotc.reconcile.processor.ReconcileProcessor]], `docs/bounds-as-refinements.md`).
  *
  * It mirrors [[com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedMonomorphicExpression]] node-for-node and adds two
  * things the representation-lowering pass used to bake into the *type*:
  *
  *   - every node carries its refinement `meta` — the value channel's [[GroundValue]] for this node (an `Interval`
  *     value) when the channel pinned one, or [[None]] for a ⊤/unknown node (which the backend lays out as a bignum).
  *     The backend derives the node's machine width from this meta, not from a lowered `Jvm*` representation type.
  *   - [[Reconcile]] nodes are inserted at meta-change edges (branch-arm merges and argument/return boundaries) so the
  *     backend emits a representation re-encode with no width policy of its own — pure lowering that never changes
  *     meaning, only machine representation.
  *
  * @param meta
  *   the value channel's meta for this node (an `Interval` [[GroundValue]]), or [[None]] for a ⊤/unknown node.
  * @param expressionType
  *   the node's concrete monomorphic type (unchanged from uncurrying — representation is no longer folded into it).
  */
case class ReconciledMonomorphicExpression(
    meta: Option[GroundValue],
    expressionType: GroundValue,
    expression: ReconciledMonomorphicExpression.Expression
)

object ReconciledMonomorphicExpression {
  sealed trait Expression

  /** Multi-argument function application (mirrors the uncurried form). */
  case class FunctionApplication(
      target: Sourced[ReconciledMonomorphicExpression],
      arguments: Seq[Sourced[ReconciledMonomorphicExpression]]
  ) extends Expression

  /** Multi-parameter function literal (mirrors the uncurried form). */
  case class FunctionLiteral(
      parameters: Seq[MonomorphicParameterDefinition],
      body: Sourced[ReconciledMonomorphicExpression]
  ) extends Expression

  case class IntegerLiteral(value: Sourced[BigInt]) extends Expression

  case class StringLiteral(value: Sourced[String]) extends Expression

  case class ParameterReference(parameterName: Sourced[String]) extends Expression

  case class MonomorphicValueReference(
      valueName: Sourced[ValueFQN],
      typeArguments: Seq[GroundValue]
  ) extends Expression

  /** A representation reconcile point: emit `source`, then re-encode its value from `source`'s own `meta` to the
    * enclosing [[ReconciledMonomorphicExpression]]'s `meta` (the target the consuming context imposes). Inserted by the
    * reconcile pass wherever a value crosses an edge that fixes a different expected meta than the value's own — a
    * branch-arm merge or an argument/return boundary. It never changes meaning, only machine representation, and is a
    * no-op on the backend when both metas resolve to the same width.
    */
  case class Reconcile(source: Sourced[ReconciledMonomorphicExpression]) extends Expression

  given Show[Expression] = {
    case IntegerLiteral(Sourced(_, _, value))                       => value.toString()
    case StringLiteral(Sourced(_, _, value))                        => s"\"$value\""
    case FunctionApplication(Sourced(_, _, targetValue), arguments) =>
      targetValue.expression.show + arguments.map(_.value.expression.show).mkString("(", ", ", ")")
    case FunctionLiteral(parameters, body)                          =>
      parameters.map(_.name.value).mkString("(", ", ", ")") + " -> " + body.value.expression.show
    case MonomorphicValueReference(valueName, _)                    => valueName.value.show
    case ParameterReference(parameterName)                          => parameterName.value
    case Reconcile(source)                                          => "reconcile(" + source.value.expression.show + ")"
  }

  extension (expression: Expression) {

    /** The parameter names this expression references but does not itself bind — its free variables. Mirrors the
      * uncurried form; a [[Reconcile]] is transparent (it wraps a single subexpression).
      */
    def freeVariables: Seq[String] =
      expression match {
        case FunctionApplication(target, arguments) =>
          target.value.expression.freeVariables ++ arguments.flatMap(_.value.expression.freeVariables)
        case FunctionLiteral(parameters, body)      =>
          val boundNames = parameters.map(_.name.value).toSet
          body.value.expression.freeVariables.filterNot(boundNames.contains)
        case Reconcile(source)                      => source.value.expression.freeVariables
        case IntegerLiteral(_)                      => Seq.empty
        case StringLiteral(_)                       => Seq.empty
        case ParameterReference(parameterName)      => Seq(parameterName.value)
        case MonomorphicValueReference(_, _)        => Seq.empty
      }
  }
}
