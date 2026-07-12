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
  * It mirrors [[com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedMonomorphicExpression]] node-for-node and adds one
  * thing the representation-lowering pass used to bake into the *type*: every node carries its refinement `meta` — the
  * value channel's [[GroundValue]] for this node (an `Interval` value) when the channel pinned one, or [[None]] for a
  * ⊤/unknown node (which the backend lays out as a bignum). The backend derives the node's machine width from this meta,
  * and inserts any needed representation re-encode itself, wherever a value's width meets a differently-sized consumer
  * (a call argument's ⊤ parameter boundary, a `fold` arm's merged width, the method return) — the width policy and the
  * conversions both live in the backend, from the ranges; this pass only carries them.
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

  given Show[Expression] = {
    case IntegerLiteral(Sourced(_, _, value))                       => value.toString()
    case StringLiteral(Sourced(_, _, value))                        => s"\"$value\""
    case FunctionApplication(Sourced(_, _, targetValue), arguments) =>
      targetValue.expression.show + arguments.map(_.value.expression.show).mkString("(", ", ", ")")
    case FunctionLiteral(parameters, body)                          =>
      parameters.map(_.name.value).mkString("(", ", ", ")") + " -> " + body.value.expression.show
    case MonomorphicValueReference(valueName, _)                    => valueName.value.show
    case ParameterReference(parameterName)                          => parameterName.value
  }

  extension (expression: Expression) {

    /** The parameter names this expression references but does not itself bind — its free variables. Mirrors the
      * uncurried form.
      */
    def freeVariables: Seq[String] =
      expression match {
        case FunctionApplication(target, arguments) =>
          target.value.expression.freeVariables ++ arguments.flatMap(_.value.expression.freeVariables)
        case FunctionLiteral(parameters, body)      =>
          val boundNames = parameters.map(_.name.value).toSet
          body.value.expression.freeVariables.filterNot(boundNames.contains)
        case IntegerLiteral(_)                      => Seq.empty
        case StringLiteral(_)                       => Seq.empty
        case ParameterReference(parameterName)      => Seq(parameterName.value)
        case MonomorphicValueReference(_, _)        => Seq.empty
      }
  }
}
