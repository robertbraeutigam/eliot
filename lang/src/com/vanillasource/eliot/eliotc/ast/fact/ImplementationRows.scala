package com.vanillasource.eliot.eliotc.ast.fact

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The **clause row** of an `implement` block: what the implementation itself performs — effects v6,
  * `docs/effects.md` §9.4 step 3.
  *
  * An implementation whose clauses declare a row is *parameterised by what they perform*, so `with recordingConsole`
  * has to write not just the marker but the implementations its clauses' `{Writer[String]}` runs on —
  * `greeting[recordingConsole[cellWriter]]`. That parameter is a phantom binder like every other one, and the only
  * thing this object arranges is that **every value of the block declares the same one, in the same order**.
  *
  * It is the **union** of the clauses' rows rather than each clause's own, because
  * [[com.vanillasource.eliot.eliotc.monomorphize.check.AbilityResolver]] hands the marker's type arguments to *every*
  * method of the implementation positionally: one list, so one prefix. A clause that performs nothing therefore
  * declares what its siblings perform — which is true of the implementation, is bound at the same `with`, and costs
  * nothing at runtime since a phantom binder is erased. Nothing here mints a binder: the entries are written into the
  * ordinary return-row position and [[com.vanillasource.eliot.eliotc.core.processor.EffectSugarDesugarer]] mints them
  * exactly as it mints any other row, which is what keeps marker and methods in step by construction.
  *
  * The **marker** carries the union on its guard slot. A row is erased from every type by that same desugar, so the
  * guard reaches [[com.vanillasource.eliot.eliotc.ability.processor.AbilityImplementationProcessor]] untouched; what
  * the marker gains is the declaration the binder is read back from — by the write
  * ([[com.vanillasource.eliot.eliotc.row.BindingWriter]]) for a `with`, and by the anonymous form's pattern match,
  * which fills a binder no pattern argument solves with `Default`.
  *
  * **Associated types are excluded.** A `type` inside an `implement` block is a type occupying a pattern slot, not a
  * clause: giving it a row would mint binders on it and change its arity at every use.
  */
object ImplementationRows {

  /** Every effect the block's clauses declare, in clause order, without repetition. */
  def union(clauses: Seq[FunctionDefinition]): Seq[UnresolvedAbilityConstraint[Sourced[Expression]]] =
    clauses.flatMap(clause => entriesOf(clause.typeDefinition)).distinctBy(UnresolvedAbilityConstraint.key)

  /** The definition with `row` as its declared return row, in place of whatever it declared. */
  def carrying(
      row: Seq[UnresolvedAbilityConstraint[Sourced[Expression]]],
      definition: FunctionDefinition
  ): FunctionDefinition =
    if (row.isEmpty) definition else definition.copy(typeDefinition = rowed(row, definition.typeDefinition))

  /** The expression with `row` as its top-level row, in place of whatever it had. */
  def rowed(
      row: Seq[UnresolvedAbilityConstraint[Sourced[Expression]]],
      expr: Sourced[Expression]
  ): Sourced[Expression] =
    if (row.isEmpty) expr
    else
      expr.value match {
        case Expression.EffectfulType(_, resultType, tail) =>
          expr.as(Expression.EffectfulType(row, resultType, tail))
        case _                                             =>
          expr.as(Expression.EffectfulType(row, expr, None))
      }

  private def entriesOf(expr: Sourced[Expression]): Seq[UnresolvedAbilityConstraint[Sourced[Expression]]] =
    expr.value match {
      case Expression.EffectfulType(effects, _, None) => effects
      case _                                          => Seq.empty
    }
}
