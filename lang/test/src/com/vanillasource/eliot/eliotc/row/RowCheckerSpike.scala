package com.vanillasource.eliot.eliotc.row

import com.vanillasource.eliot.eliotc.effect.processor.{EffectCarriers, EffectMachinery}
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.*
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}

/** The effects-as-rows **R1 spike** (docs/effects-as-rows.md §8): a standalone, unwired row checker over
  * [[OperatorResolvedValue]] facts. It derives a definition's performed row from its body by the v3 rules and checks
  * `derived ⊆ declared` — with **no types, no carriers, no metavariables**: rows are sets, derivation is one
  * bottom-up walk, and every rule reads *declared* information (callee rows, pinned capture slots), never an
  * instantiation.
  *
  * The derivation rule (one line, the spike's central finding — see the doc's Appendix A):
  *
  * `row(call) = declared(callee) ∪ ⋃ᵢ (row(argᵢ) ∖ pinnedEntries(callee, i))`
  *
  * Strict and *suspended* slots derive identically (suspension changes only elaboration — when the effect runs —
  * never whether the caller must declare it, matching v2 accounting's declaration-level conservatism); only a
  * *pinned* (capture) slot differs, subtracting the entries its declared stack discharges.
  *
  * Spike simplifications, each noted in the doc (Appendix A, "Production deltas"):
  *   - a `Row` is a set of ability *names* — production rows are multisets of (ability, type-args), so
  *     `{Throw[A], Throw[B]}` stays two entries and discharge consumes by type-arg match;
  *   - pinned-slot entries are recovered from the desugared `<Ability>Carrier` stack by the naming convention —
  *     production records the entries beside the position tag in `EffectRow` at the desugar (R2), so no name-based
  *     recognition survives;
  *   - a function-valued argument's *latent* row joins the receiving call conservatively ("the callee may run it") —
  *     production threads a row variable per arrow instead;
  *   - an ability method with no [[OperatorResolvedValue]] in the lookup map is assumed effectful (production
  *     distinguishes first-order abilities by their missing HKT binder, as [[EffectCarriers]] does).
  */
object RowCheckerSpike {

  /** A row: the set of user-facing effect-ability names. */
  type Row = Set[String]

  /** The result of checking one definition: what its body derives vs. what its signature declares. */
  case class RowResult(derived: Row, declared: Row) {

    /** The effects performed but not declared — non-empty means the v3 diagnostic "performs the effect 'X' but does
      * not declare it", located at this definition.
      */
    def leak: Row = derived -- declared
  }

  /** Check the named definition against the given fact universe. [[None]] if the value is unknown; a body-less
    * (abstract) definition derives the empty row.
    */
  def checkValue(vfqn: ValueFQN, values: Map[ValueFQN, OperatorResolvedValue]): Option[RowResult] =
    values.get(vfqn).map { orv =>
      val derived = orv.runtime.map(r => rowOfValue(peelLambdas(r.value), values)).getOrElse(Set.empty)
      RowResult(derived, declaredRow(orv))
    }

  /** A definition's declared row: its open-row return entries unioned with the effects constrained on its carrier
    * binders (the two spellings of "declared" — the surface row and the desugared `F ~ E` constraint — which today
    * carry the same information; machinery abilities excluded by [[EffectCarriers.declaredEffects]]).
    */
  def declaredRow(orv: OperatorResolvedValue): Row = {
    val view            = SignatureView.of(orv.signature)
    val fromConstraints =
      EffectCarriers
        .declaredEffects(EffectCarriers.carrierBinders(view), orv.paramConstraints)
        .map(_.abilityName)
    val fromReturnRow   = orv.effectRow.returnEffects.map(_.abilityFQN.abilityName).toSet
    fromConstraints ++ fromReturnRow
  }

  /** The row performed when *evaluating* the expression to a value. A lambda or an under-applied function reference
    * performs nothing itself (its row is latent in the arrow); a saturated call performs its callee's declared row
    * plus its arguments' rows, minus what pinned slots capture.
    */
  private def rowOfValue(expr: OperatorResolvedExpression, values: Map[ValueFQN, OperatorResolvedValue]): Row = {
    val (head, args) = spine(expr)
    head match {
      case ValueReference(name, _)                              =>
        val orvOpt     = values.get(name.value)
        val saturated  = args.size >= paramCount(name.value, values)
        val calleeRow  = if (saturated) contributionOf(name.value, values) else Set.empty[String]
        val pinnedIdx  = orvOpt.map(_.effectRow.pinnedParameterIndices).getOrElse(Set.empty[Int])
        val argRows    = args.zipWithIndex.map { case (arg, i) =>
          val r = argumentRow(arg.value, values)
          if (pinnedIdx.contains(i)) r -- orvOpt.map(pinnedEntries(_, i)).getOrElse(Set.empty)
          else r
        }
        argRows.foldLeft(calleeRow)(_ ++ _)
      case FunctionLiteral(_, _, body) if args.nonEmpty         =>
        // An applied lambda — the block/`val` desugar: the bound argument runs, then the continuation.
        args.map(a => argumentRow(a.value, values)).foldLeft(rowOfValue(body.value, values))(_ ++ _)
      case _: FunctionLiteral                                   =>
        Set.empty
      case _: ParameterReference                                =>
        // Calling a function-typed parameter: within this spike its latent row is unknown (production reads the
        // parameter's declared arrow row); the arguments' own rows still run.
        args.map(a => argumentRow(a.value, values)).foldLeft(Set.empty[String])(_ ++ _)
      case _: IntegerLiteral | _: StringLiteral                 =>
        Set.empty
      case _: FunctionApplication                               =>
        // spine() never returns an application head.
        Set.empty
    }
  }

  /** The row an argument contributes at a (non-pinned) slot: the effects of evaluating it, plus — conservatively —
    * the latent row of a function value passed in (the callee may run it).
    */
  private def argumentRow(arg: OperatorResolvedExpression, values: Map[ValueFQN, OperatorResolvedValue]): Row =
    rowOfValue(arg, values) ++ latentOf(arg, values)

  /** The latent row of a function-valued expression: what it would perform when run. Empty for a non-function value
    * (its effects already ran and are in [[rowOfValue]]).
    */
  private def latentOf(expr: OperatorResolvedExpression, values: Map[ValueFQN, OperatorResolvedValue]): Row =
    expr match {
      case FunctionLiteral(_, _, body) =>
        rowOfValue(peelLambdas(body.value), values)
      case _                           =>
        val (head, args) = spine(expr)
        head match {
          case ValueReference(name, _) if args.size < paramCount(name.value, values) =>
            contributionOf(name.value, values)
          case _                                                                     => Set.empty
        }
    }

  /** What a saturated reference to `fqn` performs: an effect-ability method performs its ability (machinery and
    * recognizably first-order abilities perform nothing); an ordinary definition performs its declared row.
    */
  private def contributionOf(fqn: ValueFQN, values: Map[ValueFQN, OperatorResolvedValue]): Row =
    EffectMachinery.abilityNameOf(fqn) match {
      case Some(ability) =>
        val firstOrder = values
          .get(fqn)
          .exists(orv => EffectCarriers.carrierBinders(SignatureView.of(orv.signature)).isEmpty)
        if (EffectMachinery.isMachineryAbility(ability) || firstOrder) Set.empty else Set(ability)
      case None          =>
        values.get(fqn).map(declaredRow).getOrElse(Set.empty)
    }

  /** The number of value parameters of `fqn`'s signature (0 when unknown — unknown callees are treated saturated so
    * their contribution is not silently dropped).
    */
  private def paramCount(fqn: ValueFQN, values: Map[ValueFQN, OperatorResolvedValue]): Int =
    values.get(fqn).map(orv => SignatureView.of(orv.signature).parameters.size).getOrElse(0)

  /** The ability entries a pinned parameter's declared carrier stack discharges, read off the desugared
    * `<Ability>Carrier[…]` chain (outermost first, descending each layer's base position). Spike-only name-based
    * inversion — production records these entries in `EffectRow` beside the position tag (R2).
    */
  private def pinnedEntries(orv: OperatorResolvedValue, index: Int): Row = {
    val view = SignatureView.of(orv.signature)
    if (index < view.parameters.size) collectStack(view.parameters(index).value, outer = true) else Set.empty
  }

  /** Walk a desugared carrier stack `<A>Carrier[abilityArgs…, base, payload]`. Only the *outer* layer is applied to
    * the payload; inner layers are `Type -> Type` shapes whose base is their *last* argument
    * (`ThrowCarrier[E, StateCarrier[S, Id], A]`).
    */
  private def collectStack(tpe: OperatorResolvedExpression, outer: Boolean): Row = {
    val (head, args) = spine(tpe)
    val minArgs      = if (outer) 2 else 1
    head match {
      case ValueReference(name, _) if name.value.name.name.endsWith("Carrier") && args.size >= minArgs =>
        val ability   = name.value.name.name.dropRight("Carrier".length)
        val baseIndex = if (outer) args.size - 2 else args.size - 1
        Set(ability) ++ collectStack(args(baseIndex).value, outer = false)
      case _                                                                                           => Set.empty
    }
  }

  /** Peel the leading parameter/generic binders of a definition's runtime — the definition's row is the row of its
    * fully-applied body (the latent row of the outer lambdas), which is what `derived ⊆ declared` checks.
    */
  private def peelLambdas(expr: OperatorResolvedExpression): OperatorResolvedExpression = expr match {
    case FunctionLiteral(_, _, body) => peelLambdas(body.value)
    case other                       => other
  }
}
