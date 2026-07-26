package com.vanillasource.eliot.eliotc.row

import com.vanillasource.eliot.eliotc.effect.processor.{EffectCarriers, EffectMachinery}
import com.vanillasource.eliot.eliotc.module.fact.{Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.*
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN

/** The effects-as-rows **per-definition row checker** (docs/effects-as-rows.md §2 / Appendix A) — R3: production code,
  * but **unwired** into the pipeline; consumed only by its unit tests and the shadow corpus sweep until the R5 flip.
  *
  * Derives each definition's performed row from its operator-resolved body and checks `derived ⊆ declared` — with no
  * types, no carriers, no metavariables. The one derivation rule (Appendix A.1):
  *
  * `row(call) = declared(callee) ∪ ⋃ᵢ (contrib(argᵢ) ∖ pinnedEntries(callee, i))`
  *
  * where a *pinned* (capture) slot subtracts the entries its declared stack discharges (a platform *run boundary* —
  * `runMain`'s `IO[A]`, tag source (ii) — captures the whole row), and every non-pinned slot — strict or
  * declared-suspended — contributes identically: suspension changes only *when* an effect runs (elaboration's
  * business, R4), never whether the caller must declare it (Appendix A.2).
  *
  * Everything read is *declared*: callee rows (open-row entries + carrier-binder constraints), pinned entries
  * (`EffectRow.pinnedParameterEffects`, recorded at the desugar — R2), suspended-parameter rows
  * (`EffectRow.parameterEffects`), and the run-boundary registry. Nothing depends on instantiation or solver state,
  * which is what makes the check per-definition and its diagnostics located at the offending definition.
  *
  * Deliberate approximations, shared with the spike and noted in Appendix A.5/A.6 (production-final semantics arrive
  * with the R4 desugar where needed):
  *   - rows are *sets* of ability FQNs — the multiset/type-args refinement matters only for multi-instance same-ability
  *     rows, which are a diagnostic case;
  *   - a function-valued argument's latent row joins the receiving call conservatively ("the callee may run it") — the
  *     row-variable-per-arrow refinement would only *narrow* this;
  *   - a definition's derived row includes its body's latent row (an eta-reduced body `def go = use` performs `use`'s
  *     row when applied);
  *   - a reference to a callee with no [[OperatorResolvedValue]] in the universe contributes its ability (for a method
  *     reference) or nothing (for a plain value), and is reported in [[RowResult.unknownCallees]] so a sweep can see
  *     its own coverage instead of silently under-deriving.
  */
object RowChecker {

  /** A row: the set of user-facing effect abilities. */
  type Row = Set[AbilityFQN]

  /** The declared world the checker reads: the operator-resolved values by name, plus the platform-registered run
    * boundaries (values whose first parameter captures the whole row — jvm's `runMain`).
    */
  case class Universe(
      values: Map[ValueFQN, OperatorResolvedValue],
      runBoundaries: Set[ValueFQN] = Set.empty
  )

  /** One definition's verdict: what its body derives vs. what its signature declares, plus the referenced names the
    * universe could not resolve (coverage information for a shadow sweep — an unknown callee means the derivation may
    * be incomplete at this definition).
    *
    * @param runCaptured
    *   True when the definition's declared return type is headed by a platform *run carrier* (`def main: IO[Unit]` —
    *   the carrier read off a registered run boundary's own parameter, never guessed from a name): the nominal-run
    *   spelling of a boundary, where the whole derived row is captured by the concrete carrier, so nothing leaks.
    */
  case class RowResult(
      vfqn: ValueFQN,
      derived: Row,
      declared: Row,
      unknownCallees: Set[ValueFQN],
      runCaptured: Boolean = false
  ) {

    /** The effects performed but not declared — non-empty is the v3 diagnostic "performs the effect 'X' but does not
      * declare it", located at this definition.
      */
    def leak: Row = if (runCaptured) Set.empty else derived -- declared
  }

  /** Row-check every checkable definition of the universe: runtime-role, non-type, body-carrying values. */
  def checkAll(universe: Universe): Seq[RowResult] =
    universe.values.values.toSeq
      .filter(checkable)
      .flatMap(orv => checkValue(orv.vfqn, universe))

  /** Whether a value is subject to the row check: it has a body (abstract signatures derive nothing), is a runtime-role
    * value (never a `@Signature` twin), and is a term-level definition (never a type constructor / alias / meta
    * companion, whose "bodies" are types).
    */
  def checkable(orv: OperatorResolvedValue): Boolean =
    orv.runtime.isDefined &&
      orv.vfqn.name.role == com.vanillasource.eliot.eliotc.module.fact.Role.Runtime &&
      (orv.vfqn.name.qualifier match {
        case Qualifier.Type | Qualifier.Meta => false
        case _                               => true
      })

  /** Check one definition against the universe. [[None]] if the value is unknown; a body-less definition derives the
    * empty row.
    */
  def checkValue(vfqn: ValueFQN, universe: Universe): Option[RowResult] =
    universe.values.get(vfqn).map { orv =>
      val view       = SignatureView.of(orv.signature)
      val derivation = orv.runtime
        .map { r =>
          val (paramNames, body) = peelLambdas(r.value)
          val env                = parameterEnvironment(orv, view, paramNames)
          valueRow(body, env, universe) |+| latentRow(body, env, universe)
        }
        .getOrElse(Derivation.empty)
      RowResult(
        vfqn,
        derivation.row,
        declaredRow(orv) ++ pinnedReturnEntries(orv),
        derivation.unknown,
        runCaptured = headOf(view.returnType.value).exists(runCarrierHeads(universe).contains)
      )
    }

  /** The platform run-carrier type heads, read off each registered run boundary's *own* declared first parameter
    * (`runMain(io: IO[A])` ⇒ `IO`) — declared information, never a name/shape guess. A definition returning such a
    * carrier (`def main: IO[Unit]`) is the nominal-run spelling of a boundary and captures its whole row. Also
    * exposed for the [[RowElaborator]]: a nominal-run body is a carrier region.
    */
  def runCarrierHeads(universe: Universe): Set[ValueFQN] =
    universe.runBoundaries.flatMap { boundary =>
      universe.values
        .get(boundary)
        .flatMap(orv => SignatureView.of(orv.signature).parameters.headOption)
        .flatMap(param => headOf(param.value))
    }

  private def headOf(tpe: OperatorResolvedExpression): Option[ValueFQN] =
    spine(tpe)._1 match {
      case ValueReference(name, _) => Some(name.value)
      case _                       => None
    }

  /** A definition's declared row: its open-row return entries unioned with the effects constrained on its carrier
    * binders — the two spellings of "declared" — machinery excluded.
    */
  def declaredRow(orv: OperatorResolvedValue): Row = {
    val view            = SignatureView.of(orv.signature)
    val fromConstraints =
      EffectCarriers.declaredEffects(EffectCarriers.carrierBinders(view), orv.paramConstraints)
    val fromReturnRow   = orv.effectRow.returnEffects.map(_.abilityFQN).toSet
    (fromConstraints ++ fromReturnRow).filterNot(machinery)
  }

  /** A *pinned* return (`def make: {X | G} A`) is a declared capture: the body's row lands in the returned stack, so
    * its entries count as declared (the capture-legality reading of `derived ⊆ declared`).
    */
  private def pinnedReturnEntries(orv: OperatorResolvedValue): Row =
    orv.effectRow.returnPinnedEffects.map(_.abilityFQN).toSet

  /** What a saturated reference to `fqn` alone performs (its declared row / its ability) — the callee half of the
    * derivation rule, exposed for the [[RowElaborator]] (its carrier-valued fallback for callees outside the
    * universe).
    */
  def calleeRow(fqn: ValueFQN, universe: Universe): Row =
    calleeContribution(fqn, universe).row

  /** Peel the leading binders of a runtime body, collecting their names — exposed for the [[RowElaborator]]. */
  def peelBinders(expr: OperatorResolvedExpression): (Seq[String], OperatorResolvedExpression) =
    peelLambdas(expr)

  /** A derivation in progress: the row performed plus the referenced names the universe could not resolve. */
  private case class Derivation(row: Row, unknown: Set[ValueFQN]) {
    def |+|(other: Derivation): Derivation      = Derivation(row ++ other.row, unknown ++ other.unknown)
    def minus(entries: Row): Derivation         = copy(row = row -- entries)
    def clearedWhen(cleared: Boolean): Derivation = if (cleared) copy(row = Set.empty) else this
  }

  private object Derivation {
    val empty: Derivation                = Derivation(Set.empty, Set.empty)
    def of(row: Row): Derivation         = Derivation(row, Set.empty)
    def unknown(v: ValueFQN): Derivation = Derivation(Set.empty, Set(v))
  }

  /** The declared rows of the enclosing definition's parameters, by binder name: a parameter carrying a declared open
    * row (a suspended slot, or an effectful-callback arrow row) contributes that row wherever it is referenced or
    * called. Alignment: the peeled runtime binders end with the value parameters (leading binders may be generics), so
    * the last `view.parameters.size` names align positionally with the signature's value parameters.
    */
  private def parameterEnvironment(
      orv: OperatorResolvedValue,
      view: SignatureView,
      paramNames: Seq[String]
  ): Map[String, Row] = {
    val valueParamNames = paramNames.takeRight(view.parameters.size)
    orv.effectRow.parameterEffects.flatMap { pe =>
      valueParamNames
        .lift(pe.parameterIndex)
        .map(name => name -> pe.effects.map(_.abilityFQN).toSet.filterNot(machinery))
    }.toMap
  }

  /** The row performed when *evaluating* the expression to a value. A lambda or an under-applied function reference
    * performs nothing itself (its row is latent); a saturated call performs its callee's declared row plus its
    * arguments' contributions, minus what pinned slots capture.
    */
  private def valueRow(expr: OperatorResolvedExpression, env: Map[String, Row], universe: Universe): Derivation = {
    val (head, args) = spine(expr)
    head match {
      case ValueReference(name, _)                      =>
        val orvOpt      = universe.values.get(name.value)
        val saturated   = args.size >= orvOpt.map(o => SignatureView.of(o.signature).parameters.size).getOrElse(0)
        val callee      = if (saturated) calleeContribution(name.value, universe) else Derivation.empty
        val runBoundary = universe.runBoundaries.contains(name.value)
        args.zipWithIndex
          .map { case (arg, i) =>
            argumentContribution(arg.value, env, universe)
              .minus(pinnedEntries(orvOpt, i))
              .clearedWhen(runBoundary && i == 0)
          }
          .foldLeft(callee)(_ |+| _)
      case FunctionLiteral(_, _, body) if args.nonEmpty =>
        // An applied lambda — the block/`val` desugar: the bound argument runs, then the continuation.
        args
          .map(a => argumentContribution(a.value, env, universe))
          .foldLeft(valueRow(body.value, env, universe))(_ |+| _)
      case _: FunctionLiteral                           =>
        Derivation.empty
      case ParameterReference(name)                     =>
        // Referencing (or calling) a parameter contributes its declared row — a suspended parameter's effects belong
        // to this definition's row wherever the body places the computation.
        args
          .map(a => argumentContribution(a.value, env, universe))
          .foldLeft(Derivation.of(env.getOrElse(name.value, Set.empty)))(_ |+| _)
      case _                                            =>
        Derivation.empty
    }
  }

  /** The contribution of an argument at a (non-pinned) slot: the effects of evaluating it, plus — conservatively — the
    * latent row of a function value passed in (the callee may run it).
    */
  private def argumentContribution(
      arg: OperatorResolvedExpression,
      env: Map[String, Row],
      universe: Universe
  ): Derivation =
    valueRow(arg, env, universe) |+| latentRow(arg, env, universe)

  /** The latent row of a function-valued expression: what it would perform when run. Empty for a non-function value
    * (its effects already ran and are in [[valueRow]]).
    */
  private def latentRow(expr: OperatorResolvedExpression, env: Map[String, Row], universe: Universe): Derivation =
    expr match {
      case FunctionLiteral(_, _, body) =>
        val (_, inner) = peelLambdas(body.value)
        valueRow(inner, env, universe)
      case _                           =>
        val (head, args) = spine(expr)
        head match {
          case ValueReference(name, _)
              if universe.values.get(name.value).exists(o => args.size < SignatureView.of(o.signature).parameters.size) =>
            calleeContribution(name.value, universe)
          case ParameterReference(name) if args.isEmpty =>
            Derivation.of(env.getOrElse(name.value, Set.empty))
          case _                                        =>
            Derivation.empty
        }
    }

  /** What a saturated reference to `fqn` performs: an effect-ability method performs its ability (machinery and
    * first-order abilities — no higher-kinded carrier binder on the method — perform nothing); an ordinary definition
    * performs its declared row. A name absent from the universe is tracked as unknown; a *method* reference still
    * contributes its ability (the fail-loud direction).
    */
  private def calleeContribution(fqn: ValueFQN, universe: Universe): Derivation =
    fqn.name.qualifier match {
      case Qualifier.Ability(abilityName) =>
        val ability = AbilityFQN(fqn.moduleName, abilityName)
        if (machinery(ability)) Derivation.empty
        else
          universe.values.get(fqn) match {
            case Some(orv) =>
              val effectful = EffectCarriers.carrierBinders(SignatureView.of(orv.signature)).nonEmpty
              if (effectful) Derivation.of(Set(ability)) else Derivation.empty
            case None      => Derivation.of(Set(ability)) |+| Derivation.unknown(fqn)
          }
      case _                              =>
        universe.values.get(fqn) match {
          case Some(orv) => Derivation.of(declaredRow(orv))
          case None      => Derivation.unknown(fqn)
        }
    }

  /** The entries a callee's pinned parameter at `index` discharges, from the R2-recorded row metadata. */
  private def pinnedEntries(callee: Option[OperatorResolvedValue], index: Int): Row =
    callee
      .flatMap(_.effectRow.pinnedParameterEffects.find(_.parameterIndex == index))
      .map(_.effects.map(_.abilityFQN).toSet)
      .getOrElse(Set.empty)

  private def machinery(ability: AbilityFQN): Boolean =
    EffectMachinery.isMachineryAbility(ability.abilityName)

  /** Peel the leading binders of a definition's runtime, collecting their names in order — the definition's row is the
    * row of its fully-applied body.
    */
  private def peelLambdas(expr: OperatorResolvedExpression): (Seq[String], OperatorResolvedExpression) = expr match {
    case FunctionLiteral(name, _, body) =>
      val (rest, inner) = peelLambdas(body.value)
      (name.value +: rest, inner)
    case other                          => (Seq.empty, other)
  }
}
