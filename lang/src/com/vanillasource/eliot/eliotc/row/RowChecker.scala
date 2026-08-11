package com.vanillasource.eliot.eliotc.row

import com.vanillasource.eliot.eliotc.effect.processor.{EffectCarriers, EffectMachinery}
import com.vanillasource.eliot.eliotc.module.fact.{Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.*
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN

/** The effects-as-rows **per-definition row checker** (docs/effects-as-rows.md §2 / Appendix A), run by
  * [[com.vanillasource.eliot.eliotc.row.processor.RowElaborationProcessor]] on every runtime definition.
  *
  * Derives each definition's performed row from its operator-resolved body and checks `derived ⊆ declared` — with no
  * types, no carriers, no metavariables. The one derivation rule (Appendix A.1):
  *
  * `row(call) = declared(callee) ∪ ⋃ᵢ (contrib(argᵢ) ∖ pinnedEntries(callee, i))`
  *
  * where a *pinned* (capture) slot subtracts the entries its declared stack discharges (a platform *run boundary* —
  * `runMain`'s `IO[A]`, tag source (ii) — captures the whole row), and every non-pinned slot — strict or
  * declared-suspended — contributes identically: suspension changes only *when* an effect runs (elaboration's business,
  * R4), never whether the caller must declare it (Appendix A.2).
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
    *
    * A batch consumer (a sweep, a test) holds every value up front. A *demand-driven* consumer (the pipeline processor)
    * cannot: it must know which names the derivation actually consults before it can fetch them. That is what `onMiss`
    * is for — every consultation of a name the map does not hold reports it, so the processor can fetch the reported
    * names and re-run until nothing is missing. Without it a demand-driven consumer would have to guess the consulted
    * set and would silently fall back to the "unknown callee" approximations on a wrong guess.
    */
  case class Universe(
      values: Map[ValueFQN, OperatorResolvedValue],
      runBoundaries: Set[ValueFQN] = Set.empty,
      onMiss: ValueFQN => Unit = _ => ()
  ) {

    /** Consult a *referenced* name (a callee, a type alias, a run boundary), reporting a miss. Reading the value
      * currently under check goes directly to [[values]] instead — its absence is not a derivation gap.
      */
    def lookup(fqn: ValueFQN): Option[OperatorResolvedValue] = {
      val found = values.get(fqn)
      if (found.isEmpty) onMiss(fqn)
      found
    }
  }

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
      runCaptured: Boolean = false,
      returnMayCarry: Boolean = false
  ) {

    /** The effects performed but not declared — non-empty is the v3 diagnostic "performs the effect 'X' but does not
      * declare it", located at this definition.
      */
    def leak: Row = if (runCaptured) Set.empty else derived -- declared

    /** Whether a [[leak]] here is decidable from declarations alone, and so may be *reported* at this definition.
      *
      * A definition that declares an ambient always is: its body's contributions ride that ambient by construction. One
      * that declares none is decidable only when its declared return **cannot itself be the carrier** — a nullary
      * concrete type (`String`, `Unit`). An *applied* return (`Box[String]`, `IO[Unit]`, `Either[E, A]`) or a
      * generic-headed one may be exactly what hosts the effect: whether `def f: Box[String] = wrap(s)` is a
      * constructor-class use or an effect leak is decided by the instantiation (`F := Box` never rides), which no
      * pre-mono derivation can see. Those stay with the post-mono
      * [[com.vanillasource.eliot.eliotc.monomorphize.channel.EffectAccountingProcessor]] and the checker's own carrier
      * resolution.
      */
    def decidable: Boolean = declared.nonEmpty || !returnMayCarry
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
        case Qualifier.Type | _: Qualifier.Meta => false
        case _                                  => true
      })

  /** Check one definition against the universe. [[None]] if the value is unknown; a body-less definition derives the
    * empty row.
    */
  def checkValue(vfqn: ValueFQN, universe: Universe): Option[RowResult] =
    universe.values.get(vfqn).map { orv =>
      val view       = SignatureView.of(orv.signature)
      val declared   = declaredRow(orv) ++ pinnedReturnEntries(orv)
      val derivation = orv.runtime
        .map { r =>
          val (paramNames, body) = peelLambdas(r.value)
          val env                = parameterEnvironment(orv, view, paramNames)
          valueRow(body, env, declared, universe) |+| latentRow(body, env, declared, universe)
        }
        .getOrElse(Derivation.empty)
      RowResult(
        vfqn,
        // Machinery drops out of the *derived* row exactly as [[declaredRow]] drops it from the declared one: a
        // `{Effect}` entry is the row variable a parameter declares (see [[parameterEnvironment]]), not an effect a
        // user can name, declare or be told about. Filtering both sides is what keeps `derived ⊆ declared` — and its
        // diagnostic — spoken entirely in user abilities.
        derivation.row.filterNot(machinery),
        declared,
        derivation.unknown,
        runCaptured = headOf(view.returnType.value).exists(runCarrierHeads(universe).contains),
        returnMayCarry = mayCarry(view.returnType.value)
      )
    }

  /** The platform run-carrier type heads, read off each registered run boundary's *own* declared first parameter
    * (`runMain(io: IO[A])` ⇒ `IO`) — declared information, never a name/shape guess. A definition returning such a
    * carrier (`def main: IO[Unit]`) is the nominal-run spelling of a boundary and captures its whole row. Also exposed
    * for the [[RowElaborator]]: a nominal-run body is a carrier region.
    */
  def runCarrierHeads(universe: Universe): Set[ValueFQN] =
    universe.runBoundaries.flatMap { boundary =>
      universe
        .lookup(boundary)
        .flatMap(orv => SignatureView.of(orv.signature).parameters.headOption)
        .flatMap(param => headOf(param.value))
    }

  /** Whether a declared type could itself be an effect carrier applied to its payload — an *applied* type
    * (`Box[String]`, `IO[Unit]`) or one headed by a generic the use site instantiates. A nullary concrete type
    * (`String`, `Unit`) cannot host a carrier at all, which is what makes a leak under it decidable here.
    */
  private def mayCarry(tpe: OperatorResolvedExpression): Boolean = {
    val (head, args) = spine(tpe)
    args.nonEmpty || head.isInstanceOf[ParameterReference]
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

  /** The row a single expression derives on the enclosing definition's *ambient* carrier — the [[RowElaborator]]'s test
    * for whether an argument must *run* at its call site. An expression *performs* iff its row is non-empty; a
    * carrier-typed **value** — a reified computation such as `pure(x)`, or a parameter holding one — derives the empty
    * row and is data to pass on, not work to sequence. So does a call whose effects [[capturedByStack]] routes onto a
    * discharge stack: it runs on a carrier of its own, not on this one.
    */
  def expressionRow(
      expr: OperatorResolvedExpression,
      env: Map[String, Row],
      ambient: Row,
      universe: Universe
  ): Row =
    valueRow(expr, env, ambient, universe).row

  /** The row an expression contributes when it is **delivered to a slot** — what evaluating it performs, plus the
    * latent row of the function it may be ([[argumentContribution]], the same derivation an argument gets inside a
    * call).
    *
    * This is the sibling of [[expressionRow]] and the difference matters at exactly one place. `expressionRow` answers
    * *"does this have to run here?"* — the [[RowElaborator]]'s hoisting test, for which a lambda is a value and
    * performs nothing. This one answers *"what row does this position instantiate the callee's row variable at?"*, and
    * there a lambda is the opposite: `items.foreach(x -> printLine(x))` puts its `{Console}` in the lambda's body, so
    * the row that reaches `.` is latent by construction. Reading only the evaluation row would call that instantiation
    * empty and write the call at `Id`.
    */
  def argumentRow(
      expr: OperatorResolvedExpression,
      env: Map[String, Row],
      ambient: Row,
      universe: Universe
  ): Row =
    argumentContribution(expr, env, ambient, universe).row

  /** The effects some signature in this universe **pins** — the ones a discharger in scope can consume, and so the only
    * ones a call can be routed onto a carrier stack of its own for (A.11.4-R's corpus-forced filter). A
    * `Suspend`-riding effect (`Console`, `Log`, `Inf`) has no `<Ability>Carrier` at all and is always provided by the
    * base carrier, so it never appears here — which is what keeps [[capturedByStack]] from "discharging" an effect onto
    * a layer that does not and cannot exist.
    */
  def dischargeableAbilities(universe: Universe): Set[AbilityFQN] =
    universe.values.values
      .flatMap { orv =>
        orv.effectRow.returnPinnedEffects ++ orv.effectRow.pinnedParameterEffects.flatMap(_.effects)
      }
      .map(_.abilityFQN)
      .toSet

  /** The entries of a callee's declared row that do **not** ride the calling definition's ambient carrier: those the
    * ambient does not declare and a discharger in scope can consume. This is the derivation half of [[RowElaborator]]'s
    * `carrierAt` (A.11.4-R, Robert's decision) and must stay its mirror — the elaborator gives such a call a carrier
    * stack of its own (`rename("after")` ⤳ `StateCarrier[String, F]` under a `{Console}` ambient), so its effects land
    * in that stack for a consumer to discharge rather than on this definition's row. A verifier that counted them
    * anyway would report a leak for every dot-chained discharge the elaborator had just routed correctly, and the two
    * would disagree about the same call.
    *
    * A capture nothing discharges is not thereby accepted: the stack it carries meets the declared return type and the
    * checker rejects it, and the post-mono accounting — whose *ride test* against the ground ambient carrier this rule
    * mirrors — remains the unconditional verifier.
    */
  def capturedByStack(calleeRow: Row, ambient: Row, universe: Universe): Row = {
    val escaping = calleeRow -- ambient
    if (escaping.isEmpty) Set.empty else escaping.intersect(dischargeableAbilities(universe))
  }

  /** The declared rows of a definition's parameters by binder name (suspended slots, effectful-callback arrows) —
    * exposed for the [[RowElaborator]], which elaborates in the same environment the checker derives in.
    */
  def parameterRowsOf(orv: OperatorResolvedValue, paramNames: Seq[String]): Map[String, Row] =
    parameterEnvironment(orv, SignatureView.of(orv.signature), paramNames)

  /** What a saturated reference to `fqn` alone performs (its declared row / its ability) — the callee half of the
    * derivation rule, exposed for the [[RowElaborator]] (its carrier-valued fallback for callees outside the universe).
    */
  def calleeRow(fqn: ValueFQN, universe: Universe): Row =
    calleeContribution(fqn, universe).row

  /** Peel the leading binders of a runtime body, collecting their names — exposed for the [[RowElaborator]]. */
  def peelBinders(expr: OperatorResolvedExpression): (Seq[String], OperatorResolvedExpression) =
    peelLambdas(expr)

  /** A derivation in progress: the row performed plus the referenced names the universe could not resolve. */
  private case class Derivation(row: Row, unknown: Set[ValueFQN]) {
    def |+|(other: Derivation): Derivation        = Derivation(row ++ other.row, unknown ++ other.unknown)
    def minus(entries: Row): Derivation           = copy(row = row -- entries)
    def clearedWhen(cleared: Boolean): Derivation = if (cleared) copy(row = Set.empty) else this

    /** Keep only what rides the enclosing definition's ambient carrier — see [[capturedByStack]]. */
    def ridingOn(ambient: Row, universe: Universe): Derivation = minus(capturedByStack(row, ambient, universe))
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
    *
    * **Machinery is kept here**, unlike everywhere else a row is built. A row-polymorphic slot (`f: A => {Effect} B`,
    * `whenTrue: {Effect} A`) declares the row *variable* ρ, whose entries are exactly the ones this derivation cannot
    * name — and "nothing I can name" is not the claim "nothing". Dropping the machinery entry states the second, and
    * the two questions the derivation answers then both come out wrong for every row-polymorphic definition: `f(e)`
    * reads as pure, so §1 rule 1's hoist never happens and the computation is passed inline to a payload slot; and the
    * position settles `ρ := {}`, so the whole call is written at `Id` and an effect lands on a carrier that cannot
    * perform it. That is `map`, `filter`, `groupBy` — every combinator taking a user function.
    *
    * The entry never reaches the user: rows are also the verification vocabulary, and [[checkValue]] drops machinery
    * from the derived row exactly as [[declaredRow]] drops it from the declared one, so `derived ⊆ declared` is decided
    * in user abilities on both sides. Keeping the entry *inside* the derivation is what lets one row answer both
    * questions instead of a second, parallel predicate.
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
        .map(name => name -> pe.effects.map(_.abilityFQN).toSet)
    }.toMap
  }

  /** The row performed when *evaluating* the expression to a value. A lambda or an under-applied function reference
    * performs nothing itself (its row is latent); a saturated call performs its callee's declared row plus its
    * arguments' contributions, minus what pinned slots capture.
    */
  private def valueRow(
      expr: OperatorResolvedExpression,
      env: Map[String, Row],
      ambient: Row,
      universe: Universe
  ): Derivation = {
    val (head, args) = spine(expr)
    head match {
      case ValueReference(name, _)                      =>
        val orvOpt      = universe.lookup(name.value)
        val view        = orvOpt.map(o => SignatureView.of(o.signature))
        val saturated   = args.size >= view.map(_.parameters.size).getOrElse(0)
        val callee      =
          if (saturated) calleeContribution(name.value, universe).ridingOn(ambient, universe) else Derivation.empty
        val runBoundary = universe.runBoundaries.contains(name.value)
        args.zipWithIndex
          .map { case (arg, i) =>
            argumentContribution(arg.value, env, ambient, universe)
              .minus(pinnedEntries(orvOpt, i))
              .clearedWhen(runBoundary && i == 0)
          }
          .foldLeft(callee)(_ |+| _)
      case FunctionLiteral(_, _, body) if args.nonEmpty =>
        // An applied lambda — the block/`val` desugar: the bound argument runs, then the continuation.
        args
          .map(a => argumentContribution(a.value, env, ambient, universe))
          .foldLeft(valueRow(body.value, env, ambient, universe))(_ |+| _)
      case _: FunctionLiteral                           =>
        Derivation.empty
      case ParameterReference(name)                     =>
        // Referencing (or calling) a parameter contributes its declared row — a suspended parameter's effects belong
        // to this definition's row wherever the body places the computation.
        args
          .map(a => argumentContribution(a.value, env, ambient, universe))
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
      ambient: Row,
      universe: Universe
  ): Derivation =
    valueRow(arg, env, ambient, universe) |+| latentRow(arg, env, ambient, universe)

  /** The latent row of a function-valued expression: what it would perform when run. Empty for a non-function value
    * (its effects already ran and are in [[valueRow]]).
    */
  private def latentRow(
      expr: OperatorResolvedExpression,
      env: Map[String, Row],
      ambient: Row,
      universe: Universe
  ): Derivation =
    expr match {
      case FunctionLiteral(_, _, body) =>
        val (_, inner) = peelLambdas(body.value)
        valueRow(inner, env, ambient, universe)
      case _                           =>
        val (head, args) = spine(expr)
        head match {
          case ValueReference(name, _)
              if universe.lookup(name.value).exists(o => args.size < SignatureView.of(o.signature).parameters.size) =>
            calleeContribution(name.value, universe).ridingOn(ambient, universe)
          case ParameterReference(name) if args.isEmpty =>
            Derivation.of(env.getOrElse(name.value, Set.empty))
          case _                                        =>
            Derivation.empty
        }
    }

  /** What a saturated reference to `fqn` performs: its **declared row**, whatever kind of definition it is. A name
    * absent from the universe is tracked as unknown.
    *
    * An ability method is not a special case, and used to be: it contributed its owning ability whenever its signature
    * had any higher-kinded binder. That is a read of *shape*, and it is wrong in both directions — it made a
    * constructor-class method (`Container`'s `wrap`/`unwrap`) perform an effect it does not have, and it overrode a row
    * the method did declare (a method of `Beep` declaring `{Console}` was still charged `{Beep}`). Effect-ness is not a
    * property an ability *has*; it is a property of what a method *does*, and a method says what it does the same way
    * every other definition does — with a row on its return, which [[declaredRow]] already reads.
    */
  private def calleeContribution(fqn: ValueFQN, universe: Universe): Derivation =
    universe.lookup(fqn) match {
      case Some(orv) => Derivation.of(declaredRow(orv))
      case None      => Derivation.unknown(fqn)
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
