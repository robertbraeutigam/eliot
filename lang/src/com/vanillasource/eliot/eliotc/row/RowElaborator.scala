package com.vanillasource.eliot.eliotc.row

import cats.Eq
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.effect.EffectCarrierNaming
import com.vanillasource.eliot.eliotc.effect.processor.{EffectCarriers, EffectMachinery}
import com.vanillasource.eliot.eliotc.module.fact.{Qualifier, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.*
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue.ResolvedAbilityConstraint
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced

import scala.collection.mutable

/** The effects-as-rows **elaboration desugar** (docs/effects-as-rows.md §3) — rewrite a direct-style definition into
  * fully explicit monadic core, the same shape the v2 checker's elaboration produces, so downstream phases are
  * unchanged consumers.
  *
  * The rewrite decides every position from *declarations*, per call and order-free (§1 rule 4): a call's result kind
  * is its declared return **instantiated from the declared shapes of the arguments supplied**, so a plain generic —
  * `.`'s `B`, `++`'s `T`, `identity`'s `A` — is a payload and a carrier binder a computation. Writing *nothing* is
  * still the reaction where a signature genuinely settles nothing (a constructor-class return whose carrier no
  * parameter mentions, a callee outside the universe): a missing rewrite is completed, or loudly rejected,
  * downstream, while a guessed one silently changes when an effect runs. The elaborator may consult only the §3
  * whitelist — declared parameter/return types, declared rows and carrier binders
  * ([[EffectCarriers.declaredCarrierBinders]]), pinned metadata, the run-boundary registry, and one alias level — and
  * in particular never inspects a sibling argument's expression shape to decide a slot's mode.
  *
  * The central notion is **carrier-valued-ness**: an elaborated node either *is* a carrier computation (an effectful
  * call, a bind chain, a discharger call, a suspended-parameter reference) or is a plain value. Recognition is by
  * declared shape:
  *
  *   - a *call* is carrier-valued when the declared type remaining after its arguments is headed by one of the
  *     callee's own carrier binders (`readLine : F[Str]`, `catchX : G[A]`) or by a platform run carrier; a
  *     *generic-headed* remainder is whatever the argument determining that generic is (see [[Kind]]);
  *   - a *parameter slot* is declared-suspended when its declared type is carrier-headed (`whenTrue: {G} A` desugars
  *     to `F[A]`) — an arrow-typed callback (`action: A => {Effect} Unit`, codomain `F[Unit]`) is NOT suspended: the
  *     lambda is a value, only its *body* is a computation;
  *   - a *parameter reference* is carrier-valued when the parameter's declared type is carrier-headed (a suspended
  *     parameter holds its computation unrun); *calling* a function-typed parameter is carrier-valued when the
  *     declared arrow's final codomain is carrier-headed and the call saturates it.
  *
  * Elaboration is positional over two context flags: `needCarrier` (does this *position* expect a computation) and
  * the **region carrier flag** (does the enclosing region have a carrier to bind on). The region flag starts at the
  * definition — true when the declared return is carrier-headed, *pinned* (`{X | G} A`), or headed by a platform run
  * carrier (the nominal-run spelling) — and flips to true inside every **pinned or run-boundary argument** (the
  * captured computation's binds ride the pinned/run stack even under a pure definition) and inside every
  * carrier-codomain lambda body. The rules:
  *
  *   - a **carrier-valued argument at a declared-concrete strict slot** is hoisted under the region's carrier when
  *     it *performs* (its row is non-empty) or *discharges* (its callee captures a pinned slot / is a run boundary —
  *     an empty row, since the capture consumed it, yet still work to sequence): `printLine(readLine)` becomes
  *     `flatMap($row$1 -> printLine($row$1), readLine)`, left-to-right arguments nesting leftmost-outermost. A
  *     carrier-typed *value* (`pure(x)`, a suspended-parameter reference with an empty row) is data and passes.
  *     Hoisting *changes the instantiation* — the slot then holds the bind's payload binder — so the core call's own
  *     result kind is re-read off the hoisted arguments before it is wrapped;
  *   - a **carrier-valued `val`/statement binding** (the block desugar's applied lambda `(x -> rest)(e)`) becomes
  *     `flatMap(x -> rest', e')`; a pure binding stays an applied lambda;
  *   - a **pure expression in a carrier position** — the innermost continuation of a bind chain, a pure body under a
  *     declared row, a pure argument at a declared-suspended, pinned or run-boundary slot — is wrapped `pure(expr)`,
  *     but only when it is pure *by declaration* ([[Kind.Payload]]);
  *   - **discharge under an empty residual row** (Appendix A.4): where a *discharging* node meets a value position
  *     in a region with *no* carrier — a pure definition's body, binding, or strict argument slot — the region's
  *     base carrier is `Id` by declaration, and the node is unwrapped with `runId(...)` at that same boundary. In a
  *     carrier region it binds like any effectful call. `Id` never appears anywhere else;
  *   - a **lambda argument** elaborates its body: *forced* to a carrier region at a declared carrier-codomain slot
  *     (`onError: E => G[A]`, `action: A => {Effect} Unit` — a pure body is `pure`-wrapped), *naturally* at a plain
  *     arrow slot (an effectful body becomes a bind chain, a pure body is untouched);
  *   - **pure code is untouched**: a definition with an empty declared row and a pure body elaborates to itself,
  *     byte-for-byte — no `Id`, nothing to erase.
  */
object RowElaborator {

  /** What a position holds, once the declarations have settled it (§1 rule 4).
    *
    * [[Unknown]] is not a third semantic category but the absence of a declaration that decides: the elaborator writes
    * nothing there and the checker finishes the node from the solved instantiation. Under rule 4 it is reached only
    * where a signature genuinely leaves the question open — a constructor-class return (`def wrap[F[_], A](a: A):
    * F[A]`, whose `F` no parameter mentions) or a callee outside this value's universe.
    */
  private enum Kind {
    case Payload, Carrier, Unknown
  }

  private object Kind {
    given Eq[Kind] = Eq.fromUniversalEquals
  }

  /** The carrier a region binds on, in the form the elaborator can *use*.
    *
    * The distinction between the last two cases is the whole of A.11.4. A region either has no carrier at all, or has
    * one the elaborator can **spell** as a term written from the definition's *own* declaration (its minted binder
    * `F`, the pinned stack its return declares, a platform run carrier, or `Id` at a pure discharge boundary), or has
    * one that exists but is expressible only in a **callee's** binders — the interior of a pinned capture, whose
    * declared domain `{Throw[E] | G} A` names the callee's `E`. Deriving that `E` from the argument's own row would
    * be inference, which is precisely what writing the carrier exists to avoid, so it stays [[Unspelled]] and the
    * checker solves it exactly as before.
    *
    * All three behave identically for *placement*: [[exists]] is what the bind/`pure`/`runId` rules read. Only
    * [[term]] differs, and a missing term means "write nothing here", which is always the fail-safe direction.
    */
  private enum RegionCarrier {
    case Absent
    case Unspelled
    case Spelled(carrierTerm: Sourced[OperatorResolvedExpression])

    /** Whether a carrier exists to bind on — the positional flag every placement rule reads. */
    def exists: Boolean = this match {
      case Absent => false
      case _      => true
    }

    /** The carrier as a writable type argument, when the elaborator can spell it. */
    def term: Option[Sourced[OperatorResolvedExpression]] = this match {
      case Spelled(t) => Some(t)
      case _          => scala.None
    }
  }

  /** What one pass over a definition's body yields: the elaborated body ([[scala.None]] for a body-less value), and
    * the **§1 rule-4 violations** the body's calls contain — every position a callee declares as a plain generic, a
    * rowless position, that the call instantiates at a *computation* (A.11.7-T step 2).
    *
    * The two are produced together because the violation is read off the *elaborated* arguments: "an effect passes
    * through a position if and only if that position declares it" is only a rule if delivering a computation to a
    * rowless slot is rejected, and without the rejection the elaborator silently re-routes it — erosion #3 in §1's
    * table, measured as five `State`-family miscompiles (A.11.7-R). Reading it after hoisting is what keeps an
    * argument that merely *performs*, and is therefore run at the call site, from counting: that is rule 1 working,
    * and `choose(readLine, readLine)` is rule 1's own example. What remains is a computation *value* — a discharge
    * stack, a pinned capture, a `pure(x)`, a carrier-typed parameter.
    *
    * The predicate is deliberately wider than "a rowless slot receives a computation": an arrow slot `f: A => B`
    * handed a function whose *codomain* is a computation instantiates the rowless `B` just as directly, and that is
    * how most of `.`'s traffic violates the rule (§6.1-A).
    */
  case class Elaborated(
      body: Option[Sourced[OperatorResolvedExpression]],
      violations: Seq[Violation]
  )

  /** One §1 rule-4/rule-2 violation: the located message, plus the explanation of the rule it broke. The help travels
    * with the message because the two directions read oppositely — a computation delivered to a *rowless* slot, and a
    * plain value delivered to a *carrier-typed* one — and a reader needs the one that applies.
    */
  case class Violation(message: Sourced[String], help: Seq[String])

  /** Why a computation may not be routed through a position that declares no row (§1 rule 4). */
  val rowlessSlotHelp: Seq[String] = Seq(
    "A type parameter that declares no effect row is a payload: it can never stand for a computation.",
    "Call the function directly instead of routing the computation through this position, for example",
    "'runStateToPair(initial, program)' rather than 'program.runStateToPair(initial)'.",
    "Infix dischargers are unaffected: 'x catch (err -> ...)' and 'setting else \"default\"' already resolve",
    "to a direct call with the computation at its own declared slot."
  )

  /** Why a plain value may not be passed where a carrier-typed slot is declared (§1 rule 2). The dual of
    * [[rowlessSlotHelp]]: there the position promised a value and got a computation, here it promised a computation
    * and got a value.
    */
  val carrierSlotHelp: Seq[String] = Seq(
    "A parameter declared on a carrier ('x: G[A]') holds a computation: 'G' is a type there, not an effect row,",
    "so a plain 'A' is not one of its values and nothing lifts it into one.",
    "If the parameter should accept a value *or* a computation, declare it as an effect row: 'x: {Effect} A'.",
    "A row accepts a pure argument because the empty row is a legal row — that is the difference between the two."
  )

  /** Elaborate a definition's runtime body. [[None]] for a body-less value. The result mirrors the original
    * [[Sourced]] positions: inserted machinery nodes are attributed to the expression they wrap.
    */
  def elaborate(orv: OperatorResolvedValue, universe: RowChecker.Universe): Option[Sourced[OperatorResolvedExpression]] =
    elaborateChecked(orv, universe).body

  /** Elaborate a definition's runtime body and collect its rule-4 violations in the same pass. See [[Elaborated]]. */
  def elaborateChecked(orv: OperatorResolvedValue, universe: RowChecker.Universe): Elaborated = {
    val collected = mutable.Buffer.empty[Violation]
    Elaborated(elaborateBody(orv, universe, collected), collected.toSeq)
  }

  private def elaborateBody(
      orv: OperatorResolvedValue,
      universe: RowChecker.Universe,
      collected: mutable.Buffer[Violation]
  ): Option[Sourced[OperatorResolvedExpression]] =
    orv.runtime.map { runtime =>
      val (paramNames, body) = RowChecker.peelBinders(runtime.value)
      val view               = SignatureView.of(orv.signature)
      val ownBinders         = EffectCarriers.declaredCarrierBinders(orv)
      val paramTypes         = paramNames.takeRight(view.parameters.size).zip(view.parameters.map(_.value)).toMap
      val topCarrier         = topRegionCarrier(orv, view, ownBinders, universe)
      val valueParamNames    = paramNames.takeRight(view.parameters.size)
      // A parameter *holding* a computation: one declared by a carrier binder of this definition (a suspended slot),
      // or one whose declared type is a pinned stack (`computation: {Dep[X] | G} A` — a discharger's captured
      // argument, carrier-typed but headed by the stack, not by the binder).
      // The **pinned** value parameters, kept apart from the suspended ones they join in `carrierParams`: §1 rule 3
      // says a pinned row is a *reified* computation and therefore an ordinary type, so passing one on is passing
      // data — which is what keeps a carrier's own generated accessor (matching on its pinned `obj`) legal.
      val pinnedParams       = valueParamNames.zipWithIndex.collect {
        case (name, index) if orv.effectRow.pinnedParameterIndices.contains(index) => name
      }.toSet
      val carrierParams      = valueParamNames.zipWithIndex.collect {
        case (name, index)
            if paramTypes.get(name).exists(carrierHeaded(_, ownBinders)) ||
              orv.effectRow.pinnedParameterIndices.contains(index) =>
          name
      }.toSet
      val elab               =
        new Elaboration(
          paramTypes,
          carrierParams,
          pinnedParams,
          RowChecker.parameterRowsOf(orv, paramNames),
          ownBinders,
          view.binders.map(_.name.value).toSet,
          universe,
          EffectCarriers.declaredEffects(ownBinders, orv.paramConstraints),
          collected
        )
      val newBody            = elab.elaborate(runtime.as(body), needCarrier = topCarrier.exists, region = topCarrier)
      runtime.as(rewrap(runtime.value, newBody))
    }

  /** The carrier of a definition's own top-level region, read off its declared return — the three spellings a
    * definition has for "my body runs on a carrier", each of which names that carrier in the definition's *own*
    * binders and is therefore always [[RegionCarrier.Spelled]]:
    *
    *   - **carrier-headed** (`F[Unit]`, the `{Console} Unit` sugar's lowering): the carrier is the binder itself;
    *   - **pinned** (`{Throw[E] | G} A` ⤳ `ThrowCarrier[E, G, A]`): the carrier is that stack with its payload
    *     argument dropped, `ThrowCarrier[E, G]` — `E` and `G` are this definition's own binders, so it is writable
    *     here even though the *same* stack at a callee's pinned parameter is not (see [[RegionCarrier]]);
    *   - **nominal-run** (`def main: IO[Unit]`): the carrier is the concrete run carrier, read off the boundary
    *     registry rather than guessed from the name.
    *
    * A pure return has no carrier at all.
    */
  private def topRegionCarrier(
      orv: OperatorResolvedValue,
      view: SignatureView,
      ownBinders: Set[String],
      universe: RowChecker.Universe
  ): RegionCarrier = {
    val returnType = view.returnType
    if (carrierHeaded(returnType.value, ownBinders) || runCarrierReturn(view, universe))
      RegionCarrier.Spelled(returnType.as(spine(returnType.value)._1))
    else if (orv.effectRow.returnPinnedEffects.nonEmpty) dropPayloadArgument(returnType)
    else RegionCarrier.Absent
  }

  /** A carrier-headed type read as its carrier: the head applied to every argument *but* the payload
    * (`ThrowCarrier[E, G, A]` ⤳ `ThrowCarrier[E, G]`, `F[Unit]` ⤳ `F`). [[RegionCarrier.Unspelled]] for a type with
    * no arguments at all, which cannot be carrying anything.
    */
  private def dropPayloadArgument(tpe: Sourced[OperatorResolvedExpression]): RegionCarrier = tpe.value match {
    case FunctionApplication(target, _) => RegionCarrier.Spelled(target)
    case _                              => RegionCarrier.Unspelled
  }

  /** Whether a definition's declared return is headed by a platform *run carrier* (`def main: IO[Unit]`, the
    * nominal-run spelling) — the head read off the registered run boundaries, per [[RowChecker.runCarrierHeads]].
    */
  private def runCarrierReturn(view: SignatureView, universe: RowChecker.Universe): Boolean =
    spine(view.returnType.value)._1 match {
      case ValueReference(name, _) => RowChecker.runCarrierHeads(universe).contains(name.value)
      case _                       => false
    }

  /** Re-attach the peeled parameter binders around the elaborated body. */
  private def rewrap(original: OperatorResolvedExpression, newBody: Sourced[OperatorResolvedExpression]): OperatorResolvedExpression =
    original match {
      case FunctionLiteral(name, tpe, body) => FunctionLiteral(name, tpe, body.as(rewrap(body.value, newBody)))
      case _                                => newBody.value
    }

  /** One definition's elaboration pass: carries the declared parameter types and the definition's own carrier
    * binders (the shape reads behind carrier-valued-ness) and mints the `$row$N` binders. The positional `region`
    * flag (does the enclosing region have a carrier to bind on) travels as a parameter — it decides
    * bind-vs-`runId` for carrier-valued nodes at value positions and flips inside pinned/run-boundary captures.
    */
  private final class Elaboration(
      paramTypes: Map[String, OperatorResolvedExpression],
      carrierParams: Set[String],
      pinnedParams: Set[String],
      paramRows: Map[String, RowChecker.Row],
      ownBinders: Set[String],
      ownGenerics: Set[String],
      universe: RowChecker.Universe,
      ambientAbilities: Set[AbilityFQN],
      violations: mutable.Buffer[Violation]
  ) {
    private var nextBinder = 0

    /** The binders currently in scope that hold a **carrier computation** rather than a plain value: a declared
      * *suspended* parameter (typed by one of this definition's own carrier binders, `whenTrue: {G} A`), and a block
      * binder the user annotated with a carrier type (the stored-computation `val stored: IO[String] = readLine`).
      *
      * A parameter of *concrete* carrier type is deliberately absent: inside a carrier's own implementation
      * (`implement Effect[IO]`'s `fa: IO[A]`) such a parameter is ordinary data being taken apart, and binding it
      * would rewrite the very machinery elaboration emits.
      */
    private var carrierBinders: Set[String] = carrierParams

    /** The binders known to hold a **plain payload by construction**: a block binder the elaborator itself bound with
      * its inserted `flatMap` (the bind hands the binder the computation's payload), or one whose bound expression
      * was itself definitely pure or `runId`-unwrapped. This is elaborator-owned information, not inference — which
      * is what entitles [[definitelyPure]] to `pure`-wrap a reference to such a binder, where a binder of unknown
      * status must be deferred (its type only inference knows).
      */
    private var payloadBinders: Set[String] = Set.empty

    /** The expression a hoisted binder holds the payload *of*, for the binders where that expression is itself a
      * declared payload — i.e. the binds came from *inside* the argument (`tag(readLine)`, whose own result is the
      * function value `String -> String`) rather than from the argument being a computation (`readLine`, whose payload
      * is atomic and cannot be applied further).
      *
      * [[payloadBinders]] alone answers only "is a bare reference to it a value?". A binder standing at an **arrow**
      * slot is asked a second question — what it yields after one *more* application — and that is what
      * [[expressionKind]] reads off `paramTypes`, where an elaborator-minted binder has no entry. Without this the
      * binder comes out [[Kind.Unknown]] the moment it lands anywhere but a bare position, the pure-wrap around the
      * hoisted core is skipped, and the checker solves the callee's rowless generic at a computation to make the
      * inserted `flatMap` fit (`Expected: IO(String -> IO(String))`) — a §1 rule-4 violation created by this pass,
      * after [[recordRowlessSlots]] has already accepted its output.
      *
      * The elaborator does not need to infer that type: the binder holds the payload of the expression it hoisted, so
      * the expression's own declaration answers for it. Storing the source rather than a reconstructed type is what
      * keeps this a declaration read and reuses [[expressionKind]] unchanged.
      */
    private var payloadBinderSources: Map[String, OperatorResolvedExpression] = Map.empty

    /** Whether the elaboration just performed left a **deferred node at a carrier position** — a node that is
      * neither carrier-valued nor definitely pure where a computation is expected. A rewrite that would place such a
      * node as the tail of an *inserted* bind chain must not happen: the checker would meet the node's flex result
      * against the machinery's declared carrier slot and commit it by first-contact unification (`resultValue(r)`'s
      * bare `A` becoming the carrier — a miscompile). The flag lets the insertion site detect this and defer the
      * whole rewrite instead (A.8.6: the elaborator only rewrites what it can fully discharge).
      */
    private var deferredAtCarrier: Boolean = false

    /** The effects the **current region's** carrier provides — the definition's own declared row, widened inside a
      * capture. It is what [[performs]] measures against: an effect this carrier provides is work to sequence here,
      * while one it does not (and a discharger in scope can consume) belongs to a carrier stack of its own and is
      * data to pass on (`RowChecker.capturedByStack`).
      *
      * The widening is the row half of [[RegionCarrier]]'s flip: a *pinned* slot's capture runs on the callee's
      * stack, which provides exactly the entries that slot pins; a *run-boundary* argument captures the whole row,
      * so nothing is subtracted there at all.
      */
    private var regionRow: Set[AbilityFQN] = ambientAbilities

    /** What the carrier of a capture at `index` provides: the entries that slot pins, or — at a *run boundary*,
      * which captures the whole row — everything a discharger in scope could consume, so that nothing is read as
      * belonging to a stack of its own inside it.
      */
    private def capturedRowAt(callee: Option[OperatorResolvedValue], index: Int, runBoundary: Boolean): Set[AbilityFQN] =
      if (runBoundary) dischargeableAbilities
      else
        callee
          .flatMap(_.effectRow.pinnedParameterEffects.find(_.parameterIndex == index))
          .map(_.effects.map(_.abilityFQN).toSet)
          .getOrElse(Set.empty)

    /** Elaborate a capture on a region whose carrier provides `provided` beyond the ambient. */
    private def withCapturedRow[A](provided: Set[AbilityFQN])(body: => A): A = {
      val outer = regionRow
      regionRow = regionRow ++ provided
      try body
      finally regionRow = outer
    }

    /** Elaborate a lambda's body under its own binders, classified from the **declared domains** of the slot the
      * lambda occupies (§1 rule 4, A.11.7-T step 1 — the binder half A.11.7-U measured into existence).
      *
      * A lambda at a slot declared `A => {Effect} B` binds `A`, a plain generic, which a caller may not instantiate at
      * a computation; so a reference to that binder is a payload *by declaration* and the body `pure`-wraps. Without
      * this the desugar could not prove the identity lambda `s -> s` pure, deferred it, and let unification solve
      * `A := IO[String]` — silently turning `o.foldOption("<none>", s -> s)` into a type error. A carrier-typed domain
      * is the mirror case: the binder holds a computation unrun. A domain the desugar cannot read as either — an
      * applied type, which may be a concrete carrier stack it has no way to name — leaves the binder unclassified,
      * which is the fail-safe direction.
      */
    private def withLambdaBinders[A](
        names: Seq[String],
        domains: Seq[OperatorResolvedExpression],
        carriers: Set[String]
    )(body: => A): A =
      names.headOption match {
        case None       => body
        case Some(name) =>
          val domain = domains.headOption
          withBinder(
            name,
            holdsCarrier = domain.exists(carrierHeaded(_, carriers)),
            isPayload = domain.exists(d => spine(d)._2.isEmpty && !carrierHeaded(d, carriers))
          )(withLambdaBinders(names.tail, domains.drop(1), carriers)(body))
      }

    /** Elaborate under a block binder, which shadows any same-named parameter. */
    private def withBinder[A](name: String, holdsCarrier: Boolean, isPayload: Boolean = false)(body: => A): A = {
      val outerCarrier = carrierBinders
      val outerPayload = payloadBinders
      val outerSources = payloadBinderSources
      carrierBinders = if (holdsCarrier) carrierBinders + name else carrierBinders - name
      payloadBinders = if (isPayload) payloadBinders + name else payloadBinders - name
      // A block/lambda binder has no hoisted source of its own, and it shadows any same-named one.
      payloadBinderSources = payloadBinderSources - name
      try body
      finally {
        carrierBinders = outerCarrier
        payloadBinders = outerPayload
        payloadBinderSources = outerSources
      }
    }

    /** Whether a declared type is carrier-shaped: headed by one of this definition's own carrier binders, or by a
      * platform run carrier (`IO[String]`, read off the boundary registry — never guessed from the name).
      */
    private def carrierShaped(tpe: OperatorResolvedExpression): Boolean =
      carrierHeaded(tpe, ownBinders) || runCarrierHead(tpe)

    /** Elaborate `expr` for a position of known polarity: a carrier position (`needCarrier`) `pure`-wraps a pure
      * node; a value position unwraps a **discharging** node with `runId` — reached only in a region with no carrier
      * (a pure definition's boundary), where the residual carrier is `Id` by declaration (A.4).
      */
    def elaborate(
        expr: Sourced[OperatorResolvedExpression],
        needCarrier: Boolean,
        region: RegionCarrier
    ): Sourced[OperatorResolvedExpression] = {
      val boundary                    = if (needCarrier || region.exists) RegionCarrier.Absent else pureBoundaryRegion(expr)
      val (elaborated, carrierValued) = core(expr, if (boundary.exists) boundary else region)
      if (needCarrier && !carrierValued) {
        if (definitelyPure(elaborated)) elaborated.as(pureWrap(elaborated, region))
        else {
          deferredAtCarrier = true
          elaborated
        }
      } else if (!needCarrier && carrierValued && boundary.exists) elaborated.as(runIdWrap(elaborated))
      else elaborated
    }

    /** The carrier of a **value position in a region with no carrier** — a pure definition's body, a `val` binding —
      * where the expression standing there is nevertheless a computation (A.4).
      *
      * These are the only two boundaries at which this pass names `Id`, and the question they ask is whether anything
      * *else* can name the computation's carrier. Two declared facts answer it, both read off the callee before
      * elaborating: the call is carrier-valued (its result rides one of the callee's own carrier binders), and that
      * binder declares **no user effect** — only the machinery `Effect`/`Suspend`. A discharger (`else`, `runAbort`,
      * `catch`) and the machinery itself (`fold`'s suspended arms, `pure`) are exactly that: nothing selects their
      * carrier by instance, and this position cannot supply one, so it is `Id` by declaration and the node is
      * unwrapped with `runId`.
      *
      * A callee whose carrier *does* declare an effect is left alone — a constructor-class use (`def f: Box[String] =
      * wrap(s)`, `Container[F]`) has its carrier chosen by instance resolution from the declared return, and a genuine
      * leak (`def echo: String = printLine(readLine)`) must stay a leak rather than be quietly run on `Id`, which
      * carries no `Suspend` and could not run it anyway.

      *
      * Reading the *original* spine head is what makes this a single pass: at a carrier-less region no bind is hoisted
      * around the call, so the head elaboration produces is the head we inspect here.
      */
    private def pureBoundaryRegion(expr: Sourced[OperatorResolvedExpression]): RegionCarrier = {
      val (head, args) = spine(expr.value)
      head match {
        case ValueReference(name, typeArgs)
            if typeArgs.isEmpty && calleeCarrierValued(name.value, args) && carrierUnconstrained(name.value) =>
          RegionCarrier.Spelled(idTerm(expr))
        case _ => RegionCarrier.Absent
      }
    }

    /** Whether a callee's declared carrier binders constrain **no user effect** — the declared half of
      * [[pureBoundaryRegion]]. True for the machinery (`pure`, `flatMap`, `fold`'s `{Effect}` arms) and for every
      * discharger, whose pinned base carrier is deliberately unconstrained; false for anything declaring an ability on
      * its carrier, whether an effect (`Console`) or a constructor class (`Container`).
      *
      * An **ability method** is excluded outright, machinery or not: its carrier is chosen by *instance resolution*
      * from the expected type — the context — and not by this boundary. That is what makes `def f: Box[String] =
      * wrap(someString)` (`Container[F]`) and `def e: Either[String, String] = pure("hello")` (`Effect[Either[String]]`)
      * the same shape, and both of them not this one. An ability method carries no `paramConstraints` entry for its
      * own ability, so membership is read off the qualifier.
      */
    private def carrierUnconstrained(callee: ValueFQN): Boolean =
      (callee.name.qualifier match {
        case _: Qualifier.Ability => false
        case _                    => true
      }) && universe.lookup(callee).exists { orv =>
        EffectCarriers.declaredEffects(EffectCarriers.declaredCarrierBinders(orv), orv.paramConstraints).isEmpty
      }

    /** Whether a node is a **plain value by declaration** — the precondition for lifting it into a carrier position
      * with `pure`.
      *
      * "Not carrier-valued" is not the same claim: it also covers everything no declaration classifies — a
      * bare-HKT application whose constructor no parameter determines (`wrap(s) : F[A]`), a reference to a binder
      * the desugar did not itself introduce, a callee outside the universe. Those get **no node**: the fail-safe
      * direction, since a missing lift is completed downstream while a spurious one silently changes the program.
      */
    private def definitelyPure(node: Sourced[OperatorResolvedExpression]): Boolean = {
      val (head, args) = spine(node.value)
      head match {
        case _: IntegerLiteral | _: StringLiteral => true
        case FunctionLiteral(_, _, body)          =>
          // A bare lambda is a value; the block desugar's binding (`(x -> rest)(e)`) is as pure as its `rest`.
          args.isEmpty || (args.sizeIs == 1 && definitelyPure(body))
        case ValueReference(name, _)              =>
          callResultKind(name.value, args, 0) === Kind.Payload
        case ParameterReference(name)             =>
          // A binder is decided by its declaration, exactly as [[expressionKind]] decides it — a payload-by-
          // construction block binder, a declared type read through [[typeKind]], or, *called*, its declared
          // codomain: `f(a)` with `f: Function[A, Either[String, B]]` yields a value, and failing to say so leaves
          // it unlifted at a carrier position, which rolls the whole rewrite back.
          expressionKind(node.value, 0) === Kind.Payload
        case _                                    => false
      }
    }

    /** Whether an expression **performs** an effect — its derived row is non-empty — which is what makes it work to
      * *sequence* rather than data to pass on. Carrier-valued-ness alone does not decide that: `pure(x)` and a
      * suspended-parameter reference are carrier-typed *values* with an empty row, and binding them would run a
      * computation that has nothing to run. A `readLine` at the same slot has the row `{Console}` and does run
      * there.
      */
    private def performs(expr: OperatorResolvedExpression): Boolean =
      RowChecker.expressionRow(expr, paramRows, regionRow, universe).nonEmpty

    /** Whether an elaborated node **discharges** a declared row: it is a call to a callee that captures a row in a
      * pinned parameter (`catch`, `else`, `runStateToPair`, every `run…`) or to a platform run boundary.
      *
      * This is what separates the two carrier-valued nodes that look alike at a pure boundary. A discharging call has
      * *consumed* the row its argument declared, so what remains is a computation over a carrier nothing else
      * constrains — `Id` by declaration, unwrapped with `runId` (A.4). A merely carrier-*returning* call (a
      * constructor-class ability method, `wrap(s) : F[String]`, or any effect performed under a pure return) has
      * discharged nothing: its carrier is whatever the context instantiates it at, so the elaborator writes nothing
      * and ordinary unification decides — either the declared return hosts the carrier (`def f: Box[String] =
      * wrap(s)`) or the program is an undeclared-effect leak the row check reports.
      *
      * The same fact also makes a discharging node *work to sequence* even though its row is empty (the capture
      * consumed it): `printLine(failing catch handler)` binds the discharged computation like any effectful
      * argument.
      */
    private def discharges(node: Sourced[OperatorResolvedExpression]): Boolean =
      spine(node.value)._1 match {
        case ValueReference(name, _) => dischargingCallee(name.value)
        case _                       => false
      }

    /** Whether an elaborated node is a **bind chain this pass inserted** — work by construction, whatever the original
      * expression's own row said.
      *
      * A discharge nested inside an argument (`printLine(foldEither(e -> e, s -> s, runThrow(bad)))`) is hoisted while
      * that argument is elaborated, which leaves the argument itself a `flatMap` even though the *original*
      * expression neither performs (the capture consumed its row) nor is headed by a discharger. Rule 1 places the
      * bind at the enclosing **region**, not at the innermost call, so the chain has to keep rising until it reaches
      * a position that can hold a computation.
      */
    private def sequences(node: Sourced[OperatorResolvedExpression]): Boolean =
      spine(node.value)._1 match {
        case ValueReference(name, _) => name.value == WellKnownTypes.effectFlatMapFQN
        case _                       => false
      }

    private def dischargingCallee(callee: ValueFQN): Boolean =
      universe.runBoundaries.contains(callee) ||
        universe.lookup(callee).exists(_.effectRow.pinnedParameterIndices.nonEmpty)

    /** Elaborate a node, returning it with its carrier-valued-ness (is the *result node* a carrier computation — a
      * bind chain, an effectful or discharging call, a suspended-parameter reference — as opposed to a plain value).
      */
    private def core(
        expr: Sourced[OperatorResolvedExpression],
        region: RegionCarrier
    ): (Sourced[OperatorResolvedExpression], Boolean) = {
      val (head, args) = spine(expr.value)
      head match {
        case FunctionLiteral(name, tpe, body) if args.sizeIs == 1 =>
          // The block desugar's binding: `val x = e; rest` / `e; rest` as `(x -> rest)(e)`.
          val bound                     = args.head
          // The binding is the second of the two `Id` boundaries: with no region carrier, a bound computation whose
          // carrier nothing else names runs on `Id` and is unwrapped there (see [[pureBoundaryRegion]]).
          val boundBoundary             = if (region.exists) RegionCarrier.Absent else pureBoundaryRegion(bound)
          val (boundElab, boundCarrier) = core(bound, if (boundBoundary.exists) boundBoundary else region)
          val storedCarrier             = tpe.exists(t => carrierShaped(t.value))
          if (storedCarrier) {
            // An explicitly carrier-*annotated* binder (`val stored: IO[String] = readLine`) **stores** the
            // computation instead of running it: the annotation is the user saying the binder holds the carrier, so
            // nothing binds here and every reference to it is a carrier value.
            val (bodyElab, bodyCar) = withBinder(name.value, holdsCarrier = true)(core(body, region))
            (expr.as(applyChain(expr.as(FunctionLiteral(name, tpe, bodyElab)), Seq(boundElab))), bodyCar)
          } else {
            val bound2 = Option.when(boundCarrier && region.exists) {
              // Attempt the bind rewrite. The inserted bind hands the binder the computation's *payload* — payload
              // by construction, so a later reference to it may be `pure`-lifted. But if the continuation's eventual
              // tail turns out deferred, the rewrite must not stand — an inserted chain may not end in a node the
              // checker would first-contact-unify against the machinery's carrier slot — so the attempt is rolled
              // back and the whole binding deferred to the checker instead.
              val savedFlag   = deferredAtCarrier
              val savedBinder = nextBinder
              deferredAtCarrier = false
              val continuationBody =
                withBinder(name.value, holdsCarrier = false, isPayload = true)(
                  elaborate(body, needCarrier = true, region)
                )
              val fullyDischarged  = !deferredAtCarrier
              deferredAtCarrier = savedFlag
              if (fullyDischarged) {
                val continuation = expr.as(FunctionLiteral(name, tpe, continuationBody))
                Some((expr.as(bindNodes(continuation, boundElab, region)), true))
              } else {
                nextBinder = savedBinder
                None
              }
            }.flatten
            bound2.getOrElse {
              // No bind inserted (no region carrier, a plain-value bound, or a rolled-back attempt). With no region
              // carrier an unconstrained binding's base is `Id` — unwrap at the binding; any other carrier-valued
              // node keeps its context-supplied carrier (see [[pureBoundaryRegion]]).
              val discharged          = boundCarrier && boundBoundary.exists
              val boundFinal          = if (discharged) boundElab.as(runIdWrap(boundElab)) else boundElab
              val (bodyElab, bodyCar) =
                withBinder(name.value, holdsCarrier = false, isPayload = discharged || definitelyPure(boundFinal))(
                  core(body, region)
                )
              val rebuilt             =
                if ((bodyElab eq body) && (boundFinal eq bound)) expr
                else expr.as(applyChain(expr.as(FunctionLiteral(name, tpe, bodyElab)), Seq(boundFinal)))
              (rebuilt, bodyCar)
            }
          }
        case _: FunctionLiteral if args.isEmpty                   =>
          // A bare lambda in a non-slot position: elaborate its body naturally; the lambda itself is a value.
          (elaborateLambdaNatural(expr, region, Seq.empty, Set.empty)._1, false)
        case ValueReference(name, _) if args.nonEmpty             =>
          elaborateCall(expr, name.value, args, region)
        case ValueReference(name, _)                              =>
          // A nullary reference (`readLine`, `state`, `abort`) is a call like any other: if its result rides its own
          // first binder, the region's carrier is written there too.
          val discharging = dischargingCallee(name.value)
          (
            writeTypeArguments(expr, name.value, Seq.empty, callRegion(region, discharging)),
            calleeCarrierValued(name.value, Seq.empty)
          )
        case ParameterReference(name) if args.isEmpty             =>
          // A suspended parameter (declared carrier-headed) holds its computation unrun: referencing it yields a
          // carrier value.
          (expr, carrierBinders.contains(name.value))
        case ParameterReference(name)                             =>
          // Calling a function-typed parameter: carrier-valued when its declared arrow's final codomain is
          // carrier-headed and the call saturates it (`action(s)` inside a callback-taking definition); its result
          // is classifiable when that codomain is a carrier or a declared payload.
          val resultCarrier    = paramCallCarrier(name.value, args.size)
          val resultClassified = resultCarrier || expressionKind(expr.value, 0) === Kind.Payload
          elaborateArguments(expr, args, resultCarrier, resultClassified, region)
        case _                                                    =>
          (expr, false)
      }
    }

    /** Elaborate a call to a named value: arguments by their declared slot mode, then hoist each carrier-valued
      * strict-slot argument into a `flatMap` around the core call (leftmost argument outermost). Every slot the
      * callee does not declare as suspended, pinned or a run boundary is **strict** — a plain generic included, since
      * rule 4 makes it a payload — and the core call's kind is read off [[callResultKind]], before the hoist from the
      * arguments as written and after it from the payload binders they became.
      */
    private def elaborateCall(
        expr: Sourced[OperatorResolvedExpression],
        callee: ValueFQN,
        args: Seq[Sourced[OperatorResolvedExpression]],
        region: RegionCarrier
    ): (Sourced[OperatorResolvedExpression], Boolean) = {
      val calleeOrv     = universe.lookup(callee)
      val calleeView    = calleeOrv.map(o => SignatureView.of(o.signature))
      val pinned        = calleeOrv.map(_.effectRow.pinnedParameterIndices).getOrElse(Set.empty)
      // The parameters the callee declared with an **open row** (`{Effect} A`, `A => {Effect} B`), read off the tag
      // the desugar records (source (i)). This is what separates "a value or a computation" from "a computation on
      // this carrier" (§1 rule 2): both are `F[A]` after desugaring, so only the tag can tell them apart, and a pure
      // actual lifts into the first and is a type error at the second.
      val rowTagged     = calleeOrv.map(_.effectRow.parameterEffects.map(_.parameterIndex).toSet).getOrElse(Set.empty)
      val runBoundary   = universe.runBoundaries.contains(callee)
      val calleeBinders = calleeOrv.map(EffectCarriers.declaredCarrierBinders).getOrElse(Set.empty)
      // §1 rule 4, third bullet: `{Effect}` is a *row variable*, and this call may instantiate it at the **empty
      // row** — `dependency.url` and `items.foreach(printLine)` use the same declaration of `.`. The empty row's
      // value is `Id`, so such a call runs on `Id` whatever the enclosing region is, and its result is projected
      // back to a plain value (see [[emptyRowCall]]).
      val emptyRow      = writableReference(expr) && emptyRowCall(callee, args)
      // Whether this call's own carrier is known to exist: the enclosing region has one, or the call discharges a
      // row and so runs on `Id`. When neither holds the carrier is context-supplied and its slots stay ordinary —
      // lifting a pure argument with `pure` would force `Id` on a call whose carrier the use site chooses.
      val ownRegion     =
        if (emptyRow) RegionCarrier.Spelled(idTerm(expr)) else callRegion(region, pinned.nonEmpty || runBoundary)
      val slotRegion    = ownRegion.exists
      // Hoisting is a rewrite the elaborator must fully discharge: the core it leaves as the chain's innermost
      // continuation must be classifiable — carrier-valued, or a payload result it can `pure`-wrap. An unclassified
      // core may *be* the computation, so hoisting around it is dropped: no binds, the checker elaborates the whole
      // call.
      val coreClassified = callResultKind(callee, args, 0) =!= Kind.Unknown
      val hoist          = region.exists && coreClassified
      // The carrier region a *pinned or run-boundary* argument captures on: the callee's own stack. The run
      // boundary's is concrete and writable (`IO`, read off its declared domain); a pinned stack's is spelled in
      // the callee's binders (`{Throw[E] | G} A`), so it exists but stays unspelled (see [[RegionCarrier]]).
      val captureRegion  =
        if (runBoundary) calleeView.flatMap(_.parameters.headOption).map(dropPayloadArgument).getOrElse(RegionCarrier.Unspelled)
        else RegionCarrier.Unspelled

      val (finalArgs, binds) =
        args.zipWithIndex.foldLeft(
          (Seq.empty[Sourced[OperatorResolvedExpression]], Seq.empty[(String, Sourced[OperatorResolvedExpression])])
        ) { case ((accArgs, accBinds), (arg, index)) =>
          val declaredSlot = calleeView.flatMap(_.parameters.lift(index)).map(_.value)
          if (pinned.contains(index) || (runBoundary && index == 0)) {
            // A pinned/run-boundary slot captures the computation whole — and is its own carrier region: internal
            // binds ride the pinned/run stack even under a pure definition. The boundary itself is not wrapped:
            // pinned means *captured*, and a pure actual does not lift into a capture — the val-bound-discharge
            // limitation's mismatch ("Expected: {Abort | IO} String") is by design, and wrapping would silently
            // replace that curated diagnostic with a downstream ability demand.
            val captured = withCapturedRow(capturedRowAt(calleeOrv, index, runBoundary))(core(arg, captureRegion)._1)
            (accArgs :+ captured, accBinds)
          } else if (declaredSlot.exists(s => carrierHeaded(s, calleeBinders))) {
            // A carrier-headed slot receives a computation on the *call's own* carrier, and an effectful argument
            // passes unrun. Whether a *pure* argument is lifted into it is decided by the slot's **row tag**, not by
            // its type (§1 rule 2, decided 2026-07-28):
            //
            //   - a **row** slot (`whenTrue: {Effect} A`, `value: {Abort} T`) means "a value or a computation" — a row
            //     can be empty — so a pure actual lifts (`if(c, "a")` ⇒ `pure("a")`);
            //   - a **carrier-typed** slot (`x: G[A]`, hand-written) means "a computation on G". A pure `A` is not a
            //     `G[A]`, so it is a type error, not a lift. The two are the same shape here — a row desugars to
            //     `F[A]` — which is why the decision reads the tag `EffectRow.parameterEffects` records at the
            //     desugar (source (i)) and never the shape.
            //
            // Before that decision every `~ Effect`-constrained carrier slot lifted, which made the calling
            // convention depend on a constraint the callee declares for its own body's sake: adding `~ Effect` to
            // `def hold[G[_]](x: G[String])` silently changed what callers could pass, and without it the same call
            // died in the quoter ("contains unresolved variable") instead of reporting a mismatch.
            //
            // The lift reads `ownRegion` (the enclosing region's carrier, `Id` at the empty row) rather than the
            // caller's. With no carrier of its own (`slotRegion`) the call's carrier is context-supplied, so the slot
            // takes whatever it is given — lifting there would force `Id` on a carrier the use site chooses.
            val suspended =
              if (!slotRegion) core(arg, region)._1
              else if (rowTagged.contains(index)) elaborate(arg, needCarrier = true, ownRegion)
              else {
                // The dual of the rowless-slot violation below, and it needs its own diagnostic for the same reason:
                // left to the checker, a plain value against `?G[A]` postpones, defaults, and dies in the quoter as
                // "Cannot resolve type — contains unresolved variable", which names neither the slot nor the fix.
                val captured = core(arg, ownRegion)._1
                if (definitelyPure(captured))
                  violations += Violation(
                    arg.as(
                      s"This argument is a plain value, but argument ${index + 1} of '${callee.name.name}' declares " +
                        s"a computation on its own carrier."
                    ),
                    RowElaborator.carrierSlotHelp
                  )
                captured
              }
            (accArgs :+ suspended, accBinds)
          } else if (declaredSlot.flatMap(concreteCarrierHead).isDefined) {
            // A slot declaring a **concrete carrier** — `runId`'s `obj: Id[A]`, or a `data` field holding a
            // computation (`data Box(action: IO[Unit])`) — hosts the computation, and it runs on *that* carrier
            // rather than on this region's. So it is a capture like a pinned slot (§1 rule 4, last bullet): nothing
            // is hoisted out of it, and — since §1 rule 2 — nothing is lifted into it either. A concrete carrier is
            // a *type*: `Box("pure")` is a type error, exactly as the pinned spelling of the same field already was.
            //
            // Hoisting here is what made an effect stored in a `data` field run at *construction* time while the
            // accessor's run did nothing — the argument's payload went into the field and the v2 carrier router
            // `pure`-wrapped it back to the declared type, so the misplacement type-checked (A.11.7-Y shape 1).
            val carrier  = arg.as(declaredSlot.flatMap(concreteCarrierHead).get)
            val captured = core(arg, RegionCarrier.Spelled(carrier))._1
            // Reported here rather than left to the checker: its one remaining pure-lift arm fires on *any* rigid
            // carrier-headed expectation, and `IO` is the ambient carrier of the definition doing the storing, so
            // `Box("pure")` would type-check by that arm alone — the asymmetry §1 rule 2 exists to remove.
            if (definitelyPure(captured))
              violations += Violation(
                arg.as(
                  s"This argument is a plain value, but argument ${index + 1} of '${callee.name.name}' declares a " +
                    s"computation on a concrete carrier."
                ),
                RowElaborator.carrierSlotHelp
              )
            (accArgs :+ captured, accBinds)
          } else if (declaredSlot.exists(s => carrierCodomain(s, calleeBinders)) && !isBareLambda(arg.value) &&
            rowTagged.contains(index)) {
            // A **non-lambda** argument at a carrier-codomain slot (`f: A => {Effect} B` given `url`, given
            // `take(indexOf("=", s))`, every non-lambda dot). §1 rule 4: the slot's declared type is an arrow, headed
            // by `Function` and not by a carrier, so it is a *rowless value position* — the row sits on the
            // **codomain**, which describes what applying the function later does, not how the function value is
            // produced. So the argument is elaborated **strictly, in the enclosing region**, exactly like any other
            // rowless slot (§1 rule 1: an effect performed while *building* it runs where it is written, not under a
            // binder this pass invents), and only the resulting *value* is adapted to the declared codomain.
            //
            // The region boundary is the **lambda**, not the slot: a bare lambda takes the branch below, where its
            // body is the region the codomain governs. There is no lambda here, so there is nothing to defer.
            //
            // Adapting means lifting *pointwise* when the value is a pure function — the only spelling that fits,
            // since the lift has to happen under the arrow. No value restriction is needed for the eta-expansion:
            // anything that performs has already been hoisted out by [[strictSlot]], so what reaches it is a binder
            // or pure code, and duplicating pure code is unobservable.
            val (strictArg, bind) = strictSlot(arg, region, hoist)
            val adapted           =
              if (pureFunctionAt(declaredSlot.get, strictArg.value)) {
                val domains = arrowChainLike(declaredSlot.get)._1
                etaPureLift(strictArg, domains.size, ownRegion, domains, calleeBinders)
              } else strictArg
            (accArgs :+ adapted, accBinds ++ bind)
          } else if (isBareLambda(arg.value)) {
            val domains = declaredSlot.map(s => arrowChainLike(s)._1).getOrElse(Seq.empty)
            if (declaredSlot.exists(s => carrierCodomain(s, calleeBinders))) {
              // A handler/callback at a carrier-codomain slot (`onError: E => {Effect} A`, `action: A => {Effect}
              // Unit`): its body is a carrier region, so its own effects chain there rather than hoisting out. Whether
              // a *pure* body is lifted via `pure` is the slot's row tag again (§1 rule 2, the same reading as the
              // carrier-headed slot above: a row admits the empty row, a bare `G[A]` codomain does not). The
              // codomain's carrier is the call's own result carrier, so the body inherits it when we can spell it,
              // and merely *has* one otherwise.
              val handler =
                if (rowTagged.contains(index)) elaborateLambdaForced(arg, forced(ownRegion), domains, calleeBinders)
                else elaborateLambdaNatural(arg, forced(ownRegion), domains, calleeBinders)._1
              (accArgs :+ handler, accBinds)
            } else {
              // A lambda at a plain arrow slot elaborates naturally: an effectful body becomes a bind chain, a pure
              // body is untouched. What its codomain instantiates the slot at is the checker's to discover.
              (accArgs :+ elaborateLambdaNatural(arg, region, domains, calleeBinders)._1, accBinds)
            }
          } else {
            // Every remaining slot is **strict** — a declared-concrete one, an unknown callee's, and (since A.11.5)
            // a *generic-headed* one (`a: A`, `initial: B`, `x: F[A]` with a bare `F[_]`). The latter used to be
            // deferred for want of a declared mode (A.8.6), which is what kept the checker's whole
            // obligation/resolver path alive. It needs no mode of its own now that the carrier is *written*: an
            // argument that performs on this region's carrier is work to sequence here and hoists, and one carrying
            // a different carrier — the dot-chained discharger's `rename("after")` under a `{Console}` ambient,
            // which [[carrierAt]] gives `StateCarrier[String, F]` — does not perform on the ambient at all and so
            // passes as data, captured rather than run. §1 rule 1 restored: effects run where they are written.
            strictArgument(accArgs, accBinds, arg, region, hoist)
          }
        }

      calleeView.foreach(view => recordRowlessSlots(callee, view, finalArgs, calleeBinders, pinned, runBoundary))

      // Hoisting *changes the instantiation*: `choose(readLine, readLine)` has computations at both slots before the
      // rewrite and payload binders at both after it, so the core call's own result goes from carrier to payload and
      // must be `pure`-wrapped as the chain's innermost continuation. Re-reading the kind off `finalArgs` is what
      // makes rule 1's canonical example ("both reads run") come out with the payload the slots then hold.
      val carried       =
        if (binds.isEmpty) declaredResultKind(callee, args, 0) === Kind.Carrier
        else declaredResultKind(callee, finalArgs, 0) === Kind.Carrier
      // A call at the empty row yields `Id[B]` where the context wants `B`, so it is projected with `runId` — the
      // written `Id` and its projection are erased together downstream, which is what "the empty row produces no
      // node" means once `Id` is its value (A.11.7-S).
      val projectId     = emptyRow && carried
      val resultCarrier = carried && !projectId
      val head          = writeTypeArguments(expr.as(spine(expr.value)._1), callee, args, ownRegion)
      (assemble(expr, head, finalArgs, binds, resultCarrier, region, projectId), binds.nonEmpty || resultCarrier)
    }

    /** The carrier a call's *own* result runs on: the enclosing region's when there is one; otherwise a
      * **discharging** call still has one (the row it consumed left a residual), but nothing here names it.
      *
      * It is tempting to call that residual `Id` — A.4 says an *unconstrained* residual is `Id` by declaration — and
      * that is wrong outside the two boundaries where this pass inserts `runId`. The residual is unconstrained only
      * where the surrounding position cannot host a carrier at all; a discharging call feeding an enclosing
      * computation has its carrier fixed by that computation instead. `Effect[AbortCarrier[G]]`'s own `flatMap` is
      * the witness: `runAbort(fa)` discharges under a return this pass reads as carrier-less (`AbortCarrier[G, B]`
      * is headed by the *instance's* binder, not the method's), yet its carrier is plainly `G`, not `Id`.
      */
    private def callRegion(enclosing: RegionCarrier, discharging: Boolean): RegionCarrier =
      if (enclosing.exists) enclosing
      else if (discharging) RegionCarrier.Unspelled
      else RegionCarrier.Absent

    /** A region known to *have* a carrier by declaration even where this pass cannot name it — a carrier-codomain
      * lambda body, whose slot declares the carrier whatever the context instantiates it at.
      */
    private def forced(region: RegionCarrier): RegionCarrier =
      if (region.exists) region else RegionCarrier.Unspelled

    /** Write the type arguments this call's *declarations determine*, as an explicit leading prefix (§3.1) —
      * `printLine[F](s)`, `readLine[F]`, `runAbort[Id](x)`, `catch[String](bad, h)` — so the checker never mints a
      * metavariable where a declaration already says what belongs there. The reference is returned unchanged wherever
      * nothing is determined, which is always the fail-safe direction: an unwritten argument is simply inferred,
      * exactly as before this step.
      *
      * Two sources determine a binder, both read off declarations:
      *
      *   - the **region** supplies the carrier ([[carrierAt]]) at a binder the call's result rides
      *     ([[ridesFirstBinder]]);
      *   - a **pinned parameter** supplies its row's ability arguments from the captured argument's own declared row
      *     ([[pinnedDetermination]]) — `catch`'s `computation: {Throw[E] | G} A` against `bad`'s declared
      *     `{Throw[String]}` determines `E := String`.
      *
      * Writing stops at the first binder nothing determines, because `ValueReference.typeArgs` applies positionally:
      * a prefix is all that can be written, and a binder left open is left inferred. That is why `catch[E, G[_] ~
      * Effect, A]` comes out as `catch[String]` — `E` is determined by the capture, and `G` is the pinned row's
      * *residual*, which the capture decides rather than this call's declarations (writing the region's carrier
      * there would be an assumption, not a determination; `else`'s carrier *is* its binder 0 and is written by the
      * first source, unchanged).
      *
      * Before this became a prefix write only the carrier was written, and only at binder 0. That restriction is
      * what left `catch`'s `E` to junk-ground to `Type` inside the checker, which the v2 bridge then had to pin back
      * from the argument's row constraints (docs/effects-as-rows.md A.11.7-Y shape 3).
      */
    private def writeTypeArguments(
        reference: Sourced[OperatorResolvedExpression],
        callee: ValueFQN,
        args: Seq[Sourced[OperatorResolvedExpression]],
        region: RegionCarrier
    ): Sourced[OperatorResolvedExpression] =
      reference.value match {
        case ValueReference(name, existing) if existing.isEmpty =>
          determinedPrefix(callee, args, region) match {
            case Seq()  => reference
            case prefix => reference.as(ValueReference(name, prefix))
          }
        case _                                                  => reference
      }

    /** The leading run of the callee's binders each of which some declaration determines — see [[writeTypeArguments]]
      * for the two sources and for why it is a prefix.
      */
    private def determinedPrefix(
        callee: ValueFQN,
        args: Seq[Sourced[OperatorResolvedExpression]],
        region: RegionCarrier
    ): Seq[Sourced[OperatorResolvedExpression]] =
      universe.lookup(callee).toSeq.flatMap { orv =>
        val carrierTerm =
          region.term.filter(_ => ridesFirstBinder(callee, args.size)).flatMap(carrierAt(callee, _))
        SignatureView
          .of(orv.signature)
          .binders
          .zipWithIndex
          .map { case (binder, index) =>
            pinnedDetermination(orv, args, binder.name.value).orElse(if (index === 0) carrierTerm else scala.None)
          }
          .takeWhile(_.isDefined)
          .flatten
      }

    /** What a **pinned parameter** determines about one of the callee's binders: the captured argument's own declared
      * row, matched entry-by-entry against the pinned row the parameter declares.
      *
      * `catch(computation: {Throw[E] | G} A, …)` given `bad : {Throw[String]} String` matches the `Throw` entries and
      * reads `E := String` straight off the argument's declaration. This is the same fact the v2 bridge recovered
      * from the checker's metavariable constraints at capture time (`eagerRowPinIntoDomain`), taken from the
      * declaration instead — where it was all along.
      *
      * Only a *call* argument answers: its callee's declared row is a declaration. Anything else (a parameter
      * reference, a block, a lambda) determines nothing here, and the prefix stops — the fail-safe direction.
      */
    private def pinnedDetermination(
        orv: OperatorResolvedValue,
        args: Seq[Sourced[OperatorResolvedExpression]],
        binderName: String
    ): Option[Sourced[OperatorResolvedExpression]] =
      orv.effectRow.pinnedParameterEffects.view
        .flatMap { pinnedParameter =>
          args.lift(pinnedParameter.parameterIndex).toSeq.flatMap { arg =>
            val supplied = argumentRow(arg)
            pinnedParameter.effects.flatMap { declared =>
              supplied
                .filter(_.abilityFQN == declared.abilityFQN)
                .flatMap(entry => declared.typeArgs.zip(entry.typeArgs))
                .collect {
                  case (ParameterReference(name), determined) if name.value === binderName => arg.as(determined)
                }
            }
          }
        }
        .headOption

    /** The declared row of an argument expression, with its type arguments — available exactly when the argument is a
      * call, whose callee's declaration states it ([[declaredEntries]]).
      */
    private def argumentRow(arg: Sourced[OperatorResolvedExpression]): Seq[ResolvedAbilityConstraint] =
      spine(arg.value)._1 match {
        case ValueReference(name, _) => declaredEntries(name.value)
        case _                       => Seq.empty
      }

    /** The carrier a call to `callee` runs on, given this definition's own ambient carrier (A.11.4-R, Robert's
      * decision).
      *
      * A callee whose declared row the ambient already provides runs on the ambient itself. A callee that needs
      * **more** than the ambient provides cannot be running on it: the extra effects have to be discharged before
      * they reach this definition's boundary, so the call runs on the canonical carrier stack of that difference
      * over the ambient —
      *
      * {{{
      * def main: {Console} Unit                                  ambient F provides {Console}
      * def rename(next: String): {Console, State[String]} Unit    needs {Console, State[String]}
      * ⇒ rename[StateCarrier[String, F]]("after")                 difference {State[String]}
      * }}}
      *
      * This is what makes a dot-chained discharger need no rule of its own. At `.`'s bare-generic slot the call now
      * carries `StateCarrier[String, F]`, so it does *not* perform on the ambient, so rule 1 passes it as data
      * instead of hoisting it — which is exactly right, because a discharger captures its argument rather than
      * running it. The alternative would have been to inspect the sibling argument to find the discharger, which
      * the §3 whitelist prohibits.
      *
      * The rule reads only declared rows, and it is order-free in the sense that matters: the stack's *order* comes
      * from the callee's own declared row, not from anything about the call site. [[scala.None]] — write nothing,
      * the fail-safe direction — whenever the difference contains an effect with no canonical carrier (a
      * `Suspend`-riding one like `Console`, which cannot be spelled as a stack layer at all; in a valid program
      * such a difference is a leak the row check reports).
      */
    private def carrierAt(
        callee: ValueFQN,
        ambient: Sourced[OperatorResolvedExpression]
    ): Option[Sourced[OperatorResolvedExpression]] = {
      val discharged = declaredEntries(callee).filter(entry =>
        !ambientAbilities.contains(entry.abilityFQN) && dischargeableAbilities.contains(entry.abilityFQN)
      )
      if (discharged.isEmpty) Some(ambient)
      else if (discharged.exists(_.typeArgs.exists(hasFreeCalleeBinder(callee, _)))) scala.None
      else
        Some(discharged.foldRight(ambient) { (entry, base) =>
          val carrier = base.as(ValueReference(base.as(EffectCarrierNaming.carrierFQN(entry.abilityFQN))))
          base.as(applyChain(carrier, entry.typeArgs.map(base.as(_)) :+ base))
        })
    }

    /** The effects that are **dischargeable here**: those some signature in this value's universe *pins*.
      *
      * This is the second half of [[carrierAt]]'s filter, and it is what keeps the rule from inventing carriers that
      * do not exist. An effect the ambient does not declare is not automatically discharged onto a layer of its own:
      * a `Suspend`-riding effect (`Console`, `Log`, `Inf`) has no `<Ability>Carrier` at all and is provided by the
      * *base* carrier instead. The synthesized entry `def main: Unit = runMain(HelloWorld::main)` is the witness —
      * it captures a `{Console}` value on `IO`, and a naive difference would "discharge" `Console` onto a
      * `ConsoleCarrier` that does not and cannot exist.
      *
      * Reading it off the universe's pinned rows answers exactly the right question with declared information and no
      * lookup: an effect is dischargeable in this body iff a discharger for it is among the names this body reaches.
      * A body with no discharger in scope captures nothing, so it needs no layer — which is self-consistent rather
      * than merely convenient.
      */
    private lazy val dischargeableAbilities: Set[AbilityFQN] = RowChecker.dischargeableAbilities(universe)

    /** The effect entries a callee's own ambient carrier is constrained by, with their type arguments and in
      * declared order — `{State[String], Console}` as `[State[String], Console]`. The machinery constraints
      * (`Effect`/`Suspend`) are dropped: they are inserted by the compiler, never discharged, and have no carrier
      * layer of their own.
      */
    private def declaredEntries(callee: ValueFQN): Seq[ResolvedAbilityConstraint] =
      universe
        .lookup(callee)
        .toSeq
        .flatMap { orv =>
          EffectCarriers
            .declaredCarrierBinders(orv)
            .toSeq
            .sorted
            .flatMap(binder => orv.paramConstraints.getOrElse(binder, Seq.empty))
        }
        .filterNot(entry => EffectMachinery.isMachineryAbility(entry.abilityFQN.abilityName))
        .map(entry => entry.copy(typeArgs = entry.typeArgs.dropRight(1)))

    /** Whether an effect entry's type argument still mentions one of the *callee's* own binders — a row like
      * `{State[S]}` generic in its state type. Such an entry cannot be spelled at the call site (the instantiation
      * decides `S`), so the whole stack is abandoned rather than half-written.
      */
    private def hasFreeCalleeBinder(callee: ValueFQN, typeArg: OperatorResolvedExpression): Boolean =
      universe.lookup(callee).exists { orv =>
        val binders = SignatureView.of(orv.signature).binders.map(_.name.value).toSet
        binders.exists(binder => OperatorResolvedExpression.containsVar(typeArg, binder))
      }

    /** Whether this call instantiates the callee's declared row variable at the **empty row** (§1 rule 4, third
      * bullet — A.11.7-T step 3).
      *
      * A row-polymorphic signature (`def .[A, B](a: A, f: A => {Effect} B): {Effect} B`) has *one* declaration
      * serving both `dependency.url` and `items.foreach(printLine)`; which of the two a call is, is read off the
      * arguments occupying the positions that mention the carrier — a carrier-headed slot (`whenTrue: {Effect} A`,
      * `initial: {Effect} B`) and an arrow slot whose codomain is carrier-headed (`f: A => {Effect} B`). **Every one
      * of them contributing the empty row** ⇒ the row is empty and the call runs on `Id`.
      *
      * That question is asked of the derivation, not of [[Kind]] — see [[carrierDeterminedBy]] for what the two
      * disagree about and which is right.
      *
      * Two exclusions, each because the row is *not* this call's to decide:
      *
      *   - a callee whose carrier is not its arguments' to settle — an **ability method**, whose carrier comes from
      *     instance resolution, or one that **declares an effect** of its own (`readLine`'s `{Console}`), which no
      *     argument can make empty. Both are exactly [[carrierUnconstrained]], which is why this asks it rather than
      *     restating it;
      *   - a **discharger** (`else`, `catch`, every `run…`), whose carrier is the residual its *capture* leaves and
      *     never what a plain argument says: `host else "localhost"` has a pure fallback at a carrier-headed slot,
      *     and the row is nonetheless whatever the captured computation runs on.
      *
      * And a callee **no argument position mentions the carrier at** (`wrap(s) : F[A]`) settles nothing, where the
      * declared return chooses it: writing `Id` there would decide a carrier the context owns.
      */
    private def emptyRowCall(callee: ValueFQN, args: Seq[Sourced[OperatorResolvedExpression]]): Boolean =
      carrierUnconstrained(callee) && !dischargingCallee(callee) && universe.lookup(callee).exists { orv =>
        val carriers    = EffectCarriers.declaredCarrierBinders(orv)
        val determining =
          SignatureView
            .of(orv.signature)
            .parameters
            .zip(args)
            .flatMap { case (declared, arg) => carrierDeterminedBy(declared.value, arg.value, 0, carriers, Map.empty) }
        carriers.nonEmpty && determining.nonEmpty && determining.forall(_.settlesEmpty)
      }

    /** What one argument settles about the row the callee's carrier binders are instantiated at: a carrier-headed
      * slot asks the argument directly, an arrow slot asks for what it yields after that many applications.
      * Positions that do not mention a carrier settle nothing.
      *
      * A position settles the **empty** row only if it is *both* a payload by [[Kind]] and derives an empty
      * [[RowChecker.argumentRow]] — see [[CarrierDetermination]] for why neither half subsumes the other.
      *
      * `binders` carries the lambda binders peeled on the way down, each at the **empty** row: a lambda at a slot
      * declared `A => {Effect} B` binds a plain generic, which §1 rule 4 forbids instantiating at a computation, so a
      * reference to it performs nothing. Binding them explicitly rather than leaving them absent is what makes a
      * binder *shadow* a same-named row-declaring parameter of the enclosing definition.
      */
    private def carrierDeterminedBy(
        declared: OperatorResolvedExpression,
        arg: OperatorResolvedExpression,
        applied: Int,
        carriers: Set[String],
        binders: Map[String, RowChecker.Row]
    ): Seq[CarrierDetermination] =
      if (carrierHeaded(declared, carriers))
        Seq(
          CarrierDetermination(
            expressionKind(arg, applied),
            RowChecker.argumentRow(arg, paramRows ++ binders, regionRow, universe)
          )
        )
      else
        underArrow(declared, arg, applied, carriers) { (codomain, inner, level, bound) =>
          carrierDeterminedBy(
            codomain,
            inner,
            level,
            carriers,
            bound.fold(binders)(name => binders + (name -> Set.empty))
          )
        }.getOrElse(Seq.empty)

    /** What one carrier-determining position settles about the row its callee's carrier binders are instantiated at.
      *
      * Two readings of the same position, and the empty row needs **both** — they answer different questions and
      * neither subsumes the other (A′ set out to replace the kind with the row; the measurement said conjunction):
      *
      *   - [[kind]] is a property of the *value* the position holds. It is what catches a slot filled by something
      *     that already **is** a computation — `else`'s own `fallback: {Effect} A` handed on to `foldOption`. Its
      *     row derives *empty*, because `{Effect}` is machinery and the derivation filters machinery out: the row
      *     says "nothing I can name", which is not the same claim as "nothing".
      *   - [[row]] is a property of the position's *evaluation*, which is what the row variable is actually
      *     instantiated at. It is what catches a value that is a payload but whose evaluation performs — a lambda
      *     whose body performs (`applyTo(s, x -> take(indexOf("=", s), x))`) is a payload by kind, because `take`
      *     returns a plain `String`. Read by kind alone the call was written at `Id`, and the body's `{Abort}` was
      *     then demanded of `Id`: "No ability implementation found for ability 'Abort' with type arguments [Id]",
      *     reported inside `stdlib`'s own `Bool.els` — a file the user never wrote.
      *
      * The conjunction is also the fail-safe direction: it can only *withhold* `ρ := {}`, moving a call from `Id`
      * onto the ambient carrier, and never the reverse. A withheld empty row costs a `pure` wrap; a wrongly granted
      * one puts an effect on a carrier that cannot perform it.
      */
    private case class CarrierDetermination(kind: Kind, row: RowChecker.Row) {
      def settlesEmpty: Boolean = kind === Kind.Payload && row.isEmpty
    }

    /** Descend one declared arrow level with the argument that fills it, classifying a **lambda's own binder** from
      * the declared domain as it goes.
      *
      * That classification is why this is not a plain recursion: `foldEither(err -> err, v -> v, e)` is read *before*
      * its handlers are elaborated, so `withLambdaBinders` has not run and a reference to `err` would come out
      * unclassified — leaving the row [[Kind.Unknown]] and the call on the ambient carrier when it is plainly pure.
      * Peeling the lambda alongside the arrow consumes an application level on both sides, so `applied` stands still;
      * a non-lambda argument is asked for one more application instead.
      */
    private def underArrow[A](
        declared: OperatorResolvedExpression,
        arg: OperatorResolvedExpression,
        applied: Int,
        carriers: Set[String]
    )(descend: (OperatorResolvedExpression, OperatorResolvedExpression, Int, Option[String]) => A): Option[A] =
      asArrowLike(declared).map { case (domain, codomain) =>
        arg match {
          case FunctionLiteral(name, _, body) =>
            withBinder(
              name.value,
              holdsCarrier = carrierHeaded(domain.value, carriers),
              isPayload = spine(domain.value)._2.isEmpty && !carrierHeaded(domain.value, carriers)
            )(descend(codomain.value, body.value, applied, Some(name.value)))
          case _                              => descend(codomain.value, arg, applied + 1, None)
        }
      }

    /** Whether an argument at an arrow-typed slot is a **function returning a plain value** — the precondition for
      * lifting it pointwise into a carrier-codomain slot.
      */
    private def pureFunctionAt(declared: OperatorResolvedExpression, arg: OperatorResolvedExpression): Boolean = {
      val arity = arrowChainLike(declared)._1.size
      arity > 0 && expressionKind(arg, arity) === Kind.Payload
    }

    /** Lift a pure function `g` into a carrier-codomain slot pointwise: `x1 -> … -> xn -> pure(g(x1, …, xn))`.
      *
      * The eta-expansion is what carries the lift under the arrow — a carrier-codomain slot wants a function whose
      * *result* is a computation, and there is no other place to put the `pure`. The binders are the pass's own
      * `$row$N`, hence payload by construction.
      */
    private def etaPureLift(
        arg: Sourced[OperatorResolvedExpression],
        arity: Int,
        region: RegionCarrier,
        domains: Seq[OperatorResolvedExpression],
        carriers: Set[String]
    ): Sourced[OperatorResolvedExpression] = {
      val binders = Seq.fill(arity)(freshBinder())
      val applied = arg.as(applyChain(arg, binders.map(binder => arg.as(ParameterReference(arg.as(binder))))))
      val eta     = binders.foldRight(applied)((binder, body) => arg.as(FunctionLiteral(arg.as(binder), None, body)))
      // Elaborated as the lambda it now is, by exactly the path a hand-written one takes: the body is a carrier
      // position, so it lifts if it is a value and passes through if it already is a computation.
      elaborateLambdaForced(eta, forced(region), domains, carriers)
    }

    /** Whether the call's own carrier is this pass's to decide: a reference carrying **explicit type arguments** has
      * had it decided by whoever wrote them, and writing `Id` (or projecting with `runId`) over that would double up
      * on hand-written monadic code — the same reason [[writeTypeArguments]] leaves such a reference alone.
      */
    private def writableReference(expr: Sourced[OperatorResolvedExpression]): Boolean =
      spine(expr.value)._1 match {
        case ValueReference(_, typeArgs) => typeArgs.isEmpty
        case _                           => true
      }

    /** The `Id` carrier as a writable type argument — the value of the empty row (§1 rule 4). */
    private def idTerm(at: Sourced[OperatorResolvedExpression]): Sourced[OperatorResolvedExpression] =
      at.as(ValueReference(at.as(WellKnownTypes.idFQN)))

    /** Whether a declared type is applied `Id` — the one *concrete* carrier this pass names, and therefore the one it
      * can recognise in a declaration.
      */
    private def idHeaded(tpe: OperatorResolvedExpression): Boolean =
      spine(tpe) match {
        case (ValueReference(name, _), args) => name.value == WellKnownTypes.idFQN && args.nonEmpty
        case _                               => false
      }

    /** The head of a declared type that is a **concrete carrier applied to its payload** — `Id[A]` (`runId`'s `obj`)
      * or a platform run carrier (`IO[Unit]`, a `data` field holding a computation) — as a writable term.
      *
      * This is the third way a slot can be carrier-headed, beside a pinned row and one of the callee's own carrier
      * binders, and §1 rule 4's last bullet says all of them mean the same thing: the slot **hosts the computation**,
      * so the argument is captured rather than run here. Both recognitions are the sanctioned declared tags — `Id` is
      * compiler-owned, a run carrier comes off the run-boundary registry — never a name or shape guess about an
      * arbitrary type.
      */
    private def concreteCarrierHead(tpe: OperatorResolvedExpression): Option[OperatorResolvedExpression] =
      spine(tpe) match {
        case (head @ ValueReference(name, _), args)
            if args.nonEmpty && (name.value == WellKnownTypes.idFQN || runCarrierHead(tpe)) =>
          Some(head)
        case _ => scala.None
      }

    /** Whether a call to `callee` **rides its first generic binder**, that binder being one of its declared carriers:
      * the declared return, further arrow-applied for arguments beyond the declared parameters, is headed by it. See
      * [[writeTypeArguments]] for why the *position* matters.
      *
      * An **under-applied** reference rides it too — `nums.foldLeft(initial, combine)` supplies two of three
      * arguments and the dot supplies the third. A type argument is applied to the signature's binders and is
      * independent of how many value arguments follow, so the carrier can be written there; declining would leave it
      * a metavariable that nothing later solves (`Actual: List(Int) -> $bad-apply(Int)`).
      */
    private def ridesFirstBinder(callee: ValueFQN, argCount: Int): Boolean =
      universe.lookup(callee).exists { orv =>
        val view     = SignatureView.of(orv.signature)
        val carriers = EffectCarriers.declaredCarrierBinders(orv)
        view.binders.headOption.exists { first =>
          carriers.contains(first.name.value) &&
          (spine(arrowApplied(view.returnType.value, math.max(0, argCount - view.parameters.size)))._1 match {
            case ParameterReference(head) => head.value == first.name.value
            case _                        => false
          })
        }
      }

    /** Elaborate a call with no declared slot information (an applied function-typed parameter): every slot strict,
      * hoisting only when the call's own result is classifiable (see `coreClassified` in [[elaborateCall]]).
      */
    private def elaborateArguments(
        expr: Sourced[OperatorResolvedExpression],
        args: Seq[Sourced[OperatorResolvedExpression]],
        resultCarrier: Boolean,
        resultClassified: Boolean,
        region: RegionCarrier
    ): (Sourced[OperatorResolvedExpression], Boolean) = {
      val (finalArgs, binds) =
        args.foldLeft((Seq.empty[Sourced[OperatorResolvedExpression]], Seq.empty[(String, Sourced[OperatorResolvedExpression])])) {
          case ((accArgs, accBinds), arg) =>
            strictArgument(accArgs, accBinds, arg, region, region.exists && resultClassified)
        }
      (assemble(expr, expr.as(spine(expr.value)._1), finalArgs, binds, resultCarrier, region), binds.nonEmpty || resultCarrier)
    }

    /** One strict-slot argument: a carrier-valued argument that *performs* (non-empty row) or *discharges* (empty
      * row by capture, still work to run) is hoisted under the region's carrier; without one (or with an
      * unclassifiable core, `hoist` false) it passes through *unchanged* — `runId` is inserted only at the two
      * boundaries the v2 checker defaults at (a definition's pure return, a `val` binding), never at an argument
      * slot, where the carrier must instead flow to the slot's expected type (the hand-monadic `runId(runAbort(x))`
      * shape: the explicit accessor absorbs the still-flex base). A value passes inline.
      */
    private def strictArgument(
        accArgs: Seq[Sourced[OperatorResolvedExpression]],
        accBinds: Seq[(String, Sourced[OperatorResolvedExpression])],
        arg: Sourced[OperatorResolvedExpression],
        region: RegionCarrier,
        hoist: Boolean
    ): (Seq[Sourced[OperatorResolvedExpression]], Seq[(String, Sourced[OperatorResolvedExpression])]) = {
      val (argFinal, bind) = strictSlot(arg, region, hoist)
      (accArgs :+ argFinal, accBinds ++ bind)
    }

    /** One strict-slot argument on its own: the elaborated node plus the bind that hoisted it, where it was work to
      * sequence rather than data to pass on.
      *
      * Separated from [[strictArgument]] because an **arrow** slot needs the strict result *before* it can adapt it.
      * An arrow type is headed by `Function`, not by a carrier, so §1 rule 4 makes the slot an ordinary rowless
      * *value* position: the argument expression runs here like any other, and only the value it evaluates to is
      * then lifted pointwise into the declared codomain.
      */
    private def strictSlot(
        arg: Sourced[OperatorResolvedExpression],
        region: RegionCarrier,
        hoist: Boolean
    ): (Sourced[OperatorResolvedExpression], Option[(String, Sourced[OperatorResolvedExpression])]) = {
      val (argElab, argCarrier) = core(arg, region)
      if (hoist && argCarrier && (performs(arg.value) || discharges(argElab) || sequences(argElab))) {
        val binder = freshBinder()
        // The inserted bind hands the binder the computation's *payload* — payload by construction, which is what
        // lets the call's result kind be re-read from the hoisted spine (§1 rule 4: the slot no longer holds a
        // computation, so a generic it determines is a plain value again).
        payloadBinders = payloadBinders + binder
        // …and, where the argument's *own* declared result is a payload, the binder holds exactly that value, so the
        // argument answers for it at every further application too (see [[payloadBinderSources]]). A carrier-valued
        // argument (`readLine`) is excluded: its payload is the carrier's, which its own declaration does not give.
        if (expressionKind(arg.value, 0) === Kind.Payload)
          payloadBinderSources = payloadBinderSources + (binder -> arg.value)
        (arg.as(ParameterReference(arg.as(binder))), Some(binder -> argElab))
      } else (argElab, scala.None)
    }

    /** Reassemble a call from its elaborated arguments and hoisted binds: the core call, `pure`-wrapped when binds
      * exist around a *definitely pure* core (the innermost continuation must be a computation), then the binds
      * folded rightward so the leftmost argument's action is outermost. A core no declaration classifies is left
      * bare: it may well *be* the computation, and the checker lifts it iff the instantiation says payload.
      */
    private def assemble(
        expr: Sourced[OperatorResolvedExpression],
        head: Sourced[OperatorResolvedExpression],
        finalArgs: Seq[Sourced[OperatorResolvedExpression]],
        binds: Seq[(String, Sourced[OperatorResolvedExpression])],
        resultCarrier: Boolean,
        region: RegionCarrier,
        projectId: Boolean = false
    ): Sourced[OperatorResolvedExpression] = {
      // Untouched code keeps its original nodes, not equal rebuilt ones: rebuilding re-attributes the application
      // spine to per-argument positions, which silently moves every diagnostic anchored at a call.
      val unchanged =
        binds.isEmpty && !projectId && (head.value.asInstanceOf[AnyRef] eq spine(expr.value)._1.asInstanceOf[AnyRef]) &&
          finalArgs.corresponds(spine(expr.value)._2)(_ eq _)
      val coreCall  = if (unchanged) expr else rebuiltSpine(expr, head, finalArgs)
      // The empty-row projection sits *inside* the binds: the binds sequence on the enclosing region's carrier and
      // the core runs on `Id`, so what the continuation returns is a plain value that then lifts into the chain.
      val projected = if (projectId) coreCall.as(runIdWrap(coreCall)) else coreCall
      if (binds.isEmpty) projected
      else {
        val coreElab =
          if (projectId || (!resultCarrier && definitelyPure(projected))) projected.as(pureWrap(projected, region))
          else projected
        binds.foldRight(coreElab) { case ((binder, action), acc) =>
          val continuation = acc.as(FunctionLiteral(acc.as(binder), None, acc))
          acc.as(bindNodes(continuation, action, region))
        }
      }
    }

    /** Rebuild an application spine around new arguments, keeping **each original application node's own position**.
      *
      * `applyChain` attributes every wrapper it builds to the argument it applies, so rebuilding a spine with it moves
      * the intermediate applications onto per-argument ranges — which silently relocates every diagnostic anchored at
      * the call and duplicates hover hints onto the argument's range. Rewriting one argument is not a reason for the
      * rest of the spine to move, so the original wrappers are reused; only where the shape no longer matches (an
      * arity the original spine does not have) does this fall back to a fresh chain.
      */
    private def rebuiltSpine(
        original: Sourced[OperatorResolvedExpression],
        head: Sourced[OperatorResolvedExpression],
        args: Seq[Sourced[OperatorResolvedExpression]]
    ): Sourced[OperatorResolvedExpression] =
      args.lastOption match {
        case None       => head
        case Some(last) =>
          original.value match {
            case FunctionApplication(target, _) =>
              original.as(FunctionApplication(rebuiltSpine(target, head, args.init), last))
            case _                              => original.as(applyChain(head, args))
          }
      }

    /** Whether applying `callee` to `argCount` arguments yields a carrier computation: the declared type remaining
      * after consuming the arguments — the return, further arrow-applied for arguments beyond the declared
      * parameters (an over-applied accessor: `runStateCarrier(fa)(s)` applies the accessor's `S => G[Pair[A, S]]`
      * result) — is headed by one of the callee's own carrier binders or by a platform *run carrier*
      * (`prog : IO[Pair[..]]`, the nominal-run spelling — Appendix A.7). Declared shape, tag-free; an under-applied
      * reference is a value; a *generic-headed* remainder is whatever [[callResultKind]] instantiates that generic
      * at. For a callee outside the universe (an effect-ability method resolved by qualifier only) the declared row
      * decides.
      */
    private def calleeCarrierValued(callee: ValueFQN, args: Seq[Sourced[OperatorResolvedExpression]]): Boolean =
      callResultKind(callee, args, 0) == Kind.Carrier

    /** The kind of the value a call to `callee` yields once `args` (and `extra` further arguments) are supplied: its
      * **declared return, instantiated from the declared shapes of the arguments supplied** (§1 rule 4, A.11.7-T
      * step 1). One step, local, order-free — no unification, no metavariables, no sibling-expression inspection: the
      * only facts read are the callee's declared parameter and return types and the *declared* type each argument
      * expression has by its own head.
      *
      * A concrete-headed return is a payload and a carrier-binder-headed one a computation, exactly as before. The
      * step that is new is a return headed by one of the callee's **plain generics** — `.`'s `B`, `++`'s `T`,
      * `identity`'s `A` — which used to be deferred wholesale for want of a mode. Rule 4 settles those: the binder is
      * whatever the argument occupying the position that mentions it is, and since a rowless slot may not receive a
      * computation at all, that is a payload wherever a signature is honest. Deferring instead is what kept the
      * checker's payload router and its whole obligation path alive (A.11.7-R).
      *
      * The difference from "a generic head is a payload" (A.11.7-R's rejected candidate, which *approximated* the
      * rule instead of reading it) is exactly the two shapes that measured wrong there: a binder no parameter
      * mentions stays [[Kind.Unknown]] (`def wrap[F[_], A](a: A): F[A]` — the constructor class, whose carrier the
      * declared return chooses), and a binder an argument supplies a *computation* to comes out [[Kind.Carrier]]
      * (the dot-chained discharger, whose `.` yields the discharge stack, not a value).
      */
    private def callResultKind(
        callee: ValueFQN,
        args: Seq[Sourced[OperatorResolvedExpression]],
        extra: Int
    ): Kind = {
      val declared = declaredResultKind(callee, args, extra)
      // A call this pass runs at the **empty row** yields `Id[B]` and is projected straight back to `B` (see
      // `projectId` in [[elaborateCall]]), so what the surrounding code receives is a plain value. Saying otherwise
      // would propagate a spurious computation outwards: `lines.foldLeft(0, e -> acc -> add(acc, 1))` would make the
      // enclosing dot carry a row, and `show` of it would read as a rule-4 violation of the caller's making.
      if (declared === Kind.Carrier && emptyRowCall(callee, args)) Kind.Payload else declared
    }

    /** The call's result kind as its *declaration* gives it, before the empty-row projection. */
    private def declaredResultKind(
        callee: ValueFQN,
        args: Seq[Sourced[OperatorResolvedExpression]],
        extra: Int
    ): Kind =
      universe.lookup(callee) match {
        case Some(orv) =>
          val view     = SignatureView.of(orv.signature)
          val supplied = args.size + extra
          if (supplied < view.parameters.size) Kind.Payload // an under-applied reference is a function value
          else {
            val carriers = EffectCarriers.declaredCarrierBinders(orv)
            typeKind(
              arrowApplied(view.returnType.value, supplied - view.parameters.size),
              carriers,
              instantiation(view, args, carriers)
            )
          }
        case None      =>
          // Outside the universe (an effect-ability method resolved by qualifier only) the declared row decides.
          if (RowChecker.calleeRow(callee, universe).nonEmpty) Kind.Carrier else Kind.Unknown
      }

    /** Classify a declared type in the scope that declares it: headed by one of `carriers` (or by a platform run
      * carrier) it is a computation; headed by any other generic it is whatever `instantiation` binds that generic
      * to; anything else is a plain value.
      *
      * A **concrete** carrier head (`Id[A]`) is deliberately not read here, only where a *parameter* declares one
      * (see [[expressionKind]]): this classifies a callee's **return** too, and `Effect[Id]`'s own
      * `def pure[A](a: A): Id[A] = Id(a)` constructs that carrier rather than performing on it — read as a
      * computation, its pure boundary unwraps the construction it just wrote (`runId(Id(a))`).
      */
    private def typeKind(
        tpe: OperatorResolvedExpression,
        carriers: Set[String],
        instantiation: => Map[String, Kind]
    ): Kind =
      if (carrierHeaded(tpe, carriers) || runCarrierHead(tpe)) Kind.Carrier
      else
        spine(tpe) match {
          case (ParameterReference(name), args) =>
            // A binder no supplied argument determines: **bare** it is a plain generic, hence a payload by §1
            // rule 4 whatever the call instantiates it at (`content : Box[A] -> A` referenced unapplied). Applied,
            // it is a constructor position whose argument the declared return chooses — the constructor class
            // (`wrap(s) : F[A]`), which stays unclassified so nothing is written for it.
            instantiation.getOrElse(name.value, if (args.isEmpty) Kind.Payload else Kind.Unknown)
          case _                                => Kind.Payload
        }

    /** Record a **§1 rule-4 violation** for every argument that instantiates one of the callee's plain generics — a
      * position declaring no row — at a computation. See [[RowElaborator.violations]] for the predicate and why it is
      * read after hoisting; this is the same relation [[instantiation]] computes, kept per parameter so the
      * diagnostic can name the slot that is wrong rather than the binder it corrupts.
      *
      * A position that *declares* a computation contributes nothing here and needs no exclusion of its own: a
      * suspended slot (`whenTrue: {G} A`), a pinned capture (`{Throw[E] | G} A`) and a carrier-codomain handler
      * (`onError: E => G[A]`) are all headed by a carrier rather than by a plain generic, which is exactly what
      * [[determinedBy]] declines to bind.
      */
    private def recordRowlessSlots(
        callee: ValueFQN,
        view: SignatureView,
        finalArgs: Seq[Sourced[OperatorResolvedExpression]],
        calleeBinders: Set[String],
        pinned: Set[Int],
        runBoundary: Boolean
    ): Unit =
      view.parameters.zip(finalArgs).zipWithIndex.foreach { case ((declared, arg), index) =>
        val declaresComputation =
          pinned.contains(index) || (runBoundary && index === 0) || hostsComputation(declared.value)
        if (!declaresComputation) {
          if (expressionKind(arg.value, 0) === Kind.Carrier && !isCapturedValue(arg.value))
            violations += Violation(
              arg.as(
                s"This argument is a computation, but argument ${index + 1} of '${callee.name.name}' declares no " +
                  s"effect row."
              ),
              RowElaborator.rowlessSlotHelp
            )
          else
            determinedBy(declared.value, arg.value, 0, calleeBinders).collectFirst {
              case (binder, Kind.Carrier) => binder
            }.foreach { binder =>
              violations += Violation(
                arg.as(
                  s"This argument is a computation, but it lands on the type parameter '$binder' of " +
                    s"'${callee.name.name}', which declares no effect row (argument ${index + 1})."
                ),
                RowElaborator.rowlessSlotHelp
              )
            }
        }
      }

    /** Whether a declared parameter type **can host a computation**, decided structurally: a carrier is always
      * *applied* to its payload, so any **applied** head — `{G} A`, `runId`'s `Id[A]`, `runMain`'s `IO[A]`, a
      * user's own identity carrier, the carrier `data` types' own `G[Either[E, A]]` field — may be one, and this pass
      * says nothing about it. What is left is what rule 4 can positively call **rowless**: a *nullary* concrete type
      * (`printLine`'s `String`) and a *bare* generic, which rule 4 calls a payload outright.
      *
      * Deciding it positively rather than by exclusion is what keeps the rule from being keyed on names: recognising
      * carriers by their FQN would miss a user-declared one (the shadow corpus declares its own `data Id[A]`) and
      * "has an `Effect` instance" is prohibited outright. Pinned and run-boundary *positions* are excluded by their
      * index at the call site, a pinned row being a declared capture rather than anything readable off the type.
      *
      * An **arrow** is not one of them, whatever its codomain or domains say. A handler slot `E => {Effect} A` holds
      * a *function*; the row belongs to what applying that function later does, and rule 4 reads the slot itself —
      * headed by `Function`, hence rowless and strict. Reading the arrow's pieces instead switched this check off for
      * the whole slot, its own value position included, so a computation delivered where a function was declared went
      * unreported and reached the checker as an ordinary mismatch.
      */
    private def hostsComputation(declared: OperatorResolvedExpression): Boolean =
      asArrowLike(declared).isEmpty && spine(declared)._2.nonEmpty

    /** Whether an argument is a **captured** computation — a reference to one of this definition's own pinned
      * parameters. §1 rule 3: a pinned row is a *reified* computation and therefore an ordinary type, so handing one
      * on is handing on data, not letting an effect through a position that does not declare it. This is what keeps a
      * carrier's own generated accessor legal, since destructuring its pinned `obj` passes it to the `match`
      * eliminator's plain slot.
      */
    private def isCapturedValue(expr: OperatorResolvedExpression): Boolean = expr match {
      case ParameterReference(name) => pinnedParams.contains(name.value)
      case _                        => false
    }

    /** The kinds this call binds the callee's own generics to, read off the arguments occupying the positions that
      * *determine* them: a slot declared as the bare binder itself (`a: A`), and the codomain of an arrow slot
      * (`f: A => B`, where `B` is what applying the argument yields). Positions that determine nothing — a binder
      * buried inside an applied type — leave their binder unbound, which reads as [[Kind.Unknown]].
      */
    private def instantiation(
        view: SignatureView,
        args: Seq[Sourced[OperatorResolvedExpression]],
        carriers: Set[String]
    ): Map[String, Kind] =
      view.parameters.zip(args).foldLeft(Map.empty[String, Kind]) { case (acc, (declared, arg)) =>
        determinedBy(declared.value, arg.value, 0, carriers).foldLeft(acc) { case (bound, (binder, kind)) =>
          bound.updated(binder, bound.get(binder).fold(kind)(joinKind(_, kind)))
        }
      }

    /** Combine what two positions say about the same binder. The join is what keeps the rule **order-free**: a binder
      * several parameters mention (`foldLeft`'s `B`, at `initial` and at `combine`'s codomain) must come out the same
      * whichever order they are read in.
      *
      * [[Kind.Unknown]] is the absence of information and loses to either verdict — one position reading
      * `add(e, acc)`, an ability method outside the universe, must not erase what `initial: B ← 0` already settled.
      * [[Kind.Carrier]] wins over [[Kind.Payload]]: a binder any position supplies a computation to is a computation,
      * which is precisely the rule-4 violation [[recordRowlessSlots]] reports at that position.
      */
    private def joinKind(left: Kind, right: Kind): Kind =
      if (left === right) left
      else if (left === Kind.Carrier || right === Kind.Carrier) Kind.Carrier
      else if (left === Kind.Unknown) right
      else left

    /** What one argument settles about the binders its declared slot mentions in a determining position. `applied`
      * counts the arrow levels already descended, so the argument is asked for the kind it yields *after that many
      * further applications*.
      */
    private def determinedBy(
        declared: OperatorResolvedExpression,
        arg: OperatorResolvedExpression,
        applied: Int,
        carriers: Set[String]
    ): Map[String, Kind] =
      declared match {
        case ParameterReference(name) if !carriers.contains(name.value) =>
          Map(name.value -> expressionKind(arg, applied))
        case _                                                          =>
          underArrow(declared, arg, applied, carriers)((codomain, inner, level, _) =>
            determinedBy(codomain, inner, level, carriers)
          ).getOrElse(Map.empty)
      }

    /** The kind of the value an argument expression yields once `applied` further arguments are given to it — read
      * off its own head's declaration, never off its elaborated form (elaboration does not change a spine's head, so
      * this is the same answer the pass reaches when it elaborates the node).
      */
    private def expressionKind(expr: OperatorResolvedExpression, applied: Int): Kind = {
      val (head, args) = spine(expr)
      head match {
        case _: IntegerLiteral | _: StringLiteral                     =>
          if (applied === 0) Kind.Payload else Kind.Unknown
        case ValueReference(name, _)                                  => callResultKind(name.value, args, applied)
        case FunctionLiteral(_, _, body) if args.isEmpty              =>
          if (applied === 0) Kind.Payload else expressionKind(body.value, applied - 1)
        case ParameterReference(name) if args.isEmpty && applied === 0 =>
          // A binder holding a computation — one of this definition's own carriers, or one declaring a **concrete**
          // carrier (`runId`'s own `obj: Id[A]`, `data Box(action: IO[Unit])`'s accessor), which §1 rule 2 reads as
          // "a computation on this carrier" and which therefore must never be offered a `pure`. Then a
          // payload-by-construction block binder, and otherwise the **declared type**, classified exactly as the
          // applied arm below classifies a codomain and [[callResultKind]] a callee's return — one classifier,
          // [[typeKind]], for all three. Reading the declared type as a payload only when it is *atomic* left
          // `a: List[A]` unclassified, so `foldLeft(a, …)` on a pure definition neither settled the empty row nor
          // lifted `a`, while the pure boundary still wrote `runId` — leaving the checker a bare `List[A]` where
          // that same `runId` implies `Id[List[A]]`.
          val declared = paramTypes.get(name.value)
          if (carrierBinders.contains(name.value) || declared.exists(concreteCarrierHead(_).isDefined)) Kind.Carrier
          else if (payloadBinders.contains(name.value)) Kind.Payload
          else declared.fold(Kind.Unknown)(typeKind(_, ownBinders, ownGenericKinds))
        case ParameterReference(name)                                 =>
          // A binder this pass hoisted holds the payload of the expression it bound, so that expression's own
          // declaration answers for it here too — a *declared* parameter's type would, and a minted binder has none
          // (see [[payloadBinderSources]]).
          payloadBinderSources.get(name.value) match {
            case Some(source) => expressionKind(source, applied + args.size)
            case None         =>
              paramTypes.get(name.value) match {
                case Some(tpe) =>
                  val (domains, codomain) = arrowChainLike(tpe)
                  if (args.size + applied < domains.size) Kind.Payload
                  else if (args.size + applied === domains.size) typeKind(codomain, ownBinders, ownGenericKinds)
                  else Kind.Unknown
                case None      => Kind.Unknown
              }
          }
        case _                                                        => Kind.Unknown
      }
    }

    /** This definition's *own* generics, as rule 4 settles them: every binder that is not one of its declared
      * carriers is a plain generic and therefore a payload, since a caller may not instantiate a rowless position at
      * a computation.
      */
    private lazy val ownGenericKinds: Map[String, Kind] =
      (ownGenerics -- ownBinders).map(_ -> Kind.Payload).toMap

    /** The declared type remaining after applying an arrow-shaped type to `extra` further arguments. */
    private def arrowApplied(tpe: OperatorResolvedExpression, extra: Int): OperatorResolvedExpression =
      if (extra <= 0) tpe
      else
        asArrowLike(tpe) match {
          case Some((_, cod)) => arrowApplied(cod.value, extra - 1)
          case None           => tpe
        }

    /** Whether a type expression is headed by a platform run carrier (read off the run-boundary registry). */
    private def runCarrierHead(tpe: OperatorResolvedExpression): Boolean =
      spine(tpe)._1 match {
        case ValueReference(name, _) => RowChecker.runCarrierHeads(universe).contains(name.value)
        case _                       => false
      }

    /** Rebuild a handler lambda with its (fully peeled) body elaborated as a carrier region: the declared codomain
      * is a carrier, so the body always has one to bind on.
      */
    private def elaborateLambdaForced(
        lambda: Sourced[OperatorResolvedExpression],
        region: RegionCarrier,
        domains: Seq[OperatorResolvedExpression],
        carriers: Set[String]
    ): Sourced[OperatorResolvedExpression] = {
      val (names, inner) = RowChecker.peelBinders(lambda.value)
      val innerElab      =
        withLambdaBinders(names, domains, carriers)(elaborate(lambda.as(inner), needCarrier = true, region))
      // An untouched body keeps the original lambda node: rebuilding an equal one reads as a changed argument
      // upstream and re-attributes the whole application spine (see [[assemble]]).
      if (innerElab.value.asInstanceOf[AnyRef] eq inner.asInstanceOf[AnyRef]) lambda
      else lambda.as(rewrap(lambda.value, innerElab))
    }

    /** Rebuild a lambda with its (fully peeled) body elaborated naturally, returning whether the body came out
      * carrier-valued (an effectful body is a bind chain; a pure body is untouched).
      */
    private def elaborateLambdaNatural(
        lambda: Sourced[OperatorResolvedExpression],
        region: RegionCarrier,
        domains: Seq[OperatorResolvedExpression],
        carriers: Set[String]
    ): (Sourced[OperatorResolvedExpression], Boolean) = {
      val (names, inner)            = RowChecker.peelBinders(lambda.value)
      val (innerElab, innerCarrier) = withLambdaBinders(names, domains, carriers)(core(lambda.as(inner), region))
      // An untouched body keeps the original lambda node (see [[elaborateLambdaForced]]).
      val rebuilt                   =
        if (innerElab.value.asInstanceOf[AnyRef] eq inner.asInstanceOf[AnyRef]) lambda
        else lambda.as(rewrap(lambda.value, innerElab))
      (rebuilt, innerCarrier)
    }

    /** Whether calling the function-typed parameter `name` with `argCount` arguments yields a carrier computation:
      * the declared arrow's final codomain is carrier-headed on this definition's own binders, and the call
      * saturates the arrow.
      */
    private def paramCallCarrier(name: String, argCount: Int): Boolean =
      paramTypes.get(name).exists { tpe =>
        val (domains, codomain) = arrowChainLike(tpe)
        domains.nonEmpty && argCount >= domains.size && carrierHeaded(codomain, ownBinders)
      }

    /** Whether a declared parameter type is an arrow whose final codomain is carrier-headed (`E => G[A]`). */
    private def carrierCodomain(paramType: OperatorResolvedExpression, carriers: Set[String]): Boolean = {
      val (domains, codomain) = arrowChainLike(paramType)
      domains.nonEmpty && carrierHeaded(codomain, carriers)
    }

    /** Unfold a declared type into its arrow domains and final codomain, seeing through the `=>` alias. */
    private def arrowChainLike(tpe: OperatorResolvedExpression): (Seq[OperatorResolvedExpression], OperatorResolvedExpression) =
      asArrowLike(tpe) match {
        case Some((dom, cod)) =>
          val (rest, codomain) = arrowChainLike(cod.value)
          (dom.value +: rest, codomain)
        case None             => (Seq.empty, tpe)
      }

    /** View a type expression as an arrow, seeing through one level of type alias (`Str => G[A]` reaches this phase
      * as the unexpanded `=>` alias application — an operator-named alias in the *Default* namespace) by expanding
      * the alias's own declared body over its binders — declared information read from the universe, no evaluation.
      * The expansion is self-guarding: a head whose declared body is not an arrow yields [[None]].
      *
      * The substitution is **simultaneous**, staged through fresh names: `=>` binds `A` and `B`, so expanding
      * `B => {Effect} B` (`=>[B, F[B]]`) one binder at a time would substitute `A := B` and then re-substitute that
      * very `B` when `B := F[B]` follows — reading `A => B => {Effect} B` as `A => {Effect} B => {Effect} B` and
      * classifying the accumulator binder as a computation.
      */
    private def asArrowLike(
        tpe: OperatorResolvedExpression
    ): Option[(Sourced[OperatorResolvedExpression], Sourced[OperatorResolvedExpression])] =
      asArrow(tpe).orElse {
        val (head, args) = spine(tpe)
        head match {
          case ValueReference(name, _) =>
            for {
              orv             <- universe.lookup(name.value)
              body            <- orv.runtime
              (binders, inner) = RowChecker.peelBinders(body.value)
              if binders.size == args.size
              staged           = binders.zipWithIndex.foldLeft(inner) { case (acc, (binder, index)) =>
                                   substitute(acc, binder, ParameterReference(body.as(s"$$alias$$$index")))
                                 }
              expanded         = args.zipWithIndex.foldLeft(staged) { case (acc, (arg, index)) =>
                                   substitute(acc, s"$$alias$$$index", arg.value)
                                 }
              arrow           <- asArrow(expanded)
            } yield arrow
          case _                       => None
        }
      }

    private def freshBinder(): String = {
      nextBinder += 1
      s"$$row$$$nextBinder"
    }
  }

  /** Whether a type expression is headed by one of the given carrier binders (`G[A]` for binder `G`). */
  private def carrierHeaded(tpe: OperatorResolvedExpression, carriers: Set[String]): Boolean =
    spine(tpe)._1 match {
      case ParameterReference(name) => carriers.contains(name.value)
      case _                        => false
    }

  private def isBareLambda(expr: OperatorResolvedExpression): Boolean = expr match {
    case _: FunctionLiteral => true
    case _                  => false
  }

  /** `flatMap[F](continuation, action)` spelled by the machinery FQN, with the region's carrier written as the
    * `Effect` ability's own binder when this pass can spell it (A.11.4) — an inserted combinator names the carrier it
    * sequences on rather than leaving the checker to solve it.
    */
  private def bindNodes(
      continuation: Sourced[OperatorResolvedExpression],
      action: Sourced[OperatorResolvedExpression],
      region: RegionCarrier
  ): OperatorResolvedExpression =
    applyChain(
      continuation.as(ValueReference(continuation.as(WellKnownTypes.effectFlatMapFQN), region.term.toSeq)),
      Seq(continuation, action)
    )

  /** `pure[F](value)` spelled by the machinery FQN, carrying the region's carrier where spellable (see
    * [[bindNodes]]).
    */
  private def pureWrap(
      value: Sourced[OperatorResolvedExpression],
      region: RegionCarrier
  ): OperatorResolvedExpression =
    applyChain(value.as(ValueReference(value.as(WellKnownTypes.effectPureFQN), region.term.toSeq)), Seq(value))

  /** `runId(computation)` — the boundary unwrap of a discharge whose residual row is empty (A.4). */
  private def runIdWrap(value: Sourced[OperatorResolvedExpression]): OperatorResolvedExpression =
    applyChain(value.as(ValueReference(value.as(WellKnownTypes.runIdFQN))), Seq(value))
}
