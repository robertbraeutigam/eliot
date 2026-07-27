package com.vanillasource.eliot.eliotc.row

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.effect.EffectCarrierNaming
import com.vanillasource.eliot.eliotc.effect.processor.{EffectCarriers, EffectMachinery}
import com.vanillasource.eliot.eliotc.module.fact.{Qualifier, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.*
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue.ResolvedAbilityConstraint
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The effects-as-rows **elaboration desugar** (docs/effects-as-rows.md §3) — rewrite a direct-style definition into
  * fully explicit monadic core, the same shape the v2 checker's elaboration produces, so downstream phases are
  * unchanged consumers.
  *
  * The rewrite decides every position whose mode the consulted *declarations* spell, and **explicitly defers** the
  * rest (A.8.6): where the deciding fact is an instantiation — a computation meeting a slot typed by a bare generic,
  * a call whose declared result is generic-headed — the elaborator writes *nothing* (no hoist, no `pure`, no
  * `runId`), and the checker finishes the node from the solved instantiation. Deferral, not approximation, is the
  * only sanctioned reaction to missing declared information: a missing rewrite is completed (or loudly rejected)
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
  *     generic-headed remainder is *deferred* — neither a value nor a computation until instantiated;
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
  *     carrier-typed *value* (`pure(x)`, a suspended-parameter reference with an empty row) is data and passes;
  *   - an argument at a **generic-headed slot is deferred**: the slot's mode belongs to its instantiation (the
  *     dot-chained discharger's capture, `foldLeft`'s accumulator, `pick`'s payload) — the argument's own interior is
  *     still elaborated, but this slot neither hoists nor wraps it;
  *   - a **carrier-valued `val`/statement binding** (the block desugar's applied lambda `(x -> rest)(e)`) becomes
  *     `flatMap(x -> rest', e')`; a pure binding stays an applied lambda;
  *   - a **pure expression in a carrier position** — the innermost continuation of a bind chain, a pure body under a
  *     declared row, a pure argument at a declared-suspended, pinned or run-boundary slot — is wrapped `pure(expr)`,
  *     but only when it is pure *by declaration* (a concrete-headed result; a generic-headed one is deferred);
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

  /** Elaborate a definition's runtime body. [[None]] for a body-less value. The result mirrors the original
    * [[Sourced]] positions: inserted machinery nodes are attributed to the expression they wrap.
    */
  def elaborate(orv: OperatorResolvedValue, universe: RowChecker.Universe): Option[Sourced[OperatorResolvedExpression]] =
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
          RowChecker.parameterRowsOf(orv, paramNames),
          ownBinders,
          universe,
          EffectCarriers.declaredEffects(ownBinders, orv.paramConstraints)
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
      paramRows: Map[String, RowChecker.Row],
      ownBinders: Set[String],
      universe: RowChecker.Universe,
      ambientAbilities: Set[AbilityFQN]
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

    /** Whether the elaboration just performed left a **deferred node at a carrier position** — a node that is
      * neither carrier-valued nor definitely pure where a computation is expected. A rewrite that would place such a
      * node as the tail of an *inserted* bind chain must not happen: the checker would meet the node's flex result
      * against the machinery's declared carrier slot and commit it by first-contact unification (`resultValue(r)`'s
      * bare `A` becoming the carrier — a miscompile). The flag lets the insertion site detect this and defer the
      * whole rewrite instead (A.8.6: the elaborator only rewrites what it can fully discharge).
      */
    private var deferredAtCarrier: Boolean = false

    /** Elaborate under a block binder, which shadows any same-named parameter. */
    private def withBinder[A](name: String, holdsCarrier: Boolean, isPayload: Boolean = false)(body: => A): A = {
      val outerCarrier = carrierBinders
      val outerPayload = payloadBinders
      carrierBinders = if (holdsCarrier) carrierBinders + name else carrierBinders - name
      payloadBinders = if (isPayload) payloadBinders + name else payloadBinders - name
      try body
      finally {
        carrierBinders = outerCarrier
        payloadBinders = outerPayload
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
          RegionCarrier.Spelled(expr.as(ValueReference(expr.as(WellKnownTypes.idFQN))))
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
      * "Not carrier-valued" is not the same claim: it also covers everything the desugar cannot classify, chiefly
      * anything whose declared result is *generic-headed* — a reference to a lambda binder (its type only inference
      * knows), a call returning a bare generic (`identity(x)`, `foldLeft(...)`) or a bare-HKT application
      * (`id[F[_]] : F[A]`), all of which an instantiation may make a computation. Those are **deferred** (A.8.6):
      * the desugar writes nothing and lets the checker decide from the solved instantiation — the fail-safe
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
          universe.lookup(name.value).exists { orv =>
            val view = SignatureView.of(orv.signature)
            args.sizeIs < view.parameters.size || // an under-applied reference is a function value
            declaredPayloadResult(arrowApplied(view.returnType.value, args.size - view.parameters.size))
          }
        case ParameterReference(name)             =>
          // A payload-by-construction block binder (see [[payloadBinders]]), or a parameter with an *atomic*
          // declared type (`s: String`, `x: X`). An applied declared type may well be a concrete carrier stack the
          // desugar cannot name (`fa: DepCarrier[X, G, A]` inside that carrier's own `Effect` instance), and lifting
          // it would wrap a computation twice.
          args.isEmpty &&
          (payloadBinders.contains(name.value) || paramTypes.get(name.value).exists(tpe => spine(tpe)._2.isEmpty))
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
      RowChecker.expressionRow(expr, paramRows, universe).nonEmpty

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
          (elaborateLambdaNatural(expr, region)._1, false)
        case ValueReference(name, _) if args.nonEmpty             =>
          elaborateCall(expr, name.value, args, region)
        case ValueReference(name, _)                              =>
          // A nullary reference (`readLine`, `state`, `abort`) is a call like any other: if its result rides its own
          // first binder, the region's carrier is written there too.
          val discharging = dischargingCallee(name.value)
          (writeCarrier(expr, name.value, 0, callRegion(region, discharging)), calleeCarrierValued(name.value, Seq.empty))
        case ParameterReference(name) if args.isEmpty             =>
          // A suspended parameter (declared carrier-headed) holds its computation unrun: referencing it yields a
          // carrier value.
          (expr, carrierBinders.contains(name.value))
        case ParameterReference(name)                             =>
          // Calling a function-typed parameter: carrier-valued when its declared arrow's final codomain is
          // carrier-headed and the call saturates it (`action(s)` inside a callback-taking definition); its result
          // is classifiable when that codomain is a carrier or a declared payload.
          val resultCarrier    = paramCallCarrier(name.value, args.size)
          val resultClassified = resultCarrier || paramTypes.get(name.value).exists { tpe =>
            val (domains, codomain) = arrowChainLike(tpe)
            domains.nonEmpty && args.sizeIs >= domains.size && declaredPayloadResult(codomain)
          }
          elaborateArguments(expr, args, resultCarrier, resultClassified, region)
        case _                                                    =>
          (expr, false)
      }
    }

    /** Elaborate a call to a named value: arguments by their declared slot mode, then hoist each carrier-valued
      * strict-slot argument into a `flatMap` around the core call (leftmost argument outermost). A slot whose
      * declared type is *generic-headed* is **deferred** (A.8.6): its mode — capture through the dot-chained
      * discharger, payload once a sibling fixes the instantiation — belongs to the instantiation, so the elaborator
      * writes nothing there and the checker finishes it. The core call is carrier-valued iff the callee's declared
      * return is carrier-headed; a generic-headed return is likewise deferred.
      */
    private def elaborateCall(
        expr: Sourced[OperatorResolvedExpression],
        callee: ValueFQN,
        args: Seq[Sourced[OperatorResolvedExpression]],
        region: RegionCarrier
    ): (Sourced[OperatorResolvedExpression], Boolean) = {
      val calleeOrv     = universe.lookup(callee)
      val calleeView    = calleeOrv.map(o => SignatureView.of(o.signature))
      val calleeCarrier = calleeCarrierValued(callee, args)
      val pinned        = calleeOrv.map(_.effectRow.pinnedParameterIndices).getOrElse(Set.empty)
      val runBoundary   = universe.runBoundaries.contains(callee)
      val calleeBinders = calleeOrv.map(EffectCarriers.declaredCarrierBinders).getOrElse(Set.empty)
      // Whether this call's own carrier is known to exist: the enclosing region has one, or the call discharges a
      // row and so runs on `Id`. When neither holds the carrier is context-supplied and its slots stay ordinary —
      // lifting a pure argument with `pure` would force `Id` on a call whose carrier the use site chooses.
      val ownRegion     = callRegion(region, pinned.nonEmpty || runBoundary)
      val slotRegion    = ownRegion.exists
      // Hoisting is a rewrite the elaborator must fully discharge: the core it leaves as the chain's innermost
      // continuation must be classifiable — carrier-valued, or a declared-payload result it can `pure`-wrap. A
      // deferred (generic-headed) core may *be* the computation, so hoisting around it is rolled into deferral: no
      // binds, the checker elaborates the whole call.
      val coreClassified = calleeCarrier || calleeOrv.exists { orv =>
        val view = SignatureView.of(orv.signature)
        args.sizeIs >= view.parameters.size &&
        declaredPayloadResult(arrowApplied(view.returnType.value, args.size - view.parameters.size))
      }
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
            (accArgs :+ core(arg, captureRegion)._1, accBinds)
          } else if (declaredSlot.exists(s => carrierHeaded(s, calleeBinders))) {
            // A declared-suspended slot (carrier-headed parameter type) receives a computation on the *caller's*
            // carrier: an effectful argument passes unrun; a pure argument is lifted (`if(c, "a")` ⇒ `pure("a")`).
            // With no carrier of its own (`slotRegion`) the call's carrier is context-supplied, so the slot takes
            // whatever it is given — lifting there would force `Id` on a carrier the use site chooses.
            val suspended =
              if (slotRegion) elaborate(arg, needCarrier = true, region) else core(arg, region)._1
            (accArgs :+ suspended, accBinds)
          } else if (isBareLambda(arg.value)) {
            if (declaredSlot.exists(s => carrierCodomain(s, calleeBinders))) {
              // A handler/callback at a carrier-codomain slot (`onError: E => G[A]`, `action: A => {Effect} Unit`):
              // its body is a carrier region — a pure body lifts via `pure`. The codomain's carrier is the call's
              // own result carrier, so the body inherits it when we can spell it, and merely *has* one otherwise.
              (accArgs :+ elaborateLambdaForced(arg, forced(ownRegion)), accBinds)
            } else {
              // A lambda at a plain arrow slot elaborates naturally: an effectful body becomes a bind chain, a pure
              // body is untouched. What its codomain instantiates the slot at is the checker's to discover.
              (accArgs :+ elaborateLambdaNatural(arg, region)._1, accBinds)
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

      val head = writeCarrier(expr.as(spine(expr.value)._1), callee, args.size, ownRegion)
      (assemble(expr, head, finalArgs, binds, calleeCarrier, region), binds.nonEmpty || calleeCarrier)
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

    /** Write the region's carrier as an explicit leading type argument of a call (A.11.4) — `printLine[F](s)`,
      * `readLine[F]`, `runAbort[Id](x)` — so the checker never mints a metavariable in carrier position and the
      * carrier is rigid from elaboration onwards. The reference is returned unchanged wherever the write is not
      * *declared* to be correct, which is always the fail-safe direction: an unwritten carrier is simply inferred,
      * exactly as before this step.
      *
      * Three conditions, all read off declarations:
      *
      *   - the region has a **spellable** carrier (see [[RegionCarrier]]);
      *   - the reference carries **no type arguments of its own** — a user who wrote them has already decided;
      *   - the callee's **first** generic binder is one of its declared carriers *and* the type remaining after this
      *     call's arguments is headed by that same binder, so the result rides it.
      *
      * The first-binder restriction is the limit of the mechanism, not a heuristic: `ValueReference.typeArgs` applies
      * positionally, so writing binder *k* means writing binders `0..k-1` too, and those are payload types this pass
      * has no declared way to name. It costs nothing in practice — [[EffectSugarDesugarer]] *prepends* the carrier it
      * mints, and an ability method's own ability parameter leads, which together are every effectful call a user
      * writes. A hand-written discharger that places its carrier later (`catch[E, G[_] ~ Effect, A]`) keeps it
      * inferred.
      */
    private def writeCarrier(
        reference: Sourced[OperatorResolvedExpression],
        callee: ValueFQN,
        argCount: Int,
        region: RegionCarrier
    ): Sourced[OperatorResolvedExpression] =
      (reference.value, region.term) match {
        case (ValueReference(name, existing), Some(ambient)) if existing.isEmpty && ridesFirstBinder(callee, argCount) =>
          carrierAt(callee, ambient) match {
            case Some(term) => reference.as(ValueReference(name, Seq(term)))
            case scala.None => reference
          }
        case _                                                                                                        =>
          reference
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
    private lazy val dischargeableAbilities: Set[AbilityFQN] =
      universe.values.values.flatMap { orv =>
        orv.effectRow.returnPinnedEffects ++ orv.effectRow.pinnedParameterEffects.flatMap(_.effects)
      }.map(_.abilityFQN).toSet

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

    /** Whether applying `callee` to `argCount` arguments yields a result headed by the callee's **first** generic
      * binder, that binder being one of its declared carriers. See [[writeCarrier]] for why the position matters.
      */
    private def ridesFirstBinder(callee: ValueFQN, argCount: Int): Boolean =
      universe.lookup(callee).exists { orv =>
        val view     = SignatureView.of(orv.signature)
        val carriers = EffectCarriers.declaredCarrierBinders(orv)
        view.binders.headOption.exists { first =>
          carriers.contains(first.name.value) && argCount >= view.parameters.size &&
          (spine(arrowApplied(view.returnType.value, argCount - view.parameters.size))._1 match {
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
      val (argElab, argCarrier) = core(arg, region)
      if (hoist && argCarrier && (performs(arg.value) || discharges(argElab))) {
        val binder = freshBinder()
        (accArgs :+ arg.as(ParameterReference(arg.as(binder))), accBinds :+ (binder -> argElab))
      } else {
        (accArgs :+ argElab, accBinds)
      }
    }

    /** Reassemble a call from its elaborated arguments and hoisted binds: the core call, `pure`-wrapped when binds
      * exist around a *definitely pure* core (the innermost continuation must be a computation), then the binds
      * folded rightward so the leftmost argument's action is outermost. A core that is neither carrier-valued nor
      * definitely pure — a generic-headed return like `andThen($row$1, abort)`, whose instantiation may well *be*
      * the computation — is deferred bare (A.8.6): the checker lifts it iff the instantiation says payload.
      */
    private def assemble(
        expr: Sourced[OperatorResolvedExpression],
        head: Sourced[OperatorResolvedExpression],
        finalArgs: Seq[Sourced[OperatorResolvedExpression]],
        binds: Seq[(String, Sourced[OperatorResolvedExpression])],
        resultCarrier: Boolean,
        region: RegionCarrier
    ): Sourced[OperatorResolvedExpression] = {
      // Untouched code keeps its original nodes, not equal rebuilt ones: rebuilding re-attributes the application
      // spine to per-argument positions, which silently moves every diagnostic anchored at a call.
      val unchanged =
        binds.isEmpty && (head.value.asInstanceOf[AnyRef] eq spine(expr.value)._1.asInstanceOf[AnyRef]) &&
          finalArgs.corresponds(spine(expr.value)._2)(_ eq _)
      val coreCall  = if (unchanged) expr else expr.as(applyChain(head, finalArgs))
      if (binds.isEmpty) coreCall
      else {
        val coreElab =
          if (resultCarrier || !definitelyPure(coreCall)) coreCall else coreCall.as(pureWrap(coreCall, region))
        binds.foldRight(coreElab) { case ((binder, action), acc) =>
          val continuation = acc.as(FunctionLiteral(acc.as(binder), None, acc))
          acc.as(bindNodes(continuation, action, region))
        }
      }
    }

    /** Whether applying `callee` to `argCount` arguments yields a carrier computation: the declared type remaining
      * after consuming the arguments — the return, further arrow-applied for arguments beyond the declared
      * parameters (an over-applied accessor: `runStateCarrier(fa)(s)` applies the accessor's `S => G[Pair[A, S]]`
      * result) — is headed by one of the callee's own carrier binders or by a platform *run carrier*
      * (`prog : IO[Pair[..]]`, the nominal-run spelling — Appendix A.7). Declared shape, tag-free; an under-applied
      * reference is a value; a *generic-headed* remainder is deferred and reads as not-carrier-valued (and, via
      * [[definitelyPure]], as not-definitely-pure either). For a callee outside the universe (an effect-ability
      * method resolved by qualifier only) the declared row decides.
      */
    private def calleeCarrierValued(callee: ValueFQN, args: Seq[Sourced[OperatorResolvedExpression]]): Boolean =
      universe.lookup(callee) match {
        case Some(orv) =>
          val view      = SignatureView.of(orv.signature)
          val binders   = EffectCarriers.declaredCarrierBinders(orv)
          val remaining = arrowApplied(view.returnType.value, args.size - view.parameters.size)
          args.sizeIs >= view.parameters.size &&
          (carrierHeaded(remaining, binders) || runCarrierHead(remaining))
        case None      =>
          RowChecker.calleeRow(callee, universe).nonEmpty
      }

    /** Whether a declared result type says **payload**: concrete-headed (a `ParameterReference` head — carrier
      * binder or bare generic alike — is the instantiation's to classify, hence deferred) and not a platform run
      * carrier.
      */
    private def declaredPayloadResult(tpe: OperatorResolvedExpression): Boolean =
      spine(tpe)._1 match {
        case _: ParameterReference => false
        case _                     => !runCarrierHead(tpe)
      }

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
        region: RegionCarrier
    ): Sourced[OperatorResolvedExpression] = {
      val (_, inner) = RowChecker.peelBinders(lambda.value)
      val innerElab  = elaborate(lambda.as(inner), needCarrier = true, region)
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
        region: RegionCarrier
    ): (Sourced[OperatorResolvedExpression], Boolean) = {
      val (_, inner)                = RowChecker.peelBinders(lambda.value)
      val (innerElab, innerCarrier) = core(lambda.as(inner), region)
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
              expanded         = binders.zip(args).foldLeft(inner) { case (acc, (binder, arg)) =>
                                   substitute(acc, binder, arg.value)
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

  /** The A.8.7 **mode splice** (docs/effects-as-rows.md A.8.7): apply the desugar's own placement rules at positions
    * the post-drain mode resolver decided — the desugar finishing its job late, never a second in-checker
    * implementation of placement. Two rewrites, both spelled with the same machinery builders the eager elaboration
    * uses:
    *
    *   - a **hoist** target (an argument classified *payload* at its solved slot) is replaced by a fresh `$row$N`
    *     binder reference and its computation bound around the *outermost* application of its spine —
    *     `flatMap($row$N -> f(.., $row$N, ..), arg)`, leftmost argument outermost, exactly the strict-slot rule. The
    *     target carries the core rule's verdict, decided by the resolver from the solved spine result (the desugar's
    *     [[Elaboration.assemble]] rule with the mode known): a *payload* core is `pure`-wrapped as the chain's
    *     innermost continuation, a carrier-valued or still-undetermined one stays bare;
    *   - a **binding** target (a deferred `let` whose bound type resolved carrier-headed) has its plain applied-lambda
    *     binding rewritten to the sequencing bind — `(x -> rest)(bound)` becomes `flatMap(x -> rest, bound)`, the
    *     binding rule applied late.
    *
    * Targets are matched by **reference identity** on the body's own [[Sourced]] nodes (the checker threads exactly
    * these nodes through its spine decomposition), and untouched subtrees keep their original nodes. Fresh binders
    * continue the body's own `$row$N` numbering, so a spliced binder never collides with an eagerly-minted one.
    */
  def spliceResolvedModes(
      body: Sourced[OperatorResolvedExpression],
      hoistArgs: Seq[(Sourced[OperatorResolvedExpression], Boolean)],
      bindLets: Seq[Sourced[OperatorResolvedExpression]]
  ): Sourced[OperatorResolvedExpression] = {
    var nextBinder = maxRowBinderIndex(body.value)

    def freshBinder(): String = { nextBinder += 1; s"$$row$$$nextBinder" }

    def isTarget(targets: Seq[Sourced[OperatorResolvedExpression]], node: Sourced[OperatorResolvedExpression]): Boolean =
      targets.exists(_.asInstanceOf[AnyRef] eq node.asInstanceOf[AnyRef])

    val hoistNodes = hoistArgs.map(_._1)

    def pureCore(node: Sourced[OperatorResolvedExpression]): Boolean =
      hoistArgs.exists { case (ref, wrap) => wrap && (ref.asInstanceOf[AnyRef] eq node.asInstanceOf[AnyRef]) }

    def rewrite(node: Sourced[OperatorResolvedExpression]): Sourced[OperatorResolvedExpression] = node.value match {
      case FunctionLiteral(name, tpe, b)                                                 =>
        val b2 = rewrite(b)
        if (b2 eq b) node else node.as(FunctionLiteral(name, tpe, b2))
      case FunctionApplication(t, a) if isBareLambda(t.value) && isTarget(bindLets, a)   =>
        val FunctionLiteral(name, tpe, letBody) = t.value: @unchecked
        val continuation                        = t.as(FunctionLiteral(name, tpe, rewrite(letBody)))
        node.as(bindNodes(continuation, rewrite(a), RegionCarrier.Unspelled))
      case FunctionApplication(_, _)                                                     => rewriteSpine(node)
      case _                                                                             => node
    }

    def rewriteSpine(node: Sourced[OperatorResolvedExpression]): Sourced[OperatorResolvedExpression] = {
      val (_, args) = spine(node.value)
      val targets   = args.filter(a => isTarget(hoistNodes, a))
      val binders   = targets.map(t => (t.asInstanceOf[AnyRef], freshBinder()))

      def binderFor(a: Sourced[OperatorResolvedExpression]): Option[String] =
        binders.collectFirst { case (ref, binder) if ref eq a.asInstanceOf[AnyRef] => binder }

      def rebuild(n: Sourced[OperatorResolvedExpression]): Sourced[OperatorResolvedExpression] = n.value match {
        case FunctionApplication(t, a) =>
          val t2 = t.value match {
            case FunctionApplication(lt, la) if isBareLambda(lt.value) && isTarget(bindLets, la) => rewrite(t)
            case FunctionApplication(_, _)                                                      => rebuild(t)
            case _                                                                              => rewrite(t)
          }
          val a2 = binderFor(a) match {
            case Some(binder) => a.as(ParameterReference(a.as(binder)))
            case None         => rewrite(a)
          }
          if ((t2 eq t) && (a2 eq a)) n else n.as(FunctionApplication(t2, a2))
        case _                         => rewrite(n)
      }

      val rebuilt = rebuild(node)
      // The core rule with the mode known (the eager `assemble`'s counterpart): a payload core — the resolver read
      // the solved spine result as a rigid non-carrier — becomes the chain's innermost continuation via `pure`.
      val core    =
        if (targets.nonEmpty && targets.forall(pureCore)) rebuilt.as(pureWrap(rebuilt, RegionCarrier.Unspelled))
        else rebuilt
      targets.zip(binders).foldRight(core) { case ((argNode, (_, binder)), acc) =>
        val continuation = acc.as(FunctionLiteral(acc.as(binder), None, acc))
        acc.as(bindNodes(continuation, rewrite(argNode), RegionCarrier.Unspelled))
      }
    }

    rewrite(body)
  }

  /** The largest `$row$N` binder index already present in a body — the eager elaboration's own minting — so the mode
    * splice continues the numbering instead of colliding.
    */
  private def maxRowBinderIndex(expr: OperatorResolvedExpression): Int = expr match {
    case FunctionLiteral(name, _, body)   =>
      val own = if (name.value.startsWith("$row$")) name.value.drop(5).toIntOption.getOrElse(0) else 0
      math.max(own, maxRowBinderIndex(body.value))
    case FunctionApplication(target, arg) => math.max(maxRowBinderIndex(target.value), maxRowBinderIndex(arg.value))
    case _                                => 0
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
