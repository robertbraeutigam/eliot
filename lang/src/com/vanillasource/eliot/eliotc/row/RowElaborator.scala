package com.vanillasource.eliot.eliotc.row

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.effect.processor.EffectCarriers
import com.vanillasource.eliot.eliotc.module.fact.{ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.*
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The effects-as-rows **elaboration desugar** (docs/effects-as-rows.md §3) — R4: rewrite a direct-style definition
  * into fully explicit monadic core, the same shape the v2 checker's elaboration produces, so downstream phases are
  * unchanged consumers. **Unwired**: consumed only by its tests until the R5 flip.
  *
  * The rewrite is decision-free — every choice reads *declared* information (the callee's row, slot modes and
  * signature shape via [[RowChecker]] and the R2 `EffectRow` metadata), never a type or an instantiation. The central
  * notion is **carrier-valued-ness**: an elaborated node either *is* a carrier computation (an effectful call, a bind
  * chain, a discharger call, a suspended-parameter reference) or is a plain value. Recognition is by declared shape:
  *
  *   - a *call* is carrier-valued when its callee's declared return is headed by one of the callee's own carrier
  *     binders (`readLine : F[Str]`, `catchX : G[A]`) — or when the return is a *bare* generic binder that a lambda
  *     argument's declared arrow codomain instantiates at a carrier (the generic-eliminator shape `apply(f, a) : B`
  *     with an effectful `f`);
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
  *   - a **carrier-valued argument at a strict slot** is hoisted under the region's carrier:
  *     `printLine(readLine)` becomes `flatMap($row$1 -> printLine($row$1), readLine)`, left-to-right arguments
  *     nesting leftmost-outermost;
  *   - a **carrier-valued `val`/statement binding** (the block desugar's applied lambda `(x -> rest)(e)`) becomes
  *     `flatMap(x -> rest', e')`; a pure binding stays an applied lambda;
  *   - a **pure expression in a carrier position** — the innermost continuation of a bind chain, a pure body under a
  *     declared row, a pure argument at a declared-suspended, pinned or run-boundary slot — is wrapped `pure(expr)`;
  *   - **discharge under an empty residual row** (Appendix A.4): where a carrier-valued node meets a value position
  *     in a region with *no* carrier — a pure definition's body, binding, or strict argument slot — the region's
  *     base carrier is `Id` by declaration, and the node is unwrapped with `runId(...)` at that same boundary. In a
  *     carrier region it binds like any effectful call. `Id` never appears anywhere else;
  *   - a **lambda argument** elaborates its body: *forced* to a carrier region at a declared carrier-codomain slot
  *     (`onError: E => G[A]`, `action: A => {Effect} Unit` — a pure body is `pure`-wrapped), *naturally* at a plain
  *     arrow slot (an effectful body becomes a bind chain, a pure body is untouched);
  *   - **pure code is untouched**: a definition with an empty declared row and a pure body elaborates to itself,
  *     byte-for-byte — no `Id`, nothing to erase.
  *
  * Not yet elaborated (later R4 slice): the end-to-end shadow compile of elaborated output.
  */
object RowElaborator {

  /** Elaborate a definition's runtime body. [[None]] for a body-less value. The result mirrors the original
    * [[Sourced]] positions: inserted machinery nodes are attributed to the expression they wrap.
    */
  def elaborate(orv: OperatorResolvedValue, universe: RowChecker.Universe): Option[Sourced[OperatorResolvedExpression]] =
    orv.runtime.map { runtime =>
      val (paramNames, body) = RowChecker.peelBinders(runtime.value)
      val view               = SignatureView.of(orv.signature)
      val ownBinders         = EffectCarriers.carrierBinders(view)
      val paramTypes         = paramNames.takeRight(view.parameters.size).zip(view.parameters.map(_.value)).toMap
      val topCarrier         = carrierHeaded(view.returnType.value, ownBinders) ||
        orv.effectRow.returnPinnedEffects.nonEmpty ||
        runCarrierReturn(view, universe)
      val elab               = new Elaboration(paramTypes, ownBinders, universe)
      val newBody            = elab.elaborate(runtime.as(body), needCarrier = topCarrier, region = topCarrier)
      runtime.as(rewrap(runtime.value, newBody))
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
      ownBinders: Set[String],
      universe: RowChecker.Universe
  ) {
    private var nextBinder = 0

    /** Elaborate `expr` for a position of known polarity: a carrier position (`needCarrier`) `pure`-wraps a pure
      * node; a value position unwraps a carrier-valued node with `runId` — which is reached only in a region with no
      * carrier (a pure definition's boundary), where the residual carrier is `Id` by declaration (A.4).
      */
    def elaborate(
        expr: Sourced[OperatorResolvedExpression],
        needCarrier: Boolean,
        region: Boolean
    ): Sourced[OperatorResolvedExpression] = {
      val (elaborated, carrierValued) = core(expr, region)
      if (needCarrier && !carrierValued) elaborated.as(pureWrap(elaborated))
      else if (!needCarrier && carrierValued) elaborated.as(runIdWrap(elaborated))
      else elaborated
    }

    /** Elaborate a node, returning it with its carrier-valued-ness (is the *result node* a carrier computation — a
      * bind chain, an effectful or discharging call, a suspended-parameter reference — as opposed to a plain value).
      */
    private def core(
        expr: Sourced[OperatorResolvedExpression],
        region: Boolean
    ): (Sourced[OperatorResolvedExpression], Boolean) = {
      val (head, args) = spine(expr.value)
      head match {
        case FunctionLiteral(name, tpe, body) if args.sizeIs == 1 =>
          // The block desugar's binding: `val x = e; rest` / `e; rest` as `(x -> rest)(e)`.
          val bound                     = args.head
          val (boundElab, boundCarrier) = core(bound, region)
          if (boundCarrier && region) {
            val continuation = expr.as(FunctionLiteral(name, tpe, elaborate(body, needCarrier = true, region)))
            (expr.as(bindNodes(continuation, boundElab)), true)
          } else {
            // No region carrier: a carrier-valued binding is a discharge whose base is `Id` — unwrap at the binding.
            val boundFinal          = if (boundCarrier) boundElab.as(runIdWrap(boundElab)) else boundElab
            val (bodyElab, bodyCar) = core(body, region)
            (expr.as(applyChain(expr.as(FunctionLiteral(name, tpe, bodyElab)), Seq(boundFinal))), bodyCar)
          }
        case _: FunctionLiteral if args.isEmpty                   =>
          // A bare lambda in a non-slot position: elaborate its body naturally; the lambda itself is a value.
          (elaborateLambdaNatural(expr, region)._1, false)
        case ValueReference(name, _) if args.nonEmpty             =>
          elaborateCall(expr, name.value, args, region)
        case ValueReference(name, _)                              =>
          (expr, calleeCarrierValued(name.value, argCount = 0))
        case ParameterReference(name) if args.isEmpty             =>
          // A suspended parameter (declared carrier-headed) holds its computation unrun: referencing it yields a
          // carrier value.
          (expr, paramTypes.get(name.value).exists(t => carrierHeaded(t, ownBinders)))
        case ParameterReference(name)                             =>
          // Calling a function-typed parameter: carrier-valued when its declared arrow's final codomain is
          // carrier-headed and the call saturates it (`action(s)` inside a callback-taking definition).
          elaborateArguments(expr, args, resultCarrier = paramCallCarrier(name.value, args.size), region)
        case _                                                    =>
          (expr, false)
      }
    }

    /** Elaborate a call to a named value: arguments by their declared slot mode, then hoist each carrier-valued
      * strict-slot argument into a `flatMap` around the core call (leftmost argument outermost) — or, with no region
      * carrier to hoist under, unwrap it in place with `runId`. The core call is carrier-valued iff the callee's
      * declared return is — including a bare-generic return a lambda argument's declared codomain instantiates at a
      * carrier.
      */
    private def elaborateCall(
        expr: Sourced[OperatorResolvedExpression],
        callee: ValueFQN,
        args: Seq[Sourced[OperatorResolvedExpression]],
        region: Boolean
    ): (Sourced[OperatorResolvedExpression], Boolean) = {
      val calleeOrv     = universe.values.get(callee)
      val calleeView    = calleeOrv.map(o => SignatureView.of(o.signature))
      val calleeCarrier = calleeCarrierValued(callee, args.size)
      val pinned        = calleeOrv.map(_.effectRow.pinnedParameterIndices).getOrElse(Set.empty)
      val runBoundary   = universe.runBoundaries.contains(callee)
      val calleeBinders = calleeView.map(EffectCarriers.carrierBinders).getOrElse(Set.empty)

      val (finalArgs, binds, instantiated) =
        args.zipWithIndex.foldLeft(
          (Seq.empty[Sourced[OperatorResolvedExpression]], Seq.empty[(String, Sourced[OperatorResolvedExpression])], Set.empty[String])
        ) { case ((accArgs, accBinds, accInst), (arg, index)) =>
          val declaredSlot = calleeView.flatMap(_.parameters.lift(index)).map(_.value)
          if (pinned.contains(index) || (runBoundary && index == 0)) {
            // A pinned/run-boundary slot captures the computation whole — and is its own carrier region: internal
            // binds ride the pinned/run stack even under a pure definition; a pure argument lifts via `pure`.
            (accArgs :+ elaborate(arg, needCarrier = true, region = true), accBinds, accInst)
          } else if (declaredSlot.exists(s => carrierHeaded(s, calleeBinders))) {
            // A declared-suspended slot (carrier-headed parameter type) receives a computation on the *caller's*
            // carrier: an effectful argument passes unrun; a pure argument is lifted (`if(c, "a")` ⇒ `pure("a")`).
            (accArgs :+ elaborate(arg, needCarrier = true, region), accBinds, accInst)
          } else if (isBareLambda(arg.value)) {
            if (declaredSlot.exists(s => carrierCodomain(s, calleeBinders))) {
              // A handler/callback at a carrier-codomain slot (`onError: E => G[A]`, `action: A => {Effect} Unit`):
              // its body is a carrier region — a pure body lifts via `pure`.
              (accArgs :+ elaborateLambdaForced(arg), accBinds, accInst)
            } else {
              // A lambda at a plain arrow slot elaborates naturally; a carrier-valued body instantiates the slot's
              // bare-generic codomain at a carrier — recorded for the return shape (the generic-eliminator rule).
              val (lamElab, bodyCarrier) = elaborateLambdaNatural(arg, region)
              val inst                   = declaredSlot
                .filter(_ => bodyCarrier)
                .flatMap(s => bareGenericCodomain(s, calleeBinders))
              (accArgs :+ lamElab, accBinds, accInst ++ inst)
            }
          } else {
            val (accA, accB) = strictArgument(accArgs, accBinds, arg, region)
            (accA, accB, accInst)
          }
        }

      val resultCarrier = calleeCarrier ||
        calleeView.exists(v => bareGenericReturn(v).exists(instantiated.contains))
      (assemble(expr, finalArgs, binds, resultCarrier), binds.nonEmpty || resultCarrier)
    }

    /** Elaborate a call with no declared slot information (an applied function-typed parameter): every slot strict. */
    private def elaborateArguments(
        expr: Sourced[OperatorResolvedExpression],
        args: Seq[Sourced[OperatorResolvedExpression]],
        resultCarrier: Boolean,
        region: Boolean
    ): (Sourced[OperatorResolvedExpression], Boolean) = {
      val (finalArgs, binds) =
        args.foldLeft((Seq.empty[Sourced[OperatorResolvedExpression]], Seq.empty[(String, Sourced[OperatorResolvedExpression])])) {
          case ((accArgs, accBinds), arg) => strictArgument(accArgs, accBinds, arg, region)
        }
      (assemble(expr, finalArgs, binds, resultCarrier), binds.nonEmpty || resultCarrier)
    }

    /** One strict-slot argument: a carrier-valued argument is hoisted under the region's carrier; with no region
      * carrier it passes through *unchanged* — `runId` is inserted only at the two boundaries the v2 checker
      * defaults at (a definition's pure return, a `val` binding), never at an argument slot, where the carrier must
      * instead flow to the slot's expected type (the hand-monadic `runId(runAbort(x))` shape: the explicit accessor
      * absorbs the still-flex base). A value passes inline.
      */
    private def strictArgument(
        accArgs: Seq[Sourced[OperatorResolvedExpression]],
        accBinds: Seq[(String, Sourced[OperatorResolvedExpression])],
        arg: Sourced[OperatorResolvedExpression],
        region: Boolean
    ): (Seq[Sourced[OperatorResolvedExpression]], Seq[(String, Sourced[OperatorResolvedExpression])]) = {
      val (argElab, argCarrier) = core(arg, region)
      if (argCarrier && region) {
        val binder = freshBinder()
        (accArgs :+ arg.as(ParameterReference(arg.as(binder))), accBinds :+ (binder -> argElab))
      } else {
        (accArgs :+ argElab, accBinds)
      }
    }

    /** Reassemble a call from its elaborated arguments and hoisted binds: the core call, `pure`-wrapped when binds
      * exist around a non-carrier core (the innermost continuation must be a computation), then the binds folded
      * rightward so the leftmost argument's action is outermost.
      */
    private def assemble(
        expr: Sourced[OperatorResolvedExpression],
        finalArgs: Seq[Sourced[OperatorResolvedExpression]],
        binds: Seq[(String, Sourced[OperatorResolvedExpression])],
        resultCarrier: Boolean
    ): Sourced[OperatorResolvedExpression] = {
      val coreCall = expr.as(applyChain(expr.as(spine(expr.value)._1), finalArgs))
      if (binds.isEmpty) coreCall
      else {
        val coreElab = if (resultCarrier) coreCall else coreCall.as(pureWrap(coreCall))
        binds.foldRight(coreElab) { case ((binder, action), acc) =>
          val continuation = acc.as(FunctionLiteral(acc.as(binder), None, acc))
          acc.as(bindNodes(continuation, action))
        }
      }
    }

    /** Whether applying `callee` to `argCount` arguments yields a carrier computation: the declared type remaining
      * after consuming the arguments — the return, further arrow-applied for arguments beyond the declared
      * parameters (an over-applied accessor: `runStateCarrier(fa)(s)` applies the accessor's `S => G[Pair[A, S]]`
      * result) — is headed by one of the callee's own carrier binders or by a platform *run carrier*
      * (`prog : IO[Pair[..]]`, the nominal-run spelling — Appendix A.7). Declared shape, tag-free; an under-applied
      * reference is a value. For a callee outside the universe (an effect-ability method resolved by qualifier only)
      * the declared row decides.
      */
    private def calleeCarrierValued(callee: ValueFQN, argCount: Int): Boolean =
      universe.values.get(callee) match {
        case Some(orv) =>
          val view = SignatureView.of(orv.signature)
          argCount >= view.parameters.size && {
            val applied = arrowApplied(view.returnType.value, argCount - view.parameters.size)
            carrierHeaded(applied, EffectCarriers.carrierBinders(view)) || runCarrierHead(applied)
          }
        case None      =>
          RowChecker.calleeRow(callee, universe).nonEmpty
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
    private def elaborateLambdaForced(lambda: Sourced[OperatorResolvedExpression]): Sourced[OperatorResolvedExpression] = {
      val (_, inner) = RowChecker.peelBinders(lambda.value)
      lambda.as(rewrap(lambda.value, elaborate(lambda.as(inner), needCarrier = true, region = true)))
    }

    /** Rebuild a lambda with its (fully peeled) body elaborated naturally, returning whether the body came out
      * carrier-valued (an effectful body is a bind chain; a pure body is untouched).
      */
    private def elaborateLambdaNatural(
        lambda: Sourced[OperatorResolvedExpression],
        region: Boolean
    ): (Sourced[OperatorResolvedExpression], Boolean) = {
      val (_, inner)                = RowChecker.peelBinders(lambda.value)
      val (innerElab, innerCarrier) = core(lambda.as(inner), region)
      (lambda.as(rewrap(lambda.value, innerElab)), innerCarrier)
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

    /** The *bare generic* final codomain of a declared arrow parameter type — a plain (non-carrier) binder name, the
      * one a carrier-valued lambda body instantiates at a carrier (`f: A => B` gives `B`). [[None]] for a non-arrow
      * or a carrier-headed/concrete codomain.
      */
    private def bareGenericCodomain(paramType: OperatorResolvedExpression, carriers: Set[String]): Option[String] = {
      val (domains, codomain) = arrowChainLike(paramType)
      Option.when(domains.nonEmpty)(codomain).flatMap {
        case ParameterReference(name) if !carriers.contains(name.value) => Some(name.value)
        case _                                                          => None
      }
    }

    /** A callee's return type as a bare generic binder name, if it is one (`weird[A, B](…): B` gives `B`). */
    private def bareGenericReturn(view: SignatureView): Option[String] =
      view.returnType.value match {
        case ParameterReference(name) if !EffectCarriers.carrierBinders(view).contains(name.value) => Some(name.value)
        case _                                                                                     => None
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
              orv             <- universe.values.get(name.value)
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

  /** `flatMap(continuation, action)` spelled by the machinery FQN — the same node the v2 checker splices. */
  private def bindNodes(
      continuation: Sourced[OperatorResolvedExpression],
      action: Sourced[OperatorResolvedExpression]
  ): OperatorResolvedExpression =
    applyChain(
      continuation.as(ValueReference(continuation.as(WellKnownTypes.effectFlatMapFQN))),
      Seq(continuation, action)
    )

  /** `pure(value)` spelled by the machinery FQN. */
  private def pureWrap(value: Sourced[OperatorResolvedExpression]): OperatorResolvedExpression =
    applyChain(value.as(ValueReference(value.as(WellKnownTypes.effectPureFQN))), Seq(value))

  /** `runId(computation)` — the boundary unwrap of a discharge whose residual row is empty (A.4). */
  private def runIdWrap(value: Sourced[OperatorResolvedExpression]): OperatorResolvedExpression =
    applyChain(value.as(ValueReference(value.as(WellKnownTypes.runIdFQN))), Seq(value))
}
