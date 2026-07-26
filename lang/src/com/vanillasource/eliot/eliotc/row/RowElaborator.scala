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
  * chain, a discharger call, a suspended-parameter reference) or is a plain value. A call is carrier-valued exactly
  * when its callee's declared return is headed by one of the callee's own carrier binders (`readLine : F[Str]`,
  * `catchX : G[A]`) — declared shape, never a name/kind guess. The rules:
  *
  *   - a **carrier-valued argument at a strict slot** is hoisted under the ambient carrier:
  *     `printLine(readLine)` becomes `flatMap($row$1 -> printLine($row$1), readLine)`, left-to-right arguments
  *     nesting leftmost-outermost;
  *   - a **carrier-valued `val`/statement binding** (the block desugar's applied lambda `(x -> rest)(e)`) becomes
  *     `flatMap(x -> rest', e')`; a pure binding stays an applied lambda;
  *   - a **pure expression in a carrier position** — the innermost continuation of a bind chain, a pure body under a
  *     declared row, a pure argument at a declared-suspended slot — is wrapped `pure(expr)`;
  *   - a **suspended or pinned slot** (declared on the callee's `EffectRow`) receives its argument unhoisted — the
  *     computation passes as a value; a suspended *parameter* referenced in the body is already carrier-valued and is
  *     never re-wrapped;
  *   - **discharge under an empty residual row** (Appendix A.4): where a carrier-valued node meets a region with no
  *     ambient carrier — a pure definition's body, binding, or strict argument slot — the region's base carrier is
  *     `Id` by declaration, and the node is unwrapped with `runId(...)` at that same boundary. Under a non-empty
  *     residual row there is nothing to unwrap: the discharger's result rides the ambient carrier like any effectful
  *     call. `Id` never appears anywhere else;
  *   - a **handler lambda at a carrier-codomain slot** (the declared parameter is an arrow ending in the callee's own
  *     carrier binder, `onError: E => G[A]`) elaborates its body as a carrier region: an effectful body is already a
  *     computation, a pure body is `pure`-wrapped;
  *   - **pure code is untouched**: a definition with an empty declared row and a pure body elaborates to itself,
  *     byte-for-byte — no `Id`, nothing to erase.
  *
  * Not yet elaborated (later R4 slices): lambdas at plain (latent-row) arrow slots and `{Effect}`-marker callback
  * arrows, compound computations at pinned slots, pinned-*return* definitions, and the end-to-end shadow compile of
  * elaborated output.
  */
object RowElaborator {

  /** Elaborate a definition's runtime body. [[None]] for a body-less value. The result mirrors the original
    * [[Sourced]] positions: inserted machinery nodes are attributed to the expression they wrap.
    */
  def elaborate(orv: OperatorResolvedValue, universe: RowChecker.Universe): Option[Sourced[OperatorResolvedExpression]] =
    orv.runtime.map { runtime =>
      val (paramNames, body) = RowChecker.peelBinders(runtime.value)
      val env                = RowChecker.parameterRowsOf(orv, paramNames)
      val ambient            = RowChecker.declaredRow(orv).nonEmpty
      val elab               = new Elaboration(env, universe, ambient)
      val newBody            = elab.elaborate(runtime.as(body), needCarrier = ambient)
      runtime.as(rewrap(runtime.value, newBody))
    }

  /** Re-attach the peeled parameter binders around the elaborated body. */
  private def rewrap(original: OperatorResolvedExpression, newBody: Sourced[OperatorResolvedExpression]): OperatorResolvedExpression =
    original match {
      case FunctionLiteral(name, tpe, body) => FunctionLiteral(name, tpe, body.as(rewrap(body.value, newBody)))
      case _                                => newBody.value
    }

  /** One definition's elaboration pass: carries the parameter environment, the region's ambient-carrier-ness (a
    * declared non-empty row — the flag that decides bind-vs-`runId` for carrier-valued nodes at value positions), and
    * mints the `$row$N` binders.
    */
  private final class Elaboration(env: Map[String, RowChecker.Row], universe: RowChecker.Universe, ambient: Boolean) {
    private var nextBinder = 0

    /** Elaborate `expr` for a position of known polarity: a carrier position (`needCarrier`) `pure`-wraps a pure
      * node; a value position unwraps a carrier-valued node with `runId` — which is reached only in a region with no
      * ambient carrier (a pure definition's boundary), where the residual carrier is `Id` by declaration (A.4).
      */
    def elaborate(expr: Sourced[OperatorResolvedExpression], needCarrier: Boolean): Sourced[OperatorResolvedExpression] = {
      val (elaborated, carrierValued) = core(expr)
      if (needCarrier && !carrierValued) elaborated.as(pureWrap(elaborated))
      else if (!needCarrier && carrierValued) elaborated.as(runIdWrap(elaborated))
      else elaborated
    }

    /** Elaborate a node, returning it with its carrier-valued-ness (is the *result node* a carrier computation — a
      * bind chain, an effectful or discharging call, a suspended-parameter reference — as opposed to a plain value).
      */
    private def core(expr: Sourced[OperatorResolvedExpression]): (Sourced[OperatorResolvedExpression], Boolean) = {
      val (head, args) = spine(expr.value)
      head match {
        case FunctionLiteral(name, tpe, body) if args.sizeIs == 1 =>
          // The block desugar's binding: `val x = e; rest` / `e; rest` as `(x -> rest)(e)`.
          val bound                       = args.head
          val (boundElab, boundCarrier)   = core(bound)
          if (boundCarrier && ambient) {
            val continuation = expr.as(FunctionLiteral(name, tpe, elaborate(body, needCarrier = true)))
            (expr.as(bindNodes(continuation, boundElab)), true)
          } else {
            // No ambient carrier: a carrier-valued binding is a discharge whose base is `Id` — unwrap at the binding.
            val boundFinal          = if (boundCarrier) boundElab.as(runIdWrap(boundElab)) else boundElab
            val (bodyElab, bodyCar) = core(body)
            (expr.as(applyChain(expr.as(FunctionLiteral(name, tpe, bodyElab)), Seq(boundFinal))), bodyCar)
          }
        case ValueReference(name, _) if args.nonEmpty             =>
          elaborateCall(expr, name.value, args)
        case ValueReference(name, _)                              =>
          (expr, saturatedBare(name.value) && calleeCarrierValued(name.value))
        case ParameterReference(name) if args.isEmpty             =>
          // A suspended parameter holds its computation unrun: referencing it yields a carrier value.
          (expr, env.getOrElse(name.value, Set.empty).nonEmpty)
        case _                                                    =>
          // Lambdas at plain slots, applied parameter references, literals: values in this slice.
          (expr, false)
      }
    }

    /** Elaborate a saturated/partial call spine: arguments by their declared slot mode, then hoist each carrier-valued
      * strict-slot argument into a `flatMap` around the core call (leftmost argument outermost) — or, with no ambient
      * carrier to hoist under, unwrap it in place with `runId`. The core call is carrier-valued iff the callee's
      * declared return is; with hoisted binds around a pure core, the innermost continuation is `pure`-wrapped by the
      * uniform rule.
      */
    private def elaborateCall(
        expr: Sourced[OperatorResolvedExpression],
        callee: ValueFQN,
        args: Seq[Sourced[OperatorResolvedExpression]]
    ): (Sourced[OperatorResolvedExpression], Boolean) = {
      val calleeOrv     = universe.values.get(callee)
      val calleeView    = calleeOrv.map(o => SignatureView.of(o.signature))
      val saturated     = args.size >= calleeView.map(_.parameters.size).getOrElse(0)
      val calleeCarrier = saturated && calleeCarrierValued(callee)
      val pinned        = calleeOrv.map(_.effectRow.pinnedParameterIndices).getOrElse(Set.empty)
      val suspended     = calleeOrv.map(_.effectRow.parameterEffects.map(_.parameterIndex).toSet).getOrElse(Set.empty)
      val runBoundary   = universe.runBoundaries.contains(callee)
      val calleeBinders = calleeView.map(EffectCarriers.carrierBinders).getOrElse(Set.empty)

      val (finalArgs, binds) = args.zipWithIndex.foldLeft((Seq.empty[Sourced[OperatorResolvedExpression]], Seq.empty[(String, Sourced[OperatorResolvedExpression])])) {
        case ((accArgs, accBinds), (arg, index)) =>
          val declaredSlot = calleeView.flatMap(_.parameters.lift(index))
          if (pinned.contains(index) || (runBoundary && index == 0)) {
            // A pinned/run-boundary slot captures the computation whole.
            (accArgs :+ core(arg)._1, accBinds)
          } else if (suspended.contains(index)) {
            // A declared-suspended slot receives a computation: an effectful argument passes unrun; a pure argument
            // is lifted into the carrier (`if(c, "a")`'s pure arm becomes `pure("a")`).
            (accArgs :+ elaborate(arg, needCarrier = true), accBinds)
          } else if (isBareLambda(arg.value) && declaredSlot.exists(s => carrierCodomain(s.value, calleeBinders))) {
            // A handler at a carrier-codomain slot (`onError: E => G[A]`): its body is a carrier region.
            (accArgs :+ arg.as(elaborateLambda(arg)), accBinds)
          } else {
            val (argElab, argCarrier) = core(arg)
            if (argCarrier && ambient) {
              val binder = freshBinder()
              (accArgs :+ arg.as(ParameterReference(arg.as(binder))), accBinds :+ (binder -> argElab))
            } else if (argCarrier) {
              // No ambient carrier: the argument is a discharge at base `Id` — unwrap in place (A.4).
              (accArgs :+ argElab.as(runIdWrap(argElab)), accBinds)
            } else {
              (accArgs :+ argElab, accBinds)
            }
          }
      }

      if (binds.isEmpty) {
        (expr.as(applyChain(expr.as(spine(expr.value)._1), finalArgs)), calleeCarrier)
      } else {
        val coreCall = expr.as(applyChain(expr.as(spine(expr.value)._1), finalArgs))
        val coreElab = if (calleeCarrier) coreCall else coreCall.as(pureWrap(coreCall))
        val wrapped  = binds.foldRight(coreElab) { case ((binder, action), acc) =>
          val continuation = acc.as(FunctionLiteral(acc.as(binder), None, acc))
          acc.as(bindNodes(continuation, action))
        }
        (wrapped, true)
      }
    }

    /** Whether a saturated reference to `callee` yields a carrier computation: its declared return is headed by one
      * of its own carrier binders (`readLine : F[Str]`, a discharger's `G[A]` — declared shape, tag-free). For a
      * callee outside the universe (an effect-ability method resolved by qualifier only) the declared row decides.
      */
    private def calleeCarrierValued(callee: ValueFQN): Boolean =
      universe.values.get(callee) match {
        case Some(orv) =>
          val view = SignatureView.of(orv.signature)
          carrierHeaded(view.returnType.value, EffectCarriers.carrierBinders(view))
        case None      =>
          RowChecker.calleeRow(callee, universe).nonEmpty
      }

    /** Rebuild a handler lambda with its (fully peeled) body elaborated as a carrier region. */
    private def elaborateLambda(lambda: Sourced[OperatorResolvedExpression]): OperatorResolvedExpression = {
      val (_, inner) = RowChecker.peelBinders(lambda.value)
      rewrap(lambda.value, elaborate(lambda.as(inner), needCarrier = true))
    }

    /** A bare (argument-less) reference performs its row only when the callee is nullary — an under-applied function
      * reference is a value whose row is latent.
      */
    private def saturatedBare(callee: ValueFQN): Boolean =
      universe.values.get(callee).forall(o => SignatureView.of(o.signature).parameters.isEmpty)

    /** Whether a declared parameter type is an arrow whose final codomain is carrier-headed (`E => G[A]`). */
    private def carrierCodomain(paramType: OperatorResolvedExpression, carriers: Set[String]): Boolean =
      asArrowLike(paramType).exists { case (_, cod) => carrierHeaded(finalCodomain(cod.value), carriers) }

    private def finalCodomain(tpe: OperatorResolvedExpression): OperatorResolvedExpression =
      asArrowLike(tpe) match {
        case Some((_, cod)) => finalCodomain(cod.value)
        case None           => tpe
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
