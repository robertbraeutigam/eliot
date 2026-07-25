package com.vanillasource.eliot.eliotc.row

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.*
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The effects-as-rows **elaboration desugar** (docs/effects-as-rows.md §3) — R4 first slice: rewrite a direct-style
  * definition into fully explicit monadic core, the same shape the v2 checker's elaboration produces, so downstream
  * phases are unchanged consumers. **Unwired**: consumed only by its tests until the R5 flip.
  *
  * The rewrite is decision-free — every choice reads *declared* information (the callee's row and slot modes, the
  * argument's derived row) via [[RowChecker]], never a type or an instantiation:
  *
  *   - an **effectful argument at a strict slot** is hoisted: `printLine(readLine)` becomes
  *     `flatMap($row$1 -> printLine($row$1), readLine)`, left-to-right arguments nesting leftmost-outermost;
  *   - an **effectful `val`/statement binding** (the block desugar's applied lambda `(x -> rest)(e)`) becomes
  *     `flatMap(x -> rest', e')`; a pure binding stays an applied lambda;
  *   - a **pure expression in a carrier position** — the innermost continuation of a bind chain, or the body of a
  *     definition with a declared row — is wrapped `pure(expr)`;
  *   - a **suspended or pinned slot** (declared on the callee's `EffectRow`) receives its argument unhoisted — the
  *     computation passes as a value;
  *   - **pure code is untouched**: a definition with an empty declared row and a pure body elaborates to itself,
  *     byte-for-byte — no `Id`, nothing to erase.
  *
  * Slice scope (the shapes the unit suite pins structurally against hand-written monadic twins): strict-slot
  * hoisting, block/`val` sequencing, pure-tail and pure-boundary wrapping, multi-argument ordering. Not yet
  * elaborated (later R4 slices): pure arguments at suspended slots (`pure`-wrapping conditional arms), the
  * discharge-region `Id` instantiation with its boundary `runId` (Appendix A.4), and user-lambda codomains.
  */
object RowElaborator {

  /** Elaborate a definition's runtime body. [[None]] for a body-less value. The result mirrors the original
    * [[Sourced]] positions: inserted machinery nodes are attributed to the expression they wrap.
    */
  def elaborate(orv: OperatorResolvedValue, universe: RowChecker.Universe): Option[Sourced[OperatorResolvedExpression]] =
    orv.runtime.map { runtime =>
      val (paramNames, body) = RowChecker.peelBinders(runtime.value)
      val env                = RowChecker.parameterRowsOf(orv, paramNames)
      val elab               = new Elaboration(env, universe)
      val needCarrier        = RowChecker.declaredRow(orv).nonEmpty
      val newBody            = elab.elaborate(runtime.as(body), needCarrier)
      runtime.as(rewrap(runtime.value, newBody))
    }

  /** Re-attach the peeled parameter binders around the elaborated body. */
  private def rewrap(original: OperatorResolvedExpression, newBody: Sourced[OperatorResolvedExpression]): OperatorResolvedExpression =
    original match {
      case FunctionLiteral(name, tpe, body) => FunctionLiteral(name, tpe, body.as(rewrap(body.value, newBody)))
      case _                                => newBody.value
    }

  /** One definition's elaboration pass: carries the parameter environment and mints the `$row$N` binders. */
  private final class Elaboration(env: Map[String, RowChecker.Row], universe: RowChecker.Universe) {
    private var nextBinder = 0

    /** Elaborate `expr`; when `needCarrier` and the elaborated node is pure, wrap it in `pure(...)` — the uniform
      * rule that covers both the innermost continuation of a bind chain and a pure body under a declared row.
      */
    def elaborate(expr: Sourced[OperatorResolvedExpression], needCarrier: Boolean): Sourced[OperatorResolvedExpression] = {
      val (elaborated, effectful) = core(expr)
      if (needCarrier && !effectful) elaborated.as(pureWrap(elaborated)) else elaborated
    }

    /** Elaborate a node, returning it with its effectfulness (does the *result node* perform a row — a bind chain or
      * an effectful call — as opposed to being a value).
      */
    private def core(expr: Sourced[OperatorResolvedExpression]): (Sourced[OperatorResolvedExpression], Boolean) = {
      val (head, args) = spine(expr.value)
      head match {
        case FunctionLiteral(name, tpe, body) if args.sizeIs == 1 =>
          // The block desugar's binding: `val x = e; rest` / `e; rest` as `(x -> rest)(e)`.
          val bound                    = args.head
          val boundEffectful           = RowChecker.expressionRow(bound.value, env, universe).nonEmpty
          if (boundEffectful) {
            val (boundElab, _)         = core(bound)
            val continuation           = expr.as(FunctionLiteral(name, tpe, elaborate(body, needCarrier = true)))
            (expr.as(bindNodes(continuation, boundElab)), true)
          } else {
            val (boundElab, _)         = core(bound)
            val (bodyElab, bodyEff)    = core(body)
            (expr.as(applyChain(expr.as(FunctionLiteral(name, tpe, bodyElab)), Seq(boundElab))), bodyEff)
          }
        case ValueReference(name, _) if args.nonEmpty             =>
          elaborateCall(expr, name.value, args)
        case ValueReference(name, _)                              =>
          (expr, RowChecker.calleeRow(name.value, universe).nonEmpty && saturatedBare(name.value))
        case _                                                    =>
          // Lambdas (user-written handlers), parameter references, literals: values in this slice.
          (expr, false)
      }
    }

    /** Elaborate a saturated/partial call spine: arguments first, then hoist each effectful strict-slot argument into
      * a `flatMap` around the core call, leftmost argument outermost. The core call is effectful iff the callee is;
      * with hoisted binds around a pure core, the innermost continuation is `pure`-wrapped by the uniform rule.
      */
    private def elaborateCall(
        expr: Sourced[OperatorResolvedExpression],
        callee: ValueFQN,
        args: Seq[Sourced[OperatorResolvedExpression]]
    ): (Sourced[OperatorResolvedExpression], Boolean) = {
      val calleeOrv     = universe.values.get(callee)
      val calleeEff     = args.size >= calleeOrv.map(o => SignatureView.of(o.signature).parameters.size).getOrElse(0) &&
        RowChecker.calleeRow(callee, universe).nonEmpty
      val pinned        = calleeOrv.map(_.effectRow.pinnedParameterIndices).getOrElse(Set.empty)
      val suspended     = calleeOrv.map(_.effectRow.parameterEffects.map(_.parameterIndex).toSet).getOrElse(Set.empty)
      val runBoundary   = universe.runBoundaries.contains(callee)

      val (finalArgs, binds) = args.zipWithIndex.foldLeft((Seq.empty[Sourced[OperatorResolvedExpression]], Seq.empty[(String, Sourced[OperatorResolvedExpression])])) {
        case ((accArgs, accBinds), (arg, index)) =>
          val strict    = !pinned.contains(index) && !suspended.contains(index) && !(runBoundary && index == 0)
          val effectful = RowChecker.expressionRow(arg.value, env, universe).nonEmpty
          if (strict && effectful) {
            val binder      = freshBinder()
            val (argElab, _) = core(arg)
            (accArgs :+ arg.as(ParameterReference(arg.as(binder))), accBinds :+ (binder -> argElab))
          } else {
            val (argElab, _) = core(arg)
            (accArgs :+ argElab, accBinds)
          }
      }

      if (binds.isEmpty) {
        (expr.as(applyChain(expr.as(spine(expr.value)._1), finalArgs)), calleeEff)
      } else {
        val coreCall = expr.as(applyChain(expr.as(spine(expr.value)._1), finalArgs))
        val coreElab = if (calleeEff) coreCall else coreCall.as(pureWrap(coreCall))
        val wrapped  = binds.foldRight(coreElab) { case ((binder, action), acc) =>
          val continuation = acc.as(FunctionLiteral(acc.as(binder), None, acc))
          acc.as(bindNodes(continuation, action))
        }
        (wrapped, true)
      }
    }

    /** A bare (argument-less) reference performs its row only when the callee is nullary — an under-applied function
      * reference is a value whose row is latent.
      */
    private def saturatedBare(callee: ValueFQN): Boolean =
      universe.values.get(callee).forall(o => SignatureView.of(o.signature).parameters.isEmpty)

    private def freshBinder(): String = {
      nextBinder += 1
      s"$$row$$$nextBinder"
    }
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
}
