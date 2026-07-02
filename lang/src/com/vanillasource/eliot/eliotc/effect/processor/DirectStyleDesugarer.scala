package com.vanillasource.eliot.eliotc.effect.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.effect.processor.CalleeSignatures.CalleeInfo
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.{
  FunctionApplication,
  FunctionLiteral,
  IntegerLiteral,
  ParameterReference,
  StringLiteral,
  ValueReference,
  applyChain,
  spine
}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The recursive bottom-up auto-lift over one expression: the headline of the effects plan (M3). It rewrites a
  * direct-style body (`printLine(readLine)`) into ordinary monadic form (`flatMap(readLine, x -> printLine(x))`).
  *
  * The single rule: an effectful sub-term (one whose static result is headed by an effect carrier) flowing into a
  * *pure value-argument position* is sequenced with the carrier's `Effect.flatMap` (or `Effect.map`, when the
  * continuation is pure), binding it to a fresh variable. Already-monadic code passes through unchanged (the rewrite
  * is idempotent), and a stored effect action (`flatMap`'s first argument, an `F[A]` storage position) is not bound.
  *
  * "Effectful" is decided structurally from operator-resolved signatures via [[CalleeSignatures]] (this phase runs
  * before monomorphization, which remains the sole arbiter and backstop): a callee's result is effectful once its
  * value parameters are applied and the result is carrier-headed; a parameter reference is effectful iff its declared
  * type is headed by one of the *current value's* `carrier` binders.
  */
class DirectStyleDesugarer(calleeSignatures: CalleeSignatures) {
  import DirectStyleDesugarer.*

  /** Auto-lift `expr`. `env` maps in-scope value parameters to their declared types; `carrier` is the current value's
    * higher-kinded carrier binder names. Fresh-variable numbering is threaded internally, starting from 0.
    */
  def desugar(
      expr: Sourced[OperatorResolvedExpression],
      env: Map[String, OperatorResolvedExpression],
      carrier: Set[String]
  )(using Platform): CompilerIO[Desugared] = desugarExpr(expr, env, carrier, 0)

  /** The recursive worker. Each result also carries the [[Desugared.usedEffects]] performed in its subtree (the union
    * of every effectful callee's abilities), so the declared-effect subset check reuses this single walk instead of a
    * second traversal; `idx` threads fresh-variable numbering so synthesized binders are unique within the body.
    */
  private def desugarExpr(
      expr: Sourced[OperatorResolvedExpression],
      env: Map[String, OperatorResolvedExpression],
      carrier: Set[String],
      idx: Int
  )(using Platform): CompilerIO[Desugared] =
    expr.value match {
      case _: IntegerLiteral | _: StringLiteral         =>
        Desugared(expr, effectful = false, idx).pure[CompilerIO]
      case ParameterReference(name)                     =>
        Desugared(expr, effectful = env.get(name.value).exists(EffectCarriers.carrierHeaded(_, carrier)), idx)
          .pure[CompilerIO]
      case FunctionLiteral(name, paramType, lambdaBody) =>
        desugarExpr(lambdaBody, env - name.value, carrier, idx).map { body =>
          // The lambda *value* is pure (a function); its body may be effectful (e.g. a `flatMap` continuation), and its
          // performed effects still propagate to the enclosing value.
          body.copy(
            expr = expr.as(FunctionLiteral(name, paramType, body.expr)),
            effectful = false,
            synthesizedBind = false
          )
        }
      // A bare value reference is just a zero-argument application; `desugarApplication` handles it (no args to bind).
      case _: ValueReference | _: FunctionApplication   =>
        desugarApplication(expr, env, carrier, idx)
    }

  private def desugarApplication(
      expr: Sourced[OperatorResolvedExpression],
      env: Map[String, OperatorResolvedExpression],
      carrier: Set[String],
      idx: Int
  )(using platform: Platform): CompilerIO[Desugared] = {
    val (head, args) = sourcedSpine(expr)
    head.value match {
      case ValueReference(fqn, _) =>
        for {
          info               <- calleeSignatures.infoFor(fqn, platform)
          (argResults, idxA) <- desugarArgs(args, env, carrier, idx)
          (coreArgs, binds, idxB) = buildArguments(info, argResults, idxA)
          core                    = expr.as(applyChain(head, coreArgs))
          // An eliminator whose branch (a result-typed position, `isBranchPosition`) carries an effectful outcome is
          // itself effectful — the branch value *is* the carrier result — even though its declared return is a bare
          // generic the callee signature cannot see as carrier-headed. This is what lets a guard combinator
          // (`orError = foldOption(o, error(msg), pure)`) stay a branching effect rather than sequencing its branch.
          branchEffectful         = argResults.zipWithIndex.exists { case (a, pos) => a.effectful && info.isBranchPosition(pos) }
          coreEffectful           = info.resultEffectful(args.size) || branchEffectful
          used                    = (if (coreEffectful) info.effectAbilities else Set.empty) ++
                                      argResults.foldMap(_.usedEffects)
        } yield wrapBinds(core, coreEffectful, binds, idxB, used)
      // An immediately-applied lambda `(x -> body)(arg)` is a `let` (the shape every `val`/statement in a `{…}` block
      // lowers to). Its parameter is an ordinary pure value position, so an effectful `arg` flowing in is sequenced —
      // `flatMap`/`map(arg, x -> body)` — exactly as it would be for a named callee's pure parameter. Threading effects
      // through block bindings depends on this case; a non-effectful `arg` (or a carrier-typed binder that deliberately
      // stores the action) is left as the plain immediately-applied lambda.
      case FunctionLiteral(param, paramType, body) if args.sizeIs == 1 =>
        desugarImmediateLambda(expr, param, paramType, body, args.head, env, carrier, idx)
      case _                      =>
        // Non-value-reference head (immediately-applied lambda, applied parameter): no signature to read, so rebuild
        // structurally without binding and let monomorphization arbitrate. Conservative, fail-safe.
        for {
          headRes            <- desugarExpr(head, env, carrier, idx)
          (argResults, idxA) <- desugarArgs(args, env, carrier, headRes.nextIdx)
        } yield Desugared(
          expr.as(applyChain(headRes.expr, argResults.map(_.expr))),
          effectful = false,
          idxA,
          usedEffects = headRes.usedEffects ++ argResults.foldMap(_.usedEffects)
        )
    }
  }

  /** Lower an immediately-applied lambda `(param -> body)(arg)`. If `arg` is effectful and `param` is not a carrier-typed
    * storage position, bind it with `flatMap`/`map` (`map` when the continuation `body` is pure); otherwise keep the
    * plain immediately-applied lambda whose result is the body's result.
    */
  private def desugarImmediateLambda(
      expr: Sourced[OperatorResolvedExpression],
      param: Sourced[String],
      paramType: Option[Sourced[TypeStack[OperatorResolvedExpression]]],
      body: Sourced[OperatorResolvedExpression],
      arg: Sourced[OperatorResolvedExpression],
      env: Map[String, OperatorResolvedExpression],
      carrier: Set[String],
      idx: Int
  )(using Platform): CompilerIO[Desugared] =
    for {
      argRes  <- desugarExpr(arg, env, carrier, idx)
      bodyRes <- desugarExpr(body, env - param.value, carrier, argRes.nextIdx)
    } yield {
      val used = argRes.usedEffects ++ bodyRes.usedEffects
      if (argRes.effectful && !paramType.exists(pt => EffectCarriers.carrierHeaded(pt.value.signature, carrier)))
        Desugared(
          EffectMachinery.sequence(argRes.expr, param.value, bodyRes.expr, bodyRes.effectful),
          effectful = true,
          bodyRes.nextIdx,
          usedEffects = used,
          synthesizedBind = true
        )
      else
        Desugared(
          expr.as(FunctionApplication(expr.as(FunctionLiteral(param, paramType, bodyRes.expr)), argRes.expr)),
          effectful = bodyRes.effectful,
          bodyRes.nextIdx,
          usedEffects = used
        )
    }

  /** Desugar each argument left to right, threading the fresh-variable index across them. */
  private def desugarArgs(
      args: Seq[Sourced[OperatorResolvedExpression]],
      env: Map[String, OperatorResolvedExpression],
      carrier: Set[String],
      idx: Int
  )(using Platform): CompilerIO[(Seq[Desugared], Int)] =
    args.foldLeftM((Seq.empty[Desugared], idx)) { case ((acc, i), arg) =>
      desugarExpr(arg, env, carrier, i).map(r => (acc :+ r, r.nextIdx))
    }

  /** Decide, per argument, whether to bind it (an effectful argument flowing into a concrete value position) or pass it
    * through, minting a fresh variable for each bind. Returns the (possibly substituted) core arguments, the binds in
    * left-to-right order, and the next fresh index.
    */
  private def buildArguments(
      info: CalleeInfo,
      argResults: Seq[Desugared],
      idx: Int
  ): (Seq[Sourced[OperatorResolvedExpression]], Seq[Bind], Int) =
    argResults.zipWithIndex.foldLeft((Seq.empty[Sourced[OperatorResolvedExpression]], Seq.empty[Bind], idx)) {
      case ((coreArgs, binds, i), (arg, pos)) =>
        if (arg.effectful && info.isBindPosition(pos) && !info.isBranchPosition(pos) && !isAuthorMachineryCall(arg)) {
          val name = freshName(i)
          (coreArgs :+ arg.expr.as(ParameterReference(arg.expr.as(name))), binds :+ Bind(name, arg.expr), i + 1)
        } else (coreArgs :+ arg.expr, binds, i)
    }

  /** Whether an argument is an *author-written* machinery call (`pure`/`flatMap`/`map`/`sync`) that this pass did not
    * itself synthesize. The author already lifted such a value into the carrier (it is how transformer instances and
    * hand-written monadic code look), so auto-binding it would un-lift it — leave it alone. The `synthesizedBind` guard
    * is essential: a `flatMap`/`map` *this pass created* while lowering an effectful sub-term (e.g. `url(get)` ⟹
    * `flatMap(get, x -> url(x))`) must still bind when it flows into a pure position (`log(url(get))`). Genuine effects
    * (`abort`/`readLine`) are not machinery and always bind.
    */
  private def isAuthorMachineryCall(arg: Desugared): Boolean =
    !arg.synthesizedBind && (spine(arg.expr.value)._1 match {
      case ValueReference(fqn, _) => EffectMachinery.abilityNameOf(fqn.value).exists(EffectMachinery.isMachineryAbility)
      case _                      => false
    })

  /** Fold the binds around the core, innermost first: the bind nearest the core uses `map` if the core is pure (lifting
    * it into the carrier) and `flatMap` if it is effectful; every outer bind wraps an effectful continuation, so it is
    * always `flatMap`. With no binds the core is returned with its own effectfulness.
    */
  private def wrapBinds(
      core: Sourced[OperatorResolvedExpression],
      coreEffectful: Boolean,
      binds: Seq[Bind],
      idx: Int,
      used: Set[AbilityFQN]
  ): Desugared =
    if (binds.isEmpty) Desugared(core, coreEffectful, idx, usedEffects = used)
    else {
      val (folded, _) = binds.foldRight((core, coreEffectful)) { case (bind, (acc, accEffectful)) =>
        (EffectMachinery.sequence(bind.action, bind.name, acc, accEffectful), true)
      }
      Desugared(folded, effectful = true, idx, usedEffects = used, synthesizedBind = true)
    }
}

object DirectStyleDesugarer {

  /** A desugared sub-expression:
    *   - `expr` — the rewritten expression;
    *   - `effectful` — whether its static result is effectful (carrier-headed);
    *   - `nextIdx` — the next fresh-variable index, threaded so synthesized binders are unique within the body;
    *   - `usedEffects` — the user-facing effect abilities performed anywhere in this subtree, accumulated during the one
    *     desugar walk and read by the declared-effect subset check (so no second traversal is needed);
    *   - `synthesizedBind` — whether this is a `flatMap`/`map` this pass synthesized (vs. an author-written machinery
    *     call), which lets [[DirectStyleDesugarer.isAuthorMachineryCall]] leave authored machinery in place while still
    *     binding the pass's own lowerings.
    */
  case class Desugared(
      expr: Sourced[OperatorResolvedExpression],
      effectful: Boolean,
      nextIdx: Int,
      usedEffects: Set[AbilityFQN] = Set.empty,
      synthesizedBind: Boolean = false
  )

  /** One pending sequencing point: the effectful action and the fresh variable its result binds to. */
  private case class Bind(name: String, action: Sourced[OperatorResolvedExpression])

  private def freshName(idx: Int): String = s"$$eff$$$idx"

  /** Decompose an application into its head (keeping its source position) and left-to-right arguments. */
  private def sourcedSpine(
      expr: Sourced[OperatorResolvedExpression]
  ): (Sourced[OperatorResolvedExpression], Seq[Sourced[OperatorResolvedExpression]]) =
    expr.value match {
      case FunctionApplication(target, arg) =>
        val (head, args) = sourcedSpine(target)
        (head, args :+ arg)
      case _                                => (expr, Seq.empty)
    }
}
