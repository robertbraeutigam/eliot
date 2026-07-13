package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.effect.processor.EffectCarriers
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.{Evaluator, Quoter}
import com.vanillasource.eliot.eliotc.monomorphize.fact.*
import com.vanillasource.eliot.eliotc.monomorphize.unify.{SemValuePrinter, UnifyError}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.SignatureView
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.{compilerAbort, compilerError}

/** Uniform top-down fold over a value's type stack. Each level is processed identically — there is no concept of
  * "generic parameters" as a separate structure.
  *
  * Holds the callbacks and the [[Checker]] as fields so internal helpers don't have to thread them.
  */
class TypeStackLoop(
    fetchBinding: ValueFQN => CompilerIO[Option[SemValue]],
    resolveAbility: (ValueFQN, Seq[GroundValue]) => CompilerIO[Option[(ValueFQN, Seq[GroundValue])]],
    track: Track,
    reduceInstance: (ValueFQN, Seq[GroundValue]) => CompilerIO[Option[SemValue]] = (_, _) => none[SemValue].pure[CompilerIO]
) {
  import TypeStackLoop.{AbilityRef, PassContext, PostDrainPass}

  private val checker = new Checker(fetchBinding, resolveAbility, track)

  def process(
      typeArguments: Seq[GroundValue],
      resolvedValue: OperatorResolvedValue
  ): CompilerIO[TypeStackLoop.Result] =
    processIO(typeArguments, resolvedValue).runA(CheckState.initial)

  /** Test seam: [[process]] but also returning the final [[CheckState]], so checker bookkeeping the
    * [[TypeStackLoop.Result]] does not carry (ambient carrier heads, metavariable roles) can be asserted directly.
    */
  private[monomorphize] def processWithState(
      typeArguments: Seq[GroundValue],
      resolvedValue: OperatorResolvedValue
  ): CompilerIO[(CheckState, TypeStackLoop.Result)] =
    processIO(typeArguments, resolvedValue).run(CheckState.initial)

  private def processIO(
      typeArguments: Seq[GroundValue],
      resolvedValue: OperatorResolvedValue
  ): CheckIO[TypeStackLoop.Result] =
    for {
      // W4 (Limit 5): a calculated return needs a body to calculate from. Reject a body-less calculated return at the
      // definition before its `Type` placeholder reaches a use site (where it would otherwise surface as a confusing
      // mismatch — or, worse, a silent `Type`).
      _ <- failOnAbstractCalculatedReturn(resolvedValue)

      // Walk type stack levels top-down — returns the final SemValue plus one kind-check SemExpression per level
      (signature, levelExprs) <- walkTypeStack(resolvedValue.typeStack)

      // Apply explicit type args
      appliedSig   <- applyTypeArgs(signature, typeArguments, resolvedValue.typeStack)
      instantiated <- instantiateRemaining(appliedSig)

      // Effect-lift bookkeeping: record the value's own *ambient* effect-carrier heads, now that every signature
      // binder is bound in ρ (an explicit type argument to its concrete value, a leftover to its instantiation meta).
      _ <- recordAmbientCarriers(resolvedValue)

      // Compiler-track carrier pinning (CP-D): a `{Throw[E]}` effect carrier in a compile-time value has no runtime
      // carrier to infer, so it is fixed to the compile-time `Throw` carrier `Either[E]` before the body is checked —
      // solving the carrier's instantiation meta lets the body's `pure`/`raise` dispatch resolve and reduce. Runtime
      // track never pins here (its carriers are inferred from the runtime use, or pinned to `IO` at `main`).
      _ <- track.pinCarriers(checker, resolvedValue)

      // Whether the kind check accepted a `{Throw[String]}`-carrier return (a guarded signature, W2b) — captured here,
      // after `walkTypeStack` checked the signature but before the body check could set it for a nested guard.
      sawGuard               <- inspect(_.sawGuardReturn)

      // The return position is settled by `track.settleReturnPosition`, exactly one of three mutually-exclusive outcomes:
      //   - a *calculated* (bare, omittable) return is filled from the body (`installReturnMeta`, W3): its position
      //     becomes a fresh metavariable that checking the body solves to the body's inferred type, then quoted into the
      //     published signature. (An abstract, body-less calculated return cannot calculate; the `calcReturn` guard
      //     below excludes it — that limit is reported earlier by `failOnAbstractCalculatedReturn`.)
      //   - an *effectful-signatures* guard return is discharged off the compile-time `Either[String, _]` carrier
      //     (`dischargeGuardedSignature`, W2b — runtime track): `Right(t)` ⤳ the plain type `t` (so the body checks
      //     against `t` and the published signature is `t` — the `Either` never reaches codegen), `Left(msg)` aborts
      //     with the author message, and a guard stuck on abstract bounds is deferred to the body via a return meta;
      //   - an ordinary explicit return is left as-is (the compiler track, the guard's *producer*, leaves the
      //     undischarged carrier signature `Either[String, Type]` for a consumer to evaluate — a compiler-track guard
      //     discharge would replace the return with a metavariable, starving the body's `pure`/`raise` of its concrete
      //     carrier and blocking the reduction).
      calcReturn              = resolvedValue.calculatedReturn && resolvedValue.runtime.isDefined
      checkResult            <- track.settleReturnPosition(checker, instantiated, calcReturn, sawGuard, resolvedValue)
      (checkSig, returnMeta)  = checkResult

      // Capture ρ now, before `check` binds the runtime value parameters as `FunctionLiteral` binders — so ρ holds only
      // the erased type-stack parameters (each already in its evaluable `groundToSem` form, bound at `applyTypeArgs`
      // time) and the phantom-parameter metas. This is exactly the env that seeds the reification gate in
      // `PostDrainQuoter`; no post-hoc `VConst → groundToSem` rewrite is needed.
      monoEnv <- inspect(_.rho)

      // Check runtime body if present — produces SemExpression with SemValue slots. A body-less value (an abstract
      // declaration) has nothing to check or emit here.
      runtime      <- resolvedValue.runtime.traverse { body =>
                        checker.check(body, checkSig).map(expr => body.as(expr))
                      }

      // Collect all ability-qualified value references from the output trees (runtime + signature levels). These
      // drive the saturation tier of the post-drain pipeline below.
      abilityRefs   = (runtime.toSeq ++ levelExprs).flatMap(checker.abilityResolver.collectAbilityRefs)

      // Post-drain resolution pipeline (D1): the named, tiered sequence that settles every metavariable —
      // saturation fixed-point (drain + ability/Combine resolution), finalization (upper-bounds, carrier kinds,
      // calculated-return fail-safe), the finalizer (default the rest to Type), and a postcondition assertion.
      // See `runPostDrainPipeline`.
      _ <- runPostDrainPipeline(
             PassContext(resolvedValue, abilityRefs, resolvedValue.paramConstraints, returnMeta)
           )
      state <- get
      _     <- state.unifier.errors.reverse.traverse_(err => liftF(reportUnifyError(err, state)))

      // If unification had errors, abort before quoting — no meaningful MonomorphicValue can be produced.
      _ <- if (state.unifier.errors.nonEmpty) liftF(abort[Unit]) else pure(())

      // Compiler backend (CP-C step b): the compiler track reduces its body to a normal form, folding each
      // drain-resolved ability impl's *body* in via NbE — so it needs those impl bindings reachable by the evaluator.
      // `track.implBindings` fetches them once from the track's pool (empty on the runtime track, whose body stays
      // structural for codegen); they are merged ahead of the checker's own binding cache below.
      implBindings <- track.implBindings(fetchBinding)

      // The SemValue analogue of `PostDrainQuoter.resolveAbilityRefs` (which rewrites ability references in a *body*
      // SemExpression): map each drain-resolved ability *method* FQN to its impl body, so an ability call that survives
      // in a type-position SemValue reduces during signature read-back — the case where a guard names an ability method
      // directly (`where equals(E1, E2)`). The impl bodies are already dependency-closed, so a single inline suffices.
      // Keyed by the ability-method FQN; a guard resolves one instance per method, so there is no conflation.
      abilityMethodBindings = state.abilityResolutions.toSeq.flatMap { case (ref, (implFqn, _)) =>
                                implBindings.get(implFqn).map(ref.value -> _)
                              }.toMap

      // Guarded-signature reduction (ability-guards Stage 4): a body-less guard marker's `where` guard reaches its
      // ability *through* an operator (`where E1 != E2` unfolds to `fold(equals(E1,E2), false, true)`), so the `Eq`
      // method `equals` is buried inside `!=`'s body. The type-stack walk evaluated the guard with `!=`'s *generic*
      // native, leaving `equals` a stuck native. To reduce it, reduce each bodied sub-value the guard calls (`!=`)
      // per-instantiation on the compiler track — yielding a `!=[Type]` body with `equals` already resolved — then
      // re-evaluate the guard return against that (so `!=` inlines the reduced form and the guard collapses to a
      // concrete `Bool`). Gated on a body-less guarded signature, so ordinary and W2b (bodied) signatures are untouched.
      guardMarker    = sawGuard && resolvedValue.runtime.isEmpty
      guardBindings <- if (guardMarker) reduceGuardSubValues(levelExprs) else pure(Map.empty[ValueFQN, SemValue])
      finalSig      <- if (guardBindings.nonEmpty) reevaluateGuardReturn(resolvedValue, guardBindings) else pure(checkSig)
      quoterState   <- get

      // Post-drain: quote SemValues to GroundValues using the pre-computed ability resolutions. This is the sole
      // SemValue → GroundValue transition and has no silent fallback; Quoter reports unresolved metas as compiler
      // errors.
      quoter     = new PostDrainQuoter(
                     quoterState.unifier.metaStore,
                     quoterState.abilityResolutions,
                     monoEnv,
                     fqn =>
                       implBindings.get(fqn).orElse(abilityMethodBindings.get(fqn)).orElse(quoterState.bindingCache.getOrElse(fqn, None)),
                     track.platform
                   )
      groundSig <- liftF(quoter.quoteSem(finalSig, resolvedValue.typeStack))
      // The compiler track reduces its body (`reduceSourced`); the runtime track keeps it structural (`quoteSourced`).
      // Narrow-representation reconciliation happens in the backend's `ExpressionCodeGenerator` off the refinement
      // channel, so the body is read back directly.
      monoBody  <- runtime.traverse(srcSem => liftF(track.readBackBody(quoter, srcSem)))
    } yield TypeStackLoop.Result(
      groundSig,
      monoBody.map(sourcedMono => sourcedMono.as(sourcedMono.value.expression))
    )

  /** Reduce the bodied sub-values a guarded signature calls, per instantiation, on the compiler track (ability-guards
    * Stage 4 — see the call site). Walks the checked signature levels for value references whose type arguments are
    * ground and reduces each via [[reduceInstance]] (compiler-track `CompilerMonomorphicValue` → self-contained reduced
    * binding). A ref with no reduced form (a native, a body-less type constructor, an unresolved instance) is skipped.
    * The returned bindings are keyed by FQN and take precedence in the read-back evaluator's lookup, so a guard's `!=`
    * reduces via its `[Type]`-instantiated body (with `equals` resolved) rather than its generic, ability-stuck native.
    */
  private def reduceGuardSubValues(
      levelExprs: Seq[Sourced[SemExpression]]
  ): CheckIO[Map[ValueFQN, SemValue]] =
    for {
      state <- get
      refs   = levelExprs.flatMap(collectValueRefs).distinctBy(_._1.value)
      result <- refs.foldLeftM(Map.empty[ValueFQN, SemValue]) { case (acc, (vfqn, typeArgs)) =>
                  typeArgs.toList.traverse(a => Quoter.quote(0, a, state.unifier.metaStore)) match {
                    case Right(groundArgs) if groundArgs.nonEmpty =>
                      liftF(reduceInstance(vfqn.value, groundArgs)).map {
                        case Some(sem) => acc + (vfqn.value -> sem)
                        case None      => acc
                      }
                    case _                                        => pure(acc)
                  }
                }
    } yield result

  /** Re-evaluate a guard marker's `where` guard with its bodied sub-values reduced per-instantiation
    * (`guardBindings`), so an ability the guard reaches through an operator (`Eq`'s `equals`, via `!=`) resolves and the
    * guard collapses to a concrete `Bool` (ability-guards Stage 4). Seeds `guardBindings` into the binding cache
    * (overriding the sub-values' generic, ability-stuck natives), then evaluates the guard expression — the stripped
    * marker signature's return — against the current env ρ, where the marker's type-parameter binders are already bound
    * to the concrete match arguments. The result is the reduced guard verdict, read back by the caller's quoter.
    */
  private def reevaluateGuardReturn(
      resolvedValue: OperatorResolvedValue,
      guardBindings: Map[ValueFQN, SemValue]
  ): CheckIO[SemValue] =
    for {
      _         <- guardBindings.toList.traverse_ { case (fqn, sem) => modify(_.cacheBinding(fqn, Some(sem))) }
      guardExpr  = SignatureView.of(resolvedValue.typeStack.as(resolvedValue.typeStack.value.signature)).returnType
      reduced   <- checker.evalExpr(guardExpr.value)
    } yield reduced

  /** Collect every [[SemExpression.ValueReference]] in a checked expression tree with its (unquoted) type arguments,
    * descending through applications and lambda bodies.
    */
  private def collectValueRefs(expr: Sourced[SemExpression]): Seq[(Sourced[ValueFQN], Seq[SemValue])] = {
    def go(se: SemExpression): Seq[(Sourced[ValueFQN], Seq[SemValue])] = se.expression match {
      case SemExpression.ValueReference(vfqn, typeArgs)        => Seq((vfqn, typeArgs))
      case SemExpression.FunctionApplication(target, argument) => go(target.value) ++ go(argument.value)
      case SemExpression.FunctionLiteral(_, _, body)           => go(body.value)
      case _                                                   => Seq.empty
    }
    go(expr.value)
  }

  /** W4 (Limit 5): a calculated return is *calculated from the body*; a value with no body the checker can see cannot
    * calculate it, and an output position must not quantify it instead, so the bare return must be stated explicitly.
    * A body-less value here is a truly abstract declaration (`runtime` is `None` — e.g. a platform-layer signature
    * awaiting an implementation): [[CalculatedReturnResolver.installReturnMeta]] would not run, leaving the `Type`
    * placeholder in the signature, so this is reported at the definition rather than letting that placeholder escape.
    */
  private def failOnAbstractCalculatedReturn(resolvedValue: OperatorResolvedValue): CheckIO[Unit] =
    if (resolvedValue.calculatedReturn && resolvedValue.runtime.isEmpty) {
      val name = resolvedValue.vfqn.name.name
      liftF(
        compilerError(
          resolvedValue.name.as(
            s"Abstract declaration '$name' must state its return type explicitly; there is no body to calculate it from."
          ),
          Seq("Add an explicit return type, or provide a concrete implementation.")
        ) >> abort[Unit]
      )
    } else pure(())

  /** Fail-safe for a calculated return (W3) the body did not pin down (Limit 2). After the drain-and-resolve loop the
    * return metavariable should have been solved to the body's inferred type; if it is still a bare metavariable the
    * body left it unconstrained, and if it forced to a stuck neutral the result depends on a variable the inputs do not
    * determine. Either way report a specific error — naming the producer and, for a neutral, the stuck head — rather
    * than letting [[defaultUnsolvedMetas]] silently make the return `Type`. (A return solved to some *other*
    * non-quotable form — an unforced top-level definition, a residual native/lambda — is caught instead by the strict
    * post-drain quoter, which knows precisely which of those are irreducible, so those are left to it.)
    */
  private def failOnUndeterminedCalculatedReturn(
      returnMeta: SemValue.VMeta,
      resolvedValue: OperatorResolvedValue
  ): CheckIO[Unit] =
    checker.force(returnMeta).flatMap {
      case _: SemValue.VMeta                                     =>
        reportUncalculableReturn(resolvedValue, "the body leaves it unconstrained")
      case SemValue.VNeutral(head, _) =>
        reportUncalculableReturn(resolvedValue, s"the result depends on '${head.name}', which the inputs do not determine")
      case _                                                     => pure(())
    }

  private def reportUncalculableReturn(resolvedValue: OperatorResolvedValue, reason: String): CheckIO[Unit] =
    liftF(
      compilerError(
        resolvedValue.name.as(s"Cannot calculate the return type of '${resolvedValue.vfqn.name.name}': $reason."),
        Seq("Write an explicit return type.")
      ) >> abort[Unit]
    )

  /** Finalize every still-unsolved meta by a **total match on its [[MetaRole]]** (D2) — the structural F2 cure. Runs
    * after the drain-and-resolve loop has reached its fixed point, so any meta unification could have determined is
    * already solved. Every still-unsolved meta ([[MetaRole.Plain]] or [[MetaRole.Instantiation]]) defaults to [[VType]].
    * Because the match is exhaustive over the sealed `MetaRole`, a future role added to the ADT forces an explicit
    * decision here rather than silently inheriting the old "default everything to `Type`" behaviour — which is exactly
    * the silent-miscompile path D2 removes.
    */
  private def defaultUnsolvedMetas: CheckIO[Unit] =
    modify { s =>
      val unifier = s.unifier
      val store   = unifier.metaStore
      val solved  = store.entries.foldLeft(store) { case (acc, (rawId, entry)) =>
        entry match {
          case Some(_) => acc // already solved by unification
          case None    =>
            unifier.roleOf(rawId) match {
              case MetaRole.Plain            => acc.solve(SemValue.MetaId(rawId), VType)
              case _: MetaRole.Instantiation => acc.solve(SemValue.MetaId(rawId), VType)
            }
        }
      }
      s.withUnifier(unifier.copy(metaStore = solved))
    }

  /** The named, ordered post-check resolution pipeline (D1).
    *
    * Two registered tiers, plus two fixed structural steps the runner appends:
    *
    *   - SATURATION (fixed-point loop): drain-interleaved ability resolution.
    *   - FINALIZATION (linear, once): upper-bounds discharge, carrier-kind verification, calculated-return fail-safe.
    *   - the FINALIZER ([[defaultUnsolvedMetas]]) and the POSTCONDITION assertion are appended by [[runPostDrainPipeline]],
    *     not listed here — the finalizer is "the step nothing runs after" and is the seam D2 replaces.
    *
    * `drain` is the equality core settling between feature passes and is deliberately not a pass (see
    * [[saturateToFixedPoint]]).
    */
  private val pipeline: List[PostDrainPass] = List(
    PostDrainPass.Saturation(
      "resolve-abilities",
      ctx => checker.abilityResolver.resolveAbilities(ctx.abilityRefs, ctx.paramConstraints)
    ),
    PostDrainPass.Finalization("carrier-kinds", _ => checker.carriers.verifyCarrierKinds),
    PostDrainPass.Finalization(
      "calc-return",
      ctx => ctx.returnMeta.traverse_(failOnUndeterminedCalculatedReturn(_, ctx.resolvedValue))
    )
  )

  /** Run the post-check resolution pipeline to completion: saturate to a fixed point, run the finalization passes
    * once in registration order, apply the finalizer, then assert the postcondition. No semantic change from the
    * former inlined sequence — only the structure is named.
    */
  private def runPostDrainPipeline(ctx: PassContext): CheckIO[Unit] = {
    val saturation   = pipeline.collect { case s: PostDrainPass.Saturation => s }
    val finalization = pipeline.collect { case f: PostDrainPass.Finalization => f }
    for {
      _ <- saturateToFixedPoint(saturation, ctx)
      _ <- finalization.traverse_(_.run(ctx))
      // A finalization pass can commit new solutions (the upper-bounds discharge commits its definitional-equality
      // successes), so the equality core settles once more before defaulting: a constraint postponed against a meta
      // those solutions ground (e.g. an unapplied combinator's codomain against a deferred-then-discharged slot) still
      // resolves instead of its metas defaulting to `Type`.
      _ <- modify(s => s.withUnifier(s.unifier.drain()))
      _ <- defaultUnsolvedMetas
      // Fail-safe (TODO.md): any constraint still postponed after the finalizer is an equality obligation the check
      // never discharged. Flush the queue to hard mismatch errors (triaging benign, now-defaulted constraints away
      // first) rather than silently carrying and forgetting them — the hole that let pre-fix applied-associated-type
      // garbage compile. Runs before the meta-postcondition assertion, which the defaulting already satisfies.
      _ <- modify(s => s.withUnifier(s.unifier.flushPostponed()))
      _ <- assertEveryMetaResolvedOrAbstract(ctx)
    } yield ()
  }

  /** Tier 1: run the saturation passes to a fixed point. Each round drains the unifier *before* each pass — so a pass
    * sees every solution the previous pass injected — and loops while any pass reports progress. This reproduces the
    * former `drainAndResolveLoop`'s `drain → abilities → drain → combines` round exactly. Bounded because each ability
    * (and each combinable meta) resolves at most once, so progress is monotone.
    */
  private def saturateToFixedPoint(passes: List[PostDrainPass.Saturation], ctx: PassContext): CheckIO[Unit] = {
    def round: CheckIO[Boolean] =
      passes.foldLeftM(false) { (acc, p) =>
        for {
          _          <- modify(s => s.withUnifier(s.unifier.drain()))
          progressed <- p.run(ctx)
        } yield acc || progressed
      }
    def loop: CheckIO[Unit] = round.flatMap(if (_) loop else pure(()))
    loop
  }

  /** Postcondition of the post-drain pipeline (D1): every metavariable is solved. The finalizer
    * ([[defaultUnsolvedMetas]]) makes this hold by construction, so it never fires in normal operation; it is the
    * *compiler-bug* backstop for D2's per-role finalization — were a future role added to [[MetaRole]] and forgotten in
    * `defaultUnsolvedMetas`'s total match, a meta could survive unsolved, and this assertion catches it. It
    * [[compilerAbort]]s rather than reporting a user diagnostic because a surviving unsolved meta is an internal
    * invariant violation, not a type error of the user's making.
    */
  private def assertEveryMetaResolvedOrAbstract(ctx: PassContext): CheckIO[Unit] =
    inspect { s =>
      s.unifier.metaStore.entries.collect { case (rawId, None) => rawId }.toList
    }.flatMap {
      case Nil      => pure(())
      case unsolved =>
        liftF(
          compilerAbort[Unit](
            ctx.resolvedValue.name.as(
              s"Internal: post-drain pipeline left metavariables unresolved: ${unsolved.mkString(", ")}."
            )
          )
        )
    }

  /** Walk the type stack top-down, folding each level against the one above as its expected kind.
    *
    * For each level we (a) kind-check the ORE against the running `expected` so ill-kinded signatures surface as errors
    * attached to the correct source, and (b) evaluate the ORE to obtain the next-lower level's expected kind. The final
    * fold result is the signature's [[SemValue]] together with the per-level kind-check [[SemExpression]]s, which the
    * drain-and-resolve loop walks to discover ability refs embedded in type positions.
    */
  private def walkTypeStack(
      typeStack: Sourced[TypeStack[OperatorResolvedExpression]]
  ): CheckIO[(SemValue, Seq[Sourced[SemExpression]])] =
    typeStack.value.levels.toSeq.reverse.foldLeftM((VType: SemValue, Seq.empty[Sourced[SemExpression]])) {
      case ((expected, acc), level) =>
        for {
          checked <- checker.check(typeStack.as(level), expected)
          next    <- checker.evalExpr(level)
        } yield (next, acc :+ typeStack.as(checked))
    }

  /** Instantiate any remaining VLam closures (unapplied type parameters) with fresh metas, binding each in the env.
    * This handles phantom type parameters and cases where fewer explicit type args were provided than type parameters
    * exist.
    */
  private def instantiateRemaining(sig: SemValue): CheckIO[SemValue] =
    checker.peelLams(sig, bindInEnv = true).map(_._1)

  /** Record the value-under-check's own *ambient* effect-carrier heads into [[CheckState.ambientCarriers]]. The
    * carrier binders are the signature's higher-kinded, ability-constrained binders (the M1 `{E...}` carrier — the
    * same `carrierBinders ∩ paramConstraints` filter the effect phase's ambient read uses, which excludes a bare
    * generic `C[_, _]`). Each is looked up in ρ — where [[applyTypeArgs]] bound an explicit argument's concrete value
    * and [[instantiateRemaining]] a leftover binder's instantiation meta — and recorded by its forced head: a
    * [[CheckState.CarrierHead.TopDef]] FQN for a concrete instantiation (`IO`), a [[CheckState.CarrierHead.Meta]] id
    * for a peeled one. Any other head shape is not a carrier and is skipped. Read by the checker-side effect lift.
    */
  private def recordAmbientCarriers(resolvedValue: OperatorResolvedValue): CheckIO[Unit] = {
    val view         = SignatureView.of(resolvedValue.typeStack.as(resolvedValue.typeStack.value.signature))
    val carrierNames = EffectCarriers.carrierBinders(view).filter(resolvedValue.paramConstraints.contains)
    if (carrierNames.isEmpty) pure(())
    else
      for {
        state <- get
        heads  = carrierNames.flatMap(name =>
                   state.rho
                     .lookupByName(name)
                     .map(value => Evaluator.force(value, state.unifier.metaStore))
                     .collect {
                       case VTopDef(fqn, _, _) => CheckState.CarrierHead.TopDef(fqn)
                       case VMeta(id, _)       => CheckState.CarrierHead.Meta(id.value)
                     }
                 )
        _     <- modify(_.recordAmbientCarriers(heads))
      } yield ()
  }

  /** Apply concrete GroundValue type arguments to the signature by converting each to a SemValue and applying to VLam
    * closures. Stops on the first non-VLam head, recording a single "Too many type arguments." error rather than one
    * per excess arg.
    *
    * Every argument is injected through the one canonical [[Evaluator.groundToSem]] conversion: a type or data value
    * becomes its *applicable* constructor `VTopDef` (so a higher-kinded carrier `IO` passed as `[F[_]] := IO` reduces
    * a later `F[A]` in the body to `IO[A]`), `Type` becomes `VType`, and a `Direct` literal a `VConst`. The same form
    * is bound into ρ and applied to the signature closure — one injection, one semantic form per ground value, so a
    * signature-embedded occurrence and a ρ-evaluated occurrence of the same argument can never disagree under
    * definitional equality. (A former variant kept a data-value argument as an inert `VConst(Structure)` in the
    * signature while ρ held its `VTopDef` constructor form; the two read back identically but do not unify, a latent
    * false "Type mismatch." for a data value occurring in a type position.)
    */
  private def applyTypeArgs(
      signature: SemValue,
      typeArgs: Seq[GroundValue],
      errorSource: Sourced[?]
  ): CheckIO[SemValue] = {
    def loop(sig: SemValue, remaining: List[GroundValue]): CheckIO[SemValue] = remaining match {
      case Nil          => pure(sig)
      case head :: tail =>
        // Two forms of one erased argument: `argVal` — the canonical evaluable `groundToSem` form — is both applied to
        // the signature closure and bound into ρ (where the reification gate and any type-level code can reduce it);
        // `argType` is bound into Γ as the argument's type — a *type* argument is its own type slot (types are
        // values), a *value* argument contributes its declared `valueType`.
        val argVal  = Evaluator.groundToSem(head)
        val argType = head.valueType match {
          case GroundValue.Type => argVal
          case vt               => Evaluator.groundToSem(vt)
        }
        for {
          forced <- checker.force(sig)
          result <- forced match {
                      case VLam(name, closure) =>
                        modify(_.bindTypeStackParam(name, argType, argVal)) >> loop(closure(argVal), tail)
                      case _                   =>
                        modify(s => s.withUnifier(s.unifier.addError(errorSource.as("Too many type arguments."))))
                          .as(sig)
                    }
        } yield result
    }
    loop(signature, typeArgs.toList)
  }

  /** Emit a [[UnifyError]] as a compiler error, including `Expected` / `Actual` hints when the error carries both
    * sides. The semantic values are re-forced through the final metastore so any metas that were solved after the error
    * was raised display their resolution.
    */
  private def reportUnifyError(err: UnifyError, state: CheckState): CompilerIO[Unit] =
    compilerError(err.context, describe(err, state))

  private def describe(err: UnifyError, state: CheckState): Seq[String] =
    (err.expected, err.actual) match {
      case (Some(expected), Some(actual)) =>
        Seq(
          s"Expected: ${SemValuePrinter.show(expected, state.unifier.metaStore)}",
          s"Actual:   ${SemValuePrinter.show(actual, state.unifier.metaStore)}"
        )
      case _                              => Seq.empty
    }
}

object TypeStackLoop {

  /** The platform-agnostic output of checking one value's type stack and body: the quoted signature and the optional
    * quoted body. Each track wraps this into its own fact ([[MonomorphicValue]] /
    * [[com.vanillasource.eliot.eliotc.monomorphize.fact.CompilerMonomorphicValue]]), so [[TypeStackLoop]] never names
    * either fact type — which is what keeps the two tracks acyclic by construction.
    */
  case class Result(
      signature: GroundValue,
      body: Option[Sourced[MonomorphicExpression.Expression]]
  )

  /** Static convenience wrapper — constructs a [[TypeStackLoop]] and runs its [[process]]. */
  def process(
      typeArguments: Seq[GroundValue],
      resolvedValue: OperatorResolvedValue,
      fetchBinding: ValueFQN => CompilerIO[Option[SemValue]],
      resolveAbility: (ValueFQN, Seq[GroundValue]) => CompilerIO[Option[(ValueFQN, Seq[GroundValue])]],
      track: Track,
      reduceInstance: (ValueFQN, Seq[GroundValue]) => CompilerIO[Option[SemValue]] = (_, _) => none[SemValue].pure[CompilerIO]
  ): CompilerIO[Result] =
    new TypeStackLoop(fetchBinding, resolveAbility, track, reduceInstance).process(typeArguments, resolvedValue)

  private type AbilityRef = AbilityResolver.AbilityRef

  /** One named step of the post-check resolution pipeline (D1). The pipeline is tiered, not flat: the unifier `drain`
    * is interleaved by the runner as the equality core settling — it is not a pass — and the finalizer plus the
    * postcondition assertion are fixed structural steps of `TypeStackLoop.runPostDrainPipeline`, not entries in the pass
    * list.
    */
  private sealed trait PostDrainPass {
    def name: String
  }

  private object PostDrainPass {

    /** Solves metavariables iteratively; `run` returns whether it made progress this round. Re-run inside the
      * saturation fixed-point loop (drain-interleaved) until no `Saturation` pass progresses.
      */
    case class Saturation(name: String, run: PassContext => CheckIO[Boolean]) extends PostDrainPass

    /** Runs once, after saturation reaches its fixed point. May report errors or record solutions. Order within the
      * tier is registration order.
      */
    case class Finalization(name: String, run: PassContext => CheckIO[Unit]) extends PostDrainPass
  }

  /** Everything a post-drain pass closes over from the current check — the pass slot's closed input interface. That
    * every pass needs *only* this (nothing from `processIO`'s locals) is the evidence the slot boundary is real.
    */
  private case class PassContext(
      resolvedValue: OperatorResolvedValue,
      abilityRefs: Seq[AbilityRef],
      paramConstraints: Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]],
      returnMeta: Option[SemValue.VMeta]
  )
}
