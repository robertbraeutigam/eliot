package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.effect.processor.EffectCarriers
import com.vanillasource.eliot.eliotc.module.fact.{ValueFQN, WellKnownTypes}
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

/** Monomorphizes a value at concrete type arguments: kind-checks its signature against its (derived) kind
  * ([[walkTypeStack]]), applies the type arguments, checks the body against the resulting type, then drains and quotes
  * to ground. The kind is a projection of the signature's generic binders — there is no stored type-stack structure.
  * (The name is historical; it predates removing the `TypeStack`.)
  *
  * Holds the callbacks and the [[Checker]] as fields so internal helpers don't have to thread them.
  */
class TypeStackLoop(
    fetchBinding: ValueFQN => CompilerIO[Option[SemValue]],
    resolveAbility: (ValueFQN, Seq[GroundValue]) => CompilerIO[Option[(ValueFQN, Seq[GroundValue])]],
    track: Track,
    reduceInstance: (ValueFQN, Seq[GroundValue]) => CompilerIO[Option[SemValue]] = (_, _) => none[SemValue].pure[CompilerIO],
    // The signature-split `Signature`-twin mode. A signature twin *is* a value whose body is its sibling's signature
    // expression and whose own signature is the derived kind; monomorphizing it means kind-checking that signature
    // against the derived kind and reducing it — exactly [[walkTypeStack]] + [[applyTypeArgs]] — with **no separate body
    // to check** (walking the signature is the whole job) and a **W3 decline** (a calculated return is solvable only by
    // the body, which a signature twin lacks). The default `false` is the ordinary runtime/compiler value mono.
    signatureOnly: Boolean = false,
    // The signature-split flip (Step 6): the value's *own* signature, already reduced to ground by its signature twin
    // (`CompilerMonomorphicValue(v@Signature, args)`), fed in by the processor. When present (a fully-applied,
    // non-calculated instance), [[establishSignature]] binds the type arguments and re-inflates this instead of walking
    // the signature in place — routing every type-level expression through the twin's own monomorphization. `None` (a
    // signature twin computing itself, a W3 value whose twin declined, or a partial application) keeps the in-place walk.
    injectedSignature: Option[GroundValue] = None
) {
  import TypeStackLoop.AbilityRef

  private val checker = new Checker(fetchBinding, resolveAbility, track, signatureOnly)

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
      // Whether this value's return is *calculated* (its source return is an under-applied omittable constructor —
      // a bare `Int`, a bare W2-grown `Counter`), derived structurally from the signature rather than a persisted flag.
      isCalc <- checker.calcReturns.isCalculatedReturnExpr(returnExprOf(resolvedValue))

      // W4 (Limit 5): a calculated return needs a body to calculate from. Reject a body-less calculated return at the
      // definition before its under-applied return reaches a use site (where it would otherwise surface as a confusing
      // mismatch). A **signature twin** (`signatureOnly`) has no body of its own to calculate from, so a calculated
      // return there is not an error but an explicit, error-free *decline*: the runtime twin's mono owns the W3
      // back-edge (`readMonomorphicReturn`) that reads such a return back from the body.
      _ <- if (signatureOnly) { if (isCalc) liftF(abort[Unit]) else pure(()) }
           else failOnAbstractCalculatedReturn(isCalc, resolvedValue)

      // The value to check: a calculated return's under-applied source return (`Int`) is kind-ill-formed as a
      // `Function` codomain (kind `BigInteger → … → Type`, not `Type`), so it is replaced by the kind-correct `Type`
      // placeholder for the kind check — the body then solves the real return via the return metavariable
      // `settleReturnPosition` installs (below), exactly as before. The real (calculated) return lives only in the
      // source `OperatorResolvedValue`; this placeholder is a checker-internal transient, never persisted or a flag.
      rv = if (isCalc) resolvedValue.copy(signature = flattenReturnToType(resolvedValue.signature)) else resolvedValue

      // The body to check against the settled signature. A **signature twin** (`signatureOnly`) has none — walking its
      // signature (its own body, in the value model) *is* the whole job — so its inert placeholder `.runtime` slot is
      // ignored and everything below follows the ordinary body-less path (no body check, structural/reduced body `None`).
      bodyToCheck = if (signatureOnly) None else rv.runtime

      // Establish the value's signature at the concrete type arguments (the signature split's Step-6 flip): read it
      // reduced-to-ground from the value's own signature twin (`injectedSignature`) and re-inflate it, or — a signature
      // twin computing itself, a calculated return, a partial application — walk it in place. Either way yields the
      // applied signature `SemValue` plus the kind-check ability-ref levels the drain loop walks (empty on the flip
      // path — the twin already resolved the signature's abilities).
      (instantiated, levelExprs) <- establishSignature(rv, typeArguments)

      // Effect-lift bookkeeping: record the value's own *ambient* effect-carrier heads, now that every signature
      // binder is bound in ρ (an explicit type argument to its concrete value, a leftover to its instantiation meta).
      _ <- recordAmbientCarriers(rv)

      // Compiler-track carrier pinning (CP-D): a `{Throw[E]}` effect carrier in a compile-time value has no runtime
      // carrier to infer, so it is fixed to the compile-time `Throw` carrier `Either[E]` before the body is checked —
      // solving the carrier's instantiation meta lets the body's `pure`/`raise` dispatch resolve and reduce. Runtime
      // track never pins here (its carriers are inferred from the runtime use, or pinned to `IO` at `main`).
      _ <- track.pinCarriers(checker, rv)

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
      calcReturn              = isCalc && bodyToCheck.isDefined
      checkResult            <- track.settleReturnPosition(checker, instantiated, calcReturn, sawGuard, rv)
      (checkSig, returnMeta)  = checkResult

      // Capture ρ now, before `check` binds the runtime value parameters as `FunctionLiteral` binders — so ρ holds only
      // the erased type-stack parameters (each already in its evaluable `groundToSem` form, bound at `applyTypeArgs`
      // time) and the phantom-parameter metas. This is exactly the env that seeds the reification gate in
      // `PostDrainQuoter`; no post-hoc `VConst → groundToSem` rewrite is needed.
      monoEnv <- inspect(_.rho)

      // Check runtime body if present — produces SemExpression with SemValue slots. A body-less value (an abstract
      // declaration, or a signature twin whose placeholder body is ignored) has nothing to check or emit here.
      runtime      <- bodyToCheck.traverse { body =>
                        checker.check(body, checkSig).map(expr => body.as(expr))
                      }

      // Collect all ability-qualified value references from the output trees (runtime + signature levels). These
      // drive the saturation tier of the post-drain pipeline below.
      abilityRefs   = (runtime.toSeq ++ levelExprs).flatMap(checker.abilityResolver.collectAbilityRefs)

      // Post-drain resolution (D1): the sequence that settles every metavariable — the ability-resolution fixed
      // point, carrier-kind verification, the calculated-return fail-safe, the finalizer (default the rest to
      // Type), the postponement flush, and a postcondition assertion. See `runPostDrainResolution`.
      _ <- runPostDrainResolution(rv, abilityRefs, returnMeta)
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
      guardMarker    = sawGuard && rv.runtime.isEmpty
      guardBindings <- if (guardMarker) reduceGuardSubValues(levelExprs) else pure(Map.empty[ValueFQN, SemValue])
      finalSig      <- if (guardBindings.nonEmpty) reevaluateGuardReturn(rv, guardBindings) else pure(checkSig)
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
      groundSig <- liftF(quoter.quoteSem(finalSig, rv.signature))
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
      guardExpr  = SignatureView.of(resolvedValue.signature).returnType
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

  /** The return-position expression of a value's signature, for the calculated-return detection. */
  private def returnExprOf(resolvedValue: OperatorResolvedValue): OperatorResolvedExpression =
    SignatureView.of(resolvedValue.signature).returnType.value

  /** Replace a value's signature return position with the kind-correct `Type` placeholder, leaving its parameter arrows
    * and generic binders intact (so the derived kind is unchanged). A calculated return's real (under-applied) return
    * would otherwise fail the `= Type` kind check; the placeholder is a checker-internal transient — the body solves the
    * real return via the metavariable [[CalculatedReturnResolver.installReturnMeta]] installs — never persisted.
    */
  private def flattenReturnToType(
      signature: Sourced[OperatorResolvedExpression]
  ): Sourced[OperatorResolvedExpression] = {
    val view    = SignatureView.of(signature)
    val typeRef = OperatorResolvedExpression.ValueReference(signature.as(WellKnownTypes.typeFQN))
    signature.as(view.withReturnType(typeRef).toExpression)
  }

  /** W4 (Limit 5): a calculated return is *calculated from the body*; a value with no body the checker can see cannot
    * calculate it, and an output position must not quantify it instead, so the bare return must be stated explicitly.
    * A body-less value here is a truly abstract declaration (`runtime` is `None` — e.g. a platform-layer signature
    * awaiting an implementation): [[CalculatedReturnResolver.installReturnMeta]] would not run, so this is reported at
    * the definition rather than letting the under-applied return escape to a use site.
    */
  private def failOnAbstractCalculatedReturn(isCalc: Boolean, resolvedValue: OperatorResolvedValue): CheckIO[Unit] =
    if (isCalc && resolvedValue.runtime.isEmpty) {
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

  /** Finalize every still-unsolved meta to [[VType]]. Runs after the drain-and-resolve loop has reached its fixed
    * point, so any meta unification could have determined is already solved; what remains is an unconstrained
    * (phantom) instantiation, whose canonical value is `Type`.
    */
  private def defaultUnsolvedMetas: CheckIO[Unit] =
    modify { s =>
      val store  = s.unifier.metaStore
      val solved = store.entries.foldLeft(store) {
        case (acc, (rawId, None)) => acc.solve(SemValue.MetaId(rawId), VType)
        case (acc, _)             => acc
      }
      s.withUnifier(s.unifier.copy(metaStore = solved))
    }

  /** The post-check resolution sequence (D1) that settles every metavariable, in order:
    *
    *   - SATURATION ([[resolveAbilitiesToFixedPoint]]): drain-interleaved ability resolution to a fixed point.
    *   - FINALIZATION (once): carrier-kind verification, then the calculated-return fail-safe.
    *   - a final drain: a finalization step can commit new solutions (carrier-kind verification unifies a solution's
    *     kind against its expectation), so the equality core settles once more before defaulting — a constraint
    *     postponed against a meta those solutions ground still resolves instead of its metas defaulting to `Type`.
    *   - the FINALIZER ([[defaultUnsolvedMetas]]), the postponement flush, and the postcondition assertion.
    */
  private def runPostDrainResolution(
      resolvedValue: OperatorResolvedValue,
      abilityRefs: Seq[AbilityRef],
      returnMeta: Option[SemValue.VMeta]
  ): CheckIO[Unit] =
    for {
      _ <- resolveAbilitiesToFixedPoint(abilityRefs, resolvedValue.paramConstraints)
      _ <- checker.carriers.verifyCarrierKinds
      _ <- returnMeta.traverse_(failOnUndeterminedCalculatedReturn(_, resolvedValue))
      _ <- modify(s => s.withUnifier(s.unifier.drain()))
      _ <- defaultUnsolvedMetas
      // Fail-safe (TODO.md): any constraint still postponed after the finalizer is an equality obligation the check
      // never discharged. Flush the queue to hard mismatch errors (triaging benign, now-defaulted constraints away
      // first) rather than silently carrying and forgetting them — the hole that let pre-fix applied-associated-type
      // garbage compile. Runs before the meta-postcondition assertion, which the defaulting already satisfies.
      _ <- modify(s => s.withUnifier(s.unifier.flushPostponed()))
      _ <- assertEveryMetaResolved(resolvedValue)
    } yield ()

  /** Saturation: drain the unifier — so the resolver sees every solution the previous round injected — then try to
    * resolve the still-unresolved ability references, looping while any reference newly resolves (a resolution records
    * an impl whose solutions the next round's drain propagates). Bounded because each ability resolves at most once,
    * so progress is monotone.
    */
  private def resolveAbilitiesToFixedPoint(
      abilityRefs: Seq[AbilityRef],
      paramConstraints: Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]]
  ): CheckIO[Unit] = {
    def round: CheckIO[Boolean] =
      for {
        _          <- modify(s => s.withUnifier(s.unifier.drain()))
        progressed <- checker.abilityResolver.resolveAbilities(abilityRefs, paramConstraints)
      } yield progressed
    def loop: CheckIO[Unit] = round.flatMap(if (_) loop else pure(()))
    loop
  }

  /** Postcondition of the post-drain pipeline (D1): every metavariable is solved. The finalizer
    * ([[defaultUnsolvedMetas]]) makes this hold by construction, so it never fires in normal operation; it is the
    * *compiler-bug* backstop — were a future resolution path to bypass the finalizer and leave a meta unsolved, this
    * assertion catches it. It
    * [[compilerAbort]]s rather than reporting a user diagnostic because a surviving unsolved meta is an internal
    * invariant violation, not a type error of the user's making.
    */
  private def assertEveryMetaResolved(resolvedValue: OperatorResolvedValue): CheckIO[Unit] =
    inspect { s =>
      s.unifier.metaStore.entries.collect { case (rawId, None) => rawId }.toList
    }.flatMap {
      case Nil      => pure(())
      case unsolved =>
        liftF(
          compilerAbort[Unit](
            resolvedValue.name.as(
              s"Internal: post-drain resolution left metavariables unresolved: ${unsolved.mkString(", ")}."
            )
          )
        )
    }

  /** Establish the value's signature at `typeArguments`, plus the ability-ref levels the drain loop walks (the
    * signature split's Step-6 flip). Two paths:
    *   - **flip** — `injectedSignature` is present (the value's signature twin produced its reduced ground signature)
    *     and this is a fully-applied, non-`signatureOnly` instance: bind each type argument to its binder in ρ/Γ (the ρ
    *     setup the body check needs, formerly a side effect of [[applyTypeArgs]]) and re-inflate the ground signature to
    *     a checkable `VPi` ([[Evaluator.groundToSemPi]]). No in-place walk, and no signature ability levels — the twin's
    *     own mono already resolved every ability embedded in the signature. The result is definitionally the in-place
    *     walk's (the signature split's Step-5 equivalence), so the settle/body-check tail is shared unchanged.
    *   - **in-place** — otherwise (a signature twin computing itself, a calculated return whose twin declined, a partial
    *     application, or no twin signature available): the kind-checking [[walkTypeStack]] + [[applyTypeArgs]] +
    *     [[instantiateRemaining]], exactly as before the flip.
    */
  private def establishSignature(
      resolvedValue: OperatorResolvedValue,
      typeArguments: Seq[GroundValue]
  ): CheckIO[(SemValue, Seq[Sourced[SemExpression]])] = {
    val binders = SignatureView.of(resolvedValue.signature).binders
    injectedSignature match {
      case Some(groundSig) if !signatureOnly && typeArguments.sizeIs == binders.size =>
        binders
          .zip(typeArguments)
          .traverse_ { case (binder, arg) =>
            modify(_.bindTypeStackParam(binder.name.value, Evaluator.groundToSem(arg.valueType), Evaluator.groundToSem(arg)))
          }
          .as((Evaluator.groundToSemPi(groundSig), Seq.empty[Sourced[SemExpression]]))
      case _                                                                          =>
        for {
          (signature, levelExprs) <- walkTypeStack(resolvedValue.signature)
          appliedSig              <- applyTypeArgs(signature, typeArguments, resolvedValue.signature)
          instantiated            <- instantiateRemaining(appliedSig)
        } yield (instantiated, levelExprs)
    }
  }

  /** Kind-check a value's signature against its (derived) kind, top-down.
    *
    * A generic signature `[A: K1, B: K2] -> …` has kind `Function[K1, Function[K2, Type]]`, derived from its leading
    * binders (the kind is a projection of the signature, never stored). A binderless signature has kind `Type`, checked
    * directly. For each level we (a) kind-check the ORE against the running `expected` so ill-kinded signatures surface
    * as errors attached to the correct source, and (b) evaluate the ORE to obtain the next-lower level's expected kind.
    * The result is the signature's [[SemValue]] plus one kind-check [[SemExpression]] per level (the kind, if generic,
    * then the signature), which the drain-and-resolve loop walks to discover ability refs embedded in type positions.
    */
  private def walkTypeStack(
      signature: Sourced[OperatorResolvedExpression]
  ): CheckIO[(SemValue, Seq[Sourced[SemExpression]])] = {
    val view    = SignatureView.of(signature)
    val typeRef = OperatorResolvedExpression.ValueReference(signature.as(WellKnownTypes.typeFQN))
    // Levels top-down (kind first), reproducing the former stored `[signature, kind]` stack walked in reverse.
    val levels  =
      if (view.binders.isEmpty) Seq(signature)
      else {
        val kindExpr = view.binders.foldRight(typeRef: OperatorResolvedExpression) { (binder, acc) =>
          OperatorResolvedExpression.arrow(signature.as(binder.parameterType.map(_.value).getOrElse(typeRef)), signature.as(acc))
        }
        Seq(signature.as(kindExpr), signature)
      }
    levels.foldLeftM((VType: SemValue, Seq.empty[Sourced[SemExpression]])) {
      case ((expected, acc), level) =>
        for {
          checked <- checker.check(level, expected)
          next    <- checker.evalExpr(level.value)
        } yield (next, acc :+ signature.as(checked))
    }
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
    val view         = SignatureView.of(resolvedValue.signature)
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
        // `argType` is bound into Γ as the argument's *type*, uniformly its declared `valueType`. For a *type* argument
        // `A` that is `A`'s kind `Type` (`Type : Type`), NOT `A` itself: a level body referencing the parameter in
        // value position (`Function[X, X]`, an arrow spine whose args are checked through the value path) then
        // kind-checks `X` against `Type`, agreeing with the type-position use (`evalExpr` reads the denoted value `A`
        // from ρ). Binding `A` here instead would make `infer(X)` report `X : A`, a spurious `Type mismatch`.
        val argVal  = Evaluator.groundToSem(head)
        val argType = Evaluator.groundToSem(head.valueType)
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
      reduceInstance: (ValueFQN, Seq[GroundValue]) => CompilerIO[Option[SemValue]] = (_, _) => none[SemValue].pure[CompilerIO],
      signatureOnly: Boolean = false,
      injectedSignature: Option[GroundValue] = None
  ): CompilerIO[Result] =
    new TypeStackLoop(fetchBinding, resolveAbility, track, reduceInstance, signatureOnly, injectedSignature)
      .process(typeArguments, resolvedValue)

  private type AbilityRef = AbilityResolver.AbilityRef
}
