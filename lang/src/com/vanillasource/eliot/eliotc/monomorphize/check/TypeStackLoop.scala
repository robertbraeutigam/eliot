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
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.{compilerAbort, compilerError}

/** Monomorphizes a value at concrete type arguments (signature-unification C1/C2 — one execution path at every arity):
  * re-inflates the value's own signature twin's ground signature ([[establishSignature]]), settles the return position
  * at the read ([[settleAtRead]]), checks the body against it, then drains and reads back to ground. There is no
  * in-place signature walk — the signature twin's mono (an ordinary body mono, [[processSignatureTwin]]) is the single
  * place a signature is computed. (The name is historical; it predates removing the `TypeStack`.)
  *
  * Holds the callbacks and the [[Checker]] as fields so internal helpers don't have to thread them.
  */
class TypeStackLoop(
    fetchBinding: ValueFQN => CompilerIO[Option[SemValue]],
    resolveAbility: (ValueFQN, Seq[GroundValue]) => CompilerIO[Option[(ValueFQN, Seq[GroundValue])]],
    track: Track,
    // Reduce a callee at concrete type arguments on the compiler track, returning its self-contained reduced binding
    // (deep — dependencies closed over their own reduced-at-instantiation forms; see `ReducedBindingClosure`). Feeds the
    // stuck-driven escalation fetch of the post-drain read-back.
    reduceInstance: (ValueFQN, Seq[GroundValue]) => CompilerIO[Option[SemValue]] =
      (_, _) => none[SemValue].pure[CompilerIO],
    // The signature-split `Signature`-twin mode ([[processSignatureTwin]]): the value's body IS its sibling's signature
    // expression (a binder-stripped arrow chain), monomorphized as an ordinary body mono — per-binder kind checks, one
    // `check(chain, VType)`, ordinary read-back. Every twin produces (a plain type, a guard verdict, a W3 hole, or — at a
    // partial-arity key — a *parametric* signature with leftover binders as `GroundValue.Param`s). The default `false`
    // is the ordinary runtime/compiler value mono.
    signatureOnly: Boolean = false,
    // The value's *own* signature, reduced to ground by its signature twin (`CompilerMonomorphicValue(v@Signature,
    // args)`), fed in **mandatorily** by the processor (C1/C2). [[establishSignature]] binds the type arguments (leftover
    // binders to fresh metas, re-inflating the twin's `Param`s) and re-inflates it — the one way a value mono obtains its
    // signature. `None` only for a signature twin computing itself (`signatureOnly`, which does not read it).
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
    if (signatureOnly) processSignatureTwin(typeArguments, resolvedValue)
    else processValueMono(typeArguments, resolvedValue)

  /** The ordinary runtime/compiler *value* mono: establish the value's signature (the Step-6 flip reading its own
    * signature twin, or the in-place walk), check its body against it, drain, and read the body back. (The signature
    * twin's own mono is [[processSignatureTwin]].)
    */
  private def processValueMono(
      typeArguments: Seq[GroundValue],
      resolvedValue: OperatorResolvedValue
  ): CheckIO[TypeStackLoop.Result] =
    for {
      // Whether this value's return is *calculated* (its source return is an under-applied omittable constructor —
      // a bare `Int`, a bare W2-grown `Counter`), derived structurally from the signature rather than a persisted flag.
      isCalc <- checker.calcReturns.isCalculatedReturnExpr(returnExprOf(resolvedValue))

      // W4 (Limit 5): a calculated return needs a body to calculate from. Reject a body-less calculated return at the
      // definition before its under-applied return reaches a use site (where it would otherwise surface as a confusing
      // mismatch).
      _ <- failOnAbstractCalculatedReturn(isCalc, resolvedValue)

      bodyToCheck = resolvedValue.runtime

      // Establish the value's signature at the concrete type arguments (signature-unification C1/C2): re-inflate its
      // signature twin's ground signature — the twin is **mandatory**, there is no in-place walk. Leftover binders the
      // key carries no argument for re-inflate to fresh metavariables (a partial-arity mono's `GroundValue.Param`s),
      // which keep its constraint-covered ability refs deferrable (§4.7).
      instantiated <- establishSignature(resolvedValue, typeArguments)

      // Effect-lift bookkeeping: record the value's own *ambient* effect-carrier heads, now that every signature
      // binder is bound in ρ (an explicit type argument to its concrete value, a leftover to its instantiation meta).
      _ <- recordAmbientCarriers(resolvedValue)

      // Compiler-track carrier pinning (CP-D): a `{Throw[E]}` effect carrier in a compile-time value has no runtime
      // carrier to infer, so it is fixed to the compile-time `Throw` carrier `Either[E]` before the body is checked —
      // solving the carrier's instantiation meta lets the body's `pure`/`raise` dispatch resolve and reduce. Runtime
      // track never pins here (its carriers are inferred from the runtime use, or pinned to `IO` at `main`).
      _ <- track.pinCarriers(checker, resolvedValue)

      // Settle the return position at the read (signature-unification C1) — one stateless, shape-driven step:
      //   - a *calculated* (bare, omittable) return with a body ⟹ `installReturnMeta` (W3): its position becomes a fresh
      //     metavariable that checking the body solves to the body's inferred type;
      //   - otherwise, on the **runtime** track, discharge a guard off the re-inflated leaf shape
      //     (`dischargeGuardedSignature`): `Right(t)` ⤳ the plain type `t`, `Left(msg)` aborts with the author message, an
      //     `Either`/`Bool` carrier-headed return (a guard the twin published undischarged) is deferred to the body — all
      //     recognised on the ground shape, no `sawGuard` flag;
      //   - on the **compiler** track (the guard's *producer*) the carrier signature is left undischarged for a consumer.
      settled                 <- settleAtRead(instantiated, isCalc, bodyToCheck.isDefined, resolvedValue)
      (checkSig, returnMeta)   = settled

      // Capture ρ now, before `check` binds the runtime value parameters as `FunctionLiteral` binders — so ρ holds only
      // the erased type-stack parameters (each already in its evaluable `groundToSem` form) and the leftover-binder
      // metas. This is exactly the env that seeds the reification gate in `PostDrainQuoter`.
      monoEnv <- inspect(_.rho)

      // Check runtime body if present — produces SemExpression with SemValue slots. A body-less value (an abstract
      // declaration) has nothing to check or emit here.
      runtime      <- bodyToCheck.traverse { body =>
                        checker.check(body, checkSig).map(expr => body.as(expr))
                      }

      // Collect all ability-qualified value references from the checked body. These drive the saturation tier of the
      // post-drain pipeline below. (The signature's own ability refs were resolved by the twin's mono, not re-walked here.)
      abilityRefs   = runtime.toSeq.flatMap(checker.abilityResolver.collectAbilityRefs)

      // Post-drain resolution + quoter assembly (shared with the signature twin): drain-and-resolve every metavariable,
      // abort on any unification error, then build the `PostDrainQuoter` over the drain-resolved impl bindings.
      quoter    <- drainAndBuildQuoter(resolvedValue, abilityRefs, returnMeta, monoEnv, runtime)
      groundSig <- liftF(quoter.quoteSem(checkSig, resolvedValue.signature))
      // The compiler track reduces its body (`reduceSourced`); the runtime track keeps it structural (`quoteSourced`).
      // Narrow-representation reconciliation happens in the backend's `ExpressionCodeGenerator` off the refinement
      // channel, so the body is read back directly.
      monoBody  <- runtime.traverse(srcSem => liftF(track.readBackBody(quoter, srcSem)))
    } yield TypeStackLoop.Result(
      groundSig,
      monoBody.map(sourcedMono => sourcedMono.as(sourcedMono.value.expression))
    )

  /** The signature twin's monomorphization as an **ordinary body mono** (signature-unification §3.1): the signature's
    * binder-stripped arrow chain *is* the body. Kind-check each declared binder against `VType` and bind its ground
    * argument in ρ/Γ, then check the arrow chain (`parameters → returnType`) against `VType` through the shared
    * resolution ladder — whose acceptance arms (ordinary type / guard carrier / W3 under-applied hole) cover every
    * return shape — run the ordinary post-drain resolution, and reduce the checked chain to its ground signature via the
    * same escalation read-back every compiler body takes. No `walkTypeStack`, no `settleReturnPosition`, no `sawGuard`
    * gate, no W3 decline: every signature twin produces — a plain type, a `Right(t)`/`Left(msg)` guard verdict, or an
    * under-applied W3 hole (§3.5). The published body is `None` (the signature is the whole product).
    */
  private def processSignatureTwin(
      typeArguments: Seq[GroundValue],
      resolvedValue: OperatorResolvedValue
  ): CheckIO[TypeStackLoop.Result] = {
    val view     = SignatureView.of(resolvedValue.signature)
    // The arrow chain with the generic binders stripped (they are bound in ρ/Γ by `bindTwinBinders`): `params → return`.
    val bodyExpr = resolvedValue.signature.as(view.copy(binders = Seq.empty).toExpression)
    // Carrier binders (the M1 `{E...}` effect carriers) a *leftover* binder must stay a metavariable for, so
    // `pinCarriers` can fix it to the compile-time `Either[String]` (signature-unification C2 fence): a generic leftover
    // binder becomes a `SignatureBinder` (→ `GroundValue.Param`), but a carrier leftover is pinned, not parameterised.
    val carrierBinderNames = EffectCarriers.carrierBinders(view).filter(resolvedValue.paramConstraints.contains)
    for {
      _                    <- bindTwinBinders(view.binders, typeArguments, carrierBinderNames)
      // Capture ρ now — holding only the erased type-stack binders (their ground values / carrier metas), *before* the
      // check binds any value / pattern binder of the arrow chain. This is the same clean env the value mono captures
      // before its body check; the read-back reduces the checked chain under it, so a signature-position `match`'s
      // pattern binder (`case Box[a] -> a`) is extracted by the match reduction rather than shadowed by a stale
      // post-check neutral. (Carrier metas the pin below solves live in the metastore, not ρ, so this stays authoritative.)
      monoEnv              <- inspect(_.rho)
      _                    <- recordAmbientCarriers(resolvedValue)
      checked              <- checker.check(bodyExpr, VType)
      // Compile-time carrier pinning runs *after* the arrow-chain check (as the value mono pins after its signature
      // walk): a declared `{Throw[String]}` carrier binder's meta is bound above and an *inline* guard's inferred
      // carrier meta is created by the check, so both are present to pin to the compile-time `Either[String]` now.
      _                    <- track.pinCarriers(checker, resolvedValue)
      abilityRefs           = checker.abilityResolver.collectAbilityRefs(bodyExpr.as(checked))
      quoter               <- drainAndBuildQuoter(resolvedValue, abilityRefs, None, monoEnv, None)
      // The signature's ground read-back. Reduce the **raw** evaluated arrow chain first (renormalising natives as it
      // quotes): this settles an ordinary type, a bare `{Throw[String]}` carrier return, a type-level `match` (whose
      // pattern binder the match reduction binds — the check would freeze it to a neutral), a W3 under-applied hole,
      // and a *pure* guard (a marker's `where fold(…)`/`E1 != E2` Bool computation — the raw `fold` has no
      // instantiation type-argument to mis-fire the native on). It is *stuck*, never wrongly reduced, only for an
      // **effectful** inline guard (`if..else..raise`), whose `else`/`runAbort` need the checker's effect-lift `pure`
      // absent from the raw signature — so fall back there to the **checked** arrow chain via the escalation loop, which
      // reduces it to its `Right(t)` / `Left(msg)` verdict. No `sawGuard`: the shape falls out of which reduction settles.
      raw                  <- checker.evalExpr(bodyExpr.value)
      groundSig            <- quoter.quoteSemOption(raw) match {
                                case Some(ground) => liftF(ground.pure[CompilerIO])
                                case None         =>
                                  liftF(quoter.reduceSignatureToGroundOption(checked)).flatMap {
                                    case Some(ground) => liftF(ground.pure[CompilerIO])
                                    case None         => publishStuckGuardCarrier(view, checked, quoter, resolvedValue)
                                  }
                              }
    } yield TypeStackLoop.Result(groundSig, None)
  }

  /** A guard whose verdict depends on a *leftover generic binder* (a [[GroundValue.Param]]) is stuck as a value — its
    * `fold`/`else` cannot reduce with the binder abstract, so neither the raw quote nor the escalation grounds it
    * (signature-unification C2). Publish its undischarged **carrier type** (`Either[String, Type]`) instead, which the
    * value mono re-inflates and defers to the body (Use-Site Verification): the guard is re-decided at every concrete
    * instance above. The guard-ness is recognised by shape — the checked return's *type* is an `Either`/`Bool` carrier
    * ([[CalculatedReturnResolver.isGuardCarrier]]) — no `sawGuard` flag. This arm is only a no-parameter guard's
    * abstract-site check (the sole Param-stuck-guard source, §7 census); anything else stuck hard-errors (fail-safe).
    */
  private def publishStuckGuardCarrier(
      view: SignatureView,
      checked: SemExpression,
      quoter: PostDrainQuoter,
      resolvedValue: OperatorResolvedValue
  ): CheckIO[GroundValue] =
    checker.calcReturns.isGuardCarrier(checked.expressionType).flatMap {
      case true if view.parameters.isEmpty =>
        liftF(quoter.quoteSem(checked.expressionType, resolvedValue.name))
      case _                               =>
        liftF(compilerAbort[GroundValue](resolvedValue.name.as("Cannot resolve type.")))
    }

  /** Kind-check each signature binder's declared kind against `VType` and bind its ground type argument in ρ/Γ (the
    * signature-unification body model, §3.1 step 1). Left-to-right, so a binder whose kind references an earlier binder
    * sees it already bound. A leftover binder with no argument (a phantom the key does not carry) instantiates to a
    * fresh meta; the mono key carries one argument per binder in practice (§4.7).
    */
  private def bindTwinBinders(
      binders: Seq[SignatureView.Binder],
      typeArguments: Seq[GroundValue],
      carrierBinderNames: Set[String]
  ): CheckIO[Unit] =
    binders.zipWithIndex.traverse_ { case (binder, i) =>
      for {
        // Kind-check the binder's declared kind against `VType`, then evaluate it to the binder's *type* for Γ. The
        // binder's declared kind is authoritative — a higher-kinded carrier binder `[F[_]]` yields `VPi(Type, _ =>
        // Type)`, whereas the ground argument's own `valueType` is merely `Type` (`IO : Type`), which cannot type the
        // application `F[Unit]`. A value binder `[N: BigInteger]` yields `BigInteger`; an unannotated `[X]` yields `Type`.
        _       <- binder.parameterType.traverse_(kind => checker.check(kind, VType).void)
        kindSem <- binder.parameterType.traverse(kind => checker.evalExpr(kind.value)).map(_.getOrElse(VType))
        // ρ's value at this binder (signature-unification C2):
        //   - an explicit ground argument ⟹ its applicable head form;
        //   - a leftover **carrier** binder ⟹ a fresh instantiation meta that stays open for `pinCarriers` to fix to the
        //     compile-time `Either[String]` (a pinned carrier, not a generic parameter);
        //   - any other leftover (a generic type parameter) ⟹ a `SignatureBinder` neutral, so the arrow chain reads back
        //     **under its binder** as a `GroundValue.Param` (a parametric signature) instead of defaulting to `Type`.
        value   <- typeArguments.lift(i) match {
                     case Some(arg)                                       => pure(Evaluator.groundToSem(arg))
                     case None if carrierBinderNames.contains(binder.name.value) => checker.freshMeta.widen[SemValue]
                     case None                                           =>
                       pure(VNeutral(NeutralHead.SignatureBinder(i, binder.name.value), Spine.SNil): SemValue)
                   }
        _       <- modify(_.bindTypeStackParam(binder.name.value, kindSem, value))
      } yield ()
    }

  /** The return-position expression of a value's signature, for the calculated-return detection. */
  private def returnExprOf(resolvedValue: OperatorResolvedValue): OperatorResolvedExpression =
    SignatureView.of(resolvedValue.signature).returnType.value

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

  /** The shared post-drain tail of both monos (the value mono and the signature twin): run the drain-and-resolve
    * pipeline ([[runPostDrainResolution]]), report and abort on any unification error, then assemble the
    * [[PostDrainQuoter]] over the drain-resolved impl bindings. `track.implBindings` does not mutate the check state, so
    * the one post-drain `state` seeds both the error check and the quoter.
    *
    * `abilityMethodBindings` maps each drain-resolved ability *method* FQN to its impl body — the SemValue analogue of
    * [[PostDrainQuoter.resolveAbilityRefs]] — so an ability call surviving in a type-position SemValue reduces during
    * read-back (a guard naming an ability method directly, `where equals(E1, E2)`); one instance per method, no conflation.
    */
  private def drainAndBuildQuoter(
      resolvedValue: OperatorResolvedValue,
      abilityRefs: Seq[AbilityRef],
      returnMeta: Option[SemValue.VMeta],
      monoEnv: Env,
      residualBody: Option[Sourced[SemExpression]]
  ): CheckIO[PostDrainQuoter] =
    for {
      _            <- runPostDrainResolution(resolvedValue, abilityRefs, returnMeta, residualBody)
      state        <- get
      _            <- state.unifier.errors.reverse.traverse_(err => liftF(reportUnifyError(err, state)))
      _            <- if (state.unifier.errors.nonEmpty) liftF(abort[Unit]) else pure(())
      implBindings <- track.implBindings(fetchBinding)
      abilityMethodBindings = state.abilityResolutions.toSeq.flatMap { case (ref, (implFqn, _)) =>
                                implBindings.get(implFqn).map(ref.value -> _)
                              }.toMap
    } yield new PostDrainQuoter(
      state.unifier.metaStore,
      state.abilityResolutions,
      monoEnv,
      fqn => implBindings.get(fqn).orElse(abilityMethodBindings.get(fqn)).orElse(state.bindingCache.getOrElse(fqn, None)),
      track.platform,
      reduceInstance
    )

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
      returnMeta: Option[SemValue.VMeta],
      residualBody: Option[Sourced[SemExpression]]
  ): CheckIO[Unit] =
    for {
      _ <- resolveAbilitiesToFixedPoint(abilityRefs, resolvedValue.paramConstraints)
      _ <- checker.carriers.verifyCarrierKinds
      _ <- returnMeta.traverse_(failOnUndeterminedCalculatedReturn(_, resolvedValue))
      _ <- modify(s => s.withUnifier(s.unifier.drain()))
      // Exact effect verification: a value's residual effects (those
      // demanded on its own ambient carrier) must be declared. Runs here — after the drain solved every reference's
      // carrier argument, before `defaultUnsolvedMetas` collapses an abstract ambient carrier to `Type`. Passed the
      // checked body only for a value mono; a signature twin passes `None` (its arrow-chain has no runtime effects).
      _ <- residualBody.traverse_(body => checker.effectResidual.check(body, resolvedValue))
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

  /** Establish the value's signature at `typeArguments` by re-inflating its signature twin's ground signature
    * (signature-unification C1/C2 — the twin is **mandatory**, there is no in-place walk). Bind each binder in ρ/Γ: a
    * binder the key carries an argument for to that ground value, a *leftover* binder (a partial-arity key, §4.7) to a
    * fresh metavariable — the same metavariable the twin's leftover `GroundValue.Param` re-inflates to
    * ([[Evaluator.groundToSemPiParam]]), so the parametric signature and ρ agree on it. The leftover metas keep a
    * partial-arity mono's constraint-covered ability refs deferrable; a full-arity key has no leftover and re-inflates to
    * a fully-ground `VPi`.
    */
  private def establishSignature(
      resolvedValue: OperatorResolvedValue,
      typeArguments: Seq[GroundValue]
  ): CheckIO[SemValue] = {
    val binders = SignatureView.of(resolvedValue.signature).binders
    injectedSignature match {
      case None                                            =>
        // The twin read is mandatory (the processor uses `getFactOrAbort`); a missing signature is an internal invariant
        // violation, not a user error.
        liftF(compilerAbort[SemValue](resolvedValue.name.as("Internal: value mono ran without its signature twin.")))
      case Some(_) if typeArguments.sizeIs > binders.size  =>
        // More type arguments than the value has binders — a user error (formerly caught by `applyTypeArgs` stopping on
        // the first non-`VLam` head). Report once and abort rather than silently ignoring the excess.
        liftF(compilerError(resolvedValue.name.as("Too many type arguments.")) >> abort[SemValue])
      case Some(groundSig)                                 =>
        for {
          // One fresh metavariable per *leftover* binder index, memoised so the ρ binding and the `GroundValue.Param`
          // re-inflation below share the same meta.
          paramMetas <- (typeArguments.size until binders.size).toList
                          .traverse(i => checker.freshMeta.map(m => i -> (m: SemValue)))
                          .map(_.toMap)
          _          <- binders.zipWithIndex.traverse_ { case (binder, i) =>
                          typeArguments.lift(i) match {
                            case Some(arg) =>
                              modify(
                                _.bindTypeStackParam(
                                  binder.name.value,
                                  Evaluator.groundToSem(arg.valueType),
                                  Evaluator.groundToSem(arg)
                                )
                              )
                            case None      =>
                              for {
                                kindSem <- binder.parameterType
                                             .traverse(k => checker.evalExpr(k.value))
                                             .map(_.getOrElse(VType))
                                _       <- modify(_.bindTypeStackParam(binder.name.value, kindSem, paramMetas(i)))
                              } yield ()
                          }
                        }
        } yield Evaluator.groundToSemPiParam(groundSig, i => paramMetas.getOrElse(i, VType))
    }
  }

  /** Settle the value's return position at the read (signature-unification C1), stateless and shape-driven:
    *   - a *calculated* return with a body ⟹ [[CalculatedReturnResolver.installReturnMeta]] (W3 — the body solves the
    *     fresh meta);
    *   - otherwise the **runtime** track discharges/defers a guard off the re-inflated ground leaf
    *     ([[CalculatedReturnResolver.dischargeGuardedSignature]], recognising the guard by its `Right`/`Left`/carrier
    *     shape, not a flag), while the **compiler** track (the guard's *producer*) leaves the carrier undischarged.
    */
  private def settleAtRead(
      instantiated: SemValue,
      isCalc: Boolean,
      hasBody: Boolean,
      resolvedValue: OperatorResolvedValue
  ): CheckIO[(SemValue, Option[SemValue.VMeta])] =
    if (isCalc && hasBody)
      checker.calcReturns.installReturnMeta(instantiated).map { case (sig, m) => (sig, Some(m)) }
    else
      track.platform match {
        case Platform.Runtime  =>
          checker.calcReturns.dischargeGuardedSignature(instantiated, hasBody, resolvedValue.name)
        case Platform.Compiler =>
          pure((instantiated, Option.empty[SemValue.VMeta]))
      }

  /** Record the value-under-check's own *ambient* effect-carrier heads into [[CheckState.ambientCarriers]], from its two
    * possible spellings of a carrier — an *open* effect row (a carrier binder) and a *pinned* one (a concrete carrier
    * stack in the return type). Read by the checker-side effect lift ([[EffectLifter.effectCarrierSplit]]).
    *
    *   - **Open-row carriers** ([[recordBinderCarriers]]): the signature's higher-kinded, ability-constrained binders
    *     (the M1 `{E...}` carrier — the same `carrierBinders ∩ paramConstraints` filter the residual check's ambient
    *     read uses, excluding a bare generic `C[_, _]`), looked up in ρ and recorded by their forced head.
    *   - **Pinned-row / concrete carriers** ([[recordConcreteReturnCarrier]]): a value whose *return type* is itself a
    *     concrete carrier stack (a pinned row `{State[S] | Id} A` ⤳ `StateCarrier[S, Id, A]`, or an explicit
    *     `IO[Unit]`) declares no `[F[_] ~ E]` binder, yet that carrier *is* its ambient — without recording it a block
    *     of such statements would not sequence (each statement's concrete carrier head is unrecognised, so no `flatMap`
    *     is inserted and every statement but the last is silently dropped).
    */
  private def recordAmbientCarriers(resolvedValue: OperatorResolvedValue): CheckIO[Unit] = {
    val view         = SignatureView.of(resolvedValue.signature)
    val carrierNames = EffectCarriers.carrierBinders(view).filter(resolvedValue.paramConstraints.contains)
    // A value carries its ambient one way or the other: an *open* row via a carrier binder (whose return then forces to
    // that binder's meta, so [[recordConcreteReturnCarrier]] would find nothing anyway), or a *pinned* / concrete-carrier
    // return with no binder. Dispatching on `carrierNames` keeps them exclusive and skips the return eval for open rows.
    if (carrierNames.nonEmpty) recordBinderCarriers(carrierNames)
    else recordConcreteReturnCarrier(view)
  }

  /** Record the open-effect-row ambient carriers: the value's higher-kinded, ability-constrained binders, each looked
    * up in ρ — where [[establishSignature]] bound an explicit argument's concrete value and a leftover binder's
    * instantiation meta — and recorded by its forced head (a [[CheckState.CarrierHead.TopDef]] FQN for a concrete
    * instantiation like `IO`, a [[CheckState.CarrierHead.Meta]] id for a peeled one). Any other head shape is skipped.
    */
  private def recordBinderCarriers(carrierNames: Set[String]): CheckIO[Unit] =
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

  /** Record the pinned-row / concrete-carrier ambient: evaluate the value's return type in the current ρ, and if it
    * forces to a **carrier-headed** concrete application `C[a.., R]` — the carrier `C[a..]` having an `Effect` instance,
    * the one authority on "is this constructor a carrier" — record `C`'s head. A plain container return (`List[X]`,
    * `Pair[A, B]`) has no `Effect[List]`/`Effect[Pair[A]]` instance and records nothing, so block sequencing is enabled
    * for pinned effect rows without misclassifying data. Open-row returns force to a carrier *meta* (not a `VTopDef`)
    * and are handled by [[recordBinderCarriers]], so they fall through here. A non-ground carrier (unsolved metas /
    * leftover binders at a partial-arity mono) cannot be resolved and is skipped — the fail-safe default of not
    * recording, never a wrong classification.
    */
  private def recordConcreteReturnCarrier(view: SignatureView): CheckIO[Unit] =
    for {
      returnSv <- checker.evalExpr(view.returnType.value)
      forced   <- checker.force(returnSv)
      _        <- forced match {
                    case VTopDef(fqn, cached, Spine.SApp(prefix, _)) =>
                      isEffectCarrierConstructor(VTopDef(fqn, cached, prefix)).flatMap {
                        case true  => modify(_.recordAmbientCarriers(Set(CheckState.CarrierHead.TopDef(fqn))))
                        case false => pure(())
                      }
                    case _                                           => pure(())
                  }
    } yield ()

  /** Whether a type constructor (partially applied to its carrier prefix) is an effect carrier — decided by whether an
    * `Effect[carrier]` instance resolves on this track's platform, the same authority the effect machinery uses. A
    * carrier that cannot be quoted to ground (residual metas) yields `false`.
    */
  private def isEffectCarrierConstructor(carrier: SemValue): CheckIO[Boolean] =
    for {
      store  <- inspect(_.unifier.metaStore)
      result <- Quoter.quote(0, carrier, store) match {
                  case Right(ground) =>
                    liftF(resolveAbility(WellKnownTypes.effectFlatMapFQN, Seq(ground))).map(_.isDefined)
                  case Left(_)       => pure(false)
                }
    } yield result

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
      reduceInstance: (ValueFQN, Seq[GroundValue]) => CompilerIO[Option[SemValue]] =
      (_, _) => none[SemValue].pure[CompilerIO],
      signatureOnly: Boolean = false,
      injectedSignature: Option[GroundValue] = None
  ): CompilerIO[Result] =
    new TypeStackLoop(fetchBinding, resolveAbility, track, reduceInstance, signatureOnly, injectedSignature)
      .process(typeArguments, resolvedValue)

  private type AbilityRef = AbilityResolver.AbilityRef
}
