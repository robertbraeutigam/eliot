package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize.fact.*
import com.vanillasource.eliot.eliotc.monomorphize.unify.{SemValuePrinter, UnifyError}
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
    track: Track
) {
  import TypeStackLoop.{AbilityRef, PassContext, PostDrainPass}

  private val checker = new Checker(fetchBinding, resolveAbility, track)

  def process(
      typeArguments: Seq[GroundValue],
      resolvedValue: OperatorResolvedValue
  ): CompilerIO[TypeStackLoop.Result] =
    processIO(typeArguments, resolvedValue).runA(CheckState.initial)

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
      calcReturn              = resolvedValue.calculatedReturn && resolvedValue.checkingRuntime.isDefined
      checkResult            <- track.settleReturnPosition(checker, instantiated, calcReturn, sawGuard, resolvedValue)
      (checkSig, returnMeta)  = checkResult

      // Capture ρ now, before `check` binds the runtime value parameters as `FunctionLiteral` binders — so ρ holds only
      // the erased type-stack parameters (each already in its evaluable `groundToSem` form, bound at `applyTypeArgs`
      // time) and the phantom-parameter metas. This is exactly the env that seeds the reification gate in
      // `PostDrainQuoter`; no post-hoc `VConst → groundToSem` rewrite is needed.
      monoEnv <- inspect(_.rho)

      // Check runtime body if present — produces SemExpression with SemValue slots. An `opaque` value presents as
      // body-less here (`checkingRuntime`), so its body is neither checked nor emitted during checking; post-checking
      // representation lowering unfolds it separately.
      runtime      <- resolvedValue.checkingRuntime.traverse { body =>
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

      // Post-drain: quote SemValues to GroundValues using the pre-computed ability resolutions. This is the sole
      // SemValue → GroundValue transition and has no silent fallback; Quoter reports unresolved metas as compiler
      // errors.
      quoter     = new PostDrainQuoter(
                     state.unifier.metaStore,
                     state.abilityResolutions,
                     monoEnv,
                     fqn => implBindings.get(fqn).orElse(state.bindingCache.getOrElse(fqn, None)),
                     track.platform
                   )
      groundSig <- liftF(quoter.quoteSem(checkSig, resolvedValue.typeStack))
      // The compiler track reduces its body (`reduceSourced`); the runtime track keeps it structural (`quoteSourced`).
      monoBody  <- runtime.traverse(srcSem => liftF(track.readBackBody(quoter, srcSem)))
    } yield TypeStackLoop.Result(
      groundSig,
      monoBody.map(sourcedMono => sourcedMono.as(sourcedMono.value.expression))
    )

  /** W4 (Limit 5): a calculated return is *calculated from the body*; a value with no body the checker can see cannot
    * calculate it, and an output position must not quantify it instead, so the bare return must be stated explicitly.
    * Two body-less cases: a truly abstract declaration (`runtime` is `None` — e.g. a platform-layer signature awaiting
    * an implementation) and an `opaque` value (whose body is deliberately hidden from the checker, `checkingRuntime` is
    * `None`). Either way [[CalculatedReturnResolver.installReturnMeta]] would not run, leaving the `Type` placeholder
    * in the signature, so this
    * is reported at the definition rather than letting that placeholder escape.
    */
  private def failOnAbstractCalculatedReturn(resolvedValue: OperatorResolvedValue): CheckIO[Unit] =
    if (resolvedValue.calculatedReturn && resolvedValue.checkingRuntime.isEmpty) {
      val name            = resolvedValue.vfqn.name.name
      val (message, hint) =
        if (resolvedValue.runtime.isEmpty)
          (
            s"Abstract declaration '$name' must state its return type explicitly; there is no body to calculate it from.",
            "Add an explicit return type, or provide a concrete implementation."
          )
        else
          (
            s"Opaque value '$name' must state its return type explicitly; its body is hidden from the type checker.",
            "Add an explicit return type."
          )
      liftF(compilerError(resolvedValue.name.as(message), Seq(hint)) >> abort[Unit])
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
      case SemValue.VNeutral(SemValue.NeutralHead.VVar(_, n), _) =>
        reportUncalculableReturn(resolvedValue, s"the result depends on '$n', which the inputs do not determine")
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
    * already solved. The match has no catch-all: an [[MetaRole.AbstractAssoc]] stays unsolved (so a constraint-covered
    * ref can quote as abstract), and a [[MetaRole.Plain]] or [[MetaRole.Instantiation]] defaults to [[VType]]. Because
    * the match is exhaustive over the sealed `MetaRole`, a future role added to the ADT forces an explicit decision
    * here rather than silently inheriting the old "default everything to `Type`" behaviour — which is exactly the
    * silent-miscompile path D2 removes.
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
              case _: MetaRole.AbstractAssoc => acc // stays abstract through quoting
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
    *   - SATURATION (fixed-point loop): drain-interleaved ability + `Combine` resolution.
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
    PostDrainPass.Saturation("resolve-combines", _ => checker.solver.resolveCombines),
    PostDrainPass.Finalization("upper-bounds", _ => checker.solver.resolveUpperBounds),
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
      _ <- defaultUnsolvedMetas
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

  /** Postcondition of the post-drain pipeline (D1): every metavariable is either solved or a protected
    * [[MetaRole.AbstractAssoc]] placeholder. The finalizer ([[defaultUnsolvedMetas]]) makes this hold by construction,
    * so it never fires in normal operation; it is the *compiler-bug* backstop for D2's per-role finalization — were a
    * future role added to [[MetaRole]] and forgotten in `defaultUnsolvedMetas`'s total match, a meta could survive
    * unsolved, and this assertion catches it. It [[compilerAbort]]s rather than reporting a user diagnostic because a
    * surviving unsolved meta is an internal invariant violation, not a type error of the user's making.
    */
  private def assertEveryMetaResolvedOrAbstract(ctx: PassContext): CheckIO[Unit] =
    inspect { s =>
      val protectedRawIds = s.unifier.abstractAssocMetaIds
      s.unifier.metaStore.entries.collect {
        case (rawId, None) if !protectedRawIds.contains(rawId) => rawId
      }.toList
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

  /** Apply concrete GroundValue type arguments to the signature by converting each to a SemValue and applying to VLam
    * closures. Stops on the first non-VLam head, recording a single "Too many type arguments." error rather than one
    * per excess arg.
    *
    * A *type-level* argument (a `GroundValue.Type`, or a `GroundValue.Structure` whose own type is `Type` — e.g. a
    * higher-kinded carrier `IO` passed as `[F[_]] := IO`, or `Int[0,255]`) is converted through
    * [[Evaluator.groundToSem]] into an *applicable* `VTopDef`/`VType`, so that a later `F[A]` in the body reduces to
    * `IO[A]`. A bare `VConst` is inert under `applyValue` (it is a non-applicable head, so `applyValue` mints a loud
    * `$bad-apply` stuck neutral that fails at read-back), so it must be converted to an applicable `VTopDef`/`VType`
    * here rather than left to collapse `F[A]`. A *value-level* argument (a data instance like `Person(...)` passed for `A: Person`, or a
    * `Direct` literal) stays a `VConst`: it is never applied, and the post-drain reification gate (`PostDrainQuoter`)
    * recognises the `VConst(ground)` form to materialise it into a runtime constructor tree.
    */
  private def applyTypeArgs(
      signature: SemValue,
      typeArgs: Seq[GroundValue],
      errorSource: Sourced[?]
  ): CheckIO[SemValue] = {
    def toSemArg(g: GroundValue): SemValue                                   = g match {
      case GroundValue.Type                              => VType
      case GroundValue.Structure(_, _, GroundValue.Type) => Evaluator.groundToSem(g)
      case _                                             => VConst(g)
    }
    def loop(sig: SemValue, remaining: List[GroundValue]): CheckIO[SemValue] = remaining match {
      case Nil          => pure(sig)
      case head :: tail =>
        // Three forms of one erased argument: `argVal` is applied to the signature closure (a value-level arg stays an
        // inert `VConst`); `evalArg` is bound into ρ in the evaluable `groundToSem` form (a data value as its
        // constructor `VTopDef`, which the reification gate and any type-level code can reduce); `argType` is bound into
        // Γ as the argument's type — a *type* argument is its own type slot (types are values), a *value* argument
        // contributes its declared `valueType`.
        val argVal  = toSemArg(head)
        val evalArg = Evaluator.groundToSem(head)
        val argType = head.valueType match {
          case GroundValue.Type => evalArg
          case vt               => Evaluator.groundToSem(vt)
        }
        for {
          forced <- checker.force(sig)
          result <- forced match {
                      case VLam(name, closure) =>
                        modify(_.bindTypeStackParam(name, argType, evalArg)) >> loop(closure(argVal), tail)
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
      track: Track
  ): CompilerIO[Result] =
    new TypeStackLoop(fetchBinding, resolveAbility, track).process(typeArguments, resolvedValue)

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
