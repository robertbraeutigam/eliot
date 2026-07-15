package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.{Evaluator, Quoter}
import com.vanillasource.eliot.eliotc.monomorphize.fact.{CompilerMonomorphicValue, GroundValue, MonomorphicValue}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.saturate.fact.SaturatedValue
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

/** The calculated-return *non-local inference* back-edge (D7): the implicit-generics (W3/W4) machinery that fills a
  * value's bare omittable return from its *body*, factored out of the
  * [[com.vanillasource.eliot.eliotc.monomorphize.check.Checker]] into one cohesive module — the same move D4 made for
  * the refinement lattice. None of this is definitional equality: it re-enters `getFactIfProduced(MonomorphicValue.Key)` to read
  * a callee's *monomorphized body* (so a caller's type can depend on a callee's calculated return), guarded against a
  * recursive producer call graph via `activeFactKeys`.
  *
  * Three entry points, one per phase of the back-edge:
  *   - [[installReturnMeta]] — the *callee* side (called from `TypeStackLoop`): replace the bare return position of a
  *     calculated-return signature with a fresh meta, so checking the body solves it.
  *   - [[resolveCalculatedReturn]] — the *applied* read side (called from `Checker.applyInferred`): when an application
  *     reaches a calculated-return producer's under-applied source return, read its monomorphized return instead.
  *   - [[resolveCompleteCalculatedReturn]] — the *by-name* read side (called from `Checker.infer`): the twin for a
  *     fully-applied calculated-return value referenced without further application.
  *
  * It also hosts the **effectful-signatures discharge** (W2b): a return-type expression may be a `{Throw[String]}`
  * computation on the compile-time `Either[String, _]` carrier, which the compiler is the handler for. This sits beside
  * the calculated return because both are "run a compile-time computation to obtain
  * the return type" — the calculated return reads it off the callee's monomorphized body, the guard `runThrow`s the
  * signature's `Either` and reads `Right(t)` (the type) or `Left(msg)` (a rejection). Three discharge hook points
  * mirror the calculated-return ones: [[isGuardCarrier]] (the *kind* position — accept an `Either[..]`-valued return
  * where a bare `Type` is expected), [[dischargeGuardedSignature]] (the *callee* side — clean the published signature),
  * and [[dischargeGuardedReturn]] (the *applied / by-name read* sides — discharge a guard a caller observes).
  *
  * Operates over [[CheckIO]], reading the shared [[CheckState]] through `get`. It depends on exactly two checker
  * primitives, passed at construction — that narrow surface is the module boundary.
  *
  * @param force
  *   Force a SemValue through the current meta store — the checker's `force`.
  * @param freshMeta
  *   Allocate a fresh metavariable — the checker's `freshMeta`.
  */
class CalculatedReturnResolver(
    force: SemValue => CheckIO[SemValue],
    freshMeta: CheckIO[VMeta],
    platform: Platform
) {

  /** Replace the return position of a calculated-return signature with a fresh metavariable, returning the rewritten
    * signature and the meta (W3 callee side). The signature is a chain of value-parameter `VPi` arrows ending in the
    * bare omittable return left by `saturate`; the meta stands in for that return so that *checking the body* against
    * this signature solves it (by ordinary unification) to the body's inferred type. A no-parameter producer
    * (`def x: Int = 5`) has no arrows, so the signature *is* the return and is replaced directly. The descent reads a
    * snapshot of the metastore to tell a `VPi` arrow from the return; that is stable, because the arrows come from the
    * (meta-independent) `Function` native, not from any meta solution.
    */
  def installReturnMeta(sig: SemValue): CheckIO[(SemValue, VMeta)] =
    for {
      meta <- freshMeta
      s    <- get
    } yield (substituteReturn(sig, meta, s.unifier.metaStore), meta)

  private def substituteReturn(sig: SemValue, meta: SemValue, metaStore: MetaStore): SemValue =
    Evaluator.force(sig, metaStore) match {
      case VPi(domain, codomain) => VPi(domain, arg => substituteReturn(codomain(arg), meta, metaStore))
      case _                     => meta
    }

  /** The structural signal of a *calculated return*, replacing the former `calculatedReturn` flag on
    * [[com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue]]: a return position whose head is a type
    * constructor applied to *fewer* arguments than its omittable arity (a bare `Int`, a bare W2-grown `Counter`, a
    * partially-applied `Counter[0]`). The omittable arity is read off the head's [[SaturatedValue]]'s `inferableArity`,
    * which saturation has already grown to include both the W1 `auto` arity and any W2 record growth (see
    * `SaturatedValueProcessor.growTypeConstructor`) — so this needs neither a persisted flag nor a re-derivation of the
    * growth oracle. A fully-applied return (`Int[0, 255]`, `IO[Unit]`, `String`), a bare `Type`, a type parameter, or a
    * non-omittable under-applied head (`IO`, arity 0) is not a calculated return.
    */
  private def arityShortfall(fqn: ValueFQN, argCount: Int): CheckIO[Boolean] =
    liftF(getFactIfProduced(SaturatedValue.Key(fqn, platform))).map {
      case Some(sv) => argCount < sv.value.inferableArity
      case None     => false
    }

  /** The [[SemValue]] form (the *read* sides): a forced return value headed by an under-applied type constructor is a
    * calculated return. A `VPi` (a partial application), a `VType`, a metavariable, a neutral (type parameter), or a
    * fully-applied constructor is not.
    */
  def isCalculatedReturn(retValue: SemValue): CheckIO[Boolean] =
    force(retValue).flatMap {
      case VTopDef(fqn, _, sp) => arityShortfall(fqn, sp.toList.size)
      case _                   => pure(false)
    }

  /** The signature-expression form (the *callee* side, before evaluation): the return position's head is an
    * under-applied omittable constructor. Mirrors [[isCalculatedReturn]] on the raw
    * [[OperatorResolvedExpression]] a value's own signature carries.
    */
  def isCalculatedReturnExpr(returnType: OperatorResolvedExpression): CheckIO[Boolean] =
    OperatorResolvedExpression.spine(returnType) match {
      case (head @ OperatorResolvedExpression.ValueReference(name, _), args)
          if !OperatorResolvedExpression.isFunctionReference(head) =>
        arityShortfall(name.value, args.length)
      case _ => pure(false)
    }

  /** When a function application has reached the bare omittable return of a *calculated-return* producer
    * (implicit-generics, W3), resolve that return from the callee's monomorphized signature rather than the
    * (under-applied) source return left by `saturate`. This is the architectural "back-edge": the callee's concrete
    * type arguments are read off the (instantiated) target value reference, and the caller reads
    * `MonomorphicValue(callee, args).signature`'s deep return type — the body-checked result the callee already
    * produced when monomorphized at those arguments — re-entering it as a [[SemValue]]. It reuses the monomorphization
    * the compiler performs anyway; no symbolic quoting on this path.
    *
    * Returns [[None]] (so the ordinary codomain stands) when this is not a reached calculated return: the target is not
    * a value reference, the return is still a function (an intermediate `VPi` of a partial application), or the callee
    * is not a calculated-return producer. Also [[None]] when the type arguments are not yet ground or no
    * monomorphization exists — the caller then keeps the bare return, which fails the ordinary check downstream rather
    * than being silently mistyped.
    */
  def resolveCalculatedReturn(
      targetExpr: SemExpression,
      rawReturn: SemValue
  ): CheckIO[Option[SemValue]] =
    // The reached codomain *is* the callee's (under-applied) source return — a bare `Int`, a bare `Counter`. Its
    // under-application is the structural signal that this is a calculated return, so read the callee's monomorphized
    // return instead of the source codomain. An intermediate `VPi` (partial application), a genuine `Type` return, or
    // any fully-applied concrete return is not under-applied and falls through to the ordinary codomain.
    isCalculatedReturn(rawReturn).flatMap {
      case true  =>
        innermostValueRef(targetExpr) match {
          case Some((fqn, typeArgs)) => readMonomorphicReturn(fqn, typeArgs)
          case None                  => pure(None)
        }
      case false => pure(None)
    }

  /** Resolve the return of a *complete* (fully applied) calculated-return value referenced by name — the read-site twin
    * of the [[resolveCalculatedReturn]] back-edge (W4, deferred W3 item 1). The value is complete iff its applied type is
    * its under-applied source return ([[isCalculatedReturn]]), i.e. no parameter `VPi` remains to apply; then its
    * body-checked return is read from `MonomorphicValue(value, args)` exactly as in the applied path, sharing
    * [[readMonomorphicReturn]]'s recursion guard. Returns [[None]] when a `VPi` still remains (the value is applied
    * later, or passed higher-order with the under-applied return buried in its codomain) so the ordinary signature
    * stands.
    */
  def resolveCompleteCalculatedReturn(
      vfqn: Sourced[ValueFQN],
      typeArgs: Seq[SemValue],
      appliedSig: SemValue
  ): CheckIO[Option[SemValue]] =
    isCalculatedReturn(appliedSig).flatMap {
      case true  => readMonomorphicReturn(vfqn, typeArgs)
      case false => pure(None)
    }

  /** The innermost [[SemExpression.ValueReference]] of a (possibly curried) application target, with its accumulated
    * type arguments — the callee whose calculated return is being resolved.
    */
  private def innermostValueRef(expr: SemExpression): Option[(Sourced[ValueFQN], Seq[SemValue])] =
    expr.expression match {
      case SemExpression.ValueReference(fqn, typeArgs)  => Some((fqn, typeArgs))
      case SemExpression.FunctionApplication(target, _) => innermostValueRef(target.value)
      case _                                            => None
    }

  /** Read the callee's body-checked return at the concrete type arguments: quote the (instantiation-meta) type args to
    * ground and read `MonomorphicValue(callee, args).signature`'s deep return type as a [[SemValue]]. [[None]] when the
    * args are not yet ground (left as a bare return for the ordinary check to reject) or no monomorphization exists.
    */
  private def readMonomorphicReturn(
      fqn: Sourced[ValueFQN],
      typeArgs: Seq[SemValue]
  ): CheckIO[Option[SemValue]] =
    for {
      s          <- get
      groundArgsE = typeArgs.toList.traverse(a => Quoter.quote(0, a, s.unifier.metaStore))
      result     <- groundArgsE match {
                      case Left(_)           => reportUngroundCalculatedReturn(fqn)
                      case Right(groundArgs) => readMonomorphicReturnGround(fqn, groundArgs)
                    }
    } yield result

  /** W4 (Limit 3 / deferred W3 item 2): a calculated return is read off `MonomorphicValue(callee, args)`, so the
    * callee's type arguments must be ground at the call. An instantiation determined only by the *surrounding*
    * context (rather than by the call's own arguments) may still be an unsolved metavariable when the return is read.
    * Reading the return eagerly here would leave the bare under-applied return, which then leaks into a confusing
    * mismatch downstream. Report a specific, actionable error instead. (Postponing the calculation until the
    * arguments ground — making such a call *compile* — is a completeness improvement deferred to W5.)
    */
  private def reportUngroundCalculatedReturn(fqn: Sourced[ValueFQN]): CheckIO[Option[SemValue]] =
    liftF(
      compilerError(
        fqn.as(
          s"Cannot calculate the return type of '${fqn.value.name.name}' here: its type arguments are not determined at this call site."
        ),
        Seq(
          "The call's type arguments are not yet known when it is checked — they are only determined by the surrounding context.",
          "Annotate the argument's type, or give the value an explicit return type."
        )
      ) >> abort[Option[SemValue]]
    )

  /** Read the callee's monomorphized return at ground type arguments, first guarding against a recursive
    * calculated-return chain (Limit 1 of the implicit-generics calculated-return limits). If the callee's FQN is already
    * an ancestor of the fact being checked now, requesting `MonomorphicValue(callee, args)` would re-enter an in-progress
    * computation and dead-lock the fact cache — and, more fundamentally, the callee's return depends (directly,
    * mutually, or through a value-dependent bound) on itself, which monomorphization-by-type cannot ground. A
    * non-recursive program has an acyclic producer call graph, so a repeated FQN on the active chain is exactly the
    * recursion signal; report it as a specific error rather than blocking forever or defaulting to `Type`.
    */
  // The back-edge re-enters the *own track*'s mono (the signature split's Step-6 W3 inversion): the runtime track reads
  // `MonomorphicValue`, the compiler track reads `CompilerMonomorphicValue`. Before the flip no compiler-track client
  // took a calculated return; now a compiler-track value referencing a calculated-return callee resolves it here, so the
  // read must target the compiler pool (reading the runtime `MonomorphicValue` of a compiler-pool-only callee would find
  // no fact and error). The recursion guard covers both key shapes.
  private def readMonomorphicReturnGround(
      fqn: Sourced[ValueFQN],
      groundArgs: Seq[GroundValue]
  ): CheckIO[Option[SemValue]] =
    liftF(activeFactKeys).flatMap { active =>
      val recursing = active.exists {
        case MonomorphicValue.Key(vfqn, _)         => vfqn == fqn.value
        case CompilerMonomorphicValue.Key(vfqn, _) => vfqn == fqn.value
        case _                                     => false
      }
      if (recursing) liftF(reportRecursiveCalculatedReturn(fqn) >> abort[Option[SemValue]])
      else
        platform match {
          case Platform.Runtime  =>
            liftF(getFactIfProduced(MonomorphicValue.Key(fqn.value, groundArgs)))
              .map(_.map(mv => Evaluator.groundToSem(mv.signature.deepReturnType)))
          case Platform.Compiler =>
            liftF(getFactIfProduced(CompilerMonomorphicValue.Key(fqn.value, groundArgs)))
              .map(_.map(cmv => Evaluator.groundToSem(cmv.signature.deepReturnType)))
        }
    }

  private def reportRecursiveCalculatedReturn(fqn: Sourced[ValueFQN]): CompilerIO[Unit] =
    compilerError(
      fqn.as(s"Cannot calculate the return type of recursive value '${fqn.value.name.name}'."),
      Seq(
        "Its result type depends on itself — directly, mutually, or through a value-dependent bound — which " +
          "monomorphization cannot ground.",
        "Write an explicit return type."
      )
    )

  // === Effectful-signatures discharge (W2b) ===

  /** The guard-carrier recognition for the *kind* position (W2b, and ability-implementation guards). A return-type
    * expression whose *value* is a guard denotes a type/verdict but may instead reject, so its inferred *type* is not
    * the bare `Type` an ordinary return position has. Two guard-carrier shapes are accepted where a `Type` kind is
    * expected:
    *   - the effectful-signatures carrier `Either[String, _]` ([[WellKnownTypes.eitherFQN]]-headed), from an inline
    *     `if(cond, T) else raise(msg)` / bare `raise(msg)` guard on the compile-time `Throw[String]` channel — discharged
    *     to its payload type (or rejected) by [[dischargeGuardedSignature]] / [[dischargeGuardedReturn]] once concrete;
    *   - a bare `Bool` ([[WellKnownTypes.boolFQN]]-headed), the applicability verdict of an ability-implementation
    *     `where` guard riding the marker's return slot (ability-guards §2.3) — read as keep/decline at the use-site
    *     discharge in the ability processor.
    *
    * `true` here means "accept this as a guarded return", since the kind check would otherwise reject `Either[..]` /
    * `Bool` as ≠ `Type`. A *normal* `Bool` or `Either[..]` used as a *return type* infers kind `Type` (a fully-applied
    * type constructor `Either[String, Int]` is a type), so `inferred` is `VType` there and this is unaffected — only a
    * `Bool`/`Either`-*typed value* sitting in a type position (a `true` / `E1 != E2` / `raise("…")`) is a guard.
    */
  def isGuardCarrier(inferred: SemValue): CheckIO[Boolean] =
    force(inferred).flatMap {
      case VTopDef(fqn, _, _) => pure(fqn === WellKnownTypes.eitherFQN || fqn === WellKnownTypes.boolFQN)
      // An *inline* guard (`if..else..raise`) whose carrier is still an unsolved **effect-carrier** meta at kind-check
      // time (`?G[?A]`, the `else` carrier not yet pinned): its concrete carrier `Either[String]` is only fixed later by
      // `Track.Compiler.pinCarriers`, but the return already *is* a `{Throw[String]}`/`{Abort}` guard. Recognising the
      // effect-carrier head here accepts it as a guarded return, so the kind check never postpones `?G[?A] ~ Type` — a
      // constraint that would become a hard `Either[String, _] ~ Type` mismatch once the carrier is pinned. Only an
      // ability-constrained higher-kinded (effect) meta qualifies — a bare HKT type parameter is not flagged — and a
      // normal effectful return (`{Console} Unit`) never reaches this ladder with a `Type` expectation, so it does not
      // over-fire (verified against the effect examples).
      case VMeta(id, _)       => inspect(_.unifier.isEffectCarrier(id.value))
      case _                  => pure(false)
    }

  /** Discharge a *single* return value on the compile-time `Throw[String]` carrier — the W2b handler. The return
    * computation has been forced to a ground `Either[String, Type]`:
    *   - `Right(t)` ⟹ `Some(t)`: the resolved return type is the payload `t`.
    *   - `Left(msg)` ⟹ `compilerError(at.as(msg))` then abort: the guard rejected, with the author's message primary.
    *   - anything else (an ordinary type, or a still-stuck guard whose bounds are abstract) ⟹ `None`: nothing to
    *     discharge / defer to the use site (a stuck guard is correct, not an error — Use-Site Verification).
    *
    * `Right`/`Left` are body-less value constructors, so a constructed carrier value is a `VTopDef` headed by
    * [[WellKnownTypes.rightFQN]]/[[WellKnownTypes.leftFQN]]; the payload spine entry and the rejection fallback are
    * the [[GuardChannel]] protocol shared with the ability-guard verdict interpreter.
    */
  def dischargeGuardedReturn(retType: SemValue, at: Sourced[?]): CheckIO[Option[SemValue]] =
    force(retType).flatMap {
      case VTopDef(fqn, _, spine) if fqn === WellKnownTypes.rightFQN =>
        pure(GuardChannel.payload(spine.toList))
      case VTopDef(fqn, _, spine) if fqn === WellKnownTypes.leftFQN  =>
        GuardChannel.payload(spine.toList) match {
          case Some(msgSem) =>
            extractGuardMessage(msgSem).flatMap(msg => liftF(compilerError(at.as(msg)) >> abort[Option[SemValue]]))
          case None         => pure(None)
        }
      case _                                                        => pure(None)
    }

  /** The author message carried by a `Left(msg)` rejection. The carrier's error type is `String`, so a rejection's
    * message is a literal `String` that reads back directly; a non-literal (computed, not-yet-reduced) message falls
    * back to the shared [[GuardChannel]] rejection message so the guard is still reported, never silently dropped
    * (fail-safe).
    */
  private def extractGuardMessage(msgSem: SemValue): CheckIO[String] =
    force(msgSem).map {
      case VConst(GroundValue.Direct(s: String, _)) => s
      case _                                        => GuardChannel.fallbackRejectionMessage
    }

  /** Discharge any guard in a *signature's* return position — the callee side (W2b). The signature is a chain of
    * value-parameter `VPi` arrows ending in the (already type-argument-applied) return computation. Descend to that
    * return — a guard depends only on the (now concrete) type parameters, never the value parameters, so a placeholder
    * suffices to peel the arrows — and [[dischargeGuardedReturn]] it. The three outcomes mirror the model:
    *   - `Right(t)` ⟹ rebuild the arrows over the payload `t`, so the published signature and the body's expected type
    *     become the plain type `t`; no return meta.
    *   - `Left` ⟹ the discharge aborts.
    *   - not reduced (a non-guard ordinary return, or a guard *stuck* on abstract bounds) ⟹ **defer**. A stuck guard
    *     (`isGuard`, recognised at the kind check) with a body becomes a fresh return metavariable the body solves —
    *     exactly the calculated-return treatment, so the body type-checks instead of erroring against the undischarged
    *     carrier, and the guard is re-decided at each concrete use (Use-Site Verification). A non-guard, or a body-less
    *     stuck guard, is returned untouched (the latter stays stuck and hard-errors at read-back — fail-safe).
    *
    * @return
    *   the resolved signature and an optional return metavariable (`Some` only in the stuck-guard-deferred case, fed to
    *   the same post-drain fail-safe as a calculated return).
    */
  def dischargeGuardedSignature(
      sig: SemValue,
      isGuard: Boolean,
      hasBody: Boolean,
      at: Sourced[?]
  ): CheckIO[(SemValue, Option[VMeta])] = {
    // A guard's return depends only on the (now concrete) type parameters, so any value stands in to peel the
    // value-parameter arrows; a fresh neutral keeps the metastore clean (unlike a meta) for the non-guard common case.
    val probe: SemValue = VNeutral(NeutralHead.Reserved(NeutralHead.Marker.GuardProbe), Spine.SNil)

    def rebuild(domains: List[SemValue], leaf: SemValue): SemValue =
      domains.foldLeft(leaf)((acc, dom) => VPi(dom, _ => acc))

    def peel(current: SemValue, domains: List[SemValue]): CheckIO[(SemValue, Option[VMeta])] =
      force(current).flatMap {
        case VPi(domain, codomain) => peel(codomain(probe), domain :: domains)
        case leaf                  =>
          dischargeGuardedReturn(leaf, at).flatMap {
            case Some(payload)              => pure((rebuild(domains, payload), None))
            case None if isGuard && hasBody => freshMeta.map(m => (rebuild(domains, m), Some(m)))
            case None                       => pure((sig, None))
          }
      }

    peel(sig, Nil)
  }

}
