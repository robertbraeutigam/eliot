package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN, WellKnownTypes}
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.fact.MonomorphicExpression
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** The per-track strategy for the two monomorphization tracks (runtime / compiler). It carries the track's [[Platform]]
  * (so fact keys read the platform off the track rather than a bare threaded value) plus the four places the checking
  * core genuinely differs between the two tracks, each extracted 1:1 from a former `platform match` conditional in
  * [[TypeStackLoop]]:
  *
  *   - [[settleReturnPosition]] — the calc-return / guard-discharge / pass-through switch for the return position;
  *   - [[pinCarriers]] — the compiler track's `{Throw[E]}` → `Either[E]` carrier pinning (a no-op on the runtime track);
  *   - [[implBindings]] — the compiler track's resolved-impl binding fetch, folded into the read-back evaluator's lookup
  *     (empty on the runtime track, whose body stays structural for codegen);
  *   - [[readBackBody]] — reduce the compile-time body ([[PostDrainQuoter.reduceSourced]]) on the compiler track, or
  *     keep it structural ([[PostDrainQuoter.quoteSourced]]) on the runtime track.
  *
  * Everything else in the checking core is track-agnostic. A hook receives exactly the collaborators / primitives it
  * needs (the [[Checker]] for the two that call checker methods, `fetchBinding` / the [[PostDrainQuoter]] for the read
  * side); state access is via [[CheckIO]].
  */
sealed trait Track {
  def platform: Platform

  /** Settle a value's return position before the body is checked, yielding the signature to check against plus an
    * optional return metavariable the body must solve. Three mutually-exclusive outcomes, exactly the former
    * `(calcReturn, platform)` match:
    *   - a *calculated* (bare, omittable) return — track-independent — becomes a fresh meta the body solves
    *     ([[CalculatedReturnResolver.installReturnMeta]], W3);
    *   - otherwise the track decides ([[settleGuardedReturn]]): the runtime track discharges an effectful-signatures
    *     guard, the compiler track (the guard's *producer*) leaves the carrier signature as-is.
    */
  final def settleReturnPosition(
      checker: Checker,
      instantiated: SemValue,
      calcReturn: Boolean,
      sawGuard: Boolean,
      resolvedValue: OperatorResolvedValue
  ): CheckIO[(SemValue, Option[VMeta])] =
    if (calcReturn)
      checker.calcReturns.installReturnMeta(instantiated).map { case (sig, m) => (sig, Some(m)) }
    else
      settleGuardedReturn(checker, instantiated, sawGuard, resolvedValue)

  /** The non-calculated return branch of [[settleReturnPosition]]. */
  protected def settleGuardedReturn(
      checker: Checker,
      instantiated: SemValue,
      sawGuard: Boolean,
      resolvedValue: OperatorResolvedValue
  ): CheckIO[(SemValue, Option[VMeta])]

  /** Pin a compile-time value's effect carriers before its body is checked (compiler track only; a no-op on runtime). */
  def pinCarriers(checker: Checker, resolvedValue: OperatorResolvedValue): CheckIO[Unit]

  /** The resolved-impl bindings to merge ahead of the checker's binding cache for the read-back evaluator (compiler
    * track only; empty on runtime).
    */
  def implBindings(fetchBinding: ValueFQN => CompilerIO[Option[SemValue]]): CheckIO[Map[ValueFQN, SemValue]]

  /** Read a checked body back into a [[MonomorphicExpression]] tree: reduced (compiler) or structural (runtime). */
  def readBackBody(
      quoter: PostDrainQuoter,
      srcSem: Sourced[SemExpression]
  ): CompilerIO[Sourced[MonomorphicExpression]]
}

object Track {

  /** The runtime track ([[Platform.Runtime]]): guarded returns are discharged at their use site, no carrier pinning, no
    * impl bindings (the body stays structural for codegen), and the body is quoted structurally.
    */
  case object Runtime extends Track {
    override val platform: Platform = Platform.Runtime

    override protected def settleGuardedReturn(
        checker: Checker,
        instantiated: SemValue,
        sawGuard: Boolean,
        resolvedValue: OperatorResolvedValue
    ): CheckIO[(SemValue, Option[VMeta])] =
      checker.calcReturns.dischargeGuardedSignature(
        instantiated,
        sawGuard,
        resolvedValue.checkingRuntime.isDefined,
        resolvedValue.name
      )

    override def pinCarriers(checker: Checker, resolvedValue: OperatorResolvedValue): CheckIO[Unit] = pure(())

    override def implBindings(
        fetchBinding: ValueFQN => CompilerIO[Option[SemValue]]
    ): CheckIO[Map[ValueFQN, SemValue]] = pure(Map.empty)

    override def readBackBody(
        quoter: PostDrainQuoter,
        srcSem: Sourced[SemExpression]
    ): CompilerIO[Sourced[MonomorphicExpression]] = quoter.quoteSourced(srcSem)
  }

  /** The compiler track ([[Platform.Compiler]]): the compiler platform *is* the runner, so a compile-time guarded
    * signature is published undischarged (this track is the guard's producer), `{Throw[E]}` carriers are pinned to the
    * compile-time `Either[E]`, each drain-resolved ability impl's body is made reachable to the read-back evaluator, and
    * the body is reduced to a normal form.
    */
  case object Compiler extends Track {
    override val platform: Platform = Platform.Compiler

    override protected def settleGuardedReturn(
        checker: Checker,
        instantiated: SemValue,
        sawGuard: Boolean,
        resolvedValue: OperatorResolvedValue
    ): CheckIO[(SemValue, Option[VMeta])] =
      pure((instantiated, Option.empty[VMeta]))

    /** Compiler-track carrier pinning (CP-D): a compile-time value's own `{Throw[E]}` effect carrier has no runtime
      * carrier to infer — it is fixed to the compiler platform's `Throw` carrier `Either[E]` (the `main`-pins-`IO`
      * analogue). For each such carrier binder, solve its freshly-instantiated metavariable to `Either[E]` so the
      * body's `pure`/`raise` ability calls dispatch to the compile-time `Effect[Either[E]]` / `Throw[E, Either[E]]`
      * instances and reduce. A no-op on a compiler value with no effect carrier.
      */
    override def pinCarriers(checker: Checker, resolvedValue: OperatorResolvedValue): CheckIO[Unit] =
      resolvedValue.paramConstraints.toList.traverse_ { case (binderName, constraints) =>
        throwCarrierErrorType(binderName, constraints)
          .traverse_(pinCarrierToEither(checker, binderName, _, resolvedValue.name))
      }

    /** Fetch each drain-resolved ability impl's [[NativeBinding]] body once from the compiler pool, so [[readBackBody]]'s
      * reduction can fold the concrete impl body in via NbE. Missing bindings are dropped. */
    override def implBindings(
        fetchBinding: ValueFQN => CompilerIO[Option[SemValue]]
    ): CheckIO[Map[ValueFQN, SemValue]] =
      for {
        state  <- get
        result <- state.abilityResolutions.values.toList
                    .distinctBy(_._1)
                    .traverse { case (implFqn, _) => liftF(fetchBinding(implFqn)).map(_.map(implFqn -> _)) }
                    .map(_.flatten.toMap)
      } yield result

    override def readBackBody(
        quoter: PostDrainQuoter,
        srcSem: Sourced[SemExpression]
    ): CompilerIO[Sourced[MonomorphicExpression]] = quoter.reduceSourced(srcSem)

    /** The error type `E` of a `Throw[E]` constraint whose carrier is `binderName`. `EffectSugarDesugarer` appends the
      * carrier as the ability's final type argument (`Throw[E]` ⤳ `Throw[E, F]`), so `binderName` is the `Throw` carrier
      * iff a `Throw` constraint's last argument references it; the error type is then that constraint's first argument.
      * [[None]] for any non-`Throw` binder or a `Throw` constraint whose carrier position is some other binder.
      */
    private def throwCarrierErrorType(
        binderName: String,
        constraints: Seq[OperatorResolvedValue.ResolvedAbilityConstraint]
    ): Option[OperatorResolvedExpression] =
      constraints.collectFirst {
        case c
            if c.abilityFQN == throwAbilityFQN && c.typeArgs.sizeIs >= 2 &&
              (c.typeArgs.last match {
                case OperatorResolvedExpression.ParameterReference(n) => n.value == binderName
                case _                                                => false
              }) =>
          c.typeArgs.head
      }

    /** Solve the carrier binder `binderName`'s instantiation meta to `Either[E]`, where `E` is `errorType` evaluated in
      * the current env. Only an as-yet-unsolved carrier meta is pinned; if unification already determined it (it never
      * does for a genuine compile-time carrier, which has no runtime use to fix it), the pin is skipped rather than risk
      * a spurious conflict.
      */
    private def pinCarrierToEither(
        checker: Checker,
        binderName: String,
        errorType: OperatorResolvedExpression,
        at: Sourced[?]
    ): CheckIO[Unit] =
      inspect(_.rho.lookupByName(binderName)).flatMap {
        case None        => pure(())
        case Some(bound) =>
          checker.force(bound).flatMap {
            case meta: VMeta =>
              for {
                errSem <- checker.evalExpr(errorType)
                either  = VTopDef(WellKnownTypes.eitherFQN, None, Spine.SNil :+ errSem)
                _      <- modify(s =>
                            s.withUnifier(s.unifier.unify(meta, either, at.as("Compile-time effect carrier `Either`.")))
                          )
              } yield ()
            case _           => pure(())
          }
      }

    /** The `Throw[E, F[_]]` effect ability. Its compile-time carrier is `Either[E]` — the fact [[pinCarriers]] realizes. */
    private val throwAbilityFQN: AbilityFQN = AbilityFQN(ModuleName(ModuleName.effectPackage, "Throw"), "Throw")
  }
}
