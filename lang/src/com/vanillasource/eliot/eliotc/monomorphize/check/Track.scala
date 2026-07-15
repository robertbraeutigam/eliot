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
  * (so fact keys read the platform off the track rather than a bare threaded value) plus the three places the checking
  * core genuinely differs between the two tracks, each extracted 1:1 from a former `platform match` conditional in
  * [[TypeStackLoop]]:
  *
  *   - [[pinCarriers]] — the compiler track's `{Throw[E]}` → `Either[E]` carrier pinning (a no-op on the runtime track);
  *   - [[implBindings]] — the compiler track's resolved-impl binding fetch, folded into the read-back evaluator's lookup
  *     (empty on the runtime track, whose body stays structural for codegen);
  *   - [[readBackBody]] — reduce the compile-time body ([[PostDrainQuoter.reduceSourced]]) on the compiler track, or
  *     keep it structural ([[PostDrainQuoter.quoteSourced]]) on the runtime track.
  *
  * The return-position settle is no longer a track hook: [[TypeStackLoop.settleAtRead]] reads the value's re-inflated
  * ground signature and settles it directly, branching on [[platform]] once for the guard's runtime-discharge vs
  * compiler-pass-through asymmetry (signature-unification C1). Everything else in the checking core is track-agnostic.
  * A hook receives exactly the collaborators / primitives it needs (the [[Checker]] for the two that call checker
  * methods, `fetchBinding` / the [[PostDrainQuoter]] for the read side); state access is via [[CheckIO]].
  */
sealed trait Track {
  def platform: Platform

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

    /** Compiler-track carrier pinning (CP-D): a compile-time value's own `{Throw[E]}` effect carrier has no runtime
      * carrier to infer — it is fixed to the compiler platform's `Throw` carrier `Either[E]` (the `main`-pins-`IO`
      * analogue). For each such carrier binder, solve its freshly-instantiated metavariable to `Either[E]` so the
      * body's `pure`/`raise` ability calls dispatch to the compile-time `Effect[Either[E]]` / `Throw[E, Either[E]]`
      * instances and reduce. A no-op on a compiler value with no effect carrier.
      */
    override def pinCarriers(checker: Checker, resolvedValue: OperatorResolvedValue): CheckIO[Unit] =
      for {
        _ <- resolvedValue.paramConstraints.toList.traverse_ { case (binderName, constraints) =>
               throwCarrierErrorType(binderName, constraints)
                 .traverse_(pinCarrierToEither(checker, binderName, _, resolvedValue.name))
             }
        // Also pin any *inferred* return-row carrier (signature split, Step 7). A guard written *inline*
        // (`if..else..raise`, or a bare `raise`) introduces its `{Throw[String]}`/`{Abort}` carrier through the
        // instantiated `if`/`else`/`raise`, with no declared `paramConstraints` binder to key off. Its base carrier is
        // the guard channel's fixed `Throw[String]` carrier `Either[String]`; the `{Abort}` layer over it
        // (`AbortCarrier[Either[String]]`) is already solved *structurally* by the `else` signature's
        // `computation: AbortCarrier[G, A]` unification, so only the still-unsolved base metas (`else`'s `G`, `raise`'s
        // carrier) need pinning. Fixing them lets the `else`/`runAbort`/`raise` dispatch resolve so the return reduces
        // to `Right(t)` / `Left(msg)`.
        _ <- pinInferredReturnCarriers(checker, resolvedValue.name)
      } yield ()

    /** Pin each still-unsolved *effect-carrier* metavariable ([[com.vanillasource.eliot.eliotc.monomorphize.unify.Unifier.effectCarrierMetaIds]])
      * to the compile-time `Either[String]`. Used for an inline guard's inferred carrier, which has no
      * `paramConstraints` binder — see [[pinCarriers]]. A carrier already solved (the `AbortCarrier[G]` layer whose base
      * `G` is the meta left open) is skipped by [[pinMetaToEither]], which only pins a still-open meta.
      */
    private def pinInferredReturnCarriers(checker: Checker, at: Sourced[?]): CheckIO[Unit] =
      inspect(_.unifier.effectCarrierMetaIds).flatMap(_.traverse_(id => pinMetaToEither(checker, MetaId(id), at)))

    /** Solve a still-open effect-carrier meta to the fixed compile-time `Either[String]` (the guard channel's error
      * type is `String`). Skipped when the meta is already solved (its carrier was fixed structurally — e.g. an
      * `AbortCarrier[G]` layer — or by a prior declared-carrier pin).
      */
    private def pinMetaToEither(checker: Checker, id: MetaId, at: Sourced[?]): CheckIO[Unit] =
      checker.force(VMeta(id, Spine.SNil)).flatMap {
        case meta: VMeta =>
          val either = VTopDef(
            WellKnownTypes.eitherFQN,
            None,
            Spine.SNil :+ VTopDef(WellKnownTypes.stringFQN, None, Spine.SNil)
          )
          modify(s =>
            s.withUnifier(s.unifier.unify(meta, either, at.as("Compile-time effect carrier `Either[String]`.")))
          )
        case _           => pure(())
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
