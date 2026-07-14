package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ability.fact.AbilityImplementation
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Quoter
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

/** The ability-resolution *saturation* concern (a checker collaborator, symmetrical with `carriers` /
  * `calcReturns`): discovering every ability-qualified value reference and resolving each to its concrete impl,
  * factored out of the [[TypeStackLoop]]'s post-drain fold. None of this is definitional equality: it re-enters
  * `getFact` to read an ability marker's arity, and it drives the `resolve-abilities` fixed-point pass.
  *
  * Two entry points, mirroring the two phases:
  *   - [[collectAbilityRefs]] — walk the checker's output trees to find every ability-qualified reference (called from
  *     `TypeStackLoop.processIO` to seed the pass context).
  *   - [[resolveAbilities]] — the `resolve-abilities` saturation pass body: try to resolve every still-unresolved
  *     reference once, recording each impl.
  *
  * The resolutions themselves live in the shared [[CheckState.abilityResolutions]] map — this module only *records*
  * into it. Operates over [[CheckIO]], reading and writing the shared [[CheckState]] through `get`/`modify`/`inspect`.
  * It depends on exactly the primitives passed at construction — that narrow surface is the module boundary.
  *
  * @param resolveAbility
  *   Resolve an ability at concrete type arguments to its impl — the checker's `resolveAbility`.
  * @param platform
  *   The track's platform, used to key the ability-marker signature fact.
  */
class AbilityResolver(
    resolveAbility: (ValueFQN, Seq[GroundValue]) => CompilerIO[Option[(ValueFQN, Seq[GroundValue])]],
    platform: Platform
) {
  import AbilityResolver.AbilityRef

  /** Walk a [[SemExpression]] tree and collect all ability-qualified [[SemExpression.ValueReference]] nodes with their
    * current type arguments. Non-ability references and other shapes are skipped.
    */
  def collectAbilityRefs(expr: Sourced[SemExpression]): Seq[AbilityRef] = {
    def go(se: SemExpression): Seq[AbilityRef] = se.expression match {
      case SemExpression.ValueReference(vfqn, typeArgs)        =>
        vfqn.value.name.qualifier match {
          case _: Qualifier.Ability => Seq((vfqn, typeArgs))
          case _                    => Seq.empty
        }
      case SemExpression.FunctionApplication(target, argument) =>
        go(target.value) ++ go(argument.value)
      case SemExpression.FunctionLiteral(_, _, body)           =>
        go(body.value)
      case _                                                   => Seq.empty
    }
    go(expr.value)
  }

  /** Saturation pass: try to resolve every still-unresolved ability reference once. Returns `true` iff at least one
    * ability newly resolved this call — a resolution records the impl, which the next round's drain propagates.
    */
  def resolveAbilities(
      allRefs: Seq[AbilityRef],
      paramConstraints: Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]]
  ): CheckIO[Boolean] =
    for {
      state      <- get
      unresolved  = allRefs.filterNot { case (ref, _) => state.abilityResolutions.contains(ref) }
      progressed <- unresolved.foldLeftM(false) { (acc, ref) =>
                      tryResolveOne(ref, paramConstraints).map(_ || acc)
                    }
    } yield progressed

  /** Try to resolve a single ability reference. Returns `true` iff the reference got newly resolved this call.
    *
    *   - If the ref is covered by an in-scope parameter constraint (e.g. calling a method of `Foo[T]` inside a `[T]
    *     with Foo[T]` context), the constraint's own type arguments are used — at monomorphization time they're already
    *     instantiated to the caller's concrete types, whereas the reference's implicit metas won't be solved until
    *     unification connects them. The constraint path is the direct way to get concrete args.
    *   - Otherwise the ref's own type arguments are used.
    *   - Quoting failure (still-unsolved metas) leaves the ref pending for the next iteration.
    *   - `resolveAbility` returning `None` at *ground* arguments is a failed **demand**: the
    *     [[AbilityImplementation]] fact's non-resolved outcome is read back and reported here, at this reference —
    *     the demand site — and the check aborts (see [[reportFailedDemand]]). Only an *absent* fact (the resolution
    *     aborted on an upstream error, already reported at its own definition) leaves the ref silently pending, to be
    *     emitted by [[PostDrainQuoter]] as an abstract reference.
    */
  private def tryResolveOne(
      ref: AbilityRef,
      paramConstraints: Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]]
  ): CheckIO[Boolean] = {
    val (abilityVfqn, refTypeArgs) = ref
    abilityVfqn.value.name.qualifier match {
      case Qualifier.Ability(abilityName) =>
        for {
          state      <- get
          // The ability query must carry ONLY the ability-level type arguments. A method reference's own type args are
          // `[abilityParams ++ methodParams]` (the ability's params are prepended to every method signature), so we
          // slice off the method-level params, keeping the leading `abilityArity` (e.g. `flatMap` of `Monad[F[_]]`
          // queries `Monad[F]`, not `Monad[F, A, B]`).
          arity      <- abilityArity(abilityVfqn.value, abilityName)
          refSliced   = refTypeArgs.take(arity)
          refGroundE  = refSliced.toList.traverse(a => Quoter.quote(0, a, state.unifier.metaStore))
          // Prefer the reference's OWN type arguments once they are fully ground: they pin the exact implementation.
          // This is essential when several constraints share one ability — e.g. `Dep[Database]` and `Dep[Logger]` in a
          // single body are both `Dep`, so the by-name constraint lookup would always return the first and every `get`
          // would mis-dispatch to it. The constraint path remains the fallback *while the ref's args are still unsolved
          // metas* (its original rationale: at the use site the caller has already instantiated the constraint to
          // concrete types, whereas the reference's implicit metas are solved only once unification connects them).
          groundArgsE = refGroundE match {
                          case Right(ground) => Right(ground)
                          case Left(_)       =>
                            state.findConstraintTypeArgs(paramConstraints, abilityName) match {
                              case Some(cArgs) =>
                                cArgs.toList.traverse(a => Quoter.quote(0, a, state.unifier.metaStore))
                              case None        => refGroundE
                            }
                        }
          progressed <- groundArgsE match {
                          case Right(groundArgs) =>
                            for {
                              resolved <- liftF(resolveAbility(abilityVfqn.value, groundArgs))
                              stepped  <- resolved match {
                                            case Some(impl) =>
                                              modify(_.recordAbilityResolution(abilityVfqn, impl)).as(true)
                                            case None       => reportFailedDemand(abilityVfqn, abilityName, groundArgs)
                                          }
                            } yield stepped
                          case Left(_)           => pure(false)
                        }
        } yield progressed
      case _                              => pure(false)
    }
  }

  /** A ground-argument demand did not resolve. The producer ([[com.vanillasource.eliot.eliotc.ability.processor.AbilityImplementationProcessor]])
    * registers the failed [[AbilityImplementation.Resolution]] outcome on the fact instead of erroring — the position
    * belongs to the demander, and this saturation pass is the demander: it holds the demanding reference, so the
    * failure is reported *here*, at the use site, and the value's monomorphization aborts (the hard error at the
    * manifest instantiation, ability-guards §3). An **absent** fact means the resolution itself aborted on an upstream
    * error already reported at its own definition, so nothing is added and the reference stays silently pending. A
    * `Resolved` outcome cannot reach this method (the `resolveAbility` callback returns it), but is left pending for
    * the next round rather than treated as an error, keeping the match total and fail-safe.
    */
  private def reportFailedDemand(
      ref: Sourced[ValueFQN],
      abilityName: String,
      groundArgs: Seq[GroundValue]
  ): CheckIO[Boolean] =
    liftF(getFactIfProduced(AbilityImplementation.Key(ref.value, groundArgs, platform))).flatMap { factOpt =>
      val argsShown = groundArgs.map(_.show).mkString("[", ", ", "]")
      factOpt.map(_.resolution) match {
        case None                                                        => pure(false)
        case Some(AbilityImplementation.Resolution.Resolved(_, _))       => pure(false)
        case Some(AbilityImplementation.Resolution.NoImplementation)     =>
          liftF(
            compilerError(
              ref.as(s"No ability implementation found for ability '$abilityName' with type arguments $argsShown.")
            ) >> abort[Boolean]
          )
        case Some(AbilityImplementation.Resolution.Rejected(messages))   =>
          liftF(messages.traverse_(message => compilerError(ref.as(message))) >> abort[Boolean])
        case Some(AbilityImplementation.Resolution.Ambiguous)            =>
          liftF(
            compilerError(
              ref.as(
                s"Multiple ability implementations found for ability '$abilityName' with type arguments $argsShown."
              )
            ) >> abort[Boolean]
          )
      }
    }

  /** The number of ability-level type parameters of the ability owning `methodVfqn` — read off the ability *marker*'s
    * signature (the synthetic value named after the ability, sharing the method's `Ability(name)` qualifier), whose
    * leading generic binders are exactly the ability's parameters. Used to slice an ability method reference's full
    * type args down to the ability-level prefix for the impl query. Falls back to `Int.MaxValue` (slice nothing) if the
    * marker has no resolvable signature, preserving prior behaviour rather than mis-slicing.
    */
  private def abilityArity(methodVfqn: ValueFQN, abilityName: String): CheckIO[Int] = {
    val markerFqn = ValueFQN(methodVfqn.moduleName, QualifiedName(abilityName, methodVfqn.name.qualifier))
    liftF(getFactIfProduced(OperatorResolvedValue.Key(markerFqn, platform))).map {
      case Some(orv) =>
        OperatorResolvedExpression.SignatureView.of(orv.signature).binders.length
      case None      => Int.MaxValue
    }
  }
}

object AbilityResolver {

  /** An ability-qualified value reference paired with its current type arguments (unquoted [[SemValue]]s). */
  type AbilityRef = (Sourced[ValueFQN], Seq[SemValue])
}
