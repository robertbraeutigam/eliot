package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Quoter
import com.vanillasource.eliot.eliotc.monomorphize.fact.*
import com.vanillasource.eliot.eliotc.monomorphize.unify.{SemValuePrinter, UnifyError}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

/** Uniform top-down fold over a value's type stack. Each level is processed identically — there is no concept of
  * "generic parameters" as a separate structure.
  *
  * Holds the callbacks and the [[Checker]] as fields so internal helpers don't have to thread them.
  */
class TypeStackLoop(
    fetchBinding: ValueFQN => CompilerIO[Option[SemValue]],
    resolveAbility: (ValueFQN, Seq[GroundValue]) => CompilerIO[Option[(ValueFQN, Seq[GroundValue])]]
) {
  import TypeStackLoop.AbilityRef

  private val checker = new Checker(fetchBinding)

  def process(
      key: MonomorphicValue.Key,
      resolvedValue: OperatorResolvedValue
  ): CompilerIO[MonomorphicValue] =
    processIO(key, resolvedValue).runA(CheckState.initial)

  private def processIO(
      key: MonomorphicValue.Key,
      resolvedValue: OperatorResolvedValue
  ): CheckIO[MonomorphicValue] =
    for {
      // Walk type stack levels top-down
      signature <- walkTypeStack(resolvedValue)

      // Apply explicit type args
      appliedSig   <- applyTypeArgs(signature, key.typeArguments, resolvedValue.typeStack)
      instantiated <- instantiateRemaining(appliedSig)

      // Check runtime body if present — produces SemExpression with SemValue slots
      runtime <- resolvedValue.runtime.traverse { body =>
                   checker.check(body, instantiated).map(expr => body.as(expr))
                 }

      // Drain-and-resolve loop: repeatedly drain the unifier, then try to resolve each pending ability reference;
      // on success, record the resolution and inject the impl's concrete associated-type values into their
      // standing metas, which re-feeds the next drain. Iterates until no new ability resolves, bounded for safety.
      _       <- drainAndResolveLoop(runtime, resolvedValue.paramConstraints)

      // Default any unsolved metas to VType. These are "phantom" type parameters whose values never get constrained
      // (e.g., a generic parameter that doesn't appear in the signature besides its declaration). Leaving them
      // unsolved would fail strict quoting; defaulting to VType matches the pre-NbE behaviour for such params.
      _     <- modify(s => s.withUnifier(s.unifier.defaultUnsolvedTo(VType)))
      state <- get
      _     <- state.unifier.errors.reverse.traverse_(err => liftF(reportUnifyError(err, state)))

      // If unification had errors, abort before quoting — no meaningful MonomorphicValue can be produced.
      _ <- if (state.unifier.errors.nonEmpty) liftF(abort[Unit]) else pure(())

      // Post-drain: quote SemValues to GroundValues using the pre-computed ability resolutions. This is the sole
      // SemValue → GroundValue transition and has no silent fallback; Quoter reports unresolved metas as compiler
      // errors.
      quoter     = new PostDrainQuoter(state.unifier.metaStore, state.abilityResolutions)
      groundSig <- liftF(quoter.quoteSem(instantiated, resolvedValue.typeStack))
      monoBody  <- runtime.traverse(srcSem => liftF(quoter.quoteSourced(srcSem)))
    } yield MonomorphicValue(
      key.vfqn,
      key.typeArguments,
      resolvedValue.typeStack.as(key.vfqn.name),
      groundSig,
      monoBody.map(sourcedMono => sourcedMono.as(sourcedMono.value.expression))
    )

  /** Outer drain-and-resolve loop. Each iteration drains the unifier to solve pending constraints, then tries to
    * resolve each still-pending ability reference. A resolution may inject associated-type solutions into the
    * metastore, feeding the next drain. Resolved refs drop out of the pending set, so iteration is bounded by the
    * initial ref count.
    */
  private def drainAndResolveLoop(
      runtime: Option[Sourced[SemExpression]],
      paramConstraints: Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]]
  ): CheckIO[Unit] = {
    def loop(pending: Seq[AbilityRef]): CheckIO[Unit] =
      for {
        _            <- modify(s => s.withUnifier(s.unifier.drain()))
        stillPending <- pending.foldLeftM(Seq.empty[AbilityRef]) { (acc, ref) =>
                          tryResolveOne(ref, paramConstraints).map {
                            case true  => acc
                            case false => acc :+ ref
                          }
                        }
        _            <- if (stillPending.size < pending.size) loop(stillPending) else pure(())
      } yield ()
    loop(runtime.toSeq.flatMap(r => TypeStackLoop.collectAbilityRefs(r.value)))
  }

  /** Try to resolve a single ability reference. Returns `true` iff the reference got newly resolved this call.
    *
    *   - If the ref is covered by an in-scope parameter constraint, the constraint's ORE type arguments are evaluated
    *     (reproducing the behaviour the old [[PostDrainQuoter]].`findConstraintParam` path had).
    *   - Otherwise the ref's own type arguments are used.
    *   - Quoting failure (still-unsolved metas) leaves the ref pending for the next iteration.
    *   - `resolveAbility` returning `None` also leaves it pending — the ref will stay unresolved in the final
    *     [[CheckState]] and [[PostDrainQuoter]] will emit it as an abstract reference.
    */
  private def tryResolveOne(
      ref: AbilityRef,
      paramConstraints: Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]]
  ): CheckIO[Boolean] = {
    val (abilityVfqn, refTypeArgs) = ref
    abilityVfqn.value.name.qualifier match {
      case Qualifier.Ability(abilityName) =>
        for {
          state        <- get
          effectiveArgs =
            TypeStackLoop.findConstraintTypeArgs(paramConstraints, abilityName, state).getOrElse(refTypeArgs)
          groundArgsE   = effectiveArgs.toList.traverse(a => Quoter.quote(0, a, state.unifier.metaStore))
          progressed   <- groundArgsE match {
                            case Right(groundArgs) =>
                              for {
                                resolved <- liftF(resolveAbility(abilityVfqn.value, groundArgs))
                                stepped  <- resolved match {
                                              case Some(impl) =>
                                                for {
                                                  _ <- modify(_.recordAbilityResolution(abilityVfqn, impl))
                                                  _ <- injectForImpl(abilityName, impl._1, abilityVfqn)
                                                } yield true
                                              case None       => pure(false)
                                            }
                              } yield stepped
                            case Left(_)           => pure(false)
                          }
        } yield progressed
      case _                              => pure(false)
    }
  }

  private def injectForImpl(
      abilityName: String,
      implFqn: ValueFQN,
      source: Sourced[?]
  ): CheckIO[Unit] = {
    def isForThisAbility(absFqn: ValueFQN): Boolean = absFqn.name.qualifier match {
      case Qualifier.Ability(aname) => aname == abilityName
      case _                        => false
    }
    def implAssocFqnFor(absFqn: ValueFQN): ValueFQN =
      ValueFQN(implFqn.moduleName, QualifiedName(absFqn.name.name, implFqn.name.qualifier))
    for {
      state <- get
      _     <- state.associatedTypeMetas.toSeq
                 .filter { case (absFqn, _) => isForThisAbility(absFqn) }
                 .traverse_ { case (absFqn, metaId) =>
                   liftF(fetchBinding(implAssocFqnFor(absFqn))).flatMap {
                     case None      => pure(())
                     case Some(sem) =>
                       modify(s =>
                         s.withUnifier(
                           s.unifier.unify(
                             VMeta(metaId, Spine.SNil),
                             sem,
                             source.as(s"Associated type '${absFqn.name.name}' mismatch.")
                           )
                         )
                       )
                   }
                 }
    } yield ()
  }

  private def walkTypeStack(resolvedValue: OperatorResolvedValue): CheckIO[SemValue] = {
    val levels = resolvedValue.typeStack.value.levels.toSeq.reverse
    levels.foldLeftM(VType: SemValue) { (expected, level) =>
      checker.check(resolvedValue.typeStack.as(level), expected) >>
        checker.evalExpr(level)
    }
  }

  /** Instantiate any remaining VLam closures (unapplied type parameters) with fresh metas, binding each in the env.
    * This handles phantom type parameters and cases where fewer explicit type args were provided than type parameters
    * exist.
    */
  private def instantiateRemaining(sig: SemValue): CheckIO[SemValue] =
    checker.peelLams(sig, (name, meta) => modify(_.bind(name, meta))).map(_._1)

  /** Apply concrete GroundValue type arguments to the signature by wrapping each in VConst and applying to VLam
    * closures.
    */
  private def applyTypeArgs(
      signature: SemValue,
      typeArgs: Seq[GroundValue],
      errorSource: Sourced[?]
  ): CheckIO[SemValue] =
    typeArgs.foldLeftM(signature) { (sig, typeArg) =>
      val argVal = VConst(typeArg)
      for {
        forced <- checker.force(sig)
        result <- forced match {
                    case VLam(name, closure) =>
                      modify(_.bind(name, argVal)).as(closure(argVal))
                    case _                   =>
                      modify(s => s.withUnifier(s.unifier.addError(errorSource.as("Too many type arguments.")))).as(sig)
                  }
      } yield result
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

  /** Static convenience wrapper — constructs a [[TypeStackLoop]] and runs its [[process]]. */
  def process(
      key: MonomorphicValue.Key,
      resolvedValue: OperatorResolvedValue,
      fetchBinding: ValueFQN => CompilerIO[Option[SemValue]],
      resolveAbility: (ValueFQN, Seq[GroundValue]) => CompilerIO[Option[(ValueFQN, Seq[GroundValue])]] = (_, _) =>
        None.pure[CompilerIO]
  ): CompilerIO[MonomorphicValue] =
    new TypeStackLoop(fetchBinding, resolveAbility).process(key, resolvedValue)

  private type AbilityRef = (Sourced[ValueFQN], Seq[SemValue])

  /** Collect every ability-qualified [[SemExpression.ValueReference]] in the tree together with its type arguments. The
    * sourced vfqn itself carries the source position used for error attribution.
    */
  private def collectAbilityRefs(expr: SemExpression): Seq[AbilityRef] =
    expr.expression match {
      case SemExpression.ValueReference(vfqn, typeArgs) =>
        vfqn.value.name.qualifier match {
          case _: Qualifier.Ability => Seq((vfqn, typeArgs))
          case _                    => Seq.empty
        }
      case SemExpression.FunctionApplication(t, a)      =>
        collectAbilityRefs(t.value) ++ collectAbilityRefs(a.value)
      case SemExpression.FunctionLiteral(_, _, body)    =>
        collectAbilityRefs(body.value)
      case _                                            => Seq.empty
    }

  /** Look up the first in-scope parameter constraint that targets the given ability name and return its type arguments
    * evaluated against the current check state. Mirrors the old [[PostDrainQuoter]] path.
    */
  private def findConstraintTypeArgs(
      paramConstraints: Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]],
      abilityName: String,
      state: CheckState
  ): Option[Seq[SemValue]] =
    paramConstraints.collectFirst(Function.unlift { (_, constraints) =>
      constraints.find(_.abilityFQN.abilityName == abilityName).map { c =>
        c.typeArgs.map(arg => state.makeEvaluator.eval(state.env, arg))
      }
    })
}
