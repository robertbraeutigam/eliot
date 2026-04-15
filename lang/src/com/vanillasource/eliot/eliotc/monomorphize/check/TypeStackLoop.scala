package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.{Evaluator, Quoter}
import com.vanillasource.eliot.eliotc.monomorphize.fact.*
import com.vanillasource.eliot.eliotc.monomorphize.unify.{SemValuePrinter, UnifyError}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

/** Uniform top-down fold over a value's type stack. Each level is processed identically — there is no concept of
  * "generic parameters" as a separate structure.
  */
object TypeStackLoop {

  def process(
      key: MonomorphicValue.Key,
      resolvedValue: OperatorResolvedValue,
      fetchBinding: ValueFQN => CompilerIO[Option[SemValue]],
      resolveAbility: (ValueFQN, Seq[GroundValue]) => CompilerIO[Option[(ValueFQN, Seq[GroundValue])]] = (_, _) =>
        None.pure[CompilerIO]
  ): CompilerIO[MonomorphicValue] = {
    val checker = new Checker(fetchBinding)
    processIO(checker, key, resolvedValue, fetchBinding, resolveAbility).runA(CheckState.initial)
  }

  private def processIO(
      checker: Checker,
      key: MonomorphicValue.Key,
      resolvedValue: OperatorResolvedValue,
      fetchBinding: ValueFQN => CompilerIO[Option[SemValue]],
      resolveAbility: (ValueFQN, Seq[GroundValue]) => CompilerIO[Option[(ValueFQN, Seq[GroundValue])]]
  ): CheckIO[MonomorphicValue] =
    for {
      // Walk type stack levels top-down
      signature <- walkTypeStack(checker, resolvedValue)

      // Apply explicit type args
      appliedSig   <- applyTypeArgs(checker, signature, key.typeArguments, resolvedValue.typeStack)
      instantiated <- instantiateRemaining(checker, appliedSig)

      // Check runtime body if present — produces SemExpression with SemValue slots
      runtime <- resolvedValue.runtime.traverse { body =>
                   checker.check(body, instantiated).map(expr => body.as(expr))
                 }

      // Drain-and-resolve loop: repeatedly drain the unifier, then try to resolve each pending ability reference;
      // on success, record the resolution and inject the impl's concrete associated-type values into their
      // standing metas, which re-feeds the next drain. Iterates until no new ability resolves, bounded for safety.
      _       <- drainAndResolveLoop(runtime, resolvedValue.paramConstraints, resolveAbility, fetchBinding)

      // Default any unsolved metas to VType. These are "phantom" type parameters whose values never get constrained
      // (e.g., a generic parameter that doesn't appear in the signature besides its declaration). Leaving them
      // unsolved would fail strict quoting; defaulting to VType matches the pre-NbE behaviour for such params.
      _     <- modify(s => s.withUnifier(defaultUnsolvedMetas(s.unifier)))
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

  /** Outer drain loop. Each iteration drains the unifier to solve pending constraints, then tries to resolve any
    * unresolved ability references. A resolution may inject associated-type solutions into the metastore, feeding the
    * next drain. The loop stops as soon as an iteration produces no new ability resolution, so the total iterations are
    * bounded by the number of ability references in the expression tree.
    *
    * An explicit iteration cap (twice the initial ref count) guards against pathological cases — hitting it would
    * indicate a bug in the resolver rather than a user error.
    */
  private def drainAndResolveLoop(
      runtime: Option[Sourced[SemExpression]],
      paramConstraints: Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]],
      resolveAbility: (ValueFQN, Seq[GroundValue]) => CompilerIO[Option[(ValueFQN, Seq[GroundValue])]],
      fetchBinding: ValueFQN => CompilerIO[Option[SemValue]]
  ): CheckIO[Unit] = {
    val allRefs                             = runtime.toSeq.flatMap(r => collectAbilityRefs(r.value))
    val maxIter                             = (allRefs.size * 2) + 2
    def loop(remaining: Int): CheckIO[Unit] =
      if (remaining <= 0) pure(())
      else
        for {
          _          <- modify(s => s.withUnifier(s.unifier.drain()))
          progressed <- resolvePendingAbilities(allRefs, paramConstraints, resolveAbility, fetchBinding)
          _          <- if (progressed) loop(remaining - 1) else pure(())
        } yield ()
    loop(maxIter)
  }

  /** Walk the set of ability references once, attempting to resolve each that isn't already resolved. Returns `true`
    * iff at least one ref was newly resolved in this pass.
    */
  private def resolvePendingAbilities(
      refs: Seq[(Sourced[ValueFQN], Seq[SemValue], Sourced[?])],
      paramConstraints: Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]],
      resolveAbility: (ValueFQN, Seq[GroundValue]) => CompilerIO[Option[(ValueFQN, Seq[GroundValue])]],
      fetchBinding: ValueFQN => CompilerIO[Option[SemValue]]
  ): CheckIO[Boolean] =
    refs.foldLeftM(false) { case (progressed, (vfqn, refTypeArgs, source)) =>
      for {
        state <- get
        step  <- if (state.abilityResolutions.contains(vfqn)) pure(false)
                 else
                   tryResolveOne(vfqn, refTypeArgs, source, paramConstraints, resolveAbility, fetchBinding)
      } yield progressed || step
    }

  /** Try to resolve a single ability reference. Returns `true` iff the reference got newly resolved this call.
    *
    *   - If the ref is covered by an in-scope parameter constraint, the constraint's ORE type arguments are evaluated
    *     (reproducing the behaviour the old [[PostDrainQuoter]].`findConstraintParam` path had).
    *   - Otherwise the ref's own (explicit ++ implicit) type arguments are used.
    *   - Quoting failure (still-unsolved metas) leaves the ref pending for the next iteration.
    *   - `resolveAbility` returning `None` also leaves it pending — the ref will stay unresolved in the final
    *     [[CheckState]] and [[PostDrainQuoter]] will emit it as an abstract reference.
    */
  private def tryResolveOne(
      abilityVfqn: Sourced[ValueFQN],
      refTypeArgs: Seq[SemValue],
      source: Sourced[?],
      paramConstraints: Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]],
      resolveAbility: (ValueFQN, Seq[GroundValue]) => CompilerIO[Option[(ValueFQN, Seq[GroundValue])]],
      fetchBinding: ValueFQN => CompilerIO[Option[SemValue]]
  ): CheckIO[Boolean] =
    abilityVfqn.value.name.qualifier match {
      case Qualifier.Ability(abilityName) =>
        for {
          state        <- get
          effectiveArgs = findConstraintTypeArgs(paramConstraints, abilityName, state).getOrElse(refTypeArgs)
          groundArgsE   = effectiveArgs.toList.traverse(a => Quoter.quote(0, a, state.unifier.metaStore))
          progressed   <- groundArgsE match {
                            case Right(groundArgs) =>
                              for {
                                resolved <- liftF(resolveAbility(abilityVfqn.value, groundArgs))
                                stepped  <- resolved match {
                                              case Some(impl) =>
                                                for {
                                                  _ <- modify(_.recordAbilityResolution(abilityVfqn, impl))
                                                  _ <- injectForImpl(abilityName, impl._1, source, fetchBinding)
                                                } yield true
                                              case None       => pure(false)
                                            }
                              } yield stepped
                            case Left(_)           => pure(false)
                          }
        } yield progressed
      case _                              => pure(false)
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
        val evaluator = new Evaluator(vfqn => state.bindingCache.getOrElse(vfqn, None), state.nameLevels)
        c.typeArgs.map(arg => evaluator.eval(state.env, arg))
      }
    })

  /** Collect every ability-qualified [[SemExpression.ValueReference]] in the tree together with its explicit and
    * implicit type arguments and its source position. Only references whose qualifier is [[Qualifier.Ability]] are
    * candidates for resolution.
    */
  private def collectAbilityRefs(
      expr: SemExpression
  ): Seq[(Sourced[ValueFQN], Seq[SemValue], Sourced[?])] = {
    val here     = expr.expression match {
      case SemExpression.ValueReference(vfqn, explicit, implicits) =>
        vfqn.value.name.qualifier match {
          case _: Qualifier.Ability => Seq((vfqn, explicit ++ implicits, vfqn))
          case _                    => Seq.empty
        }
      case _                                                       => Seq.empty
    }
    val children = expr.expression match {
      case SemExpression.FunctionApplication(t, a)   =>
        collectAbilityRefs(t.value) ++ collectAbilityRefs(a.value)
      case SemExpression.FunctionLiteral(_, _, body) =>
        collectAbilityRefs(body.value)
      case _                                         => Seq.empty
    }
    here ++ children
  }

  private def injectForImpl(
      abilityName: String,
      implFqn: ValueFQN,
      source: Sourced[?],
      fetchBinding: ValueFQN => CompilerIO[Option[SemValue]]
  ): CheckIO[Unit] =
    for {
      state <- get
      _     <- state.associatedTypeMetas.toSeq.traverse_ { case (absFqn, metaId) =>
                 absFqn.name.qualifier match {
                   case Qualifier.Ability(aname) if aname == abilityName =>
                     val implAssocFqn = ValueFQN(
                       implFqn.moduleName,
                       QualifiedName(absFqn.name.name, implFqn.name.qualifier)
                     )
                     for {
                       binding <- liftF(fetchBinding(implAssocFqn))
                       _       <- binding match {
                                    case Some(sem) =>
                                      modify(s =>
                                        s.withUnifier(
                                          s.unifier.unify(
                                            VMeta(metaId, Spine.SNil, VType),
                                            sem,
                                            source.as(s"Associated type '${absFqn.name.name}' mismatch.")
                                          )
                                        )
                                      )
                                    case None      => pure(())
                                  }
                     } yield ()
                   case _                                                => pure(())
                 }
               }
    } yield ()

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

  /** After drain, solve any still-unsolved metavariables with [[VType]]. This handles phantom type parameters — metas
    * introduced for type parameters that never get referenced or constrained (e.g. the `NAME` param of a generated
    * `TypeMatch[Person[NAME]]` impl when matching `t match { case Person[n] -> ... }`). Leaving them unsolved would
    * fail the strict post-drain quoter; they are semantically free to be any type.
    */
  private def defaultUnsolvedMetas(
      unifier: com.vanillasource.eliot.eliotc.monomorphize.unify.Unifier
  ): com.vanillasource.eliot.eliotc.monomorphize.unify.Unifier = {
    val store          = unifier.metaStore
    val defaultedStore = store.entries.foldLeft(store) {
      case (acc, (id, None))   => acc.solve(SemValue.MetaId(id), VType)
      case (acc, (_, Some(_))) => acc
    }
    unifier.copy(metaStore = defaultedStore)
  }

  private def walkTypeStack(
      checker: Checker,
      resolvedValue: OperatorResolvedValue
  ): CheckIO[SemValue] = {
    val levels = resolvedValue.typeStack.value.levels.toSeq.reverse
    levels.foldLeftM(VType.asInstanceOf[SemValue]) { (expected, level) =>
      checker.check(resolvedValue.typeStack.as(level), expected) >>
        checker.evalExpr(level)
    }
  }

  /** Instantiate any remaining VLam closures (unapplied type parameters) with fresh metas. This handles phantom type
    * parameters and cases where fewer explicit type args were provided than type parameters exist.
    */
  private def instantiateRemaining(checker: Checker, sig: SemValue): CheckIO[SemValue] =
    for {
      forced <- checker.force(sig)
      result <- forced match {
                  case VLam(name, closure) =>
                    for {
                      meta <- checker.freshMeta
                      _    <- modify(_.bind(name, meta))
                      rest <- instantiateRemaining(checker, closure(meta))
                    } yield rest
                  case other               => pure(other)
                }
    } yield result

  /** Apply concrete GroundValue type arguments to the signature by wrapping each in VConst and applying to VLam
    * closures.
    */
  private def applyTypeArgs(
      checker: Checker,
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
}
