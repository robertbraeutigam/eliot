package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.eval.Quoter
import com.vanillasource.eliot.eliotc.monomorphize.fact.*
import com.vanillasource.eliot.eliotc.monomorphize.unify.{SemValuePrinter, UnifyError}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
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
      signature <- walkTypeStack(resolvedValue.typeStack)

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
      _       <- drainAndResolveLoop(resolvedValue.paramConstraints)

      // Default any still-unsolved phantom metas (those allocated while peeling VLam closures from polytype
      // signatures) to VType. Any other unsolved meta surfaces as a compiler error at quoting time.
      _     <- defaultPhantomMetas
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

  /** Solve every still-unsolved phantom meta (see [[CheckState.phantomMetas]]) to [[VType]]. Runs after the
    * drain-and-resolve loop has reached its fixed point, so any meta that unification could have determined has already
    * been solved.
    */
  private def defaultPhantomMetas: CheckIO[Unit] =
    modify { s =>
      val solved = s.phantomMetas.foldLeft(s.unifier.metaStore) { (store, id) =>
        store.lookup(id) match {
          case Some(_) => store
          case None    => store.solve(id, VType)
        }
      }
      s.withUnifier(s.unifier.copy(metaStore = solved))
    }

  /** Single fixed-point loop over the combined (unifier-drain + ability-resolve) state. Each step drains the unifier to
    * a fixed point and then attempts every still-unresolved ability reference from [[CheckState.abilityRefs]]. A
    * successful ability resolution may inject associated-type solutions into the metastore, so if any ability newly
    * resolved during the step we iterate again.
    *
    * Because each ability resolves at most once (resolved refs live in [[CheckState.abilityResolutions]] and are
    * skipped on subsequent iterations), the outer loop is bounded by the initial ability count.
    */
  private def drainAndResolveLoop(
      paramConstraints: Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]]
  ): CheckIO[Unit] = {
    val step: CheckIO[Boolean] =
      for {
        _          <- modify(s => s.withUnifier(s.unifier.drain()))
        state      <- get
        unresolved  = state.abilityRefs.toSeq.filterNot { case (ref, _) => state.abilityResolutions.contains(ref) }
        progressed <- unresolved.foldLeftM(false) { (acc, ref) =>
                        tryResolveOne(ref, paramConstraints).map(_ || acc)
                      }
      } yield progressed

    def loop: CheckIO[Unit] = step.flatMap(if (_) loop else pure(()))
    loop
  }

  /** Try to resolve a single ability reference. Returns `true` iff the reference got newly resolved this call.
    *
    *   - If the ref is covered by an in-scope parameter constraint (e.g. calling a method of `Foo[T]` inside a `[T]
    *     with Foo[T]` context), the constraint's own type arguments are used — at monomorphization time they're already
    *     instantiated to the caller's concrete types, whereas the reference's implicit metas won't be solved until
    *     unification connects them. The constraint path is the direct way to get concrete args.
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
            state.findConstraintTypeArgs(paramConstraints, abilityName).getOrElse(refTypeArgs)
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
      _     <- state.abstractTypeMetas.iterator
                 .filter { case (absFqn, _) => isForThisAbility(absFqn) }
                 .toSeq
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

  /** Walk the type stack top-down, folding each level against the one above as its expected kind.
    *
    * For each level we (a) kind-check the ORE against the running `expected` so ill-kinded signatures surface as errors
    * attached to the correct source, and (b) evaluate the ORE to obtain the next-lower level's expected kind. The
    * [[SemExpression]] returned by the kind-check is discarded — the checker is invoked purely for its unification side
    * effects and param-name bindings. The final fold result is the signature's [[SemValue]].
    */
  private def walkTypeStack(typeStack: Sourced[TypeStack[OperatorResolvedExpression]]): CheckIO[SemValue] =
    typeStack.value.levels.toSeq.reverse.foldLeftM(VType: SemValue) { (expected, level) =>
      checker.check(typeStack.as(level), expected) >>
        checker.evalExpr(level)
    }

  /** Instantiate any remaining VLam closures (unapplied type parameters) with fresh metas, binding each in the env.
    * This handles phantom type parameters and cases where fewer explicit type args were provided than type parameters
    * exist.
    */
  private def instantiateRemaining(sig: SemValue): CheckIO[SemValue] =
    checker.peelLams(sig, bindInEnv = true).map(_._1)

  /** Apply concrete GroundValue type arguments to the signature by wrapping each in VConst and applying to VLam
    * closures. Stops on the first non-VLam head, recording a single "Too many type arguments." error rather than one
    * per excess arg.
    */
  private def applyTypeArgs(
      signature: SemValue,
      typeArgs: Seq[GroundValue],
      errorSource: Sourced[?]
  ): CheckIO[SemValue] = {
    def loop(sig: SemValue, remaining: List[GroundValue]): CheckIO[SemValue] = remaining match {
      case Nil          => pure(sig)
      case head :: tail =>
        val argVal = VConst(head)
        for {
          forced <- checker.force(sig)
          result <- forced match {
                      case VLam(name, closure) =>
                        modify(_.bind(name, argVal)) >> loop(closure(argVal), tail)
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

  /** Static convenience wrapper — constructs a [[TypeStackLoop]] and runs its [[process]]. */
  def process(
      key: MonomorphicValue.Key,
      resolvedValue: OperatorResolvedValue,
      fetchBinding: ValueFQN => CompilerIO[Option[SemValue]],
      resolveAbility: (ValueFQN, Seq[GroundValue]) => CompilerIO[Option[(ValueFQN, Seq[GroundValue])]]
  ): CompilerIO[MonomorphicValue] =
    new TypeStackLoop(fetchBinding, resolveAbility).process(key, resolvedValue)

  private type AbilityRef = (Sourced[ValueFQN], Seq[SemValue])
}
