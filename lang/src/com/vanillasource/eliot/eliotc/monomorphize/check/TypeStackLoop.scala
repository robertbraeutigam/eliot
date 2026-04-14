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

      // Drain unifier and report any unification errors
      _       <- modify(s => s.withUnifier(s.unifier.drain()))
      // Inject solutions for associated-type metas from resolved ability impls, then re-drain. This closes the
      // soundness loop: an abstract `type X` inside an ability becomes a meta during check, and when an ability
      // call resolves to a concrete impl, the impl's `type X = Concrete` value unifies into that meta.
      _       <- runtime.traverse_(r => injectAssociatedTypeSolutions(r.value, resolveAbility, fetchBinding))
      _       <- modify(s => s.withUnifier(s.unifier.drain()))
      // Default any unsolved metas to VType. These are "phantom" type parameters whose values never get constrained
      // (e.g., a generic parameter that doesn't appear in the signature besides its declaration). Leaving them
      // unsolved would fail strict quoting; defaulting to VType matches the pre-NbE behaviour for such params.
      _       <- modify(s => s.withUnifier(defaultUnsolvedMetas(s.unifier)))
      state   <- get
      _       <- state.unifier.errors.reverse.traverse_(err => liftF(reportUnifyError(err, state)))

      // If unification had errors, abort before quoting — no meaningful MonomorphicValue can be produced.
      _ <- if (state.unifier.errors.nonEmpty) liftF(abort[Unit]) else pure(())

      // Post-drain: quote SemValues to GroundValues. This is the sole SemValue → GroundValue
      // transition and has no silent fallback; Quoter reports unresolved metas as compiler errors.
      quoter     = new PostDrainQuoter(
                     state.unifier.metaStore,
                     resolvedValue.paramConstraints,
                     resolveAbility,
                     state.bindingCache,
                     state.env,
                     state.nameLevels
                   )
      groundSig <- liftF(quoter.quoteSem(instantiated, resolvedValue.typeStack))
      monoBody  <- runtime.traverse(srcSem => liftF(quoter.quoteSourced(srcSem)))
    } yield MonomorphicValue(
      key.vfqn,
      key.typeArguments,
      resolvedValue.typeStack.as(key.vfqn.name),
      groundSig,
      monoBody.map(sourcedMono => sourcedMono.as(sourcedMono.value.expression))
    )

  /** Walk a [[SemExpression]] tree and unify each abstract associated-type meta against the concrete impl's
    * corresponding associated-type value. For every ability-qualified value reference encountered, the method resolves
    * the call to its impl and — for each associated-type meta registered in [[CheckState]] whose ability name matches
    * the ref's qualifier — fetches the impl's concrete value (same local name, impl's module, impl's
    * AbilityImplementation qualifier) and unifies it into the meta.
    */
  private def injectAssociatedTypeSolutions(
      expr: SemExpression,
      resolveAbility: (ValueFQN, Seq[GroundValue]) => CompilerIO[Option[(ValueFQN, Seq[GroundValue])]],
      fetchBinding: ValueFQN => CompilerIO[Option[SemValue]]
  ): CheckIO[Unit] =
    collectAbilityRefs(expr).traverse_ { case (vfqn, typeArgs, source) =>
      resolveAndInject(vfqn, typeArgs, source, resolveAbility, fetchBinding)
    }

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

  private def resolveAndInject(
      abilityVfqn: Sourced[ValueFQN],
      typeArgs: Seq[SemValue],
      source: Sourced[?],
      resolveAbility: (ValueFQN, Seq[GroundValue]) => CompilerIO[Option[(ValueFQN, Seq[GroundValue])]],
      fetchBinding: ValueFQN => CompilerIO[Option[SemValue]]
  ): CheckIO[Unit] =
    abilityVfqn.value.name.qualifier match {
      case Qualifier.Ability(abilityName) =>
        for {
          state      <- get
          // Quote type args — skip injection if any can't be quoted yet (constraint-covered refs, for example).
          groundArgsE = typeArgs.toList.traverse(a => Quoter.quote(0, a, state.unifier.metaStore))
          _          <- groundArgsE match {
                          case Right(groundArgs) =>
                            for {
                              resolved <- liftF(resolveAbility(abilityVfqn.value, groundArgs))
                              _        <- resolved match {
                                            case Some((implFqn, _)) => injectForImpl(abilityName, implFqn, source, fetchBinding)
                                            case None               => pure(())
                                          }
                            } yield ()
                          case Left(_)           => pure(())
                        }
        } yield ()
      case _                              => pure(())
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
