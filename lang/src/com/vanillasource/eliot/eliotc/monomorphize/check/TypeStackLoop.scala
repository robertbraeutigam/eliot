package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
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
      fetchValueType: ValueFQN => CompilerIO[Option[SemValue]],
      resolveAbility: (ValueFQN, Seq[GroundValue]) => CompilerIO[Option[(ValueFQN, Seq[GroundValue])]] =
        (_, _) => None.pure[CompilerIO]
  ): CompilerIO[MonomorphicValue] = {
    val checker = new Checker(fetchBinding, fetchValueType)
    processIO(checker, key, resolvedValue, resolveAbility).runA(CheckState.initial)
  }

  private def processIO(
      checker: Checker,
      key: MonomorphicValue.Key,
      resolvedValue: OperatorResolvedValue,
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
      _     <- modify(s => s.withUnifier(s.unifier.drain()))
      state <- get
      _     <- state.unifier.errors.reverse.traverse_(err => liftF(reportUnifyError(err, state)))

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

  /** Emit a [[UnifyError]] as a compiler error, including `Expected` / `Actual` hints when the error carries both sides.
    * The semantic values are re-forced through the final metastore so any metas that were solved after the error was
    * raised display their resolution.
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
                      modify(s =>
                        s.withUnifier(s.unifier.addError(errorSource.as("Too many type arguments.")))
                      ).as(sig)
                  }
      } yield result
    }
}
