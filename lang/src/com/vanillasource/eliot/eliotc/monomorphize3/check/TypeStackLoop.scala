package com.vanillasource.eliot.eliotc.monomorphize3.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize3.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize3.domain.*
import com.vanillasource.eliot.eliotc.monomorphize3.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize3.eval.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize3.fact.*
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

/** Uniform top-down fold over a value's type stack. Each level is processed identically — there is no concept of
  * "generic parameters" as a separate structure.
  */
object TypeStackLoop {

  def process(
      key: Monomorphic3Value.Key,
      resolvedValue: OperatorResolvedValue,
      fetchBinding: ValueFQN => CompilerIO[Option[SemValue]],
      fetchValueType: ValueFQN => CompilerIO[Option[SemValue]],
      resolveAbility: (ValueFQN, Seq[Value]) => CompilerIO[Option[ValueFQN]] = (_, _) => None.pure[CompilerIO]
  ): CompilerIO[Monomorphic3Value] = {
    val checker = new Checker(
      fetchBinding,
      fetchValueType,
      resolvedValue.paramConstraints,
      resolveAbility
    )

    processIO(checker, key, resolvedValue).runA(CheckState.initial)
  }

  private def processIO(
      checker: Checker,
      key: Monomorphic3Value.Key,
      resolvedValue: OperatorResolvedValue
  ): CheckIO[Monomorphic3Value] =
    for {
      // Pre-fetch all bindings referenced in the type stack and runtime body
      _ <- prefetchAllBindings(checker, resolvedValue)

      // Walk type stack levels top-down
      signature <- walkTypeStack(checker, resolvedValue)

      // Apply explicit type args (pre-fetch their bindings too)
      _            <- key.specifiedTypeArguments.traverse_(ta => checker.prefetchBindings(ta.value))
      appliedSig   <- applyTypeArgs(checker, signature, key.specifiedTypeArguments)
      instantiated <- instantiateRemaining(checker, appliedSig)

      // Check runtime body if present
      runtime <- resolvedValue.runtime.traverse { body =>
                   checker.check(body, instantiated).map(expr => body.as(expr.expression))
                 }

      // Drain unifier and produce output
      _         <- modify(s => s.withUnifier(s.unifier.drain()))
      state     <- get
      _         <- state.unifier.errors.reverse.traverse_(msg => liftF(compilerError(msg)))
      groundSig <- checker.forceAndConst(instantiated)
    } yield Monomorphic3Value(
      key.vfqn,
      key.specifiedTypeArguments,
      groundSig,
      runtime
    )

  private def prefetchAllBindings(
      checker: Checker,
      resolvedValue: OperatorResolvedValue
  ): CheckIO[Unit] = {
    val levels = resolvedValue.typeStack.value.levels.toSeq
    levels.traverse_(checker.prefetchBindings) >>
      resolvedValue.runtime.traverse_(body => checker.prefetchBindings(body.value)) >>
      resolvedValue.paramConstraints.values.toSeq.flatten.traverse_ { constraint =>
        constraint.typeArgs.traverse_(checker.prefetchBindings)
      }
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
      forced <- inspect(s => Evaluator.force(sig, s.unifier.metaStore))
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

  private def applyTypeArgs(
      checker: Checker,
      signature: SemValue,
      typeArgs: Seq[Sourced[OperatorResolvedExpression]]
  ): CheckIO[SemValue] =
    typeArgs.foldLeftM(signature) { (sig, typeArg) =>
      for {
        argVal <- checker.evalExpr(typeArg.value)
        forced <- inspect(s => Evaluator.force(sig, s.unifier.metaStore))
        result <- forced match {
                    case VLam(name, closure) =>
                      modify(_.bind(name, argVal)).as(closure(argVal))
                    case _                   =>
                      modify(s =>
                        s.withUnifier(
                          s.unifier.copy(errors = typeArg.as("Too many type arguments.") :: s.unifier.errors)
                        )
                      ).as(sig)
                  }
      } yield result
    }
}
