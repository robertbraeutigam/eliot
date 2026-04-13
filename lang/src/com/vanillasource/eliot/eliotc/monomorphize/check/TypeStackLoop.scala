package com.vanillasource.eliot.eliotc.monomorphize.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.check.CheckIO.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.*
import com.vanillasource.eliot.eliotc.monomorphize.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize.fact.*
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
      resolveAbility: (ValueFQN, Seq[GroundValue]) => CompilerIO[Option[ValueFQN]] = (_, _) => None.pure[CompilerIO]
  ): CompilerIO[MonomorphicValue] = {
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
      key: MonomorphicValue.Key,
      resolvedValue: OperatorResolvedValue
  ): CheckIO[MonomorphicValue] =
    for {
      // Walk type stack levels top-down
      signature <- walkTypeStack(checker, resolvedValue)

      // Apply explicit type args
      appliedSig   <- applyTypeArgs(checker, signature, key.typeArguments, resolvedValue.typeStack)
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
    } yield MonomorphicValue(
      key.vfqn,
      key.typeArguments,
      resolvedValue.typeStack.as(key.vfqn.name),
      groundSig,
      runtime
    )

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
