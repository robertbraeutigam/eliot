package com.vanillasource.eliot.eliotc.monomorphize3.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize3.domain.*
import com.vanillasource.eliot.eliotc.monomorphize3.domain.SemValue.*
import com.vanillasource.eliot.eliotc.monomorphize3.eval.{Evaluator, Quoter}
import com.vanillasource.eliot.eliotc.monomorphize3.fact.*
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.{compilerError, compilerAbort}

/** Uniform top-down fold over a value's type stack. Each level is processed identically — there is no concept of
  * "generic parameters" as a separate structure.
  */
object TypeStackLoop {

  def process(
      key: Monomorphic3Value.Key,
      resolvedValue: OperatorResolvedValue,
      fetchBinding: ValueFQN => CompilerIO[Option[SemValue]],
      fetchValueType: ValueFQN => CompilerIO[Option[SemValue]]
  ): CompilerIO[Monomorphic3Value] = {
    val checker = new Checker(CheckState.initial, fetchBinding, fetchValueType)

    for {
      // Pre-fetch all bindings referenced in the type stack and runtime body
      _ <- prefetchAllBindings(checker, resolvedValue)

      // Step 1-2: Walk type stack levels top-down
      signature <- walkTypeStack(checker, resolvedValue)

      // Step 4: Apply explicit type args (pre-fetch their bindings too)
      _         <- key.specifiedTypeArguments.traverse_(ta => checker.prefetchBindings(ta.value))
      appliedSig = instantiateRemaining(checker, applyTypeArgs(checker, signature, key.specifiedTypeArguments))

      // Step 5: Check runtime body if present
      runtime <- resolvedValue.runtime.traverse { body =>
                   checker.check(body, appliedSig).map(expr => body.as(expr.expression))
                 }

      // Step 6: Drain unifier and produce output
      _        = checker.state.unifier.drain()

      // Report any unification errors (each error carries its own source position)
      _        <- checker.state.unifier.errors.reverse.foldLeft(().pure[CompilerIO]) { (acc, sourcedMsg) =>
                    acc >> compilerError(sourcedMsg)
                  }

      // Quote the signature to GroundValue
      groundSig = checker.forceAndConst(appliedSig)
    } yield Monomorphic3Value(
      key.vfqn,
      key.specifiedTypeArguments,
      groundSig,
      runtime
    )
  }

  private def prefetchAllBindings(
      checker: Checker,
      resolvedValue: OperatorResolvedValue
  ): CompilerIO[Unit] = {
    val levels = resolvedValue.typeStack.value.levels.toSeq
    levels.traverse_(level => checker.prefetchBindings(level)) >>
      resolvedValue.runtime.traverse_(body => checker.prefetchBindings(body.value))
  }

  private def walkTypeStack(
      checker: Checker,
      resolvedValue: OperatorResolvedValue
  ): CompilerIO[SemValue] = {
    val levels = resolvedValue.typeStack.value.levels.toSeq.reverse
    levels.foldLeft(VType.asInstanceOf[SemValue].pure[CompilerIO]) { (accIO, level) =>
      for {
        expected <- accIO
        _        <- checker.check(resolvedValue.typeStack.as(level), expected)
        evaluated = checker.evalExpr(checker.state.env, level)
      } yield evaluated
    }
  }

  /** Instantiate any remaining VLam closures (unapplied type parameters) with fresh metas. This handles phantom type
    * parameters and cases where fewer explicit type args were provided than type parameters exist.
    */
  private def instantiateRemaining(checker: Checker, sig: SemValue): SemValue = {
    val forced = Evaluator.force(sig, checker.state.unifier.metaStore)
    forced match {
      case VLam(name, closure) =>
        val (metaId, freshStore) = checker.state.unifier.metaStore.fresh
        checker.state.unifier.metaStore = freshStore
        val freshMeta            = VMeta(metaId, Spine.SNil, VType)
        checker.state = checker.state.bind(name, freshMeta)
        instantiateRemaining(checker, closure(freshMeta))
      case other               => other
    }
  }

  private def applyTypeArgs(
      checker: Checker,
      signature: SemValue,
      typeArgs: Seq[Sourced[OperatorResolvedExpression]]
  ): SemValue =
    typeArgs.foldLeft(signature) { (sig, typeArg) =>
      val argVal = checker.evalExpr(checker.state.env, typeArg.value)
      val forced = Evaluator.force(sig, checker.state.unifier.metaStore)
      forced match {
        case VLam(name, closure) =>
          // Bind the type parameter to its concrete value so the runtime body check
          // resolves type parameters to their monomorphized types
          checker.state = checker.state.bind(name, argVal)
          closure(argVal)
        case _                   =>
          // Too many type arguments — signature is no longer polymorphic (VLam)
          checker.state.unifier.errors = typeArg.as("Too many type arguments.") :: checker.state.unifier.errors
          sig
      }
    }
}
