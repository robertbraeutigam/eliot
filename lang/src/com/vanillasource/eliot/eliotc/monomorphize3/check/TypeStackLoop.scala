package com.vanillasource.eliot.eliotc.monomorphize3.check

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.Value
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
  * "generic parameters" as a separate structure. All state is threaded functionally through CheckState.
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

    for {
      // Pre-fetch all bindings referenced in the type stack and runtime body
      state0 <- prefetchAllBindings(checker, CheckState.initial, resolvedValue)

      // Step 1-2: Walk type stack levels top-down
      (signature, state1) <- walkTypeStack(checker, state0, resolvedValue)

      // Step 4: Apply explicit type args (pre-fetch their bindings too)
      state2                 <- key.specifiedTypeArguments.foldLeftM(state1) { (s, ta) =>
                                  checker.prefetchBindings(s, ta.value)
                                }
      (appliedSig, state3)    = applyTypeArgs(checker, state2, signature, key.specifiedTypeArguments)
      (instantiated, state4)  = instantiateRemaining(checker, state3, appliedSig)

      // Step 5: Check runtime body if present
      (runtime, state5) <- resolvedValue.runtime match {
                              case Some(body) =>
                                checker.check(state4, body, instantiated).map { case (expr, s) =>
                                  (Some(body.as(expr.expression)), s)
                                }
                              case None       =>
                                (None: Option[Sourced[Monomorphic3Expression.Expression]], state4).pure[CompilerIO]
                            }

      // Step 6: Drain unifier and produce output
      state6 = state5.withUnifier(state5.unifier.drain())

      // Report any unification errors (each error carries its own source position)
      _        <- state6.unifier.errors.reverse.foldLeft(().pure[CompilerIO]) { (acc, sourcedMsg) =>
                    acc >> compilerError(sourcedMsg)
                  }

      // Quote the signature to GroundValue
      groundSig = checker.forceAndConst(state6, instantiated)
    } yield Monomorphic3Value(
      key.vfqn,
      key.specifiedTypeArguments,
      groundSig,
      runtime
    )
  }

  private def prefetchAllBindings(
      checker: Checker,
      state: CheckState,
      resolvedValue: OperatorResolvedValue
  ): CompilerIO[CheckState] = {
    val levels = resolvedValue.typeStack.value.levels.toSeq
    for {
      s1 <- levels.foldLeftM(state) { (s, level) => checker.prefetchBindings(s, level) }
      s2 <- resolvedValue.runtime match {
              case Some(body) => checker.prefetchBindings(s1, body.value)
              case None       => s1.pure[CompilerIO]
            }
      s3 <- resolvedValue.paramConstraints.values.toSeq.flatten.foldLeftM(s2) { (s, constraint) =>
              constraint.typeArgs.foldLeftM(s) { (s4, arg) => checker.prefetchBindings(s4, arg) }
            }
    } yield s3
  }

  private def walkTypeStack(
      checker: Checker,
      state: CheckState,
      resolvedValue: OperatorResolvedValue
  ): CompilerIO[(SemValue, CheckState)] = {
    val levels = resolvedValue.typeStack.value.levels.toSeq.reverse
    levels.foldLeftM((VType.asInstanceOf[SemValue], state)) { case ((expected, s), level) =>
      for {
        (_, s1) <- checker.check(s, resolvedValue.typeStack.as(level), expected)
        evaluated = checker.evalExpr(s1, level)
      } yield (evaluated, s1)
    }
  }

  /** Instantiate any remaining VLam closures (unapplied type parameters) with fresh metas. This handles phantom type
    * parameters and cases where fewer explicit type args were provided than type parameters exist.
    */
  private def instantiateRemaining(checker: Checker, state: CheckState, sig: SemValue): (SemValue, CheckState) = {
    val forced = Evaluator.force(sig, state.unifier.metaStore)
    forced match {
      case VLam(name, closure) =>
        val (metaId, freshStore) = state.unifier.metaStore.fresh
        val freshMeta            = VMeta(metaId, Spine.SNil, VType)
        val newState             = state.withUnifier(state.unifier.copy(metaStore = freshStore)).bind(name, freshMeta)
        instantiateRemaining(checker, newState, closure(freshMeta))
      case other               => (other, state)
    }
  }

  private def applyTypeArgs(
      checker: Checker,
      state: CheckState,
      signature: SemValue,
      typeArgs: Seq[Sourced[OperatorResolvedExpression]]
  ): (SemValue, CheckState) =
    typeArgs.foldLeft((signature, state)) { case ((sig, s), typeArg) =>
      val argVal = checker.evalExpr(s, typeArg.value)
      val forced = Evaluator.force(sig, s.unifier.metaStore)
      forced match {
        case VLam(name, closure) =>
          (closure(argVal), s.bind(name, argVal))
        case _                   =>
          val newUnifier = s.unifier.copy(errors = typeArg.as("Too many type arguments.") :: s.unifier.errors)
          (sig, s.withUnifier(newUnifier))
      }
    }
}
