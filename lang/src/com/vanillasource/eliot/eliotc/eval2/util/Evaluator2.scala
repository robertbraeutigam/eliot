package com.vanillasource.eliot.eliotc.eval2.util

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.Types
import com.vanillasource.eliot.eliotc.eval2.fact.*
import com.vanillasource.eliot.eliotc.eval2.util.MetaState.EvalIO
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced

object Evaluator2 {

  /** Evaluate an OperatorResolvedExpression to its semantic normal form. */
  def eval(env: Env, expr: OperatorResolvedExpression): EvalIO[Sem] = expr match {
    case OperatorResolvedExpression.IntegerLiteral(s) =>
      Sem.Lit(com.vanillasource.eliot.eliotc.eval.fact.Value.Direct(s.value, Types.bigIntType)).pure[EvalIO]

    case OperatorResolvedExpression.StringLiteral(s) =>
      Sem.Lit(com.vanillasource.eliot.eliotc.eval.fact.Value.Direct(s.value, Types.stringType)).pure[EvalIO]

    case OperatorResolvedExpression.ParameterReference(s) =>
      env.params.get(s.value) match {
        case Some(sem) => sem.pure[EvalIO]
        case None      => Sem.Neut(Head.Param(s.value), Seq.empty).pure[EvalIO]
      }

    case OperatorResolvedExpression.ValueReference(s, typeArgs) =>
      evalValueReference(env, s, typeArgs)

    case OperatorResolvedExpression.FunctionApplication(target, arg) =>
      for {
        targetSem <- eval(env, target.value)
        argSem    <- eval(env, arg.value)
        result    <- apply(targetSem, argSem)
      } yield result

    case OperatorResolvedExpression.FunctionLiteral(paramName, paramTypeOpt, body) =>
      for {
        dom <- paramTypeOpt match {
                 case Some(pt) => eval(env, pt.value.signature)
                 case None     => (Sem.TypeUniv: Sem).pure[EvalIO]
               }
      } yield Sem.Lam(paramName.value, dom, Closure(env, body.value))
  }

  /** Apply a semantic function to an argument. Performs beta reduction for lambdas,
    * extends the spine for neutral terms.
    */
  def apply(f: Sem, arg: Sem): EvalIO[Sem] = f match {
    case Sem.Lam(name, _, Closure(closureEnv, body)) =>
      eval(closureEnv.extend(name, arg), body)
    case Sem.Neut(head, spine)                        =>
      Sem.Neut(head, spine :+ arg).pure[EvalIO]
    case _                                            =>
      Sem.Neut(Head.Param("$apply-error"), Seq(f, arg)).pure[EvalIO]
  }

  private def evalValueReference(
      env: Env,
      vfqnSourced: Sourced[ValueFQN],
      typeArgs: Seq[Sourced[OperatorResolvedExpression]]
  ): EvalIO[Sem] = {
    val vfqn = vfqnSourced.value
    if (vfqn === Types.typeFQN) {
      applyTypeArgs(env, Sem.TypeUniv, typeArgs)
    } else {
      for {
        inProg <- MetaState.isInProgress(vfqn)
        result <- if (inProg) {
                    applyTypeArgs(env, Sem.Neut(Head.Ref(vfqn), Seq.empty), typeArgs)
                  } else {
                    MetaState.withInProgress(vfqn) {
                      for {
                        resolved <- MetaState.lift(
                                      getFactOrAbort[OperatorResolvedValue, OperatorResolvedValue.Key](
                                        OperatorResolvedValue.Key(vfqn)
                                      )
                                    )
                        sem      <- resolved.runtime match {
                                      case Some(body) => eval(Env.empty, body.value)
                                      case None       => Sem.Neut(Head.Ref(vfqn), Seq.empty).pure[EvalIO]
                                    }
                      } yield sem
                    }.flatMap(sem => applyTypeArgs(env, sem, typeArgs))
                  }
      } yield result
    }
  }

  private def applyTypeArgs(
      env: Env,
      sem: Sem,
      typeArgs: Seq[Sourced[OperatorResolvedExpression]]
  ): EvalIO[Sem] =
    typeArgs.foldLeftM(sem) { (acc, ta) =>
      eval(env, ta.value).flatMap(apply(acc, _))
    }
}
