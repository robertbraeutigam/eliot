package com.vanillasource.eliot.eliotc.eval2.util

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.Types.{bigIntType, stringType, typeFQN}
import com.vanillasource.eliot.eliotc.eval2.fact.Sem
import com.vanillasource.eliot.eliotc.eval2.fact.Sem.*
import com.vanillasource.eliot.eliotc.eval2.util.MetaState.{EvalIO, liftCompilerIO}
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** NbE evaluator: converts OperatorResolvedExpression into the semantic domain (Sem). Always reduces eagerly, only
  * stopping at neutral terms (free parameters, blocked top-level definitions).
  */
object Evaluator2 {

  /** Evaluate an ORE in the given environment to a semantic value. */
  def eval(env: Env, expr: OperatorResolvedExpression): EvalIO[Sem] = expr match {
    case OperatorResolvedExpression.IntegerLiteral(v) =>
      Sem.Lit(com.vanillasource.eliot.eliotc.eval.fact.Value.Direct(v.value, bigIntType)).pure[EvalIO]

    case OperatorResolvedExpression.StringLiteral(v) =>
      Sem.Lit(com.vanillasource.eliot.eliotc.eval.fact.Value.Direct(v.value, stringType)).pure[EvalIO]

    case OperatorResolvedExpression.ParameterReference(name) =>
      env.params.getOrElse(name.value, Sem.Neut(Head.Param(name.value), Seq.empty)).pure[EvalIO]

    case OperatorResolvedExpression.ValueReference(vfqnSourced, typeArgs) =>
      evalValueReference(env, vfqnSourced, typeArgs)

    case OperatorResolvedExpression.FunctionApplication(target, arg) =>
      for {
        targetSem <- eval(env, target.value)
        argSem    <- eval(env, arg.value)
        result    <- apply(targetSem, argSem)
      } yield result

    case OperatorResolvedExpression.FunctionLiteral(paramName, paramTypeOpt, body) =>
      for {
        domSem <- paramTypeOpt match {
                    case Some(pt) => eval(env, pt.value.signature)
                    case None     =>
                      liftCompilerIO(
                        Sourced.compilerAbort(
                          paramName.as("Cannot infer type of unannotated lambda without type context.")
                        )
                      )
                  }
      } yield Sem.Lam(paramName.value, domSem, Closure(env, body))
  }

  /** Apply a semantic function to an argument. For lambdas this triggers beta-reduction by re-evaluating the body in
    * the extended environment. For neutral terms, the argument is appended to the spine.
    */
  def apply(f: Sem, arg: Sem): EvalIO[Sem] = f match {
    case Sem.Lam(name, _, Closure(closureEnv, body)) =>
      eval(closureEnv.extend(name, arg), body.value)

    case Sem.Neut(head, spine) =>
      Sem.Neut(head, spine :+ arg).pure[EvalIO]

    case _ =>
      Sem.Neut(Head.Param("_apply_error"), Seq.empty).pure[EvalIO]
  }

  private def evalValueReference(
      env: Env,
      vfqnSourced: Sourced[ValueFQN],
      typeArgs: Seq[Sourced[OperatorResolvedExpression]]
  ): EvalIO[Sem] = {
    val vfqn = vfqnSourced.value
    if (vfqn === typeFQN) {
      applyTypeArgs(env, Sem.TypeUniv, typeArgs)
    } else {
      for {
        inProg  <- MetaState.isInProgress(vfqn)
        baseSem <- if (inProg) Sem.Neut(Head.Ref(vfqn), Seq.empty).pure[EvalIO]
                   else evalTopLevel(vfqn)
        result  <- applyTypeArgs(env, baseSem, typeArgs)
      } yield result
    }
  }

  private def evalTopLevel(vfqn: ValueFQN): EvalIO[Sem] =
    for {
      resolved <- liftCompilerIO(getFactOrAbort[OperatorResolvedValue, OperatorResolvedValue.Key](
                    OperatorResolvedValue.Key(vfqn)
                  ))
      result   <- resolved.runtime match {
                    case Some(body) =>
                      MetaState.withInProgress(vfqn)(eval(Env.empty, body.value))
                    case None       =>
                      Sem.Neut(Head.Ref(vfqn), Seq.empty).pure[EvalIO]
                  }
    } yield result

  private def applyTypeArgs(
      env: Env,
      base: Sem,
      typeArgs: Seq[Sourced[OperatorResolvedExpression]]
  ): EvalIO[Sem] =
    typeArgs.foldLeftM(base) { (acc, ta) =>
      eval(env, ta.value).flatMap(apply(acc, _))
    }
}
