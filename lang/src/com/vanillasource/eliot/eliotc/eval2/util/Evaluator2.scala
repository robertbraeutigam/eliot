package com.vanillasource.eliot.eliotc.eval2.util

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.Qualifier as CoreQualifier
import com.vanillasource.eliot.eliotc.eval.fact.Types.{bigIntType, stringType, typeFQN}
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.eval2.fact.*
import com.vanillasource.eliot.eliotc.eval2.util.MetaState.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

/** NbE evaluator: converts OperatorResolvedExpression to Sem values in weak head normal form.
  *
  * Always reduces eagerly. Only stops at neutral terms (free parameters, unsolved metas, blocked top-level
  * definitions).
  */
object Evaluator2 extends Logging {

  /** Evaluate an ORE expression in the given environment. */
  def eval(env: Env, expr: OperatorResolvedExpression): EvalIO[Sem] = expr match {
    case OperatorResolvedExpression.IntegerLiteral(s) =>
      Sem.Lit(Value.Direct(s.value, bigIntType)).pure[EvalIO]

    case OperatorResolvedExpression.StringLiteral(s) =>
      Sem.Lit(Value.Direct(s.value, stringType)).pure[EvalIO]

    case OperatorResolvedExpression.ParameterReference(name) =>
      env.lookup(name.value) match {
        case Some(sem) => sem.pure[EvalIO]
        case None      => Sem.Neut(Head.Param(name.value)).pure[EvalIO]
      }

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
                    case Some(paramType) => eval(env, paramType.value.signature)
                    case None            => freshMeta("lambdaParam", Sem.TypeUniv)
                  }
      } yield Sem.Lam(paramName.value, domSem, Closure(env, body.value))
  }

  /** Apply a semantic function to an argument. */
  def apply(f: Sem, arg: Sem): EvalIO[Sem] =
    force(f).flatMap {
      case Sem.Lam(name, _, Closure(closureEnv, body)) =>
        eval(closureEnv.extend(name, arg), body)
      case Sem.Neut(head, spine)                       =>
        Sem.Neut(head, spine :+ arg).pure[EvalIO]
      case other                                       =>
        Sem.Neut(Head.Param("$error"), Seq(other, arg)).pure[EvalIO]
    }

  /** Evaluate a top-level value reference, including type argument application. */
  private def evalValueReference(
      env: Env,
      vfqnSourced: Sourced[ValueFQN],
      typeArgs: Seq[Sourced[OperatorResolvedExpression]]
  ): EvalIO[Sem] = {
    val vfqn = vfqnSourced.value
    if (vfqn === typeFQN) {
      applyTypeArgs(Sem.TypeUniv, env, typeArgs)
    } else {
      isInProgress(vfqn).flatMap { inProg =>
        if (inProg) {
          applyTypeArgs(Sem.Neut(Head.Ref(vfqn)), env, typeArgs)
        } else {
          evalTopLevel(vfqn, vfqnSourced).flatMap { baseSem =>
            applyTypeArgs(baseSem, env, typeArgs)
          }
        }
      }
    }
  }

  /** Look up and evaluate a top-level definition. */
  private def evalTopLevel(vfqn: ValueFQN, source: Sourced[ValueFQN]): EvalIO[Sem] =
    liftCompilerIO(getFact(OperatorResolvedValue.Key(vfqn))).flatMap {
      case Some(resolved) =>
        if (resolved.opaque) {
          Sem.Neut(Head.Ref(vfqn)).pure[EvalIO]
        } else {
          withInProgress(vfqn) {
            resolved.runtime match {
              case Some(body) => eval(Env.empty, body.value)
              case None       =>
                eval(Env.empty, resolved.typeStack.value.signature).flatMap { sigSem =>
                  buildTypeStructure(vfqn, sigSem)
                }
            }
          }
        }
      case None           =>
        vfqn.name.qualifier match {
          case _: CoreQualifier.Ability | _: CoreQualifier.AbilityImplementation =>
            Sem.TypeUniv.pure[EvalIO]
          case _                                                                 =>
            liftCompilerIO(
              compilerAbort(
                source.as("Could not evaluate expression."),
                Seq(s"Named value '${vfqn.show}' not found.")
              )
            )
        }
    }

  /** For values without a runtime body (e.g., opaque types, data declarations), build a Sem from the signature. If the
    * signature evaluates to a lambda chain, wrap it as a type constructor. If it's a concrete type, return a struct for
    * that type.
    */
  private def buildTypeStructure(vfqn: ValueFQN, sigSem: Sem): EvalIO[Sem] =
    sigSem match {
      case Sem.TypeUniv =>
        Sem
          .Struct(
            vfqn,
            Map(
              "$typeName" -> Sem.Lit(
                Value.Direct(vfqn, com.vanillasource.eliot.eliotc.eval.fact.Types.fullyQualifiedNameType)
              )
            )
          )
          .pure[EvalIO]
      case _            =>
        Sem.Neut(Head.Ref(vfqn)).pure[EvalIO]
    }

  /** Apply type arguments sequentially. */
  private def applyTypeArgs(
      base: Sem,
      env: Env,
      typeArgs: Seq[Sourced[OperatorResolvedExpression]]
  ): EvalIO[Sem] =
    typeArgs.foldLeftM(base) { (acc, ta) =>
      eval(env, ta.value).flatMap(apply(acc, _))
    }
}
