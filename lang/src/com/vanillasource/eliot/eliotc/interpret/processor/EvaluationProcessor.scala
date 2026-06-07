package com.vanillasource.eliot.eliotc.interpret.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.interpret.fact.EvaluatedValue
import com.vanillasource.eliot.eliotc.interpret.processor.EvaluationProcessor.*
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor
import com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedMonomorphicExpression
import com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedMonomorphicExpression.*
import com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedMonomorphicValue

/** Compiler-internal evaluation backend: runs a pure, fully-applied monomorphic value by interpreting its uncurried IR,
  * producing a [[GroundValue]].
  *
  * This is the initial implementation behind the [[EvaluatedValue]] fact boundary (see the fact's documentation). It
  * supports only pure code; impure / unsupported references abort. Nested ground value calls are reduced by recursively
  * requesting [[EvaluatedValue]] facts (caching + the compile-then-run fixpoint); natives and closures are interpreted
  * inline (closures cannot cross the ground-only fact boundary).
  */
class EvaluationProcessor extends SingleFactProcessor[EvaluatedValue.Key] {

  override protected def generateSingleFact(key: EvaluatedValue.Key): CompilerIO[EvaluatedValue] =
    for {
      result <- evalValue(key.vfqn, key.typeArguments, key.arguments.map(EvalValue.Ground.apply))
      ground <- requireGround(result)
    } yield EvaluatedValue(key.vfqn, key.typeArguments, key.arguments, ground)

  /** Run a value (by FQN + monomorphic type args) applied to argument values. */
  private def evalValue(vfqn: ValueFQN, typeArguments: Seq[GroundValue], args: Seq[EvalValue]): CompilerIO[EvalValue] =
    natives.get(vfqn) match {
      case Some(native) => native(args)
      case None         =>
        getFactOrAbort(UncurriedMonomorphicValue.Key(vfqn, typeArguments, args.length)).flatMap { umv =>
          umv.body match {
            case Some(body) =>
              val env = umv.parameters.map(_.name.value).zip(args).toMap
              eval(env, body.value)
            case None       =>
              // Body-less and not a known native: treat as a data constructor application.
              args
                .traverse(asGround)
                .map(groundArgs => EvalValue.Ground(GroundValue.Structure(vfqn, groundArgs, umv.returnType)))
          }
        }
    }

  /** Evaluate an uncurried expression under an environment of parameter bindings. */
  private def eval(env: Env, expression: UncurriedMonomorphicExpression.Expression): CompilerIO[EvalValue] =
    expression match {
      case IntegerLiteral(value) =>
        EvalValue.Ground(GroundValue.Direct(value.value, bigIntegerType)).pure[CompilerIO]

      case StringLiteral(value) =>
        EvalValue.Ground(GroundValue.Direct(value.value, stringType)).pure[CompilerIO]

      case ParameterReference(name) =>
        env.get(name.value) match {
          case Some(v) => v.pure[CompilerIO]
          case None    => abort
        }

      case MonomorphicValueReference(valueName, typeArguments) =>
        evalValue(valueName.value, typeArguments, Seq.empty)

      case FunctionLiteral(parameters, body) =>
        EvalValue.Closure(parameters.map(_.name.value), body.value.expression, env).pure[CompilerIO]

      case FunctionApplication(target, arguments) =>
        for {
          argValues <- arguments.toList.traverse(arg => eval(env, arg.value.expression))
          result    <- applyTarget(env, target.value.expression, argValues)
        } yield result
    }

  /** Apply an evaluated target to already-evaluated arguments. */
  private def applyTarget(
      env: Env,
      target: UncurriedMonomorphicExpression.Expression,
      args: Seq[EvalValue]
  ): CompilerIO[EvalValue] =
    target match {
      case MonomorphicValueReference(valueName, typeArguments) =>
        evalValue(valueName.value, typeArguments, args)

      case FunctionLiteral(parameters, body) =>
        eval(env ++ parameters.map(_.name.value).zip(args), body.value.expression)

      case other =>
        eval(env, other).flatMap {
          case EvalValue.Closure(params, body, capturedEnv) => eval(capturedEnv ++ params.zip(args), body)
          case _                                            => abort
        }
    }

  private def asGround(v: EvalValue): CompilerIO[GroundValue] = v match {
    case EvalValue.Ground(g) => g.pure[CompilerIO]
    case _: EvalValue.Closure => abort
  }

  private def requireGround(v: EvalValue): CompilerIO[GroundValue] = asGround(v)
}

object EvaluationProcessor {
  private type Env = Map[String, EvalValue]

  /** Runtime values of the interpreter: either a fully-evaluated ground value, or a closure (needed for the
    * Church-encoded handlers that `match` desugars to).
    */
  private enum EvalValue {
    case Ground(value: GroundValue)
    case Closure(parameters: Seq[String], body: UncurriedMonomorphicExpression.Expression, env: Map[String, EvalValue])
  }

  private val bigIntegerType: GroundValue =
    GroundValue.Structure(
      ValueFQN(ModuleName(defaultSystemPackage, "BigInteger"), QualifiedName("BigInteger", Qualifier.Type)),
      Seq.empty,
      GroundValue.Type
    )

  private val stringType: GroundValue =
    GroundValue.Structure(
      ValueFQN(ModuleName(defaultSystemPackage, "String"), QualifiedName("String", Qualifier.Type)),
      Seq.empty,
      GroundValue.Type
    )

  private val incFQN: ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "BigInteger"), QualifiedName("inc", Qualifier.Default))

  /** Pure built-in implementations (this backend's equivalent of the JVM backend's NativeImplementation map). */
  private val natives: Map[ValueFQN, Seq[EvalValue] => CompilerIO[EvalValue]] =
    Map(
      incFQN -> {
        case Seq(EvalValue.Ground(GroundValue.Direct(n: BigInt, tpe))) =>
          EvalValue.Ground(GroundValue.Direct(n + 1, tpe)).pure[CompilerIO]
        case _                                                         => abort
      }
    )
}
