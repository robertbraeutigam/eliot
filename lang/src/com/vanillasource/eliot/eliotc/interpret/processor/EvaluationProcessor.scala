package com.vanillasource.eliot.eliotc.interpret.processor

import cats.kernel.Order.catsKernelOrderingForOrder
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ability.util.ImplementationMarkerUtils
import com.vanillasource.eliot.eliotc.core.fact.RoleHint
import com.vanillasource.eliot.eliotc.interpret.fact.EvaluatedValue
import com.vanillasource.eliot.eliotc.interpret.processor.EvaluationProcessor.*
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.module.fact.{
  ModuleName,
  QualifiedName,
  Qualifier,
  UnifiedModuleNames,
  UnifiedModuleValue,
  ValueFQN
}
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
  *
  * `match` is reduced here exactly as on the JVM backend: dispatch was already resolved upstream, so a `match` appears
  * as a call to a concrete `PatternMatch.handleCases` / `TypeMatch.typeMatch` ability implementation. Those impls are
  * abstract (the JVM backend generates their bodies), so this backend supplies them as its own natives — see
  * [[evalHandleCases]] and [[evalTypeMatch]].
  */
class EvaluationProcessor extends SingleFactProcessor[EvaluatedValue.Key] {

  override protected def generateSingleFact(key: EvaluatedValue.Key): CompilerIO[EvaluatedValue] =
    for {
      result <- evalValue(key.vfqn, key.typeArguments, key.arguments.map(EvalValue.Ground.apply))
      ground <- requireGround(result)
    } yield EvaluatedValue(key.vfqn, key.typeArguments, key.arguments, ground)

  /** Run a value (by FQN + monomorphic type args) applied to argument values. */
  private def evalValue(vfqn: ValueFQN, typeArguments: Seq[GroundValue], args: Seq[EvalValue]): CompilerIO[EvalValue] =
    if (isHandleCasesImpl(vfqn)) evalHandleCases(args)
    else if (isTypeMatchImpl(vfqn)) evalTypeMatch(vfqn, args)
    else
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
        eval(env, other).flatMap(fn => applyValue(fn, args))
    }

  /** Apply a runtime value to already-evaluated arguments. Generated `match` code is fully saturated (uncurrying
    * flattens both lambdas and applications), so a closure is always applied to exactly its parameter count.
    */
  private def applyValue(fn: EvalValue, args: Seq[EvalValue]): CompilerIO[EvalValue] =
    fn match {
      case EvalValue.Closure(parameters, body, capturedEnv) if parameters.length == args.length =>
        eval(capturedEnv ++ parameters.zip(args), body)
      case EvalValue.Native(run)                                                                 =>
        run(args)
      case _                                                                                     => abort
    }

  /** `PatternMatch.handleCases(value, cases)` native: `value = Structure(ctorᵢ, fields)`. Build the Church selector that
    * picks `ctorᵢ`'s declaration-order index out of its siblings, apply `cases` to it to obtain `ctorᵢ`'s handler, then
    * apply that handler to the constructor's fields (or to a unit argument for a field-less constructor, mirroring the
    * JVM backend's `null`).
    */
  private def evalHandleCases(args: Seq[EvalValue]): CompilerIO[EvalValue] =
    args match {
      case Seq(EvalValue.Ground(GroundValue.Structure(ctorFqn, fields, _)), cases) =>
        for {
          index    <- constructorIndex(ctorFqn)
          selector  = EvalValue.Native(handlers => handlers(index).pure[CompilerIO])
          handler  <- applyValue(cases, Seq(selector))
          result   <- applyHandlerToFields(handler, fields)
        } yield result
      case _                                                                       => abort
    }

  /** `TypeMatch.typeMatch(obj, matched, notMatched)` native: the running impl's FQN encodes the target type constructor
    * `C`. If `obj`'s head constructor is `C`, apply `matched` to `obj`'s fields; otherwise apply `notMatched` to a unit
    * argument.
    */
  private def evalTypeMatch(vfqn: ValueFQN, args: Seq[EvalValue]): CompilerIO[EvalValue] =
    args match {
      case Seq(EvalValue.Ground(GroundValue.Structure(headFqn, fields, _)), matched, notMatched) =>
        ImplementationMarkerUtils.firstPatternTypeConstructorName(vfqn, "TypeMatch").flatMap {
          case Some(targetName) if headFqn.name.name === targetName => applyHandlerToFields(matched, fields)
          case _                                                    => applyValue(notMatched, Seq(unitArgument))
        }
      case _                                                                                      => abort
    }

  /** Apply a pattern handler to a constructor's fields. A field-less constructor's handler still takes one (ignored)
    * argument — the desugarer always emits a wildcard lambda — so it is applied to a unit placeholder.
    */
  private def applyHandlerToFields(handler: EvalValue, fields: Seq[GroundValue]): CompilerIO[EvalValue] =
    if (fields.isEmpty) applyValue(handler, Seq(unitArgument))
    else applyValue(handler, fields.map(EvalValue.Ground.apply))

  /** The declaration-order index of a value constructor among the constructors of its data type. */
  private def constructorIndex(ctorFqn: ValueFQN): CompilerIO[Int] =
    for {
      umv      <- getFactOrAbort(UnifiedModuleValue.Key(ctorFqn))
      dataType <- umv.namedValue.roleHint match {
                    case RoleHint.ValueConstructor(dt, _) => dt.pure[CompilerIO]
                    case _                                => abort
                  }
      ordered  <- orderedConstructors(ctorFqn.moduleName, dataType)
      index     = ordered.indexOf(ctorFqn)
      result   <- if (index >= 0) index.pure[CompilerIO] else abort
    } yield result

  /** The value constructors of a data type, in source-declaration order (= handler order). */
  private def orderedConstructors(moduleName: ModuleName, dataType: QualifiedName): CompilerIO[Seq[ValueFQN]] =
    for {
      moduleNames     <- getFactOrAbort(UnifiedModuleNames.Key(moduleName))
      constructorVfqns = moduleNames.names.keys
                           .filter(qn => qn.qualifier == Qualifier.Default && qn.name.head.isUpper)
                           .map(qn => ValueFQN(moduleName, qn))
                           .toSeq
      ordered         <- constructorVfqns.traverseFilter { vfqn =>
                           getFactOrAbort(UnifiedModuleValue.Key(vfqn)).map { umv =>
                             umv.namedValue.roleHint match {
                               case RoleHint.ValueConstructor(dt, _) if dt == dataType =>
                                 Some((vfqn, umv.namedValue.qualifiedName.range.from))
                               case _                                                  => None
                             }
                           }
                         }
    } yield ordered.sortBy(_._2).map(_._1)

  private def asGround(v: EvalValue): CompilerIO[GroundValue] = v match {
    case EvalValue.Ground(g)  => g.pure[CompilerIO]
    case _: EvalValue.Closure => abort
    case _: EvalValue.Native  => abort
  }

  private def requireGround(v: EvalValue): CompilerIO[GroundValue] = asGround(v)
}

object EvaluationProcessor {
  private type Env = Map[String, EvalValue]

  /** Runtime values of the interpreter: a fully-evaluated ground value, a closure (needed for the Church-encoded
    * handlers that `match` desugars to), or a native function (e.g. the constructor selector built by `handleCases`).
    */
  private enum EvalValue {
    case Ground(value: GroundValue)
    case Closure(parameters: Seq[String], body: UncurriedMonomorphicExpression.Expression, env: Map[String, EvalValue])
    case Native(run: Seq[EvalValue] => CompilerIO[EvalValue])
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

  /** Placeholder argument supplied to field-less constructor handlers. The handler's single parameter is a wildcard
    * that never inspects it (this mirrors the JVM backend passing `null`).
    */
  private val unitArgument: EvalValue = EvalValue.Ground(GroundValue.Direct((), GroundValue.Type))

  private val incFQN: ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "BigInteger"), QualifiedName("inc", Qualifier.Default))

  private def isHandleCasesImpl(vfqn: ValueFQN): Boolean =
    vfqn.name.name == "handleCases" && isAbilityImpl(vfqn, "PatternMatch")

  private def isTypeMatchImpl(vfqn: ValueFQN): Boolean =
    vfqn.name.name == "typeMatch" && isAbilityImpl(vfqn, "TypeMatch")

  private def isAbilityImpl(vfqn: ValueFQN, abilityName: String): Boolean =
    vfqn.name.qualifier match {
      case Qualifier.AbilityImplementation(name, _) => name.value == abilityName
      case _                                        => false
    }

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
