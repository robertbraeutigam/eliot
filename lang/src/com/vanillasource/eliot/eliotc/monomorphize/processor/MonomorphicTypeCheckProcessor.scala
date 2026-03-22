package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{Qualifier as CoreQualifier}
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, NamedEvaluable, Types, Value}
import com.vanillasource.eliot.eliotc.eval.util.Evaluator
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.implementation.fact.AbilityImplementation
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.*
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.{compilerAbort, compilerError}
import com.vanillasource.eliot.eliotc.symbolic.fact.SymbolicType

/** Processor that monomorphizes (specializes) generic functions.
  *
  * Given a MonomorphicValue.Key(vfqn, typeArgs), it:
  *   1. Fetches the OperatorResolvedValue for vfqn
  *   2. Evaluates the type signature with concrete type args using the eval package
  *   3. Walks the runtime expression body, computing concrete types and resolving abilities
  *   4. Produces a MonomorphicValue with fully concrete types
  */
class MonomorphicTypeCheckProcessor extends SingleKeyTypeProcessor[MonomorphicValue.Key] with Logging {

  override protected def generateFact(key: MonomorphicValue.Key): CompilerIO[Unit] =
    for {
      resolvedValue <- getFactOrAbort(OperatorResolvedValue.Key(key.vfqn))
      _             <- debug[CompilerIO](
                         s"Monomorphizing ${key.vfqn.show}, type arguments: ${key.typeArguments.map(_.show).mkString(", ")}"
                       )
      // Evaluate the type stack signature expression to get the type as ExpressionValue
      typeExprValue <- Evaluator.evaluate(
                         resolvedValue.typeStack.as(resolvedValue.typeStack.value.signature)
                       )
      allTypeParams  = ExpressionValue.extractLeadingLambdaParams(typeExprValue)
      bodyExprValue  = ExpressionValue.stripLeadingLambdas(typeExprValue)
      // Strip constraint-only type params (those not appearing in the body type)
      typeParams     = allTypeParams.filter((name, _) => ExpressionValue.containsVar(bodyExprValue, name))
      _             <- if (typeParams.length != key.typeArguments.length)
                         compilerAbort(
                           resolvedValue.name.as(
                             s"Type argument count mismatch: expected ${typeParams.length}, got ${key.typeArguments.length}"
                           )
                         )
                       else ().pure[CompilerIO]
      typeParamSubst = typeParams.map(_._1).zip(key.typeArguments).toMap
      signature     <- applyTypeArgsStripped(typeExprValue, allTypeParams, typeParamSubst, resolvedValue.name)
      _             <- debug[CompilerIO](s"Monomorphized ${key.vfqn.show} to: ${signature.show}")
      runtime       <- resolvedValue.runtime.traverse { body =>
                         transformExpression(body.value, signature, typeParamSubst, Map.empty, body)
                           .map(body.as)
                       }
      _             <- runtime match {
                         case Some(body) => checkReturnType(body.value.expression, signature, body)
                         case None       => ().pure[CompilerIO]
                       }
      _             <- registerFactIfClear(
                         MonomorphicValue(
                           key.vfqn,
                           key.typeArguments,
                           resolvedValue.name,
                           signature,
                           runtime.map(_.map(_.expression))
                         )
                       )
    } yield ()

  /** Strip constraint-only FunctionLiterals from a type ExpressionValue and apply only the relevant type args. */
  private def applyTypeArgsStripped(
      typeExprValue: ExpressionValue,
      allTypeParams: Seq[(String, Value)],
      typeParamSubst: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[Value] = {
    // Strip leading FunctionLiterals for constraint-only params (those not in typeParamSubst)
    val stripped = stripConstraintOnlyLambdas(typeExprValue, typeParamSubst.keySet)
    val relevantArgs = allTypeParams.flatMap { (name, _) => typeParamSubst.get(name) }
    applyTypeArgs(stripped, relevantArgs, source)
  }

  /** Strip leading FunctionLiterals whose parameter names are NOT in the set of relevant params. */
  private def stripConstraintOnlyLambdas(ev: ExpressionValue, relevantParams: Set[String]): ExpressionValue =
    ev match {
      case FunctionLiteral(name, _, body) if !relevantParams.contains(name) =>
        stripConstraintOnlyLambdas(body.value, relevantParams)
      case _ => ev
    }

  /** Apply concrete type arguments to a type ExpressionValue. For non-generic types, returns the concrete Value
    * directly. For generic types (FunctionLiterals representing type lambdas), builds application chain and reduces.
    */
  private def applyTypeArgs(
      typeExprValue: ExpressionValue,
      typeArgs: Seq[Value],
      source: Sourced[?]
  ): CompilerIO[Value] =
    if (typeArgs.isEmpty) {
      typeExprValue match {
        case ConcreteValue(v) => v.pure[CompilerIO]
        case _                =>
          compilerAbort(source.as("Non-generic type signature did not evaluate to concrete value."))
      }
    } else {
      val applied = typeArgs.foldLeft[ExpressionValue](typeExprValue) { (fn, arg) =>
        FunctionApplication(
          ExpressionValue.unsourced(fn),
          ExpressionValue.unsourced(ConcreteValue(arg))
        )
      }
      Evaluator.reduce(applied, source).flatMap {
        case ConcreteValue(v) => v.pure[CompilerIO]
        case other            =>
          compilerAbort(
            source.as("Type signature did not evaluate to concrete value after applying type arguments."),
            Seq(s"Result: ${other.show}")
          )
      }
    }

  /** Transform an OperatorResolvedExpression to a MonomorphicExpression, computing concrete types and recursively
    * monomorphizing called functions.
    *
    * @param callSiteType
    *   The expected type of this expression from the enclosing context.
    */
  private def transformExpression(
      expr: OperatorResolvedExpression,
      callSiteType: Value,
      typeParamSubst: Map[String, Value],
      runtimeParams: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[MonomorphicExpression] =
    expr match {
      case OperatorResolvedExpression.IntegerLiteral(v)      =>
        MonomorphicExpression(Types.bigIntType, MonomorphicExpression.IntegerLiteral(v)).pure[CompilerIO]
      case OperatorResolvedExpression.StringLiteral(v)       =>
        MonomorphicExpression(Types.stringType, MonomorphicExpression.StringLiteral(v)).pure[CompilerIO]
      case OperatorResolvedExpression.ParameterReference(n)  =>
        runtimeParams.get(n.value) match {
          case Some(paramType) =>
            MonomorphicExpression(paramType, MonomorphicExpression.ParameterReference(n)).pure[CompilerIO]
          case None            =>
            compilerAbort(n.as(s"Unknown parameter: ${n.value}"))
        }
      case vr: OperatorResolvedExpression.ValueReference     =>
        transformValueReference(vr, callSiteType, typeParamSubst, runtimeParams, source)
      case fa: OperatorResolvedExpression.FunctionApplication =>
        transformFunctionApplication(fa, typeParamSubst, runtimeParams, source)
      case fl: OperatorResolvedExpression.FunctionLiteral    =>
        transformFunctionLiteral(fl, callSiteType, typeParamSubst, runtimeParams, source)
    }

  /** Evaluate a value's type signature from its OperatorResolvedValue's type stack. */
  private def evaluateValueType(vfqn: ValueFQN, source: Sourced[?]): CompilerIO[ExpressionValue] =
    for {
      resolvedValue    <- getFactOrAbort(OperatorResolvedValue.Key(vfqn))
      typeExprValue    <- Evaluator.evaluate(
                            resolvedValue.typeStack.as(resolvedValue.typeStack.value.signature)
                          )
    } yield typeExprValue

  /** Transform a value reference by determining concrete type arguments for the referenced value. */
  private def transformValueReference(
      vr: OperatorResolvedExpression.ValueReference,
      callSiteType: Value,
      typeParamSubst: Map[String, Value],
      runtimeParams: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[MonomorphicExpression] =
    for {
      refTypeExprValue <- evaluateValueType(vr.valueName.value, source)
      typeParams        = ExpressionValue.extractLeadingLambdaParams(refTypeExprValue)
      typeArgs         <- if (typeParams.nonEmpty) {
                            inferTypeArguments(refTypeExprValue, typeParams, callSiteType, typeParamSubst, source)
                          } else {
                            Seq.empty[Value].pure[CompilerIO]
                          }
      concreteType     <- applyTypeArgs(refTypeExprValue, typeArgs, source)
      result           <- if (isAbilityRef(vr.valueName.value) && typeArgs.nonEmpty) {
                            resolveAbilityCall(vr.valueName, typeArgs, concreteType, source)
                          } else {
                            MonomorphicExpression(
                              concreteType,
                              MonomorphicExpression.MonomorphicValueReference(vr.valueName, typeArgs)
                            ).pure[CompilerIO]
                          }
    } yield result

  /** Resolve an ability method call to its concrete implementation. */
  private def resolveAbilityCall(
      vfqn: Sourced[ValueFQN],
      typeArgs: Seq[Value],
      concreteType: Value,
      source: Sourced[?]
  ): CompilerIO[MonomorphicExpression] =
    for {
      abilityTypeParamCount <- countAbilityTypeParams(vfqn.value)
      abilityTypeArgs        = typeArgs.take(abilityTypeParamCount)
      // TODO: change AbilityImplementation.Key to Seq[Value] to remove this bridge
      abilitySymbolicArgs    = abilityTypeArgs.map(SymbolicType.fromValue)
      impl                  <- getFactOrAbort(AbilityImplementation.Key(vfqn.value, abilitySymbolicArgs))
    } yield MonomorphicExpression(
      concreteType,
      MonomorphicExpression.MonomorphicValueReference(vfqn.as(impl.implementationFQN), Seq.empty)
    )

  private def isAbilityRef(vfqn: ValueFQN): Boolean =
    vfqn.name.qualifier.isInstanceOf[CoreQualifier.Ability]

  private def countAbilityTypeParams(vfqn: ValueFQN): CompilerIO[Int] = {
    val abilityName = vfqn.name.qualifier.asInstanceOf[CoreQualifier.Ability].name
    val markerVFQN  =
      ValueFQN(
        vfqn.moduleName,
        com.vanillasource.eliot.eliotc.core.fact.QualifiedName(abilityName, CoreQualifier.Ability(abilityName))
      )
    evaluateValueType(markerVFQN, ExpressionValue.unsourced(()))
      .map(ev => ExpressionValue.extractLeadingLambdaParams(ev).size)
  }

  /** Infer concrete type arguments for a referenced value by matching the call-site type against the polymorphic
    * signature.
    */
  private def inferTypeArguments(
      evalValue: ExpressionValue,
      typeParams: Seq[(String, Value)],
      callSiteType: Value,
      typeParamSubst: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[Seq[Value]] = {
    val body     = ExpressionValue.stripLeadingLambdas(evalValue)
    val bindings = ExpressionValue.matchTypes(body, ConcreteValue(callSiteType))

    typeParams.traverse { (param, _) =>
      bindings.get(param) match {
        case Some(ConcreteValue(v)) => v.pure[CompilerIO]
        case Some(_)                =>
          typeParamSubst.get(param) match {
            case Some(v) => v.pure[CompilerIO]
            case None    =>
              compilerAbort(source.as(s"Cannot infer type argument for parameter: $param (non-concrete match)"))
          }
        case None                   =>
          typeParamSubst.get(param) match {
            case Some(v) => v.pure[CompilerIO]
            case None    =>
              compilerAbort(source.as(s"Cannot infer type argument for parameter: $param"))
          }
      }
    }
  }

  /** Transform a function application. For value reference targets, type arguments are inferred from argument types. */
  private def transformFunctionApplication(
      fa: OperatorResolvedExpression.FunctionApplication,
      typeParamSubst: Map[String, Value],
      runtimeParams: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[MonomorphicExpression] =
    fa.target.value match {
      case vr: OperatorResolvedExpression.ValueReference =>
        // For value reference targets, we infer type args from the argument
        transformApplicationWithValueTarget(vr, fa, typeParamSubst, runtimeParams, source)
      case _                                             =>
        // For other targets (parameter refs, nested applications), transform normally
        for {
          transformedTarget       <- transformExpression(
                                       fa.target.value,
                                       Value.Type,
                                       typeParamSubst,
                                       runtimeParams,
                                       fa.target
                                     )
          (paramType, returnType) <- extractFunctionParamAndReturn(transformedTarget.expressionType, fa.target)
          transformedArg          <- transformExpression(
                                       fa.argument.value,
                                       paramType,
                                       typeParamSubst,
                                       runtimeParams,
                                       fa.argument
                                     )
        } yield MonomorphicExpression(
          returnType,
          MonomorphicExpression.FunctionApplication(
            fa.target.as(transformedTarget),
            fa.argument.as(transformedArg)
          )
        )
    }

  /** Transform a function application whose target is a value reference. Infers type arguments from argument types. */
  private def transformApplicationWithValueTarget(
      vr: OperatorResolvedExpression.ValueReference,
      fa: OperatorResolvedExpression.FunctionApplication,
      typeParamSubst: Map[String, Value],
      runtimeParams: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[MonomorphicExpression] =
    for {
      refTypeExprValue <- evaluateValueType(vr.valueName.value, source)
      typeParams        = ExpressionValue.extractLeadingLambdaParams(refTypeExprValue)
      bodyType          = ExpressionValue.stripLeadingLambdas(refTypeExprValue)
      // For generic targets: transform arg first (for type inference), then compute concrete type
      // For non-generic targets: compute concrete type first (to know param type for lambdas), then transform arg
      typeArgs         <- if (typeParams.nonEmpty) {
                            // Transform argument with placeholder, infer type args from its type
                            for {
                              transformedArgForInference <- transformExpression(
                                                             fa.argument.value,
                                                             Value.Type,
                                                             typeParamSubst,
                                                             runtimeParams,
                                                             fa.argument
                                                           )
                              args                       <- inferTypeArgsFromArgument(
                                                              bodyType,
                                                              transformedArgForInference.expressionType,
                                                              typeParams,
                                                              typeParamSubst,
                                                              source
                                                            )
                            } yield args
                          } else {
                            Seq.empty[Value].pure[CompilerIO]
                          }
      concreteType     <- applyTypeArgs(refTypeExprValue, typeArgs, source)
      (paramType, returnType) <- extractFunctionParamAndReturn(concreteType, fa.target)
      // Now transform the argument with the proper expected type (needed for lambda inference)
      transformedArg   <- transformExpression(
                            fa.argument.value,
                            paramType,
                            typeParamSubst,
                            runtimeParams,
                            fa.argument
                          )
      result           <- if (isAbilityRef(vr.valueName.value) && typeArgs.nonEmpty) {
                            resolveAbilityCall(vr.valueName, typeArgs, concreteType, source).map { implExpr =>
                              MonomorphicExpression(
                                returnType,
                                MonomorphicExpression.FunctionApplication(
                                  fa.target.as(implExpr),
                                  fa.argument.as(transformedArg)
                                )
                              )
                            }
                          } else {
                            MonomorphicExpression(
                              returnType,
                              MonomorphicExpression.FunctionApplication(
                                fa.target.as(
                                  MonomorphicExpression(
                                    concreteType,
                                    MonomorphicExpression.MonomorphicValueReference(vr.valueName, typeArgs)
                                  )
                                ),
                                fa.argument.as(transformedArg)
                              )
                            ).pure[CompilerIO]
                          }
    } yield result

  /** Extract parameter type and return type from a function type ExpressionValue (possibly partially evaluated). Handles
    * both fully reduced types (ConcreteValue-based) and partially applied types (NativeFunction-based).
    */
  private def extractFunctionExprParamAndReturn(
      expr: ExpressionValue
  ): Option[(ExpressionValue, ExpressionValue)] =
    expr match {
      case ExpressionValue.FunctionType(p, r) => Some((p, r))
      // Handle partially applied Function type constructor (NativeFunction base)
      case FunctionApplication(target, returnType) =>
        target.value match {
          case FunctionApplication(_, paramType) => Some((paramType.value, returnType.value))
          case _                                 => None
        }
      case _                                       => None
    }

  /** Infer type arguments from argument type by matching the function's parameter type pattern. */
  private def inferTypeArgsFromArgument(
      bodyType: ExpressionValue,
      argType: Value,
      typeParams: Seq[(String, Value)],
      typeParamSubst: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[Seq[Value]] = {
    // Extract parameter type from the function body type expression
    val paramType = extractFunctionExprParamAndReturn(bodyType).map(_._1).getOrElse(bodyType)
    val bindings  = ExpressionValue.matchTypes(paramType, ConcreteValue(argType))

    typeParams.traverse { (param, _) =>
      bindings.get(param) match {
        case Some(ConcreteValue(v)) => v.pure[CompilerIO]
        case Some(_)                =>
          typeParamSubst.get(param).map(_.pure[CompilerIO]).getOrElse(
            compilerAbort(source.as(s"Cannot infer type argument for parameter: $param (non-concrete match)"))
          )
        case None                   =>
          typeParamSubst.get(param).map(_.pure[CompilerIO]).getOrElse(
            compilerAbort(source.as(s"Cannot infer type argument for parameter: $param"))
          )
      }
    }
  }

  /** Extract parameter type and return type from a function type Value. */
  private def extractFunctionParamAndReturn(functionType: Value, source: Sourced[?]): CompilerIO[(Value, Value)] =
    functionType match {
      case Value.Structure(fields, Value.Type) =>
        fields.get("$typeName") match {
          case Some(Value.Direct(vfqn: ValueFQN, _)) if vfqn === Types.functionDataTypeFQN =>
            (fields("A"), fields("B")).pure[CompilerIO]
          case _                                                                           =>
            compilerAbort(source.as("Expected function type."), Seq(s"Found: ${functionType.show}"))
        }
      case _                                   =>
        compilerAbort(source.as("Expected function type."), Seq(s"Found: ${functionType.show}"))
    }

  /** Transform a function literal. */
  private def transformFunctionLiteral(
      fl: OperatorResolvedExpression.FunctionLiteral,
      expectedType: Value,
      typeParamSubst: Map[String, Value],
      runtimeParams: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[MonomorphicExpression] =
    for {
      (paramType, returnType) <- extractFunctionParamAndReturn(expectedType, source)
      newRuntimeParams         = runtimeParams + (fl.parameterName.value -> paramType)
      transformedBody         <- transformExpression(
                                   fl.body.value,
                                   returnType,
                                   typeParamSubst,
                                   newRuntimeParams,
                                   fl.body
                                 )
    } yield MonomorphicExpression(
      expectedType,
      MonomorphicExpression.FunctionLiteral(
        fl.parameterName,
        paramType,
        fl.body.as(transformedBody)
      )
    )

  private def checkReturnType(
      bodyExpr: MonomorphicExpression.Expression,
      signature: Value,
      source: Sourced[?]
  ): CompilerIO[Unit] =
    extractMonomorphicReturnType(bodyExpr) match {
      case Some((bodyReturnType, bodySource, depth)) =>
        val signatureReturnType = extractSignatureReturnType(signature, depth)
        if (bodyReturnType != signatureReturnType)
          compilerAbort(
            bodySource.as("Return type mismatch."),
            Seq(
              s"Expected: ${signatureReturnType.show}",
              s"Actual:   ${bodyReturnType.show}"
            )
          )
        else ().pure[CompilerIO]
      case None                                      => ().pure[CompilerIO]
    }

  private def extractMonomorphicReturnType(
      expr: MonomorphicExpression.Expression
  ): Option[(Value, Sourced[?], Int)] =
    expr match {
      case MonomorphicExpression.FunctionLiteral(_, _, body) =>
        extractMonomorphicReturnType(body.value.expression) match {
          case Some((v, s, d)) => Some((v, s, d + 1))
          case None            => Some((body.value.expressionType, body, 1))
        }
      case _                                                => None
    }

  private def extractSignatureReturnType(value: Value, depth: Int): Value =
    if (depth <= 0) value
    else
      value match {
        case Value.Structure(fields, Value.Type) =>
          fields.get("$typeName") match {
            case Some(Value.Direct(vfqn: ValueFQN, _)) if vfqn === Types.functionDataTypeFQN =>
              extractSignatureReturnType(fields("B"), depth - 1)
            case _                                                                           => value
          }
        case _                                   => value
      }
}
