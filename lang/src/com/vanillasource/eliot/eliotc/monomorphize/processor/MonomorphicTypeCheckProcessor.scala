package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier as CoreQualifier}
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Types, Value}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.implementation.fact.AbilityImplementation
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort
import com.vanillasource.eliot.eliotc.symbolic.fact.TypedExpression
import com.vanillasource.eliot.eliotc.symbolic.types.SymbolicType
import com.vanillasource.eliot.eliotc.symbolic.types.SymbolicType.{toExpressionValue, fromExpressionValue}
import com.vanillasource.eliot.eliotc.abilitycheck.AbilityCheckedValue

/** Processor that monomorphizes (specializes) generic functions.
  *
  * Given a MonomorphicValue.Key(vfqn, typeArgs), it:
  *   1. Fetches the AbilityCheckedValue for vfqn 2. Builds substitution from universal type params to concrete args 3.
  *      Evaluates the signature type to a Value 4. Transforms the runtime body, replacing types and recursively
  *      monomorphizing called functions
  */
class MonomorphicTypeCheckProcessor extends SingleKeyTypeProcessor[MonomorphicValue.Key] with Logging {

  override protected def generateFact(key: MonomorphicValue.Key): CompilerIO[Unit] =
    for {
      typeChecked <- getFactOrAbort(AbilityCheckedValue.Key(key.vfqn))
      _           <-
        debug[CompilerIO](
          s"Monomorphizing ${key.vfqn.show}, signature: ${typeChecked.signature.show}, type arguments: ${key.typeArguments.map(_.show).mkString(", ")}"
        )
      typeParams   = TypeEvaluator.extractBodyTypeParams(toExpressionValue(typeChecked.signature))
      _           <- if (typeParams.length != key.typeArguments.length)
                       compilerAbort(
                         typeChecked.name.as(
                           s"Type argument count mismatch: expected ${typeParams.length}, got ${key.typeArguments.length}"
                         )
                       )
                     else ().pure[CompilerIO]
      substitution = typeParams.zip(key.typeArguments).toMap
      _           <-
        debug[CompilerIO](
          s"Monomorphic eval ${key.vfqn.show} with substitution: ${substitution.toSeq
              .map((param, paramType) => s"$param <- ${paramType.show}")
              .mkString(", ")}"
        )
      signature   <- TypeEvaluator.evaluate(
                       TypeEvaluator.stripNonBodyUniversals(toExpressionValue(typeChecked.signature)),
                       key.typeArguments,
                       typeChecked.name
                     )
      _           <- debug[CompilerIO](s"Monomorphized ${key.vfqn.show} to: ${signature.show}")
      runtime     <- typeChecked.runtime.traverse { body =>
                       transformExpression(body.value, toExpressionValue(typeChecked.signature), substitution, body).map(body.as)
                     }
      _           <- runtime match {
                       case Some(body) =>
                         checkReturnType(body.value, signature, body)
                       case None       => ().pure[CompilerIO]
                     }
      _           <- registerFactIfClear(
                       MonomorphicValue(
                         key.vfqn,
                         key.typeArguments,
                         typeChecked.name,
                         signature,
                         runtime
                       )
                     )
    } yield ()

  /** Transform a TypedExpression.Expression to MonomorphicExpression.Expression, evaluating all types with the given
    * substitution and recursively monomorphizing called functions.
    *
    * @param callSiteType
    *   The type of this expression at the call site (after unification). Used to infer type arguments for value
    *   references.
    */
  private def transformExpression(
      expr: TypedExpression.Expression,
      callSiteType: ExpressionValue,
      substitution: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[MonomorphicExpression.Expression] =
    TypedExpression.foldExpression[CompilerIO, MonomorphicExpression.Expression](
      value => MonomorphicExpression.IntegerLiteral(value).pure[CompilerIO],
      value => MonomorphicExpression.StringLiteral(value).pure[CompilerIO],
      name => MonomorphicExpression.ParameterReference(name).pure[CompilerIO],
      vfqn => transformValueReference(vfqn, callSiteType, substitution, source),
      (target, arg) => transformFunctionApplication(target, arg, substitution),
      (paramName, paramType, body) => transformFunctionLiteral(paramName, paramType, body, substitution)
    )(expr)

  /** Transform a value reference by determining concrete type arguments for the referenced value.
    *
    * @param callSiteType
    *   The type of this value reference at the call site (after unification). For a generic function like `id[A]: A ->
    *   A` called with Int, this would be `Int -> Int`.
    */
  private def transformValueReference(
      vfqn: Sourced[ValueFQN],
      callSiteType: ExpressionValue,
      substitution: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[MonomorphicExpression.Expression] =
    for {
      typeChecked <- getFactOrAbort(AbilityCheckedValue.Key(vfqn.value))
      typeParams   = TypeEvaluator.extractBodyTypeParams(toExpressionValue(typeChecked.signature))
      typeArgs    <- if (typeParams.nonEmpty) {
                       inferTypeArguments(toExpressionValue(typeChecked.signature), typeParams, callSiteType, substitution, source)
                     } else {
                       Seq.empty[Value].pure[CompilerIO]
                     }
      result      <-
        if (isAbilityRef(vfqn.value) && typeArgs.nonEmpty)
          for {
            abilityTypeParamCount <- countAbilityTypeParams(vfqn.value)
            abilityTypeArgs        = typeArgs.take(abilityTypeParamCount)
            impl                  <-
              getFactOrAbort(AbilityImplementation.Key(vfqn.value, abilityTypeArgs.map(v => fromExpressionValue(ConcreteValue(v)))))
          } yield MonomorphicExpression.MonomorphicValueReference(vfqn.as(impl.implementationFQN), Seq.empty)
        else
          for {
            _ <- checkTypeConsistency(toExpressionValue(typeChecked.signature), typeArgs, callSiteType, substitution, source)
          } yield MonomorphicExpression.MonomorphicValueReference(vfqn, typeArgs)
    } yield result

  private def isAbilityRef(vfqn: ValueFQN): Boolean =
    vfqn.name.qualifier.isInstanceOf[CoreQualifier.Ability]

  private def countAbilityTypeParams(vfqn: ValueFQN): CompilerIO[Int] = {
    val abilityName = vfqn.name.qualifier.asInstanceOf[CoreQualifier.Ability].name
    val markerVFQN  =
      ValueFQN(vfqn.moduleName, QualifiedName(abilityName, CoreQualifier.Ability(abilityName)))
    getFactOrAbort(AbilityCheckedValue.Key(markerVFQN))
      .map(marker => TypeEvaluator.extractBodyTypeParams(toExpressionValue(marker.signature)).size)
  }

  /** Infer concrete type arguments for a referenced value by matching the call-site type against the polymorphic
    * signature.
    *
    * For example, if:
    *   - Polymorphic signature is `[A] A -> A`
    *   - Call-site type is `Int -> Int`
    *   - Type params are `Seq("A")`
    *
    * Then we match `A -> A` against `Int -> Int` to infer `A = Int`.
    */
  private def inferTypeArguments(
      signature: ExpressionValue,
      typeParams: Seq[String],
      callSiteType: ExpressionValue,
      substitution: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[Seq[Value]] = {
    val strippedSignature = stripLeadingLambdas(signature)
    val bindings          = ExpressionValue.matchTypes(strippedSignature, callSiteType)

    typeParams.traverse { param =>
      bindings.get(param) match {
        case Some(exprValue) =>
          TypeEvaluator.evaluateWithSubstitution(exprValue, substitution, source)
        case None            =>
          // Try substitution as fallback (for nested generics)
          substitution.get(param) match {
            case Some(concreteType) => concreteType.pure[CompilerIO]
            case None               =>
              compilerAbort(source.as(s"Cannot infer type argument for parameter: $param"))
          }
      }
    }
  }

  /** Transform a function application.
    */
  private def transformFunctionApplication(
      target: Sourced[TypedExpression],
      arg: Sourced[TypedExpression],
      substitution: Map[String, Value]
  ): CompilerIO[MonomorphicExpression.Expression] =
    for {
      transformedTarget <- transformTypedExpression(target, substitution)
      transformedArg    <- transformTypedExpression(arg, substitution)
    } yield MonomorphicExpression.FunctionApplication(transformedTarget, transformedArg)

  /** Transform a function literal.
    */
  private def transformFunctionLiteral(
      paramName: Sourced[String],
      paramType: Sourced[SymbolicType],
      body: Sourced[TypedExpression],
      substitution: Map[String, Value]
  ): CompilerIO[MonomorphicExpression.Expression] =
    for {
      concreteParamType <- TypeEvaluator.evaluateWithSubstitution(toExpressionValue(paramType.value), substitution, paramType)
      transformedBody   <- transformTypedExpression(body, substitution)
    } yield MonomorphicExpression.FunctionLiteral(paramName, concreteParamType, transformedBody)

  /** Transform a TypedExpression to Sourced[MonomorphicExpression].
    */
  private def transformTypedExpression(
      typed: Sourced[TypedExpression],
      substitution: Map[String, Value]
  ): CompilerIO[Sourced[MonomorphicExpression]] =
    for {
      concreteType <- TypeEvaluator.evaluateWithSubstitution(toExpressionValue(typed.value.expressionType), substitution, typed)
      transformed  <-
        transformExpression(typed.value.expression, toExpressionValue(typed.value.expressionType), substitution, typed)
    } yield typed.as(MonomorphicExpression(concreteType, transformed))

  private def checkTypeConsistency(
      implSignature: ExpressionValue,
      typeArgs: Seq[Value],
      callSiteType: ExpressionValue,
      substitution: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[Unit] =
    for {
      implType   <- TypeEvaluator.evaluate(
                      TypeEvaluator.stripNonBodyUniversals(implSignature),
                      typeArgs,
                      source
                    )
      callerType <- TypeEvaluator.evaluateWithSubstitution(callSiteType, substitution, source)
      _          <- if (implType != callerType)
                      compilerAbort(
                        source.as(
                          s"Type mismatch"
                        ),
                        Seq(s"Expected: ${showValueType(implType)}", s"Actual:   ${showValueType(callerType)}")
                      )
                    else ().pure[CompilerIO]
    } yield ()

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
              s"Expected: ${showValueType(signatureReturnType)}",
              s"Actual:   ${showValueType(bodyReturnType)}"
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

  private def showValueType(value: Value): String = value match {
    case Value.Structure(fields, Value.Type) =>
      fields.get("$typeName") match {
        case Some(Value.Direct(vfqn: ValueFQN, _)) if vfqn === Types.functionDataTypeFQN =>
          val paramStr  = fields.get("A").map(showValueType).getOrElse("?")
          val returnStr = fields.get("B").map(showValueType).getOrElse("?")
          s"$paramStr -> $returnStr"
        case Some(Value.Direct(vfqn: ValueFQN, _))                                      =>
          val typeName = vfqn.name.name
          val typeArgs = fields.removed("$typeName").values.map(showValueType).toSeq
          if (typeArgs.isEmpty) typeName
          else s"$typeName[${typeArgs.mkString(", ")}]"
        case _                                                                           =>
          Value.valueUserDisplay.show(value)
      }
    case _                                   => Value.valueUserDisplay.show(value)
  }

}
