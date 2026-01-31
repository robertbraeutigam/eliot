package com.vanillasource.eliot.eliotc.monomorphize.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module2.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort
import com.vanillasource.eliot.eliotc.symbolic.fact.{TypeCheckedValue, TypedExpression}

/** Processor that monomorphizes (specializes) generic functions.
  *
  * Given a MonomorphicValue.Key(vfqn, typeArgs), it:
  *   1. Fetches the TypeCheckedValue for vfqn
  *   2. Builds substitution from universal type params to concrete args
  *   3. Evaluates the signature type to a ConcreteType
  *   4. Transforms the runtime body, replacing types and recursively monomorphizing called functions
  */
class MonomorphicTypeCheckProcessor
    extends SingleKeyTypeProcessor[MonomorphicValue.Key]
    with Logging {

  override protected def generateFact(key: MonomorphicValue.Key): CompilerIO[Unit] =
    for {
      typeChecked <- getFactOrAbort(TypeCheckedValue.Key(key.vfqn))

      typeParams = TypeEvaluator.extractUniversalParams(typeChecked.signature)

      _ <- if (typeParams.length != key.typeArguments.length)
             compilerAbort(
               typeChecked.name.as(
                 s"Type argument count mismatch: expected ${typeParams.length}, got ${key.typeArguments.length}"
               )
             )
           else ().pure[CompilerIO]

      substitution = TypeEvaluator.buildSubstitution(typeParams, key.typeArguments)

      _ <- debug[CompilerIO](s"Monomorphizing ${key.vfqn} with substitution: $substitution")

      strippedSignature = TypeEvaluator.stripUniversalIntros(typeChecked.signature)
      signature        <- TypeEvaluator.evaluate(strippedSignature, substitution, typeChecked.name)

      runtime <- typeChecked.runtime.traverse { body =>
                   transformExpression(body.value, strippedSignature, substitution, body).map(body.as)
                 }

      _ <- registerFactIfClear(
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
    * substitution and recursively monomorphizing value references.
    *
    * @param callSiteType
    *   The type of this expression at the call site (after unification). Used to infer type arguments for value
    *   references.
    */
  private def transformExpression(
      expr: TypedExpression.Expression,
      callSiteType: ExpressionValue,
      substitution: Map[String, ConcreteType],
      source: Sourced[?]
  ): CompilerIO[MonomorphicExpression.Expression] =
    expr match {
      case TypedExpression.IntegerLiteral(value) =>
        MonomorphicExpression.IntegerLiteral(value).pure[CompilerIO]

      case TypedExpression.StringLiteral(value) =>
        MonomorphicExpression.StringLiteral(value).pure[CompilerIO]

      case TypedExpression.ParameterReference(name) =>
        MonomorphicExpression.ParameterReference(name).pure[CompilerIO]

      case TypedExpression.ValueReference(vfqn) =>
        transformValueReference(vfqn, callSiteType, substitution, source)

      case TypedExpression.FunctionApplication(target, arg) =>
        transformFunctionApplication(target, arg, substitution, source)

      case TypedExpression.FunctionLiteral(paramName, paramType, body) =>
        transformFunctionLiteral(paramName, paramType, body, substitution, source)
    }

  /** Transform a value reference, determining concrete type arguments and triggering monomorphization of the
    * referenced value.
    *
    * @param callSiteType
    *   The type of this value reference at the call site (after unification). For a generic function like `id[A]: A ->
    *   A` called with Int, this would be `Int -> Int`.
    */
  private def transformValueReference(
      vfqn: Sourced[ValueFQN],
      callSiteType: ExpressionValue,
      substitution: Map[String, ConcreteType],
      source: Sourced[?]
  ): CompilerIO[MonomorphicExpression.Expression] =
    for {
      typeChecked <- getFactOrAbort(TypeCheckedValue.Key(vfqn.value))

      typeParams = TypeEvaluator.extractUniversalParams(typeChecked.signature)

      typeArgs <- if (typeParams.nonEmpty) {
                    inferTypeArguments(typeChecked.signature, typeParams, callSiteType, substitution, source)
                  } else {
                    Seq.empty[ConcreteType].pure[CompilerIO]
                  }

      _ <- if (typeArgs.nonEmpty || typeParams.isEmpty)
             getFact(MonomorphicValue.Key(vfqn.value, typeArgs)).void
           else ().pure[CompilerIO]
    } yield MonomorphicExpression.MonomorphicValueReference(vfqn, typeArgs)

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
      substitution: Map[String, ConcreteType],
      source: Sourced[?]
  ): CompilerIO[Seq[ConcreteType]] = {
    val strippedSignature = TypeEvaluator.stripUniversalIntros(signature)
    val bindings          = matchTypes(strippedSignature, callSiteType)

    typeParams.traverse { param =>
      bindings.get(param) match {
        case Some(exprValue) =>
          TypeEvaluator.evaluate(exprValue, substitution, source)
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

  /** Match a polymorphic type pattern against a concrete (or partially concrete) type to extract bindings for type
    * parameters.
    */
  private def matchTypes(
      pattern: ExpressionValue,
      target: ExpressionValue
  ): Map[String, ExpressionValue] =
    (pattern, target) match {
      case (ParameterReference(name, _), _) =>
        // Universal type parameter matches the target type
        Map(name -> target)

      case (FunctionType(patParam, patRet), FunctionType(tgtParam, tgtRet)) =>
        matchTypes(patParam, tgtParam) ++ matchTypes(patRet, tgtRet)

      case (FunctionApplication(patTarget, patArg), FunctionApplication(tgtTarget, tgtArg)) =>
        matchTypes(patTarget, tgtTarget) ++ matchTypes(patArg, tgtArg)

      case (FunctionLiteral(_, patParamType, patBody), FunctionLiteral(_, tgtParamType, tgtBody))
          if patParamType != Value.Type =>
        // Regular function literal (not universal intro)
        matchTypes(patBody, tgtBody)

      case _ =>
        // Concrete types or no match
        Map.empty
    }

  /** Transform a function application.
    */
  private def transformFunctionApplication(
      target: Sourced[TypeStack[TypedExpression]],
      arg: Sourced[TypeStack[TypedExpression]],
      substitution: Map[String, ConcreteType],
      source: Sourced[?]
  ): CompilerIO[MonomorphicExpression.Expression] =
    for {
      transformedTarget <- transformTypedExpressionStack(target, substitution)
      transformedArg    <- transformTypedExpressionStack(arg, substitution)
    } yield MonomorphicExpression.FunctionApplication(transformedTarget, transformedArg)

  /** Transform a function literal.
    */
  private def transformFunctionLiteral(
      paramName: Sourced[String],
      paramType: Sourced[TypeStack[TypedExpression]],
      body: Sourced[TypeStack[TypedExpression]],
      substitution: Map[String, ConcreteType],
      source: Sourced[?]
  ): CompilerIO[MonomorphicExpression.Expression] =
    for {
      concreteParamType <- evaluateTypeStack(paramType.value, substitution, paramType)
      transformedBody   <- transformTypedExpressionStack(body, substitution)
    } yield MonomorphicExpression.FunctionLiteral(paramName, concreteParamType, transformedBody)

  /** Transform a TypeStack[TypedExpression] to Sourced[MonomorphicExpression]. Extracts the signature level and
    * transforms it.
    */
  private def transformTypedExpressionStack(
      stack: Sourced[TypeStack[TypedExpression]],
      substitution: Map[String, ConcreteType]
  ): CompilerIO[Sourced[MonomorphicExpression]] =
    stack.value.signature match {
      case Some(typed) =>
        for {
          concreteType <- TypeEvaluator.evaluate(typed.expressionType, substitution, stack)
          transformed  <- transformExpression(typed.expression, typed.expressionType, substitution, stack)
        } yield stack.as(MonomorphicExpression(concreteType, transformed))
      case None        =>
        compilerAbort(stack.as("Empty expression stack in monomorphization"))
    }

  /** Evaluate a TypeStack to get the concrete signature type.
    */
  private def evaluateTypeStack(
      stack: TypeStack[TypedExpression],
      substitution: Map[String, ConcreteType],
      source: Sourced[?]
  ): CompilerIO[ConcreteType] =
    stack.signature match {
      case Some(typed) =>
        TypeEvaluator.evaluate(typed.expressionType, substitution, source)
      case None        =>
        compilerAbort(source.as("Empty type stack in monomorphization"))
    }
}
