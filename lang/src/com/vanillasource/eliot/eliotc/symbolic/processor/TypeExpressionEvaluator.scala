package com.vanillasource.eliot.eliotc.symbolic.processor

import cats.data.{NonEmptySeq, StateT}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.Types.typeFQN
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Types, Value}
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression as Expr
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import com.vanillasource.eliot.eliotc.symbolic.fact.TypedExpression
import com.vanillasource.eliot.eliotc.symbolic.types.TypeCheckState.*

/** Evaluates expressions in type position, turning them into ExpressionValue.
  *
  * Processes type stacks by validating levels top-down, where each level's value must have valueType matching the level
  * above (with implicit Type at top). This is the type-level interpretation of Expression, distinct from runtime body
  * inference done by BodyTypeInferrer.
  *
  * Two public entry points control how universal type introductions are handled:
  *   - processStackForDeclaration: universal vars become FunctionLiteral (for the value's own type declaration)
  *   - processStackForInstantiation: universal vars become unification vars (for referenced values at call sites)
  */
object TypeExpressionEvaluator {

  private sealed trait InstantiationMode
  private case object Declaration                                        extends InstantiationMode
  private case class Instantiation(pendingArgs: List[ExpressionValue])  extends InstantiationMode

  /** Process a type stack in declaration context.
    *
    * Universal type introductions (e.g. [A] ->) are recorded as universal variables and the result is wrapped in a
    * FunctionLiteral to preserve the polymorphic type for monomorphization.
    *
    * @return
    *   Tuple of (signatureType, typedStack)
    */
  def processStackForDeclaration(
      stack: Sourced[TypeStack[OperatorResolvedExpression]]
  ): TypeGraphIO[(ExpressionValue, Sourced[TypeStack[TypedExpression]])] =
    processStack(stack, Declaration)

  /** Process a type stack in instantiation context.
    *
    * Universal type introductions are instantiated: explicit type args (if provided) are consumed one-by-one, and any
    * remaining universal vars become fresh unification variables. This allows type inference at call sites.
    *
    * @param explicitTypeArgs
    *   Explicit type arguments to consume in order per universal intro. Any remaining after processing signals too many
    *   type args.
    * @return
    *   Tuple of (signatureType, typedStack)
    */
  def processStackForInstantiation(
      stack: Sourced[TypeStack[OperatorResolvedExpression]],
      explicitTypeArgs: Seq[ExpressionValue] = Seq.empty
  ): TypeGraphIO[(ExpressionValue, Sourced[TypeStack[TypedExpression]])] =
    processStack(stack, Instantiation(explicitTypeArgs.toList))

  /** Shared handling for parameter references in both type-level and body-level contexts. */
  def handleParameterReference(name: Sourced[String]): TypeGraphIO[TypedExpression] =
    lookupParameter(name.value).map { maybeType =>
      val exprValue = maybeType.getOrElse(ParameterReference(name.value, Value.Type))
      TypedExpression(exprValue, TypedExpression.ParameterReference(name))
    }

  private def processStack(
      stack: Sourced[TypeStack[OperatorResolvedExpression]],
      mode: InstantiationMode
  ): TypeGraphIO[(ExpressionValue, Sourced[TypeStack[TypedExpression]])] =
    for {
      (signatureType, typedLevels) <- processLevels(stack.value.levels.toList.reverse, Value.Type, stack, mode)
    } yield (signatureType, stack.as(TypeStack(NonEmptySeq.fromSeqUnsafe(typedLevels.reverse))))

  /** Recursively process type levels from top (highest) to bottom (signature).
    *
    * @param levels
    *   Remaining levels to process (from top to bottom)
    * @param expectedType
    *   The expected type for the current level's value
    * @param source
    *   Source location for error messages
    * @param mode
    *   Whether we're in declaration or instantiation context
    * @return
    *   Tuple of (signatureType, typedLevels in reverse order)
    */
  private def processLevels(
      levels: List[OperatorResolvedExpression],
      expectedType: Value,
      source: Sourced[?],
      mode: InstantiationMode
  ): TypeGraphIO[(ExpressionValue, Seq[TypedExpression])] =
    levels match {
      case Nil =>
        generateUnificationVar.map((_, Seq.empty))

      case head :: Nil =>
        buildExpression(head, mode).map(typeResult => (typeResult.expressionType, Seq(typeResult)))

      case expr :: rest =>
        for {
          typeResult                       <- buildExpression(expr, mode)
          evaluatedValue                   <- extractConcreteValue(typeResult, expectedType, source)
          (signatureType, restTypedLevels) <- processLevels(rest, evaluatedValue, source, mode)
        } yield (signatureType, typeResult +: restTypedLevels)
    }

  /** Evaluate a single type-position expression in declaration context. Used by BodyTypeInferrer for evaluating explicit
    * type arguments.
    */
  def evaluateTypeExpression(expression: OperatorResolvedExpression): TypeGraphIO[TypedExpression] =
    buildExpression(expression, Declaration)

  /** Build a typed expression from a single type expression. */
  private def buildExpression(expression: OperatorResolvedExpression, mode: InstantiationMode): TypeGraphIO[TypedExpression] =
    expression match {
      case Expr.FunctionLiteral(paramName, Some(paramType), body) =>
        buildUniversalIntro(paramName, paramType, body, mode)

      case Expr.FunctionLiteral(paramName, None, _) =>
        StateT.liftF(compilerError(paramName.as("Lambda parameter in type annotation must have an explicit type."))) *>
          TypedExpression(ParameterReference(paramName.value, Value.Type), TypedExpression.ParameterReference(paramName))
            .pure[TypeGraphIO]

      case Expr.ValueReference(vfqn, _) =>
        buildValueReference(vfqn)

      case Expr.ParameterReference(name) =>
        handleParameterReference(name)

      case Expr.FunctionApplication(target, arg) =>
        buildTypeApplication(target, arg, mode)

      case Expr.IntegerLiteral(value) =>
        val exprValue = ConcreteValue(Value.Direct(value.value, Types.bigIntType))
        TypedExpression(exprValue, TypedExpression.IntegerLiteral(value)).pure[TypeGraphIO]

      case Expr.StringLiteral(value) =>
        val exprValue = ConcreteValue(Value.Direct(value.value, Types.stringType))
        TypedExpression(exprValue, TypedExpression.StringLiteral(value)).pure[TypeGraphIO]
    }

  /** Universal variable introduction: A -> ... where A is of type Type
    *
    * In Instantiation mode (when processing a referenced value's type), the type parameter becomes a fresh unification
    * variable instead of a universal variable. This allows type inference at call sites. In this mode, the body type is
    * returned directly (type params are instantiated).
    *
    * In Declaration mode, the polymorphic type (FunctionLiteral wrapping body type) is preserved so that
    * MonomorphicTypeCheckProcessor can apply type arguments during monomorphization.
    */
  private def buildUniversalIntro(
      paramName: Sourced[String],
      paramType: Sourced[TypeStack[OperatorResolvedExpression]],
      body: Sourced[TypeStack[OperatorResolvedExpression]],
      mode: InstantiationMode
  ): TypeGraphIO[TypedExpression] =
    mode match {
      case Declaration =>
        for {
          _                                 <- addUniversalVar(paramName.value)
          (paramTypeValue, typedParamStack) <- processStack(paramType, Declaration)
          (bodyTypeValue, typedBodyStack)   <- processStack(body, Declaration)
          resultType                         = FunctionLiteral(paramName.value, Value.Type, bodyTypeValue)
        } yield TypedExpression(
          resultType,
          TypedExpression.FunctionLiteral(paramName, paramType.as(paramTypeValue), body.as(typedBodyStack.value.signature))
        )

      case Instantiation(pendingArgs) =>
        // Consume the first pending arg if available; otherwise generate a fresh unification var
        val (bindAction, remainingArgs) = pendingArgs match {
          case head :: tail => (decrementExplicitTypeArgCount *> bindParameter(paramName.value, head), tail)
          case Nil          =>
            val action = generateUnificationVar.flatMap(uniVar => bindParameter(paramName.value, uniVar))
            (action, Nil)
        }
        for {
          _                                 <- bindAction
          (paramTypeValue, typedParamStack) <- processStack(paramType, Declaration)
          (bodyTypeValue, typedBodyStack)   <- processStack(body, Instantiation(remainingArgs))
        } yield TypedExpression(
          bodyTypeValue,
          TypedExpression.FunctionLiteral(paramName, paramType.as(paramTypeValue), body.as(typedBodyStack.value.signature))
        )
    }

  /** Value reference - could be a type like Int, String, or a universal var */
  private def buildValueReference(
      vfqn: Sourced[ValueFQN]
  ): TypeGraphIO[TypedExpression] =
    isUniversalVar(vfqn.value.name.name).map { isUniv =>
      val exprValue =
        if (isUniv) ParameterReference(vfqn.value.name.name, Value.Type)
        else ConcreteValue(Types.dataType(vfqn.value))
      TypedExpression(exprValue, TypedExpression.ValueReference(vfqn))
    }

  /** Function application in type position: A(B) means A parameterized by B */
  private def buildTypeApplication(
      target: Sourced[TypeStack[OperatorResolvedExpression]],
      arg: Sourced[TypeStack[OperatorResolvedExpression]],
      mode: InstantiationMode
  ): TypeGraphIO[TypedExpression] =
    for {
      (targetTypeValue, typedTargetStack) <- processStack(target, mode)
      (argTypeValue, typedArgStack)       <- processStack(arg, mode)
      resultType                           = FunctionApplication(targetTypeValue, argTypeValue)
    } yield TypedExpression(
      resultType,
      TypedExpression.FunctionApplication(
        target.as(typedTargetStack.value.signature),
        arg.as(typedArgStack.value.signature)
      )
    )

  private def extractConcreteValue(
      typeResult: TypedExpression,
      expectedType: Value,
      source: Sourced[?]
  ): TypeGraphIO[Value] =
    typeResult.expressionType match {
      case ConcreteValue(v) if v.valueType == expectedType =>
        v.pure[TypeGraphIO]
      case ConcreteValue(v)                                =>
        StateT.liftF(
          compilerError(source.as(s"Type level mismatch: expected $expectedType, but got ${v.valueType}"))
        ) *> v.pure[TypeGraphIO]
      case _                                               =>
        StateT.liftF(
          compilerError(source.as("Higher level type annotation must evaluate to a concrete type."))
        ) *> Value.Type.pure[TypeGraphIO]
    }
}
