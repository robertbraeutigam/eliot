package com.vanillasource.eliot.eliotc.symbolic.processor

import cats.data.{NonEmptySeq, StateT}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{Qualifier as CoreQualifier, TypeStack}
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.Types.typeFQN
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Types, Value}
import com.vanillasource.eliot.eliotc.eval.util.Evaluator
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
  private case object Declaration                                                extends InstantiationMode
  private case class Instantiation(pendingArgs: List[Sourced[ExpressionValue]])  extends InstantiationMode

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
    processStack(stack, Declaration).map { case (sourced, typed) => (sourced.value, typed) }

  /** Process a type stack in instantiation context.
    *
    * Universal type introductions are instantiated: explicit type args (if provided) are consumed one-by-one, and any
    * remaining universal vars become fresh unification variables. This allows type inference at call sites.
    *
    * @param explicitTypeArgs
    *   Explicit type arguments to consume in order per universal intro, each carrying the call-site source position.
    *   Any remaining after processing signals too many type args.
    * @return
    *   Tuple of (signatureType, typedStack)
    */
  def processStackForInstantiation(
      stack: Sourced[TypeStack[OperatorResolvedExpression]],
      explicitTypeArgs: Seq[Sourced[ExpressionValue]] = Seq.empty
  ): TypeGraphIO[(ExpressionValue, Sourced[TypeStack[TypedExpression]])] =
    processStack(stack, Instantiation(explicitTypeArgs.toList)).map { case (sourced, typed) => (sourced.value, typed) }

  /** Shared handling for parameter references in both type-level and body-level contexts. */
  def handleParameterReference(name: Sourced[String]): TypeGraphIO[TypedExpression] =
    handleParameterReferenceSourced(name).map(_._2)

  private def handleParameterReferenceSourced(
      name: Sourced[String]
  ): TypeGraphIO[(Sourced[ExpressionValue], TypedExpression)] =
    lookupParameter(name.value).map { maybeSourced =>
      val sourced = maybeSourced.getOrElse(unsourced(ParameterReference(name.value, Value.Type): ExpressionValue))
      (sourced, TypedExpression(sourced.value, TypedExpression.ParameterReference(name)))
    }

  private def processStack(
      stack: Sourced[TypeStack[OperatorResolvedExpression]],
      mode: InstantiationMode
  ): TypeGraphIO[(Sourced[ExpressionValue], Sourced[TypeStack[TypedExpression]])] =
    for {
      (sourcedSignatureType, typedLevels) <- processLevels(stack.value.levels.toList.reverse, Value.Type, stack, mode)
    } yield (sourcedSignatureType, stack.as(TypeStack(NonEmptySeq.fromSeqUnsafe(typedLevels.reverse))))

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
    *   Tuple of (sourcedSignatureType, typedLevels in reverse order)
    */
  private def processLevels(
      levels: List[OperatorResolvedExpression],
      expectedType: Value,
      source: Sourced[?],
      mode: InstantiationMode
  ): TypeGraphIO[(Sourced[ExpressionValue], Seq[TypedExpression])] =
    levels match {
      case Nil =>
        generateUnificationVar.map(v => (unsourced(v: ExpressionValue), Seq.empty))

      case head :: Nil =>
        buildExpression(head, mode).map { case (sourced, typeResult) => (sourced, Seq(typeResult)) }

      case expr :: rest =>
        for {
          (_, typeResult)                                <- buildExpression(expr, mode)
          evaluatedValue                                 <- extractConcreteValue(typeResult, expectedType, source)
          (sourcedSignatureType, restTypedLevels)        <- processLevels(rest, evaluatedValue, source, mode)
        } yield (sourcedSignatureType, typeResult +: restTypedLevels)
    }

  /** Evaluate a single type-position expression in declaration context. Used by BodyTypeInferrer for evaluating explicit
    * type arguments.
    */
  def evaluateTypeExpression(expression: OperatorResolvedExpression): TypeGraphIO[TypedExpression] =
    buildExpression(expression, Declaration).map(_._2)

  /** Build a typed expression from a single type expression, returning its preferred source alongside. */
  private def buildExpression(
      expression: OperatorResolvedExpression,
      mode: InstantiationMode
  ): TypeGraphIO[(Sourced[ExpressionValue], TypedExpression)] =
    expression match {
      case Expr.FunctionLiteral(paramName, Some(paramType), body) =>
        buildUniversalIntro(paramName, paramType, body, mode)

      case Expr.FunctionLiteral(paramName, None, _) =>
        val exprValue = ParameterReference(paramName.value, Value.Type)
        StateT.liftF(compilerError(paramName.as("Lambda parameter in type annotation must have an explicit type."))) *>
          (unsourced(exprValue: ExpressionValue),
           TypedExpression(exprValue, TypedExpression.ParameterReference(paramName)))
            .pure[TypeGraphIO]

      case Expr.ValueReference(vfqn, _) =>
        buildValueReference(vfqn)

      case Expr.ParameterReference(name) =>
        handleParameterReferenceSourced(name)

      case Expr.FunctionApplication(target, arg) =>
        buildTypeApplication(target, arg, mode)

      case Expr.IntegerLiteral(value) =>
        val exprValue = ConcreteValue(Value.Direct(value.value, Types.bigIntType))
        (unsourced(exprValue: ExpressionValue), TypedExpression(exprValue, TypedExpression.IntegerLiteral(value)))
          .pure[TypeGraphIO]

      case Expr.StringLiteral(value) =>
        val exprValue = ConcreteValue(Value.Direct(value.value, Types.stringType))
        (unsourced(exprValue: ExpressionValue), TypedExpression(exprValue, TypedExpression.StringLiteral(value)))
          .pure[TypeGraphIO]
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
  ): TypeGraphIO[(Sourced[ExpressionValue], TypedExpression)] =
    mode match {
      case Declaration =>
        for {
          _                                          <- addUniversalVar(paramName.value)
          (sourcedParamTypeValue, typedParamStack)   <- processStack(paramType, Declaration)
          (sourcedBodyTypeValue, typedBodyStack)     <- processStack(body, Declaration)
          resultType                                  = FunctionLiteral(paramName.value, Value.Type, sourcedBodyTypeValue)
        } yield (
          unsourced(resultType: ExpressionValue),
          TypedExpression(
            resultType,
            TypedExpression.FunctionLiteral(paramName, sourcedParamTypeValue.map(identity), body.as(typedBodyStack.value.signature))
          )
        )

      case Instantiation(pendingArgs) =>
        // Consume the first pending arg if available; otherwise generate a fresh unification var
        val (bindAction, remainingArgs) = pendingArgs match {
          case head :: tail => (decrementExplicitTypeArgCount *> bindParameter(paramName.value, head), tail)
          case Nil          =>
            val action = generateUnificationVar.flatMap(uniVar => bindParameter(paramName.value, unsourced(uniVar: ExpressionValue)))
            (action, Nil)
        }
        for {
          _                                          <- bindAction
          (sourcedParamTypeValue, typedParamStack)   <- processStack(paramType, Declaration)
          (sourcedBodyTypeValue, typedBodyStack)     <- processStack(body, Instantiation(remainingArgs))
        } yield (
          sourcedBodyTypeValue,
          TypedExpression(
            sourcedBodyTypeValue.value,
            TypedExpression.FunctionLiteral(paramName, sourcedParamTypeValue.map(identity), body.as(typedBodyStack.value.signature))
          )
        )
    }

  /** Value reference - could be a type like Int, String, or a universal var */
  private def buildValueReference(
      vfqn: Sourced[ValueFQN]
  ): TypeGraphIO[(Sourced[ExpressionValue], TypedExpression)] =
    isUniversalVar(vfqn.value.name.name).flatMap { isUniv =>
      if (isUniv) {
        val exprValue = ParameterReference(vfqn.value.name.name, Value.Type)
        (unsourced(exprValue: ExpressionValue), TypedExpression(exprValue, TypedExpression.ValueReference(vfqn)))
          .pure[TypeGraphIO]
      } else {
        StateT
          .liftF(Evaluator.evaluateValueToNormalForm(vfqn.value, vfqn))
          .flatMap { exprValue =>
            if (isAbstractAbilityType(exprValue, vfqn.value)) {
              generateUnificationVar.map(uniVar =>
                (unsourced(uniVar: ExpressionValue), TypedExpression(uniVar, TypedExpression.ValueReference(vfqn)))
              )
            } else {
              (unsourced(exprValue: ExpressionValue), TypedExpression(exprValue, TypedExpression.ValueReference(vfqn)))
                .pure[TypeGraphIO]
            }
          }
      }
    }

  /** Detect abstract ability type aliases: they have an Ability qualifier and evaluate to a bare data type marker
    * (meaning they have no body to reduce). These should be treated as unification variables in the symbolic phase,
    * since their concrete type is determined by the ability implementation.
    */
  private def isAbstractAbilityType(exprValue: ExpressionValue, vfqn: ValueFQN): Boolean =
    vfqn.name.qualifier match {
      case _: CoreQualifier.Ability =>
        exprValue match {
          case ConcreteValue(Value.Structure(fields, Value.Type)) =>
            fields.size == 1 && fields.contains("$typeName")
          case _                                                  => false
        }
      case _                        => false
    }

  /** Function application in type position: A(B) means A parameterized by B */
  private def buildTypeApplication(
      target: Sourced[TypeStack[OperatorResolvedExpression]],
      arg: Sourced[TypeStack[OperatorResolvedExpression]],
      mode: InstantiationMode
  ): TypeGraphIO[(Sourced[ExpressionValue], TypedExpression)] =
    for {
      (sourcedTargetTypeValue, typedTargetStack) <- processStack(target, mode)
      (sourcedArgTypeValue, typedArgStack)       <- processStack(arg, mode)
      resultType                                  = ExpressionValue.betaReduce(FunctionApplication(sourcedTargetTypeValue, sourcedArgTypeValue))
    } yield (
      unsourced(resultType: ExpressionValue),
      TypedExpression(
        resultType,
        TypedExpression.FunctionApplication(
          target.as(typedTargetStack.value.signature),
          arg.as(typedArgStack.value.signature)
        )
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
