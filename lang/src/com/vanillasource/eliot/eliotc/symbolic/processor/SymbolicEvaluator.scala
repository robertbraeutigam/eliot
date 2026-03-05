package com.vanillasource.eliot.eliotc.symbolic.processor

import cats.data.{NonEmptySeq, StateT}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier as CoreQualifier, TypeStack}
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Types, Value}
import com.vanillasource.eliot.eliotc.eval.fact.Types.typeFQN
import com.vanillasource.eliot.eliotc.eval.util.Evaluator
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression as Expr
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import com.vanillasource.eliot.eliotc.symbolic.fact.TypedExpression
import com.vanillasource.eliot.eliotc.symbolic.types.{SymbolicUnification, TypeCheckState}
import com.vanillasource.eliot.eliotc.symbolic.types.TypeCheckState.*

/** Unified symbolic evaluator that processes type stacks by walking levels top-down and dispatching
  * to either type-context evaluation or body-context inference at the bottom level.
  *
  * The recursive backbone is processStack/processLevels, which handles any TypeStack uniformly:
  *   - Higher levels are always evaluated in type context (must be concrete)
  *   - The bottom level is dispatched based on StackContext: type evaluation or body inference
  *
  * Type-context evaluation (evaluateExpression) turns type-position expressions into ExpressionValue,
  * using Evaluator for value references. Body-context inference (inferExpression) generates unification
  * variables and emits constraints for runtime expressions.
  */
object SymbolicEvaluator {

  // --- Stack context: controls how the bottom level of a stack is processed ---

  sealed trait StackContext
  private case class TypeContext(mode: InstantiationMode) extends StackContext
  private case object BodyContext                         extends StackContext

  private sealed trait InstantiationMode
  private case object Declaration                                                extends InstantiationMode
  private case class Instantiation(pendingArgs: List[Sourced[ExpressionValue]])  extends InstantiationMode

  // --- Public API ---

  /** Process a type stack in declaration context. Universal type introductions are recorded as
    * universal variables and wrapped in FunctionLiteral to preserve polymorphic structure.
    */
  def processStackForDeclaration(
      stack: Sourced[TypeStack[OperatorResolvedExpression]]
  ): TypeGraphIO[(ExpressionValue, Sourced[TypeStack[TypedExpression]])] =
    processStack(stack, TypeContext(Declaration)).map { case (sourced, typed) => (sourced.value, typed) }

  /** Process a type stack in instantiation context. Universal type introductions consume explicit
    * type args or become fresh unification variables for type inference at call sites.
    */
  def processStackForInstantiation(
      stack: Sourced[TypeStack[OperatorResolvedExpression]],
      explicitTypeArgs: Seq[Sourced[ExpressionValue]] = Seq.empty
  ): TypeGraphIO[(ExpressionValue, Sourced[TypeStack[TypedExpression]])] =
    processStack(stack, TypeContext(Instantiation(explicitTypeArgs.toList))).map { case (sourced, typed) =>
      (sourced.value, typed)
    }

  /** Infer a type stack in body context. Higher levels (if any) are evaluated in type context,
    * and the bottom level is inferred with constraint generation.
    */
  def inferStack(
      stack: Sourced[TypeStack[OperatorResolvedExpression]]
  ): TypeGraphIO[TypedExpression] =
    processStack(stack, BodyContext).map { case (_, typedStack) => typedStack.value.signature }

  /** Infer a single body expression, generating unification variables and emitting constraints. */
  def inferExpression(body: Sourced[OperatorResolvedExpression]): TypeGraphIO[TypedExpression] =
    inferExpressionImpl(body)

  /** Evaluate a single type-position expression in declaration context. */
  def evaluateTypeExpression(expression: OperatorResolvedExpression): TypeGraphIO[TypedExpression] =
    evaluateExpressionImpl(expression, Declaration).map(_._2)

  // --- Stack processing backbone ---

  private def processStack(
      stack: Sourced[TypeStack[OperatorResolvedExpression]],
      context: StackContext
  ): TypeGraphIO[(Sourced[ExpressionValue], Sourced[TypeStack[TypedExpression]])] =
    for {
      (sourcedSignatureType, typedLevels) <- processLevels(stack.value.levels.toList.reverse, Value.Type, stack, context)
    } yield (sourcedSignatureType, stack.as(TypeStack(NonEmptySeq.fromSeqUnsafe(typedLevels.reverse))))

  /** Recursively process type levels from top (highest) to bottom (signature).
    *
    * Higher levels are always evaluated in type context and must produce ConcreteValues.
    * The bottom level is dispatched based on context: type evaluation or body inference.
    */
  private def processLevels(
      levels: List[OperatorResolvedExpression],
      expectedType: Value,
      source: Sourced[?],
      context: StackContext
  ): TypeGraphIO[(Sourced[ExpressionValue], Seq[TypedExpression])] =
    levels match {
      case Nil =>
        generateUnificationVar.map(v => (unsourced(v: ExpressionValue), Seq.empty))

      case head :: Nil =>
        context match {
          case TypeContext(mode) =>
            evaluateExpressionImpl(head, mode).map { case (sourced, typeResult) => (sourced, Seq(typeResult)) }
          case BodyContext       =>
            inferExpressionImpl(source.as(head)).map { typed =>
              (unsourced(typed.expressionType: ExpressionValue), Seq(typed))
            }
        }

      case expr :: rest =>
        for {
          (_, typeResult)                         <- evaluateExpressionImpl(expr, Declaration)
          evaluatedValue                          <- extractConcreteValue(typeResult, expectedType, source)
          (sourcedSignatureType, restTypedLevels) <- processLevels(rest, evaluatedValue, source, context)
        } yield (sourcedSignatureType, typeResult +: restTypedLevels)
    }

  // --- Type-context expression evaluation ---

  private def evaluateExpressionImpl(
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
          (sourcedParamTypeValue, typedParamStack)   <- processStack(paramType, TypeContext(Declaration))
          (sourcedBodyTypeValue, typedBodyStack)     <- processStack(body, TypeContext(Declaration))
          resultType                                  = FunctionLiteral(paramName.value, Value.Type, sourcedBodyTypeValue)
        } yield (
          unsourced(resultType: ExpressionValue),
          TypedExpression(
            resultType,
            TypedExpression.FunctionLiteral(paramName, sourcedParamTypeValue.map(identity), body.as(typedBodyStack.value.signature))
          )
        )

      case Instantiation(pendingArgs) =>
        val (bindAction, remainingArgs) = pendingArgs match {
          case head :: tail => (decrementExplicitTypeArgCount *> bindParameter(paramName.value, head), tail)
          case Nil          =>
            val action = generateUnificationVar.flatMap(uniVar => bindParameter(paramName.value, unsourced(uniVar: ExpressionValue)))
            (action, Nil)
        }
        for {
          _                                          <- bindAction
          (sourcedParamTypeValue, typedParamStack)   <- processStack(paramType, TypeContext(Declaration))
          (sourcedBodyTypeValue, typedBodyStack)     <- processStack(body, TypeContext(Instantiation(remainingArgs)))
        } yield (
          sourcedBodyTypeValue,
          TypedExpression(
            sourcedBodyTypeValue.value,
            TypedExpression.FunctionLiteral(paramName, sourcedParamTypeValue.map(identity), body.as(typedBodyStack.value.signature))
          )
        )
    }

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

  private def buildTypeApplication(
      target: Sourced[TypeStack[OperatorResolvedExpression]],
      arg: Sourced[TypeStack[OperatorResolvedExpression]],
      mode: InstantiationMode
  ): TypeGraphIO[(Sourced[ExpressionValue], TypedExpression)] =
    for {
      (sourcedTargetTypeValue, typedTargetStack) <- processStack(target, TypeContext(mode))
      (sourcedArgTypeValue, typedArgStack)       <- processStack(arg, TypeContext(mode))
      reduced                                     = ExpressionValue.betaReduce(FunctionApplication(sourcedTargetTypeValue, sourcedArgTypeValue))
      resultType                                 <- resolveTypeApplication(reduced)
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

  private def resolveTypeApplication(reduced: ExpressionValue): TypeGraphIO[ExpressionValue] =
    reduced match {
      case fa @ FunctionApplication(_, _) if !isConstructorApplication(fa) =>
        val (base, argCount) = extractBaseAndArgCount(fa)
        extractFQN(base) match {
          case Some(vfqn) =>
            for {
              result <- StateT.liftF(
                          getFactOrAbort(OperatorResolvedValue.Key(vfqn)).flatMap { resolved =>
                            processStackForDeclaration(resolved.typeStack)
                              .runA(TypeCheckState())
                              .map { case (signatureType, _) =>
                                val stripped = ExpressionValue.stripUniversalTypeIntros(signatureType)
                                stripFunctionTypes(stripped, argCount).getOrElse(reduced)
                              }
                          }
                        )
              _      <- collectParameterRefs(result).traverse_(addUniversalVar)
            } yield result
          case None       => reduced.pure[TypeGraphIO]
        }
      case _                                                               => reduced.pure[TypeGraphIO]
    }

  private def collectParameterRefs(expr: ExpressionValue): List[String] =
    ExpressionValue.fold[List[String]](
      onConcrete = _ => Nil,
      onNative = _ => Nil,
      onParamRef = (name, _) => List(name),
      onFunApp = (a, b) => a ++ b,
      onFunLit = (_, _, body) => body
    )(expr)

  private def extractBaseAndArgCount(expr: ExpressionValue): (ExpressionValue, Int) =
    expr match {
      case FunctionApplication(target, _) =>
        val (base, count) = extractBaseAndArgCount(target.value)
        (base, count + 1)
      case other                          => (other, 0)
    }

  private def extractFQN(expr: ExpressionValue): Option[ValueFQN] =
    expr match {
      case ConcreteValue(Value.Structure(fields, _)) =>
        fields.get("$typeName").collect { case Value.Direct(vfqn: ValueFQN, _) => vfqn }
      case _                                         => None
    }

  @scala.annotation.tailrec
  private def stripFunctionTypes(expr: ExpressionValue, n: Int): Option[ExpressionValue] =
    if (n <= 0) Some(expr)
    else
      expr match {
        case FunctionType(_, returnType) => stripFunctionTypes(returnType, n - 1)
        case _                           => None
      }

  private def isConstructorApplication(expr: ExpressionValue): Boolean =
    ExpressionValue.stripLeadingFunctionApplications(expr) match {
      case ConcreteValue(Value.Structure(fields, _)) =>
        fields.get("$typeName").exists {
          case Value.Direct(vfqn: ValueFQN, _) => vfqn.name.qualifier == CoreQualifier.Type
          case _                                => false
        }
      case ParameterReference(_, _)                  => true
      case _                                         => false
    }

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

  // --- Body-context expression inference ---

  private def inferExpressionImpl(body: Sourced[OperatorResolvedExpression]): TypeGraphIO[TypedExpression] =
    body.value match {
      case Expr.IntegerLiteral(value)                            =>
        inferLiteral(value, "Number", QualifiedName("Int", CoreQualifier.Type), TypedExpression.IntegerLiteral(value))
      case Expr.StringLiteral(value)                             =>
        inferLiteral(value, "String", QualifiedName("String", CoreQualifier.Type), TypedExpression.StringLiteral(value))
      case Expr.ParameterReference(name)                         =>
        handleParameterReference(name)
      case Expr.ValueReference(vfqn, typeArgs)                   =>
        inferValueReference(vfqn, typeArgs)
      case Expr.FunctionApplication(target, arg)                 =>
        inferFunctionApplication(body, target, arg)
      case Expr.FunctionLiteral(paramName, paramType, bodyStack) =>
        inferFunctionLiteral(paramName, paramType, bodyStack)
    }

  private def inferLiteral[T](
      value: Sourced[T],
      moduleName: String,
      typeName: QualifiedName,
      typedExpr: TypedExpression.Expression
  ): TypeGraphIO[TypedExpression] =
    TypedExpression(
      ConcreteValue(Types.dataType(ValueFQN(ModuleName(Seq("eliot", "lang"), moduleName), typeName))),
      typedExpr
    ).pure[TypeGraphIO]

  private def inferValueReference(
      vfqn: Sourced[ValueFQN],
      typeArgs: Seq[Sourced[OperatorResolvedExpression]] = Seq.empty
  ): TypeGraphIO[TypedExpression] = {
    if (vfqn.value === typeFQN) {
      val exprValue = ConcreteValue(Types.dataType(vfqn.value))
      TypedExpression(exprValue, TypedExpression.ValueReference(vfqn)).pure[TypeGraphIO]
    } else {
      for {
        resolved              <- StateT.liftF(getFactOrAbort(OperatorResolvedValue.Key(vfqn.value)))
        sourcedEvaluatedTypeArgs <-
          typeArgs.traverse(arg =>
            evaluateTypeExpression(arg.value).map(result => arg.as(result.expressionType))
          )
        _                     <- setExplicitTypeArgCount(sourcedEvaluatedTypeArgs.length)
        (signatureType, _)    <- processStackForInstantiation(resolved.typeStack, sourcedEvaluatedTypeArgs)
        remaining             <- getExplicitTypeArgCount
        _                     <- if (remaining > 0)
                                   StateT.liftF(compilerError(vfqn.as("Too many explicit type arguments.")))
                                 else ().pure[TypeGraphIO]
      } yield TypedExpression(signatureType, TypedExpression.ValueReference(vfqn))
    }
  }

  private def inferFunctionApplication(
      body: Sourced[OperatorResolvedExpression],
      target: Sourced[TypeStack[OperatorResolvedExpression]],
      arg: Sourced[TypeStack[OperatorResolvedExpression]]
  ): TypeGraphIO[TypedExpression] =
    for {
      argTypeVar      <- generateUnificationVar
      retTypeVar      <- generateUnificationVar
      targetResult    <- inferStack(target)
      argResult       <- inferStack(arg)
      expectedFuncType = functionType(argTypeVar, retTypeVar)
      _               <- tellConstraint(
                           SymbolicUnification.constraint(
                             expectedFuncType,
                             target.as(targetResult.expressionType),
                             "Target of function application is not a Function. Possibly too many arguments."
                           )
                         )
      _               <- tellConstraint(
                           SymbolicUnification.constraint(argTypeVar, arg.as(argResult.expressionType), "Argument type mismatch.")
                         )
      typedTarget      = target.as(targetResult)
      typedArg         = arg.as(argResult)
    } yield TypedExpression(retTypeVar, TypedExpression.FunctionApplication(typedTarget, typedArg))

  private def inferFunctionLiteral(
      paramName: Sourced[String],
      paramType: Option[Sourced[TypeStack[OperatorResolvedExpression]]],
      bodyStack: Sourced[TypeStack[OperatorResolvedExpression]]
  ): TypeGraphIO[TypedExpression] =
    for {
      typedParamType <- paramType match {
                          case Some(pt) =>
                            processStackForDeclaration(pt).map { case (v, _) => pt.as(v) }
                          case None     => generateUnificationVar.map(v => paramName.as(v: ExpressionValue))
                        }
      _              <- bindParameter(paramName.value, typedParamType)
      bodyResult     <- inferStack(bodyStack)
      funcType        = functionType(typedParamType.value, bodyResult.expressionType)
    } yield TypedExpression(
      funcType,
      TypedExpression.FunctionLiteral(paramName, typedParamType, bodyStack.as(bodyResult))
    )

  // --- Shared helpers ---

  private def handleParameterReference(name: Sourced[String]): TypeGraphIO[TypedExpression] =
    handleParameterReferenceSourced(name).map(_._2)

  private def handleParameterReferenceSourced(
      name: Sourced[String]
  ): TypeGraphIO[(Sourced[ExpressionValue], TypedExpression)] =
    lookupParameter(name.value).map { maybeSourced =>
      val sourced = maybeSourced.getOrElse(unsourced(ParameterReference(name.value, Value.Type): ExpressionValue))
      (sourced, TypedExpression(sourced.value, TypedExpression.ParameterReference(name)))
    }
}
