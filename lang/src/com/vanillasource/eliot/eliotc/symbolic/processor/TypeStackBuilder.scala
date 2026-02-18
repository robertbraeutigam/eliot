package com.vanillasource.eliot.eliotc.symbolic.processor

import cats.data.{NonEmptySeq, StateT}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Types}
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.Types.{functionDataTypeFQN, typeFQN}
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, ResolvedValue}
import com.vanillasource.eliot.eliotc.resolve.fact.Expression as Expr
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import com.vanillasource.eliot.eliotc.symbolic.fact.TypedExpression
import com.vanillasource.eliot.eliotc.symbolic.types.SymbolicUnification
import com.vanillasource.eliot.eliotc.symbolic.types.TypeCheckState.*

/** Processes type stacks and expressions in both type-level and body-level contexts. For type stacks, validates levels
  * top-down where each level's value must have valueType matching the level above (with implicit Type at top). For body
  * expressions, infers types by generating unification variables and emitting constraints.
  */
object TypeStackBuilder {

  /** Process a type stack by processing all type levels from top to bottom.
    *
    * @return
    *   Tuple of (signatureType, typedStack)
    */
  def processStack(
      stack: Sourced[TypeStack[Expression]]
  ): TypeGraphIO[(ExpressionValue, Sourced[TypeStack[TypedExpression]])] =
    for {
      (signatureType, typedLevels) <- processLevels(stack.value.levels.toList.reverse, Value.Type, stack)
    } yield (signatureType, stack.as(TypeStack(NonEmptySeq.fromSeqUnsafe(typedLevels.reverse))))

  /** Build constraints from a body expression, inferring types. */
  def inferBody(body: Sourced[Expression]): TypeGraphIO[TypedExpression] =
    body.value match {
      case Expr.IntegerLiteral(value)                            =>
        inferLiteral(value, "Number", QualifiedName("Int", Qualifier.Type), TypedExpression.IntegerLiteral(value))
      case Expr.StringLiteral(value)                             =>
        inferLiteral(value, "String", QualifiedName("String", Qualifier.Type), TypedExpression.StringLiteral(value))
      case Expr.ParameterReference(name)                         =>
        handleParameterReference(name)
      case Expr.ValueReference(vfqn)                             =>
        inferValueReference(vfqn)
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

  private def inferValueReference(vfqn: Sourced[ValueFQN]): TypeGraphIO[TypedExpression] = {
    if (vfqn.value === typeFQN) {
      val exprValue = ConcreteValue(Types.dataType(vfqn.value))
      TypedExpression(exprValue, TypedExpression.ValueReference(vfqn)).pure[TypeGraphIO]
    } else {

      for {
        resolved <- StateT.liftF(getFactOrAbort(ResolvedValue.Key(vfqn.value)))
        // Use instantiation mode so type parameters become unification vars
        result   <- withInstantiationMode {
                      processStack(resolved.typeStack).map { case (signatureType, _) =>
                        TypedExpression(signatureType, TypedExpression.ValueReference(vfqn))
                      }
                    }
      } yield result
    }
  }

  private def inferFunctionApplication(
      body: Sourced[Expression],
      target: Sourced[TypeStack[Expression]],
      arg: Sourced[TypeStack[Expression]]
  ): TypeGraphIO[TypedExpression] =
    for {
      argTypeVar      <- generateUnificationVar(arg)
      retTypeVar      <- generateUnificationVar(body)
      targetResult    <- inferBodyStack(target)
      argResult       <- inferBodyStack(arg)
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
      paramType: Sourced[TypeStack[Expression]],
      bodyStack: Sourced[TypeStack[Expression]]
  ): TypeGraphIO[TypedExpression] =
    for {
      (paramTypeValue, _) <- processStack(paramType)
      _                   <- bindParameter(paramName.value, paramTypeValue)
      bodyResult          <- inferBodyStack(bodyStack)
      funcType             = functionType(paramTypeValue, bodyResult.expressionType)
    } yield TypedExpression(
      funcType,
      TypedExpression.FunctionLiteral(paramName, paramType.as(paramTypeValue), bodyStack.as(bodyResult))
    )

  /** Build from a body stack by extracting and processing the signature expression. */
  private def inferBodyStack(stack: Sourced[TypeStack[Expression]]): TypeGraphIO[TypedExpression] =
    inferBody(stack.as(stack.value.signature))

  /** Shared handling for parameter references in both type-level and body-level contexts. */
  private def handleParameterReference(name: Sourced[String]): TypeGraphIO[TypedExpression] =
    lookupParameter(name.value).map { maybeType =>
      val exprValue = maybeType.getOrElse(ParameterReference(name.value, Value.Type))
      TypedExpression(exprValue, TypedExpression.ParameterReference(name))
    }

  /** Recursively process type levels from top (highest) to bottom (signature).
    *
    * @param levels
    *   Remaining levels to process (from top to bottom)
    * @param expectedType
    *   The expected type for the current level's value
    * @param source
    *   Source location for error messages
    * @return
    *   Tuple of (signatureType, typedLevels in reverse order)
    */
  private def processLevels(
      levels: List[Expression],
      expectedType: Value,
      source: Sourced[?]
  ): TypeGraphIO[(ExpressionValue, Seq[TypedExpression])] =
    levels match {
      case Nil =>
        generateUnificationVar(source).map((_, Seq.empty))

      case head :: Nil =>
        buildExpression(head).map(typeResult => (typeResult.expressionType, Seq(typeResult)))

      case expr :: rest =>
        for {
          typeResult                       <- buildExpression(expr)
          evaluatedValue                   <- extractConcreteValue(typeResult, expectedType, source)
          (signatureType, restTypedLevels) <- processLevels(rest, evaluatedValue, source)
        } yield (signatureType, typeResult +: restTypedLevels)
    }

  /** Check if a type stack represents a kind annotation (for universal introductions). A kind annotation is:
    *   - Type (for simple type parameters)
    *   - Function(Type, Type) (for type constructors of arity 1)
    *   - Function(Type, Function(Type, Type)) (for type constructors of arity 2)
    *   - etc.
    */
  private def isKindAnnotation(stack: TypeStack[Expression]): Boolean =
    stack.levels.length == 1 && isKindExpression(stack.levels.head)

  private def isKindExpression(expr: Expression): Boolean =
    expr match {
      case Expr.ValueReference(vfqn)                       =>
        vfqn.value === typeFQN
      case Expr.FunctionApplication(targetStack, argStack) =>
        // Check if this is Function(<kind>, <kind>) - a function from kinds to kinds
        targetStack.value.signature match {
          case Expr.FunctionApplication(fnStack, argKindStack) =>
            isFunctionReference(fnStack.value.signature) &&
            isKindExpression(argKindStack.value.signature) &&
            isKindExpression(argStack.value.signature)
          case _                                               => false
        }
      case _                                               => false
    }

  private def isFunctionReference(expr: Expression): Boolean =
    expr match {
      case Expr.ValueReference(vfqn) => vfqn.value === functionDataTypeFQN
      case _                         => false
    }

  /** Build a typed expression from a single type expression. */
  private def buildExpression(expression: Expression): TypeGraphIO[TypedExpression] =
    expression match {
      case Expr.FunctionLiteral(paramName, paramType, body) if isKindAnnotation(paramType.value) =>
        buildUniversalIntro(paramName, paramType, body)

      case Expr.FunctionLiteral(paramName, paramType, body) =>
        buildFunctionType(paramName, paramType, body)

      case Expr.ValueReference(vfqn) =>
        buildValueReference(vfqn)

      case Expr.ParameterReference(name) =>
        handleParameterReference(name)

      case Expr.FunctionApplication(target, arg) =>
        buildTypeApplication(target, arg)

      case Expr.IntegerLiteral(value) =>
        val exprValue = ConcreteValue(Value.Direct(value.value, Types.bigIntType))
        TypedExpression(exprValue, TypedExpression.IntegerLiteral(value)).pure[TypeGraphIO]

      case Expr.StringLiteral(value) =>
        val exprValue = ConcreteValue(Value.Direct(value.value, Types.stringType))
        TypedExpression(exprValue, TypedExpression.StringLiteral(value)).pure[TypeGraphIO]
    }

  /** Universal variable introduction: A -> ... where A is of type Type
    *
    * In instantiation mode (when processing a referenced value's type), the type parameter becomes a fresh unification
    * variable instead of a universal variable. This allows type inference at call sites. In this mode, the body type is
    * returned directly (type params are instantiated).
    *
    * In declaration mode, the polymorphic type (FunctionLiteral wrapping body type) is preserved so that
    * MonomorphicTypeCheckProcessor can apply type arguments during monomorphization.
    */
  private def buildUniversalIntro(
      paramName: Sourced[String],
      paramType: Sourced[TypeStack[Expression]],
      body: Sourced[TypeStack[Expression]]
  ): TypeGraphIO[TypedExpression] =
    for {
      inInstMode                        <- isInstantiationMode
      _                                 <- if (inInstMode) {
                                             // Generate fresh unification var and bind it so later references find it
                                             generateUnificationVar(paramName).flatMap(uniVar => bindParameter(paramName.value, uniVar))
                                           } else {
                                             addUniversalVar(paramName.value)
                                           }
      (paramTypeValue, typedParamStack) <- processStack(paramType)
      (bodyTypeValue, typedBodyStack)   <- processStack(body)
      // In instantiation mode, return body type directly (type params are already instantiated as unification vars).
      // In declaration mode, wrap in FunctionLiteral to preserve polymorphic type for monomorphization.
      resultType                         = if (inInstMode) bodyTypeValue else FunctionLiteral(paramName.value, Value.Type, bodyTypeValue)
    } yield TypedExpression(
      resultType,
      TypedExpression.FunctionLiteral(paramName, paramType.as(paramTypeValue), body.as(typedBodyStack.value.signature))
    )

  /** Regular function literal (lambda type): (a: A) -> B becomes FunctionType */
  private def buildFunctionType(
      paramName: Sourced[String],
      paramType: Sourced[TypeStack[Expression]],
      body: Sourced[TypeStack[Expression]]
  ): TypeGraphIO[TypedExpression] =
    for {
      (paramTypeValue, typedParamStack) <- processStack(paramType)
      _                                 <- bindParameter(paramName.value, paramTypeValue)
      (bodyTypeValue, typedBodyStack)   <- processStack(body)
      funcType                           = functionType(paramTypeValue, bodyTypeValue)
    } yield TypedExpression(
      funcType,
      TypedExpression.FunctionLiteral(paramName, paramType.as(paramTypeValue), body.as(typedBodyStack.value.signature))
    )

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
      target: Sourced[TypeStack[Expression]],
      arg: Sourced[TypeStack[Expression]]
  ): TypeGraphIO[TypedExpression] =
    for {
      (targetTypeValue, typedTargetStack) <- processStack(target)
      (argTypeValue, typedArgStack)       <- processStack(arg)
      resultType                           = applyTypeApplication(targetTypeValue, argTypeValue)
    } yield TypedExpression(
      resultType,
      TypedExpression.FunctionApplication(
        target.as(typedTargetStack.value.signature),
        arg.as(typedArgStack.value.signature)
      )
    )

  /** Apply a type constructor to an argument. */
  private def applyTypeApplication(
      target: ExpressionValue,
      arg: ExpressionValue
  ): ExpressionValue =
    FunctionApplication(target, arg)

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
