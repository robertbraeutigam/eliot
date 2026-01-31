package com.vanillasource.eliot.eliotc.symbolic.processor

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.eval.util.Types
import com.vanillasource.eliot.eliotc.module2.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.resolve2.fact.Expression
import com.vanillasource.eliot.eliotc.resolve2.fact.Expression as Expr
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import com.vanillasource.eliot.eliotc.symbolic.fact.TypedExpression
import com.vanillasource.eliot.eliotc.symbolic.types.TypeCheckState.*

/** Processes type stacks and type-level expressions. Validates type levels top-down where each level's value must have
  * valueType matching the level above (with implicit Type at top).
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
    } yield (signatureType, stack.as(TypeStack(typedLevels.reverse)))

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
      case Nil          =>
        generateUnificationVar(source).map((_, Seq.empty))

      case head :: Nil  =>
        buildExpression(head).map(typeResult => (typeResult.expressionType, Seq(typeResult)))

      case expr :: rest =>
        for {
          typeResult                       <- buildExpression(expr)
          evaluatedValue                   <- extractConcreteValue(typeResult, expectedType, source)
          (signatureType, restTypedLevels) <- processLevels(rest, evaluatedValue, source)
        } yield (signatureType, typeResult +: restTypedLevels)
    }

  /** Build a typed expression from a single type expression. */
  private def buildExpression(expression: Expression): TypeGraphIO[TypedExpression] =
    expression match {
      case Expr.FunctionLiteral(paramName, paramType, body) if paramType.value.levels.isEmpty =>
        buildUniversalIntro(paramName, paramType, body)

      case Expr.FunctionLiteral(paramName, paramType, body) =>
        buildFunctionType(paramName, paramType, body)

      case Expr.ValueReference(vfqn) =>
        buildValueReference(vfqn)

      case Expr.ParameterReference(name) =>
        lookupParameter(name.value).map { maybeType =>
          val exprValue = maybeType.getOrElse(ParameterReference(name.value, Value.Type))
          TypedExpression(exprValue, TypedExpression.ParameterReference(name))
        }

      case Expr.FunctionApplication(target, arg) =>
        buildTypeApplication(target, arg)

      case Expr.IntegerLiteral(value) =>
        val exprValue = ConcreteValue(Value.Direct(value.value, Types.bigIntType))
        TypedExpression(exprValue, TypedExpression.IntegerLiteral(value)).pure[TypeGraphIO]

      case Expr.StringLiteral(value) =>
        val exprValue = ConcreteValue(Value.Direct(value.value, Types.stringType))
        TypedExpression(exprValue, TypedExpression.StringLiteral(value)).pure[TypeGraphIO]
    }

  /** Universal variable introduction: A -> ... where A has empty type */
  private def buildUniversalIntro(
      paramName: Sourced[String],
      paramType: Sourced[TypeStack[Expression]],
      body: Sourced[TypeStack[Expression]]
  ): TypeGraphIO[TypedExpression] =
    for {
      _                               <- addUniversalVar(paramName.value)
      (bodyTypeValue, typedBodyStack) <- processStack(body)
      typedParamType                   = paramType.as(TypeStack[TypedExpression](Seq.empty))
    } yield TypedExpression(
      bodyTypeValue,
      TypedExpression.FunctionLiteral(paramName, typedParamType, typedBodyStack)
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
    } yield TypedExpression(funcType, TypedExpression.FunctionLiteral(paramName, typedParamStack, typedBodyStack))

  /** Value reference - could be a type like Int, String, or a universal var */
  private def buildValueReference(
      vfqn: Sourced[ValueFQN]
  ): TypeGraphIO[TypedExpression] =
    isUniversalVar(vfqn.value.name).map { isUniv =>
      val exprValue =
        if (isUniv) ParameterReference(vfqn.value.name, Value.Type)
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
    } yield TypedExpression(resultType, TypedExpression.FunctionApplication(typedTargetStack, typedArgStack))

  /** Apply a type constructor to an argument. Handles Function type specially. */
  private def applyTypeApplication(
      target: ExpressionValue,
      arg: ExpressionValue
  ): ExpressionValue =
    target match {
      case FunctionApplication(ConcreteValue(v), paramType) if isFunctionType(v) =>
        functionType(paramType, arg)
      case _                                                                     =>
        FunctionApplication(target, arg)
    }

  private def isFunctionType(v: Value): Boolean =
    v match {
      case Value.Structure(fields, _) =>
        fields.get("$typeName") match {
          case Some(Value.Direct(vfqn: ValueFQN, _)) =>
            vfqn.moduleName === ModuleName.systemFunctionModuleName && vfqn.name === "Function$DataType"
          case _                                     => false
        }
      case _                          => false
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
}
