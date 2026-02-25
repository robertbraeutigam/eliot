package com.vanillasource.eliot.eliotc.symbolic.processor

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Types}
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.Types.typeFQN
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.operator.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.operator.OperatorResolvedExpression as Expr
import com.vanillasource.eliot.eliotc.operator.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import com.vanillasource.eliot.eliotc.symbolic.fact.TypedExpression
import com.vanillasource.eliot.eliotc.symbolic.types.SymbolicUnification
import com.vanillasource.eliot.eliotc.symbolic.types.TypeCheckState.*

/** Infers types of runtime body expressions by generating unification variables and emitting constraints.
  *
  * Depends on TypeExpressionEvaluator for evaluating type annotations encountered in the body (parameter types, explicit
  * type arguments). TypeExpressionEvaluator has no dependency on BodyTypeInferrer.
  */
object BodyTypeInferrer {

  /** Build constraints from a body expression, inferring types. */
  def inferBody(body: Sourced[OperatorResolvedExpression]): TypeGraphIO[TypedExpression] =
    body.value match {
      case Expr.IntegerLiteral(value)                            =>
        inferLiteral(value, "Number", QualifiedName("Int", Qualifier.Type), TypedExpression.IntegerLiteral(value))
      case Expr.StringLiteral(value)                             =>
        inferLiteral(value, "String", QualifiedName("String", Qualifier.Type), TypedExpression.StringLiteral(value))
      case Expr.ParameterReference(name)                         =>
        TypeExpressionEvaluator.handleParameterReference(name)
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
        resolved          <- StateT.liftF(getFactOrAbort(OperatorResolvedValue.Key(vfqn.value)))
        evaluatedTypeArgs <- typeArgs.traverse(arg => TypeExpressionEvaluator.evaluateTypeExpression(arg.value).map(_.expressionType))
        _                 <- setExplicitTypeArgCount(evaluatedTypeArgs.length)
        (signatureType, _) <- TypeExpressionEvaluator.processStackForInstantiation(resolved.typeStack, evaluatedTypeArgs)
        remaining         <- getExplicitTypeArgCount
        _                 <- if (remaining > 0)
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
      paramType: Option[Sourced[TypeStack[OperatorResolvedExpression]]],
      bodyStack: Sourced[TypeStack[OperatorResolvedExpression]]
  ): TypeGraphIO[TypedExpression] =
    for {
      typedParamType <- paramType match {
                          case Some(pt) => TypeExpressionEvaluator.processStackForDeclaration(pt).map { case (v, _) => pt.as(v) }
                          case None     => generateUnificationVar.map(v => paramName.as(v: ExpressionValue))
                        }
      _              <- bindParameter(paramName.value, typedParamType.value)
      bodyResult     <- inferBodyStack(bodyStack)
      funcType        = functionType(typedParamType.value, bodyResult.expressionType)
    } yield TypedExpression(
      funcType,
      TypedExpression.FunctionLiteral(paramName, typedParamType, bodyStack.as(bodyResult))
    )

  /** Build from a body stack by extracting and processing the signature expression. */
  private def inferBodyStack(stack: Sourced[TypeStack[OperatorResolvedExpression]]): TypeGraphIO[TypedExpression] =
    inferBody(stack.as(stack.value.signature))
}
