package com.vanillasource.eliot.eliotc.monomorphize2.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.Types.{
  bigIntType,
  fullyQualifiedNameType,
  functionDataTypeFQN,
  stringType
}
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Value}
import com.vanillasource.eliot.eliotc.eval.util.Evaluator
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.monomorphize2.fact.*
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints.ConstraintExtract.collectConstraints
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints.{Constraints, TypeCheckState}
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.solution.ConstraintSolver.solve
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.solution.Solution
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

class MonomorphicTypeCheckProcessor
    extends TransformationProcessor[OperatorResolvedValue.Key, MonomorphicValue.Key](key =>
      OperatorResolvedValue.Key(key.vfqn)
    )
    with Logging {

  override protected def generateFromKeyAndFact(
      key: MonomorphicValue.Key,
      resolvedValue: OperatorResolvedValue
  ): CompilerIO[MonomorphicValue] =
    for {
      endState             <- collectConstraints(key, resolvedValue).runS(TypeCheckState())
      _                    <- debug[CompilerIO](s"Constraints (of ${key.vfqn.show}): ${endState.constraints.show}")
      solution             <- solve(endState.constraints)
      _                    <- debug[CompilerIO](s"Solution (of ${key.vfqn.show}): ${solution.show}")
      (signature, runtime) <- typeSubstitute(key, solution, endState, resolvedValue)
    } yield MonomorphicValue(
      key.vfqn,
      key.typeArguments,
      signature,
      runtime
    )

  // --- Phase 3: Type substitution and MonomorphicExpression building ---

  private def typeSubstitute(
      key: MonomorphicValue.Key,
      solution: Solution,
      endState: TypeCheckState,
      resolvedValue: OperatorResolvedValue
  ): CompilerIO[(Value, Option[Sourced[MonomorphicExpression.Expression]])] =
    for {
      typeExprValue <- Evaluator.evaluate(resolvedValue.typeStack.map(_.signature))
      signature     <- Evaluator.applyTypeArgs(
                         ExpressionValue.stripLeadingLambdas(typeExprValue),
                         key.typeArguments,
                         resolvedValue.name
                       )
      paramTypes     = resolveParameterTypes(endState, solution)
      runtime       <- resolvedValue.runtime.traverse { body =>
                         buildMonomorphicExpression(body.value, paramTypes, body).map(body.as)
                       }
    } yield (signature, runtime.map(_.map(_.expression)))

  private def resolveParameterTypes(endState: TypeCheckState, solution: Solution): Map[String, Value] =
    endState.parameterTypes.flatMap { case (name, sourced) =>
      val resolved = solution.resolveExpressionValue(sourced.value)
      ExpressionValue.concreteValueOf(resolved).map(name -> _)
    }

  private def buildMonomorphicExpression(
      expression: OperatorResolvedExpression,
      paramTypes: Map[String, Value],
      source: Sourced[?]
  ): CompilerIO[MonomorphicExpression] =
    expression match {
      case OperatorResolvedExpression.IntegerLiteral(value)                          =>
        MonomorphicExpression(bigIntType, MonomorphicExpression.IntegerLiteral(value)).pure[CompilerIO]
      case OperatorResolvedExpression.StringLiteral(value)                           =>
        MonomorphicExpression(stringType, MonomorphicExpression.StringLiteral(value)).pure[CompilerIO]
      case OperatorResolvedExpression.ParameterReference(name)                       =>
        paramTypes.get(name.value) match {
          case Some(paramType) =>
            MonomorphicExpression(paramType, MonomorphicExpression.ParameterReference(name)).pure[CompilerIO]
          case None            =>
            compilerAbort(name.as("Could not determine concrete type for parameter."))
        }
      case OperatorResolvedExpression.ValueReference(vfqn, typeArgs)                 =>
        for {
          resolved     <- getFactOrAbort(OperatorResolvedValue.Key(vfqn.value))
          rawValueType <- Evaluator.evaluate(resolved.typeStack.map(_.signature))
          valueType    <- Evaluator.applyTypeArgs(
                            ExpressionValue.stripLeadingLambdas(rawValueType),
                            Seq.empty,
                            vfqn
                          )
        } yield MonomorphicExpression(
          valueType,
          MonomorphicExpression.MonomorphicValueReference(vfqn, Seq.empty)
        )
      case OperatorResolvedExpression.FunctionApplication(target, arg)               =>
        for {
          targetExpr <- buildMonomorphicExpression(target.value, paramTypes, target)
          argExpr    <- buildMonomorphicExpression(arg.value, paramTypes, arg)
          returnType  = targetExpr.expressionType.asFunctionType match {
                          case Some((_, ret)) => ret
                          case None           => Value.Type
                        }
        } yield MonomorphicExpression(
          returnType,
          MonomorphicExpression.FunctionApplication(target.as(targetExpr), arg.as(argExpr))
        )
      case OperatorResolvedExpression.FunctionLiteral(paramName, paramTypeOpt, body) =>
        val paramType = paramTypes.getOrElse(paramName.value, Value.Type)
        for {
          bodyExpr <- buildMonomorphicExpression(body.value, paramTypes, body)
          funcType  = Value.Structure(
                        Map(
                          "$typeName" -> Value.Direct(functionDataTypeFQN, fullyQualifiedNameType),
                          "A"         -> paramType,
                          "B"         -> bodyExpr.expressionType
                        ),
                        Value.Type
                      )
        } yield MonomorphicExpression(
          funcType,
          MonomorphicExpression.FunctionLiteral(paramName, paramType, body.as(bodyExpr))
        )
    }

}
