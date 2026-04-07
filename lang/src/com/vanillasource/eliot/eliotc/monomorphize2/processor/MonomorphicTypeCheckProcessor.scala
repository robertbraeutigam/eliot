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
      _                    <-
        debug[CompilerIO](
          s"Type checking ${key.vfqn.show}, with type arguments: ${key.specifiedTypeArguments.map(_.show).mkString(", ")}"
        )
      endState             <- collectConstraints(key, resolvedValue).runS(TypeCheckState())
      _                    <- debug[CompilerIO](s"Constraints (of ${key.vfqn.show}): ${endState.constraints.show}")
      solution             <- solve(endState.constraints, endState.shortIds)
      _                    <- debug[CompilerIO](s"Solution (of ${key.vfqn.show}): ${solution.show}")
      (signature, runtime) <- typeSubstitute(key, solution, endState, resolvedValue)
    } yield MonomorphicValue(
      key.vfqn,
      key.specifiedTypeArguments,
      Seq.empty, // TODO: left out for now
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
      typeArgValues <- key.specifiedTypeArguments.toList.traverse { arg =>
                         Evaluator.evaluate(arg).flatMap { ev =>
                           ExpressionValue.concreteValueOf(ev) match {
                             case Some(v) => v.pure[CompilerIO]
                             case None    =>
                               compilerAbort(arg.as("Type argument did not evaluate to concrete value."))
                           }
                         }
                       }
      typeExprValue <- Evaluator.evaluate(resolvedValue.typeStack.map(_.signature))
      signature     <- Evaluator.applyTypeArgs(typeExprValue, typeArgValues, resolvedValue.name)
      paramTypes    <- resolveParameterTypes(endState, solution)
      runtime       <- resolvedValue.runtime.traverse { body =>
                         buildMonomorphicExpression(body.value, paramTypes, endState, solution, body).map(body.as)
                       }
    } yield (signature, runtime.map(_.map(_.expression)))

  private def resolveParameterTypes(
      endState: TypeCheckState,
      solution: Solution
  ): CompilerIO[Map[String, Value]] =
    endState.parameterTypes.toList
      .traverse { case (name, sourced) =>
        Evaluator.evaluate(sourced).map { ev =>
          ExpressionValue.concreteValueOf(solution.resolveExpressionValue(ev)).map(name -> _)
        }
      }
      .map(_.flatten.toMap)

  private def buildMonomorphicExpression(
      expression: OperatorResolvedExpression,
      paramTypes: Map[String, Value],
      endState: TypeCheckState,
      solution: Solution,
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
          resolved  <- getFactOrAbort(OperatorResolvedValue.Key(vfqn.value))
          rawSig    <- Evaluator.evaluate(resolved.typeStack.map(_.signature))
          // For a standalone reference (not inside an application) the signature must already evaluate to a concrete
          // value. Generic uses are handled by the FunctionApplication case below, which feeds the argument's type
          // back into the evaluator.
          valueType <- ExpressionValue.concreteValueOf(rawSig) match {
                         case Some(v) => v.pure[CompilerIO]
                         case None    =>
                           compilerAbort(vfqn.as("Could not determine concrete type for value reference."))
                       }
        } yield MonomorphicExpression(
          valueType,
          MonomorphicExpression.MonomorphicValueReference(vfqn, Seq.empty)
        )
      case OperatorResolvedExpression.FunctionApplication(target, arg)               =>
        for {
          argExpr    <- buildMonomorphicExpression(arg.value, paramTypes, endState, solution, arg)
          targetExpr <- buildApplicationTarget(target, argExpr.expressionType, paramTypes, endState, solution)
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
          bodyExpr <- buildMonomorphicExpression(body.value, paramTypes, endState, solution, body)
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

  /** Build the target of a function application. If the target is a value reference whose evaluated signature is a
    * function abstraction (a generic), feed the actual argument's type into [[Evaluator.applyTypeArgs]] so the
    * abstraction is instantiated. We never pattern-match the ORE; we ask the evaluator to "run" the signature and act
    * on what comes back.
    */
  private def buildApplicationTarget(
      target: Sourced[OperatorResolvedExpression],
      argType: Value,
      paramTypes: Map[String, Value],
      endState: TypeCheckState,
      solution: Solution
  ): CompilerIO[MonomorphicExpression] = target.value match {
    case OperatorResolvedExpression.ValueReference(vfqn, _) =>
      for {
        resolved  <- getFactOrAbort(OperatorResolvedValue.Key(vfqn.value))
        rawSig    <- Evaluator.evaluate(resolved.typeStack.map(_.signature))
        valueType <- rawSig match {
                       case _: ExpressionValue.FunctionLiteral =>
                         // Generic: instantiate by applying the argument's type as a type argument.
                         Evaluator.applyTypeArgs(rawSig, Seq(argType), vfqn)
                       case ExpressionValue.ConcreteValue(v)   =>
                         v.pure[CompilerIO]
                       case other                              =>
                         compilerAbort(vfqn.as(s"Could not determine target type from: ${other.show}"))
                     }
      } yield MonomorphicExpression(
        valueType,
        MonomorphicExpression.MonomorphicValueReference(vfqn, Seq.empty)
      )
    case _                                                  =>
      buildMonomorphicExpression(target.value, paramTypes, endState, solution, target)
  }
}
