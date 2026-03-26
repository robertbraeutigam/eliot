package com.vanillasource.eliot.eliotc.monomorphize2.processor

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
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
import com.vanillasource.eliot.eliotc.monomorphize2.processor.ConstraintSolver.solve
import com.vanillasource.eliot.eliotc.monomorphize2.processor.TypeCheckState.*
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
      endState             <- typeCheck(key, resolvedValue).runS(TypeCheckState())
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

  // --- Phase 1: Constraint generation ---

  private def typeCheck(
      key: MonomorphicValue.Key,
      resolvedValue: OperatorResolvedValue
  ): TypeGraphIO[Unit] =
    for {
      typeExprValue <- StateT.liftF(Evaluator.evaluate(resolvedValue.typeStack.map(_.signature)))
      typeParams     = ExpressionValue.extractLeadingLambdaParams(typeExprValue)
      typeArgSubst   = typeParams.zip(key.typeArguments).map { case ((name, _), value) => (name, value) }.toMap
      _             <- StateT.modify[CompilerIO, TypeCheckState](_.copy(typeArgSubstitution = typeArgSubst))
      signatureExpr  = instantiateTypeParams(typeExprValue, typeParams, key.typeArguments)
      _             <- resolvedValue.runtime match {
                         case Some(body) => typeCheckExpression(signatureExpr, body).void
                         case None       => StateT.pure[CompilerIO, TypeCheckState, Unit](())
                       }
    } yield ()

  private def instantiateTypeParams(
      typeExprValue: ExpressionValue,
      typeParams: Seq[(String, Value)],
      typeArguments: Seq[Value]
  ): ExpressionValue = {
    val body = ExpressionValue.stripLeadingLambdas(typeExprValue)
    typeParams.zip(typeArguments).foldLeft(body) { case (expr, ((paramName, _), argValue)) =>
      ExpressionValue.substitute(expr, paramName, ConcreteValue(argValue))
    }
  }

  private def typeCheckExpression(
      resultType: ExpressionValue,
      expression: Sourced[OperatorResolvedExpression]
  ): TypeGraphIO[ExpressionValue] =
    expression.value match {
      case OperatorResolvedExpression.IntegerLiteral(_)                              =>
        val exprType = ConcreteValue(bigIntType)
        tellConstraint(Constraints.constraint(resultType, expression.as(exprType), "Type mismatch.")) *>
          exprType.pure[TypeGraphIO]
      case OperatorResolvedExpression.StringLiteral(_)                               =>
        val exprType = ConcreteValue(stringType)
        tellConstraint(Constraints.constraint(resultType, expression.as(exprType), "Type mismatch.")) *>
          exprType.pure[TypeGraphIO]
      case OperatorResolvedExpression.ParameterReference(name)                       =>
        for {
          maybeType <- lookupParameter(name.value)
          exprType  <- maybeType match {
                         case Some(paramType) => paramType.value.pure[TypeGraphIO]
                         case None            =>
                           StateT.liftF(compilerAbort[ExpressionValue](name.as("Parameter not found.")))
                       }
          _         <- tellConstraint(Constraints.constraint(resultType, expression.as(exprType), "Type mismatch."))
        } yield exprType
      case OperatorResolvedExpression.ValueReference(vfqn, typeArgs)                 =>
        for {
          resolved        <- StateT.liftF(getFactOrAbort(OperatorResolvedValue.Key(vfqn.value)))
          rawValueType    <- StateT.liftF(Evaluator.evaluate(resolved.typeStack.map(_.signature)))
          typeParams       = ExpressionValue.extractLeadingLambdaParams(rawValueType)
          state           <- StateT.get[CompilerIO, TypeCheckState]
          evaluatedArgs   <-
            typeArgs.traverse(ta =>
              StateT.liftF(
                Evaluator
                  .evaluate(ta, paramContext = state.typeArgSubstitution.view.mapValues(_.valueType).toMap)
              )
            )
          instantiatedArgs = evaluatedArgs.map(substituteTypeArgs(_, state.typeArgSubstitution))
          valueType       <- instantiateValueType(rawValueType, typeParams, instantiatedArgs)
          _               <- recordValueRefType(vfqn, valueType)
          _               <- tellConstraint(
                               Constraints.constraint(resultType, vfqn.as(valueType), "Type mismatch.")
                             )
        } yield valueType
      case OperatorResolvedExpression.FunctionApplication(target, arg)               =>
        for {
          argTypeVar <- generateUnificationVar
          retTypeVar <- generateUnificationVar
          _          <- typeCheckExpression(ExpressionValue.functionType(argTypeVar, retTypeVar), target)
          _          <- typeCheckExpression(argTypeVar, arg)
          _          <- tellConstraint(
                          Constraints.constraint(resultType, expression.as(retTypeVar), "Type mismatch.")
                        )
        } yield retTypeVar
      case OperatorResolvedExpression.FunctionLiteral(paramName, paramTypeOpt, body) =>
        for {
          state          <- StateT.get[CompilerIO, TypeCheckState]
          typedParamType <- paramTypeOpt match {
                              case Some(paramTypeStack) =>
                                for {
                                  evaluated <- StateT.liftF(
                                                 Evaluator.evaluate(
                                                   paramTypeStack.map(_.signature),
                                                   paramContext =
                                                     state.typeArgSubstitution.view.mapValues(_.valueType).toMap
                                                 )
                                               )
                                } yield paramName.as(
                                  substituteTypeArgs(evaluated, state.typeArgSubstitution)
                                )
                              case None                 =>
                                generateUnificationVar.map(v => paramName.as(v: ExpressionValue))
                            }
          _              <- bindParameter(paramName.value, typedParamType)
          retTypeVar     <- generateUnificationVar
          _              <- typeCheckExpression(retTypeVar, body)
          funcType        = ExpressionValue.functionType(typedParamType.value, retTypeVar)
          _              <- tellConstraint(
                              Constraints.constraint(resultType, body.as(funcType), "Type mismatch.")
                            )
        } yield funcType
    }

  private def instantiateValueType(
      rawValueType: ExpressionValue,
      typeParams: Seq[(String, Value)],
      evaluatedArgs: Seq[ExpressionValue]
  ): TypeGraphIO[ExpressionValue] = {
    val body = ExpressionValue.stripLeadingLambdas(rawValueType)
    typeParams.zipWithIndex.foldLeftM(body) { case (current, ((paramName, _), idx)) =>
      if (idx < evaluatedArgs.size) {
        ExpressionValue.substitute(current, paramName, evaluatedArgs(idx)).pure[TypeGraphIO]
      } else {
        generateUnificationVar.map(v => ExpressionValue.substitute(current, paramName, v))
      }
    }
  }

  private def substituteTypeArgs(expr: ExpressionValue, subst: Map[String, Value]): ExpressionValue =
    subst.foldLeft(expr) { case (e, (name, value)) =>
      ExpressionValue.substitute(e, name, ConcreteValue(value))
    }

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
                         buildMonomorphicExpression(body.value, solution, endState, paramTypes, body).map(body.as)
                       }
    } yield (signature, runtime.map(_.map(_.expression)))

  private def resolveParameterTypes(endState: TypeCheckState, solution: Solution): Map[String, Value] =
    endState.parameterTypes.flatMap { case (name, sourced) =>
      val resolved = solution.resolveExpressionValue(sourced.value)
      ExpressionValue.concreteValueOf(resolved).map(name -> _)
    }

  private def buildMonomorphicExpression(
      expression: OperatorResolvedExpression,
      solution: Solution,
      endState: TypeCheckState,
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
          concreteTypeArgs <- resolveValueRefTypeArgs(vfqn, solution, endState)
          resolved         <- getFactOrAbort(OperatorResolvedValue.Key(vfqn.value))
          rawValueType     <- Evaluator.evaluate(resolved.typeStack.map(_.signature))
          valueType        <- Evaluator.applyTypeArgs(
                                ExpressionValue.stripLeadingLambdas(rawValueType),
                                concreteTypeArgs,
                                vfqn
                              )
        } yield MonomorphicExpression(
          valueType,
          MonomorphicExpression.MonomorphicValueReference(vfqn, concreteTypeArgs)
        )
      case OperatorResolvedExpression.FunctionApplication(target, arg)               =>
        for {
          targetExpr <- buildMonomorphicExpression(target.value, solution, endState, paramTypes, target)
          argExpr    <- buildMonomorphicExpression(arg.value, solution, endState, paramTypes, arg)
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
          bodyExpr <- buildMonomorphicExpression(body.value, solution, endState, paramTypes, body)
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

  /** Resolve the concrete type arguments for a value reference by matching the solved instantiated type against the raw
    * type pattern.
    */
  private def resolveValueRefTypeArgs(
      vfqn: Sourced[?],
      solution: Solution,
      endState: TypeCheckState
  ): CompilerIO[Seq[Value]] =
    endState.valueRefTypes.get((vfqn.uri, vfqn.range)) match {
      case Some(instantiatedType) =>
        val concreteExpr = solution.resolveExpressionValue(instantiatedType)
        for {
          resolved     <-
            getFactOrAbort(
              OperatorResolvedValue.Key(vfqn.value.asInstanceOf[com.vanillasource.eliot.eliotc.module.fact.ValueFQN])
            )
          rawValueType <- Evaluator.evaluate(resolved.typeStack.map(_.signature))
          typeParams    = ExpressionValue.extractLeadingLambdaParams(rawValueType)
          body          = ExpressionValue.stripLeadingLambdas(rawValueType)
          bindings      = ExpressionValue.matchTypes(body, concreteExpr)
        } yield typeParams.map { case (name, _) =>
          bindings.get(name).flatMap(ExpressionValue.concreteValueOf).getOrElse(Value.Type)
        }
      case None                   =>
        Seq.empty.pure[CompilerIO]
    }
}
