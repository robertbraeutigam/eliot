package com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.ConcreteValue
import com.vanillasource.eliot.eliotc.eval.fact.Types.{bigIntType, stringType}
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Value}
import com.vanillasource.eliot.eliotc.eval.util.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize2.fact.MonomorphicValue
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.{CompilerIO, getFactOrAbort}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort
import TypeCheckState._

object ConstraintExtract {
  def extractConstraints(
      key: MonomorphicValue.Key,
      resolvedValue: OperatorResolvedValue
  ): TypeGraphIO[Unit] =
    for {
      typeExprValue <- StateT.liftF(Evaluator.evaluate(resolvedValue.typeStack.map(_.signature)))
      typeParams     = ExpressionValue.extractLeadingLambdaParams(typeExprValue)
      typeArgSubst   = typeParams.zip(key.typeArguments).map { case ((name, _), value) => (name, value) }.toMap
      _             <- StateT.modify[CompilerIO, TypeCheckState](_.copy(typeArgSubstitution = typeArgSubst))
      signatureExpr  = instantiateTypeParams(typeExprValue, key.typeArguments)
      _             <- resolvedValue.runtime match {
                         case Some(body) => typeCheckExpression(signatureExpr, body).void
                         case None       => StateT.pure[CompilerIO, TypeCheckState, Unit](())
                       }
    } yield ()

  private def instantiateTypeParams(
      typeExprValue: ExpressionValue,
      typeArguments: Seq[Value]
  ): ExpressionValue = ExpressionValue.betaReduce(
    typeArguments.foldLeft(typeExprValue) { (expr, argValue) =>
      expr.apply(ConcreteValue(argValue))
    }
  )

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
  ): TypeGraphIO[ExpressionValue] =
    for {
      extraVars <- (evaluatedArgs.size until typeParams.size).toList.traverse(_ => generateUnificationVar)
      allArgs    = evaluatedArgs ++ extraVars
      applied    = allArgs.foldLeft(rawValueType) { (expr, arg) => expr(arg) }
    } yield ExpressionValue.betaReduce(applied)

  private def substituteTypeArgs(expr: ExpressionValue, subst: Map[String, Value]): ExpressionValue =
    subst.foldLeft(expr) { case (e, (name, value)) =>
      ExpressionValue.substitute(e, name, ConcreteValue(value))
    }

}
