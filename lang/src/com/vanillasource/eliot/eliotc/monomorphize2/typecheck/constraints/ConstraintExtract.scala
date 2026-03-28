package com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.ConcreteValue
import com.vanillasource.eliot.eliotc.eval.fact.Types.{bigIntType, stringType, typeFQN}
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Value}
import com.vanillasource.eliot.eliotc.eval.util.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize2.fact.MonomorphicValue
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.{CompilerIO, getFactOrAbort}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort
import TypeCheckState.*
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.eval.util.Evaluator.evaluate
import com.vanillasource.eliot.eliotc.feedback.Logging

object ConstraintExtract extends Logging {
  def collectConstraints(key: MonomorphicValue.Key, resolvedValue: OperatorResolvedValue): TypeGraphIO[Unit] = {
    val typeExpressions = resolvedValue.typeStack.value.levels.map(resolvedValue.typeStack.as(_)).reverse

    for {
      _           <- debug[TypeGraphIO]("Type checking top...")
      // Iterate all levels, where each level computes the type of the underlying level, starting at the top with Type
      // The output is the evaluated signature (the last level)
      signature   <- typeExpressions.foldLeftM[TypeGraphIO, ExpressionValue](ExpressionValue.ConcreteValue(Value.Type)) {
                       (assumedType, level) =>
                         inferType(assumedType, level) >> StateT.liftF(evaluate(level))
                     }
      typeParams   = ExpressionValue.extractLeadingLambdaParams(signature)
      body         = ExpressionValue.stripLeadingLambdas(signature)
      runtimeType <- typeParams.zip(key.typeArguments).foldLeftM[TypeGraphIO, ExpressionValue](body) {
                       case (currentBody, ((paramName, _), typeArg)) =>
                         for {
                           _      <- StateT.modify[CompilerIO, TypeCheckState](s =>
                                       s.copy(typeArgSubstitution = s.typeArgSubstitution + (paramName -> typeArg))
                                     )
                           uniVar <- generateUnificationVar
                           _      <- tellConstraint(
                                       Constraints.constraint(
                                         uniVar,
                                         ExpressionValue.unsourced(ConcreteValue(typeArg)),
                                         "Type argument mismatch."
                                       )
                                     )
                         } yield ExpressionValue.substitute(currentBody, paramName, uniVar)
                     }
      _           <- resolvedValue.runtime.traverse_(inferType(runtimeType, _))
    } yield ()
  }

  private def inferType(
      assumedType: ExpressionValue,
      expression: Sourced[OperatorResolvedExpression]
  ): TypeGraphIO[Unit] =
    expression.value match {
      case OperatorResolvedExpression.IntegerLiteral(_)                              =>
        tellConstraint(Constraints.constraint(assumedType, expression.as(ConcreteValue(bigIntType)), "Type mismatch."))
      case OperatorResolvedExpression.StringLiteral(_)                               =>
        tellConstraint(Constraints.constraint(assumedType, expression.as(ConcreteValue(stringType)), "Type mismatch."))
      case OperatorResolvedExpression.ParameterReference(name)                       =>
        for {
          maybeType <- lookupParameter(name.value)
          exprType  <- maybeType match {
                         case Some(paramType) => paramType.value.pure[TypeGraphIO]
                         case None            =>
                           StateT.liftF(compilerAbort[ExpressionValue](name.as("Parameter not found.")))
                       }
          _         <- tellConstraint(Constraints.constraint(assumedType, expression.as(exprType), "Type mismatch."))
        } yield ()
      case OperatorResolvedExpression.ValueReference(vfqn, typeArgs)                 =>
        for {
          resolved        <- StateT.liftF(getFactOrAbort(OperatorResolvedValue.Key(vfqn.value)))
          rawValueType    <- StateT.liftF(Evaluator.evaluate(resolved.typeStack.map(_.signature)))
          state           <- StateT.get[CompilerIO, TypeCheckState]
          evaluatedArgs   <-
            typeArgs.traverse(ta =>
              StateT.liftF(
                Evaluator
                  .evaluate(ta, paramContext = state.typeArgSubstitution.view.mapValues(_.valueType).toMap)
              )
            )
          instantiatedArgs = evaluatedArgs.map(substituteTypeArgs(_, state.typeArgSubstitution))
          valueType       <- instantiateValueType(rawValueType, instantiatedArgs)
          _               <- recordValueRefType(vfqn, valueType)
          _               <- tellConstraint(
                               Constraints.constraint(assumedType, vfqn.as(valueType), "Type mismatch.")
                             )
        } yield ()
      case OperatorResolvedExpression.FunctionApplication(target, arg)               =>
        for {
          argTypeVar <- generateUnificationVar
          retTypeVar <- generateUnificationVar
          _          <- inferType(ExpressionValue.functionType(argTypeVar, retTypeVar), target)
          _          <- inferType(argTypeVar, arg)
          _          <- tellConstraint(
                          Constraints.constraint(assumedType, expression.as(retTypeVar), "Type mismatch.")
                        )
        } yield ()
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
          _              <- inferType(retTypeVar, body)
          funcType        = ExpressionValue.functionType(typedParamType.value, retTypeVar)
          _              <- tellConstraint(
                              Constraints.constraint(assumedType, body.as(funcType), "Type mismatch.")
                            )
        } yield ()
    }

  private def instantiateValueType(
      rawValueType: ExpressionValue,
      evaluatedArgs: Seq[ExpressionValue]
  ): TypeGraphIO[ExpressionValue] = {
    val applied = ExpressionValue.betaReduce(
      evaluatedArgs.foldLeft(rawValueType) { (expr, arg) =>
      ExpressionValue.FunctionApplication(ExpressionValue.unsourced(expr), ExpressionValue.unsourced(arg))
    }
    )
    fillRemainingTypeParams(applied)
  }

  private def fillRemainingTypeParams(expr: ExpressionValue): TypeGraphIO[ExpressionValue] =
    expr match {
      case ExpressionValue.FunctionLiteral(_, Value.Type, _) =>
        generateUnificationVar.flatMap(v =>
        fillRemainingTypeParams(
          ExpressionValue.betaReduce(
            ExpressionValue.FunctionApplication(ExpressionValue.unsourced(expr), ExpressionValue.unsourced(v))
          )
        )
      )
      case _                                                 => expr.pure[TypeGraphIO]
    }

  private def substituteTypeArgs(expr: ExpressionValue, subst: Map[String, Value]): ExpressionValue =
    subst.foldLeft(expr) { case (e, (name, value)) =>
      ExpressionValue.substitute(e, name, ConcreteValue(value))
    }

}
