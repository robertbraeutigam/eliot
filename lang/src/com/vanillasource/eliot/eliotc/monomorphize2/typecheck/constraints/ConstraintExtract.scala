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
import TypeCheckState.*
import com.vanillasource.eliot.eliotc.eval.util.Evaluator.evaluate
import com.vanillasource.eliot.eliotc.feedback.Logging

object ConstraintExtract extends Logging {
  def collectConstraints(key: MonomorphicValue.Key, resolvedValue: OperatorResolvedValue): TypeGraphIO[Unit] = {
    val typeExpressions = resolvedValue.typeStack.value.levels.map(resolvedValue.typeStack.as(_)).reverse
    val aboveLevels     = typeExpressions.init
    val signatureLevel  = typeExpressions.last

    for {
      _           <- debug[TypeGraphIO]("Type checking top...")
      // Iterate levels above signature, where each level computes the type of the underlying level
      kindType    <- aboveLevels.foldLeftM[TypeGraphIO, ExpressionValue](ExpressionValue.ConcreteValue(Value.Type)) {
                       (assumedType, level) =>
                         inferType(assumedType, level).void >> StateT.liftF(evaluate(level))
                     }
      // Infer the signature level, consuming type arguments through leading lambdas
      runtimeType <- inferType(kindType, signatureLevel, key.typeArguments)
      // Handle runtime level, if available
      _           <- resolvedValue.runtime.traverse_(inferType(runtimeType, _).void)
    } yield ()
  }

  private def inferType(
      assumedType: ExpressionValue,
      expression: Sourced[OperatorResolvedExpression],
      typeArguments: Seq[Value] = Seq.empty
  ): TypeGraphIO[ExpressionValue] =
    expression.value match {
      case OperatorResolvedExpression.IntegerLiteral(_)                              =>
        val exprType = ConcreteValue(bigIntType)
        checkNoTypeArgs(expression, typeArguments) >>
          tellConstraint(Constraints.constraint(assumedType, expression.as(exprType), "Type mismatch."))
            .as(exprType)
      case OperatorResolvedExpression.StringLiteral(_)                               =>
        val exprType = ConcreteValue(stringType)
        checkNoTypeArgs(expression, typeArguments) >>
          tellConstraint(Constraints.constraint(assumedType, expression.as(exprType), "Type mismatch."))
            .as(exprType)
      case OperatorResolvedExpression.ParameterReference(name)                       =>
        for {
          _         <- checkNoTypeArgs(expression, typeArguments)
          maybeType <- lookupParameter(name.value)
          exprType  <- maybeType match {
                         case Some(paramType) => paramType.value.pure[TypeGraphIO]
                         case None            =>
                           StateT.liftF(compilerAbort[ExpressionValue](name.as("Parameter not found.")))
                       }
          _         <- tellConstraint(Constraints.constraint(assumedType, expression.as(exprType), "Type mismatch."))
        } yield exprType
      case OperatorResolvedExpression.ValueReference(vfqn, typeArgs)                 =>
        for {
          _               <- checkNoTypeArgs(expression, typeArguments)
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
        } yield valueType
      case OperatorResolvedExpression.FunctionApplication(target, arg)               =>
        for {
          _          <- checkNoTypeArgs(expression, typeArguments)
          argTypeVar <- generateUnificationVar
          retTypeVar <- generateUnificationVar
          _          <- inferType(ExpressionValue.functionType(argTypeVar, retTypeVar), target)
          _          <- inferType(argTypeVar, arg)
          _          <- tellConstraint(
                          Constraints.constraint(assumedType, expression.as(retTypeVar), "Type mismatch.")
                        )
        } yield retTypeVar
      case OperatorResolvedExpression.FunctionLiteral(paramName, paramTypeOpt, body) =>
        typeArguments match {
          case typeArg +: remainingTypeArgs =>
            for {
              _              <- StateT.modify[CompilerIO, TypeCheckState](s =>
                                  s.copy(typeArgSubstitution = s.typeArgSubstitution + (paramName.value -> typeArg))
                                )
              uniVar         <- generateUnificationVar
              _              <- tellConstraint(
                                  Constraints.constraint(
                                    uniVar,
                                    ExpressionValue.unsourced(ConcreteValue(typeArg)),
                                    "Type argument mismatch."
                                  )
                                )
              _              <- bindParameter(paramName.value, paramName.as(uniVar: ExpressionValue))
              retTypeVar     <- generateUnificationVar
              bodyType       <- inferType(retTypeVar, body, remainingTypeArgs)
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
                                    } yield substituteTypeArgs(evaluated, state.typeArgSubstitution)
                                  case None                 => (uniVar: ExpressionValue).pure[TypeGraphIO]
                                }
              funcType        = ExpressionValue.functionType(typedParamType, retTypeVar)
              _              <- tellConstraint(
                                  Constraints.constraint(assumedType, body.as(funcType), "Type mismatch.")
                                )
            } yield bodyType
          case _                            =>
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
            } yield funcType
        }
    }

  private def checkNoTypeArgs(
      expression: Sourced[OperatorResolvedExpression],
      typeArguments: Seq[Value]
  ): TypeGraphIO[Unit] =
    if (typeArguments.nonEmpty)
      StateT.liftF(compilerAbort[Unit](expression.as("Unconsumed type arguments in non-lambda expression.")))
    else ().pure[TypeGraphIO]

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
