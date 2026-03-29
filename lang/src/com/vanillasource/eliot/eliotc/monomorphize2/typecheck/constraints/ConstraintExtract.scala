package com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.ConcreteValue
import com.vanillasource.eliot.eliotc.eval.fact.Types.{bigIntType, stringType, typeFQN}
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Value}
import com.vanillasource.eliot.eliotc.eval.util.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize2.fact.MonomorphicValue
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.getFactOrAbort
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
      _           <- debug[TypeGraphIO]("Collecting constraints from higher levels...")
      // Iterate levels above signature, where each level computes the type of the underlying level
      kindType    <- aboveLevels.foldLeftM[TypeGraphIO, ExpressionValue](ExpressionValue.ConcreteValue(Value.Type)) {
                       (assumedType, level) =>
                         inferType(assumedType, level).void >> StateT.liftF(evaluate(level))
                     }
      // Infer the signature level, with adding supplied type arguments (this differs from above)
      _           <-
        debug[TypeGraphIO](
          s"Collecting constraints from signature, kind: ${kindType.show}, signature: ${signatureLevel.value.show}, type arguments: ${key.typeArguments.map(_.show).mkString(", ")}"
        )
      _           <- inferType(kindType, signatureLevel, key.typeArguments.map(ConcreteValue(_)))
      runtimeType <- StateT.liftF(evaluate(signatureLevel))
      // Handle runtime level, if available
      _           <- debug[TypeGraphIO](s"Collecting constraints from runtime, signature: ${runtimeType.show}")
      _           <- resolvedValue.runtime.traverse_(inferType(runtimeType, _).void)
    } yield ()
  }

  private def inferType(
      assumedType: ExpressionValue,
      expression: Sourced[OperatorResolvedExpression],
      typeArguments: Seq[ExpressionValue] = Seq.empty
  ): TypeGraphIO[ExpressionValue] =
    expression.value match {
      case OperatorResolvedExpression.IntegerLiteral(_)                                 =>
        val exprType = ConcreteValue(bigIntType)
        checkNoTypeArgs(expression, typeArguments) >>
          tellConstraint(Constraints.constraint(assumedType, expression.as(exprType), "Type mismatch."))
            .as(exprType)
      case OperatorResolvedExpression.StringLiteral(_)                                  =>
        val exprType = ConcreteValue(stringType)
        checkNoTypeArgs(expression, typeArguments) >>
          tellConstraint(Constraints.constraint(assumedType, expression.as(exprType), "Type mismatch."))
            .as(exprType)
      case OperatorResolvedExpression.ParameterReference(name)                          =>
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
      case OperatorResolvedExpression.ValueReference(vfqn, _) if vfqn.value === typeFQN =>
        checkNoTypeArgs(expression, typeArguments) >>
          ConcreteValue(Value.Type).pure[TypeGraphIO]
      case OperatorResolvedExpression.ValueReference(vfqn, typeArgs)                    =>
        for {
          _             <- checkNoTypeArgs(expression, typeArguments)
          resolved      <- StateT.liftF(getFactOrAbort(OperatorResolvedValue.Key(vfqn.value)))
          sigExpr        = resolved.typeStack.map(_.signature)
          evaluatedArgs <- typeArgs.traverse(ta => StateT.liftF(Evaluator.evaluate(ta)))
          valueType     <- inferType(assumedType, sigExpr, evaluatedArgs)
        } yield valueType
      case OperatorResolvedExpression.FunctionApplication(target, arg)                  =>
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
      case OperatorResolvedExpression.FunctionLiteral(paramName, paramTypeOpt, body)    =>
        for {
          typedParamType <- paramTypeOpt match {
                              case Some(paramTypeStack) =>
                                StateT.liftF(Evaluator.evaluate(paramTypeStack.map(_.signature)))
                              case None                 =>
                                generateUnificationVar.map(v => v: ExpressionValue)
                            }
          _              <- typeArguments.headOption.traverse_ { typeArg =>
                              tellConstraint(
                                Constraints.constraint(
                                  typedParamType,
                                  ExpressionValue.unsourced(typeArg),
                                  "Type argument mismatch."
                                )
                              )
                            }
          _              <- bindParameter(paramName.value, paramName.as(typedParamType))
          retTypeVar     <- generateUnificationVar
          bodyType       <- inferType(retTypeVar, body, typeArguments.drop(1))
          funcType        = ExpressionValue.functionType(typedParamType, retTypeVar)
          _              <- tellConstraint(
                              Constraints.constraint(assumedType, body.as(funcType), "Type mismatch.")
                            )
        } yield if (typeArguments.nonEmpty) bodyType else funcType
    }

  private def checkNoTypeArgs(
      expression: Sourced[OperatorResolvedExpression],
      typeArguments: Seq[ExpressionValue]
  ): TypeGraphIO[Unit] =
    StateT
      .liftF(compilerAbort[Unit](expression.as("Unconsumed type arguments in non-lambda expression.")))
      .whenA(typeArguments.nonEmpty)

}
