package com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.{ConcreteValue, ParameterReference}
import com.vanillasource.eliot.eliotc.eval.fact.Types.{bigIntType, stringType, typeFQN}
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Value}
import com.vanillasource.eliot.eliotc.eval.util.Evaluator
import com.vanillasource.eliot.eliotc.monomorphize2.fact.MonomorphicValue
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.{getFact, getFactOrAbort}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort
import TypeCheckState.*
import com.vanillasource.eliot.eliotc.eval.fact.Value.Direct
import com.vanillasource.eliot.eliotc.feedback.Logging

object ConstraintExtract extends Logging {
  def collectConstraints(key: MonomorphicValue.Key, resolvedValue: OperatorResolvedValue): TypeGraphIO[Unit] = {
    val typeExpressions = resolvedValue.typeStack.value.levels.map(resolvedValue.typeStack.as(_)).reverse
    val aboveLevels     = typeExpressions.init
    val signatureLevel  = typeExpressions.last

    for {
      // Iterate levels above signature, where each level computes the type of the underlying level
      kindType    <- aboveLevels.foldLeftM[TypeGraphIO, ExpressionValue](ExpressionValue.ConcreteValue(Value.Type)) {
                       (assumedType, level) =>
                         debug[TypeGraphIO](
                           s"Collecting constraints from higher levels, kind: ${assumedType.show}, type expression: ${level.value.show}"
                         ) >>
                           collectConstraints(assumedType, level, Seq.empty, false)
                     }
      // Infer the signature level, with adding supplied type arguments (this differs from above)
      _           <-
        debug[TypeGraphIO](
          s"Collecting constraints from signature, kind: ${kindType.show}, signature: ${signatureLevel.value.show}, type arguments: ${key.typeArguments.map(_.show).mkString(", ")}"
        )
      runtimeType <- collectConstraints(kindType, signatureLevel, key.typeArguments.map(ConcreteValue(_)), false)
      // Create constraints from runtime level against the signature, if runtime is available
      _           <- debug[TypeGraphIO](s"Collecting constraints from runtime, signature: ${runtimeType.show}")
      _           <- resolvedValue.runtime.traverse_(collectConstraints(runtimeType, _, Seq.empty, true).void)
    } yield ()
  }

  /** Collects constraints and returns the evaluated expression, which should be the type of the next expression one
    * level below. It strips all lambda expressions and returns expressions that include only unification vars.
    */
  private def collectConstraints(
      assumedType: ExpressionValue,
      expression: Sourced[OperatorResolvedExpression],
      typeArguments: Seq[ExpressionValue],
      runtime: Boolean // Prevent evaluations if in runtime
  ): TypeGraphIO[ExpressionValue] =
    expression.value match {
      case OperatorResolvedExpression.IntegerLiteral(integerLiteral)                    =>
        checkNoTypeArgs(expression, typeArguments) >>
          tellConstraint(
            Constraints.constraint(assumedType, expression.as(ConcreteValue(bigIntType)), "Type mismatch.")
          )
            .as(ConcreteValue(Direct(integerLiteral.value, bigIntType)))
      case OperatorResolvedExpression.StringLiteral(stringLiteral)                      =>
        checkNoTypeArgs(expression, typeArguments) >>
          tellConstraint(
            Constraints.constraint(assumedType, expression.as(ConcreteValue(stringType)), "Type mismatch.")
          )
            .as(ConcreteValue(Direct(stringLiteral.value, stringType)))
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
          // Bind the assumed type to the type of the resolved value's signature.
          // We don't check the signature here, but it will be checked when it is monomorphized
          resolvedMaybe <- StateT.liftF(getFact(OperatorResolvedValue.Key(vfqn.value)))
          _             <- resolvedMaybe match {
                             case Some(resolved) =>
                               for {
                                 // TODO: We don't supply type arguments here, so that's a problem
                                 signatureType <- StateT.liftF(Evaluator.evaluate(resolved.typeStack.map(_.signature)))
                                 _             <- tellConstraint(
                                                    Constraints.constraint(assumedType, vfqn.as(signatureType), "Type mismatch.")
                                                  )
                               } yield ()
                             case None           => StateT.liftF(compilerAbort(vfqn.as(s"Value not defined.")))
                           }
          // We need to return the evaluated call, but remember, that the runtime maybe
          // specialized to the eval, so we need to call eval on the current expression (which is a value call)
          // TODO: This has bad parameter names, not referring to unification vars
          bodyEvaled    <- if (runtime) {
                             // If we're in runtime, we don't need to evaluate the body, maybe it's not even there
                             ExpressionValue
                               .ConcreteValue(Value.Type)
                               .pure[TypeGraphIO] // This is ignored, so can be anything
                           } else {
                             StateT.liftF(Evaluator.evaluate(expression))
                           }
        } yield bodyEvaled
      case OperatorResolvedExpression.FunctionApplication(target, arg)                  =>
        for {
          _            <- checkNoTypeArgs(expression, typeArguments)
          argTypeVar   <- generateUnificationVar
          retTypeVar   <- generateUnificationVar
          funcType     <- StateT.liftF(Evaluator.functionType(argTypeVar, retTypeVar))
          targetEvaled <- collectConstraints(funcType, target, Seq.empty, runtime)
          argEvaled    <- collectConstraints(ParameterReference(argTypeVar), arg, Seq.empty, runtime)
          _            <- tellConstraint(
                            Constraints.constraint(assumedType, expression.as(ParameterReference(retTypeVar)), "Type mismatch.")
                          )
        } yield ExpressionValue.FunctionApplication(target.as(targetEvaled), arg.as(argEvaled))
      case OperatorResolvedExpression.FunctionLiteral(paramName, paramTypeOpt, body)    =>
        for {
          paramVar   <- generateUnificationVar
          _          <- paramTypeOpt.traverse_ { paramType =>
                          // TODO: I think this ignores the rest of the stack
                          for {
                            paramTypeEvaled <- StateT.liftF(Evaluator.evaluate(paramType.map(_.signature)))
                            _               <- tellConstraint(
                                                 Constraints.constraint(
                                                   ParameterReference(paramVar),
                                                   paramType.as(paramTypeEvaled),
                                                   "Type argument mismatch."
                                                 )
                                               )
                          } yield ()
                        }
          _          <- typeArguments.headOption.traverse_ { typeArg =>
                          tellConstraint(
                            Constraints.constraint(
                              ParameterReference(paramVar),
                              ExpressionValue.unsourced(typeArg),
                              "Type argument mismatch."
                            )
                          )
                        }
          _          <- bindParameter(paramName.value, paramName.as(ParameterReference(paramVar)))
          retTypeVar <- generateUnificationVar
          bodyEvaled <- collectConstraints(ParameterReference(retTypeVar), body, typeArguments.drop(1), runtime)
          funcType   <- StateT.liftF(Evaluator.functionType(paramVar, retTypeVar))
          _          <- tellConstraint(
                          Constraints.constraint(assumedType, body.as(funcType), "Type mismatch.")
                        )
        } yield bodyEvaled
    }

  private def checkNoTypeArgs(
      expression: Sourced[OperatorResolvedExpression],
      typeArguments: Seq[ExpressionValue]
  ): TypeGraphIO[Unit] =
    StateT
      .liftF(compilerAbort[Unit](expression.as("Unconsumed type arguments in non-lambda expression.")))
      .whenA(typeArguments.nonEmpty)

}
