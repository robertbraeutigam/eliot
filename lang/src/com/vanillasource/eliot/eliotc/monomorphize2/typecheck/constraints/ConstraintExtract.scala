package com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.monomorphize2.fact.MonomorphicValue
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.{getFact, getFactOrAbort}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.eval.fact.Types.{bigIntFQN, functionDataTypeFQN, stringFQN, typeFQN}
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import TypeCheckState.*
import com.vanillasource.eliot.eliotc.core.fact.TypeStack

object ConstraintExtract extends Logging {
  def collectConstraints(key: MonomorphicValue.Key, resolvedValue: OperatorResolvedValue): TypeGraphIO[Unit] = {
    val rawTypeExpressions = resolvedValue.typeStack.value.levels.map(resolvedValue.typeStack.as(_)).reverse
    // Apply the specified type arguments to the signature expression
    val typeExpressions    = rawTypeExpressions.init :+
      key.specifiedTypeArguments.foldLeft(rawTypeExpressions.last) { (acc, arg) =>
        acc.as(FunctionApplication(acc, arg))
      }

    for {
      // Iterate type levels, where each level computes the type of the underlying level. The last one is the signature.
      signature <-
        typeExpressions.foldLeftM[TypeGraphIO, OperatorResolvedExpression](
          ValueReference(resolvedValue.name.as(typeFQN))
        ) { (assumedType, level) =>
          debug[TypeGraphIO](
            s"Collecting constraints from type levels, kind: ${assumedType.show}, type expression: ${level.value.show}"
          ) >> collectConstraints(assumedType, level)
        }
      // Create constraints from runtime level against the signature, if runtime is available
      _         <- debug[TypeGraphIO](s"Collecting constraints from runtime, signature: ${signature.show}")
      _         <- resolvedValue.runtime.traverse_(collectConstraints(signature, _))
    } yield ()
  }

  /** Collects constraints and returns the evaluated expression, which should be the type of the next expression one
    * level below. It strips all lambda expressions and returns expressions that include only unification vars.
    */
  private def collectConstraints(
      assumedType: OperatorResolvedExpression,
      expression: Sourced[OperatorResolvedExpression]
  ): TypeGraphIO[OperatorResolvedExpression] =
    expression.value match {
      case IntegerLiteral(integerLiteral)                    =>
        tellConstraint(
          Constraints.constraint(
            assumedType,
            expression.as(ValueReference(expression.as(bigIntFQN))),
            "Type mismatch."
          )
        )
          .as(expression.value)
      case StringLiteral(stringLiteral)                      =>
        tellConstraint(
          Constraints.constraint(
            assumedType,
            expression.as(ValueReference(expression.as(stringFQN))),
            "Type mismatch."
          )
        )
          .as(expression.value)
      case ParameterReference(name)                          =>
        for {
          _         <- trace[TypeGraphIO](s"Collecting from parameter reference '${name.value}'")
          maybeType <- lookupParameter(name.value)
          exprType  <- maybeType match {
                         case Some(paramType) => paramType.value.pure[TypeGraphIO]
                         case None            =>
                           StateT.liftF(compilerAbort(name.as("Parameter not found.")))
                       }
          _         <- tellConstraint(Constraints.constraint(assumedType, expression.as(exprType), "Type mismatch."))
        } yield exprType
      case ValueReference(vfqn, _) if vfqn.value === typeFQN =>
        ValueReference(expression.as(typeFQN)).pure[TypeGraphIO]
      case ValueReference(vfqn, typeArgs)                    =>
        for {
          _             <- trace[TypeGraphIO](s"Collecting from value reference '${vfqn.show}'")
          // Bind the assumed type to the resolved value's signature. The signature may be a generic (a function
          // abstraction or any other code that produces one); the solver, working with the evaluator, will instantiate
          // it as needed.
          resolvedMaybe <- StateT.liftF(getFact(OperatorResolvedValue.Key(vfqn.value)))
          _             <- resolvedMaybe match {
                             case Some(resolved) =>
                               tellConstraint(
                                 Constraints.constraint(
                                   assumedType,
                                   expression.as(resolved.typeStack.value.signature),
                                   "Type mismatch."
                                 )
                               )
                             case None           => StateT.liftF(compilerAbort(vfqn.as(s"Value not defined.")))
                           }
        } yield expression.value
      case FunctionApplication(
            targetSource @ Sourced(_, _, target @ FunctionLiteral(paramName, paramTypeOpt, body)),
            arg
          ) =>
        // This is a special case to short-circuit applied type arguments for most cases
        // TODO: if this does not apply, function literals can still "escape" into the solver
        for {
          _ <- bindParameter(paramName.value, arg)
        } yield substitute(body.value, paramName.value, arg.value)
      case FunctionApplication(target, arg)                  =>
        for {
          _            <- trace[TypeGraphIO](s"Collecting from function application")
          argTypeVar   <- generateUnificationVar
          retTypeVar   <- generateUnificationVar
          funcType      =
            FunctionApplication(
              expression.as(
                FunctionApplication(
                  expression.as(ValueReference(expression.as(functionDataTypeFQN))),
                  target.as(ParameterReference(target.as(argTypeVar)))
                )
              ),
              arg.as(ParameterReference(target.as(retTypeVar)))
            )
          targetEvaled <- collectConstraints(funcType, target)
          argEvaled    <- collectConstraints(ParameterReference(arg.as(argTypeVar)), arg)
          _            <- tellConstraint(
                            Constraints.constraint(
                              assumedType,
                              expression.as(ParameterReference(arg.as(retTypeVar))),
                              "Type mismatch."
                            )
                          )
        } yield FunctionApplication(target.as(targetEvaled), arg.as(argEvaled))
      case FunctionLiteral(paramName, paramTypeOpt, body)    =>
        for {
          _            <- trace[TypeGraphIO](s"Collecting from function literal")
          paramTypeVar <- generateUnificationVar
          _            <- paramTypeOpt.traverse_ { paramType =>
                            // The parameter type annotation IS the type expression we want to bind. Emit the constraint
                            // directly: recursing through collectConstraints would compute the *kind* of the annotation
                            // (e.g. Type for String) instead of the annotation itself.
                            // TODO: ignores higher levels of the type stack
                            tellConstraint(
                              Constraints.constraint(
                                ParameterReference(paramType.as(paramTypeVar)),
                                paramType.map(_.signature),
                                "Type mismatch."
                              )
                            )
                          }
          paramVar     <- generateUnificationVar
          // Bind the parameter name to its TYPE variable. Otherwise lookupParameter would return an unconstrained
          // fresh var and the body's type cannot be checked against the actual parameter type.
          _            <- bindParameter(paramName.value, paramName.as(ParameterReference(paramName.as(paramTypeVar))))
          retTypeVar   <- generateUnificationVar
          bodyEvaled   <- collectConstraints(ParameterReference(body.as(retTypeVar)), body)
          funcType      = FunctionApplication(
                            expression.as(
                              FunctionApplication(
                                expression.as(ValueReference(expression.as(functionDataTypeFQN))),
                                paramName.as(ParameterReference(paramName.as(paramTypeVar)))
                              )
                            ),
                            body.as(ParameterReference(body.as(retTypeVar)))
                          )
          _            <- tellConstraint(
                            Constraints.constraint(assumedType, body.as(funcType), "Type mismatch.")
                          )
        } yield FunctionLiteral(
          paramName.as(paramVar),
          Some(paramName.as(TypeStack.of(ParameterReference(paramName.as(paramTypeVar))))),
          body.as(bodyEvaled)
        )
    }
}
