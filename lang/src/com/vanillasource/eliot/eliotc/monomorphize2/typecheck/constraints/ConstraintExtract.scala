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

object ConstraintExtract extends Logging {
  def collectConstraints(key: MonomorphicValue.Key, resolvedValue: OperatorResolvedValue): TypeGraphIO[Unit] = {
    val typeExpressions = resolvedValue.typeStack.value.levels.map(resolvedValue.typeStack.as(_)).reverse

    for {
      // Iterate type levels, where each level computes the type of the underlying level. The last one is the signature.
      signature             <-
        typeExpressions.foldLeftM[TypeGraphIO, Sourced[OperatorResolvedExpression]](
          resolvedValue.name.as(ValueReference(resolvedValue.name.as(typeFQN)))
        ) { (assumedType, level) =>
          debug[TypeGraphIO](
            s"Collecting constraints from type levels, kind: ${assumedType.value.show}, type expression: ${level.value.show}"
          ) >> collectConstraints(assumedType, level).map(level.as(_))
        }
      // Apply the specified type arguments to the signature to obtain the monomorphized signature.
      // We don't apply them inside the typeExpressions fold, because that would break the level
      // above (the kind of the substituted signature is no longer the original kind).
      monomorphizedSignature = key.specifiedTypeArguments
                                 .foldLeft(resolvedValue.typeStack.as(signature.value)) { (acc, arg) =>
                                   acc.as(FunctionApplication(acc, arg))
                                 }
      // Create constraints from runtime level against the monomorphized signature, if runtime is available
      _                     <- debug[TypeGraphIO](s"Collecting constraints from runtime, signature: ${monomorphizedSignature.value.show}")
      _                     <- resolvedValue.runtime.traverse_(collectConstraints(monomorphizedSignature, _))
    } yield ()
  }

  /** Collects constraints and returns the evaluated expression, which should be the type of the next expression one
    * level below. It strips all lambda expressions and returns expressions that include only unification vars.
    *
    * Side effect: records the assumed (expected-from-parent) ORE type at this node into
    * `TypeCheckState.nodeAssumedTypes`. The processor walk later reads from that side-table by node identity, instead
    * of replaying the extractor's fresh-variable generator.
    */
  private def collectConstraints(
      assumedType: Sourced[OperatorResolvedExpression],
      expression: Sourced[OperatorResolvedExpression]
  ): TypeGraphIO[OperatorResolvedExpression] =
    recordAssumedType(expression, assumedType.value) >> (expression.value match {
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
        } yield expression.value
      case ValueReference(vfqn, _) if vfqn.value === typeFQN =>
        // This is to prevent infinite checks, since Type's type is Type
        for {
          _ <- tellConstraint(Constraints.constraint(assumedType, expression, "Type mismatch."))
        } yield ValueReference(expression.as(typeFQN))
      case ValueReference(vfqn, typeArgs)                    =>
        for {
          _             <- trace[TypeGraphIO](s"Collecting from value reference '${vfqn.show}'")
          // Bind the assumed type to the type of the resolved value's signature, with the
          // explicit type arguments applied as FunctionApplication wrappers. The solver's
          // beta-reduction rules consume the wrapping by reducing against the polytype
          // signature, the same way the top-level fold handles `key.specifiedTypeArguments`.
          resolvedMaybe <- StateT.liftF(getFact(OperatorResolvedValue.Key(vfqn.value)))
          _             <- resolvedMaybe match {
                             case Some(resolved) =>
                               val appliedSig =
                                 typeArgs.foldLeft(expression.as(resolved.typeStack.value.signature)) { (acc, arg) =>
                                   acc.as(FunctionApplication(acc, arg))
                                 }
                               tellConstraint(
                                 Constraints.constraint(assumedType, appliedSig, "Type mismatch.")
                               )
                             case None           => StateT.liftF(compilerAbort(vfqn.as(s"Value not defined.")))
                           }
        } yield expression.value
      case FunctionApplication(target, arg)                  =>
        for {
          _            <- trace[TypeGraphIO](s"Collecting from function application ${target.value.show}(${arg.value.show})")
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
          targetEvaled <- collectConstraints(expression.as(funcType), target)
          argEvaled    <- collectConstraints(expression.as(ParameterReference(arg.as(argTypeVar))), arg)
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
          _          <- trace[TypeGraphIO](s"Collecting from function literal")
          // The parameter's declared type is the annotation expression itself, not a fresh
          // unification variable. This avoids conflating the parameter's identity with its kind.
          // If there's no annotation, fall back to a fresh variable as a placeholder.
          paramType  <- paramTypeOpt match {
                          case Some(pt) =>
                            // TODO: I think this ignores the rest of the stack, fix this!
                            pt.map(_.signature).pure[TypeGraphIO]
                          case None     =>
                            generateUnificationVar.map(v =>
                              paramName.as(ParameterReference(paramName.as(v))): Sourced[OperatorResolvedExpression]
                            )
                        }
          _          <- bindParameter(paramName.value, paramType)
          retTypeVar <- generateUnificationVar
          _          <- collectConstraints(expression.as(ParameterReference(body.as(retTypeVar))), body)
          funcType    = FunctionApplication(
                          expression.as(
                            FunctionApplication(
                              expression.as(ValueReference(expression.as(functionDataTypeFQN))),
                              paramType
                            )
                          ),
                          body.as(ParameterReference(body.as(retTypeVar)))
                        )
          _          <- tellConstraint(
                          Constraints.constraint(assumedType, expression.as(funcType), "Type mismatch.")
                        )
        } yield expression.value
    })
}
