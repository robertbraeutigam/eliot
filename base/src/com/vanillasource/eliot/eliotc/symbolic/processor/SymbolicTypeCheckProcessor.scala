package com.vanillasource.eliot.eliotc.symbolic.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.ExpressionStack
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve2.fact.{Expression, ResolvedValue}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.symbolic.fact.*
import com.vanillasource.eliot.eliotc.symbolic.types.*
import com.vanillasource.eliot.eliotc.symbolic.types.TypeCheckState.*

/** Type checks resolved values by building type constraints and solving them through unification.
  *
  * The processor handles expression stacks where each level describes the type of the level below:
  *   - The implicit top level is TypeType
  *   - Each higher level (above signature) must evaluate to ConcreteValue
  *   - Each level's value must have valueType matching the level above's value
  *   - Signature (level 1) and runtime (level 0) become the final typed output
  */
class SymbolicTypeCheckProcessor
    extends TransformationProcessor[ResolvedValue.Key, TypeCheckedValue.Key](key => ResolvedValue.Key(key.vfqn))
    with Logging {

  override protected def generateFromKeyAndFact(
      key: TypeCheckedValue.Key,
      resolvedValue: ResolvedValue
  ): CompilerIO[TypeCheckedValue] =
    resolvedValue.value.value.runtime match {
      case Some(bodyExpr) => typeCheckWithBody(resolvedValue, bodyExpr)
      case None           => typeCheckWithoutBody(resolvedValue)
    }

  private def typeCheckWithBody(
      resolvedValue: ResolvedValue,
      bodyExpr: Expression
  ): CompilerIO[TypeCheckedValue] = {
    val body = resolvedValue.value.as(bodyExpr)
    for {
      result             <- (for {
                              (declaredType, typedLevels) <- processTypeLevels(resolvedValue.value)
                              bodyResult                  <- BodyInferenceBuilder.build(body)
                              _                           <- tellConstraint(
                                                               SymbolicUnification.constraint(
                                                                 declaredType,
                                                                 body.as(bodyResult.expressionType),
                                                                 "Type mismatch."
                                                               )
                                                             )
                              constraints                 <- getConstraints
                              universalVars               <- getUniversalVars
                              unificationVars             <- getUnificationVars
                            } yield (declaredType, typedLevels, bodyResult, constraints, universalVars, unificationVars))
                              .runA(TypeCheckState())
      (declaredType, typedLevels, typedBody, constraints, universalVars, unificationVars) = result
      _                  <- debug[CompilerIO](s"Constraints: ${constraints.show}")
      solution           <- constraints.solve(universalVars, unificationVars)
      _                  <- debug[CompilerIO](s"Solution: ${solution.show}")
      resolvedTypedLevels = typedLevels.map(applySubstitutions(_, solution))
      resolvedTypedBody   = applySubstitutions(typedBody, solution)
      signatureType       =
        resolvedTypedLevels.headOption.map(_.expressionType).getOrElse(solution.substitute(declaredType))
    } yield TypeCheckedValue(
      resolvedValue.vfqn,
      resolvedValue.name,
      signatureType,
      Some(resolvedValue.value.as(resolvedTypedBody.expression))
    )
  }

  private def typeCheckWithoutBody(
      resolvedValue: ResolvedValue
  ): CompilerIO[TypeCheckedValue] =
    for {
      (signatureType, _) <- processTypeLevels(resolvedValue.value).runA(TypeCheckState())
    } yield TypeCheckedValue(
      resolvedValue.vfqn,
      resolvedValue.name,
      signatureType,
      None
    )

  /** Process type levels by delegating to TypeExpressionBuilder.processStack.
    *
    * @return
    *   Tuple of (signatureType, typedLevels)
    */
  private def processTypeLevels(
      typeExpr: Sourced[ExpressionStack[Expression]]
  ): TypeGraphIO[(ExpressionValue, Seq[TypedExpression])] =
    TypeExpressionBuilder.processStack(typeExpr).map { case (signatureType, typedStack) =>
      (signatureType, typedStack.value.expressions)
    }

  private def applySubstitutions(typed: TypedExpression, solution: UnificationState): TypedExpression =
    typed.transformTypes(solution.substitute)
}
