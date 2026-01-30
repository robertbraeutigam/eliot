package com.vanillasource.eliot.eliotc.typesystem2.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.ExpressionStack
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.ConcreteValue
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve2.fact.{Expression, ResolvedValue}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import com.vanillasource.eliot.eliotc.typesystem2.fact.*
import com.vanillasource.eliot.eliotc.typesystem2.processor.SymbolicTypeCheckProcessor.TypeCheckResult
import com.vanillasource.eliot.eliotc.typesystem2.types.*
import com.vanillasource.eliot.eliotc.typesystem2.types.TypeCheckState.*

/** Type checks resolved values by building type constraints and solving them through unification.
  *
  * This processor orchestrates:
  *   - TypeExpressionBuilder: evaluates declared type signatures
  *   - BodyInferenceBuilder: infers types for body expressions
  *   - SymbolicUnification: solves the collected constraints
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
      checkResult           <- runTypeCheck(resolvedValue, body)
      fullConstraints        = checkResult.constraints |+| SymbolicUnification.unificationVars(checkResult.unificationVars)
      _                     <- debug[CompilerIO](s"Constraints: ${fullConstraints.show}")
      solution              <- fullConstraints.solve()
      _                     <- debug[CompilerIO](s"Solution: ${solution.show}")
      resolvedTypedLevels    = checkResult.typedSignature.value.expressions.map(applySubstitutions(_, solution))
      resolvedTypedBody      = applySubstitutions(checkResult.typedBody, solution)
      _                     <- verifyHigherLevelsAreConcreteValue(resolvedTypedLevels.drop(1), resolvedValue.value)
      signatureOnly          = resolvedTypedLevels.take(1)
      unifiedStack           = ExpressionStack(resolvedTypedBody +: signatureOnly, true)
    } yield TypeCheckedValue(
      resolvedValue.vfqn,
      resolvedValue.name,
      resolvedValue.value.as(unifiedStack)
    )
  }

  private def typeCheckWithoutBody(
      resolvedValue: ResolvedValue
  ): CompilerIO[TypeCheckedValue] =
    for {
      typedSignature <- TypeExpressionBuilder.build(resolvedValue.value).map(_.typed).runA(TypeCheckState())
      _              <- verifyHigherLevelsAreConcreteValue(typedSignature.value.expressions.drop(1), resolvedValue.value)
      signatureOnly   = typedSignature.map(s => ExpressionStack(s.expressions.take(1), false))
    } yield TypeCheckedValue(
      resolvedValue.vfqn,
      resolvedValue.name,
      signatureOnly
    )

  private def runTypeCheck(
      resolvedValue: ResolvedValue,
      body: Sourced[Expression]
  ): CompilerIO[TypeCheckResult] =
    (for {
      typeResult      <- TypeExpressionBuilder.build(resolvedValue.value)
      bodyResult      <- BodyInferenceBuilder.build(body)
      _               <- tellConstraint(
                           SymbolicUnification.constraint(
                             typeResult.exprValue,
                             body.as(bodyResult.exprValue),
                             "Type mismatch."
                           )
                         )
      constraints     <- getConstraints
      unificationVars <- getUnificationVars
    } yield TypeCheckResult(
      typeResult.exprValue,
      typeResult.typed,
      bodyResult.exprValue,
      bodyResult.typed,
      constraints,
      unificationVars
    )).runA(TypeCheckState())

  private def applySubstitutions(typed: TypedExpression, solution: UnificationState): TypedExpression =
    typed.transformTypes(solution.substitute)

  private def verifyHigherLevelsAreConcreteValue(
      higherLevels: Seq[TypedExpression],
      source: Sourced[?]
  ): CompilerIO[Unit] =
    higherLevels.find(typed => !typed.expressionType.isInstanceOf[ConcreteValue]) match {
      case Some(_) =>
        compilerError(source.as("Higher level type annotation must evaluate to a concrete type."))
      case None    =>
        ().pure[CompilerIO]
    }
}

object SymbolicTypeCheckProcessor {
  private case class TypeCheckResult(
      declaredType: com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue,
      typedSignature: com.vanillasource.eliot.eliotc.source.content.Sourced[ExpressionStack[TypedExpression]],
      bodyType: com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue,
      typedBody: TypedExpression,
      constraints: SymbolicUnification,
      unificationVars: Set[String]
  )
}
