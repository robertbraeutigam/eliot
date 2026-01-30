package com.vanillasource.eliot.eliotc.typesystem2.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.ConcreteValue
import com.vanillasource.eliot.eliotc.eval.fact.Value
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
      checkResult       <- runTypeCheck(resolvedValue, body)
      fullConstraints    = checkResult.constraints |+| SymbolicUnification.unificationVars(checkResult.unificationVars)
      _                 <- debug[CompilerIO](s"Constraints: ${fullConstraints.show}")
      solution          <- fullConstraints.solve()
      _                 <- debug[CompilerIO](s"Solution: ${solution.show}")
      resolvedTypedLevels = checkResult.typedLevels.map(applySubstitutions(_, solution))
      resolvedTypedBody   = applySubstitutions(checkResult.typedBody, solution)
      _                 <- verifyHigherLevels(resolvedTypedLevels, resolvedValue.value)
      signatureType      = resolvedTypedLevels.headOption.map(_.expressionType).getOrElse(solution.substitute(checkResult.declaredType))
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
      buildResult  <- TypeExpressionBuilder.build(resolvedValue.value).runA(TypeCheckState())
      _            <- verifyHigherLevels(buildResult.typedLevels, resolvedValue.value)
      signatureType = buildResult.typedLevels.headOption.map(_.expressionType).getOrElse(buildResult.exprValue)
    } yield TypeCheckedValue(
      resolvedValue.vfqn,
      resolvedValue.name,
      signatureType,
      None
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
      typeResult.typedLevels,
      bodyResult.exprValue,
      bodyResult.typed,
      constraints,
      unificationVars
    )).runA(TypeCheckState())

  private def applySubstitutions(typed: TypedExpression, solution: UnificationState): TypedExpression =
    typed.transformTypes(solution.substitute)

  /** Verify that higher levels (above signature) are ConcreteValue and have correct valueType relationships.
    *
    * The type hierarchy is: TypeType (implicit) -> level N -> level N-1 -> ... -> level 1 (signature) Each level's
    * evaluated value must have valueType matching the value from the level above.
    */
  private def verifyHigherLevels(
      levels: Seq[TypedExpression],
      source: Sourced[?]
  ): CompilerIO[Unit] = {
    val higherLevels = levels.drop(1) // Skip signature (index 0)

    // Check all higher levels are ConcreteValue
    higherLevels.find(typed => !typed.expressionType.isInstanceOf[ConcreteValue]) match {
      case Some(_) =>
        compilerError(source.as("Higher level type annotation must evaluate to a concrete type."))
      case None    =>
        // Verify type relationships: each level's valueType must match the level above
        verifyTypeChain(higherLevels, Value.TypeType, source)
    }
  }

  /** Verify the chain of type relationships from top to bottom. */
  private def verifyTypeChain(
      levels: Seq[TypedExpression],
      expectedType: Value,
      source: Sourced[?]
  ): CompilerIO[Unit] =
    levels.reverse.foldLeftM(expectedType) { (expected, typed) =>
      typed.expressionType match {
        case ConcreteValue(value) if value.valueType == expected =>
          value.pure[CompilerIO]
        case ConcreteValue(value) =>
          compilerError(source.as(s"Type level mismatch: expected type ${expected}, but got ${value.valueType}")) *>
            value.pure[CompilerIO]
        case other =>
          compilerError(source.as("Higher level type annotation must evaluate to a concrete type.")) *>
            Value.TypeType.pure[CompilerIO]
      }
    }.void
}

object SymbolicTypeCheckProcessor {
  private case class TypeCheckResult(
      declaredType: ExpressionValue,
      typedLevels: Seq[TypedExpression],
      bodyType: ExpressionValue,
      typedBody: TypedExpression,
      constraints: SymbolicUnification,
      unificationVars: Set[String]
  )
}
