package com.vanillasource.eliot.eliotc.typesystem2.processor

import cats.data.StateT
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
import com.vanillasource.eliot.eliotc.typesystem2.processor.SymbolicTypeCheckProcessor.{
  TypeCheckResult,
  TypeLevelsResult
}
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
      checkResult        <- runTypeCheck(resolvedValue, body)
      fullConstraints     = checkResult.constraints |+| SymbolicUnification.unificationVars(checkResult.unificationVars)
      _                  <- debug[CompilerIO](s"Constraints: ${fullConstraints.show}")
      solution           <- fullConstraints.solve()
      _                  <- debug[CompilerIO](s"Solution: ${solution.show}")
      resolvedTypedLevels = checkResult.typedLevels.map(applySubstitutions(_, solution))
      resolvedTypedBody   = applySubstitutions(checkResult.typedBody, solution)
      signatureType       =
        resolvedTypedLevels.headOption.map(_.expressionType).getOrElse(solution.substitute(checkResult.declaredType))
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
      levelsResult <- processTypeLevels(resolvedValue.value).runA(TypeCheckState())
      signatureType = levelsResult.signatureType
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
      levelsResult    <- processTypeLevels(resolvedValue.value)
      bodyResult      <- BodyInferenceBuilder.build(body)
      _               <- tellConstraint(
                           SymbolicUnification.constraint(
                             levelsResult.signatureType,
                             body.as(bodyResult.expressionType),
                             "Type mismatch."
                           )
                         )
      constraints     <- getConstraints
      unificationVars <- getUnificationVars
    } yield TypeCheckResult(
      levelsResult.signatureType,
      levelsResult.typedLevels,
      bodyResult,
      constraints,
      unificationVars
    )).runA(TypeCheckState())

  /** Process type levels recursively from top to bottom.
    *
    * The implicit top level is TypeType. For each level:
    *   1. Infer the type of the expression 2. Check it matches the expected type (TypeType for top, evaluated value
    *      from above otherwise) 3. Evaluate to get a concrete value 4. Use the evaluated value as expected type for the
    *      next level
    */
  private def processTypeLevels(
      typeExpr: Sourced[com.vanillasource.eliot.eliotc.core.fact.ExpressionStack[Expression]]
  ): TypeGraphIO[TypeLevelsResult] = {
    val expressions = typeExpr.value.expressions
    val hasRuntime  = typeExpr.value.hasRuntime

    // Get type levels (excluding runtime if present)
    val typeLevelExprs =
      if (hasRuntime && expressions.length > 1) expressions.drop(1)
      else expressions

    typeLevelExprs.toList.reverse match {
      case Nil    =>
        // No explicit type levels - generate unification variable for signature
        for {
          uvar <- generateUnificationVar(typeExpr)
        } yield TypeLevelsResult(uvar, Seq.empty)
      case levels =>
        // Process from top to bottom, starting with TypeType as expected
        processLevelsRecursive(levels, Value.TypeType, typeExpr)
    }
  }

  /** Recursively process type levels from top (highest) to bottom (signature).
    *
    * @param levels
    *   Remaining levels to process (from top to bottom)
    * @param expectedType
    *   The expected type for the current level
    * @param source
    *   Source location for error messages
    */
  private def processLevelsRecursive(
      levels: List[Expression],
      expectedType: Value,
      source: Sourced[?]
  ): TypeGraphIO[TypeLevelsResult] =
    levels match {
      case Nil =>
        // Base case: no more levels
        TypeLevelsResult(ConcreteValue(expectedType), Seq.empty).pure[TypeGraphIO]

      case expr :: rest if rest.isEmpty =>
        // Signature level - infer type, check against expected, return inferred type
        for {
          typeResult <- TypeExpressionBuilder.build(expr, source)
          // Note: For signature level, we don't strictly check against expectedType
          // because the signature can involve unification variables (generics)
        } yield TypeLevelsResult(typeResult.expressionType, Seq(typeResult))

      case expr :: rest =>
        // Higher level (above signature) - must evaluate to ConcreteValue
        for {
          // 1. Infer the type of this expression
          typeResult <- TypeExpressionBuilder.build(expr, source)

          // 2. Extract the concrete value for use as expected type in next level
          evaluatedValue <- typeResult.expressionType match {
                              case ConcreteValue(v) =>
                                // 3. Check the value's type matches expected
                                if (v.valueType == expectedType) {
                                  v.pure[TypeGraphIO]
                                } else {
                                  StateT.liftF(
                                    compilerError(
                                      source.as(
                                        s"Type level mismatch: expected ${expectedType}, but got ${v.valueType}"
                                      )
                                    )
                                  ) *> v.pure[TypeGraphIO]
                                }
                              case other            =>
                                StateT.liftF(
                                  compilerError(
                                    source.as("Higher level type annotation must evaluate to a concrete type.")
                                  )
                                ) *> Value.TypeType.pure[TypeGraphIO]
                            }

          // 4. Process remaining levels with evaluated value as expected type
          restResult     <- processLevelsRecursive(rest, evaluatedValue, source)
        } yield TypeLevelsResult(
          restResult.signatureType,
          typeResult +: restResult.typedLevels
        )
    }

  private def applySubstitutions(typed: TypedExpression, solution: UnificationState): TypedExpression =
    typed.transformTypes(solution.substitute)
}

object SymbolicTypeCheckProcessor {

  /** Result of processing type levels recursively.
    *
    * @param signatureType
    *   The inferred type of the signature level
    * @param typedLevels
    *   Typed expressions for all levels, from signature (index 0) to higher levels
    */
  private case class TypeLevelsResult(
      signatureType: ExpressionValue,
      typedLevels: Seq[TypedExpression]
  )

  private case class TypeCheckResult(
      declaredType: ExpressionValue,
      typedLevels: Seq[TypedExpression],
      typedBody: TypedExpression,
      constraints: SymbolicUnification,
      unificationVars: Set[String]
  )
}
