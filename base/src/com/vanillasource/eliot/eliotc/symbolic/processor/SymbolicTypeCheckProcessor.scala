package com.vanillasource.eliot.eliotc.symbolic.processor

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

  /** Process type levels recursively from top to bottom.
    *
    * The implicit top level is TypeType. For each level:
    *   1. Infer the type of the expression 2. Check it matches the expected type (TypeType for top, evaluated value
    *      from above otherwise) 3. Evaluate to get a concrete value 4. Use the evaluated value as expected type for the
    *      next level
    *
    * @return
    *   Tuple of (signatureType, typedLevels)
    */
  private def processTypeLevels(
      typeExpr: Sourced[com.vanillasource.eliot.eliotc.core.fact.ExpressionStack[Expression]]
  ): TypeGraphIO[(ExpressionValue, Seq[TypedExpression])] = {
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
        } yield (uvar, Seq.empty)
      case levels =>
        // Process from top to bottom, starting with TypeType as expected
        processLevelsRecursive(levels, Value.Type, typeExpr)
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
    * @return
    *   Tuple of (signatureType, typedLevels)
    */
  private def processLevelsRecursive(
      levels: List[Expression],
      expectedType: Value,
      source: Sourced[?]
  ): TypeGraphIO[(ExpressionValue, Seq[TypedExpression])] =
    levels match {
      case Nil =>
        // Base case: no more levels
        (ConcreteValue(expectedType), Seq.empty[TypedExpression]).pure[TypeGraphIO]

      case expr :: rest if rest.isEmpty =>
        // Signature level - infer type, check against expected, return inferred type
        for {
          typeResult <- TypeExpressionBuilder.build(expr)
          // Note: For signature level, we don't strictly check against expectedType
          // because the signature can involve unification variables (generics)
        } yield (typeResult.expressionType, Seq(typeResult))

      case expr :: rest =>
        // Higher level (above signature) - must evaluate to ConcreteValue
        for {
          // 1. Infer the type of this expression
          typeResult <- TypeExpressionBuilder.build(expr)

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
                                ) *> Value.Type.pure[TypeGraphIO]
                            }

          // 4. Process remaining levels with evaluated value as expected type
          (restSignatureType, restTypedLevels) <- processLevelsRecursive(rest, evaluatedValue, source)
        } yield (restSignatureType, typeResult +: restTypedLevels)
    }

  private def applySubstitutions(typed: TypedExpression, solution: UnificationState): TypedExpression =
    typed.transformTypes(solution.substitute)
}
