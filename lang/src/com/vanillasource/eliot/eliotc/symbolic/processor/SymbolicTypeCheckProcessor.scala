package com.vanillasource.eliot.eliotc.symbolic.processor

import cats.data.NonEmptySeq
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Value}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.matchdesugar.fact.MatchDesugaredExpression
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.{
  QualifiedName as ResolveQualifiedName,
  Qualifier as ResolveQualifier
}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.symbolic.fact.*
import com.vanillasource.eliot.eliotc.symbolic.types.*
import com.vanillasource.eliot.eliotc.symbolic.types.TypeCheckState.*

/** Type checks resolved values by building type constraints and solving them through unification.
  *
  * The processor handles type stacks where each level describes the type of the level below:
  *   - The implicit top level is TypeType
  *   - Each higher level (above signature) must evaluate to ConcreteValue
  *   - Each level's value must have valueType matching the level above's value
  *   - Signature (level 0) and optional runtime become the final typed output
  *
  * After solving, ability function references in the body are replaced with their concrete implementations.
  * If an ability is called with abstract type parameters, a compiler error is emitted.
  */
class SymbolicTypeCheckProcessor
    extends TransformationProcessor[OperatorResolvedValue.Key, TypeCheckedValue.Key](key =>
      OperatorResolvedValue.Key(key.vfqn)
    )
    with Logging {

  private case class TypeCheckResult(
      declaredType: ExpressionValue,
      typedLevels: NonEmptySeq[TypedExpression],
      typedBody: TypedExpression,
      constraints: SymbolicUnification,
      universalVars: Set[String],
      unificationVars: Set[String],
      qualifierParams: Seq[ExpressionValue]
  )

  override protected def generateFromKeyAndFact(
      key: TypeCheckedValue.Key,
      resolvedValue: OperatorResolvedValue
  ): CompilerIO[TypeCheckedValue] =
    resolvedValue.runtime match {
      case Some(body) => typeCheckWithBody(resolvedValue, body)
      case None       => typeCheckWithoutBody(resolvedValue)
    }

  private def typeCheckWithBody(
      resolvedValue: OperatorResolvedValue,
      body: Sourced[OperatorResolvedExpression]
  ): CompilerIO[TypeCheckedValue] =
    for {
      result <- (for {
                  (declaredType, typedLevels) <-
                    TypeExpressionEvaluator.processStackForDeclaration(resolvedValue.typeStack).map { case (signatureType, typedStack) =>
                      (signatureType, typedStack.value.levels)
                    }
                  bodyResult                  <- BodyTypeInferrer.inferBody(body)
                  // For constraint building, strip universal intros (type params) from declared type
                  // since the body's type won't have them - they're handled via universalVars
                  strippedDeclaredType         = stripUniversalIntros(declaredType)
                  _                           <- tellConstraint(
                                                   SymbolicUnification.constraint(
                                                     strippedDeclaredType,
                                                     body.as(bodyResult.expressionType),
                                                     "Type mismatch."
                                                   )
                                                 )
                  qualifierParams             <- resolveQualifierParams(resolvedValue.name)
                  constraints                 <- getConstraints
                  universalVars               <- getUniversalVars
                  unificationVars             <- getUnificationVars
                } yield TypeCheckResult(
                  declaredType,
                  typedLevels,
                  bodyResult,
                  constraints,
                  universalVars,
                  unificationVars,
                  qualifierParams
                ))
                  .runA(TypeCheckState())

      _                   <- debug[CompilerIO](s"Constraints (of ${resolvedValue.vfqn.show}): ${result.constraints.show}")
      solution            <- ConstraintSolver.solve(result.constraints, result.universalVars, result.unificationVars)
      _                   <- debug[CompilerIO](s"Solution (of ${resolvedValue.vfqn.show}): ${solution.show}")
      resolvedTypedLevels  = result.typedLevels.map(_.transformTypes(solution.substitute))
      resolvedTypedBody    = result.typedBody.transformTypes(solution.substitute)
      signatureType        = resolvedTypedLevels.head.expressionType
      resolvedQualifierParams = result.qualifierParams.map(solution.substitute)
      _                   <-
        debug[CompilerIO](
          s"Produced symbolic checked (of ${resolvedValue.vfqn.show}) signature: ${signatureType.show}, body: ${resolvedTypedBody.expression.show}"
        )
    } yield TypeCheckedValue(
      resolvedValue.vfqn,
      convertQualifiedName(resolvedValue.name, resolvedQualifierParams),
      signatureType,
      Some(resolvedValue.typeStack.as(resolvedTypedBody.expression))
    )

  private def typeCheckWithoutBody(
      resolvedValue: OperatorResolvedValue
  ): CompilerIO[TypeCheckedValue] =
    for {
      result <- (for {
                  (signatureType, _) <- TypeExpressionEvaluator
                                          .processStackForDeclaration(resolvedValue.typeStack)
                                          .map { case (signatureType, typedStack) =>
                                            (signatureType, typedStack.value.levels)
                                          }
                  qualifierParams    <- resolveQualifierParams(resolvedValue.name)
                } yield (signatureType, qualifierParams))
                  .runA(TypeCheckState())
      (signatureType, qualifierParams) = result
    } yield TypeCheckedValue(
      resolvedValue.vfqn,
      convertQualifiedName(resolvedValue.name, qualifierParams),
      signatureType,
      None
    )

  private def resolveQualifierParams(
      name: Sourced[ResolveQualifiedName]
  ): TypeGraphIO[Seq[ExpressionValue]] =
    name.value.qualifier match {
      case ResolveQualifier.AbilityImplementation(_, params) =>
        params.traverse { param =>
          TypeExpressionEvaluator
            .processStackForDeclaration(name.as(TypeStack.of(OperatorResolvedExpression.fromExpression(MatchDesugaredExpression.fromExpression(param)))))
            .map(_._1)
        }
      case _                                                 => Seq.empty[ExpressionValue].pure[TypeGraphIO]
    }

  private def convertQualifiedName(
      name: Sourced[ResolveQualifiedName],
      qualifierParams: Seq[ExpressionValue]
  ): Sourced[QualifiedName] =
    name.map { n =>
      val qualifier = n.qualifier match {
        case ResolveQualifier.Default                       => Qualifier.Default
        case ResolveQualifier.Type                          => Qualifier.Type
        case ResolveQualifier.Ability(an)                   => Qualifier.Ability(an)
        case ResolveQualifier.AbilityImplementation(an, _)  => Qualifier.AbilityImplementation(an, qualifierParams)
      }
      QualifiedName(n.name, qualifier)
    }

  /** Strip FunctionLiteral wrappers that represent universal type introductions. These have Value.Type as the parameter
    * type. This is used for constraint building where the body's type won't have them - they're handled via universalVars.
    */
  @scala.annotation.tailrec
  private def stripUniversalIntros(expr: ExpressionValue): ExpressionValue =
    expr match {
      case ExpressionValue.FunctionLiteral(_, Value.Type, body) => stripUniversalIntros(body)
      case other                                                => other
    }
}
