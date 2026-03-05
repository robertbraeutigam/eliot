package com.vanillasource.eliot.eliotc.symbolic.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
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

class SymbolicTypeCheckProcessor
    extends TransformationProcessor[OperatorResolvedValue.Key, TypeCheckedValue.Key](key =>
      OperatorResolvedValue.Key(key.vfqn)
    )
    with Logging {

  override protected def generateFromKeyAndFact(
      key: TypeCheckedValue.Key,
      resolvedValue: OperatorResolvedValue
  ): CompilerIO[TypeCheckedValue] = {
    val typeStack   = resolvedValue.typeStack
    val typeLevels  = typeStack.value.levels.toSeq.map(typeStack.as(_))
    val expressions = typeLevels ++ resolvedValue.runtime.toSeq
    for {
      (result, constraints, universalVars, qualifierParams) <-
        (for {
          result          <- SymbolicEvaluator2.typeCheck(expressions)
          qualifierParams <- resolveQualifierParams(resolvedValue.name)
          constraints     <- getConstraints
          universalVars   <- getUniversalVars
        } yield (result, constraints, universalVars, qualifierParams))
          .runA(TypeCheckState())

      _                      <- debug[CompilerIO](s"Constraints (of ${resolvedValue.vfqn.show}): ${constraints.show}")
      solution               <- ConstraintSolver.solve(constraints, universalVars)
      _                      <- debug[CompilerIO](s"Solution (of ${resolvedValue.vfqn.show}): ${solution.show}")
      resolvedResult          = result.transformTypes(solution.substitute)
      resolvedQualifierParams = qualifierParams.map(solution.substitute)
      signatureType           = resolvedValue.runtime match {
                                  case Some(_) => resolvedResult.expressionType
                                  case None    => SymbolicEvaluator2.evaluateToNormalForm(typeStack.as(typeStack.value.signature))
                                }
      runtime                 = resolvedValue.runtime.map(_ => typeStack.as(resolvedResult.expression))
      _                      <-
        debug[CompilerIO](
          s"Produced symbolic checked (of ${resolvedValue.vfqn.show}) signature: ${signatureType.show}"
        )
    } yield TypeCheckedValue(
      resolvedValue.vfqn,
      resolvedValue.name.as(QualifiedName.from(resolvedValue.name.value, resolvedQualifierParams)),
      signatureType,
      runtime
    )
  }

  private def resolveQualifierParams(
      name: Sourced[ResolveQualifiedName]
  ): TypeGraphIO[Seq[ExpressionValue]] =
    name.value.qualifier match {
      case ResolveQualifier.AbilityImplementation(_, params) =>
        params.traverse { param =>
          SymbolicEvaluator2
            .typeCheck(
              Seq(name.as(OperatorResolvedExpression.fromExpression(MatchDesugaredExpression.fromExpression(param))))
            )
            .map(_.expressionType)
        }
      case _                                                 => Seq.empty[ExpressionValue].pure[TypeGraphIO]
    }
}
