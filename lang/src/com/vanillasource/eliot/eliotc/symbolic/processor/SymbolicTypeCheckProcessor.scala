package com.vanillasource.eliot.eliotc.symbolic.processor

import cats.syntax.all.*
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
import com.vanillasource.eliot.eliotc.symbolic.processor.SymbolicTypeCheck.typeCheck
import com.vanillasource.eliot.eliotc.symbolic.types.*
import com.vanillasource.eliot.eliotc.symbolic.types.SymbolicType.*

class SymbolicTypeCheckProcessor
    extends TransformationProcessor[OperatorResolvedValue.Key, TypeCheckedValue.Key](key =>
      OperatorResolvedValue.Key(key.vfqn)
    )
    with Logging {

  override protected def generateFromKeyAndFact(
      key: TypeCheckedValue.Key,
      resolvedValue: OperatorResolvedValue
  ): CompilerIO[TypeCheckedValue] = {
    val typeStack  = resolvedValue.typeStack
    val typeLevels = typeStack.value.levels.map(typeStack.as(_))
    val vfqnShow   = resolvedValue.vfqn.show

    for {
      (endState, result)       <- typeCheck(typeLevels.appendSeq(resolvedValue.runtime.toSeq)).run(TypeCheckState())
      _                        <- debug[CompilerIO](s"Constraints (of $vfqnShow): ${endState.constraints.show}")
      solution                 <- ConstraintSolver.solve(endState.constraints, endState.universalVars)
      _                        <- debug[CompilerIO](s"Solution (of $vfqnShow): ${solution.show}")
      (signatureType, runtime) <- resolvedValue.runtime match {
                                    case Some(_) =>
                                      val substitutedResult =
                                        result.transformTypes(st => solution.substitute(st))
                                      (
                                        substitutedResult.expressionType,
                                        Some(typeStack.as(substitutedResult.expression))
                                      ).pure[CompilerIO]
                                    case None    =>
                                      NormalFormEvaluator
                                        .evaluate(typeStack.as(typeStack.value.signature))
                                        .map(st => st -> None)
                                  }
      resolvedQualifierParams  <- resolveQualifierParams(resolvedValue.name)
    } yield TypeCheckedValue(
      resolvedValue.vfqn,
      resolvedValue.name.as(
        QualifiedName.from(
          resolvedValue.name.value,
          resolvedQualifierParams.map(st => solution.substitute(st))
        )
      ),
      signatureType,
      runtime
    )
  }

  private def resolveQualifierParams(
      name: Sourced[ResolveQualifiedName]
  ): CompilerIO[Seq[SymbolicType]] =
    name.value.qualifier match {
      case ResolveQualifier.AbilityImplementation(_, expressions) =>
        expressions.traverse { expression =>
          NormalFormEvaluator.evaluate(
            name.as(OperatorResolvedExpression.fromExpression(MatchDesugaredExpression.fromExpression(expression)))
          )
        }
      case _                                                      => Seq.empty.pure[CompilerIO]
    }
}
