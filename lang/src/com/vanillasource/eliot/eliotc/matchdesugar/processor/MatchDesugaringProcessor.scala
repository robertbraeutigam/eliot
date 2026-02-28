package com.vanillasource.eliot.eliotc.matchdesugar.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.matchdesugar.fact.{MatchDesugaredExpression, MatchDesugaredValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, ResolvedValue}
import com.vanillasource.eliot.eliotc.source.content.Sourced

class MatchDesugaringProcessor
    extends TransformationProcessor[ResolvedValue.Key, MatchDesugaredValue.Key](key =>
      ResolvedValue.Key(key.vfqn)
    )
    with Logging {

  private lazy val dataMatchDesugarer = new DataMatchDesugarer(desugarMatch, desugarInTypeStack)
  private lazy val typeMatchDesugarer = new TypeMatchDesugarer(desugarInTypeStack, dataMatchDesugarer.buildFieldLambdas)

  override protected def generateFromKeyAndFact(
      key: MatchDesugaredValue.Key,
      resolvedValue: ResolvedValue
  ): CompilerIO[MatchDesugaredValue] =
    for {
      desugaredRuntime <- resolvedValue.runtime.traverse(expr => desugarExpression(expr.value).map(expr.as))
    } yield MatchDesugaredValue(
      resolvedValue.vfqn,
      resolvedValue.name,
      desugaredRuntime.map(_.map(MatchDesugaredExpression.fromExpression)),
      convertTypeStack(resolvedValue.typeStack),
      convertParamConstraints(resolvedValue.paramConstraints),
      resolvedValue.fixity,
      resolvedValue.precedence
    )

  private def convertTypeStack(
      stack: Sourced[TypeStack[Expression]]
  ): Sourced[TypeStack[MatchDesugaredExpression]] =
    stack.map(ts => TypeStack(ts.levels.map(MatchDesugaredExpression.fromExpression)))

  private def convertParamConstraints(
      constraints: Map[String, Seq[ResolvedValue.ResolvedAbilityConstraint]]
  ): Map[String, Seq[MatchDesugaredValue.ResolvedAbilityConstraint]] =
    constraints.map { (key, cs) =>
      key -> cs.map(c =>
        MatchDesugaredValue.ResolvedAbilityConstraint(
          c.abilityFQN,
          c.typeArgs.map(MatchDesugaredExpression.fromExpression)
        )
      )
    }

  private def desugarExpression(expr: Expression): CompilerIO[Expression] =
    expr match {
      case Expression.MatchExpression(scrutinee, cases) => desugarMatch(scrutinee, cases)
      case other                                        => Expression.mapChildrenM(desugarExpression)(other)
    }

  private def desugarInTypeStack(
      stack: Sourced[TypeStack[Expression]]
  ): CompilerIO[Sourced[TypeStack[Expression]]] =
    stack.value.levels.traverse(desugarExpression).map(levels => stack.as(TypeStack(levels)))

  private def desugarMatch(
      scrutinee: Sourced[TypeStack[Expression]],
      cases: Seq[Expression.MatchCase]
  ): CompilerIO[Expression] =
    if (TypeMatchDesugarer.isTypeMatch(cases)) typeMatchDesugarer.desugar(scrutinee, cases)
    else dataMatchDesugarer.desugar(scrutinee, cases)
}
