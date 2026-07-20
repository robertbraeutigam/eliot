package com.vanillasource.eliot.eliotc.matchdesugar.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.block.fact.BlockDesugaredValue
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.matchdesugar.fact.{MatchDesugaredExpression, MatchDesugaredValue}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, ResolvedValue}
import com.vanillasource.eliot.eliotc.source.content.Sourced

class MatchDesugaringProcessor
    extends TransformationProcessor[BlockDesugaredValue.Key, MatchDesugaredValue.Key](key =>
      BlockDesugaredValue.Key(key.vfqn, key.platform)
    )
    with Logging {

  private lazy val context            = new MatchDesugarContext(desugarMatch, desugarSourced)
  private lazy val dataMatchDesugarer = new DataMatchDesugarer(context)
  private lazy val typeMatchDesugarer = new TypeMatchDesugarer(context)

  override protected def generateFromKeyAndFact(
      key: MatchDesugaredValue.Key,
      blockDesugaredValue: BlockDesugaredValue
  ): CompilerIO[MatchDesugaredValue] =
    for {
      desugaredRuntime <-
        blockDesugaredValue.runtime.traverse(expr => desugarExpression(expr.value, blockDesugaredValue.platform).map(expr.as))
      // Desugar the value's own signature too, not just the runtime body: a `match` written in a type position (the
      // signature) is an ordinary expression and must go through match desugaring like any other, or it survives as a
      // `MatchExpression` into a phase that rejects it.
      desugaredSignature <- desugarSourced(blockDesugaredValue.signature, blockDesugaredValue.platform)
    } yield MatchDesugaredValue(
      blockDesugaredValue.vfqn,
      blockDesugaredValue.name,
      desugaredRuntime.map(_.map(MatchDesugaredExpression.fromExpression)),
      desugaredSignature.map(MatchDesugaredExpression.fromExpression),
      convertParamConstraints(blockDesugaredValue.paramConstraints),
      blockDesugaredValue.fixity,
      blockDesugaredValue.precedence,
      blockDesugaredValue.inferableArity,
      blockDesugaredValue.roleHint,
      blockDesugaredValue.platform
    )

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

  private def desugarExpression(expr: Expression, platform: Platform): CompilerIO[Expression] =
    expr match {
      case Expression.MatchExpression(scrutinee, cases) => desugarMatch(scrutinee, cases, platform)
      case other                                        => Expression.mapChildrenM(desugarExpression(_, platform))(other)
    }

  private def desugarSourced(expr: Sourced[Expression], platform: Platform): CompilerIO[Sourced[Expression]] =
    desugarExpression(expr.value, platform).map(expr.as)

  private def desugarMatch(
      scrutinee: Sourced[Expression],
      cases: Seq[Expression.MatchCase],
      platform: Platform
  ): CompilerIO[Expression] =
    if (TypeMatchDesugarer.isTypeMatch(cases)) typeMatchDesugarer.desugar(scrutinee, cases)(using platform)
    else dataMatchDesugarer.desugar(scrutinee, cases)(using platform)
}
