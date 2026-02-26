package com.vanillasource.eliot.eliotc.operator.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.Fixity
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import TokenClassifier.AnnotatedPart
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.matchdesugar.fact.{MatchDesugaredExpression, MatchDesugaredValue}
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced

class OperatorResolverProcessor
    extends TransformationProcessor[MatchDesugaredValue.Key, OperatorResolvedValue.Key](key =>
      MatchDesugaredValue.Key(key.vfqn)
    ) {

  override protected def generateFromKeyAndFact(
      key: OperatorResolvedValue.Key,
      desugaredValue: MatchDesugaredValue
  ): CompilerIO[OperatorResolvedValue] =
    for {
      resolvedRuntime <- desugaredValue.runtime.traverse(expr => resolveInExpression(expr.value).map(expr.as))
    } yield OperatorResolvedValue(
      desugaredValue.vfqn,
      desugaredValue.name,
      resolvedRuntime,
      convertTypeStack(desugaredValue.typeStack),
      convertParamConstraints(desugaredValue.paramConstraints)
    )

  private def resolveInExpression(expr: MatchDesugaredExpression): CompilerIO[OperatorResolvedExpression] =
    expr match {
      case MatchDesugaredExpression.FlatExpression(parts)                       =>
        for {
          resolvedParts <- parts.traverse(resolveInTypeStack)
          result        <- resolveFlatExpression(resolvedParts)
        } yield result
      case MatchDesugaredExpression.FunctionApplication(target, arg)            =>
        for {
          resolvedTarget <- resolveInTypeStack(target)
          resolvedArg    <- resolveInTypeStack(arg)
        } yield OperatorResolvedExpression.FunctionApplication(resolvedTarget, resolvedArg)
      case MatchDesugaredExpression.FunctionLiteral(paramName, paramType, body) =>
        resolveInTypeStack(body).map(OperatorResolvedExpression.FunctionLiteral(paramName, paramType.map(convertTypeStack), _))
      case MatchDesugaredExpression.IntegerLiteral(v)                           =>
        OperatorResolvedExpression.IntegerLiteral(v).pure[CompilerIO]
      case MatchDesugaredExpression.StringLiteral(v)                            =>
        OperatorResolvedExpression.StringLiteral(v).pure[CompilerIO]
      case MatchDesugaredExpression.ParameterReference(v)                       =>
        OperatorResolvedExpression.ParameterReference(v).pure[CompilerIO]
      case MatchDesugaredExpression.ValueReference(name, typeArgs)              =>
        OperatorResolvedExpression.ValueReference(name, typeArgs.map(ta => ta.map(OperatorResolvedExpression.fromExpression))).pure[CompilerIO]
    }

  private def resolveInTypeStack(
      stack: Sourced[TypeStack[MatchDesugaredExpression]]
  ): CompilerIO[Sourced[TypeStack[OperatorResolvedExpression]]] =
    stack.value.levels.traverse(resolveInExpression).map(levels => stack.as(TypeStack(levels)))

  private def resolveFlatExpression(
      parts: Seq[Sourced[TypeStack[OperatorResolvedExpression]]]
  ): CompilerIO[OperatorResolvedExpression] =
    for {
      annotated <- parts.traverse(annotatePart)
      tokens     = TokenClassifier.classifyTokens(annotated)
      afterPost  = TokenClassifier.applyPostfix(tokens)
      afterPre   = TokenClassifier.applyPrefix(afterPost)
      result    <- InfixPrecedenceResolver.resolve(afterPre)
    } yield result

  private def annotatePart(part: Sourced[TypeStack[OperatorResolvedExpression]]): CompilerIO[AnnotatedPart] =
    part.value.signature match {
      case OperatorResolvedExpression.ValueReference(vfqnSrc, _) =>
        for {
          resolved <- getFactOrAbort(MatchDesugaredValue.Key(vfqnSrc.value))
        } yield AnnotatedPart(part, resolved.fixity, Some(vfqnSrc.value))
      case _                                                     => AnnotatedPart(part, Fixity.Application, None).pure[CompilerIO]
    }

  private def convertTypeStack(
      stack: Sourced[TypeStack[MatchDesugaredExpression]]
  ): Sourced[TypeStack[OperatorResolvedExpression]] =
    stack.map(ts => TypeStack(ts.levels.map(OperatorResolvedExpression.fromExpression)))

  private def convertParamConstraints(
      constraints: Map[String, Seq[MatchDesugaredValue.ResolvedAbilityConstraint]]
  ): Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]] =
    constraints.map { (key, cs) =>
      key -> cs.map(c =>
        OperatorResolvedValue.ResolvedAbilityConstraint(
          c.abilityFQN,
          c.typeArgs.map(OperatorResolvedExpression.fromExpression)
        )
      )
    }
}
