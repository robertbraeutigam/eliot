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
      resolvedRuntime     <- desugaredValue.runtime.traverse(expr => resolveInExpression(expr.value).map(expr.as))
      resolvedTypeStack   <- resolveInTypeStack(desugaredValue.typeStack)
      resolvedConstraints <- resolveParamConstraints(desugaredValue.paramConstraints)
    } yield OperatorResolvedValue(
      desugaredValue.vfqn,
      desugaredValue.name,
      resolvedRuntime,
      resolvedTypeStack,
      resolvedConstraints
    )

  private def resolveInExpression(expr: MatchDesugaredExpression): CompilerIO[OperatorResolvedExpression] =
    expr match {
      case MatchDesugaredExpression.FlatExpression(parts)                       =>
        for {
          resolvedParts <- parts.traverse(part => resolveInExpression(part.value.signature).map(part.as))
          result        <- resolveFlatExpression(resolvedParts)
        } yield result
      case MatchDesugaredExpression.FunctionApplication(target, arg)            =>
        for {
          resolvedTarget <- resolveInExpression(target.value).map(target.as)
          resolvedArg    <- resolveInExpression(arg.value).map(arg.as)
        } yield OperatorResolvedExpression.FunctionApplication(resolvedTarget, resolvedArg)
      case MatchDesugaredExpression.FunctionLiteral(paramName, paramType, body) =>
        for {
          resolvedParamType <- paramType.traverse(resolveInTypeStack)
          resolvedBody      <- resolveInExpression(body.value.signature).map(body.as)
        } yield OperatorResolvedExpression.FunctionLiteral(paramName, resolvedParamType, resolvedBody)
      case MatchDesugaredExpression.IntegerLiteral(v)                           =>
        OperatorResolvedExpression.IntegerLiteral(v).pure[CompilerIO]
      case MatchDesugaredExpression.StringLiteral(v)                            =>
        OperatorResolvedExpression.StringLiteral(v).pure[CompilerIO]
      case MatchDesugaredExpression.ParameterReference(v)                       =>
        OperatorResolvedExpression.ParameterReference(v).pure[CompilerIO]
      case MatchDesugaredExpression.ValueReference(name, typeArgs)              =>
        typeArgs.traverse(ta => resolveInExpression(ta.value).map(ta.as)).map(OperatorResolvedExpression.ValueReference(name, _))
    }

  private def resolveInTypeStack(
      stack: Sourced[TypeStack[MatchDesugaredExpression]]
  ): CompilerIO[Sourced[TypeStack[OperatorResolvedExpression]]] =
    stack.value.levels.traverse(resolveInExpression).map(levels => stack.as(TypeStack(levels)))

  private def resolveFlatExpression(
      parts: Seq[Sourced[OperatorResolvedExpression]]
  ): CompilerIO[OperatorResolvedExpression] =
    for {
      annotated <- parts.traverse(annotatePart)
      tokens     = TokenClassifier.classifyTokens(annotated)
      afterPost  = TokenClassifier.applyPostfix(tokens)
      afterPre   = TokenClassifier.applyPrefix(afterPost)
      result    <- InfixPrecedenceResolver.resolve(afterPre)
    } yield result

  private def annotatePart(part: Sourced[OperatorResolvedExpression]): CompilerIO[AnnotatedPart] =
    part.value match {
      case OperatorResolvedExpression.ValueReference(vfqnSrc, _) =>
        for {
          resolved <- getFactOrAbort(MatchDesugaredValue.Key(vfqnSrc.value))
        } yield AnnotatedPart(part, resolved.fixity, Some(vfqnSrc.value))
      case _                                                     => AnnotatedPart(part, Fixity.Application, None).pure[CompilerIO]
    }

  private def resolveParamConstraints(
      constraints: Map[String, Seq[MatchDesugaredValue.ResolvedAbilityConstraint]]
  ): CompilerIO[Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]]] =
    constraints.toSeq
      .traverse { (key, cs) =>
        cs.traverse(c =>
          c.typeArgs.traverse(resolveInExpression).map(OperatorResolvedValue.ResolvedAbilityConstraint(c.abilityFQN, _))
        ).map(key -> _)
      }
      .map(_.toMap)
}
