package com.vanillasource.eliot.eliotc.operator

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.Fixity
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.operator.TokenClassifier.AnnotatedPart
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, ResolvedValue}
import com.vanillasource.eliot.eliotc.source.content.Sourced

class OperatorResolverProcessor
    extends TransformationProcessor[ResolvedValue.Key, OperatorResolvedValue.Key](key => ResolvedValue.Key(key.vfqn)) {

  override protected def generateFromKeyAndFact(
      key: OperatorResolvedValue.Key,
      resolvedValue: ResolvedValue
  ): CompilerIO[OperatorResolvedValue] =
    for {
      resolvedRuntime <- resolvedValue.runtime.traverse(expr => resolveInExpression(expr.value).map(expr.as))
    } yield OperatorResolvedValue(
      resolvedValue.vfqn,
      resolvedValue.name,
      resolvedRuntime,
      resolvedValue.typeStack,
      resolvedValue.paramConstraints
    )

  private def resolveInExpression(expr: Expression): CompilerIO[Expression] =
    expr match {
      case Expression.FlatExpression(parts)                       =>
        for {
          resolvedParts <- parts.traverse(resolveInTypeStack)
          result        <- resolveFlatExpression(resolvedParts)
        } yield result
      case Expression.FunctionApplication(target, arg)            =>
        for {
          resolvedTarget <- resolveInTypeStack(target)
          resolvedArg    <- resolveInTypeStack(arg)
        } yield Expression.FunctionApplication(resolvedTarget, resolvedArg)
      case Expression.FunctionLiteral(paramName, paramType, body) =>
        resolveInTypeStack(body).map(Expression.FunctionLiteral(paramName, paramType, _))
      case _                                                      => expr.pure[CompilerIO]
    }

  private def resolveInTypeStack(
      stack: Sourced[TypeStack[Expression]]
  ): CompilerIO[Sourced[TypeStack[Expression]]] =
    stack.value.levels.traverse(resolveInExpression).map(levels => stack.as(TypeStack(levels)))

  private def resolveFlatExpression(parts: Seq[Sourced[TypeStack[Expression]]]): CompilerIO[Expression] =
    for {
      annotated <- parts.traverse(annotatePart)
      tokens     = TokenClassifier.classifyTokens(annotated)
      afterPost  = TokenClassifier.applyPostfix(tokens)
      afterPre   = TokenClassifier.applyPrefix(afterPost)
      result    <- InfixPrecedenceResolver.resolve(afterPre)
    } yield result

  private def annotatePart(part: Sourced[TypeStack[Expression]]): CompilerIO[AnnotatedPart] =
    part.value.signature match {
      case Expression.ValueReference(vfqnSrc, _) =>
        for {
          resolved <- getFactOrAbort(ResolvedValue.Key(vfqnSrc.value))
        } yield AnnotatedPart(part, resolved.fixity, Some(vfqnSrc.value))
      case _                                     => AnnotatedPart(part, Fixity.Application, None).pure[CompilerIO]
    }
}
