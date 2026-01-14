package com.vanillasource.eliot.eliotc.resolve2.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{ExpressionStack, Expression as CoreExpression}
import com.vanillasource.eliot.eliotc.core.fact.Expression.*
import com.vanillasource.eliot.eliotc.module2.fact.{ModuleName, UnifiedModuleValue, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve2.fact.{Expression, ResolvedValue}
import com.vanillasource.eliot.eliotc.resolve2.processor.ValueResolverScope.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

class ValueResolver
    extends TransformationProcessor[UnifiedModuleValue.Key, ResolvedValue.Key](key =>
      UnifiedModuleValue.Key(key.vfqn)
    ) {

  override protected def generateFromKeyAndFact(
      key: ResolvedValue.Key,
      unifiedValue: UnifiedModuleValue
  ): CompilerIO[ResolvedValue] = {
    val namedValue = unifiedValue.namedValue
    val scope      = ValueResolverScope(unifiedValue.dictionary, Map.empty)

    val resolveProgram = for {
      resolvedType  <- resolveExpressionStack(namedValue.name.as(namedValue.typeStack))
      resolvedValue <- namedValue.value.traverse(v => resolveExpression(v))
    } yield ResolvedValue(
      unifiedValue.vfqn,
      namedValue.name,
      resolvedType,
      resolvedValue
    )

    resolveProgram.runA(scope)
  }

  // FIXME: all expressions should be resolved from the top (i.e. the most abstract) to the bottom (i.e. runtime value)
  // Expression variables from above should be visible on below levels, but go out of scope outside of the expression stack.
  private def resolveExpressionStack(stack: Sourced[ExpressionStack]): ScopedIO[Sourced[Expression]] =
    stack.value.expressions match {
      case Seq()     =>
        (compilerError(stack.as("Empty expression stack.")) *> abort[Sourced[Expression]]).liftToScoped
      case Seq(expr) => resolveExpression(stack.as(expr))
      case exprs     =>
        (compilerError(stack.as(s"Expression stack with multiple expressions not supported.")) *> abort[
          Sourced[Expression]
        ]).liftToScoped
    }

  private def resolveExpression(expr: Sourced[CoreExpression]): ScopedIO[Sourced[Expression]] =
    expr.value match {
      case NamedValueReference(nameSrc, None)          =>
        isParameter(nameSrc.value).flatMap { isParam =>
          if (isParam) {
            expr.as(Expression.ParameterReference(nameSrc)).pure[ScopedIO]
          } else {
            getValue(nameSrc.value).flatMap {
              case Some(vfqn) =>
                expr.as(Expression.ValueReference(nameSrc.as(vfqn))).pure[ScopedIO]
              case None       =>
                (compilerError(nameSrc.as("Name not defined.")) *> abort[Sourced[Expression]]).liftToScoped
            }
          }
        }
      case NamedValueReference(nameSrc, Some(qualSrc)) =>
        val moduleName = ModuleName.parse(qualSrc.value)
        val vfqn       = ValueFQN(moduleName, nameSrc.value)
        val outline    = Sourced.outline(Seq(qualSrc, nameSrc))

        getFact(UnifiedModuleValue.Key(vfqn)).liftToScoped.flatMap {
          case Some(_) => expr.as(Expression.ValueReference(outline.as(vfqn))).pure[ScopedIO]
          case None    => (compilerError(nameSrc.as("Name not defined.")) *> abort[Sourced[Expression]]).liftToScoped
        }

      case FunctionApplication(targetStack, argStack) =>
        for {
          resolvedTarget <- resolveExpressionStack(targetStack)
          resolvedArg    <- resolveExpressionStack(argStack)
        } yield expr.as(Expression.FunctionApplication(resolvedTarget, resolvedArg))

      case FunctionLiteral(paramName, paramType, body) =>
        for {
          resolvedParamType <- resolveExpressionStack(paramName.as(paramType))
          _                 <- addParameter(paramName)
          resolvedBody      <- resolveExpressionStack(body)
        } yield expr.as(Expression.FunctionLiteral(paramName, resolvedParamType, resolvedBody))

      case IntegerLiteral(s @ Sourced(_, _, value)) =>
        expr.as(Expression.IntegerLiteral(s.as(BigInt(value)))).pure[ScopedIO]

      case StringLiteral(s @ Sourced(_, _, value)) =>
        expr.as(Expression.StringLiteral(s.as(value))).pure[ScopedIO]
    }
}
