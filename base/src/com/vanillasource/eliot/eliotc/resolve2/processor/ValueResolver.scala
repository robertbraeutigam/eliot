package com.vanillasource.eliot.eliotc.resolve2.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.Expression.*
import com.vanillasource.eliot.eliotc.core.fact.{ExpressionStack, Expression as CoreExpression}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module2.fact.{ModuleName, UnifiedModuleValue, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve2.fact.{Expression, ResolvedValue}
import com.vanillasource.eliot.eliotc.resolve2.processor.ValueResolverScope.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

class ValueResolver
    extends TransformationProcessor[UnifiedModuleValue.Key, ResolvedValue.Key](key => UnifiedModuleValue.Key(key.vfqn))
    with Logging {

  override protected def generateFromKeyAndFact(
      key: ResolvedValue.Key,
      unifiedValue: UnifiedModuleValue
  ): CompilerIO[ResolvedValue] = {
    val namedValue = unifiedValue.namedValue
    // Pre-add generic params from signature so they're visible in runtime
    val genericParams = collectGenericParams(namedValue.value)
    val scope = genericParams.foldLeft(ValueResolverScope(unifiedValue.dictionary, Set.empty, Map.empty)) { (s, name) =>
      s.addPreAddedParam(name)
    }

    val resolveProgram = for {
      resolvedStack <- resolveExpressionStack(namedValue.name.as(namedValue.value))
      _             <- debug[ScopedIO](s"Resolved value name: ${key.vfqn.show}\nValue: ${resolvedStack.value.show}")
    } yield ResolvedValue(
      unifiedValue.vfqn,
      namedValue.name,
      resolvedStack
    )

    resolveProgram.runA(scope)
  }

  /** Collects generic parameter names from the signature. Generic params are FunctionLiterals with empty param type. */
  private def collectGenericParams(stack: ExpressionStack[CoreExpression]): Seq[String] =
    stack.signature.toSeq.flatMap(collectGenericParamsFromExpr)

  private def collectGenericParamsFromExpr(expr: CoreExpression): Seq[String] =
    expr match {
      case FunctionLiteral(paramName, paramType, body) if paramType.expressions.isEmpty =>
        paramName.value +: collectGenericParamsFromExpr(body.value.expressions.head)
      case _ => Seq.empty
    }

  /** Resolves an expression stack from top (most abstract) to bottom (runtime value). Expression variables from above
    * are visible on below levels, but go out of scope outside the expression stack. Returns only the bottom expression
    * as the resolved result.
    */
  private def resolveExpressionStack(
      stack: Sourced[ExpressionStack[CoreExpression]]
  ): ScopedIO[Sourced[ExpressionStack[Expression]]] =
    withLocalScope {
      stack.value.expressions.reverse
        .traverse(resolveExpression)
        .map(es => stack.as(ExpressionStack(es.reverse, stack.value.hasRuntime)))
    }

  private def resolveExpression(expression: CoreExpression): ScopedIO[Expression] =
    expression match {
      case NamedValueReference(nameSrc, None)          =>
        isParameter(nameSrc.value).flatMap { isParam =>
          if (isParam) {
            Expression.ParameterReference(nameSrc).pure[ScopedIO]
          } else {
            // TODO: Hardcoded: anything that's not a parameter reference (above), AND starts with an upper-case letter,
            //  is a reference to a type. Therefore we need to add "$DataType" to the call. This is a hack, fix this later!
            // Note: we can't do this earlier, because we have to know, whether it's a generic parameter
            val valueName = if (nameSrc.value.charAt(0).isUpper) nameSrc.value + "$DataType" else nameSrc.value
            getValue(valueName).flatMap {
              case Some(vfqn) =>
                Expression.ValueReference(nameSrc.as(vfqn)).pure[ScopedIO]
              case None       =>
                compilerAbort(nameSrc.as("Name not defined.")).liftToScoped
            }
          }
        }
      case NamedValueReference(nameSrc, Some(qualSrc)) =>
        val moduleName = ModuleName.parse(qualSrc.value)
        val vfqn       = ValueFQN(moduleName, nameSrc.value)
        val outline    = Sourced.outline(Seq(qualSrc, nameSrc))

        getFact(UnifiedModuleValue.Key(vfqn)).liftToScoped.flatMap {
          case Some(_) => Expression.ValueReference(outline.as(vfqn)).pure[ScopedIO]
          case None    => compilerAbort(nameSrc.as("Name not defined.")).liftToScoped
        }
      case FunctionApplication(targetStack, argStack)  =>
        for {
          resolvedTarget <- resolveExpressionStack(targetStack)
          resolvedArg    <- resolveExpressionStack(argStack)
        } yield Expression.FunctionApplication(resolvedTarget, resolvedArg)
      case FunctionLiteral(paramName, paramType, body) =>
        for {
          resolvedParamType <- resolveExpressionStack(paramName.as(paramType))
          resolvedBody      <- withLocalScope {
                                 for {
                                   _    <- addParameter(paramName)
                                   body <- resolveExpressionStack(body)
                                 } yield body
                               }
        } yield Expression.FunctionLiteral(paramName, resolvedParamType, resolvedBody)
      case IntegerLiteral(s @ Sourced(_, _, value))    =>
        Expression.IntegerLiteral(s.as(BigInt(value))).pure[ScopedIO]
      case StringLiteral(s @ Sourced(_, _, value))     =>
        Expression.StringLiteral(s.as(value)).pure[ScopedIO]
    }
}
