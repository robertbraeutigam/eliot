package com.vanillasource.eliot.eliotc.resolve2.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.Expression.*
import com.vanillasource.eliot.eliotc.core.fact.{TypeStack, Expression as CoreExpression}
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
    val namedValue    = unifiedValue.namedValue
    // Pre-add generic params from signature so they're visible in runtime
    val genericParams = collectGenericParams(namedValue.typeStack)
    val scope         = genericParams.foldLeft(ValueResolverScope(unifiedValue.dictionary, Set.empty, Map.empty)) { (s, name) =>
      s.addPreAddedParam(name)
    }

    val resolveProgram = for {
      resolvedRuntime <- namedValue.runtime.traverse(expr => resolveExpression(expr).map(namedValue.name.as))
      resolvedStack   <- resolveTypeStack(namedValue.name.as(namedValue.typeStack))
      _               <- debug[ScopedIO](s"Resolved ${key.vfqn.show}: ${resolvedStack.value.show}")
    } yield ResolvedValue(
      unifiedValue.vfqn,
      namedValue.name,
      resolvedRuntime,
      resolvedStack
    )

    resolveProgram.runA(scope)
  }

  /** Collects generic parameter names from the signature. Generic params are FunctionLiterals with a kind annotation
    * (Type or Function returning Type) as param type.
    */
  private def collectGenericParams(stack: TypeStack[CoreExpression]): Seq[String] =
    collectGenericParamsFromExpr(stack.signature)

  private def collectGenericParamsFromExpr(expr: CoreExpression): Seq[String] =
    expr match {
      case FunctionLiteral(paramName, paramType, body) if isKindAnnotation(paramType) =>
        paramName.value +: collectGenericParamsFromExpr(body.value.signature)
      case _                                                                          => Seq.empty
    }

  /** Check if a type stack represents a kind annotation (for universal introductions). A kind annotation is:
    *   - Type (for simple type parameters)
    *   - Function(Type, Type) (for type constructors of arity 1)
    *   - Function(Type, Function(Type, Type)) (for type constructors of arity 2)
    *   - etc.
    */
  private def isKindAnnotation(stack: TypeStack[CoreExpression]): Boolean =
    stack.levels.length == 1 && isKindExpression(stack.signature)

  private def isKindExpression(expr: CoreExpression): Boolean =
    expr match {
      case NamedValueReference(name, None) =>
        name.value == "Type"
      case FunctionApplication(targetStack, argStack) =>
        // Check if this is Function(<kind>, <kind>) - a function from kinds to kinds
        targetStack.value.signature match {
          case FunctionApplication(fnStack, argKindStack) =>
            isFunctionReference(fnStack.value.signature) &&
            isKindExpression(argKindStack.value.signature) &&
            isKindExpression(argStack.value.signature)
          case _ => false
        }
      case _ => false
    }

  private def isFunctionReference(expr: CoreExpression): Boolean =
    expr match {
      case NamedValueReference(name, None) => name.value == "Function"
      case _                               => false
    }

  /** Resolves a type stack from top (most abstract) to bottom (signature). Expression variables from above are visible
    * on below levels, but go out of scope outside the type stack.
    */
  private def resolveTypeStack(
      stack: Sourced[TypeStack[CoreExpression]]
  ): ScopedIO[Sourced[TypeStack[Expression]]] =
    withLocalScope {
      stack.value.levels.reverse
        .traverse(resolveExpression)
        .map(es => stack.as(TypeStack(es.reverse)))
    }

  private def resolveExpression(expression: CoreExpression): ScopedIO[Expression] =
    expression match {
      case NamedValueReference(nameSrc, None)          =>
        isParameter(nameSrc.value).flatMap { isParam =>
          if (isParam) {
            Expression.ParameterReference(nameSrc).pure[ScopedIO]
          } else if (nameSrc.value == "Type") {
            // Type is a special builtin for type-level parameters
            val typeVfqn = ValueFQN(ModuleName(Seq("eliot", "compile"), "Type"), "Type")
            Expression.ValueReference(nameSrc.as(typeVfqn)).pure[ScopedIO]
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
          resolvedTarget <- resolveTypeStack(targetStack)
          resolvedArg    <- resolveTypeStack(argStack)
        } yield Expression.FunctionApplication(resolvedTarget, resolvedArg)
      case FunctionLiteral(paramName, paramType, body) =>
        for {
          resolvedParamType <- resolveTypeStack(paramName.as(paramType))
          resolvedBody      <- withLocalScope {
                                 for {
                                   _    <- addParameter(paramName)
                                   body <- resolveTypeStack(body)
                                 } yield body
                               }
        } yield Expression.FunctionLiteral(paramName, resolvedParamType, resolvedBody)
      case IntegerLiteral(s @ Sourced(_, _, value))    =>
        Expression.IntegerLiteral(s.as(BigInt(value))).pure[ScopedIO]
      case StringLiteral(s @ Sourced(_, _, value))     =>
        Expression.StringLiteral(s.as(value)).pure[ScopedIO]
    }
}
