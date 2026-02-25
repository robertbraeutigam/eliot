package com.vanillasource.eliot.eliotc.resolve.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName as CoreQualifiedName, Qualifier as CoreQualifier}
import com.vanillasource.eliot.eliotc.core.fact.Expression.*
import com.vanillasource.eliot.eliotc.core.fact.{Fixity as CoreFixity, NamedValue, PrecedenceDeclaration as CorePrecedenceDeclaration, TypeStack, Expression as CoreExpression}
import com.vanillasource.eliot.eliotc.eval.fact.Types.typeFQN
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, UnifiedModuleValue, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.{AbilityFQN, Expression, Fixity, PrecedenceDeclaration, QualifiedName, Qualifier, ResolvedValue}
import com.vanillasource.eliot.eliotc.resolve.processor.ValueResolverScope.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

import scala.collection.immutable.{AbstractSeq, LinearSeq}

class ValueResolver
    extends TransformationProcessor[UnifiedModuleValue.Key, ResolvedValue.Key](key => UnifiedModuleValue.Key(key.vfqn))
    with Logging {

  override protected def generateFromKeyAndFact(
      key: ResolvedValue.Key,
      unifiedValue: UnifiedModuleValue
  ): CompilerIO[ResolvedValue] = {
    val namedValue    = unifiedValue.namedValue
    val genericParams = collectGenericParams(namedValue.typeStack)
    val scope         = ValueResolverScope(unifiedValue.dictionary, genericParams.toSet)

    val resolveProgram = for {
      resolvedRuntime     <-
        namedValue.runtime.traverse(expr => resolveExpression(expr, true).map(namedValue.qualifiedName.as))
      resolvedStack       <- resolveTypeStack(namedValue.qualifiedName.as(namedValue.typeStack), false)
      resolvedName        <- convertQualifiedName(namedValue.qualifiedName)
      resolvedConstraints <- resolveParamConstraints(namedValue.paramConstraints)
      resolvedPrecedence  <- resolvePrecedenceDeclarations(namedValue.precedence)
      _                   <- debug[ScopedIO](s"Resolved ${key.vfqn.show} type: ${resolvedStack.value.show}")
      _                   <- debug[ScopedIO](
                               s"Resolved ${key.vfqn.show} runtime: ${resolvedRuntime.map(_.value.show).getOrElse("<abstract>")}"
                             )
    } yield ResolvedValue(
      unifiedValue.vfqn,
      resolvedName,
      resolvedRuntime,
      resolvedStack,
      resolvedConstraints,
      convertFixity(namedValue.fixity),
      resolvedPrecedence
    )

    resolveProgram.runA(scope)
  }

  private def convertQualifiedName(
      name: Sourced[CoreQualifiedName]
  ): ScopedIO[Sourced[QualifiedName]] =
    convertQualifier(name.value.qualifier).map(q => name.map(n => QualifiedName(n.name, q)))

  private def convertQualifier(qualifier: CoreQualifier): ScopedIO[Qualifier] =
    qualifier match {
      case CoreQualifier.Default                             => (Qualifier.Default: Qualifier).pure[ScopedIO]
      case CoreQualifier.Type                                => (Qualifier.Type: Qualifier).pure[ScopedIO]
      case CoreQualifier.Ability(n)                          => (Qualifier.Ability(n): Qualifier).pure[ScopedIO]
      case CoreQualifier.AbilityImplementation(name, params) =>
        for {
          resolvedName  <- resolveAbilityName(name)
          resolvedTypes <- params.traverse(resolveExpression(_, false))
        } yield Qualifier.AbilityImplementation(resolvedName, resolvedTypes)
    }

  private def resolveAbilityName(name: Sourced[String]): ScopedIO[AbilityFQN] =
    getAbility(name.value).flatMap {
      case Some(abilityName) => abilityName.pure[ScopedIO]
      case None              => compilerAbort(name.as(s"Ability not found.")).liftToScoped
    }

  private def resolveParamConstraints(
      paramConstraints: Map[String, Seq[NamedValue.CoreAbilityConstraint]]
  ): ScopedIO[Map[String, Seq[ResolvedValue.ResolvedAbilityConstraint]]] =
    paramConstraints.toSeq
      .traverse { case (paramName, constraints) =>
        constraints
          .traverse { c =>
            for {
              abilityFQN   <- resolveAbilityName(c.abilityName)
              resolvedArgs <- c.typeArgs.traverse(resolveExpression(_, false))
            } yield ResolvedValue.ResolvedAbilityConstraint(abilityFQN, resolvedArgs)
          }
          .map(paramName -> _)
      }
      .map(_.toMap)

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

  private def isKindAnnotation(stackMaybe: Option[TypeStack[CoreExpression]]): Boolean = {
    stackMaybe match {
      case Some(stack) => stack.levels.length == 1 && isKindExpression(stack.signature)
      case None        => false
    }
  }

  private def isKindExpression(expr: CoreExpression): Boolean =
    expr match {
      case NamedValueReference(name, None, _)         =>
        name.value.name === "Type"
      case FunctionApplication(targetStack, argStack) =>
        targetStack.value.signature match {
          case FunctionApplication(fnStack, argKindStack) =>
            isFunctionReference(fnStack.value.signature) &&
            isKindExpression(argKindStack.value.signature) &&
            isKindExpression(argStack.value.signature)
          case _                                          => false
        }
      case _                                          => false
    }

  private def isFunctionReference(expr: CoreExpression): Boolean =
    expr match {
      case NamedValueReference(name, None, _) =>
        name.value === CoreQualifiedName("Function", CoreQualifier.Type)
      case _                                  => false
    }

  /** Resolves a type stack from top (most abstract) to bottom (signature). Expression variables from above are visible
    * on below levels, but go out of scope outside the type stack.
    *
    * @param runtime
    *   Whether the signature (bottom level) is in runtime context. Higher levels are always type-level.
    */
  private def resolveTypeStack(
      stack: Sourced[TypeStack[CoreExpression]],
      runtime: Boolean
  ): ScopedIO[Sourced[TypeStack[Expression]]] =
    withLocalScope {
      val levels    = stack.value.levels.reverse
      val numLevels = levels.length
      levels.zipWithIndex
        .traverse { case (expression, idx) =>
          val isSignature = idx == numLevels - 1
          resolveExpression(expression, if (isSignature) runtime else false)
        }
        .map(es => stack.as(TypeStack(es.reverse)))
    }

  private def resolveExpression(expression: CoreExpression, runtime: Boolean): ScopedIO[Expression] =
    expression match {
      case NamedValueReference(nameSrc, None, typeArgExprs)          =>
        isParameter(nameSrc.value.name).flatMap { isParam =>
          if (isParam) {
            // This is a parameter defined in the expression - parameters can't have explicit type args
            Expression.ParameterReference(nameSrc.map(_.name)).pure[ScopedIO]
          } else if (nameSrc.value.name === "Type") {
            // Type is a special builtin for type-level parameters (same on all plains)
            Expression.ValueReference(nameSrc.as(typeFQN)).pure[ScopedIO]
          } else {
            // This should be a referenced value
            getValue(nameSrc.value).flatMap {
              case Some(vfqn) =>
                // This is a normal value; resolve any explicit type args
                typeArgExprs
                  .traverse(arg => resolveExpression(arg.value, false).map(arg.as(_)))
                  .map(resolvedTypeArgs => Expression.ValueReference(nameSrc.as(vfqn), resolvedTypeArgs))
              case None       =>
                // Not a normal value, it might be coming from an ability
                searchAbilities(nameSrc.value.name).flatMap {
                  case Nil         => compilerAbort(nameSrc.as("Name not defined.")).liftToScoped
                  case head :: Nil => Expression.ValueReference(nameSrc.as(head)).pure[ScopedIO]
                  case as          =>
                    compilerAbort(
                      nameSrc.as("Name defined in multiple abilities."),
                      Seq(
                        s"Abilities: ${as.map(_.name.qualifier.asInstanceOf[CoreQualifier.Ability].name).mkString(", ")}"
                      )
                    ).liftToScoped
                }
            }
          }
        }
      case NamedValueReference(nameSrc, Some(qualSrc), typeArgExprs) =>
        val moduleName = ModuleName.parse(qualSrc.value)
        val vfqn       = ValueFQN(moduleName, nameSrc.value)
        val outline    = Sourced.outline(Seq(qualSrc, nameSrc))

        getFact(UnifiedModuleValue.Key(vfqn)).liftToScoped.flatMap {
          case Some(_) =>
            typeArgExprs
              .traverse(arg => resolveExpression(arg.value, false).map(arg.as(_)))
              .map(resolvedTypeArgs => Expression.ValueReference(outline.as(vfqn), resolvedTypeArgs))
          case None    => compilerAbort(nameSrc.as("Qualified named value not available.")).liftToScoped
        }
      case FunctionApplication(targetStack, argStack)  =>
        for {
          resolvedTarget <- resolveTypeStack(targetStack, runtime)
          resolvedArg    <- resolveTypeStack(argStack, runtime)
        } yield Expression.FunctionApplication(resolvedTarget, resolvedArg)
      case FunctionLiteral(paramName, paramType, body) =>
        for {
          resolvedParamType <- paramType.traverse(t => resolveTypeStack(paramName.as(t), false))
          resolvedBody      <- withLocalScope {
                                 for {
                                   _    <- addParameter(paramName.value)
                                   body <- resolveTypeStack(body, runtime)
                                 } yield body
                               }
        } yield Expression.FunctionLiteral(paramName, resolvedParamType, resolvedBody)
      case IntegerLiteral(s @ Sourced(_, _, value))    =>
        Expression.IntegerLiteral(s.as(BigInt(value))).pure[ScopedIO]
      case StringLiteral(s @ Sourced(_, _, value))     =>
        Expression.StringLiteral(s.as(value)).pure[ScopedIO]
      case FlatExpression(parts)                       =>
        parts.traverse(part => resolveTypeStack(part, runtime)).map(Expression.FlatExpression(_))
    }

  private def convertFixity(fixity: CoreFixity): Fixity = fixity match {
    case CoreFixity.Application    => Fixity.Application
    case CoreFixity.Prefix         => Fixity.Prefix
    case CoreFixity.Infix(assoc)   => Fixity.Infix(convertAssociativity(assoc))
    case CoreFixity.Postfix        => Fixity.Postfix
  }

  private def convertAssociativity(assoc: CoreFixity.Associativity): Fixity.Associativity = assoc match {
    case CoreFixity.Associativity.Left  => Fixity.Associativity.Left
    case CoreFixity.Associativity.Right => Fixity.Associativity.Right
    case CoreFixity.Associativity.None  => Fixity.Associativity.None
  }

  private def resolvePrecedenceDeclarations(
      decls: Seq[CorePrecedenceDeclaration]
  ): ScopedIO[Seq[PrecedenceDeclaration]] =
    decls.traverse(resolvePrecedenceDeclaration)

  private def resolvePrecedenceDeclaration(
      decl: CorePrecedenceDeclaration
  ): ScopedIO[PrecedenceDeclaration] =
    decl.targets
      .traverse(target =>
        getValue(CoreQualifiedName(target.value, CoreQualifier.Default)).flatMap {
          case Some(vfqn) => target.as(vfqn).pure[ScopedIO]
          case None       => compilerAbort(target.as("Precedence target name not defined.")).liftToScoped
        }
      )
      .map(resolvedTargets => PrecedenceDeclaration(convertRelation(decl.relation), resolvedTargets))

  private def convertRelation(rel: CorePrecedenceDeclaration.Relation): PrecedenceDeclaration.Relation =
    rel match {
      case CorePrecedenceDeclaration.Relation.Above => PrecedenceDeclaration.Relation.Above
      case CorePrecedenceDeclaration.Relation.Below => PrecedenceDeclaration.Relation.Below
      case CorePrecedenceDeclaration.Relation.At    => PrecedenceDeclaration.Relation.At
    }
}
