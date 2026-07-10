package com.vanillasource.eliot.eliotc.resolve.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.{PrecedenceDeclaration as AstPrecedenceDeclaration, Visibility}
import com.vanillasource.eliot.eliotc.core.fact.Expression.*
import com.vanillasource.eliot.eliotc.core.fact.{
  NamedValue,
  TypeStack,
  Expression as CoreExpression,
  Pattern as CorePattern,
  PrecedenceDeclaration as CorePrecedenceDeclaration
}
import com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes.{typeFQN, patternMatchAbilityName, typeMatchAbilityName}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{
  ModuleName,
  UnifiedModuleValue,
  ValueFQN,
  QualifiedName as CoreQualifiedName,
  Qualifier as CoreQualifier
}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.*
import com.vanillasource.eliot.eliotc.resolve.processor.ValueResolverScope.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

class ValueResolver
    extends TransformationProcessor[UnifiedModuleValue.Key, ResolvedValue.Key](key =>
      UnifiedModuleValue.Key(key.vfqn, key.platform)
    )
    with Logging {

  override protected def generateFromKeyAndFact(
      key: ResolvedValue.Key,
      unifiedValue: UnifiedModuleValue
  ): CompilerIO[ResolvedValue] = {
    val namedValue    = unifiedValue.namedValue
    val genericParams = collectGenericParams(namedValue.typeStack)
    val scope         =
      ValueResolverScope(
        key.vfqn.moduleName,
        unifiedValue.dictionary,
        unifiedValue.privateNames,
        genericParams.toSet,
        namedValue.qualifiedName.value.qualifier,
        key.platform
      )

    val resolveProgram = for {
      resolvedRuntime     <-
        namedValue.runtime.traverse(expr => resolveExpression(expr.value, true).map(expr.as))
      resolvedStack       <- resolveTypeStack(namedValue.qualifiedName.as(namedValue.typeStack), false)
      resolvedName        <- convertQualifiedName(namedValue.qualifiedName)
      resolvedConstraints <- resolveParamConstraints(namedValue.paramConstraints)
      resolvedDischarged  <- namedValue.dischargedEffects.traverse(resolveAbilityName)
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
      namedValue.fixity,
      resolvedPrecedence,
      namedValue.opaque,
      namedValue.inferableArity,
      namedValue.roleHint,
      key.platform,
      resolvedDischarged
    )

    resolveProgram.runA(scope)
  }

  private def convertQualifiedName(
      name: Sourced[CoreQualifiedName]
  ): ScopedIO[Sourced[QualifiedName]] =
    convertQualifier(name.value.qualifier, name).map(q => name.map(n => QualifiedName(n.name, q)))

  private def convertQualifier(qualifier: CoreQualifier, at: Sourced[?]): ScopedIO[Qualifier] =
    qualifier match {
      case CoreQualifier.Default                               => (Qualifier.Default: Qualifier).pure[ScopedIO]
      case CoreQualifier.Type                                  => (Qualifier.Type: Qualifier).pure[ScopedIO]
      case CoreQualifier.Meta                                  => (Qualifier.Meta: Qualifier).pure[ScopedIO]
      case CoreQualifier.Ability(n)                            => (Qualifier.Ability(n): Qualifier).pure[ScopedIO]
      case CoreQualifier.AbilityImplementation(name, pattern) =>
        // The module qualifier carries the ability name as a bare string (identity is position-independent); borrow the
        // enclosing name's position for any "Ability not found" diagnostic.
        resolveAbilityName(at.as(name)).map(resolvedName => Qualifier.AbilityImplementation(resolvedName, pattern))
    }

  private def resolveAbilityName(name: Sourced[String]): ScopedIO[AbilityFQN] =
    getAbility(name.value).flatMap {
      case Some(abilityName) => abilityName.pure[ScopedIO]
      case None              =>
        // The desugaring-machinery abilities (`PatternMatch`/`TypeMatch`) are referenced by compiler-generated
        // `implement` markers, which should not depend on the user's import scope. They live at fixed FQNs in the
        // `eliot.compiler.internal` package, so resolve them directly instead of requiring an (auto-)import.
        ValueResolver.compilerInternalAbilities.get(name.value) match {
          case Some(abilityFQN) => abilityFQN.pure[ScopedIO]
          case None             => compilerAbort(name.as(s"Ability not found.")).liftToScoped
        }
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

  /** Collects generic parameter names from the signature. Generic params are FunctionLiterals with a type annotation
    * (paramType is Some). All FunctionLiterals in type position are universal intros.
    */
  private def collectGenericParams(stack: TypeStack[CoreExpression]): Seq[String] =
    collectGenericParamsFromExpr(stack.signature)

  private def collectGenericParamsFromExpr(expr: CoreExpression): Seq[String] =
    expr match {
      case FunctionLiteral(paramName, Some(_), body) =>
        paramName.value +: collectGenericParamsFromExpr(body.value.signature)
      case _                                         => Seq.empty
    }

  /** Resolve a bare reference to a member of the *current* ability implementation (found via the implementation scope).
    * An associated type's declaration carries the implementation's generic parameters — they are prepended to every
    * member — so a *bare* reference names the unapplied bound function. Apply it to those generics, which are all in
    * scope right here, so it resolves to the concrete associated type: inside
    * `implement Arithmetic[Int[L1, H1], Int[L2, H2]]`, a method's `AddResult` return resolves to
    * `Int[add(L1, L2), add(H1, H2)]`, not to the λ over the four bounds. Only *bare* references reach this path: an
    * explicitly-applied member reference (`AddResult[S1, S2]`) is a *dispatch* through the ability at the given type
    * arguments — resolved on the ability path in [[resolveExpression]] — never this implementation's own member.
    */
  private def resolveImplementationScopedName(
      implVfqn: ValueFQN,
      nameSrc: Sourced[CoreQualifiedName]
  ): ScopedIO[Expression] =
      for {
        platform    <- getPlatform
        implValue   <- getFactIfProduced(UnifiedModuleValue.Key(implVfqn, platform)).liftToScoped
        params      <- getParameters
        implGenerics =
          implValue.map(v => collectGenericParams(v.namedValue.typeStack)).getOrElse(Seq.empty).takeWhile(params.contains)
        argRefs      = implGenerics.map(g => nameSrc.as(Expression.ParameterReference(nameSrc.as(g)): Expression))
      } yield Expression.ValueReference(nameSrc.as(implVfqn), argRefs)

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

  /** @param applied
    *   Whether this expression is the *target* of an application (the head of an application chain). An upper-case
    *   type application `AddResult[S1, S2]` reaches resolution as a curried application chain over a *bare* head, so
    *   the head's explicit-argument nature is only visible through this flag.
    */
  private def resolveExpression(
      expression: CoreExpression,
      runtime: Boolean,
      applied: Boolean = false
  ): ScopedIO[Expression] =
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
                // In type context, a *bare, unapplied* name prefers the implementation scope (an associated type
                // auto-applied to the impl's generics). An explicitly-applied name (`AddResult[S1, S2]` — explicit
                // bracket args on the reference, or an application chain over the head) is a *dispatch* through the
                // ability at those arguments — the member is an ordinary named value whose leading parameters are the
                // ability's — so it always resolves on the ability path, even inside an implementation.
                val implSearch =
                  if (!runtime && typeArgExprs.isEmpty && !applied) searchImplementationScope(nameSrc.value.name)
                  else None.pure[ScopedIO]
                implSearch.flatMap {
                  case Some(implVfqn) => resolveImplementationScopedName(implVfqn, nameSrc)
                  case None           =>
                    // Not in implementation scope, it might be coming from an ability
                    searchAbilities(nameSrc.value.name).flatMap {
                      case Nil         =>
                        // Check if the name exists as a private import
                        getPrivateName(nameSrc.value).flatMap {
                          case Some(_) => compilerAbort(nameSrc.as("Name is private.")).liftToScoped
                          case None    => compilerAbort(nameSrc.as("Name not defined.")).liftToScoped
                        }
                      case head :: Nil =>
                        // Explicit type arguments are the ability-level dispatch arguments — resolve and keep them
                        // (they were formerly dropped here, leaving the reference unconstrained).
                        typeArgExprs
                          .traverse(arg => resolveExpression(arg.value, false).map(arg.as(_)))
                          .map(resolved => Expression.ValueReference(nameSrc.as(head), resolved))
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
        }
      case NamedValueReference(nameSrc, Some(qualSrc), typeArgExprs) =>
        val moduleName = ModuleName.parse(qualSrc.value)
        val vfqn       = ValueFQN(moduleName, nameSrc.value)
        val outline    = Sourced.outline(Seq(qualSrc, nameSrc))

        getPlatform.flatMap(platform => getFactIfProduced(UnifiedModuleValue.Key(vfqn, platform)).liftToScoped).flatMap {
          case Some(umv) =>
            getCurrentModule.flatMap { currentMod =>
              if (umv.namedValue.visibility == Visibility.Private && moduleName != currentMod) {
                compilerAbort(nameSrc.as("Name is private.")).liftToScoped
              } else {
                typeArgExprs
                  .traverse(arg => resolveExpression(arg.value, false).map(arg.as(_)))
                  .map(resolvedTypeArgs => Expression.ValueReference(outline.as(vfqn), resolvedTypeArgs))
              }
            }
          case None      => compilerAbort(nameSrc.as("Qualified named value not available.")).liftToScoped
        }
      case FunctionApplication(target, arg)                          =>
        for {
          resolvedTarget <- resolveExpression(target.value, runtime, applied = true).map(target.as)
          resolvedArg    <- resolveExpression(arg.value, runtime).map(arg.as)
        } yield Expression.FunctionApplication(resolvedTarget, resolvedArg)
      case FunctionLiteral(paramName, paramType, body)               =>
        for {
          resolvedParamType <- paramType.traverse(t => resolveTypeStack(paramName.as(t), false))
          resolvedBody      <- withLocalScope {
                                 for {
                                   _    <- addParameter(paramName.value)
                                   body <- resolveTypeStack(body, runtime)
                                 } yield body
                               }
        } yield Expression.FunctionLiteral(paramName, resolvedParamType, resolvedBody)
      case IntegerLiteral(s @ Sourced(_, _, value))                  =>
        Expression.IntegerLiteral(s.as(BigInt(value))).pure[ScopedIO]
      case StringLiteral(s @ Sourced(_, _, value))                   =>
        Expression.StringLiteral(s.as(value)).pure[ScopedIO]
      case FlatExpression(parts)                                     =>
        parts.traverse(part => resolveTypeStack(part, runtime)).map(Expression.FlatExpression(_))
      case CoreExpression.MatchExpression(scrutinee, cases)          =>
        for {
          resolvedScrutinee <- resolveTypeStack(scrutinee, runtime)
          resolvedCases     <- cases.traverse { c =>
                                 withLocalScope {
                                   for {
                                     resolvedPattern <- resolvePattern(c.pattern)
                                     resolvedBody    <- resolveTypeStack(c.body, runtime)
                                   } yield Expression.MatchCase(resolvedPattern, resolvedBody)
                                 }
                               }
        } yield Expression.MatchExpression(resolvedScrutinee, resolvedCases)
      case BlockExpression(lines)                                    =>
        withLocalScope(resolveBlockLines(lines, runtime)).map(Expression.BlockExpression.apply)
    }

  /** Resolves a block's lines in order, threading the binder scope: each binder is added to scope *before* its own line
    * (and every later line) is resolved, so a self-reference resolves to that local — caught as a hard error at
    * lowering — and later lines can reference it. The whole block is wrapped in a [[withLocalScope]] by the caller so
    * the binders do not leak past the block.
    */
  private def resolveBlockLines(lines: Seq[CoreExpression.BlockLine], runtime: Boolean): ScopedIO[Seq[Expression.BlockLine]] =
    lines match {
      case Nil          => Seq.empty[Expression.BlockLine].pure[ScopedIO]
      case line :: rest =>
        for {
          resolvedType <- (line.binderName, line.binderType) match {
                            case (Some(n), Some(t)) => resolveTypeStack(n.as(t), false).map(Some(_))
                            case _                  => None.pure[ScopedIO]
                          }
          _            <- line.binderName.traverse(n => addParameter(n.value))
          resolvedExpr <- resolveExpression(line.expression.value, runtime).map(line.expression.as)
          resolvedRest <- resolveBlockLines(rest, runtime)
        } yield Expression.BlockLine(line.binderName, resolvedType, resolvedExpr) +: resolvedRest
    }

  private def resolvePattern(pattern: Sourced[CorePattern]): ScopedIO[Sourced[Pattern]] =
    pattern.value match {
      case CorePattern.ConstructorPattern(None, nameSrc, subPatterns)          =>
        getValue(nameSrc.value).flatMap {
          case Some(vfqn) =>
            subPatterns
              .traverse(resolvePattern)
              .map(resolved => pattern.as(Pattern.ConstructorPattern(nameSrc.as(vfqn), resolved)))
          case None       =>
            compilerAbort(nameSrc.as("Constructor not defined.")).liftToScoped
        }
      case CorePattern.ConstructorPattern(Some(qualSrc), nameSrc, subPatterns) =>
        val moduleName = ModuleName.parse(qualSrc.value)
        val vfqn       = ValueFQN(moduleName, nameSrc.value)
        val outline    = Sourced.outline(Seq(qualSrc, nameSrc))

        getPlatform.flatMap(platform => getFactIfProduced(UnifiedModuleValue.Key(vfqn, platform)).liftToScoped).flatMap {
          case Some(_) =>
            subPatterns
              .traverse(resolvePattern)
              .map(resolved => pattern.as(Pattern.ConstructorPattern(outline.as(vfqn), resolved)))
          case None    =>
            compilerAbort(nameSrc.as("Qualified constructor not available.")).liftToScoped
        }
      case CorePattern.VariablePattern(name)                                   =>
        addParameter(name.value).as(pattern.as(Pattern.VariablePattern(name)))
      case CorePattern.WildcardPattern(source)                                 =>
        (pattern.as(Pattern.WildcardPattern(source)): Sourced[Pattern]).pure[ScopedIO]
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
      .flatMap(validateApplyPrecedence(decl.relation, _))

  private def validateApplyPrecedence(
      relation: AstPrecedenceDeclaration.Relation,
      resolvedTargets: Seq[Sourced[ValueFQN]]
  ): ScopedIO[PrecedenceDeclaration] =
    resolvedTargets.find(_.value === ValueFQN.applyFQN) match {
      case Some(applyTarget) if relation == AstPrecedenceDeclaration.Relation.Above =>
        compilerAbort(applyTarget.as("Infix operator cannot have higher precedence than application.")).liftToScoped
      case Some(applyTarget) if relation == AstPrecedenceDeclaration.Relation.At    =>
        compilerAbort(applyTarget.as("Infix operator cannot have the same precedence as application.")).liftToScoped
      case _                                                                        =>
        PrecedenceDeclaration(relation, resolvedTargets).pure[ScopedIO]
    }
}

object ValueResolver {

  /** The desugaring-machinery abilities resolved by fixed FQN rather than via import scope: `PatternMatch`/`TypeMatch`,
    * which only compiler-generated `implement` markers (and `match` desugaring) ever name. They live in the
    * `eliot.compiler.internal` package, kept out of the user-facing prelude and *not* auto-imported. */
  private val compilerInternalAbilities: Map[String, AbilityFQN] =
    Seq(patternMatchAbilityName, typeMatchAbilityName)
      .map(name => name -> AbilityFQN(ModuleName(ModuleName.compilerInternalPackage, name), name))
      .toMap
}
