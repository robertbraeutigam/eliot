package com.vanillasource.eliot.eliotc.matchdesugar.processor

import cats.kernel.Order.catsKernelOrderingForOrder
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{Expression => CoreExpression, QualifiedName, Qualifier, TypeStack}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.matchdesugar.fact.{MatchDesugaredExpression, MatchDesugaredValue}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, UnifiedModuleNames, UnifiedModuleValue, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, Pattern, ResolvedValue}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort

class MatchDesugaringProcessor
    extends TransformationProcessor[ResolvedValue.Key, MatchDesugaredValue.Key](key =>
      ResolvedValue.Key(key.vfqn)
    )
    with Logging {

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
    if (isTypeMatch(cases)) desugarTypeMatch(scrutinee, cases)
    else
      for {
        moduleAndType   <- findConstructorModuleAndTypeName(cases)
        (constructorModule, dataTypeName) = moduleAndType
        allConstructors <- findAllConstructors(constructorModule, dataTypeName)
        _               <- checkExhaustiveness(cases, allConstructors)
        orderedHandlers <- buildOrderedHandlers(scrutinee, cases, allConstructors)
        eliminatorName   = s"handle${dataTypeName}With"
        handleWithVfqn   = ValueFQN(constructorModule, QualifiedName(eliminatorName, Qualifier.Default))
      } yield buildHandleWithCall(scrutinee, handleWithVfqn, orderedHandlers)

  private def isTypeMatch(cases: Seq[Expression.MatchCase]): Boolean =
    cases
      .flatMap(c => collectConstructorPatterns(c.pattern.value))
      .headOption
      .exists(_.name.qualifier == Qualifier.Type)

  private def desugarTypeMatch(
      scrutinee: Sourced[TypeStack[Expression]],
      cases: Seq[Expression.MatchCase]
  ): CompilerIO[Expression] = {
    val constructorCases = cases.filter(_.pattern.value.isInstanceOf[Pattern.ConstructorPattern])
    val wildcardCase     = cases.find(c => bindingName(c.pattern.value).isDefined)

    wildcardCase match {
      case None     =>
        val Pattern.ConstructorPattern(ctorName, _) = constructorCases.head.pattern.value: @unchecked
        compilerAbort(ctorName.as("Type match must have a wildcard case."))
      case Some(wc) =>
        for {
          wildcardBody <- desugarInTypeStack(wc.body)
          handlers     <- constructorCases.traverse { ctorCase =>
                            val Pattern.ConstructorPattern(ctor, _) = ctorCase.pattern.value: @unchecked
                            buildTypeMatchHandler(scrutinee, ctorCase).map(h => (ctor.value, h))
                          }
        } yield chainTypeMatches(scrutinee, handlers, wildcardBody)
    }
  }

  /** Chain type match expressions left to right with the wildcard body as the innermost fallback. */
  private def chainTypeMatches(
      scrutinee: Sourced[TypeStack[Expression]],
      cases: Seq[(ValueFQN, Sourced[TypeStack[Expression]])],
      wildcardBody: Sourced[TypeStack[Expression]]
  ): Expression = {
    val (vfqn, handler) = cases.head
    val elseLambdaBody  = if (cases.tail.isEmpty) wildcardBody
                          else wrapExpr(scrutinee, chainTypeMatches(scrutinee, cases.tail, wildcardBody))
    val elseCase        = wrapExpr(scrutinee, Expression.FunctionLiteral(scrutinee.as("_"), None, elseLambdaBody))
    buildTypeMatchExpression(scrutinee, vfqn, handler, elseCase)
  }

  private def buildTypeMatchHandler(
      scrutinee: Sourced[TypeStack[Expression]],
      ctorCase: Expression.MatchCase
  ): CompilerIO[Sourced[TypeStack[Expression]]] = {
    val Pattern.ConstructorPattern(_, subPatterns) = ctorCase.pattern.value: @unchecked
    for {
      desugaredBody <- desugarInTypeStack(ctorCase.body)
      handler       <- if (subPatterns.isEmpty)
                          wrapExpr(scrutinee, Expression.FunctionLiteral(scrutinee.as("_"), None, desugaredBody)).pure[CompilerIO]
                        else
                          buildFieldLambdas(scrutinee, subPatterns, desugaredBody)
    } yield handler
  }

  private def buildTypeMatchExpression(
      scrutinee: Sourced[TypeStack[Expression]],
      ctorVfqn: ValueFQN,
      handler: Sourced[TypeStack[Expression]],
      elseCase: Sourced[TypeStack[Expression]]
  ): Expression = {
    val typeMatchName = s"typeMatch${ctorVfqn.name.name}"
    val typeMatchFqn  = ValueFQN(ctorVfqn.moduleName, QualifiedName(typeMatchName, Qualifier.Default))
    buildCurriedCall(scrutinee, Expression.ValueReference(scrutinee.as(typeMatchFqn)), Seq(scrutinee, handler, elseCase))
  }

  /** Find the module and data type name from the first constructor pattern in the cases. */
  private def findConstructorModuleAndTypeName(
      cases: Seq[Expression.MatchCase]
  ): CompilerIO[(ModuleName, String)] =
    cases
      .flatMap(c => collectConstructorPatterns(c.pattern.value))
      .headOption match {
      case Some(vfqn) =>
        for {
          umv          <- getFactOrAbort(UnifiedModuleValue.Key(vfqn))
          dataTypeName <- extractDataTypeName(umv.namedValue.typeStack.signature) match {
                            case Some(name) => name.pure[CompilerIO]
                            case None       => compilerAbort(umv.namedValue.qualifiedName.as("Could not determine data type for constructor."))
                          }
        } yield (vfqn.moduleName, dataTypeName)
      case None       =>
        cases.headOption match {
          case Some(c) => compilerAbort(c.pattern.as("Match expression must have at least one constructor pattern."))
          case None    => abort
        }
    }

  private def collectConstructorPatterns(pattern: Pattern): Seq[ValueFQN] =
    pattern match {
      case Pattern.ConstructorPattern(ctor, subs) =>
        ctor.value +: subs.flatMap(s => collectConstructorPatterns(s.value))
      case _                                      => Seq.empty
    }

  /** Find all constructors of the given data type in definition order. */
  private def findAllConstructors(
      moduleName: ModuleName,
      dataTypeName: String
  ): CompilerIO[Seq[ValueFQN]] =
    for {
      moduleNames <- getFactOrAbort(UnifiedModuleNames.Key(moduleName))
      constructorNames = moduleNames.names.keys
                           .filter(qn => qn.qualifier == Qualifier.Default && qn.name.head.isUpper)
                           .toSeq
      constructorVfqns = constructorNames.map(qn => ValueFQN(moduleName, qn))
      ordered <- constructorVfqns.traverseFilter { vfqn =>
                   getFactOrAbort(UnifiedModuleValue.Key(vfqn)).map { umv =>
                     val typeName = extractDataTypeName(umv.namedValue.typeStack.signature)
                     Option.when(typeName.contains(dataTypeName))(
                       (vfqn, umv.namedValue.qualifiedName.range.from)
                     )
                   }
                 }
    } yield ordered.sortBy(_._2).map(_._1)

  /** Check that all constructors are covered by explicit patterns or wildcards. */
  private def checkExhaustiveness(
      cases: Seq[Expression.MatchCase],
      allConstructors: Seq[ValueFQN]
  ): CompilerIO[Unit] = {
    val explicitConstructors = cases.flatMap { c =>
      c.pattern.value match {
        case Pattern.ConstructorPattern(ctor, _) => Some(ctor.value)
        case _                                   => None
      }
    }.toSet
    val hasWildcard          = cases.exists(c => bindingName(c.pattern.value).isDefined)
    val missing              = allConstructors.filterNot(explicitConstructors.contains)

    if (!hasWildcard && missing.nonEmpty) {
      val missingText = missing.map(_.name.name).mkString(", ")
      Sourced
        .compilerError(cases.head.pattern.as(s"Non-exhaustive match. Missing constructors: $missingText."))
        .flatMap(_ => abort)
    } else {
      ().pure[CompilerIO]
    }
  }

  /** Build handlers in constructor definition order. */
  private def buildOrderedHandlers(
      scrutinee: Sourced[TypeStack[Expression]],
      cases: Seq[Expression.MatchCase],
      allConstructors: Seq[ValueFQN]
  ): CompilerIO[Seq[Sourced[TypeStack[Expression]]]] = {
    val casesByConstructor: Map[ValueFQN, Seq[Expression.MatchCase]] =
      cases.flatMap { c =>
        c.pattern.value match {
          case Pattern.ConstructorPattern(ctor, _) => Some(ctor.value -> c)
          case _                                   => None
        }
      }.groupMap(_._1)(_._2)

    val wildcardCase = cases.find(c => bindingName(c.pattern.value).isDefined)

    allConstructors.traverse { ctorVfqn =>
      casesByConstructor.get(ctorVfqn) match {
        case Some(ctorCases) =>
          buildConstructorHandler(scrutinee, ctorVfqn, ctorCases)
        case None            =>
          wildcardCase match {
            case Some(wc) => buildWildcardHandler(scrutinee, ctorVfqn, wc)
            case None     => compilerAbort(scrutinee.as(s"No handler for constructor ${ctorVfqn.name.name}."))
          }
      }
    }
  }

  /** Build a handler for a specific constructor from one or more cases. */
  private def buildConstructorHandler(
      scrutinee: Sourced[TypeStack[Expression]],
      ctorVfqn: ValueFQN,
      cases: Seq[Expression.MatchCase]
  ): CompilerIO[Sourced[TypeStack[Expression]]] = {
    val Pattern.ConstructorPattern(_, ctorFields) = cases.head.pattern.value: @unchecked

    if (ctorFields.isEmpty) {
      // Nullary constructor: handler takes Unit parameter
      buildNullaryHandler(scrutinee, cases)
    } else if (cases.size == 1) {
      // Single case: direct handler
      buildSingleCaseHandler(scrutinee, cases.head, ctorFields)
    } else {
      // Multiple cases for same constructor: compile nested match
      buildMultiCaseHandler(scrutinee, ctorVfqn, cases, ctorFields.size)
    }
  }

  /** Build handler for nullary constructor (takes Unit, ignored). */
  private def buildNullaryHandler(
      scrutinee: Sourced[TypeStack[Expression]],
      cases: Seq[Expression.MatchCase]
  ): CompilerIO[Sourced[TypeStack[Expression]]] =
    desugarInTypeStack(cases.head.body).map { desugaredBody =>
      wrapExpr(scrutinee, Expression.FunctionLiteral(scrutinee.as("_"), None, desugaredBody))
    }

  /** Build handler for single case with field patterns. */
  private def buildSingleCaseHandler(
      scrutinee: Sourced[TypeStack[Expression]],
      matchCase: Expression.MatchCase,
      fieldPatterns: Seq[Sourced[Pattern]]
  ): CompilerIO[Sourced[TypeStack[Expression]]] =
    desugarInTypeStack(matchCase.body).flatMap(buildFieldLambdas(scrutinee, fieldPatterns, _))

  /** Build curried lambdas for field patterns, handling embedded constructor patterns recursively. */
  private def buildFieldLambdas(
      scrutinee: Sourced[TypeStack[Expression]],
      fieldPatterns: Seq[Sourced[Pattern]],
      body: Sourced[TypeStack[Expression]]
  ): CompilerIO[Sourced[TypeStack[Expression]]] =
    fieldPatterns match {
      case Seq() => body.pure[CompilerIO]
      case init :+ last =>
        buildFieldLambda(scrutinee, last, body).flatMap(innerBody =>
          buildFieldLambdas(scrutinee, init, innerBody)
        )
    }

  private def buildFieldLambda(
      scrutinee: Sourced[TypeStack[Expression]],
      fieldPat: Sourced[Pattern],
      innerBody: Sourced[TypeStack[Expression]]
  ): CompilerIO[Sourced[TypeStack[Expression]]] =
    bindingName(fieldPat.value) match {
      case Some(name) =>
        wrapExpr(scrutinee, Expression.FunctionLiteral(name, None, innerBody)).pure[CompilerIO]
      case None       =>
        val freshName = fieldPat.as("$match_field")
        val fieldRef  = wrapExpr(scrutinee, Expression.ParameterReference(freshName))
        desugarMatch(fieldRef, Seq(Expression.MatchCase(fieldPat, innerBody))).map { nestedMatch =>
          wrapExpr(scrutinee, Expression.FunctionLiteral(freshName, None, wrapExpr(scrutinee, nestedMatch)))
        }
    }

  /** Build handler for multiple cases with the same constructor.
    * Groups cases by their first constructor-pattern field and creates nested matches.
    */
  private def buildMultiCaseHandler(
      scrutinee: Sourced[TypeStack[Expression]],
      ctorVfqn: ValueFQN,
      cases: Seq[Expression.MatchCase],
      fieldCount: Int
  ): CompilerIO[Sourced[TypeStack[Expression]]] = {
    // Generate fresh parameter names for each field
    val freshNames = (0 until fieldCount).map(i => scrutinee.as(s"$$match_${ctorVfqn.name.name}_$i"))

    // Build the inner body by finding the first field with constructor patterns and matching on it
    val fieldPatternRows: Seq[Seq[Sourced[Pattern]]] = cases.map { c =>
      val Pattern.ConstructorPattern(_, subs) = c.pattern.value: @unchecked
      subs
    }

    for {
      innerBody <- buildMultiCaseBody(scrutinee, freshNames, fieldPatternRows, cases.map(_.body))
      handler    = freshNames.foldRight(innerBody) { (name, body) =>
                     wrapExpr(scrutinee, Expression.FunctionLiteral(name, None, body))
                   }
    } yield handler
  }

  /** Build the body for multi-case handlers by finding columns with constructor patterns
    * and generating nested matches.
    */
  private def buildMultiCaseBody(
      scrutinee: Sourced[TypeStack[Expression]],
      freshNames: Seq[Sourced[String]],
      patternRows: Seq[Seq[Sourced[Pattern]]],
      bodies: Seq[Sourced[TypeStack[Expression]]]
  ): CompilerIO[Sourced[TypeStack[Expression]]] = {
    val constructorColumnIdx = patternRows.head.indices.find { col =>
      patternRows.exists(_(col).value.isInstanceOf[Pattern.ConstructorPattern])
    }

    constructorColumnIdx match {
      case Some(colIdx) =>
        val fieldRef    = wrapExpr(scrutinee, Expression.ParameterReference(freshNames(colIdx)))
        val nestedCases = patternRows.zip(bodies).map { case (row, body) =>
          Expression.MatchCase(row(colIdx), wrapWithBindings(scrutinee, freshNames, row, colIdx, body))
        }
        desugarMatch(fieldRef, nestedCases).map(wrapExpr(scrutinee, _))

      case None =>
        desugarInTypeStack(wrapWithBindings(scrutinee, freshNames, patternRows.head, -1, bodies.head))
    }
  }

  /** Wrap a body expression with let-bindings (immediately-invoked lambdas) for variable patterns. */
  private def wrapWithBindings(
      scrutinee: Sourced[TypeStack[Expression]],
      freshNames: Seq[Sourced[String]],
      patterns: Seq[Sourced[Pattern]],
      skipColumn: Int,
      body: Sourced[TypeStack[Expression]]
  ): Sourced[TypeStack[Expression]] =
    patterns.zipWithIndex.foldRight(body) { case ((pat, idx), innerBody) =>
      if (idx == skipColumn) innerBody
      else
        pat.value match {
          case Pattern.VariablePattern(varName) if varName.value != freshNames(idx).value =>
            val lambda = Expression.FunctionLiteral(varName, None, innerBody)
            val app    = Expression.FunctionApplication(
              wrapExpr(scrutinee, lambda),
              wrapExpr(scrutinee, Expression.ParameterReference(freshNames(idx)))
            )
            wrapExpr(scrutinee, app)
          case _                                                                          => innerBody
        }
    }

  /** Build a wildcard handler for a constructor not explicitly matched. */
  private def buildWildcardHandler(
      scrutinee: Sourced[TypeStack[Expression]],
      ctorVfqn: ValueFQN,
      wildcardCase: Expression.MatchCase
  ): CompilerIO[Sourced[TypeStack[Expression]]] =
    for {
      umv           <- getFactOrAbort(UnifiedModuleValue.Key(ctorVfqn))
      arity          = countConstructorFields(umv)
      desugaredBody <- desugarInTypeStack(wildcardCase.body)
    } yield {
      val lambdaCount = math.max(1, arity)
      (0 until lambdaCount).foldRight(desugaredBody) { (_, body) =>
        wrapExpr(scrutinee, Expression.FunctionLiteral(scrutinee.as("_"), None, body))
      }
    }

  /** Count the number of fields a constructor has from its type signature.
    * The type has the form: GenParam -> ... -> Function^Type(Field1, Function^Type(Field2, ... DataType)).
    * We skip leading FunctionLiterals (generic params) then count Function^Type applications.
    */
  private def countConstructorFields(umv: UnifiedModuleValue): Int =
    countFieldsInType(umv.namedValue.typeStack.signature)

  private def countFieldsInType(expr: CoreExpression): Int =
    expr match {
      case CoreExpression.FunctionLiteral(_, _, body) => countFieldsInType(body.value.signature)
      case _                                          => asFunctionTypeReturnType(expr).fold(0)(ret => 1 + countFieldsInType(ret))
    }

  private def extractDataTypeName(expr: CoreExpression): Option[String] =
    expr match {
      case CoreExpression.FunctionLiteral(_, _, body) => extractDataTypeName(body.value.signature)
      case _ =>
        asFunctionTypeReturnType(expr) match {
          case Some(returnType) => extractDataTypeName(returnType)
          case None             => findTypeConstructorName(expr)
        }
    }

  private def findTypeConstructorName(expr: CoreExpression): Option[String] =
    expr match {
      case CoreExpression.NamedValueReference(qn, _, _) if qn.value.qualifier == Qualifier.Type && qn.value.name != "Function" =>
        Some(qn.value.name)
      case CoreExpression.FunctionApplication(target, _) =>
        findTypeConstructorName(target.value.signature)
      case _ => None
    }

  /** If expr is a Function type application Function(paramType)(returnType), returns the return type. */
  private def asFunctionTypeReturnType(expr: CoreExpression): Option[CoreExpression] =
    expr match {
      case CoreExpression.FunctionApplication(target, arg) =>
        target.value.signature match {
          case CoreExpression.FunctionApplication(innerTarget, _) =>
            innerTarget.value.signature match {
              case CoreExpression.NamedValueReference(qn, _, _) if qn.value == QualifiedName("Function", Qualifier.Type) =>
                Some(arg.value.signature)
              case _ => None
            }
          case _ => None
        }
      case _ => None
    }

  private def buildHandleWithCall(
      scrutinee: Sourced[TypeStack[Expression]],
      handleWithVfqn: ValueFQN,
      handlers: Seq[Sourced[TypeStack[Expression]]]
  ): Expression =
    buildCurriedCall(scrutinee, Expression.ValueReference(scrutinee.as(handleWithVfqn)), scrutinee +: handlers)

  private def buildCurriedCall(
      scrutinee: Sourced[TypeStack[Expression]],
      ref: Expression,
      args: Seq[Sourced[TypeStack[Expression]]]
  ): Expression =
    args.foldLeft(ref) { (acc, arg) =>
      Expression.FunctionApplication(wrapExpr(scrutinee, acc), arg)
    }

  private def wrapExpr(src: Sourced[?], expr: Expression): Sourced[TypeStack[Expression]] =
    src.as(TypeStack.of(expr))

  private def bindingName(pattern: Pattern): Option[Sourced[String]] =
    pattern match {
      case Pattern.VariablePattern(name)   => Some(name)
      case Pattern.WildcardPattern(source) => Some(source)
      case _                               => None
    }
}
