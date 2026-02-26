package com.vanillasource.eliot.eliotc.matchdesugar.processor

import cats.kernel.Order.catsKernelOrderingForOrder
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier, TypeStack}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.matchdesugar.fact.MatchDesugaredValue
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
      desugaredRuntime,
      desugarTypeStack(resolvedValue.typeStack),
      resolvedValue.paramConstraints,
      resolvedValue.fixity,
      resolvedValue.precedence
    )

  private def desugarTypeStack(
      stack: Sourced[TypeStack[Expression]]
  ): Sourced[TypeStack[Expression]] =
    stack.map(ts => TypeStack(ts.levels.map(desugarExpressionPure)))

  private def desugarExpressionPure(expr: Expression): Expression =
    expr match {
      case Expression.MatchExpression(_, _)                          =>
        throw IllegalStateException("MatchExpression in type position is not supported")
      case Expression.FunctionApplication(target, arg)               =>
        Expression.FunctionApplication(desugarTypeStackPure(target), desugarTypeStackPure(arg))
      case Expression.FunctionLiteral(paramName, paramType, body)    =>
        Expression.FunctionLiteral(paramName, paramType.map(desugarTypeStackPure), desugarTypeStackPure(body))
      case Expression.FlatExpression(parts)                          =>
        Expression.FlatExpression(parts.map(desugarTypeStackPure))
      case other                                                     => other
    }

  private def desugarTypeStackPure(
      stack: Sourced[TypeStack[Expression]]
  ): Sourced[TypeStack[Expression]] =
    stack.map(ts => TypeStack(ts.levels.map(desugarExpressionPure)))

  private def desugarExpression(expr: Expression): CompilerIO[Expression] =
    expr match {
      case Expression.MatchExpression(scrutinee, cases) =>
        desugarMatch(scrutinee, cases)
      case Expression.FunctionApplication(target, arg)  =>
        for {
          desugaredTarget <- desugarInTypeStack(target)
          desugaredArg    <- desugarInTypeStack(arg)
        } yield Expression.FunctionApplication(desugaredTarget, desugaredArg)
      case Expression.FunctionLiteral(paramName, paramType, body) =>
        desugarInTypeStack(body).map(Expression.FunctionLiteral(paramName, paramType, _))
      case Expression.FlatExpression(parts)             =>
        parts.traverse(desugarInTypeStack).map(Expression.FlatExpression(_))
      case other                                        =>
        other.pure[CompilerIO]
    }

  private def desugarInTypeStack(
      stack: Sourced[TypeStack[Expression]]
  ): CompilerIO[Sourced[TypeStack[Expression]]] =
    stack.value.levels.traverse(desugarExpression).map(levels => stack.as(TypeStack(levels)))

  private def desugarMatch(
      scrutinee: Sourced[TypeStack[Expression]],
      cases: Seq[Expression.MatchCase]
  ): CompilerIO[Expression] =
    for {
      constructorModule <- findConstructorModule(cases)
      allConstructors   <- findAllConstructors(constructorModule)
      _                 <- checkExhaustiveness(cases, allConstructors)
      orderedHandlers   <- buildOrderedHandlers(scrutinee, cases, allConstructors)
      handleWithVfqn     = ValueFQN(constructorModule, QualifiedName("handleWith", Qualifier.Default))
    } yield buildHandleWithCall(scrutinee, handleWithVfqn, orderedHandlers)

  /** Find the module that defines the data type from the first constructor pattern in the cases. */
  private def findConstructorModule(
      cases: Seq[Expression.MatchCase]
  ): CompilerIO[ModuleName] =
    cases
      .flatMap(c => collectConstructorPatterns(c.pattern.value))
      .headOption match {
      case Some(vfqn) => vfqn.moduleName.pure[CompilerIO]
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

  /** Find all constructors of the data type in definition order. */
  private def findAllConstructors(
      moduleName: ModuleName
  ): CompilerIO[Seq[ValueFQN]] =
    for {
      moduleNames <- getFactOrAbort(UnifiedModuleNames.Key(moduleName))
      constructorNames = moduleNames.names.keys
                           .filter(qn => qn.qualifier == Qualifier.Default && qn.name.head.isUpper)
                           .toSeq
      constructorVfqns = constructorNames.map(qn => ValueFQN(moduleName, qn))
      ordered <- constructorVfqns.traverse { vfqn =>
                   getFactOrAbort(UnifiedModuleValue.Key(vfqn)).map(umv =>
                     (vfqn, umv.namedValue.qualifiedName.range.from)
                   )
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
    val hasWildcard          = cases.exists { c =>
      c.pattern.value match {
        case Pattern.WildcardPattern(_) | Pattern.VariablePattern(_) => true
        case _                                                       => false
      }
    }

    if (!hasWildcard) {
      val missing = allConstructors.filterNot(explicitConstructors.contains)
      if (missing.nonEmpty) {
        val firstCase   = cases.head.pattern
        val missingText = missing.map(_.name.name).mkString(", ")
        Sourced
          .compilerError(firstCase.as(s"Non-exhaustive match. Missing constructors: $missingText."))
          .flatMap(_ => abort)
      } else {
        ().pure[CompilerIO]
      }
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
      cases
        .filter(c =>
          c.pattern.value match {
            case Pattern.ConstructorPattern(_, _) => true
            case _                                => false
          }
        )
        .groupBy(c =>
          c.pattern.value.asInstanceOf[Pattern.ConstructorPattern].constructor.value
        )

    val wildcardCases: Seq[Expression.MatchCase] = cases.filter { c =>
      c.pattern.value match {
        case Pattern.WildcardPattern(_) | Pattern.VariablePattern(_) => true
        case _                                                       => false
      }
    }

    allConstructors.traverse { ctorVfqn =>
      casesByConstructor.get(ctorVfqn) match {
        case Some(ctorCases) =>
          buildConstructorHandler(scrutinee, ctorVfqn, ctorCases)
        case None            =>
          wildcardCases.headOption match {
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
    val ctorFields = cases.head.pattern.value.asInstanceOf[Pattern.ConstructorPattern].subPatterns

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
  ): CompilerIO[Sourced[TypeStack[Expression]]] = {
    val firstCase = cases.head
    for {
      desugaredBody <- desugarInTypeStack(firstCase.body)
    } yield {
      val handler = Expression.FunctionLiteral(
        scrutinee.as("_"),
        None,
        desugaredBody
      )
      scrutinee.as(TypeStack.of(handler))
    }
  }

  /** Build handler for single case with field patterns. */
  private def buildSingleCaseHandler(
      scrutinee: Sourced[TypeStack[Expression]],
      matchCase: Expression.MatchCase,
      fieldPatterns: Seq[Sourced[Pattern]]
  ): CompilerIO[Sourced[TypeStack[Expression]]] =
    for {
      desugaredBody <- desugarInTypeStack(matchCase.body)
      handler       <- buildFieldLambdas(scrutinee, fieldPatterns, desugaredBody)
    } yield handler

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
    fieldPat.value match {
      case Pattern.VariablePattern(name) =>
        val lambda = Expression.FunctionLiteral(name, None, innerBody)
        scrutinee.as(TypeStack.of(lambda)).pure[CompilerIO]

      case Pattern.WildcardPattern(source) =>
        val lambda = Expression.FunctionLiteral(source, None, innerBody)
        scrutinee.as(TypeStack.of(lambda)).pure[CompilerIO]

      case Pattern.ConstructorPattern(_, _) =>
        // Generate fresh name for the field, then nested match
        val freshName = fieldPat.as("$match_field")
        val fieldRef  = scrutinee.as(
          TypeStack.of(Expression.ParameterReference(freshName): Expression)
        )
        for {
          nestedMatch <- desugarMatch(
                           fieldRef,
                           Seq(
                             Expression.MatchCase(
                               fieldPat,
                               innerBody
                             )
                           )
                         )
        } yield {
          val lambda = Expression.FunctionLiteral(
            freshName,
            None,
            scrutinee.as(TypeStack.of(nestedMatch))
          )
          scrutinee.as(TypeStack.of(lambda))
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
      c.pattern.value.asInstanceOf[Pattern.ConstructorPattern].subPatterns
    }

    for {
      innerBody <- buildMultiCaseBody(scrutinee, freshNames, fieldPatternRows, cases.map(_.body))
      handler    = freshNames.foldRight(innerBody) { (name, body) =>
                     scrutinee.as(TypeStack.of(Expression.FunctionLiteral(name, None, body)))
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
    // Find first column with a constructor pattern
    val constructorColumnIdx = patternRows.head.indices.find { col =>
      patternRows.exists(row =>
        row(col).value match {
          case Pattern.ConstructorPattern(_, _) => true
          case _                                => false
        }
      )
    }

    constructorColumnIdx match {
      case Some(colIdx) =>
        // Generate a match on this column's field
        val fieldRef = scrutinee.as(
          TypeStack.of(Expression.ParameterReference(freshNames(colIdx)): Expression)
        )

        // Build cases for the nested match, binding remaining columns
        val nestedCases = patternRows.zip(bodies).map { case (row, body) =>
          val fieldPattern = row(colIdx)
          // Wrap the body with let-bindings for the variable columns
          val wrappedBody = wrapWithBindings(scrutinee, freshNames, row, colIdx, body)
          Expression.MatchCase(fieldPattern, wrappedBody)
        }

        for {
          desugaredMatch <- desugarMatch(fieldRef, nestedCases)
        } yield scrutinee.as(TypeStack.of(desugaredMatch))

      case None =>
        // All patterns are variables/wildcards - use the first matching case
        // Wrap with variable bindings
        val firstBody = wrapWithBindings(scrutinee, freshNames, patternRows.head, -1, bodies.head)
        for {
          desugared <- desugarInTypeStack(firstBody)
        } yield desugared
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
            // Bind the variable: ((varName -> body)(freshName))
            val lambda = Expression.FunctionLiteral(varName, None, innerBody)
            val app    = Expression.FunctionApplication(
              scrutinee.as(TypeStack.of(lambda)),
              scrutinee.as(TypeStack.of(Expression.ParameterReference(freshNames(idx))))
            )
            scrutinee.as(TypeStack.of(app))
          case _                                                                          => innerBody
        }
    }

  /** Build a wildcard handler for a constructor not explicitly matched. */
  private def buildWildcardHandler(
      scrutinee: Sourced[TypeStack[Expression]],
      ctorVfqn: ValueFQN,
      wildcardCase: Expression.MatchCase
  ): CompilerIO[Sourced[TypeStack[Expression]]] = {
    // Look up the constructor to determine its arity
    val ctorModule = ctorVfqn.moduleName
    for {
      umv           <- getFactOrAbort(UnifiedModuleValue.Key(ctorVfqn))
      arity          = countConstructorFields(umv)
      desugaredBody <- desugarInTypeStack(wildcardCase.body)
    } yield {
      if (arity == 0) {
        // Nullary: handler takes Unit
        wildcardCase.pattern.value match {
          case Pattern.VariablePattern(varName) =>
            // Can't meaningfully bind the whole value to a name for nullary constructors
            val handler = Expression.FunctionLiteral(scrutinee.as("_"), None, desugaredBody)
            scrutinee.as(TypeStack.of(handler))
          case _                                =>
            val handler = Expression.FunctionLiteral(scrutinee.as("_"), None, desugaredBody)
            scrutinee.as(TypeStack.of(handler))
        }
      } else {
        // N-ary: handler takes N curried parameters, all ignored
        val handler = (0 until arity).foldRight(desugaredBody) { (_, body) =>
          scrutinee.as(TypeStack.of(Expression.FunctionLiteral(scrutinee.as("_"), None, body)))
        }
        handler
      }
    }
  }

  /** Count the number of fields a constructor has by analyzing its type signature.
    * Constructor type is of the form: A -> B -> ... -> DataType, where each A -> is a field.
    * For data with generic params, it's: GenParam -> ... -> Field1 -> ... -> DataType.
    * We count the number of FunctionLiteral wrappers in the body (which are the curried params).
    */
  private def countConstructorFields(umv: UnifiedModuleValue): Int = {
    val namedValue = umv.namedValue
    // Count leading FunctionLiteral wrappers in the runtime body
    // For constructors, the body preserves field structure
    namedValue.runtime match {
      case Some(body) => countLeadingLambdas(body.value)
      case None       => 0
    }
  }

  private def countLeadingLambdas(expr: com.vanillasource.eliot.eliotc.core.fact.Expression): Int =
    expr match {
      case com.vanillasource.eliot.eliotc.core.fact.Expression.FunctionLiteral(_, _, body) =>
        1 + countLeadingLambdas(body.value.signature)
      case _ => 0
    }

  /** Build the curried handleWith call: Module::handleWith(scrutinee)(handler1)(handler2)... */
  private def buildHandleWithCall(
      scrutinee: Sourced[TypeStack[Expression]],
      handleWithVfqn: ValueFQN,
      handlers: Seq[Sourced[TypeStack[Expression]]]
  ): Expression = {
    val handleWithRef = Expression.ValueReference(scrutinee.as(handleWithVfqn))
    val withScrutinee = Expression.FunctionApplication(
      scrutinee.as(TypeStack.of(handleWithRef)),
      scrutinee
    )
    handlers.foldLeft[Expression](withScrutinee) { (acc, handler) =>
      Expression.FunctionApplication(
        scrutinee.as(TypeStack.of(acc)),
        handler
      )
    }
  }
}
