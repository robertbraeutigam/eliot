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
    for {
      moduleAndType   <- findConstructorModuleAndTypeName(cases)
      (constructorModule, dataTypeName) = moduleAndType
      allConstructors <- findAllConstructors(constructorModule, dataTypeName)
      _               <- checkExhaustiveness(cases, allConstructors)
      orderedHandlers <- buildOrderedHandlers(scrutinee, cases, allConstructors)
      eliminatorName   = s"handle${dataTypeName}With"
      handleWithVfqn   = ValueFQN(constructorModule, QualifiedName(eliminatorName, Qualifier.Default))
    } yield buildHandleWithCall(scrutinee, handleWithVfqn, orderedHandlers)

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
    val hasWildcard          = cases.exists { c =>
      c.pattern.value match {
        case _: Pattern.WildcardPattern | _: Pattern.VariablePattern => true
        case _                                                       => false
      }
    }
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

    val wildcardCase: Option[Expression.MatchCase] = cases.find { c =>
      c.pattern.value match {
        case _: Pattern.WildcardPattern | _: Pattern.VariablePattern => true
        case _                                                       => false
      }
    }

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
      val handler = Expression.FunctionLiteral(scrutinee.as("_"), None, desugaredBody)
      scrutinee.as(TypeStack.of(handler))
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
      val Pattern.ConstructorPattern(_, subs) = c.pattern.value: @unchecked
      subs
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
  ): CompilerIO[Sourced[TypeStack[Expression]]] =
    for {
      umv           <- getFactOrAbort(UnifiedModuleValue.Key(ctorVfqn))
      arity          = countConstructorFields(umv)
      desugaredBody <- desugarInTypeStack(wildcardCase.body)
    } yield {
      // Nullary constructors still need one lambda for the Unit parameter
      val lambdaCount = math.max(1, arity)
      (0 until lambdaCount).foldRight(desugaredBody) { (_, body) =>
        scrutinee.as(TypeStack.of(Expression.FunctionLiteral(scrutinee.as("_"), None, body)))
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
      case CoreExpression.FunctionLiteral(_, _, body)      =>
        countFieldsInType(body.value.signature)
      case CoreExpression.FunctionApplication(target, arg) =>
        target.value.signature match {
          case CoreExpression.FunctionApplication(innerTarget, _) =>
            innerTarget.value.signature match {
              case CoreExpression.NamedValueReference(qn, _, _) if qn.value == QualifiedName("Function", Qualifier.Type) =>
                1 + countFieldsInType(arg.value.signature)
              case _ => 0
            }
          case _ => 0
        }
      case _                                               => 0
    }

  /** Extract the data type name from a constructor's core type signature.
    * Walks through FunctionLiteral (generic param lambdas) and Function type applications
    * to find the return type's NamedValueReference with Type qualifier.
    */
  private def extractDataTypeName(expr: CoreExpression): Option[String] =
    expr match {
      case CoreExpression.FunctionLiteral(_, _, body)      =>
        extractDataTypeName(body.value.signature)
      case CoreExpression.FunctionApplication(target, arg) =>
        target.value.signature match {
          case CoreExpression.FunctionApplication(innerTarget, _) =>
            innerTarget.value.signature match {
              case CoreExpression.NamedValueReference(qn, _, _) if qn.value == QualifiedName("Function", Qualifier.Type) =>
                extractDataTypeName(arg.value.signature)
              case _ =>
                extractDataTypeName(target.value.signature)
            }
          case CoreExpression.NamedValueReference(qn, _, _) if qn.value.qualifier == Qualifier.Type && qn.value.name != "Function" =>
            Some(qn.value.name)
          case _ => None
        }
      case CoreExpression.NamedValueReference(qn, _, _) if qn.value.qualifier == Qualifier.Type && qn.value.name != "Function" =>
        Some(qn.value.name)
      case _                                               => None
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
