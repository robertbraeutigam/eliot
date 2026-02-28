package com.vanillasource.eliot.eliotc.matchdesugar.processor

import cats.kernel.Order.catsKernelOrderingForOrder
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier, TypeStack}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, UnifiedModuleNames, UnifiedModuleValue, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, Pattern}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort
import MatchDesugarUtils.*

class DataMatchDesugarer(
    desugarMatch: (Sourced[TypeStack[Expression]], Seq[Expression.MatchCase]) => CompilerIO[Expression],
    desugarInTypeStack: Sourced[TypeStack[Expression]] => CompilerIO[Sourced[TypeStack[Expression]]]
) {

  def desugar(
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

  def buildFieldLambdas(
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

  private def findConstructorModuleAndTypeName(
      cases: Seq[Expression.MatchCase]
  ): CompilerIO[(ModuleName, String)] =
    cases
      .flatMap(c => collectConstructorPatterns(c.pattern.value))
      .headOption match {
      case Some(vfqn) =>
        for {
          umv          <- getFactOrAbort(UnifiedModuleValue.Key(vfqn))
          dataTypeName <- ConstructorTypeAnalyzer.extractDataTypeName(umv.namedValue.typeStack.signature) match {
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
                     val typeName = ConstructorTypeAnalyzer.extractDataTypeName(umv.namedValue.typeStack.signature)
                     Option.when(typeName.contains(dataTypeName))(
                       (vfqn, umv.namedValue.qualifiedName.range.from)
                     )
                   }
                 }
    } yield ordered.sortBy(_._2).map(_._1)

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

  private def buildConstructorHandler(
      scrutinee: Sourced[TypeStack[Expression]],
      ctorVfqn: ValueFQN,
      cases: Seq[Expression.MatchCase]
  ): CompilerIO[Sourced[TypeStack[Expression]]] = {
    val Pattern.ConstructorPattern(_, ctorFields) = cases.head.pattern.value: @unchecked

    if (ctorFields.isEmpty) {
      buildNullaryHandler(scrutinee, cases)
    } else if (cases.size == 1) {
      buildSingleCaseHandler(scrutinee, cases.head, ctorFields)
    } else {
      buildMultiCaseHandler(scrutinee, ctorVfqn, cases, ctorFields.size)
    }
  }

  private def buildNullaryHandler(
      scrutinee: Sourced[TypeStack[Expression]],
      cases: Seq[Expression.MatchCase]
  ): CompilerIO[Sourced[TypeStack[Expression]]] =
    desugarInTypeStack(cases.head.body).map { desugaredBody =>
      wrapExpr(scrutinee, Expression.FunctionLiteral(scrutinee.as("_"), None, desugaredBody))
    }

  private def buildSingleCaseHandler(
      scrutinee: Sourced[TypeStack[Expression]],
      matchCase: Expression.MatchCase,
      fieldPatterns: Seq[Sourced[Pattern]]
  ): CompilerIO[Sourced[TypeStack[Expression]]] =
    desugarInTypeStack(matchCase.body).flatMap(buildFieldLambdas(scrutinee, fieldPatterns, _))

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

  private def buildMultiCaseHandler(
      scrutinee: Sourced[TypeStack[Expression]],
      ctorVfqn: ValueFQN,
      cases: Seq[Expression.MatchCase],
      fieldCount: Int
  ): CompilerIO[Sourced[TypeStack[Expression]]] = {
    val freshNames = (0 until fieldCount).map(i => scrutinee.as(s"$$match_${ctorVfqn.name.name}_$i"))

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

  private def buildWildcardHandler(
      scrutinee: Sourced[TypeStack[Expression]],
      ctorVfqn: ValueFQN,
      wildcardCase: Expression.MatchCase
  ): CompilerIO[Sourced[TypeStack[Expression]]] =
    for {
      umv           <- getFactOrAbort(UnifiedModuleValue.Key(ctorVfqn))
      arity          = ConstructorTypeAnalyzer.countConstructorFields(umv)
      desugaredBody <- desugarInTypeStack(wildcardCase.body)
    } yield {
      val lambdaCount = math.max(1, arity)
      (0 until lambdaCount).foldRight(desugaredBody) { (_, body) =>
        wrapExpr(scrutinee, Expression.FunctionLiteral(scrutinee.as("_"), None, body))
      }
    }

  private def buildHandleWithCall(
      scrutinee: Sourced[TypeStack[Expression]],
      handleWithVfqn: ValueFQN,
      handlers: Seq[Sourced[TypeStack[Expression]]]
  ): Expression =
    buildCurriedCall(scrutinee, Expression.ValueReference(scrutinee.as(handleWithVfqn)), scrutinee +: handlers)
}
