package com.vanillasource.eliot.eliotc.matchdesugar.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier, TypeStack}
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, Pattern}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort
import MatchDesugarUtils.*

class TypeMatchDesugarer(
    desugarInTypeStack: Sourced[TypeStack[Expression]] => CompilerIO[Sourced[TypeStack[Expression]]],
    buildFieldLambdas: (Sourced[TypeStack[Expression]], Seq[Sourced[Pattern]], Sourced[TypeStack[Expression]]) => CompilerIO[Sourced[TypeStack[Expression]]]
) {

  def desugar(
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
}

object TypeMatchDesugarer {

  def isTypeMatch(cases: Seq[Expression.MatchCase]): Boolean =
    cases
      .flatMap(c => collectConstructorPatterns(c.pattern.value))
      .headOption
      .exists(_.name.qualifier == Qualifier.Type)
}
