package com.vanillasource.eliot.eliotc.matchdesugar.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.TypeStack
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, Pattern}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import MatchDesugarUtils.*

class MatchDesugarContext(
    val desugarMatch: (Sourced[TypeStack[Expression]], Seq[Expression.MatchCase]) => CompilerIO[Expression],
    val desugarInTypeStack: Sourced[TypeStack[Expression]] => CompilerIO[Sourced[TypeStack[Expression]]]
) {

  def buildPatternHandler(
      scrutinee: Sourced[TypeStack[Expression]],
      subPatterns: Seq[Sourced[Pattern]],
      body: Sourced[TypeStack[Expression]]
  ): CompilerIO[Sourced[TypeStack[Expression]]] =
    for {
      desugaredBody <- desugarInTypeStack(body)
      handler       <-
        buildFieldLambdas(
          scrutinee,
          if subPatterns.isEmpty then Seq(scrutinee.as(Pattern.WildcardPattern(scrutinee.as("_")))) else subPatterns,
          desugaredBody
        )
    } yield handler

  def buildFieldLambdas(
      scrutinee: Sourced[TypeStack[Expression]],
      fieldPatterns: Seq[Sourced[Pattern]],
      body: Sourced[TypeStack[Expression]]
  ): CompilerIO[Sourced[TypeStack[Expression]]] =
    fieldPatterns.reverse.foldM(body)((innerBody, pat) => buildFieldLambda(scrutinee, pat, innerBody))

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
}
