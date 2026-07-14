package com.vanillasource.eliot.eliotc.matchdesugar.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, Pattern}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import MatchDesugarUtils.*

/** The recursion hooks back into the match desugarer, carrying the [[Platform]] marker (CP1) so that nested-match and
  * sub-expression desugaring resolve every cross-value fact (constructors, ability-method markers) in the same pool as
  * the value being desugared. The marker is a plain parameter on the hooks (and on this context's own builders) rather
  * than captured, so the single shared context instance stays correct across both phases.
  */
class MatchDesugarContext(
    val desugarMatch: (Sourced[Expression], Seq[Expression.MatchCase], Platform) => CompilerIO[Expression],
    val desugarSourced: (Sourced[Expression], Platform) => CompilerIO[Sourced[Expression]]
) {

  def buildPatternHandler(
      scrutinee: Sourced[Expression],
      subPatterns: Seq[Sourced[Pattern]],
      body: Sourced[Expression],
      platform: Platform
  ): CompilerIO[Sourced[Expression]] =
    for {
      desugaredBody <- desugarSourced(body, platform)
      handler       <-
        buildFieldLambdas(
          scrutinee,
          if subPatterns.isEmpty then Seq(scrutinee.as(Pattern.WildcardPattern(scrutinee.as("_")))) else subPatterns,
          desugaredBody,
          platform
        )
    } yield handler

  def buildFieldLambdas(
      scrutinee: Sourced[Expression],
      fieldPatterns: Seq[Sourced[Pattern]],
      body: Sourced[Expression],
      platform: Platform
  ): CompilerIO[Sourced[Expression]] =
    fieldPatterns.reverse.foldM(body)((innerBody, pat) => buildFieldLambda(scrutinee, pat, innerBody, platform))

  private def buildFieldLambda(
      scrutinee: Sourced[Expression],
      fieldPat: Sourced[Pattern],
      innerBody: Sourced[Expression],
      platform: Platform
  ): CompilerIO[Sourced[Expression]] =
    bindingName(fieldPat.value) match {
      case Some(name) =>
        wrapExpr(scrutinee, Expression.FunctionLiteral(name, None, innerBody)).pure[CompilerIO]
      case None       =>
        val freshName = fieldPat.as("$match_field")
        val fieldRef  = wrapExpr(scrutinee, Expression.ParameterReference(freshName))
        desugarMatch(fieldRef, Seq(Expression.MatchCase(fieldPat, innerBody)), platform).map { nestedMatch =>
          wrapExpr(scrutinee, Expression.FunctionLiteral(freshName, None, wrapExpr(scrutinee, nestedMatch)))
        }
    }
}
