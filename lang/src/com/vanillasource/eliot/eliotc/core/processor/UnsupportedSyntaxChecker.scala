package com.vanillasource.eliot.eliotc.core.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.{AST, DataDefinition, Expression, FunctionDefinition}
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Rejects the effects v6 surface that is parsed but not yet implemented (`docs/effects.md` §10.1 step 4, "landed
  * dark"): an `effect` declaration, a named `implement`, and `with` in either of its positions. Each occurrence is one
  * error at its own position, so a file using the new surface reads as "not supported yet" rather than as a parse
  * failure or a silently different program. Deleted at the flag day (§10.2 F1), when the desugar takes these nodes.
  */
object UnsupportedSyntaxChecker {

  def check(ast: AST): Seq[Sourced[String]] =
    ast.effectDefinitions.map(_.name.as("Effect declarations are not supported yet.")) ++
      ast.namedImplementations.map(_.name.as("Named implementations are not supported yet.")) ++
      ast.functionDefinitions.flatMap(inFunction) ++
      ast.typeDefinitions.flatMap(inData) ++
      ast.effectDefinitions.flatMap(_.functions.flatMap(inFunction)) ++
      ast.namedImplementations.flatMap(_.functions.flatMap(inFunction))

  private def inFunction(fd: FunctionDefinition): Seq[Sourced[String]] =
    (fd.args.map(_.typeExpression) ++ fd.body.toSeq :+ fd.typeDefinition).flatMap(inExpression)

  private def inData(dd: DataDefinition): Seq[Sourced[String]] =
    dd.constructors.toSeq.flatten.flatMap(_.fields.map(_.typeExpression)).flatMap(inExpression)

  private def inExpression(expr: Sourced[Expression]): Seq[Sourced[String]] = expr.value match {
    case Expression.WithBinding(subject, implementation) =>
      (implementation.as("Implementation binding with 'with' is not supported yet.") +: inExpression(subject)) ++
        inExpression(implementation)
    case Expression.FunctionApplication(_, _, genericArgs, args)         =>
      (genericArgs.toSeq.flatten ++ args).flatMap(inExpression)
    case Expression.FunctionLiteral(_, body)                             => inExpression(body)
    case Expression.FlatExpression(parts)                                => parts.flatMap(inExpression)
    case Expression.MatchExpression(scrutinee, cases)                    =>
      inExpression(scrutinee) ++ cases.flatMap(c => inExpression(c.body))
    case Expression.BlockExpression(lines)                               => lines.flatMap(l => inExpression(l.expression))
    case Expression.EffectfulType(effects, resultType, tail)             =>
      effects.flatMap(_.typeArgs).flatMap(inExpression) ++ inExpression(resultType) ++ tail.toSeq.flatMap(inExpression)
    case Expression.IntegerLiteral(_) | Expression.StringLiteral(_)      => Seq.empty
  }
}
