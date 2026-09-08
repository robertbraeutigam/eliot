package com.vanillasource.eliot.eliotc.core.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.{AST, DataDefinition, Expression, FunctionDefinition}
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Rejects the last piece of the effects v6 surface that parses but is not yet consumed: `with`, in either of its
  * positions. The `row` phase is what writes a binding into the phantom binders inside a `with`'s subject and erases
  * the node (§9.4 step 3, F1 part 3); until it does, a `with` would reach the checker as an unhandled node. Each
  * occurrence is one error at its own position, so a file using it reads as "not supported yet" rather than as a
  * silently different program. Deleted when the write lands.
  *
  * `effect` and the named `implement` are no longer rejected: [[EffectDefinitionDesugarer]] and
  * [[NamedImplementationDesugarer]] lower them.
  */
object UnsupportedSyntaxChecker {

  def check(ast: AST): Seq[Sourced[String]] =
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
