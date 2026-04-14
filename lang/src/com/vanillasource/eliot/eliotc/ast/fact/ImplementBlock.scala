package com.vanillasource.eliot.eliotc.ast.fact

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.ASTComponent.component
import com.vanillasource.eliot.eliotc.ast.fact.Expression.FunctionApplication
import com.vanillasource.eliot.eliotc.ast.fact.Primitives.*
import com.vanillasource.eliot.eliotc.ast.parser.Parser.{
  acceptIfAll,
  between,
  optional,
  or,
  recoveringAnyTimes,
  recoveringAtLeastOnce
}
import com.vanillasource.eliot.eliotc.ast.parser.{Parser, ParserError}
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token

object ImplementBlock {
  val implementBlock: ASTComponent[(Seq[ParserError], Seq[FunctionDefinition])] =
    new ASTComponent[(Seq[ParserError], Seq[FunctionDefinition])] {
      override val parser: Parser[Sourced[Token], (Seq[ParserError], Seq[FunctionDefinition])] =
        for {
          _                   <- keyword("implement")
          genericParameters   <- component[Seq[GenericParameter]]
          name                <- acceptIfAll(isIdentifier, isUpperCase)("ability name")
          pattern             <- bracketedCommaSeparatedItems("[", sourced(Expression.typeParser), "]")
          (errors, functions) <-
            (component[FunctionDefinition] or TypeAliasDefinition.typeAliasDefinition.parser)
              .recoveringAtLeastOnce(t => isKeyword(t) && (hasContent("def")(t) || hasContent("type")(t)))
              .between(symbol("{"), symbol("}"))
              .optional()
              .map(_.getOrElse(Seq.empty, Seq.empty))
        } yield (
          errors,
          functions.map(f =>
            // Transform the function into an "implement" function. Change name into implement qualifier,
            // and also prepend the common generic parameters.
            // Note: the implement qualifier has to include the instantiation type parameters
            // Visibility is always public for implementation functions.
            FunctionDefinition(
              f.name.map(n => QualifiedName(n.name, Qualifier.AbilityImplementation(name.map(_.content), -1))),
              genericParameters ++ f.genericParameters,
              f.args,
              f.typeDefinition,
              f.body,
              visibility = Visibility.Public
            )
          ) :+
            // We add the implementation to the default method as a marker, that this type implements the marker.
            // The marker takes one argument per pattern element so its signature fully encodes the pattern,
            // which is what drives implementation selection during dispatch.
            FunctionDefinition(
              name.as(
                QualifiedName(name.value.content, Qualifier.AbilityImplementation(name.map(_.content), -1))
              ),
              genericParameters,
              pattern.zipWithIndex.map { case (p, i) => ArgumentDefinition(name.as(s"arg$i"), p) },
              pattern.head,
              Some(name.as(FunctionApplication(None, name.as("arg0"), Seq.empty, Seq.empty)))
            )
        )
    }
}
