package com.vanillasource.eliot.eliotc.ast.fact

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.ASTComponent.component
import com.vanillasource.eliot.eliotc.ast.fact.Primitives.*
import com.vanillasource.eliot.eliotc.ast.parser.Parser.{
  acceptIfAll,
  between,
  optional,
  recoveringAnyTimes,
  recoveringAtLeastOnce
}
import com.vanillasource.eliot.eliotc.ast.parser.{Parser, ParserError}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token

object AbilityBlock {
  val abilityBlock: ASTComponent[(Seq[ParserError], Seq[FunctionDefinition])] =
    new ASTComponent[(Seq[ParserError], Seq[FunctionDefinition])] {
      override val parser: Parser[Sourced[Token], (Seq[ParserError], Seq[FunctionDefinition])] =
        for {
          _                       <- keyword("ability")
          name                    <- acceptIfAll(isIdentifier, isUpperCase)("ability name")
          commonGenericParameters <- bracketedCommaSeparatedItems("[", component[GenericParameter], "]")
          (errors, functions)     <- component[FunctionDefinition]
                                       .recoveringAtLeastOnce(t => isKeyword(t) && hasContent("def")(t))
                                       .between(symbol("{"), symbol("}"))
                                       .optional()
                                       .map(_.getOrElse(Seq.empty, Seq.empty))
        } yield (
          errors,
          functions.map(f =>
            // Transform the function into an "ability" function. Change name into ability qualifier,
            // and also prepend the common generic parameters. Visibility is always public for ability functions.
            FunctionDefinition(
              f.name.map(n => QualifiedName(n.name, Qualifier.Ability(name.value.content))),
              commonGenericParameters ++ f.genericParameters,
              f.args,
              f.typeDefinition,
              f.body,
              visibility = Visibility.Public
            )
          ) :+
            // We add a default/invisible function/value to just indicate that this ability exists, even if it is empty.
            // It is named as the ability (upper case) and uses no additional type dependencies: Ability(a: A): A
            FunctionDefinition(
              name.as(QualifiedName(name.value.content, Qualifier.Ability(name.value.content))),
              commonGenericParameters,
              Seq(ArgumentDefinition(name.as("arg"), TypeReference(commonGenericParameters.head.name, Seq.empty))),
              TypeReference(commonGenericParameters.head.name, Seq.empty),
              None
            )
        )
    }
}
