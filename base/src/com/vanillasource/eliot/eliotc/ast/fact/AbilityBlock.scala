package com.vanillasource.eliot.eliotc.ast.fact

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.ASTComponent.component
import com.vanillasource.eliot.eliotc.ast.fact.Primitives.*
import com.vanillasource.eliot.eliotc.ast.parser.Parser.{acceptIfAll, between, recoveringAnyTimes}
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
          commonGenericParameters <- component[Seq[GenericParameter]]
          (errors, functions)     <- component[FunctionDefinition]
                                       .recoveringAnyTimes(t => isKeyword(t) && hasContent("def")(t))
                                       .between(symbol("{"), symbol("}"))
        } yield (
          errors,
          functions.map(f =>
            // Transform the function into an "ability" function. Change name into ability qualifier,
            // and also prepend the common generic parameters.
            FunctionDefinition(
              f.name.map(n => QualifiedName(n.name, Qualifier.Ability(name.value.content))),
              commonGenericParameters ++ f.genericParameters,
              f.args,
              f.typeDefinition,
              f.body
            )
          )
        )
    }
}
