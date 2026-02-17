package com.vanillasource.eliot.eliotc.ast.fact

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.ASTComponent.component
import com.vanillasource.eliot.eliotc.ast.fact.Primitives.*
import com.vanillasource.eliot.eliotc.ast.parser.Parser.{acceptIfAll, between, recoveringAnyTimes}
import com.vanillasource.eliot.eliotc.ast.parser.{Parser, ParserError}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token

object ImplementBlock {
  val implementBlock: ASTComponent[(Seq[ParserError], Seq[FunctionDefinition])] =
    new ASTComponent[(Seq[ParserError], Seq[FunctionDefinition])] {
      override val parser: Parser[Sourced[Token], (Seq[ParserError], Seq[FunctionDefinition])] =
        for {
          _                   <- keyword("implement")
          name                <- acceptIfAll(isIdentifier, isUpperCase)("ability name")
          genericParameters   <- component[Seq[GenericParameter]]
          (errors, functions) <- component[FunctionDefinition]
                                   .recoveringAnyTimes(t => isKeyword(t) && hasContent("def")(t))
                                   .between(symbol("{"), symbol("}"))
        } yield (
          errors,
          functions.map(f =>
            // Transform the function into an "implement" function. Change name into implement qualifier,
            // and also prepend the common generic parameters.
            // Note: the implement qualifier has to include the instantiation type parameters
            FunctionDefinition(
              f.name.map(n =>
                QualifiedName(n.name, Qualifier.AbilityImplementation(name.value.content, genericParameters))
              ),
              f.genericParameters, // Don't add parameters to the function, it'll have to have those already
              f.args,
              f.typeDefinition,
              f.body
            )
          )
        )
    }
}
