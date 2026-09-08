package com.vanillasource.eliot.eliotc.ast.fact

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.ASTComponent.component
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
          (errors, functions)     <-
            (component[FunctionDefinition] or TypeAliasDefinition.typeAliasDefinition.parser)
              .recoveringAtLeastOnce(t => isKeyword(t) && (hasContent("def")(t) || hasContent("type")(t)))
              .between(symbol("{"), symbol("}"))
              .optional()
              .map(_.getOrElse(Seq.empty, Seq.empty))
        } yield (errors, AbilityMembers.lower(name.map(_.content), commonGenericParameters, functions, performsItself = false))
    }
}
