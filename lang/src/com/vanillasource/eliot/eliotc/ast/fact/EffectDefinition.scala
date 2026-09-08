package com.vanillasource.eliot.eliotc.ast.fact

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.ASTComponent.component
import com.vanillasource.eliot.eliotc.ast.fact.Primitives.*
import com.vanillasource.eliot.eliotc.ast.parser.Parser.{acceptIfAll, between, optional, or, recoveringAtLeastOnce}
import com.vanillasource.eliot.eliotc.ast.parser.{Parser, ParserError}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token

/** An `effect` declaration — effects v6's ability with no carrier binder (`docs/effects.md` §9.3):
  *
  * {{{
  * effect Console {
  *    def printLine(s: String): Unit
  *    def readLine: Option[String]
  * }
  * }}}
  *
  * Its members are the effect's operations; a member's own row lists what it performs *beyond* this effect. Landed dark
  * (§10.1 step 4): parsed into this node, carried on the [[AST]] beside the ordinary definitions, and rejected at core
  * as not supported yet. Unlike [[AbilityBlock]] the members are kept as written — the flag-day desugar (§10.2 F1)
  * decides their qualifier and marker.
  *
  * @param doc
  *   The `/** ... */` documentation comment preceding the declaration, attached by position in `ASTParser`.
  */
case class EffectDefinition(
    name: Sourced[String],
    genericParameters: Seq[GenericParameter],
    functions: Seq[FunctionDefinition],
    doc: Option[Sourced[String]] = None
)

object EffectDefinition {
  extension (self: EffectDefinition) def render: String = s"effect ${self.name.show}"

  val effectDefinition: ASTComponent[(Seq[ParserError], EffectDefinition)] =
    new ASTComponent[(Seq[ParserError], EffectDefinition)] {
      override val parser: Parser[Sourced[Token], (Seq[ParserError], EffectDefinition)] =
        for {
          _                   <- keyword("effect")
          name                <- acceptIfAll(isIdentifier, isUpperCase)("effect name")
          genericParameters   <- optionalBracketedCommaSeparatedItems("[", component[GenericParameter], "]")
          (errors, functions) <-
            (component[FunctionDefinition] or TypeAliasDefinition.typeAliasDefinition.parser)
              .recoveringAtLeastOnce(t => isKeyword(t) && (hasContent("def")(t) || hasContent("type")(t)))
              .between(symbol("{"), symbol("}"))
              .optional()
              .map(_.getOrElse(Seq.empty, Seq.empty))
        } yield (errors, EffectDefinition(name.map(_.content), genericParameters, functions))
    }
}
