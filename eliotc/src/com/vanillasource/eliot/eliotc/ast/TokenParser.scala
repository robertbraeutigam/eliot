package com.vanillasource.eliot.eliotc.ast

import cats.Show
import com.vanillasource.eliot.eliotc.ast.ASTComponent.component
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import com.vanillasource.eliot.eliotc.token.Token.{Identifier, Keyword, Symbol}
import com.vanillasource.parser.Parser
import com.vanillasource.parser.Parser.*

object TokenParser {
  lazy val astParser: Parser[Sourced[Token], AST] = component[AST].fully()

  given Show[Token] = {
    case Identifier(content)           => s"identifier '$content'"
    case Symbol(content)               => s"symbol '$content'"
    case Keyword(content)              => s"keyword '$content'"
    case Token.IntegerLiteral(content) => s"number literal '$content'"
  }
}
