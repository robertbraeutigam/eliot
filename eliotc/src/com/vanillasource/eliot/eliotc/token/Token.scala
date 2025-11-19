package com.vanillasource.eliot.eliotc.token

import cats.Show

sealed trait Token {
  val content: String
}

object Token {
  case class Identifier(content: String)     extends Token
  case class Symbol(content: String)         extends Token
  case class Keyword(content: String)        extends Token
  case class IntegerLiteral(content: String) extends Token
  case class StringLiteral(content: String)  extends Token

  given Show[Token] = {
    case Identifier(content)           => s"identifier '$content'"
    case Symbol(content)               => s"symbol '$content'"
    case Keyword(content)              => s"keyword '$content'"
    case Token.IntegerLiteral(content) => s"number literal '$content'"
    case Token.StringLiteral(content)  => s"string literal '$content'"
  }
}
