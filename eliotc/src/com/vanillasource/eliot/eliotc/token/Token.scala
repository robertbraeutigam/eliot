package com.vanillasource.eliot.eliotc.token

sealed trait Token {
  val content: String
}

object Token {
  case class Identifier(content: String)   extends Token
  case class Symbol(content: String)       extends Token
  case class Keyword(content: String)      extends Token
  case class IntegerLiteral(value: BigInt) extends Token {
    override val content: String = value.toString
  }
}
