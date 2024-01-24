package com.vanillasource.eliot.eliotc.ast

import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token

import scala.util.parsing.input.{NoPosition, Position, Reader}

case class TokenStream(tokens: Seq[Sourced[Token]]) extends Reader[Sourced[Token]] {
  override def first: Sourced[Token] = tokens.head

  override def rest: Reader[Sourced[Token]] = TokenStream(tokens.tail)

  override def pos: Position = NoPosition

  override def atEnd: Boolean = tokens.isEmpty
}
