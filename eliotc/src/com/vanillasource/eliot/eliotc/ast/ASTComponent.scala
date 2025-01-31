package com.vanillasource.eliot.eliotc.ast

import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token
import com.vanillasource.parser.Parser

trait ASTComponent[C] {
  def parser: Parser[Sourced[Token], C]
}

object ASTComponent {
  def component[C](using astComponent: ASTComponent[C]): Parser[Sourced[Token], C] = astComponent.parser
}
