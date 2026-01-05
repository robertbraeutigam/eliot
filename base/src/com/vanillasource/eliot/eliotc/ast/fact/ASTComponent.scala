package com.vanillasource.eliot.eliotc.ast.fact

import com.vanillasource.eliot.eliotc.ast.fact.ASTComponent
import com.vanillasource.eliot.eliotc.ast.parser.Parser
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token

trait ASTComponent[C] {
  def parser: Parser[Sourced[Token], C]
}

object ASTComponent {
  def component[C](using astComponent: ASTComponent[C]): Parser[Sourced[Token], C] = astComponent.parser
}
