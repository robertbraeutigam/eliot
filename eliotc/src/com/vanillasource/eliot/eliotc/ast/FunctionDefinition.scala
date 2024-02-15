package com.vanillasource.eliot.eliotc.ast

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token

case class FunctionDefinition(name: Sourced[Token], body: FunctionBody)

object FunctionDefinition {
  given Show[FunctionDefinition] = (t: FunctionDefinition) => s"${t.name.show}: ${t.body.show}"
}
