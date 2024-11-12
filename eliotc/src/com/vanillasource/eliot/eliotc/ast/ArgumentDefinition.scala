package com.vanillasource.eliot.eliotc.ast

import cats.Show
import cats.syntax.all._
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token

case class ArgumentDefinition(name: Sourced[Token], typeDefinition: TypeDefinition)

object ArgumentDefinition {
  given Show[ArgumentDefinition] = _.name.show
}
