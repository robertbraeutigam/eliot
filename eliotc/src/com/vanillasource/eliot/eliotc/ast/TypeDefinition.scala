package com.vanillasource.eliot.eliotc.ast

import cats.Show
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token

case class TypeDefinition(typeName: Sourced[Token])

object TypeDefinition {
  given Show[TypeDefinition] = _.typeName.value.content
}
