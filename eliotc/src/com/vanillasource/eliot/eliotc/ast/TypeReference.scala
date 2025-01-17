package com.vanillasource.eliot.eliotc.ast

import cats.Show
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token

case class TypeReference(typeName: Sourced[Token])

object TypeReference {
  given Show[TypeReference] = _.typeName.value.content
}
