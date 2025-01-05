package com.vanillasource.eliot.eliotc.ast

import cats.Show
import cats.syntax.all.*
import com.vanillasource.collections.Tree
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token

case class TypeDefinition(
    name: Sourced[Token]
)

object TypeDefinition {
  given Show[TypeDefinition] = (fd: TypeDefinition) => s"${fd.name.show}"
}
