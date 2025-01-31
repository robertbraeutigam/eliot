package com.vanillasource.eliot.eliotc.ast

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token

case class DataDefinition(
    name: Sourced[Token]
)

object DataDefinition {
  given Show[DataDefinition] = (fd: DataDefinition) => s"${fd.name.show}"
}
