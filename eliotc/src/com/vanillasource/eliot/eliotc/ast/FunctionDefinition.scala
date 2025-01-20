package com.vanillasource.eliot.eliotc.ast

import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token

case class FunctionDefinition(
    name: Sourced[Token],
    args: Seq[ArgumentDefinition],
    typeDefinition: TypeReference,
    body: Option[Expression] // Can be empty for abstract functions
)

object FunctionDefinition {
  given Show[FunctionDefinition] = (fd: FunctionDefinition) =>
    s"${fd.name.show}(${fd.args.map(_.show).mkString(", ")}): ${fd.body.show}"
}
