package com.vanillasource.eliot.eliotc.ast

import com.vanillasource.eliot.eliotc.ast.ASTComponent.component
import com.vanillasource.eliot.eliotc.ast.Primitives.*
import com.vanillasource.eliot.eliotc.token.Token
import com.vanillasource.parser.Parser
import com.vanillasource.parser.Parser.acceptIfAll
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.source.pos.Sourced

case class GenericParameter(name: Sourced[String], genericParameters: Seq[TypeReference])

object GenericParameter {
  given ASTComponent[Seq[GenericParameter]] = new ASTComponent[Seq[GenericParameter]] {
    override def parser: Parser[Sourced[Token], Seq[GenericParameter]] =
      optionalBracketedCommaSeparatedItems("[", genericParameter, "]")

    private val genericParameter = for {
      name              <- acceptIfAll(isUpperCase, isIdentifier)("generic type parameter")
      genericParameters <- optionalBracketedCommaSeparatedItems("[", component[TypeReference], "]")
    } yield GenericParameter(name.map(_.content), genericParameters)
  }
}
