package com.vanillasource.eliot.eliotc.ast.fact

import cats.syntax.all.*
import Primitives.*
import com.vanillasource.eliot.eliotc.ast.parser.Parser
import com.vanillasource.eliot.eliotc.ast.parser.Parser.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.token.Token

sealed trait Visibility

object Visibility {
  case object Public    extends Visibility
  case object Qualified extends Visibility
  case object Private   extends Visibility

  given ASTComponent[Visibility] = new ASTComponent[Visibility] {
    override val parser: Parser[Sourced[Token], Visibility] =
      identifierWith("qualified").as(Visibility.Qualified: Visibility) or
        identifierWith("private").as(Visibility.Private: Visibility)
  }
}
