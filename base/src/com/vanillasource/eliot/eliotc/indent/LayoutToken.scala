package com.vanillasource.eliot.eliotc.indent

import cats.{Eq, Show}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.token.Token

sealed trait LayoutToken

object LayoutToken {
  case class ContentToken(token: Token) extends LayoutToken
  case object Dedent                    extends LayoutToken
  case object Indent                    extends LayoutToken
  case object Newline                   extends LayoutToken

  given Eq[LayoutToken] = Eq.fromUniversalEquals

  given Show[LayoutToken] = {
    case ContentToken(token) => token.show
    case Dedent              => "dedent"
    case Indent              => "indent"
    case Newline             => "newline"
  }
}
