package com.vanillasource.eliot.eliotc.ast

import cats.Show
import cats.syntax.all.*
import com.vanillasource.collections.Tree
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token

sealed trait FunctionBody

object FunctionBody {
  case class Native(nativeKeyword: Sourced[Token]) extends FunctionBody
  case class NonNative(body: Tree[Expression])     extends FunctionBody

  given Show[FunctionBody] = {
    case Native(_)       => "<native>"
    case NonNative(body) => body.show
  }
}
