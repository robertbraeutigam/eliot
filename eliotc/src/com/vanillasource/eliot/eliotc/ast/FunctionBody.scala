package com.vanillasource.eliot.eliotc.ast

import cats.Show
import cats.syntax.all.*
import com.vanillasource.collections.Tree
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token

sealed trait FunctionBody

object FunctionBody {
  case class Native(nativeKeyword: Sourced[Token], args: Seq[Sourced[Token]]) extends FunctionBody
  case class NonNative(args: Seq[Sourced[Token]], body: Tree[Expression])     extends FunctionBody

  given Show[FunctionBody] = {
    case Native(nativeKeyword, args) => s"(${args.map(_.show).mkString(", ")}) <native>"
    case NonNative(args, body)       => s"(${args.map(_.show).mkString(", ")}) ${body.show}"
  }
}
