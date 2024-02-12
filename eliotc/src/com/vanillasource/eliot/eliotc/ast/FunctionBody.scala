package com.vanillasource.eliot.eliotc.ast

import com.vanillasource.collections.Tree
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token

sealed trait FunctionBody

object FunctionBody {
  case class Native(nativeKeyword: Sourced[Token], args: Seq[Sourced[Token]]) extends FunctionBody
  case class NonNative(args: Seq[Sourced[Token]], body: Tree[Expression])     extends FunctionBody
}
