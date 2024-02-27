package com.vanillasource.eliot.eliotc.resolve

import com.vanillasource.collections.Tree
import com.vanillasource.eliot.eliotc.source.Sourced

sealed trait FunctionBody

object FunctionBody {
  case class Native(keyword: Sourced[Unit])    extends FunctionBody
  case class NonNative(body: Tree[Expression]) extends FunctionBody
}
