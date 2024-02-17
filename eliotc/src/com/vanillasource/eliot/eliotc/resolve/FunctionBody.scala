package com.vanillasource.eliot.eliotc.resolve

import com.vanillasource.collections.Tree
import com.vanillasource.eliot.eliotc.source.Sourced

sealed trait FunctionBody

object FunctionBody {
  case class Native(keyword: Sourced[Unit], args: Seq[Sourced[String]])    extends FunctionBody
  case class NonNative(args: Seq[Sourced[String]], body: Tree[Expression]) extends FunctionBody
}
