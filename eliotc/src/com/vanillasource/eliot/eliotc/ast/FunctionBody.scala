package com.vanillasource.eliot.eliotc.ast

import com.vanillasource.collections.Tree

sealed trait FunctionBody

object FunctionBody {
  case object Native     extends FunctionBody
  case class NonNative() extends FunctionBody
}
