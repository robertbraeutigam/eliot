package com.vanillasource.eliot.eliotc.ast

import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.token.Token

sealed trait Expression

object Expression {
  case class FunctionApplication(functionName: Sourced[Token]) extends Expression
}
