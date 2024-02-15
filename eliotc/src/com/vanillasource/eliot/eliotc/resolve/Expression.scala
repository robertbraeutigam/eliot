package com.vanillasource.eliot.eliotc.resolve

import com.vanillasource.eliot.eliotc.module.FunctionFQN
import com.vanillasource.eliot.eliotc.source.Sourced

sealed trait Expression

object Expression {
  case class FunctionApplication(functionName: Sourced[FunctionFQN]) extends Expression
  case class IntegerLiteral(integerLiteral: Sourced[BigInt])         extends Expression
}
