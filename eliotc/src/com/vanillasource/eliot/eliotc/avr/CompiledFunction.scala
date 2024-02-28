package com.vanillasource.eliot.eliotc.avr

import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.module.FunctionFQN

case class CompiledFunction(ffqn: FunctionFQN, instr: AVRInstruction) extends CompilerFact {
  override def key(): CompilerFactKey = CompiledFunction.Key(ffqn)
}

object CompiledFunction {
  case class Key(ffqn: FunctionFQN) extends CompilerFactKey {
    override type FactType = CompiledFunction
  }
}
