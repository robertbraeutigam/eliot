package com.vanillasource.eliot.eliotc.output

import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.module.ModuleName

case class Output(moduleName: ModuleName, content: Array[Byte]) extends CompilerFact {
  override def key(): CompilerFactKey = Output.Key(moduleName)
}

object Output {
  case class Key(moduleName: ModuleName) extends CompilerFactKey {
    override type FactType = Output
  }
}
