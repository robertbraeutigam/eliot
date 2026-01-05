package com.vanillasource.eliot.eliotc.module.fact

import com.vanillasource.eliot.eliotc.ast.fact.FunctionDefinition
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

import java.io.File

case class ModuleFunction(
    file: File,
    ffqn: FunctionFQN,
    functionDictionary: Map[String, FunctionFQN],
    typeDictionary: Map[String, TypeFQN],
    functionDefinition: FunctionDefinition
) extends CompilerFact {
  override def key(): CompilerFactKey[ModuleFunction] = ModuleFunction.Key(file, ffqn)
}

object ModuleFunction {
  case class Key(file: File, ffqn: FunctionFQN) extends CompilerFactKey[ModuleFunction]
}
