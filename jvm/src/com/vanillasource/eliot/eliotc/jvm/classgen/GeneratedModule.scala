package com.vanillasource.eliot.eliotc.jvm.classgen

import com.vanillasource.eliot.eliotc.jvm.classgen.GeneratedModule.ClassFile
import com.vanillasource.eliot.eliotc.module.fact.ModuleName
import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}

case class GeneratedModule(moduleName: ModuleName, classFiles: Seq[ClassFile]) extends CompilerFact {
  override def key(): CompilerFactKey[GeneratedModule] = GeneratedModule.Key(moduleName)
}

object GeneratedModule {
  case class Key(moduleName: ModuleName) extends CompilerFactKey[GeneratedModule]

  case class ClassFile(fileName: String, bytecode: Array[Byte])
}
