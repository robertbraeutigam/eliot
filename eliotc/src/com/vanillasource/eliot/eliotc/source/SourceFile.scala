package com.vanillasource.eliot.eliotc.source

import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey, CompilerSignal}

import java.io.File

case class SourceFile(path: File, rootPath: File) extends CompilerFact {
  override def key(): CompilerFactKey = SourceFile.Key(path)
}

object SourceFile {
  case class Key(path: File) extends CompilerFactKey {
    override type FactType = SourceFile
  }

}
