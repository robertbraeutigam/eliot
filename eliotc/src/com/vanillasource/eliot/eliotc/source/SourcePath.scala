package com.vanillasource.eliot.eliotc.source

import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey, CompilerSignal}

import java.io.File

/** Path to a source file or directory containing source files.
  */
case class SourcePath(path: File, rootPath: File) extends CompilerFact {
  override def key(): CompilerFactKey = SourcePath.Key(path)
}

object SourcePath {
  case class Key(path: File) extends CompilerFactKey {
    override type FactType = SourcePath
  }
}
