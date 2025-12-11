package com.vanillasource.eliot.eliotc.source.scan

import com.vanillasource.eliot.eliotc.{CompilerFact, CompilerFactKey}

import java.io.File
import java.nio.file.Path

case class PathScan(path: Path, files: Seq[File]) extends CompilerFact {
  override def key(): CompilerFactKey[PathScan] = PathScan.Key(path)
}

object PathScan {
  case class Key(path: Path) extends CompilerFactKey[PathScan]
}
