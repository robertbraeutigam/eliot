package com.vanillasource.eliot.eliotc.source.scan

import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

import java.io.File
import java.net.URI
import java.nio.file.Path

case class PathScan(path: Path, files: Seq[URI]) extends CompilerFact {
  override def key(): CompilerFactKey[PathScan] = PathScan.Key(path)
}

object PathScan {
  case class Key(path: Path) extends CompilerFactKey[PathScan]
}
