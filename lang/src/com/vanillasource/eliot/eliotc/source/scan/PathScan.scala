package com.vanillasource.eliot.eliotc.source.scan

import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

import java.io.File
import java.net.URI
import java.nio.file.Path

case class PathScan(path: Path, files: Seq[URI], platform: Platform = Platform.Runtime) extends CompilerFact {
  override def key(): CompilerFactKey[PathScan] = PathScan.Key(path, platform)
}

object PathScan {
  case class Key(path: Path, platform: Platform = Platform.Runtime) extends CompilerFactKey[PathScan]
}
