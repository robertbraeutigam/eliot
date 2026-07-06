package com.vanillasource.eliot.eliotc.source.scan

import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

import java.io.File
import java.net.URI
import java.nio.file.Path

/** The result of resolving a module path against a platform's mount pool.
  *
  * `files` are every URI the module resolves to (across all mounts of the pool, in mount order). `overrideFiles` is the
  * subset of `files` that came from an **override-preferred** mount — the compiler-platform overlay, for a
  * [[Platform.Compiler]] scan. The layer merge prefers an implementation from an override file over a non-override one
  * for the same name (the compiler-as-platform override); it is always empty for a [[Platform.Runtime]] scan.
  */
case class PathScan(
    path: Path,
    files: Seq[URI],
    platform: Platform = Platform.Runtime,
    overrideFiles: Set[URI] = Set.empty
) extends CompilerFact {
  override def key(): CompilerFactKey[PathScan] = PathScan.Key(path, platform)
}

object PathScan {
  case class Key(path: Path, platform: Platform = Platform.Runtime) extends CompilerFactKey[PathScan]
}
