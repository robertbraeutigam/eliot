package com.vanillasource.eliot.eliotc.compiler.cache

import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

import java.io.File

/** Whether an output file (e.g. a generated JAR) is currently present on disk.
  *
  * This is a leaf fact (no dependencies), modeled exactly like a source `FileStat`: a side-effecting processor that
  * writes `file` takes a dependency on `OutputFileStat(file)`, so that deleting the output flips `present` to `false`,
  * differs from the cached `true`, and forces the writer to run again on an otherwise-unchanged build.
  *
  * Presence (rather than an mtime) is used deliberately: writing the file changes its mtime, so an mtime-valued output
  * fact would self-invalidate every run, whereas presence is stable across the write.
  */
case class OutputFileStat(file: File, present: Boolean) extends CompilerFact {
  override def key(): CompilerFactKey[OutputFileStat] = OutputFileStat.Key(file)
}

object OutputFileStat {
  case class Key(file: File) extends CompilerFactKey[OutputFileStat]
}
