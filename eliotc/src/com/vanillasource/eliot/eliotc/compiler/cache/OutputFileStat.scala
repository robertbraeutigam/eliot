package com.vanillasource.eliot.eliotc.compiler.cache

import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

import java.io.File

/** The content of an output file (e.g. a generated JAR) as it currently stands on disk, digested — `None` when the file
  * is not there at all.
  *
  * This is a leaf fact (no dependencies), modeled exactly like a source `FileStat`: a side-effecting processor that
  * writes `file` takes a dependency on `OutputFileStat(file)`, and that dependency is the only thing tying its cached
  * fact to the file's real state. Deleting, truncating or replacing the output changes the digest, differs from the
  * cached one, and forces the writer to run again on an otherwise-unchanged build.
  *
  * A digest rather than an mtime, because writing the file changes its mtime, so an mtime-valued output fact would
  * self-invalidate every run. It is read *after* the write, so what it records is the content this run left behind;
  * output being deterministic, an unchanged rebuild recomputes the same digest and skips the write.
  *
  * Presence alone — the earlier shape of this fact — was not enough: a JAR corrupted, truncated or replaced behind the
  * compiler's back is still *present*, and so was silently accepted by an otherwise successful build.
  */
case class OutputFileStat(file: File, digest: Option[String]) extends CompilerFact {
  override def key(): CompilerFactKey[OutputFileStat] = OutputFileStat.Key(file)
}

object OutputFileStat {
  case class Key(file: File) extends CompilerFactKey[OutputFileStat]
}
