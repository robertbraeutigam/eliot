package com.vanillasource.eliot.eliotc.source.stat

import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

import java.io.File
import java.time.Instant

/** Return file stats.
  */
case class FileStat(file: File, lastModified: Option[Instant]) extends CompilerFact {
  override def key(): CompilerFactKey[FileStat] = FileStat.Key(file)
}

object FileStat {
  case class Key(file: File) extends CompilerFactKey[FileStat]
}
