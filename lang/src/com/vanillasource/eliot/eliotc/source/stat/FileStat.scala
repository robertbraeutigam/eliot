package com.vanillasource.eliot.eliotc.source.stat

import com.vanillasource.eliot.eliotc.codec.LangFactCodecs

import com.vanillasource.eliot.eliotc.compiler.cache.codec.FactCodec

import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

import java.io.File
import java.time.Instant

/** Return file stats.
  */
case class FileStat(file: File, lastModified: Option[Instant]) extends CompilerFact {
  override def key(): CompilerFactKey[FileStat] = FileStat.Key(file)
}

object FileStat {
  case class Key(file: File) extends CompilerFactKey[FileStat] {
    override def valueCodec: Option[FactCodec[FileStat]] = Some(LangFactCodecs.fileStatCodec)
  }
}
