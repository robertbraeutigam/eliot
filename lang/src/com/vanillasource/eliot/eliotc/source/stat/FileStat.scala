package com.vanillasource.eliot.eliotc.source.stat

import com.vanillasource.eliot.eliotc.codec.LangFactCodecs

import com.vanillasource.eliot.eliotc.compiler.cache.codec.FactCodec

import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

import java.io.File
import java.time.Instant

/** Return file stats: the identity the incremental cache compares a source file (or a walked directory) by.
  *
  * `lastModified` is `None` exactly when the file does not exist, which is what the mounts probe for.
  *
  * `unsettled` is the guard on the one thing an mtime cannot answer. A filesystem stamps mtimes at a coarse tick (4 ms
  * on a stock Linux kernel — the timestamp's nanosecond field is populated at that same tick, so precision is not the
  * issue), so two writes inside one tick leave the *same* mtime with *different* content. Comparing on mtime alone then
  * reports "unchanged" for a file that changed, and the build silently compiles the previous version.
  *
  * So a stat taken too soon after the mtime it read does not claim to identify anything: `unsettled` holds a token
  * unique to that one observation, so such a file never compares equal to itself and is always re-read. Equality one
  * level up at [[com.vanillasource.eliot.eliotc.source.file.FileContent]] — which holds the whole content — then stops
  * propagation whenever the content did not actually change, so the cost of the doubt is one file read and nothing
  * downstream.
  *
  * The token is a **nonce, deliberately not a timestamp**: the defect being guarded against is a clock too coarse to
  * tell two observations apart, and `IO.realTime` is millisecond-resolution, so dating the observation would rebuild
  * the same hole one level down. Nothing reads the token; only its inequality is load-bearing.
  *
  * `None` means the mtime is old enough to be conclusive, and comparison is on the mtime alone: see
  * [[FileStatProcessor]] for why that is sound and what "old enough" has to dominate.
  */
case class FileStat(file: File, lastModified: Option[Instant], unsettled: Option[String]) extends CompilerFact {
  override def key(): CompilerFactKey[FileStat] = FileStat.Key(file)
}

object FileStat {
  case class Key(file: File) extends CompilerFactKey[FileStat] {
    override def valueCodec: Option[FactCodec[FileStat]] = Some(LangFactCodecs.fileStatCodec)
  }
}
