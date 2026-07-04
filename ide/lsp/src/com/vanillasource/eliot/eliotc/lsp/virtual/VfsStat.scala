package com.vanillasource.eliot.eliotc.lsp.virtual

import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

import java.io.File

/** The buffer-override state of a file: the monotonic edit stamp of its unsaved editor buffer, or `None` when the
  * file has no override. A *total* fact (always produced) and a leaf (no dependencies), so it is re-read every
  * compile: opening/closing a buffer flips the value and invalidates the scan that routed on it, and every edit bumps
  * the stamp and invalidates the buffer-served content.
  */
case class VfsStat(file: File, stamp: Option[Long]) extends CompilerFact {
  override def key(): CompilerFactKey[VfsStat] = VfsStat.Key(file)
}

object VfsStat {
  case class Key(file: File) extends CompilerFactKey[VfsStat]
}
