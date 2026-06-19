package com.vanillasource.eliot.eliotc.lsp.virtual

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.source.stat.FileStat

import java.time.Instant

/** Reports a file's modification time from its editor buffer's monotonic stamp while an unsaved override is held, so an
  * edit invalidates that file's dependency cone under incremental compilation.
  *
  * `FileStat` is the leaf the whole source chain hangs off and is regenerated every compile, so this processor —
  * ordered ahead of the on-disk [[com.vanillasource.eliot.eliotc.source.stat.FileStatProcessor]], its registration
  * winning by being first — is the precise point at which a buffer change becomes visible to the cache. Files without
  * an override are left untouched (no fact registered) for the on-disk processor to handle.
  */
class VirtualFileStatProcessor(vfs: VirtualFileSystem) extends SingleKeyTypeProcessor[FileStat.Key] with Logging {
  override protected def generateFact(key: FileStat.Key): CompilerIO[Unit] =
    vfs.get(key.file) match {
      case Some(document) => registerFactIfClear(FileStat(key.file, Some(Instant.ofEpochMilli(document.stamp))))
      case None           => ().pure[CompilerIO]
    }
}
