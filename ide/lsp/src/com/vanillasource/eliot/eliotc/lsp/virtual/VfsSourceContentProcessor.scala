package com.vanillasource.eliot.eliotc.lsp.virtual

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}

/** Serves [[SourceContent]] for the `vfs:` namespace — a file's live editor-buffer content instead of its on-disk
  * state. Owns exactly the `vfs:` scheme (the on-disk reader owns `file:`); the [[VfsRoutedMount]] only emits a `vfs:`
  * URI while an override is held, so this processor is only ever asked for overlaid files. It reads the buffer's
  * [[VfsStat]] first, so the content re-derives when — and only when — the edit stamp changes; if the override was
  * dropped between scan and read (a close racing a compile), it declines and the next compile re-scans to the on-disk
  * identity.
  */
class VfsSourceContentProcessor(vfs: VirtualFileSystem) extends SingleKeyTypeProcessor[SourceContent.Key] {
  override protected def generateFact(key: SourceContent.Key): CompilerIO[Unit] =
    if (!VfsUris.isVfs(key.uri)) ().pure[CompilerIO]
    else {
      val file = VfsUris.fileOf(key.uri)
      getFactOrAbort(VfsStat.Key(file)) >> {
        vfs.get(file) match {
          case Some(document) =>
            registerFactIfClear(SourceContent(key.uri, Sourced(key.uri, PositionRange.zero, document.content)))
          case None           => abort
        }
      }
    }
}
