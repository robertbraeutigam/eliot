package com.vanillasource.eliot.eliotc.lsp.virtual

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.source.file.FileContent
import com.vanillasource.eliot.eliotc.source.stat.FileStat

/** Serves a file's content from its editor buffer while an unsaved override is held, instead of reading from disk.
  *
  * Ordered ahead of the on-disk [[com.vanillasource.eliot.eliotc.source.file.FileContentReader]] so its registration
  * wins. It depends on [[FileStat]] exactly as the on-disk reader does (via [[VirtualFileStatProcessor]] this resolves
  * to the buffer's stamp), so the content is re-derived when — and only when — the stamp changes, and dropping the
  * override later flips `FileStat` back to the disk time and re-reads from disk. Files without an override register
  * nothing here, leaving the on-disk reader to handle them.
  */
class VirtualFileContentReader(vfs: VirtualFileSystem) extends SingleKeyTypeProcessor[FileContent.Key] with Logging {
  override protected def generateFact(key: FileContent.Key): CompilerIO[Unit] =
    vfs.get(key.file) match {
      case Some(document) =>
        getFactOrAbort(FileStat.Key(key.file)) >> registerFactIfClear(FileContent(key.file, document.content))
      case None           => ().pure[CompilerIO]
    }
}
