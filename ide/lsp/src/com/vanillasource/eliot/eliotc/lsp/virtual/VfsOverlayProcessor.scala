package com.vanillasource.eliot.eliotc.lsp.virtual

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.{CompilerFactKey, CompilerProcessor}
import com.vanillasource.eliot.eliotc.source.file.FileContent
import com.vanillasource.eliot.eliotc.source.stat.FileStat

import java.time.Instant

/** Serves `FileStat` / `FileContent` for files with an unsaved editor override from the [[VirtualFileSystem]] overlay,
  * delegating every other request — and every request for a file without an override — to the wrapped processor tree.
  *
  * This is the explicit, order-independent form of the overlay: it wraps the *complete* assembled processor tree (via
  * the wrapper parameter of [[com.vanillasource.eliot.eliotc.compiler.CompilationSession.create]]), so for an overlaid
  * file the on-disk readers are never consulted and no key is ever produced twice. It replaces the earlier arrangement
  * of two overlay processors sequenced ahead of the on-disk readers and winning the registration race by plugin-fold
  * order.
  *
  * The overlay `FileStat` carries the buffer's monotonic edit stamp, so an edit invalidates exactly that file's
  * dependency cone under incremental compilation; `FileContent` depends on `FileStat` exactly as the on-disk reader
  * does, so dropping the override flips the stat back to disk time and re-reads from disk.
  */
class VfsOverlayProcessor(vfs: VirtualFileSystem, underlying: CompilerProcessor)
    extends CompilerProcessor
    with Logging {

  override def generate(factKey: CompilerFactKey[?]): CompilerIO[Unit] =
    factKey match {
      case key: FileStat.Key    =>
        vfs.get(key.file) match {
          case Some(document) => registerFactIfClear(FileStat(key.file, Some(Instant.ofEpochMilli(document.stamp))))
          case None           => underlying.generate(key)
        }
      case key: FileContent.Key =>
        vfs.get(key.file) match {
          case Some(document) =>
            getFactOrAbort(FileStat.Key(key.file)) >> registerFactIfClear(FileContent(key.file, document.content))
          case None           => underlying.generate(key)
        }
      case other                => underlying.generate(other)
    }

  /** Wrappers (e.g. the visualization tracker) apply to the underlying tree; the overlay itself stays outermost so it
    * keeps intercepting before any wrapped processor runs.
    */
  override def wrapWith(wrapper: CompilerProcessor => CompilerProcessor): CompilerProcessor =
    new VfsOverlayProcessor(vfs, underlying.wrapWith(wrapper))
}
