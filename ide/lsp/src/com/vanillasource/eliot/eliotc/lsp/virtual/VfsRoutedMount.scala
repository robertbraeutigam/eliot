package com.vanillasource.eliot.eliotc.lsp.virtual

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.scan.{FilesystemMount, SourceMount}

import java.net.URI
import java.nio.file.Path

/** A filesystem source root whose files route to the `vfs:` namespace while an unsaved editor buffer overrides them.
  *
  * For each candidate file it reads the total [[VfsStat]] leaf: an override present yields the file's `vfs:` URI —
  * *regardless of whether the file exists on disk*, so a never-saved new buffer is still visible to the scan — and no
  * override falls through to the plain filesystem resolution. Because the routing decision is an ordinary fact read
  * recorded as the scan's dependency, opening or closing a buffer invalidates exactly the scans it rerouted.
  */
final class VfsRoutedMount(root: Path) extends SourceMount {
  private val filesystem = FilesystemMount(root)

  override def resolve(path: Path): CompilerIO[Option[URI]] = {
    val file = root.resolve(path).toFile
    getFactOrAbort(VfsStat.Key(file)).flatMap { overlay =>
      overlay.stamp match {
        case Some(_) => Option(VfsUris.uriOf(file)).pure[CompilerIO]
        case None    => filesystem.resolve(path)
      }
    }
  }

  /** Enumerates the on-disk tree under `root` (the saved files), reusing the plain filesystem walk. A never-saved new
    * buffer that exists only as an editor overlay is not listed — an LSP-only completeness edge, harmless because the
    * whole-pool enumeration serves the compiler's `namedValues` reflection, which the language server does not run.
    */
  override def enumerate: CompilerIO[Seq[Path]] = filesystem.enumerate

  override def toString: String = s"VfsRoutedMount($root)"
}
