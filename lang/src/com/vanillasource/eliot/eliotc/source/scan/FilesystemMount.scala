package com.vanillasource.eliot.eliotc.source.scan

import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.source.stat.FileStat

import java.net.URI
import java.nio.file.Path

/** A filesystem source root: resolves a module-relative path to a `file:` URI when the file exists under `root`.
  * Existence is read through the [[FileStat]] fact (a leaf), so the scan's incremental invalidation hangs off the
  * file's stat exactly as before mounts existed.
  */
final case class FilesystemMount(root: Path) extends SourceMount {
  override def resolve(path: Path): CompilerIO[Option[URI]] =
    getFactOrAbort(FileStat.Key(root.resolve(path).toFile))
      .map(stat => Option.when(stat.lastModified.isDefined)(stat.file.toURI))
}
