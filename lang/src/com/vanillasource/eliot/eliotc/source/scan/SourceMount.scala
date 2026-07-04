package com.vanillasource.eliot.eliotc.source.scan

import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO

import java.net.URI
import java.nio.file.Path

/** One namespace of source files a [[PathScanner]] pool resolves against.
  *
  * A mount answers a module-relative source path with the URI it serves that file under, or `None` when the mount does
  * not have it. Each mount owns a URI namespace (a scheme, or a filesystem subtree of `file:`), and the processor that
  * produces [[com.vanillasource.eliot.eliotc.source.content.SourceContent]] for that namespace is the mount's
  * counterpart — so every source fact has exactly one producer by key-space construction, with no override or
  * interception mechanism. Examples: [[FilesystemMount]] (a filesystem root, `file:` URIs, content read from disk), the
  * jvm target's synthetic `main.els` mount (a computed module), the LSP's buffer-overlay routing.
  *
  * `resolve` runs inside the `PathScan` generation, so any facts a mount reads (file stats, overlay stats) are recorded
  * as the scan's dependencies and drive its incremental invalidation.
  */
trait SourceMount {
  def resolve(path: Path): CompilerIO[Option[URI]]
}
