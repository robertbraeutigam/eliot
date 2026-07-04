package com.vanillasource.eliot.eliotc.lsp.virtual

import java.io.File
import java.net.URI

/** The `vfs:` URI namespace: the identity a workspace file compiles under while an unsaved editor buffer overrides
  * it. A `vfs:` URI carries the same path as the file's `file:` URI, only the scheme differs, so translating between
  * the compiler's source identity and the editor's document identity is a scheme swap.
  */
object VfsUris {
  val scheme: String = "vfs"

  def isVfs(uri: URI): Boolean = uri.getScheme == scheme

  /** The `vfs:` identity of a file (used by the routed mount when an override is present). */
  def uriOf(file: File): URI = {
    val fileUri = file.toURI
    new URI(scheme, fileUri.getSchemeSpecificPart, fileUri.getFragment)
  }

  /** The file behind a `vfs:` URI. */
  def fileOf(uri: URI): File = new File(new URI("file", uri.getSchemeSpecificPart, uri.getFragment))

  /** The editor-facing (`file:`) form of any source URI; non-`vfs:` URIs pass through unchanged. Used wherever
    * compiler-side URIs meet editor documents (position indices, source-root lookup).
    */
  def toFileUri(uri: URI): URI = if (isVfs(uri)) new URI("file", uri.getSchemeSpecificPart, uri.getFragment) else uri
}
