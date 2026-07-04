package com.vanillasource.eliot.eliotc.source.content

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.source.file.FileContent

import java.io.File
import java.net.URI

/** Generates the source code for a `file:` URI, i.e. reads it from disk. Note, that this generator does not fail if
  * the File is missing, so it can be used to probe whether a file is present. If File is not present, or not readable,
  * this will just silently ignore the issue and not produce a fact.
  *
  * This reader owns the `file:` namespace of [[SourceContent]] only; a URI of any other scheme belongs to the processor
  * counterpart of the [[com.vanillasource.eliot.eliotc.source.scan.SourceMount]] that emitted it (the jvm target's
  * synthetic `main.els`, the LSP's `vfs:` buffer overlay), so such keys are left untouched here.
  */
class SourceContentReader extends SingleKeyTypeProcessor[SourceContent.Key] with Logging {
  override protected def generateFact(key: SourceContent.Key): CompilerIO[Unit] =
    if (key.uri.getScheme != "file") ().pure[CompilerIO]
    else
      generateContent(key.uri)
        .map(content => SourceContent(key.uri, Sourced(key.uri, PositionRange.zero, content)))
        .flatMap(registerFactIfClear)

  private def generateContent(uri: URI): CompilerIO[String] =
    getFactOrAbort(FileContent.Key(new File(uri.toURL.getFile))).map(_.content)
}
