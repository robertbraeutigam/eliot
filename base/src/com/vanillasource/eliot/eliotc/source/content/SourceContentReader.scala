package com.vanillasource.eliot.eliotc.source.content

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor
import com.vanillasource.eliot.eliotc.source.file.FileContent
import com.vanillasource.eliot.eliotc.source.resource.ResourceContent

import java.io.File
import java.net.URI

/** Generates the source code for a given File, i.e. reads it from disk. Note, that this generator does not fail if the
  * File is missing, so it can be used to probe whether a file is present. If File is not present, or not readable, this
  * will just silently ignore the issue and not produce a fact.
  */
class SourceContentReader extends SingleFactProcessor[SourceContent.Key] with Logging {
  override protected def generateSingleFact(key: SourceContent.Key): CompilerIO[SourceContent] =
    generateContent(key.uri).map(content => SourceContent(key.uri, Sourced(key.uri, PositionRange.zero, content)))

  private def generateContent(uri: URI): CompilerIO[String] =
    if (uri.getScheme === "file") {
      // Get file based content
      getFactOrAbort(FileContent.Key(new File(uri.toURL.getFile))).map(_.content)
    } else {
      getFactOrAbort(ResourceContent.Key(uri)).map(_.content)
    }
}
