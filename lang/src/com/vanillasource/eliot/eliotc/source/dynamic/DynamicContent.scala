package com.vanillasource.eliot.eliotc.source.dynamic

import com.vanillasource.eliot.eliotc.pos.PositionRange
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.{CompilerIO, registerFactIfClear}
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}
import com.vanillasource.eliot.eliotc.source.scan.PathScan

import java.net.URI
import java.nio.file.Path

/** Produce dynamic content to be processed by eliot.
  */
object DynamicContent {
  def addDynamicSource(path: Path, content: String): CompilerIO[Unit] = {
    val uri = URI.create(s"dynamic-source:${path.toString}")
    registerFactIfClear(SourceContent(uri, Sourced(uri, PositionRange.zero, content))) >>
      registerFactIfClear(PathScan(path, Seq(uri)))
  }

}
