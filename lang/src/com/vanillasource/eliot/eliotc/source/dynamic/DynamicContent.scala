package com.vanillasource.eliot.eliotc.source.dynamic

import com.vanillasource.eliot.eliotc.pos.PositionRange
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.{CompilerIO, registerInjectedFactIfClear}
import com.vanillasource.eliot.eliotc.source.content.{SourceContent, Sourced}
import com.vanillasource.eliot.eliotc.source.scan.PathScan

import java.net.URI
import java.nio.file.Path

/** Produce dynamic content to be processed by eliot. Registered as *injected* facts: no processor can reproduce them,
  * and they are read back by the very generation that registers them, so they must not inherit its dependency set.
  */
object DynamicContent {
  def addDynamicSource(path: Path, content: String): CompilerIO[Unit] = {
    val uri = URI.create(s"dynamic-source:${path.toString}")
    registerInjectedFactIfClear(SourceContent(uri, Sourced(uri, PositionRange.zero, content))) >>
      registerInjectedFactIfClear(PathScan(path, Seq(uri)))
  }

}
