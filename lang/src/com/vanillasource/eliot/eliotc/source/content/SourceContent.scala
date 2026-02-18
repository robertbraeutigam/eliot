package com.vanillasource.eliot.eliotc.source.content

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.scan.PathScan

import java.io.File
import java.net.URI
import java.nio.file.Path

/** The contents of a source file.
  */
case class SourceContent(uri: URI, content: Sourced[String]) extends CompilerFact {
  override def key(): CompilerFactKey[SourceContent] = SourceContent.Key(uri)
}

object SourceContent {
  case class Key(uri: URI) extends CompilerFactKey[SourceContent]
}
