package com.vanillasource.eliot.eliotc.source.resource

import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.source.content.Sourced

import java.net.URI

case class ResourceContent(uri: URI, content: String) extends CompilerFact {
  override def key(): CompilerFactKey[ResourceContent] = ResourceContent.Key(uri)
}

object ResourceContent {
  case class Key(uri: URI) extends CompilerFactKey[ResourceContent]
}
