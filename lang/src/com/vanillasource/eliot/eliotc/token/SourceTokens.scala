package com.vanillasource.eliot.eliotc.token

import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}

import java.net.URI

/** @param docComments
  *   The documentation comments found in the source, in source order, each focused on its full range and carrying its
  *   inner text (delimiters excluded). They are kept out of [[tokens]] (the lexer discards all comments) and attached to
  *   the declaration each one precedes by [[com.vanillasource.eliot.eliotc.ast.processor.ASTParser]].
  */
case class SourceTokens(uri: URI, tokens: Sourced[Seq[Sourced[Token]]], docComments: Seq[Sourced[String]] = Seq.empty)
    extends CompilerFact {
  override def key(): SourceTokens.Key = SourceTokens.Key(uri)
}

object SourceTokens {
  case class Key(uri: URI) extends CompilerFactKey[SourceTokens]
}
