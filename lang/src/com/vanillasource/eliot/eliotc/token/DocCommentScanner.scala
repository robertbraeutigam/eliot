package com.vanillasource.eliot.eliotc.token

import com.vanillasource.eliot.eliotc.pos.{Position, PositionRange}
import com.vanillasource.eliot.eliotc.source.content.Sourced

import java.net.URI

/** Extracts documentation comments (block comments opening with a doubled star) from raw source content.
  *
  * Eliot's lexer (see [[TokenParser]]) treats every block comment as whitespace and discards it, so documentation
  * comments never reach the token stream. This scanner recovers them directly from the source text by walking it left
  * to right, skipping string literals (the only place where a block-comment opener does not start a comment) and
  * line comments.
  *
  * Each returned [[Sourced]] holds the comment's *inner* text — what is written between the doubled-star opener and the
  * closer, delimiters excluded — focused on the comment's full range in the file (`from` at the leading slash, `to`
  * right after the closing slash). The standard empty block comment (open immediately followed by close) is
  * deliberately not treated as a documentation comment, matching the Scala/Java convention.
  *
  * It is only ever run on content the lexer has already accepted, so every comment and string is guaranteed
  * well-formed (terminated); it therefore never re-reports lexical errors.
  */
object DocCommentScanner {
  def scan(uri: URI, content: String): Seq[Sourced[String]] = {
    val docs   = scala.collection.mutable.ArrayBuffer.empty[Sourced[String]]
    val length = content.length
    var i      = 0
    var line   = 1
    var col    = 1

    def advance(): Unit = {
      if (content.charAt(i) == '\n') { line += 1; col = 1 }
      else col += 1
      i += 1
    }

    def at(prefix: String): Boolean = content.startsWith(prefix, i)

    while (i < length) {
      if (content.charAt(i) == '"') {
        advance() // opening quote
        var closed = false
        while (i < length && !closed) {
          if (content.charAt(i) == '\\' && i + 1 < length) { advance(); advance() } // escape: skip the escaped char
          else if (content.charAt(i) == '"') { advance(); closed = true }
          else advance()
        }
      } else if (at("//")) {
        while (i < length && content.charAt(i) != '\n') advance() // newline is consumed on the next iteration
      } else if (at("/*")) {
        val from  = Position(line, col)
        val isDoc = at("/**") && !at("/**/")
        advance(); advance()        // consume `/*`
        if (isDoc) advance()        // consume the third `*` so the captured text begins right after `/**`
        val inner  = new StringBuilder
        var closed = false
        while (i < length && !closed) {
          if (at("*/")) { advance(); advance(); closed = true }
          else { inner.append(content.charAt(i)); advance() }
        }
        if (isDoc) docs += Sourced(uri, PositionRange(from, Position(line, col)), inner.toString)
      } else advance()
    }

    docs.toSeq
  }
}
