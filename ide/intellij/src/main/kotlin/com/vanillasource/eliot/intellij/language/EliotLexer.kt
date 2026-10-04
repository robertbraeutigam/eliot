package com.vanillasource.eliot.intellij.language

import com.intellij.lexer.LexerBase
import com.intellij.psi.TokenType
import com.intellij.psi.tree.IElementType

/**
 * Splits a file into runs of whitespace ([TokenType.WHITE_SPACE]) and runs of anything else
 * ([EliotParserDefinition.WORD]): `def main: {Console} Unit` is `def`, ` `, `main:`, ` `, `{Console}`, ` `, `Unit`.
 *
 * That is all the structure IntelliJ needs from the plugin — a leaf on every line to hang a marker on. It is not
 * Eliot's tokenizer, and nothing reads meaning from it: colouring is the TextMate grammar's and everything semantic is
 * the language server's.
 */
class EliotLexer : LexerBase() {
  private var buffer: CharSequence = ""
  private var bufferEnd = 0
  private var tokenStart = 0
  private var tokenEnd = 0

  override fun start(buffer: CharSequence, startOffset: Int, endOffset: Int, initialState: Int) {
    this.buffer = buffer
    this.bufferEnd = endOffset
    this.tokenStart = startOffset
    this.tokenEnd = startOffset
    advance()
  }

  override fun getState(): Int = 0

  override fun getTokenType(): IElementType? = when {
    tokenStart >= bufferEnd -> null
    buffer[tokenStart].isWhitespace() -> TokenType.WHITE_SPACE
    else -> EliotParserDefinition.WORD
  }

  override fun getTokenStart(): Int = tokenStart

  override fun getTokenEnd(): Int = tokenEnd

  override fun advance() {
    tokenStart = tokenEnd
    if (tokenStart >= bufferEnd) return
    val whitespace = buffer[tokenStart].isWhitespace()
    var end = tokenStart + 1
    while (end < bufferEnd && buffer[end].isWhitespace() == whitespace) end++
    tokenEnd = end
  }

  override fun getBufferSequence(): CharSequence = buffer

  override fun getBufferEnd(): Int = bufferEnd
}
