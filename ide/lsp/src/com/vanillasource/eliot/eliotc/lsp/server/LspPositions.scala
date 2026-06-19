package com.vanillasource.eliot.eliotc.lsp.server

import com.vanillasource.eliot.eliotc.pos.{Position, PositionRange}
import org.eclipse.lsp4j.{Position as LspPosition, Range as LspRange}

/** Translates between the compiler's source positions and LSP's.
  *
  * The compiler is 1-based in both line and column, with an exclusive `to`; LSP is 0-based in both line and character,
  * also with an exclusive range end. So only the 1→0 base shift is needed — the exclusive ends already line up.
  */
object LspPositions {

  /** A compiler range as an LSP range. */
  def toRange(range: PositionRange): LspRange =
    new LspRange(toLspPosition(range.from), toLspPosition(range.to))

  /** A compiler position as an LSP position (1-based → 0-based, never negative). */
  def toLspPosition(position: Position): LspPosition =
    new LspPosition(math.max(position.line - 1, 0), math.max(position.col - 1, 0))

  /** An LSP position as a compiler position (0-based → 1-based). */
  def toCompilerPosition(position: LspPosition): Position =
    Position(position.getLine + 1, position.getCharacter + 1)
}
