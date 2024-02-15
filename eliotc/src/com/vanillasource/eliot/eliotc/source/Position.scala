package com.vanillasource.eliot.eliotc.source

import cats.{Order, Show}
import cats.syntax.all.*

/** A single character's position inside a source.
  *
  * @param line
  *   The line of the position, starting from 1 (!).
  * @param col
  *   The column of the position, starting from 1 (!).
  */
case class Position(line: Position.Line, col: Position.Column) {
  def next: Position = Position(line, col + 1)
}

object Position {
  type Line   = Int
  type Column = Int

  given Order[Position] = (x: Position, y: Position) =>
    x.line - y.line match
      case result if result === 0 => x.col - y.col
      case result                 => result

  given Show[Position] = (x: Position) => s"${x.line}:${x.col}"
}
