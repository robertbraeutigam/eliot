package com.vanillasource.eliot.eliotc.source

/** A single character's position inside a source.
 *
 * @param line
  *   The line of the position, starting from 1 (!).
  * @param col
  *   The column of the position, starting from 1 (!).
  */
case class Position(line: Position.Line, col: Position.Column) {
  def next = Position(line, col + 1)
}

object Position {
  type Line   = Int
  type Column = Int
}
