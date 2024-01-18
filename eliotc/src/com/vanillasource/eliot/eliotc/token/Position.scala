package com.vanillasource.eliot.eliotc.token

/** A single character's position inside a source.
  * @param line
  *   The line of the position, starting from 0.
  * @param col
  *   The column of the position, starting from 0.
  */
case class Position(line: Position.Line, col: Position.Column)

object Position {
  opaque type Line   = Int
  opaque type Column = Int
}
