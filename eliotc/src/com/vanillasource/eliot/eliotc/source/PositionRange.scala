package com.vanillasource.eliot.eliotc.source

import cats.{Order, Show}
import cats.syntax.all.*

/** Delimiting a snippet of continuous code in a source.
  *
  * @param from
  *   The position where the snippet starts, inclusive. This is the position of the first character.
  * @param to
  *   The position right after the last character.
  */
case class PositionRange(from: Position, to: Position)

object PositionRange {
  val zero = PositionRange(Position.zero, Position.zero)

  given Show[PositionRange] = (r: PositionRange) => s"${r.from.show}->${r.to.show}"
}
