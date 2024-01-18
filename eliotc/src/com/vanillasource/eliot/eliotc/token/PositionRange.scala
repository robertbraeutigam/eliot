package com.vanillasource.eliot.eliotc.token

/** Delimiting a snippet of continuous code in a source.
  * @param from
  *   The position where the snippet starts, inclusive. This is the position of the first character.
  * @param to
  *   The position right after the last character.
  */
case class PositionRange(from: Position, to: Position)
