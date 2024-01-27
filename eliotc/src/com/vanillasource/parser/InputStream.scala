package com.vanillasource.parser

/** The input stream that will be read by the parser.
  * @param remainder
  *   The remaining items in the stream.
  * @param pos
  *   The position in the original stream. 0=at the beginning.
  */
case class InputStream[I](remainder: Seq[I], pos: Int) {
  def headOption: Option[I] = remainder.headOption

  def tail: InputStream[I] = InputStream(remainder.tail, pos + 1)
}

object InputStream {
  def of[I](input: Seq[I]): InputStream[I] = InputStream(input, 0)
}
