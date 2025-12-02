package com.vanillasource.parser

trait InputStream[I] {
  def headOption: Option[I]

  def tail: InputStream[I]

  def pos: Int
}

object InputStream {
  def of[I](input: Seq[I]): InputStream[I] = SeqInputStream(input, 0)

  /** The input stream that will be read by the parser.
    *
    * @param remainder
    *   The remaining items in the stream.
    * @param pos
    *   The position in the original stream. 0=at the beginning.
    */
  case class SeqInputStream[I](remainder: Seq[I], pos: Int) extends InputStream[I] {
    def headOption: Option[I] = remainder.headOption

    def tail: InputStream[I] = SeqInputStream(remainder.tail, pos + 1)
  }

  /** An input that will maintain a list of items consumed.
    */
  case class LimitedInputStream[I](input: InputStream[I], maxPos: Int) extends InputStream[I] {
    override def headOption: Option[I] = if (pos < maxPos) input.headOption else None

    override def tail: InputStream[I] = LimitedInputStream(input.tail, maxPos)

    override def pos: Int = input.pos
  }
}
