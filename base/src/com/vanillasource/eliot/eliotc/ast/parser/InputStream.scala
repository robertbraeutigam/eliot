package com.vanillasource.eliot.eliotc.ast.parser

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

}
