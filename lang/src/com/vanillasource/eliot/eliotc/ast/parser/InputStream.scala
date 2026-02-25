package com.vanillasource.eliot.eliotc.ast.parser

case class InputStream[I](remainder: Seq[I], pos: Int) {
  def headOption: Option[I] = remainder.headOption

  def tail: InputStream[I] = InputStream(remainder.tail, pos + 1)

  def dropWhile(predicate: I => Boolean): InputStream[I] = {
    val dropped = remainder.takeWhile(predicate).length
    InputStream(remainder.drop(dropped), pos + dropped)
  }

  def lastBefore(maxPos: Int): I = remainder.apply(math.min(remainder.length - 1, maxPos - pos - 1))
}

object InputStream {
  def of[I](input: Seq[I]): InputStream[I] = InputStream(input, 0)
}
