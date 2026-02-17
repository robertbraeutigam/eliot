package com.vanillasource.eliot.eliotc.ast.parser

import scala.annotation.tailrec

case class InputStream[I](remainder: Seq[I], pos: Int) {
  def headOption: Option[I] = remainder.headOption

  def tail: InputStream[I] = InputStream(remainder.tail, pos + 1)

  def lastBefore(maxPos: Int): I = remainder.apply(math.min(remainder.length - 1, maxPos - 1))
}

object InputStream {
  def of[I](input: Seq[I]): InputStream[I] = InputStream(input, 0)
}
