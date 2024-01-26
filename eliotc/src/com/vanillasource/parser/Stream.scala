package com.vanillasource.parser

trait Stream[I] {
  def head: Option[I]

  def tail: Stream[I]
}

object Stream {
  def ofString(input: String): Stream[Char] = new Stream[Char] {
    override def head: Option[Char] = input.headOption

    override def tail: Stream[Char] = ofString(input.tail)
  }
}
