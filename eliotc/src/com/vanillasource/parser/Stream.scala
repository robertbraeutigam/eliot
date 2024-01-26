package com.vanillasource.parser

trait Stream[I] {
  def head: Option[I]

  def tail: Stream[I]
}

object Stream {}
