package com.vanillasource.parser

case class ParserError[I](remaining: Seq[I], expected: Seq[String])
