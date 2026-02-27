package com.vanillasource.eliot.eliotc.jvm.classgen.asm

import cats.Eq

case class JvmIdentifier(value: String)

object JvmIdentifier {
  def encode(name: String): JvmIdentifier =
    JvmIdentifier(name.map(encodeChar).mkString)

  private val namedEncodings: Map[Char, String] = Map(
    '_' -> "_us_",
    '.' -> "_dot_",
    '-' -> "_dash_",
    '+' -> "_plus_",
    '*' -> "_star_",
    '/' -> "_slash_",
    '=' -> "_eq_",
    '<' -> "_lt_",
    '>' -> "_gt_",
    '!' -> "_bang_",
    '?' -> "_qm_",
    ':' -> "_colon_",
    '&' -> "_amp_",
    '|' -> "_pipe_",
    '%' -> "_pct_",
    '^' -> "_caret_",
    '~' -> "_tilde_",
    '@' -> "_at_"
  )

  private def encodeChar(c: Char): String =
    namedEncodings.getOrElse(
      c,
      if (c == '$' || Character.isJavaIdentifierPart(c)) c.toString
      else f"_${c.toInt}%04X_"
    )

  given Eq[JvmIdentifier] = Eq.fromUniversalEquals
}
