package com.vanillasource.eliot.eliotc.jvm.classgen.asm

import cats.Eq

case class JvmIdentifier(value: String)

object JvmIdentifier {
  def encode(name: String): JvmIdentifier =
    JvmIdentifier(name.map(encodeChar).mkString)

  private def encodeChar(c: Char): String =
    if (c == '_') "_005F_"
    else if (c == '$' || Character.isJavaIdentifierPart(c)) c.toString
    else f"_${c.toInt}%04X_"

  given Eq[JvmIdentifier] = Eq.fromUniversalEquals
}
