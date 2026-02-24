package com.vanillasource.eliot.eliotc.symbolic.types

import scala.annotation.tailrec

case class ShortUniqueIdentifiers(nextIdentifierIndex: Int = 0) {

  /** Generate the next identifier string and return the advanced state. */
  def generateNext(): (String, ShortUniqueIdentifiers) =
    (generateIdentifier(nextIdentifierIndex, "$"), ShortUniqueIdentifiers(nextIdentifierIndex + 1))

  @tailrec
  private def generateIdentifier(remainingIndex: Int, alreadyGeneratedSuffix: String): String =
    if (remainingIndex < 0) {
      alreadyGeneratedSuffix
    } else {
      generateIdentifier(
        (remainingIndex / 26) - 1,
        ('A'.toInt + (remainingIndex % 26)).toChar.toString + alreadyGeneratedSuffix
      )
    }
}
