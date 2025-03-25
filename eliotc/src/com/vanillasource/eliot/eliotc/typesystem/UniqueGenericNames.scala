package com.vanillasource.eliot.eliotc.typesystem

import cats.data.State

import scala.annotation.tailrec

case class UniqueGenericNames(nextNameIndex: Int = 0) {
  def reserveNextName(): (UniqueGenericNames, String) =
    (UniqueGenericNames(nextNameIndex + 1), generateName(nextNameIndex, ""))

  @tailrec
  private def generateName(remainingIndex: Int, alreadyGeneratedSuffix: String): String =
    if (remainingIndex < 0) {
      alreadyGeneratedSuffix
    } else {
      generateName(
        (remainingIndex / 26) - 1,
        ('A'.toInt + (remainingIndex % 26)).toChar.toString + alreadyGeneratedSuffix
      )
    }
}

object UniqueGenericNames {
  def reserveNextName(): State[UniqueGenericNames, String] =
    State.apply(_.reserveNextName())
}
