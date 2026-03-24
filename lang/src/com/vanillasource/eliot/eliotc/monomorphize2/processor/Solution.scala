package com.vanillasource.eliot.eliotc.monomorphize2.processor

import cats.Show
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.eval.fact.Value.valueUserDisplay
import com.vanillasource.eliot.eliotc.source.content.Sourced

case class Solution(substitutions: Map[String, Value] = Map.empty)

object Solution {
  given Show[Solution] = solution =>
    solution.substitutions
      .map { case (name, value) => s"?$name -> ${valueUserDisplay.show(value)}" }
      .mkString(", ")
}
