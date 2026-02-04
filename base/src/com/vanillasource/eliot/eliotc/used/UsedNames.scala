package com.vanillasource.eliot.eliotc.used

import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey}
import com.vanillasource.eliot.eliotc.used.UsedNames.UsageStats

case class UsedNames(rootFQN: ValueFQN, usedNames: Map[ValueFQN, UsageStats]) extends CompilerFact {
  override def key(): CompilerFactKey[UsedNames] = UsedNames.Key(rootFQN)
}

object UsedNames {
  case class Key(rootFQN: ValueFQN) extends CompilerFactKey[UsedNames]

  case class UsageStats(
      // A list of monomorphic type parameters this value is used with
      monomorphicTypeParameters: Seq[Seq[Value]],
      // Statistics about how many arguments are applied directly to the value,
      // with how many times this happened. Constants would always have 0 -> n, where
      // n is the number of times it occurs in the source.
      directCallApplications: Map[Int, Int]
  ) {
    def highestArity: Option[Int] = directCallApplications.values.maxOption
  }
}
