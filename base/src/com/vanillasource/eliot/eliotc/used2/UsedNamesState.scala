package com.vanillasource.eliot.eliotc.used2

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.used2.UsedNames.UsageStats

case class UsedNamesState(
    usedNames: Map[ValueFQN, UsedNamesState.UsageStatsBuilder] = Map.empty,
    visited: Set[(ValueFQN, Seq[Value])] = Set.empty
)

object UsedNamesState {
  type UsedNamesIO[T] = StateT[CompilerIO, UsedNamesState, T]

  case class UsageStatsBuilder(
      monomorphicTypeParameters: Seq[Seq[Value]] = Seq.empty,
      directCallApplications: Map[Int, Int] = Map.empty
  ) {
    def addTypeParameters(typeParams: Seq[Value]): UsageStatsBuilder =
      copy(monomorphicTypeParameters = monomorphicTypeParameters :+ typeParams)

    def addDirectCallApplication(argCount: Int): UsageStatsBuilder =
      copy(directCallApplications = directCallApplications.updatedWith(argCount) {
        case Some(count) => Some(count + 1)
        case None        => Some(1)
      })

    def toUsageStats: UsageStats =
      UsageStats(monomorphicTypeParameters, directCallApplications)
  }

  extension [T](io: CompilerIO[T]) {
    def liftToUsedNames: UsedNamesIO[T] = StateT.liftF(io)
  }

  def getUsedNames(rootFQN: ValueFQN, state: UsedNamesState): UsedNames =
    UsedNames(
      rootFQN,
      usedNames = state.usedNames.map((vfqn, builder) => vfqn -> builder.toUsageStats)
    )

  def isVisited(vfqn: ValueFQN, typeArgs: Seq[Value]): UsedNamesIO[Boolean] =
    StateT.get[CompilerIO, UsedNamesState].map(_.visited.contains((vfqn, typeArgs)))

  def markVisited(vfqn: ValueFQN, typeArgs: Seq[Value]): UsedNamesIO[Unit] =
    StateT.modify[CompilerIO, UsedNamesState] { state =>
      state.copy(visited = state.visited + ((vfqn, typeArgs)))
    }

  def recordUsage(vfqn: ValueFQN, typeArgs: Seq[Value], applicationCount: Int): UsedNamesIO[Unit] =
    StateT.modify[CompilerIO, UsedNamesState] { state =>
      val builder = state.usedNames.getOrElse(vfqn, UsageStatsBuilder())
      val updated = builder.addTypeParameters(typeArgs).addDirectCallApplication(applicationCount)
      state.copy(usedNames = state.usedNames.updated(vfqn, updated))
    }
}
