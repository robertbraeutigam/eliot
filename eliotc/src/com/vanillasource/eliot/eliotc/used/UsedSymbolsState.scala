package com.vanillasource.eliot.eliotc.used

import cats.data.StateT
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, TypeFQN}
import com.vanillasource.eliot.eliotc.source.pos.Sourced

case class UsedSymbolsState(
    usedFunctions: Map[FunctionFQN, Sourced[_]] = Map.empty,
    usedTypes: Map[TypeFQN, Sourced[_]] = Map.empty
)

object UsedSymbolsState {
  type UsedSymbolsIO[T] = StateT[IO, UsedSymbolsState, T]

  extension [T](io: IO[T]) {
    def liftToUsedSymbols: UsedSymbolsIO[T] = StateT.liftF(io)
  }

  def getUsedSymbols(ffqn: FunctionFQN, state: UsedSymbolsState): UsedSymbols =
    UsedSymbols(
      ffqn,
      usedFunctions = state.usedFunctions.map(t => t._2.as(t._1)).toSeq,
      usedTypes = state.usedTypes.map(t => t._2.as(t._1)).toSeq
    )

  def isFunctionUsed(ffqn: FunctionFQN): UsedSymbolsIO[Boolean] =
    for {
      state <- StateT.get[IO, UsedSymbolsState]
    } yield state.usedFunctions.contains(ffqn)

  def isTypeUsed(tfqn: TypeFQN): UsedSymbolsIO[Boolean] =
    for {
      state <- StateT.get[IO, UsedSymbolsState]
    } yield state.usedTypes.contains(tfqn)

  def addFunctionUsed(ffqn: Sourced[FunctionFQN]): UsedSymbolsIO[Unit] =
    StateT.modify[IO, UsedSymbolsState] { state =>
      state.copy(usedFunctions = state.usedFunctions + ((ffqn.value, ffqn)))
    }

  def addTypeUsed(tfqn: Sourced[TypeFQN]): UsedSymbolsIO[Unit] =
    StateT.modify[IO, UsedSymbolsState] { state =>
      state.copy(usedTypes = state.usedTypes + ((tfqn.value, tfqn)))
    }
}
