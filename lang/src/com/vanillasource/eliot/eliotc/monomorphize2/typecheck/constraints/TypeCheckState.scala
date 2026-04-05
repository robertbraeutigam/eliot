package com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints.Constraints
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.source.content.Sourced

case class TypeCheckState(
    shortIds: ShortUniqueIdentifiers = ShortUniqueIdentifiers(),
    parameterTypes: Map[String, Sourced[OperatorResolvedExpression]] = Map.empty,
    constraints: Constraints = Constraints.empty
)

object TypeCheckState {
  type TypeGraphIO[T] = StateT[CompilerIO, TypeCheckState, T]

  def generateUnificationVar: TypeGraphIO[String] =
    StateT { state =>
      val (id, newShortIds) = state.shortIds.generateNext()
      (state.copy(shortIds = newShortIds), id).pure[CompilerIO]
    }

  def bindParameter(name: String, typ: Sourced[OperatorResolvedExpression]): TypeGraphIO[Unit] =
    StateT.modify(state => state.copy(parameterTypes = state.parameterTypes + (name -> typ)))

  def lookupParameter(name: String): TypeGraphIO[Option[Sourced[OperatorResolvedExpression]]] =
    StateT.inspect(_.parameterTypes.get(name))

  def tellConstraint(constraint: Constraints): TypeGraphIO[Unit] =
    StateT.modify(state => state.copy(constraints = state.constraints |+| constraint))

}
