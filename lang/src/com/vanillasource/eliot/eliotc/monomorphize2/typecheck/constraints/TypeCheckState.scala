package com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.ParameterReference
import com.vanillasource.eliot.eliotc.eval.fact.{ExpressionValue, Value}
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints.Constraints
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.source.content.Sourced

import java.net.URI

case class TypeCheckState(
    shortIds: ShortUniqueIdentifiers = ShortUniqueIdentifiers(),
    parameterTypes: Map[String, Sourced[ExpressionValue]] = Map.empty,
    constraints: Constraints = Constraints.empty,
    typeArgSubstitution: Map[String, Value] = Map.empty,
    valueRefTypes: Map[(URI, PositionRange), ExpressionValue] = Map.empty
)

object TypeCheckState {
  type TypeGraphIO[T] = StateT[CompilerIO, TypeCheckState, T]

  def generateUnificationVar: TypeGraphIO[ParameterReference] =
    StateT { state =>
      val (id, newShortIds) = state.shortIds.generateNext()
      (state.copy(shortIds = newShortIds), ParameterReference(id, Value.Type)).pure[CompilerIO]
    }

  def bindParameter(name: String, typ: Sourced[ExpressionValue]): TypeGraphIO[Unit] =
    StateT.modify(state => state.copy(parameterTypes = state.parameterTypes + (name -> typ)))

  def lookupParameter(name: String): TypeGraphIO[Option[Sourced[ExpressionValue]]] =
    StateT.inspect(_.parameterTypes.get(name))

  def tellConstraint(constraint: Constraints): TypeGraphIO[Unit] =
    StateT.modify(state => state.copy(constraints = state.constraints |+| constraint))

  def recordValueRefType(source: Sourced[?], exprType: ExpressionValue): TypeGraphIO[Unit] =
    StateT.modify(state => state.copy(valueRefTypes = state.valueRefTypes + ((source.uri, source.range) -> exprType)))
}
