package com.vanillasource.eliot.eliotc.symbolic.types

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.typesystem.processor.ShortUniqueIdentifiers

/** Combined state for type checking, including constraint accumulation. */
case class TypeCheckState(
    shortIds: ShortUniqueIdentifiers = ShortUniqueIdentifiers(),
    parameterTypes: Map[String, ExpressionValue] = Map.empty,
    universalVars: Set[String] = Set.empty,
    unificationVars: Set[String] = Set.empty,
    constraints: SymbolicUnification = SymbolicUnification.empty
)

object TypeCheckState {
  type TypeGraphIO[T] = StateT[CompilerIO, TypeCheckState, T]

  def generateUnificationVar(source: Sourced[?]): TypeGraphIO[ParameterReference] =
    StateT { state =>
      val id       = state.shortIds.generateCurrentIdentifier()
      val newState = state.copy(
        shortIds = state.shortIds.advanceIdentifierIndex(),
        unificationVars = state.unificationVars + id
      )
      (newState, ParameterReference(id, Value.TypeType)).pure[CompilerIO]
    }

  def bindParameter(name: String, typ: ExpressionValue): TypeGraphIO[Unit] =
    StateT.modify(state => state.copy(parameterTypes = state.parameterTypes + (name -> typ)))

  def addUniversalVar(name: String): TypeGraphIO[Unit] =
    StateT.modify(state => state.copy(universalVars = state.universalVars + name))

  def lookupParameter(name: String): TypeGraphIO[Option[ExpressionValue]] =
    StateT.inspect(_.parameterTypes.get(name))

  def tellConstraint(constraint: SymbolicUnification): TypeGraphIO[Unit] =
    StateT.modify(state => state.copy(constraints = state.constraints |+| constraint))

  def getConstraints: TypeGraphIO[SymbolicUnification] =
    StateT.inspect(_.constraints)

  def getUniversalVars: TypeGraphIO[Set[String]] =
    StateT.inspect(_.universalVars)

  def getUnificationVars: TypeGraphIO[Set[String]] =
    StateT.inspect(_.unificationVars)

  def isUniversalVar(name: String): TypeGraphIO[Boolean] =
    StateT.inspect(_.universalVars.contains(name))
}
