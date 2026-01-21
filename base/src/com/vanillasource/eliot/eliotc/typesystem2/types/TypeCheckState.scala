package com.vanillasource.eliot.eliotc.typesystem2.types

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.typesystem.processor.ShortUniqueIdentifiers
import com.vanillasource.eliot.eliotc.typesystem2.fact.{NormalizedExpression, TypedExpression}
import com.vanillasource.eliot.eliotc.typesystem2.fact.NormalizedExpression.UnificationVar

/** Combined state for type checking, including constraint accumulation. */
case class TypeCheckState(
    shortIds: ShortUniqueIdentifiers = ShortUniqueIdentifiers(),
    parameterTypes: Map[String, NormalizedExpression] = Map.empty,
    universalVars: Set[String] = Set.empty,
    constraints: SymbolicUnification = SymbolicUnification.empty
)

object TypeCheckState {
  type TypeGraphIO[T] = StateT[CompilerIO, TypeCheckState, T]

  def generateUnificationVar(source: Sourced[?]): TypeGraphIO[UnificationVar] =
    StateT { state =>
      val id       = state.shortIds.generateCurrentIdentifier()
      val newState = state.copy(shortIds = state.shortIds.advanceIdentifierIndex())
      (newState, UnificationVar(id, source)).pure[CompilerIO]
    }

  def bindParameter(name: String, typ: NormalizedExpression): TypeGraphIO[Unit] =
    StateT.modify(state => state.copy(parameterTypes = state.parameterTypes + (name -> typ)))

  def addUniversalVar(name: String): TypeGraphIO[Unit] =
    StateT.modify(state => state.copy(universalVars = state.universalVars + name))

  def lookupParameter(name: String): TypeGraphIO[Option[NormalizedExpression]] =
    StateT.inspect(_.parameterTypes.get(name))

  def tellConstraint(constraint: SymbolicUnification): TypeGraphIO[Unit] =
    StateT.modify(state => state.copy(constraints = state.constraints |+| constraint))

  def tellUniversalVar(name: String): TypeGraphIO[Unit] =
    tellConstraint(SymbolicUnification.universalVar(name))

  def getConstraints: TypeGraphIO[SymbolicUnification] =
    StateT.inspect(_.constraints)

  def isUniversalVar(name: String): TypeGraphIO[Boolean] =
    StateT.inspect(_.universalVars.contains(name))

  def resolveParameterRef(name: Sourced[String]): TypeGraphIO[TypeWithTyped] =
    lookupParameter(name.value).map { maybeType =>
      val normalized = maybeType.getOrElse(NormalizedExpression.ParameterRef(name))
      TypeWithTyped(normalized, TypedExpression(normalized, TypedExpression.ParameterReference(name)))
    }
}
