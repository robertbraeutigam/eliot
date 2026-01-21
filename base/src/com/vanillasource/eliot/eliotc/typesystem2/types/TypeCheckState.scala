package com.vanillasource.eliot.eliotc.typesystem2.types

import cats.data.{StateT, WriterT}
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.typesystem.processor.ShortUniqueIdentifiers
import com.vanillasource.eliot.eliotc.typesystem2.fact.NormalizedExpression
import com.vanillasource.eliot.eliotc.typesystem2.fact.NormalizedExpression.UnificationVar

/** State maintained during type checking. Tracks:
  *   - Fresh unification variable generation
  *   - Bound parameter types
  *   - Universal variable names
  */
case class TypeCheckState(
    shortIds: ShortUniqueIdentifiers = ShortUniqueIdentifiers(),
    parameterTypes: Map[String, NormalizedExpression] = Map.empty,
    universalVars: Set[String] = Set.empty
)

object TypeCheckState {
  type TypeCheckIO[T] = StateT[CompilerIO, TypeCheckState, T]
  type TypeGraphIO[T] = WriterT[TypeCheckIO, SymbolicUnification, T]

  def generateUnificationVar(source: Sourced[?]): TypeCheckIO[UnificationVar] =
    StateT { state =>
      val id       = state.shortIds.generateCurrentIdentifier()
      val newState = state.copy(shortIds = state.shortIds.advanceIdentifierIndex())
      (newState, UnificationVar(id, source)).pure[CompilerIO]
    }

  def bindParameter(name: String, typ: NormalizedExpression): TypeCheckIO[Unit] =
    StateT.modify(state => state.copy(parameterTypes = state.parameterTypes + (name -> typ)))

  def addUniversalVar(name: String): TypeCheckIO[Unit] =
    StateT.modify(state => state.copy(universalVars = state.universalVars + name))

  def lookupParameter(name: String): TypeCheckIO[Option[NormalizedExpression]] =
    StateT.inspect(_.parameterTypes.get(name))

  def isUniversal(name: String): TypeCheckIO[Boolean] =
    StateT.inspect(_.universalVars.contains(name))

  def tellConstraint(constraint: SymbolicUnification): TypeGraphIO[Unit] =
    WriterT.tell(constraint)

  def tellUniversalVar(name: String): TypeGraphIO[Unit] =
    tellConstraint(SymbolicUnification.universalVar(name))

  def liftState[T](stateIO: TypeCheckIO[T]): TypeGraphIO[T] =
    WriterT.liftF(stateIO)
}
