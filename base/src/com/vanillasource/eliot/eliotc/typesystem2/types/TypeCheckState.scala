package com.vanillasource.eliot.eliotc.typesystem2.types

import cats.data.StateT
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
) {

  /** Generate a fresh unification variable. */
  def generateUnificationVar(source: Sourced[?]): (TypeCheckState, UnificationVar) = {
    val id       = shortIds.generateCurrentIdentifier()
    val newState = copy(shortIds = shortIds.advanceIdentifierIndex())
    (newState, UnificationVar(id, source))
  }

  /** Bind a parameter name to its type. */
  def bindParameter(name: String, typ: NormalizedExpression): TypeCheckState =
    copy(parameterTypes = parameterTypes + (name -> typ))

  /** Add a universal variable. */
  def addUniversalVar(name: String): TypeCheckState =
    copy(universalVars = universalVars + name)

  /** Look up the type of a bound parameter. */
  def lookupParameter(name: String): Option[NormalizedExpression] =
    parameterTypes.get(name)

  /** Check if a name is a universal variable. */
  def isUniversal(name: String): Boolean =
    universalVars.contains(name)

  /** Create a new scope with fresh parameter bindings (for lambda bodies). */
  def withParameterScope(bindings: Map[String, NormalizedExpression]): TypeCheckState =
    copy(parameterTypes = parameterTypes ++ bindings)
}

object TypeCheckState {
  type TypeCheckIO[T] = StateT[CompilerIO, TypeCheckState, T]

  def generateUnificationVar(source: Sourced[?]): TypeCheckIO[UnificationVar] =
    StateT { state =>
      val (newState, uvar) = state.generateUnificationVar(source)
      (newState, uvar).pure[CompilerIO]
    }

  def bindParameter(name: String, typ: NormalizedExpression): TypeCheckIO[Unit] =
    StateT.modify(_.bindParameter(name, typ))

  def addUniversalVar(name: String): TypeCheckIO[Unit] =
    StateT.modify(_.addUniversalVar(name))

  def lookupParameter(name: String): TypeCheckIO[Option[NormalizedExpression]] =
    StateT.inspect(_.lookupParameter(name))

  def isUniversal(name: String): TypeCheckIO[Boolean] =
    StateT.inspect(_.isUniversal(name))
}
