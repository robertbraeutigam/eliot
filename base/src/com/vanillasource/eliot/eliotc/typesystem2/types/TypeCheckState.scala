package com.vanillasource.eliot.eliotc.typesystem2.types

import cats.Monad
import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.typesystem.processor.ShortUniqueIdentifiers
import com.vanillasource.eliot.eliotc.typesystem2.types.NormalizedExpression.UnificationVar

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
  type TypeCheckIO[F[_], T] = StateT[F, TypeCheckState, T]

  def generateUnificationVar[F[_]: Monad](source: Sourced[?]): TypeCheckIO[F, UnificationVar] =
    StateT { state =>
      val (newState, uvar) = state.generateUnificationVar(source)
      (newState, uvar).pure[F]
    }

  def bindParameter[F[_]: Monad](name: String, typ: NormalizedExpression): TypeCheckIO[F, Unit] =
    StateT.modify(_.bindParameter(name, typ))

  def addUniversalVar[F[_]: Monad](name: String): TypeCheckIO[F, Unit] =
    StateT.modify(_.addUniversalVar(name))

  def lookupParameter[F[_]: Monad](name: String): TypeCheckIO[F, Option[NormalizedExpression]] =
    StateT.inspect(_.lookupParameter(name))

  def isUniversal[F[_]: Monad](name: String): TypeCheckIO[F, Boolean] =
    StateT.inspect(_.isUniversal(name))
}
