package com.vanillasource.eliot.eliotc.symbolic.types

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Combined state for type checking, including constraint accumulation.
  *
  * @param remainingExplicitTypeArgs
  *   Counts how many explicit type arguments remain unconsumed during instantiation. Set by SymbolicEvaluator before
  *   calling processStackForInstantiation, decremented each time a universal intro consumes an explicit arg. After
  *   instantiation, SymbolicEvaluator checks this to detect too-many-args errors.
  */
case class TypeCheckState(
    shortIds: ShortUniqueIdentifiers = ShortUniqueIdentifiers(),
    parameterTypes: Map[String, Sourced[ExpressionValue]] = Map.empty,
    universalVars: Set[String] = Set.empty,
    constraints: SymbolicUnification = SymbolicUnification.empty
)

object TypeCheckState {
  type TypeGraphIO[T] = StateT[CompilerIO, TypeCheckState, T]

  def generateUnificationVar: TypeGraphIO[ParameterReference] =
    StateT { state =>
      val (id, newShortIds) = state.shortIds.generateNext()
      val newState          = state.copy(shortIds = newShortIds)
      (newState, ParameterReference(id, Value.Type)).pure[CompilerIO]
    }

  def bindParameter(name: String, typ: Sourced[ExpressionValue]): TypeGraphIO[Unit] =
    StateT.modify(state => state.copy(parameterTypes = state.parameterTypes + (name -> typ)))

  def addUniversalVar(name: String): TypeGraphIO[Unit] =
    StateT.modify(state => state.copy(universalVars = state.universalVars + name))

  def lookupParameter(name: String): TypeGraphIO[Option[Sourced[ExpressionValue]]] =
    StateT.inspect(_.parameterTypes.get(name))

  def tellConstraint(constraint: SymbolicUnification): TypeGraphIO[Unit] =
    StateT.modify(state => state.copy(constraints = state.constraints |+| constraint))
}
