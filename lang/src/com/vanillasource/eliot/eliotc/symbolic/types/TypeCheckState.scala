package com.vanillasource.eliot.eliotc.symbolic.types

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue.*
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO

/** Combined state for type checking, including constraint accumulation.
  *
  * @param remainingExplicitTypeArgs
  *   Counts how many explicit type arguments remain unconsumed during instantiation. Set by BodyTypeInferrer before
  *   calling TypeExpressionEvaluator.processStackForInstantiation, decremented each time a universal intro consumes an
  *   explicit arg. After instantiation, BodyTypeInferrer checks this to detect too-many-args errors.
  */
case class TypeCheckState(
    shortIds: ShortUniqueIdentifiers = ShortUniqueIdentifiers(),
    parameterTypes: Map[String, ExpressionValue] = Map.empty,
    universalVars: Set[String] = Set.empty,
    unificationVars: Set[String] = Set.empty,
    constraints: SymbolicUnification = SymbolicUnification.empty,
    remainingExplicitTypeArgs: Int = 0
)

object TypeCheckState {
  type TypeGraphIO[T] = StateT[CompilerIO, TypeCheckState, T]

  def generateUnificationVar: TypeGraphIO[ParameterReference] =
    StateT { state =>
      val (id, newShortIds) = state.shortIds.generateNext()
      val newState          = state.copy(shortIds = newShortIds, unificationVars = state.unificationVars + id)
      (newState, ParameterReference(id, Value.Type)).pure[CompilerIO]
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

  /** Set the explicit type arg counter before processing an instantiation. */
  def setExplicitTypeArgCount(n: Int): TypeGraphIO[Unit] =
    StateT.modify(_.copy(remainingExplicitTypeArgs = n))

  /** Consume one explicit type arg from the counter. Called by buildUniversalIntro when an explicit arg is used. */
  def decrementExplicitTypeArgCount: TypeGraphIO[Unit] =
    StateT.modify(s => s.copy(remainingExplicitTypeArgs = s.remainingExplicitTypeArgs - 1))

  /** Read how many explicit type args are still unconsumed after instantiation. */
  def getExplicitTypeArgCount: TypeGraphIO[Int] =
    StateT.inspect(_.remainingExplicitTypeArgs)
}
