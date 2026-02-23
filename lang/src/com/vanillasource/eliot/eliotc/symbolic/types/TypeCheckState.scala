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
  * @param instantiationMode
  *   When true, type parameters in referenced values become fresh unification vars instead of universal vars. This
  *   allows type inference at call sites.
  * @param pendingTypeArgs
  *   Explicit type arguments to consume (in order) when instantiating type parameters during instantiation mode.
  *   Consumed one-by-one by `buildUniversalIntro`; any remaining after processing signals too many type args.
  */
case class TypeCheckState(
    shortIds: ShortUniqueIdentifiers = ShortUniqueIdentifiers(),
    parameterTypes: Map[String, ExpressionValue] = Map.empty,
    universalVars: Set[String] = Set.empty,
    unificationVars: Set[String] = Set.empty,
    constraints: SymbolicUnification = SymbolicUnification.empty,
    instantiationMode: Boolean = false,
    pendingTypeArgs: List[ExpressionValue] = List.empty
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

  def withInstantiationMode[T](computation: TypeGraphIO[T]): TypeGraphIO[T] =
    for {
      original <- StateT.inspect[CompilerIO, TypeCheckState, Boolean](_.instantiationMode)
      _        <- StateT.modify[CompilerIO, TypeCheckState](_.copy(instantiationMode = true))
      result   <- computation
      _        <- StateT.modify[CompilerIO, TypeCheckState](_.copy(instantiationMode = original))
    } yield result

  def isInstantiationMode: TypeGraphIO[Boolean] =
    StateT.inspect(_.instantiationMode)

  /** Run a computation with the given explicit type arguments pending for consumption by `buildUniversalIntro`. Restores
    * previous pending args on exit.
    */
  def withPendingTypeArgs[T](args: Seq[ExpressionValue])(computation: TypeGraphIO[T]): TypeGraphIO[T] =
    for {
      original <- StateT.inspect[CompilerIO, TypeCheckState, List[ExpressionValue]](_.pendingTypeArgs)
      _        <- StateT.modify[CompilerIO, TypeCheckState](_.copy(pendingTypeArgs = args.toList))
      result   <- computation
      _        <- StateT.modify[CompilerIO, TypeCheckState](_.copy(pendingTypeArgs = original))
    } yield result

  /** Consume the next pending type argument, returning None if none remain. */
  def consumeNextPendingTypeArg: TypeGraphIO[Option[ExpressionValue]] =
    StateT { state =>
      state.pendingTypeArgs match {
        case head :: tail => (state.copy(pendingTypeArgs = tail), Some(head)).pure[CompilerIO]
        case Nil          => (state, None).pure[CompilerIO]
      }
    }

  def getRemainingPendingTypeArgCount: TypeGraphIO[Int] =
    StateT.inspect(_.pendingTypeArgs.length)
}
