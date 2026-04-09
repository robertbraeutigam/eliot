package com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.monomorphize2.typecheck.constraints.Constraints
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedExpression
import com.vanillasource.eliot.eliotc.pos.PositionRange
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import com.vanillasource.eliot.eliotc.source.content.Sourced

import java.net.URI

case class TypeCheckState(
    shortIds: ShortUniqueIdentifiers = ShortUniqueIdentifiers(),
    parameterTypes: Map[String, Sourced[OperatorResolvedExpression]] = Map.empty,
    nodeAssumedTypes: Map[TypeCheckState.NodeId, OperatorResolvedExpression] = Map.empty,
    constraints: Constraints = Constraints.empty
)

object TypeCheckState {

  /** Identity of an ORE node by source position. Each AST node has a unique source range, so this
    * is a stable key for recording per-node information across the extract → solve → substitute
    * pipeline.
    */
  type NodeId = (URI, PositionRange)

  def nodeIdOf(s: Sourced[?]): NodeId = (s.uri, s.range)

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

  /** Record the assumed (expected-from-parent) type expression at a given ORE node. The processor
    * walk reads from this map by node identity to discover each node's expected type, instead of
    * replaying the extractor's fresh-variable generator.
    */
  def recordAssumedType(
      node: Sourced[OperatorResolvedExpression],
      assumedType: OperatorResolvedExpression
  ): TypeGraphIO[Unit] =
    StateT.modify(state =>
      state.copy(nodeAssumedTypes = state.nodeAssumedTypes + (nodeIdOf(node) -> assumedType))
    )

  def tellConstraint(constraint: Constraints): TypeGraphIO[Unit] =
    StateT.modify(state => state.copy(constraints = state.constraints |+| constraint))

}
