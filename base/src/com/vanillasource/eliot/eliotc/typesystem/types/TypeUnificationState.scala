package com.vanillasource.eliot.eliotc.typesystem.types

import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference
import TypeUnificationState.UnifiedType
import cats.Show
import cats.data.State
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.*

case class TypeUnificationState(states: Map[String, UnifiedType] = Map.empty) {
  def getCurrentType(typeReference: TypeReference): TypeReference =
    unifiedTypeOf(typeReference).current

  def unifyTo(
      target: TypeReference,
      source: TypeReference,
      unifiedTypeReference: TypeReference
  ): TypeUnificationState = {
    val targetUnifiedType = unifiedTypeOf(target)
    val sourceUnifiedType = unifiedTypeOf(source)

    val unifiedType = UnifiedType(unifiedTypeReference, targetUnifiedType.memberNames ++ sourceUnifiedType.memberNames)

    TypeUnificationState(states ++ unifiedType.memberNames.map(_ -> unifiedType).toMap)
  }

  private def unifiedTypeOf(typeReference: TypeReference): UnifiedType =
    states.getOrElse(typeReference.identifier, UnifiedType(typeReference, Set(typeReference.identifier)))
}

object TypeUnificationState {
  case class UnifiedType(current: TypeReference, memberNames: Set[String])

  given Show[TypeUnificationState] {
    override def show(t: TypeUnificationState): String =
      t.states.map((name, unifiedType) => s"$name -> $unifiedType").mkString(", ")
  }
}
