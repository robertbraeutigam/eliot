package com.vanillasource.eliot.eliotc.typesystem

import com.vanillasource.eliot.eliotc.resolve.TypeReference
import com.vanillasource.eliot.eliotc.typesystem.TypeUnificationState.UnifiedType

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
    states.getOrElse(typeReference.name, UnifiedType(typeReference, Set(typeReference.name)))
}

object TypeUnificationState {
  case class UnifiedType(current: TypeReference, memberNames: Set[String])
}
