package com.vanillasource.eliot.eliotc.core.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.{
  ArgumentDefinition,
  FunctionDefinition,
  Expression as SourceExpression
}
import com.vanillasource.eliot.eliotc.core.fact.RoleHint
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}

/** Desugars a type declaration's meta-slot brace into the type's `^Meta` constructor (bounds-as-refinements §4.2, Step
  * 4a). Works entirely in the source AST domain, mirroring [[DataDefinitionDesugarer]] but for the meta channel.
  *
  * `type Int {range: Interval[BigInteger, BigInteger]}` desugars to a value in the [[Qualifier.Meta]] namespace:
  * {{{
  *   Int^Meta(range: Interval[BigInteger, BigInteger]): Interval[BigInteger, BigInteger] = range
  * }}}
  *
  * For a **single-slot** type the meta value simply *is* the slot's domain value (an `Int`'s meta is its `Interval` —
  * exactly how `RefinementChannelProcessor` already represents it), so the constructor is the identity on the slot and
  * needs no generated record type. This is the foundational, inert piece: nothing consumes `^Meta` yet (Steps 4b/4c
  * wire the channel to it). Multi-slot types — whose meta value is a record of the slot domains — are not yet emitted
  * here; the only slotted types today (`Int`, the toy test type) are single-slot.
  */
object MetaConstructorDesugarer {

  /** The meta constructor(s) generated from `definition`'s meta-slot brace, or empty when it declares no slots (every
    * ordinary `def`/alias) or has more than one slot (multi-slot records are a later generalization).
    */
  def desugar(definition: FunctionDefinition): Seq[(FunctionDefinition, RoleHint)] =
    definition.metaSlots match {
      case Seq(slot) => Seq(createSingleSlotConstructor(definition, slot))
      case _         => Seq.empty
    }

  private def createSingleSlotConstructor(
      definition: FunctionDefinition,
      slot: ArgumentDefinition
  ): (FunctionDefinition, RoleHint) = {
    val metaConstructor = FunctionDefinition(
      definition.name.map(qn => QualifiedName(qn.name, Qualifier.Meta)),
      Seq.empty,
      Seq(slot),
      slot.typeExpression,
      Some(slot.name.as(SourceExpression.FunctionApplication(None, slot.name, None, Seq.empty))),
      visibility = definition.visibility
    )
    (metaConstructor, RoleHint.NoHint)
  }
}
