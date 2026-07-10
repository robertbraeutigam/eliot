package com.vanillasource.eliot.eliotc.core.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.{DataConstructor, DataDefinition, FunctionDefinition}
import com.vanillasource.eliot.eliotc.core.fact.RoleHint

/** Desugars a type declaration's meta-slot brace into the type's **meta structure** — a real one-constructor `data`
  * type carrying the slots as fields (bounds-as-refinements §4.2, Step 4a).
  *
  * `type Int {range: Interval[BigInteger]}` desugars exactly as if the user had written
  * `data Int$Meta(range: Interval[BigInteger])` — a type constructor, a value constructor, a per-slot accessor, and a
  * `PatternMatch` implementation the accessor reduces through. So the meta value of an `Int` is an `Int$Meta` structure
  * whose `range` field is an `Interval`, and the meta-value machinery is the ordinary `data` machinery.
  *
  * The `$Meta` suffix keeps the meta type's name distinct from the ordinary `Int^Type` without a new namespace: `$` is
  * not a valid identifier character (the lexer's `isLetter`/`isLetterOrDigit`), so `Int$Meta` can never collide with a
  * user type. This is what makes the transfer companion (Step 4b) generatable with **no lookup** — a value of type `T`
  * has meta type `T$Meta`, a pure name transform. The `^Meta` *transfer* companions themselves live in
  * [[com.vanillasource.eliot.eliotc.module.fact.Qualifier.Meta]]; the meta *structure* is an ordinary `data` type so
  * every reference to it resolves by the normal `Type`/`Default` rules. These structures are compiler-pool-only (the
  * channel evaluates them) — dead in the runtime pool, never code-generated.
  *
  * Reuses [[DataDefinitionDesugarer]] verbatim (per "reuse, don't write parallel generators"), so multi-slot types are
  * multi-field structures for free. Type generic parameters are not yet threaded onto the meta structure — the only
  * slotted types today have concrete-domain single slots.
  */
object MetaConstructorDesugarer {

  /** The `data`-style meta structure generated from `definition`'s meta-slot brace, or empty when it declares no slots
    * (every ordinary `def`/alias).
    */
  def desugar(definition: FunctionDefinition): Seq[(FunctionDefinition, RoleHint)] =
    if (definition.metaSlots.isEmpty) Seq.empty
    else DataDefinitionDesugarer.desugar(metaStructure(definition))

  /** The synthetic `data <Name>$Meta(<slots>)` whose desugaring *is* the meta structure. */
  private def metaStructure(definition: FunctionDefinition): DataDefinition = {
    val metaTypeName = definition.name.map(_.name + metaTypeSuffix)
    DataDefinition(
      metaTypeName,
      Seq.empty,
      Some(Seq(DataConstructor(metaTypeName, definition.metaSlots))),
      visibility = definition.visibility
    )
  }

  /** Appended to a type's name to form its meta structure's name. `$` is not an identifier character, so the result
    * cannot collide with any user-declared type.
    */
  val metaTypeSuffix: String = "$Meta"
}
