package com.vanillasource.eliot.eliotc.core.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.{
  ArgumentDefinition,
  DataConstructor,
  DataDefinition,
  FunctionDefinition,
  Expression as SourceExpression
}
import com.vanillasource.eliot.eliotc.core.fact.RoleHint
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.source.content.Sourced

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

  /** The `data`-style meta structure generated from `definition`'s meta-slot brace **plus** the auto-derived
    * `Meta[<Name>$Meta]` instance, or empty when it declares no slots (every ordinary `def`/alias).
    */
  def desugar(definition: FunctionDefinition): Seq[(FunctionDefinition, RoleHint)] =
    if (definition.metaSlots.isEmpty) Seq.empty
    else DataDefinitionDesugarer.desugar(metaStructure(definition)) ++ metaJoinInstance(definition)

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

  /** The auto-derived `Meta[<Name>$Meta]` **instance** — the lattice join over the type's meta structure (Step 1 of
    * `docs/generic-refinement-merges.md`). A *user* declares `Meta` only for their own domain (`Meta[Interval[T]]`); the
    * compiler derives the matching instance for the compound *meta structure* `Int$Meta` field-wise, so `fold`'s generic
    * `^Meta` companion (`join(whenTrue, whenFalse)` over `metaOf(A)`) has a `Meta[Int$Meta]` to reduce through:
    *
    * {{{
    *   implement Meta[Int$Meta] {
    *     def join(a: Int$Meta, b: Int$Meta): Int$Meta = Int$Meta(intervalJoin(range(a), range(b)))
    *   }
    * }}}
    *
    * Each slot is joined by its domain's **plain join function** (`Interval` ⤳ `intervalJoin`, decapitalized head +
    * `Join`) — not the `Meta[Interval]` *instance*, because a transitively-reached Eliot-body instance does not dispatch
    * under the channel's NbE (the same Step-4c reason the arithmetic vessels bottom at `intervalAdd`), so the compound
    * join must bottom out at a plain function. The result is rewrapped in the meta *constructor* `Int$Meta(..)`.
    *
    * The marker/method shapes mirror [[DataDefinitionDesugarer.createPatternMatchImpl]] and the surface `implement`
    * desugar ([[com.vanillasource.eliot.eliotc.ast.fact.ImplementBlock]]): a body-less marker whose sole arg is the
    * pattern type and whose return slot is the (unguarded) `true`, keyed by `Qualifier.AbilityImplementation("Meta",
    * "<Name>$Meta")` — the meta structure is nullary, so its pattern key is just its name. A slot whose domain head is
    * not a simple type application yields no instance (fail-safe: the channel then hard-errors "No ability
    * implementation found", never accepts a wrong join); the only slotted type today is `Int`'s clean `Interval` slot.
    */
  private def metaJoinInstance(definition: FunctionDefinition): Seq[(FunctionDefinition, RoleHint)] = {
    val metaTypeName = definition.name.map(_.name + metaTypeSuffix)
    val metaTypeRef  = app(metaTypeName)
    val implQualifier = Qualifier.AbilityImplementation("Meta", metaTypeName.value)
    val perSlotJoins  = definition.metaSlots.traverse(slotJoin)
    perSlotJoins.toSeq.flatMap { joins =>
      val marker = FunctionDefinition(
        metaTypeName.as(QualifiedName("Meta", implQualifier)),
        Seq.empty,
        Seq(ArgumentDefinition(metaTypeName.as("arg"), metaTypeRef)),
        SourceExpression.trueReference(metaTypeName),
        None,
        visibility = definition.visibility
      )
      val join   = FunctionDefinition(
        metaTypeName.as(QualifiedName("join", implQualifier)),
        Seq.empty,
        Seq(ArgumentDefinition(metaTypeName.as("a"), metaTypeRef), ArgumentDefinition(metaTypeName.as("b"), metaTypeRef)),
        metaTypeRef,
        Some(app(metaTypeName, joins)),
        visibility = definition.visibility
      )
      Seq(marker -> RoleHint.NoHint, join -> RoleHint.NoHint)
    }
  }

  /** The per-slot join expression `<domainJoin>(<slot>(a), <slot>(b))` — e.g. `intervalJoin(range(a), range(b))` — or
    * `None` when the slot's domain head is not a simple type application (see [[metaJoinInstance]]).
    */
  private def slotJoin(slot: ArgumentDefinition): Option[Sourced[SourceExpression]] =
    slot.typeExpression.value match {
      case SourceExpression.FunctionApplication(_, domainHead, _, _) =>
        val joinFn = domainHead.map(name => name.take(1).toLowerCase + name.drop(1) + "Join")
        Some(app(joinFn, Seq(project(slot.name, "a"), project(slot.name, "b"))))
      case _                                                         => None
    }

  /** `<slotName>(<binder>)` — the slot accessor applied to a join parameter (`range(a)`). */
  private def project(slotName: Sourced[String], binder: String): Sourced[SourceExpression] =
    app(slotName, Seq(app(slotName.as(binder))))

  /** A bare-name function application `name(args)` — resolves as the type constructor in a type position and the value
    * constructor / function in a value position, exactly as [[DataDefinitionDesugarer]]'s generated references do.
    */
  private def app(name: Sourced[String], args: Seq[Sourced[SourceExpression]] = Seq.empty): Sourced[SourceExpression] =
    name.as(SourceExpression.FunctionApplication(None, name, None, args))

  /** Appended to a type's name to form its meta structure's name. `$` is not an identifier character, so the result
    * cannot collide with any user-declared type.
    */
  val metaTypeSuffix: String = "$Meta"
}
