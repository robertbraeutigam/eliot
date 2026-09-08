package com.vanillasource.eliot.eliotc.monomorphize.fact

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.effect.EffectRowRendering
import com.vanillasource.eliot.eliotc.module.fact.{Qualifier, WellKnownTypes}

/** Renders a [[GroundValue]] — a fully evaluated, concrete monomorphic type — as a compact, human-readable type string
  * for anything a user reads: LSP hover / type hints, and the type arguments of an ability-demand diagnostic.
  *
  * The compiler's own `GroundValue.debugString` is deliberately terse (it collapses every non-`Type` structure to
  * `"Structure(...)"` and drops type arguments), which is fine for debug logging but useless — and, for a carrier,
  * actively misleading — in front of a user. This renderer instead shows the structure a reader expects:
  *
  *   - a data/type-constructor application as `Name[arg, …]` (`Int[0, 255]`, `IO[Unit]`), or just `Name` with no args;
  *   - a function type as a right-associative arrow (`String -> IO[Unit]`), parenthesising a function on the left of an
  *     arrow so `(A -> B) -> C` stays unambiguous;
  *   - a direct (literal / bound) value by its underlying `toString` (e.g. the `0` / `255` bounds of `Int[0, 255]`);
  *   - a canonical effect-carrier stack as its **pinned effect row** — the surface syntax that spells it —
  *     `ThrowCarrier[E, StateCarrier[S, Id], A]` rendering as `{Throw[E], State[S] | Id} A`, so no carrier machinery
  *     name is ever shown ([[EffectRowRendering]]);
  *   - the identity carrier's **payload wrapper** `Id[X]` as plain `X`.
  *
  * '''Two entry points, because the value cannot tell you which it is.''' A carrier's last argument is its payload in
  * `ThrowCarrier[E, G, A]` but its *base* in the payload-unapplied `ThrowCarrier[E, G]` (the shape a carrier takes in
  * an `F[_]` slot), and those two are structurally indistinguishable without the ability's argument count: `Abort`
  * takes none, `Throw` takes one, so a two-argument application is a full `AbortCarrier[G, A]` *or* a partial
  * `ThrowCarrier[E, G]`. `GroundValue.valueType` does **not** help — it is `Type` for every structure, including the
  * nullary type constructor `IO`. So the caller states its context: [[render]] for a **type**, [[renderConstructor]]
  * for a **type constructor** (an ability's `F[_]` argument). Getting this wrong is how `ThrowCarrier[String,
  * StateCarrier[String, IO]]` once printed as `{Throw | String} {State | String} IO` instead of `{Throw[String],
  * State[String] | IO}` — a confidently wrong reading of the same characters.
  *
  * '''Two deliberate decisions about `Id`''' (docs/effects-as-channel.md §9 — `Id` and carriers are never rendered to
  * users):
  *   - `Id[X]` is **erased** to `X`. It is pure machinery the checker inserts at a pure boundary and the
  *     Id-normalization stage erases downstream; a consumer reading a pre-erasure `MonomorphicValue` would otherwise
  *     show it. (Erasing here is a rendering fallback — a consumer holding pre-erasure facts should still Id-normalize
  *     its input, so the machinery *nodes* disappear too, not just their names.)
  *   - an `Id` **row base is kept**: `{Throw[E] | Id} A` is exactly the legal surface a user writes for a stack pinned
  *     to the pure base, and it is *not* the same type as the open row `{Throw[E]} A` (whose carrier the caller
  *     chooses). Suppressing the base would render two different types identically.
  */
object GroundValueRenderer {

  /** Render a ground **type** as a one-line string: a carrier application here is applied to its payload, so
    * `ThrowCarrier[E, G, A]` reads `{Throw[E] | G} A`.
    */
  def render(value: GroundValue): String = go(value, appliedToPayload = true)

  /** Render a ground **type constructor** — a value occupying an `F[_]` slot, such as an ability's carrier argument. A
    * carrier application here is *not* applied to a payload, so `ThrowCarrier[E, G]` reads `{Throw[E] | G}` and its
    * last argument is correctly read as the base rather than as a payload.
    */
  def renderConstructor(value: GroundValue): String = go(value, appliedToPayload = false)

  private def go(value: GroundValue, appliedToPayload: Boolean): String = value match {
    case GroundValue.Type                                                              => "Type"
    case GroundValue.Direct(direct, _)                                                 => direct.toString
    // `Id[X] ⤳ X`: the identity carrier is a newtype over its payload and pure machinery — never shown. Matched with a
    // trailing wildcard for the same reason `GroundValue.carrierFQN` does: only the payload slot is load-bearing.
    case GroundValue.Structure(name, payload +: _, _) if name === WellKnownTypes.idFQN => render(payload)
    case structure: GroundValue.Structure                                              =>
      structure.asFunctionType match {
        case Some((from, to)) => s"${renderOperand(from)} -> ${render(to)}"
        case None             => pinnedRow(structure, appliedToPayload).getOrElse(application(structure))
      }
    case GroundValue.Param(index, Nil, _)                                              => s"?p$index"
    case GroundValue.Param(index, args, _)                                             =>
      s"?p$index[${args.map(render).mkString(", ")}]"
  }

  /** A function type used as the left operand of an arrow is parenthesised so the arrow nesting reads unambiguously. */
  def renderOperand(value: GroundValue): String =
    value.asFunctionType match {
      case Some(_) => s"(${render(value)})"
      case None    => render(value)
    }

  /** A plain application, bracketed by the head's qualifier so it reads the way the user writes it: `[]` for a type
    * constructor ([[Qualifier.Type]], e.g. `Box[String]` the type), `()` for a value constructor (e.g. `Box("a")` the
    * value). [[SemValuePrinter]] applies the same rule on the semantic side, so the two printers agree.
    */
  private def application(structure: GroundValue.Structure): String =
    if (structure.args.isEmpty) structure.typeName.name.name
    else if (structure.typeName.name.qualifier === Qualifier.Type)
      s"${structure.typeName.name.name}[${structure.args.map(render).mkString(", ")}]"
    else s"${structure.typeName.name.name}(${structure.args.map(render).mkString(", ")})"

  /** The carrier a **type constructor** in an ability's `F[_]` slot ultimately sits on: `IO` for
    * `{Throw[E], State[S] | IO}`, and the value itself when it is not a carrier stack at all. Diagnostics use it to
    * speak about the *base* of a row rather than about the machinery layered over it — the same rendering-only,
    * recognised-by-name licence as the rest of this object.
    */
  def baseCarrier(value: GroundValue): GroundValue = EffectRowRendering.baseOf(value, peel)

  /** A canonical-carrier application rendered as the pinned row that spells it, split according to the caller's context
    * (see the class doc). Ability arguments and the payload are types; the base slot is a type constructor, so [[peel]]
    * reads nested layers in the payload-unapplied form.
    */
  private def pinnedRow(structure: GroundValue.Structure, appliedToPayload: Boolean): Option[String] =
    EffectRowRendering
      .layerOf(structure.typeName, structure.args, appliedToPayload)
      .map(EffectRowRendering.row(_, peel, render))

  private def peel(value: GroundValue): Option[EffectRowRendering.Layer[GroundValue]] = value match {
    case GroundValue.Structure(name, args, _) => EffectRowRendering.layerOf(name, args, appliedToPayload = false)
    case _                                    => None
  }
}
