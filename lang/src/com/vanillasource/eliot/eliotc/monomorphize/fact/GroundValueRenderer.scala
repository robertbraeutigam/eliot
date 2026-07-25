package com.vanillasource.eliot.eliotc.monomorphize.fact

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.effect.EffectRowRendering
import com.vanillasource.eliot.eliotc.module.fact.WellKnownTypes

/** Renders a [[GroundValue]] — a fully evaluated, concrete monomorphic type — as a compact, human-readable type string
  * for anything a user reads: LSP hover / type hints, and the type arguments of an ability-demand diagnostic.
  *
  * The compiler's own `Show[GroundValue]` is deliberately terse (it collapses every non-`Type` structure to
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
  * '''Two deliberate decisions about `Id`''' (docs/effects-as-channel.md §9 — `Id` and carriers are never rendered to
  * users):
  *   - `Id[X]` is **erased** to `X`. It is pure machinery the checker inserts at a pure boundary and the
  *     Id-normalization stage erases downstream; a consumer reading a pre-erasure `MonomorphicValue` would otherwise
  *     show it. (Erasing here is a rendering fallback — a consumer holding pre-erasure facts should still
  *     Id-normalize its input, so the machinery *nodes* disappear too, not just their names.)
  *   - an `Id` **row base is kept**: `{Throw[E] | Id} A` is exactly the legal surface a user writes for a stack pinned
  *     to the pure base, and it is *not* the same type as the open row `{Throw[E]} A` (whose carrier the caller
  *     chooses). Suppressing the base would render two different types identically.
  */
object GroundValueRenderer {

  /** Render a ground value as a one-line type string. */
  def render(value: GroundValue): String = value match {
    case GroundValue.Type                                                                  => "Type"
    case GroundValue.Direct(direct, _)                                                     => direct.toString
    // `Id[X] ⤳ X`: the identity carrier is a newtype over its payload and pure machinery — never shown. Matched with a
    // trailing wildcard for the same reason `GroundValue.carrierFQN` does: only the payload slot is load-bearing.
    case GroundValue.Structure(name, payload +: _, _) if name === WellKnownTypes.idFQN     => render(payload)
    case structure: GroundValue.Structure                                                  =>
      structure.asFunctionType match {
        case Some((from, to)) => s"${renderOperand(from)} -> ${render(to)}"
        case None             => pinnedRow(structure).getOrElse(application(structure))
      }
    case GroundValue.Param(index, Nil, _)                                                  => s"?p$index"
    case GroundValue.Param(index, args, _)                                                 =>
      s"?p$index[${args.map(render).mkString(", ")}]"
  }

  /** A function type used as the left operand of an arrow is parenthesised so the arrow nesting reads unambiguously. */
  def renderOperand(value: GroundValue): String =
    value.asFunctionType match {
      case Some(_) => s"(${render(value)})"
      case None    => render(value)
    }

  private def application(structure: GroundValue.Structure): String =
    if (structure.args.isEmpty) structure.typeName.name.name
    else s"${structure.typeName.name.name}[${structure.args.map(render).mkString(", ")}]"

  /** A canonical-carrier application rendered as the pinned row that spells it. Whether the last argument is the
    * payload or the base is read off `valueType`: a structure whose type is `Type` is applied to a result
    * (`ThrowCarrier[E, G, A]`), while a partially applied one (`ThrowCarrier[E, G]`, the shape a carrier takes in an
    * `F[_]` slot) still has an arrow kind. That is an exact signal, not a guess about arity.
    */
  private def pinnedRow(structure: GroundValue.Structure): Option[String] =
    EffectRowRendering
      .layerOf(structure.typeName, structure.args, appliedToPayload = structure.valueType === GroundValue.Type)
      .map(EffectRowRendering.row(_, peel, render))

  private def peel(value: GroundValue): Option[EffectRowRendering.Layer[GroundValue]] = value match {
    case GroundValue.Structure(name, args, _) => EffectRowRendering.layerOf(name, args, appliedToPayload = false)
    case _                                    => None
  }
}
