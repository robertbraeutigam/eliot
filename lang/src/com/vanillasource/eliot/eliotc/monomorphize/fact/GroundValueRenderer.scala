package com.vanillasource.eliot.eliotc.monomorphize.fact

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.Qualifier

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
  *   - a direct (literal / bound) value by its underlying `toString` (e.g. the `0` / `255` bounds of `Int[0, 255]`).
  *
  * The **carrier inverter** this used to front — a canonical `ThrowCarrier[E, StateCarrier[S, Id], A]` stack rendered
  * back as the pinned row `{Throw[E], State[S] | Id} A`, plus the `Id[X]` ⤳ `X` erasure — went with the carrier
  * (effects v6, F3). An effect is an ordinary nullary ability now and an implementation is a name, so a type contains
  * no machinery to hide and there is nothing to invert: every type prints as what the user wrote. That also collapsed
  * the two entry points this had, which existed only because a carrier's last argument means one thing when the stack
  * is applied to a payload and another when it is not.
  */
object GroundValueRenderer {

  /** Render a ground type as a one-line string. */
  def render(value: GroundValue): String = value match {
    case GroundValue.Type                 => "Type"
    case GroundValue.Direct(direct, _)    => direct.toString
    case structure: GroundValue.Structure =>
      structure.asFunctionType match {
        case Some((from, to)) => s"${renderOperand(from)} -> ${render(to)}"
        case None             => application(structure)
      }
    case GroundValue.Param(index, Nil, _)  => s"?p$index"
    case GroundValue.Param(index, args, _) => s"?p$index[${args.map(render).mkString(", ")}]"
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

}
