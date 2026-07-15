package com.vanillasource.eliot.eliotc.lsp.index

import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue.*

/** Renders a [[GroundValue]] — a fully evaluated, concrete monomorphic type — as a compact, human-readable type string
  * for hover / type hints.
  *
  * The compiler's own `Show[GroundValue]` is deliberately terse (it collapses every non-`Type` structure to
  * `"Structure(...)"` and drops type arguments), which is fine for debug logging but useless as an editor type hint.
  * This renderer instead shows the structure a reader expects:
  *
  *   - a data/type-constructor application as `Name[arg, …]` (`Int[0, 255]`, `IO[Unit]`), or just `Name` with no args;
  *   - a function type as a right-associative arrow (`String -> IO[Unit]`), parenthesising a function on the left of an
  *     arrow so `(A -> B) -> C` stays unambiguous;
  *   - a direct (literal / bound) value by its underlying `toString` (e.g. the `0` / `255` bounds of `Int[0, 255]`).
  */
object GroundValueRenderer {

  /** Render a ground value as a one-line type string. */
  def render(value: GroundValue): String = value match {
    case GroundValue.Type                 => "Type"
    case GroundValue.Direct(direct, _)    => direct.toString
    case structure: GroundValue.Structure =>
      structure.asFunctionType match {
        case Some((from, to)) => s"${renderOperand(from)} -> ${render(to)}"
        case None             =>
          if (structure.args.isEmpty) structure.typeName.name.name
          else s"${structure.typeName.name.name}[${structure.args.map(render).mkString(", ")}]"
      }
    case GroundValue.Param(index, Nil, _) => s"?p$index"
    case GroundValue.Param(index, args, _) => s"?p$index[${args.map(render).mkString(", ")}]"
  }

  /** A function type used as the left operand of an arrow is parenthesised so the arrow nesting reads unambiguously. */
  private def renderOperand(value: GroundValue): String =
    value.asFunctionType match {
      case Some(_) => s"(${render(value)})"
      case None    => render(value)
    }
}
