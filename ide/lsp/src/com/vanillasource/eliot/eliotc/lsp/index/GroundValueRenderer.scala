package com.vanillasource.eliot.eliotc.lsp.index

import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
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
  *   - a direct (literal / bound) value by its underlying `toString` (e.g. the `0` / `255` bounds of `Int[0, 255]`);
  *   - a canonical effect-carrier stack as its *pinned effect row* — the surface syntax that spells it —
  *     `ThrowCarrier[E, StateCarrier[S, Id], A]` rendering as `{Throw[E], State[S] | Id} A`, so hover never surfaces
  *     the carrier machinery names.
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
          renderPinnedRow(structure).getOrElse {
            if (structure.args.isEmpty) structure.typeName.name.name
            else s"${structure.typeName.name.name}[${structure.args.map(render).mkString(", ")}]"
          }
      }
    case GroundValue.Param(index, Nil, _) => s"?p$index"
    case GroundValue.Param(index, args, _) => s"?p$index[${args.map(render).mkString(", ")}]"
  }

  /** The canonical effect carriers, recognized by FQN, each mapped to its effect's surface name and the number of
    * ability type arguments preceding the base-carrier slot in the carrier's parameter list.
    */
  private def carrierInfo(fqn: ValueFQN): Option[(String, Int)] =
    Option
      .when(fqn.moduleName.packages == Seq("eliot", "effect"))((fqn.moduleName.name, fqn.name.name))
      .collect {
        case ("Throw", "ThrowCarrier") => ("Throw", 1)
        case ("Abort", "AbortCarrier") => ("Abort", 0)
        case ("State", "StateCarrier") => ("State", 1)
        case ("Dep", "DepCarrier")     => ("Dep", 1)
      }

  /** A *full* canonical-carrier application renders as the pinned effect row that spells it: the outermost carrier's
    * ability becomes the first row entry, nested *partial* carrier applications in the base slot flatten into further
    * entries (leftmost = outermost), and the first non-carrier base ends up after the `|`.
    */
  private def renderPinnedRow(structure: GroundValue.Structure): Option[String] =
    carrierInfo(structure.typeName).collect { case (effect, abilityArgCount) if structure.args.size == abilityArgCount + 2 =>
      val (entries, base) =
        peelLayers(structure.args(abilityArgCount), Seq(effectEntry(effect, structure.args.take(abilityArgCount))))
      s"{${entries.mkString(", ")} | ${render(base)}} ${render(structure.args.last)}"
    }

  private def peelLayers(base: GroundValue, entries: Seq[String]): (Seq[String], GroundValue) = base match {
    case s: GroundValue.Structure =>
      carrierInfo(s.typeName) match {
        case Some((effect, abilityArgCount)) if s.args.size == abilityArgCount + 1 =>
          peelLayers(s.args.last, entries :+ effectEntry(effect, s.args.take(abilityArgCount)))
        case _                                                                     => (entries, base)
      }
    case _                        => (entries, base)
  }

  private def effectEntry(effect: String, args: Seq[GroundValue]): String =
    if (args.isEmpty) effect else s"$effect[${args.map(render).mkString(", ")}]"

  /** A function type used as the left operand of an arrow is parenthesised so the arrow nesting reads unambiguously. */
  private def renderOperand(value: GroundValue): String =
    value.asFunctionType match {
      case Some(_) => s"(${render(value)})"
      case None    => render(value)
    }
}
