package com.vanillasource.eliot.eliotc.lsp.index

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.monomorphize.channel.RefinementTable
import com.vanillasource.eliot.eliotc.monomorphize.fact.{GroundValue, MonomorphicExpression, MonomorphicValue}
import com.vanillasource.eliot.eliotc.lsp.virtual.VfsUris
import com.vanillasource.eliot.eliotc.pos.{Position, PositionRange}
import com.vanillasource.eliot.eliotc.source.content.Sourced

import java.net.URI
import java.nio.file.Paths

/** Position → concrete type index, the data behind hover type hints.
  *
  * Unlike [[PositionIndex]] (which renders a referenced value's *declared* signature), this index reports the actual
  * *monomorphic* type each expression node was checked at — the per-node `expressionType` produced by the NbE checker.
  * That information only exists after monomorphization, which is driven from a `main`: the
  * [[com.vanillasource.eliot.eliotc.lsp.plugin.LspPlugin]] demands
  * [[com.vanillasource.eliot.eliotc.used.UsedNames]] for every file that declares its own `main`, forcing a
  * [[MonomorphicValue]] for every reachable instantiation. This index is rebuilt from those facts after each compile.
  *
  * Because a generic value reachable at several instantiations yields several [[MonomorphicValue]]s sharing source
  * ranges, a single node range can carry more than one type; a lookup returns the distinct set at the most specific
  * (innermost) node containing the queried position. Positions are in the compiler's 1-based domain; the protocol layer
  * converts from LSP's 0-based positions before querying.
  */
final class TypeHintIndex private (hintsByUri: Map[String, Seq[TypeHintIndex.Hint]]) {
  import TypeHintIndex.*

  /** The distinct monomorphic type(s) at the most specific expression node containing `position`, paired with that
    * node's range, or `None` if no typed node covers the position. More than one type means the enclosing definition was
    * monomorphized at several instantiations.
    */
  def typeHintsAt(uri: URI, position: Position): Option[(PositionRange, Seq[GroundValue])] = {
    val containing = hintsByUri.getOrElse(uriKey(uri), Seq.empty).filter(hint => contains(hint.range, position))
    containing
      .reduceOption((a, b) => if (moreSpecific(a.range, b.range)) a else b)
      .map(innermost => innermost.range -> containing.filter(_.range == innermost.range).map(_.tpe).distinct)
  }

  /** The refinement channel's value range for the node at exactly `range` in `uri`, or `None` when the channel could not
    * pin one (a ⊤/bignum node — a parameter, a call result, anything outside a literal/arithmetic-transfer/`if`-join
    * spine). When the enclosing definition was monomorphized at several instantiations that pinned *different* ranges at
    * this node, their **union** `[min of mins, max of maxes]` is returned — the sound over-approximation to display.
    * Callers pass the `range` [[typeHintsAt]] returned for the same position, so no new position math is needed.
    */
  def intervalAt(uri: URI, range: PositionRange): Option[(BigInt, BigInt)] = {
    val intervals =
      hintsByUri.getOrElse(uriKey(uri), Seq.empty).filter(_.range == range).flatMap(_.interval)
    Option.when(intervals.nonEmpty)(intervals.map(_._1).min -> intervals.map(_._2).max)
  }
}

object TypeHintIndex {

  /** One typed node: a source range, the concrete type the checker assigned to the expression occupying it, and — when
    * the refinement channel could pin one — the value's range interval `[min, max]` at that node.
    */
  final case class Hint(uri: URI, range: PositionRange, tpe: GroundValue, interval: Option[(BigInt, BigInt)])

  val empty: TypeHintIndex = new TypeHintIndex(Map.empty)

  /** Build the index from all monomorphized values materialised by a compile, grouping the typed nodes per document.
    * Each value's [[RefinementTable]] (keyed identically to its [[MonomorphicValue]]) supplies the per-node value-range
    * intervals, joined here by position so a hover can show the range the channel computed.
    */
  def build(values: Seq[MonomorphicValue], refinements: Seq[RefinementTable]): TypeHintIndex = {
    val intervalsByKey = refinements.map(rt => (rt.vfqn, rt.typeArguments) -> unambiguousIntervals(rt)).toMap
    new TypeHintIndex(
      values.flatMap(value => hintsOf(value, intervalsByKey.getOrElse((value.vfqn, value.typeArguments), Map.empty)))
        .groupBy(hint => uriKey(hint.uri))
    )
  }

  /** Collapse a table's per-node intervals to a position-keyed map, keeping a position only when every entry at it
    * agrees on one interval (mirroring `MonomorphicUncurryingProcessor.unambiguousIntervalsByPosition`): a position
    * carrying two or more distinct intervals is dropped rather than displayed ambiguously.
    */
  private def unambiguousIntervals(table: RefinementTable): Map[PositionRange, (BigInt, BigInt)] =
    table.intervals
      .groupBy(_.position)
      .collect {
        case (position, entries) if entries.map(ni => (ni.min, ni.max)).distinct.sizeIs == 1 =>
          position -> (entries.head.min, entries.head.max)
      }

  /** Every typed node of one monomorphized value. Compound bodies (an application or a function literal) are fully
    * described by their child nodes, which carry their own precise types and ranges; annotating the whole-body range
    * with the signature would only collide with those children (the outermost node's range coincides with a child's).
    * Only a *leaf* body — a bare value reference, literal, or parameter — has no child node to carry its type, so for
    * those the body range is annotated with the value's signature (e.g. `main = greeting` ⇒ `greeting` reads `IO[Unit]`).
    */
  private def hintsOf(value: MonomorphicValue, intervals: Map[PositionRange, (BigInt, BigInt)]): Seq[Hint] =
    value.runtime.toSeq.flatMap { body =>
      val topHint = body.value match {
        case _: MonomorphicExpression.FunctionApplication | _: MonomorphicExpression.FunctionLiteral => Seq.empty
        case _ => Seq(Hint(body.uri, body.range, value.signature, intervals.get(body.range)))
      }
      topHint ++ fromExpression(body.value, intervals)
    }

  private def fromNode(node: Sourced[MonomorphicExpression], intervals: Map[PositionRange, (BigInt, BigInt)]): Seq[Hint] =
    Hint(node.uri, node.range, node.value.expressionType, intervals.get(node.range)) +:
      fromExpression(node.value.expression, intervals)

  private def fromExpression(
      expression: MonomorphicExpression.Expression,
      intervals: Map[PositionRange, (BigInt, BigInt)]
  ): Seq[Hint] = expression match {
    case MonomorphicExpression.FunctionApplication(target, argument) =>
      fromNode(target, intervals) ++ fromNode(argument, intervals)
    case MonomorphicExpression.FunctionLiteral(_, _, body)           => fromNode(body, intervals)
    case _: MonomorphicExpression.MonomorphicValueReference | _: MonomorphicExpression.IntegerLiteral |
        _: MonomorphicExpression.StringLiteral | _: MonomorphicExpression.ParameterReference =>
      Seq.empty
  }

  /** Inclusive containment in the compiler's 1-based domain (the end is included so a caret immediately after a node
    * still matches), mirroring [[PositionIndex]].
    */
  private def contains(range: PositionRange, position: Position): Boolean =
    range.from <= position && position <= range.to

  /** Whether the first range is more specific (more deeply nested) than the second: it starts later, or starts together
    * but ends sooner.
    */
  private def moreSpecific(a: PositionRange, b: PositionRange): Boolean =
    a.from > b.from || (a.from === b.from && a.to < b.to)

  /** Normalise a URI to a stable key so the editor's `file:///…` URIs match the compiler's `file:/…` URIs, as in
    * [[PositionIndex]]. Non-file URIs fall back to their string form and simply never match a workspace document.
    */
  private def uriKey(uri: URI): String =
    try Paths.get(VfsUris.toFileUri(uri)).toString
    catch { case _: IllegalArgumentException | _: java.nio.file.FileSystemNotFoundException => uri.toString }
}
