package com.vanillasource.eliot.eliotc.core.processor

import cats.Order
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.Visibility
import com.vanillasource.eliot.eliotc.core.fact.NamedValue
import com.vanillasource.eliot.eliotc.pos.Position
import com.vanillasource.eliot.eliotc.source.content.Sourced

/** Visibility-order check — a file's public API must be a *prefix* of its declarations.
  *
  * Reading a module should not require scanning it. `private` alone does not deliver that: with declarations in any
  * order, establishing that a name is public means reading every declaration to confirm none of them said `private`,
  * because public is signalled by the *absence* of a marker. Sorting by visibility fixes it — the first `private` is a
  * terminator, so finding the public API becomes a prefix read instead of a full scan.
  *
  * This is C++'s `private:` access specifier expressed as an ordering rule rather than a block: Eliot has no block
  * structure to hang sections on, but the same guarantee falls out of "once private, everything after is private".
  *
  * The check runs on the **desugared** [[NamedValue]]s rather than on the source AST, so it needs no per-construct
  * arms. `def`, `type`, `data`, `ability` and `implement` all reduce to named values carrying a `visibility`, and the
  * rule is then a single statement about that one field. Two consequences of running here, both intended:
  *   - An `ability` or `implement` block is public (both parsers mint `Visibility.Public`), so it may not follow a
  *     private declaration. No exception is carved out for either.
  *   - A `data` fans out to a type constructor, value constructors, field accessors and its synthetic
  *     `PatternMatch`/`TypeMatch` implementations. All of them inherit the declaration's visibility (see
  *     [[DataDefinitionDesugarer]]), so a private `data` is wholly private and never trips the rule against itself.
  *
  * Ordering is by **source position**, not by position in the sequence: one `data` fans out to many named values, and
  * the desugarers append the synthetic ones after the main ones, so sequence order is not source order. Errors are
  * deduplicated **by line**, which is what keeps that fan-out from reporting a single misplaced `data` several times —
  * its type constructor, value constructors, accessors and synthetic implementations are all positioned inside the one
  * declaration.
  */
object VisibilityOrderChecker {
  private given Ordering[Position] = Order[Position].toOrdering

  /** One sourced error per offending public declaration — those positioned after the file's first `private` — in
    * source order; empty when the file is ordered, has no private declarations, or is entirely private. The source
    * position points at the offending *public* declaration, which is the one that has to move; the private block it
    * follows is innocent, and is named by line so the target of the move is unambiguous.
    */
  def check(namedValues: Seq[NamedValue]): Seq[Sourced[String]] =
    namedValues
      .filter(_.visibility == Visibility.Private)
      .map(_.qualifiedName.range.from)
      .minOption
      .toSeq
      .flatMap { firstPrivate =>
        namedValues
          .filter(namedValue =>
            namedValue.visibility == Visibility.Public && namedValue.qualifiedName.range.from > firstPrivate
          )
          .sortBy(_.qualifiedName.range.from)
          .distinctBy(_.qualifiedName.range.from.line)
          .map(
            _.qualifiedName.as(
              s"Public declaration after the private declarations starting on line ${firstPrivate.line}."
            )
          )
      }
}
