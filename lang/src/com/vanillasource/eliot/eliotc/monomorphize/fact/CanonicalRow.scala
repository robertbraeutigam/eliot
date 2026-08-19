package com.vanillasource.eliot.eliotc.monomorphize.fact

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN

/** The **canonical form** of an effect row (effects-as-channel v4, `docs/effects-as-channel-v4.md` §4) — the single
  * spelling every [[GroundValue.Row]] is expected to be in.
  *
  * '''Why this is a precondition of the type former existing at all.''' Ground values compare structurally
  * (`Eq.fromUniversalEquals`) and a monomorphic key is a `Seq[GroundValue]`, so two spellings of the same row read as
  * two different types. This project has already paid for that lesson once, in the meta channel: the outer `Bound`
  * wrapper was collapsed precisely because verdicts are compared structurally and *"two spellings of the same top read
  * as disagreement"* (`docs/total-meta-transfers.md`). Canonicalisation is not a detail to be added later.
  *
  * '''The form.''' Entries are ordered by a deterministic structural key — the ability's fully qualified name first,
  * then its own type arguments read structurally — and *exactly equal* entries are deduplicated. Two entries naming the
  * same ability at **different** arguments (`State[Int]` and `State[String]`) are distinct effects and both survive, in
  * key order; the order among them is therefore a function of the row, not of how it was written (§10 Q2).
  *
  * '''What deduplication may not do.''' Only structural equality dedupes. A row is a *set* of ability references, so
  * `{Console, Console}` is `{Console}` — but `{State[S], State[T]}` is two entries, and the canonical order fixes which
  * is outermost once that row is lowered to a carrier stack ([[com.vanillasource.eliot.eliotc.row.CanonicalStack]],
  * §10 R8).
  */
object CanonicalRow {

  /** Canonicalise a row: order entries by [[entryKey]] and drop exact duplicates. Idempotent. */
  def canonicalise(entries: Seq[GroundValue.Row.Entry]): Seq[GroundValue.Row.Entry] =
    entries.distinct.sortBy(entryKey)

  /** Build a canonical row value from arbitrary entries. */
  def row(entries: Seq[GroundValue.Row.Entry]): GroundValue.Row = GroundValue.Row(canonicalise(entries))

  /** The union of two rows, canonical by construction — the derivation's only combinator (effects accumulate by union,
    * never by a join or a lattice: v4 standing rule 1).
    */
  def union(left: GroundValue.Row, right: GroundValue.Row): GroundValue.Row =
    row(left.entries ++ right.entries)

  /** `left` without every entry of `right` — the *discharge* subtraction, used to derive the stack a call needs beyond
    * its caller's ambient. Structural, like every other row operation here.
    */
  def difference(left: GroundValue.Row, right: GroundValue.Row): GroundValue.Row =
    GroundValue.Row(left.entries.filterNot(right.entries.contains))

  /** Whether every entry of `left` also occurs in `right` — the `derived ⊆ declared` check's set inclusion. */
  def subsetOf(left: GroundValue.Row, right: GroundValue.Row): Boolean =
    left.entries.forall(right.entries.contains)

  /** The deterministic ordering key of one row entry: the ability's FQN, then its arguments structurally. */
  private def entryKey(entry: GroundValue.Row.Entry): String =
    (fqnKey(entry.ability) +: entry.args.map(valueKey)).mkString("(", ",", ")")

  /** A deterministic, structural key for any ground value. It exists only to *order* entries — it never decides
    * equality, which stays structural — so it may be lossy in principle; it is written total so that it is not.
    */
  private def valueKey(value: GroundValue): String = value match {
    case GroundValue.Type                     => "Type"
    case GroundValue.Direct(literal, _)       => s"D:${literal.debugString}"
    case GroundValue.Structure(name, args, _) => s"S:${fqnKey(name)}${args.map(valueKey).mkString("[", ",", "]")}"
    case GroundValue.Param(index, args, _)    => s"P:$index${args.map(valueKey).mkString("[", ",", "]")}"
    case GroundValue.Row(entries)             => entries.map(entryKey).mkString("{", ",", "}")
    case GroundValue.Computation(row, payload) => s"C:${valueKey(row)}:${valueKey(payload)}"
  }

  private def fqnKey(fqn: ValueFQN): String = fqn.show
}
