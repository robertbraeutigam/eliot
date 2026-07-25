package com.vanillasource.eliot.eliotc.ast.fact

import cats.Applicative
import cats.syntax.all.*

/** The structured **effect-row metadata** of a value's signature — the effects-as-channel "declared row + row
  * positions" (docs/effects-as-channel.md §4, Phase 1). Captured at [[core.processor.EffectSugarDesugarer]] from the
  * *open* effect rows (`{Console} A`, no `| base` tail) of a signature **before** they are rewritten onto the carrier
  * generic, and forwarded down the value fact chain (FunctionDefinition → NamedValue → ResolvedValue →
  * BlockDesugaredValue → MatchDesugaredValue → OperatorResolvedValue) alongside `paramConstraints`, converting the
  * entry representation at each hop exactly as `paramConstraints` is converted.
  *
  * **Rendering metadata** (U4-c-0b): the effect verifier (`EffectAccountingProcessor`) reads "declared" from the
  * carrier-binder constraints, not this row; the row is the LSP's declared-row vocabulary. The carrier desugar still runs, so
  * the same row information is *also* present in the desugared signature (as `F ~ E` constraints); this field is the
  * explicit, position-attributed copy, and is deliberately **not** part of any `signatureEquality` (the desugared
  * signature already carries the character-exact merge check).
  *
  * The *open*-row entries populate `returnEffects`/`parameterEffects`. A **pinned** row (`{Throw[E] | Id} A`) is a
  * concrete carrier stack, not a channel constraint, so it contributes no entries — but its *positions* are recorded
  * separately (`returnPinned` / `pinnedParameterIndices`): the effects-as-channel **carrier-stack recognition tag**
  * (docs/effects-as-channel.md, finding 14 / §7 step 3). This is the by-construction primitive that lets the checker
  * later classify a discharger/handler parameter's expected slot (`catch`'s `{Throw[E] | G} A`, a value's pinned-row
  * return) as a carrier slot *without* re-deriving carrier-ness from shape or name at the use site. It is recorded at
  * the one point that knows — `EffectSugarDesugarer`, where the pinned row collapses to `<Ability>Carrier[...]` — and
  * is **inert for now** (nothing routes on it yet; step 4 will). Body rows are excluded too (the body is the runtime,
  * not the declared signature); so are generic-parameter-bound rows (they are neither the value's ambient nor a value
  * parameter).
  *
  * @param returnEffects
  *   Open-row entries at the **return** position — the value's own declared ambient row (what §5.1's `derived ⊆
  *   declared` check verifies the body against). Empty when the return carries no open row.
  * @param parameterEffects
  *   One entry per value parameter that itself carried an open row — an arrow-codomain callback row like
  *   `action: A => {Effect} Unit` — holding that parameter's index (into the definition's value args) and its open-row
  *   entries. Effect-transparent under §5. Empty for a first-order signature.
  * @param returnPinned
  *   True when the **return** position's declared type is itself a *pinned* row — a canonical carrier stack, e.g.
  *   `def counter: {State[S] | Id} A`. Position-only: a pinned row is a concrete type, so it carries no entries.
  * @param pinnedParameterIndices
  *   The value-parameter indices (same indexing as `parameterEffects`) whose declared type is itself a *pinned* row —
  *   the discharger/handler capture domains (`catch`'s `computation: {Throw[E] | G} A`). Disjoint from
  *   `parameterEffects` by construction (a position's top-level type is either an open row or a pinned row, never both).
  *   Empty when no parameter is a pinned carrier stack.
  * @tparam C
  *   The phase's ability-constraint representation: ast [[GenericParameter.AbilityConstraint]] at the desugarer, then
  *   `NamedValue.CoreAbilityConstraint`, then each later phase's `ResolvedAbilityConstraint`.
  */
case class EffectRow[C](
    returnEffects: Seq[C] = Seq.empty[C],
    parameterEffects: Seq[EffectRow.ParameterEffects[C]] = Seq.empty[EffectRow.ParameterEffects[C]],
    returnPinned: Boolean = false,
    pinnedParameterIndices: Set[Int] = Set.empty[Int]
) {

  /** Convert every entry with a pure function, preserving positions — the pure fact-chain hops (ast→core,
    * resolve→matchdesugar) use this, mirroring their `paramConstraints` conversion. The `C`-independent pinned-position
    * tag (`returnPinned` / `pinnedParameterIndices`) rides through unchanged.
    */
  def map[D](f: C => D): EffectRow[D] =
    EffectRow(returnEffects.map(f), parameterEffects.map(_.map(f)), returnPinned, pinnedParameterIndices)

  /** Convert every entry with an effectful function, preserving positions — the resolving fact-chain hops
    * (core→resolve, matchdesugar→operator) use this, mirroring their monadic `paramConstraints` conversion. The
    * `C`-independent pinned-position tag rides through unchanged.
    */
  def traverse[F[_]: Applicative, D](f: C => F[D]): F[EffectRow[D]] =
    (returnEffects.traverse(f), parameterEffects.traverse(_.traverse(f)))
      .mapN((r, p) => EffectRow(r, p, returnPinned, pinnedParameterIndices))
}

object EffectRow {

  /** A single value parameter's open-row entries, tagged with the parameter's positional index into the definition's
    * value args (stable through the fact chain — no phase reorders value parameters).
    */
  case class ParameterEffects[C](parameterIndex: Int, effects: Seq[C]) {
    def map[D](f: C => D): ParameterEffects[D] = ParameterEffects(parameterIndex, effects.map(f))

    def traverse[F[_]: Applicative, D](f: C => F[D]): F[ParameterEffects[D]] =
      effects.traverse(f).map(ParameterEffects(parameterIndex, _))
  }

  def empty[C]: EffectRow[C] = EffectRow(Seq.empty, Seq.empty)
}
