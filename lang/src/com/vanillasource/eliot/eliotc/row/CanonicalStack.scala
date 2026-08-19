package com.vanillasource.eliot.eliotc.row

import com.vanillasource.eliot.eliotc.effect.EffectCarrierNaming
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN

/** The **canonical carrier stack of a row** — the second half of v4's canonicalisation obligation
  * (`docs/effects-as-channel-v4.md` §6, `docs/effects-v4-p0-spike.md` §3 S3).
  *
  * P0 measured that a *stored* computation's carrier is ground at the `WovenValue` seam, but that the seam reads it off
  * the base its author pinned into the field's type (`{Throw[String] | Id}`). A v4 computation type has no base — a row
  * is only a set of abilities — so the lowering has to **compute** one, and producer and consumer share nothing but the
  * type. The rule, fixed here rather than discovered in P2 because adding an ordering decision after the canonical form
  * exists is exactly §4's two-spellings trap:
  *
  * > the canonical stack of a row is its canonical (sorted, deduplicated) ability order lowered to carriers, over the
  * > pure base when the row rides no `Suspend`, and over the platform's run carrier when it does.
  *
  * Leftmost canonical entry = outermost layer = discharged first, matching the v3 pinned-row ordering the stdlib
  * dischargers are written in. **This fixes a semantics the author picks today** (§10 R8): a stored
  * `{Throw[E], State[S]}` has no handler in the term where it is built, so the canonical order — not the author — says
  * whether `State` sits outside `Throw` (state survives a raise) or inside it (state is discarded). That is a
  * consequence of the design, recorded, not smuggled.
  *
  * '''An ability rides `Suspend` exactly when it has no carrier.''' `carrierOf` answers "does this ability have a
  * canonical carrier type", and the caller supplies it — the naming convention alone cannot, because the answer is
  * whether the type *resolves*, which is a fact read. Recognition of carrier-ness is never done here by name: this
  * object only ever *constructs*, which is the direction the convention has always been sound in
  * ([[EffectCarrierNaming]]).
  *
  * '''What P0 could not settle, surfaced rather than routed around''' (v4 standing rule 5). §10 R4 asks whether `Id`
  * has a second reason to exist beyond "the empty row still needs a carrier". It does: a *non-empty* row that rides no
  * `Suspend` lowers to a transformer stack, and the innermost transformer still needs a base monad to sit on —
  * `ThrowCarrier[E, ?]`. So the *empty* row genuinely needs no representation (a pure computation lowers to itself, and
  * [[Stack.representation]] returns the payload untouched), but the *pure base* does not disappear with it. It is
  * therefore a parameter here, not a constant: today's tree passes `Id`, and a backend that lowers `Throw` to a branch
  * rather than to a transformer (§8) passes whatever it lowers onto. §7's deletion list, which retires `Id` outright,
  * is one line too broad on this point.
  */
object CanonicalStack {

  /** One layer of a canonical stack: the carrier type realizing one row entry, with that entry's own type arguments in
    * the carrier's leading slots (`Throw[E]` ⤳ `ThrowCarrier[E, base]`).
    */
  final case class Layer(carrier: ValueFQN, args: Seq[GroundValue])

  /** A row's canonical stack: its dischargeable layers outermost-first, over a base.
    *
    * @param layers
    *   The row's carrier-bearing entries, in canonical order, outermost first.
    * @param base
    *   The carrier the innermost layer sits on: the platform run carrier when any entry rides `Suspend`, the pure base
    *   otherwise.
    * @param suspends
    *   Whether the row rides `Suspend` — i.e. whether any entry has no carrier of its own and so can only be performed
    *   on the platform's run carrier. It is what decides [[base]], and it is what makes a row with no dischargeable
    *   layer still have a representation.
    */
  final case class Stack(layers: Seq[Layer], base: GroundValue, suspends: Boolean) {

    /** The payload-unapplied carrier value — the shape a carrier takes in an `F[_]` slot. [[None]] for a row that
      * lowers to nothing at all (empty, and riding no `Suspend`): pure code lowers to itself.
      */
    def constructor: Option[GroundValue] =
      Option.when(layers.nonEmpty || suspends)(layers.foldRight(base)(applyLayer))

    /** The representation type of `{row} payload`. A row that lowers to nothing yields the payload itself. */
    def representation(payload: GroundValue): GroundValue =
      constructor.fold(payload)(carrier => applyPayload(carrier, payload))

    private def applyLayer(layer: Layer, inner: GroundValue): GroundValue =
      GroundValue.Structure(layer.carrier, layer.args :+ inner, GroundValue.Type)

    private def applyPayload(carrier: GroundValue, payload: GroundValue): GroundValue = carrier match {
      case GroundValue.Structure(name, args, _) => GroundValue.Structure(name, args :+ payload, GroundValue.Type)
      case other                                => other
    }
  }

  /** The canonical stack of a row.
    *
    * @param row
    *   The row, in canonical form (see [[com.vanillasource.eliot.eliotc.monomorphize.fact.CanonicalRow]]).
    * @param carrierOf
    *   The canonical carrier type of an ability, or [[None]] when the ability has none and therefore rides `Suspend`
    *   (`Console`, `Log`, `Inf`). A fact read, supplied by the caller.
    * @param pureBase
    *   The base a `Suspend`-free stack sits on. See the class note on §10 R4.
    * @param runCarrier
    *   The platform's run carrier, the base a `Suspend`-riding stack sits on.
    */
  def of(
      row: GroundValue.Row,
      carrierOf: ValueFQN => Option[ValueFQN],
      pureBase: GroundValue,
      runCarrier: GroundValue
  ): Stack = {
    val classified = row.entries.map(entry => entry -> carrierOf(entry.ability))
    val rides      = classified.exists(_._2.isEmpty)
    val layers     = classified.collect { case (entry, Some(carrier)) => Layer(carrier, entry.args) }
    Stack(layers, if (rides) runCarrier else pureBase, rides)
  }

  /** The canonical carrier FQN an ability *would* have by the `<Ability>Carrier` convention — the construction
    * direction only. Whether that type actually exists is the caller's `carrierOf` read.
    */
  def conventionalCarrier(ability: AbilityFQN): ValueFQN = EffectCarrierNaming.carrierFQN(ability)
}
