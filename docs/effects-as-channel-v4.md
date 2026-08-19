# Effects as a Channel, v4: The Row Leaves the Type, the Carrier Leaves the Language

**Status (2026-08-19): PROPOSAL. P0, P1 and P3 executed; P2/P4/P5 — the flag day — not started, and held: a
readiness check (`docs/effects-v4-flag-day-readiness.md`) found one design hole (R9), one amendment (R10) and one
open decision (R7) between here and the flag day, the first of which the plan does not know about.** The one phase §11 allows to start
with — the R1 spike — has been run and written up in `docs/effects-v4-p0-spike.md`: R1 is **cleared by
measurement**, with two amendments folded into §6 and two new risks (R7, R8) added to §10. That gate opens P1; it
does not decide v4. This document is a design sketch written from a
question Robert asked — *"what if effects were part of the type properly, so `{Eff} A` is not `[E[_] ~
Eff] … : E[A]`; a parallel mechanism rather than something the checker implements — would that be easier
and simpler?"* — and answered against the tree as it stands at `claude/type-aliases-non-pinned-effects`.
It states a design, its cost, and what would have to be true for it to be right. **It decides nothing.**
Per `docs/effects-as-rows.md` standing rule 1, the decision is Robert's; per standing rule 2, the parts
below marked *conflict* are to be surfaced, not routed around.

**One-sentence summary.** The effect row is in the type today only in order to carry the **carrier**,
which is a *runtime representation*, not a typing fact — so put the row in the type only where a
computation is genuinely a **value** (arrows and stored computations), keep everything else in the
channel that already verifies it, and move the carrier out of the language entirely into one
post-monomorphization lowering where every type is ground.

**What that buys, in one line each.** §1 rule 4 — agreed and eroded four times — stops being a rule the
elaborator enforces and becomes ordinary definitional equality. `Id`, `runId`, `~ Effect`, `~ Suspend`,
the `<Ability>Carrier` convention, the open/pinned split, the `{Effect}` reuse rule, the inferable
carrier binder and the type-alias limitation all become unreachable states rather than handled ones.

**What it costs, in one line each.** One new type former and one canonicalisation obligation; effect
lowering becomes a compiler pass instead of a desugar over library code; the lowered core is
machine-generated and must be re-checked; and the change is a **flag day** — the checker, the elaborator
and ability resolution move together.

**How to read this document.** §§1–9 state the design. §10 is the risk register and §11 the migration
plan with its gates. Where this document contradicts `docs/effects-as-rows.md` (v3, LANDED), v3 is the
current tree and this is a proposal about it; nothing here amends v3 in place.

---

## 0. Why revisit v3

v3 is landed, correct and internally consistent. The reason to look again is not a defect but a
**shape**: the amount of mechanism that exists solely to move a carrier around, and the fact that this
mechanism keeps generating new edge cases at its boundary.

The immediate provocation is small and typical. `type Test = {Writer[List[TestCase]] | Id} Unit` works;
`type Test = {Writer[List[TestCase]]} Unit` cannot, and is rejected at core
(`EffectSugarDesugarer.rowErrors`, `docs/effects-as-rows.md` A.11.13). The reason is purely
representational: an open row lowers by minting a carrier binder **on the definition that spells the
row**, and for an alias that is the alias, whose binders no user of the alias can reach. Nothing about
effects makes this impossible; the encoding makes it impossible.

The same encoding is upstream of most of the rest of the effect surface:

| mechanism | exists because |
| --- | --- |
| the inferable carrier binder, `{Effect}` binder reuse | the row must find a carrier to live on |
| open vs pinned rows | a row that must be a *type* needs a concrete carrier stack |
| `<Ability>Carrier` naming convention | the pinned form has to name that stack |
| `EffectRowRendering` / `EffectCarrierNaming` (140 lines) | undo the above before showing a user anything |
| `Id`, `runId`, `IdNormalizer` (310), `assertNoIdResidue` | the empty row still needs *a* carrier |
| `EffectLifter.tryPureWrap` (in 342) | a pure term meeting a carrier-headed slot |
| rule 4's slot trichotomy in `RowElaborator` (1,919) | decide, per slot, whether a carrier may pass |
| `~ Effect` / `~ Suspend` machinery abilities | mark which generics are carriers |

None of these is about *which effects a program performs*. They are all about *where the monad goes*.

The question this document answers is whether that can be separated, and whether the separation is
cheaper than the status quo.

## 1. The observation

**The row is not in the type for soundness.** Soundness comes from the channel:
`monomorphize/channel/EffectAccountingProcessor` verifies `derived ⊆ declared` on **ground** mono
instances and is a codegen precondition (`WovenValueProcessor` reads it via `getFactOrAbort`, so a leak
cannot reach bytecode). Whole-program monomorphization from `main` means every higher-order position is
concrete by the time that check runs — the use-site-verification cornerstone, already paid for.

The row is in the type because **the carrier is the runtime representation** — `flatMap`, `pure`, `IO`,
the transformer stacks the dischargers are written over — and the elaborator needs somewhere to write it
before checking.

Separate the two jobs and each becomes simple:

- *what effects happen* → the channel (already there, already exact),
- *how they are represented* → a lowering, which can run when everything is ground.

## 2. The three tiers

**Tier 1 — the channel (most code).** A definition's own row is declaration metadata, exactly as an
`Int`'s range is. `def readLine: {Console} String` gives `readLine : String` to the checker, plus the
declared row `{Console}` on the side. `printLine("hi")` is a call of type `Unit`. A block is a sequence.
The checker never sees an effect, has no arm for one, and needs no vocabulary for one.

**Tier 2 — the type (only where a computation is a value).** The moment a computation is *stored,
passed, or returned as data*, its row must be in its type, because nothing else can recover it: a
`List[TestCase]` holding suspended bodies, a parameter `f: A => {Console} B`, a `data` field. This is
today's §1 rule 3 (*pinned means captured*) generalised and made the **only** place a row appears in a
type. It is spelled with a primitive computation type, not with a carrier:

```
{Console} String                 -- the type of a suspended computation performing Console, yielding String
A => {Console} B                 -- an arrow with a latent row
def map[A, B, r](f: A => {r} B, xs: List[A]): {r} List[B]
```

**Tier 3 — the carrier (compiler-internal, post-mono).** `IO`, `flatMap`, `pure`, the transformer
stacks: never named by a user, never seen by the checker, introduced by one lowering pass at the
`WovenValue` seam where every type argument is a `GroundValue`. The `eliot.carrier` package stops being
"machinery the language deliberately hides" and becomes machinery the language does not contain.

## 3. What happens to the four user rules

v3's four rules are the user model and they survive — but three of them stop being *enforced* and start
being *true*.

1. **Effects run where they are written.** Unchanged, and now unenforceable-against: a call performs at
   its site because there is nothing else it could do; there is no carrier to hoist it onto.
2. **Suspension is declared.** Unchanged in spelling (`whenTrue: {Effect} A`, `value: {Abort} T`), but
   the slot's type is now a **computation type**, not `F[A]` with a row tag beside it. The tag-not-shape
   discipline disappears with the ambiguity that motivated it: a row position and a carrier position are
   no longer "the same shape".
   *A pure argument at a suspended slot still lifts* — `host else "localhost"` — but the lift is a
   syntactic zero-row coercion at a declared slot, not a `pure` call into a solved carrier.
3. **Pinned means captured.** Promoted: this is now the *only* way a row enters a type, and it needs no
   `| G` tail to say so, because there is no carrier to pin to. `{Throw[E]} A` is a computation type on
   its own. The `<Ability>Carrier` convention and the "must pin a stored row" rule both go.
4. **An effect passes through a position iff that position declares it.** **This is the change that
   matters.** Today rule 4 is a discipline the elaborator implements, and Appendix A of v3 records it
   being agreed and worked around four times, each time locally reasonably, each time expensively. In
   v4 it is not a rule at all: a computation has a computation *type*, a plain generic `A` is
   instantiated with a payload type, and handing a computation to a rowless slot fails in `unify` as an
   ordinary type mismatch. There is no arm to erode, no exemption to grant, and no
   "carrier-headed / rowless / pinned" trichotomy to classify a slot into — `RowElaborator`'s largest
   single concern.

This is the strongest argument for v4 and it should be weighed on its own: **the rule whose erosion
caused every stall in v3's history becomes a theorem.**

## 4. The type-language addition, and why it is not a new sort

The Types-Are-Values cornerstone forbids adding a kind or sort. v4 does not add one:

- A **row is an ordinary value** — a canonical set of ability references. Its type is an ordinary type
  (`Row`). A row variable `r` is an ordinary generic parameter whose type is `Row`, instantiated by the
  ordinary instantiation machinery, and **written down** by the same "every type argument a declaration
  determines is written" rule the elaborator already implements (v3 §3.1).
- A **computation type** `{r} A` is one primitive type former, `Computation[r, A]`, beside `VPi`. It is
  a former and not a `data` for the same reason `VPi` is (the *`VPi` is the one primitive Π-former*
  guardrail generalises to "computation is the one primitive effect former"): its introduction and
  elimination are the lowering's business, not a constructor's.
- An **arrow with a latent row** is the codomain being a computation type: `A => {r} B` is
  `VPi(A, _ => Computation(r, B))`. **`VPi` does not gain a field.** This matters — it is what keeps the
  change out of the checker's core.
- **Row equality is definitional equality** on the canonical value. Nothing bespoke; `unify` compares
  two normalised values as it already does.

**The canonicalisation obligation.** A row must have exactly **one** spelling — sorted, deduplicated,
with a fixed rendering of ability arguments — or two spellings of the same row will read as disagreement
in structural comparison. This project has already paid for this lesson once, in the meta channel: the
outer `Bound` wrapper was collapsed precisely because `ReconcileProcessor.metaByPosition` compares
verdicts structurally and *"two spellings of the same top read as disagreement"*
(`docs/total-meta-transfers.md`). Canonicalisation is not a detail to be added later; it is a
precondition of the type former existing at all.

**No row inference. Ever.** Row variables are instantiated from declarations and written, exactly like
type arguments. There is no row metavariable, no join, no lattice, no ordering-sensitive solve. This is
the same prohibition v3 states for carriers, carried over verbatim, and for the same reason: the moment
a row is *solved for* rather than *written*, carrier theft and premature commitment come back under a
new name. **A row metavariable in `Unifier` is the tell that v4 has failed.**

## 5. The channel, unchanged

Everything in v3 §2 survives, with the *carrier-binder* reading of "declared" replaced by the direct one
(a definition's declared row is now literally its declared row, not the constraints on its minted
binder):

- **Two verifiers, one vocabulary.** The pre-mono per-definition check (`RowChecker`'s derivation +
  subset check, reported at the definition) and the post-mono `EffectAccountingProcessor` (`derived ⊆
  declared` at ground instances, codegen precondition) both stay, both keep their message.
- **The ride test gets simpler.** Accounting's `ambientCarriers` comparison exists to decide whether a
  reference performs on *this* value's carrier. With no carrier in the type, a reference's contribution
  is its callee's declared row, and the "foreign concrete carrier" bound
  (`RowChecker.fixesCarrier`) — the fake-carrier testing strategy of `docs/testing-effects.md` L2 —
  becomes "the callee's row was consumed by a discharger", which is visible in the term.
- **`Inf` is unaffected**: still an ability, still originating only on a native, still propagating by
  subset, still the one effect allowed to reach `main` undischarged.

`EffectRow` stays as the position-attributed declaration metadata it already is. `aliasPinnedEffects`
and the alias problem that prompted this document both disappear: a type alias whose body is a
computation type is an ordinary alias of an ordinary type.

## 6. The lowering

One pass, at one seam, with one job.

**Where.** `MonomorphicValue → WovenValue` (`monomorphize/channel/WovenValueProcessor`), the existing
codegen seam that `used`/`uncurry`/the jvm backend already read in place of `MonomorphicValue`. Today
that seam erases `Id`; in v4 it *introduces* the carrier instead. Everything at the seam is ground: the
signature is a `GroundValue`, the type arguments are `GroundValue`s, the ambient carrier is fixed by the
platform run boundary (`row/RunBoundaryFunctions`, `SyntheticMainSourceProcessor`) or by the discharge
stack the declared row demands.

**Keyed by the stack, and demanded from the run boundary down** (P0, measured). A v4 mono key holds only
*payload* type arguments, and the payload key does **not** determine the carrier: one `{Console}` helper called
both inside and outside a `catch`-discharged region is two instances, with two different bodies and one payload
key (`docs/effects-v4-p0-spike.md` S2). So the weave is keyed `(vfqn, payload arguments, carrier stack)` — the
dimension `WovenValue`'s own scaladoc already reserves (*"weave key = mono key × stack"*) — and the lowering
runs as an ordinary demand from the run boundary downwards, each call demanding its callee *at the stack that
call runs on*. This costs no instance that is not already paid for: today's mono key carries the same stack
inside its type arguments. It terminates for the same reason today's monomorphization does — the value-reference
graph is acyclic (*Total by Default*) and each call's stack is the discharge stack of a finite row difference
over its caller's. The lowering also reads each callee's **declared row** sideways off
`ResolvedValue`/`OperatorResolvedValue` (keyed by `(vfqn, platform)`, not by the mono key): a read
`WovenValueProcessor` does not perform today.

**A stored computation takes its stack from its row, not from a base its type no longer has** (P0, measured). A
computation held in a `data` field is reached through the *type*, and producer and consumer share nothing else —
today the author writes the base into the pin (`{Throw[String] | Id}`) and the seam reads it off the field. A v4
computation type has no base, so the lowering computes one: the **canonical stack of a row** is its canonical
ability order lowered to carriers, over the pure base when the row rides no `Suspend`, and over the platform's
run carrier when it does. Both are ground at the seam. This extends §4's canonicalisation obligation from the
row to the stack the row lowers to, and it must be fixed in P1 with the canonical form — not discovered in P2.
Its two consequences are R7 and R8.

**What it writes.** Exactly what `RowElaborator` writes today — `flatMap` chains for sequenced effects,
`pure` at pure boundaries, the derived discharge stack at a call needing more than the ambient, captures
at suspended slots — but on ground types, so every decision it makes today from *declarations* it now
makes from *the actual instance*. The region trichotomy (`Spelled`/`Unspelled`/`Absent`), the
leading-type-argument-prefix rule, the pinned-parameter instantiation and the alias expansion all become
unnecessary: there is nothing to be unable to name.

**Demand direction (load-bearing).** The lowering introduces references — `flatMap@IO`, `pure@IO`,
`Console[IO]::printLine` — that monomorphization never demanded, because they never appeared in a type.
So the lowering must **demand mono of the machinery it inserts**: `WovenValue(f)` demands
`MonomorphicValue(flatMap, [IO, …])`. That direction is acyclic — `MonomorphicValue` never depends on
`WovenValue` — and it is the ordinary demand-driven pattern. It does mean **ability resolution for
effect methods moves into the lowering**: `Console[IO]` is selected there rather than by
`check/AbilityResolver` during checking. At the seam the carrier is concrete, so selection is a lookup,
not a search — but it is a relocation of real machinery and should be planned as one, not discovered.

**The re-check obligation.** Today elaboration runs *before* the checker precisely so the checker
validates the monadic core it emits. Move it after, and the emitted core is machine-generated and
unchecked — a silent-miscompile surface, which the *gaps must be fail-safe* rule does not permit. The
mitigation is cheap and must be part of the design, not a follow-up: **re-check the woven body**. It is
ground, so this is NbE evaluation plus definitional equality with no metavariables and no unification —
the cheapest form of checking this compiler has. Budget it, measure it, and keep it on by default;
`assertNoIdResidue` is the precedent for a mandatory tripwire at this seam.

**What it does not change.** Dischargers stay ordinary Eliot over monad transformers
(`runThrow`/`runState`/`catch`/`else`), abilities stay ordinary abilities, `IO` stays the platform's
`data`. v4 does *not* require building an effect-handler runtime or delimited control. The lowering
chooses the carrier stack from the declared row exactly as the elaborator does today.

## 7. Deleted, stays, added

**Deleted.**

| what | where | lines |
| --- | --- | --- |
| carrier minting, `{Effect}` reuse, open/pinned rewrite | `core/processor/EffectSugarDesugarer` | ~398 |
| the elaborator's slot classification, region carriers, alias expansion, discharge-stack derivation | `row/RowElaborator` | 1,919 (rewritten far smaller at the seam) |
| `Id` normalisation and its residue tripwire | `monomorphize/channel/IdNormalizer` + `WovenValueProcessor.assertNoIdResidue` | 310 + |
| the last effect rule in the checker | `monomorphize/check/EffectLifter.tryPureWrap` | in 342 |
| carrier-ness by name, and the inverter that undoes it | `effect/EffectCarrierNaming`, `effect/EffectRowRendering` | 140 |
| carrier binder recognition | `effect/processor/EffectCarriers`, `EffectMachinery` | 158 |
| `Effect` / `Suspend` machinery abilities, `Id` + `Effect[Id]` | `stdlib/eliot/eliot/carrier/`, `lang/eliot-compiler/…/Id.els` | — |

Also deleted, and worth naming because they are *states*, not code: the inferable carrier binder and its
`inferableArity` contribution; the pinned/open distinction; the `| G` tail syntax; the "stored row must
be pinned" rule; the type-alias limitation; the `Suspend`-riding-effects-cannot-be-pinned limitation; the
"handler reached through a declared carrier-typed parameter" limitation.

**Stays.** `EffectRow` as declaration metadata; `RowChecker`'s derivation and subset check;
`EffectAccountingProcessor` and its codegen-precondition wiring; the dischargers and `eliot.effect`; the
run-boundary registry; the `Inf` story; the compile-track `Either` discharge; every cornerstone.

**Added.** `Row` as a canonical value type + its canonicaliser; `Computation[r, A]` as a primitive type
former with its `unify` case; the seam lowering (with ability selection and machinery demand); the woven
re-check.

Honest arithmetic: this is not obviously a large net *deletion* of lines. It is a large deletion of
**interacting rules across seven phases** in exchange for **one pass at one seam with ground inputs**.
That is the claim to test, and §11's gates are written to test it.

## 8. Interactions

- **Layers.** `IO` stops being reachable from any user or stdlib signature; the platform contributes it
  to the lowering, not to the language. `SyntheticMainSourceProcessor` no longer instantiates a carrier
  by unification — it names the run boundary the lowering already knows.
- **Compile track.** The compile-time platform runs pure bodies; with effects out of the types it runs
  strictly less type-level effect machinery than today. The mid-spine default ladder and the deferred
  slot (`Track.Compiler`, the one live reader of the higher-kinded-meta record) exist for the inline
  guard's carrier — which, under v4, is not a carrier at all. **Candidate for deletion**; verify, do not
  assume.
- **LSP.** Hover shows the declared row and the payload type directly, with no inverter in between. The
  `TypeHintIndex`'s "Id-normalize first" tax disappears with `Id`.
- **Flow grades (`TODO.md`, "quantitative computation tracking").** The planned generalisation — the row
  becomes *"abilities + named grades"* (`{Timer, cycles: <=800}`), with the current row as the powerset
  special case (`seq` = `branch` = union, `within` = `subset`) — lands better here than on v3. Under v4 a row is
  already an ordinary value with an algebra, so a grade entry is a new *entry kind*, not a new mechanism;
  and the carve-out the sketch already anticipates (*"grade-only rows on pure functions do not force a
  carrier"*) stops being a carve-out, because no row ever forces a carrier — the lowering decides
  representation, and a row with no ability entries lowers to nothing. This is the one place where v4's
  cost is paid back by work that is already planned rather than by work that is already done.

- **Backends, and the microcontroller argument.** With the carrier internal, a backend is free not to
  build one: `Throw`/`Abort` are a branch, `State` is an extra in/out parameter, `Console` is a direct
  call. On an ATtiny that is the difference between an allocation per effectful step and none. v4 does
  not require this — the transformer lowering is the default — but v4 is what makes it *possible*, and
  today's design forecloses it. Explicitly out of scope here.

## 9. What must not happen (standing rules for this design)

1. **No row inference.** Rows are declared and written, never solved. A row metavariable, a row join, or
   an ordering-sensitive row decision means v4 has become v2 with new nouns.
2. **One spelling per row.** Canonical order, deduplicated, one rendering. The meta channel's `Bound`
   collapse is the precedent; do not rediscover it.
3. **No carrier before the seam.** If a carrier, a `flatMap`, or a `pure` appears in any fact upstream of
   `WovenValue`, the separation has leaked and the edge cases will come back with it.
4. **No unchecked lowering.** The woven re-check is part of the design, not an optimisation to defer.
5. **Stop on conflict** (v3 standing rule 2, inherited). If a step finds itself narrowing, bounding,
   deferring, approximating or exempting one of the four above, surface it rather than landing it.

## 10. Risks and open questions

- **R1 — the seam may not be late enough. CLEARED by P0 (2026-08-19), `docs/effects-v4-p0-spike.md`.**
  Measured over the real layers on the three shapes P0 names: every instance's carrier is fully ground at the
  seam (no `Param` residue anywhere), including a definition reached *only* through a stored computation. Two
  amendments fell out and are folded into §6 — the seam must be keyed by the carrier stack as well as by the
  payload arguments (the payload key alone does not determine the carrier), and a stored computation's stack
  must be computed from its row by a canonical rule rather than read off a base the type no longer has.
- **R2 — ability selection at the seam. SIZED (2026-08-19), `docs/effects-v4-p2-sizing.md` §2.** It is an
  **addition at the seam, not a move**: `check/AbilityResolver` (234 lines) is one of four post-drain
  collaborators and serves *every* ability, not only effect methods, so the checker keeps it and the seam
  gains a second, ground-carrier entry point — a lookup per stack, as P0 §4 measured. Budget the lookup plus
  §6's demand plumbing.
- **R3 — re-check cost.** A ground re-check per mono instance is cheap per instance but runs on every
  instance. Measure against the existing cold-build baseline (`--statistics`), not by estimate.
- **R4 — the empty row. AMENDED by P1 (2026-08-19): there *is* a second reason.** The first half holds —
  the empty row needs no representation at all, and `CanonicalStack.representation` hands back the payload
  untouched for it, so pure code lowers to itself. But a **non-empty** row that rides no `Suspend` lowers to
  a transformer stack, and its innermost transformer still needs a base monad to sit on: `ThrowCarrier[E, ?]`.
  That base is exactly what `Id` is, and removing the empty row's need for a carrier does not remove it. So
  §7's deletion line for `Id` + `Effect[Id]` is one line too broad: `Id` leaves the *language* (no user or
  stdlib signature names it, and no checker decision reads it) but stays as the pure base of the
  *representation* — a parameter of the lowering, which a backend lowering `Throw` to a branch rather than to
  a transformer (§8) supplies differently. `IdNormalizer` and `assertNoIdResidue` still go: what they erase is
  `Id` appearing in a *type*, which under v4 cannot happen.
- **R5 — diagnostics.** Rule 4's violations become `unify` mismatches, which is correct but must not read
  as `Expected: Computation[{Console}, String] / Actual: String`. The user vocabulary is rows and
  payloads; the renderer must say "this position may not receive a computation" as today.
- **R6 — flag day.** There is no partial state in which signatures both do and do not desugar to
  carriers. The checker, the elaborator and ability resolution move in one change. This is the single
  biggest practical objection to v4 and it does not have a mitigation, only a plan (§11).
- **R7 — a stored computation may need a hoist, which v3 never needs.** v3 answers "this callee must run on a
  bigger stack than it was written for" by *re-monomorphizing the callee at that stack* — the same `{Console}`
  helper woven twice (P0 S2). A **stored** computation cannot be re-monomorphized: its representation is fixed at
  construction by its canonical stack, so a consumer whose ambient differs needs a base hoist (`Id`-based to
  `IO`-based, or a stack embedded in a larger one). v3 never meets this because it *forbids the programs that
  would*: a `Suspend`-riding row cannot be pinned, and a pinned `{Throw[E] | Id}` forces its handler pure. v4
  admits them, so v4 must budget the hoist — either a per-carrier `hoist`/`mapBase` in the stdlib, or a rule that
  a stored computation is discharged at its canonical base (which is what today's programs do, and why P0's S3
  needs no hoist). **Sized, with a proposal, 2026-08-19** (`docs/effects-v4-p2-sizing.md` §4): no `hoist`/`mapBase`
  exists in the tree and no program needs one, so the proposal is the fail-safe rule — a stored computation is
  discharged at its canonical base, and a consumer whose ambient differs gets a hard error naming both, never a
  silent lift. A hoist is then written when a program asks for one, which is also when its semantics can be judged.
  The decision is Robert's.
- **R8 — canonicalisation fixes the stack order of a stored computation.** §3 rule 3's replacement says the order
  of interacting effects "lives in the term (handler nesting)". That holds at a *discharge* site and not at a
  *storage* site: a stored `{Throw[E], State[S]}` computation has no handler in the term where it is built, so its
  transformer order is whatever the canonical form says — a semantics today's author picks by writing the pin's
  order (`State` outside `Throw` keeps the state a raise discards; the other order does not). Either the canonical
  order is declared to *be* the semantics for stored computations, or the type must carry the order — and that
  decision must be made **before** the canonical form is fixed, since adding it afterwards is exactly the
  two-spellings trap of §4. Neighbour of Q2.
- **R9 — v4 deletes the only way a program can substitute an effect implementation, and nothing replaces it.
  Found by the readiness check (2026-08-19), `docs/effects-v4-flag-day-readiness.md` §2. STOP CONDITION under
  standing rule 5; the decision is Robert's.** `docs/testing-effects.md` is marked *adopted, done* and its
  mechanism is "the carrier **is** the injection point": the test declares its own carrier and its own instance,
  and the production code is instantiated at it. v4 removes all three legs — a user cannot name a carrier (§2 tier
  3, §8, §11 P5), cannot choose one (§6's canonical stack puts every `Suspend`-riding row on the platform's run
  carrier, and the run-boundary registry is compiler configuration), and so never has their instance queried. §5
  touches the strategy once, and only about accounting's `fixesCarrier` bound; "discharge it instead" is not
  available, because the `Suspend`-riding abilities have no carrier for a discharger to be written over and
  user-visible handlers are what §6 says v4 does not build. Six example programs and two integration test classes
  depend on the mechanism, so the flag day's own gate (`__.test` green, jars `md5sum`-identical) cannot be met
  while this is open. Three options are laid out in the readiness note §2.4; the recommendation there is to keep
  one explicit, import-required interpretation seam for tests and leave ordinary code carrier-free.
- **R10 — §7's deletion list retires the machinery that the lowering emits. Amendment, the second of R4's kind
  (2026-08-19), `docs/effects-v4-flag-day-readiness.md` §3.** The 16 `Effect`/`Suspend` instances in the layers
  *are* the representation the seam writes calls to, they are ordinary Eliot over carrier generics, and their `~`
  constraints are what lets their own bodies typecheck. So the machinery leaves the *language* (the `{Effect}`
  sugar and the 9 `~` constraints on ordinary `def` heads go) and stays as code; `eliot.carrier` can only be
  removed from *user scope*, which being import-required it already is. Same note records one unlisted work item:
  a native with a carrier-typed parameter (`foldLeftInternal[F[_] ~ Effect, A, B]`) means the lowering must lower
  leaf **signatures**, not only bodies.
- **Q1** — does anything besides accounting need a *definition-site* row certificate? If yes, `RowChecker`
  grows; if no, it shrinks.
- **Q2** — do two occurrences of the same effect in one row (two `State`s) need distinguishing? Today the
  pinned stack orders them structurally. In v4 the order lives in the *term* (handler nesting), which is
  the standard answer, but the stdlib dischargers must be read for a case that relies on the type. P0
  sharpened the question: "in the term" holds only where there *is* a term — a stored computation has none
  at its storage site (R8), so the answer must cover both.
- **Q4** — does the grade generalisation above want the grade in the *type* of a first-class computation
  (tier 2), or only in the channel? A stored computation's cycle bound is as much a part of its contract as
  its abilities are, which suggests the type; settle it before the canonical form is fixed, since adding an
  entry kind to a canonical value after the fact is exactly the two-spellings trap of §4.
- **Q3** — is `Computation[r, A]` genuinely primitive, or can it be `data` without reintroducing an
  encoding? Prefer primitive; record the reasoning either way.

## 11. Migration plan and gates

Each phase ends at a **measured** gate. The standing gate for the whole plan is v3's: `__.test` green and
every example jar `md5sum`-identical to the pre-change build, except where a phase's own note says an
output legitimately changes.

- **P0 — spike R1 only. DONE (2026-08-19), gate met.** Three shapes (a `{Console}` block, a `catch`
  discharge, a stored computation) measured at the `WovenValue` seam over the real layers; the note is
  `docs/effects-v4-p0-spike.md`, the measurement the permanent
  `jvm/test/…/EffectsV4SeamGroundnessTest.scala`. **Yes on all three**: S1 unconditionally, S2 with the
  seam key gaining its stack dimension, S3 conditional on the canonical row ⤳ stack rule — both folded
  into §6. Two new risks (R7, R8) came out of it; neither is a stop condition. The stored shape had to be
  measured in its v3-expressible form (a pinned row in a `data` field), a list element being a payload
  today.
- **P1 — `Row` and `Computation` in the type language. DONE (2026-08-19), gate met.**
  `GroundValue.Row` / `GroundValue.Computation` and their semantic twins `VRow` / `VComputation`, the
  canonicaliser (`monomorphize/fact/CanonicalRow`), the `unify` cases, both printers, a read-back that
  canonicalises unconditionally (so no producer can mint a second spelling), and the canonical row ⤳ stack
  rule (`row/CanonicalStack`). Nothing produces either former yet, so there is no behaviour change; the
  obligations are pinned by `lang/test/…/row/EffectsV4RowAndComputationTest.scala`. **Gate met:** `__.test`
  green, all 44 example jars `md5sum`-identical to the pre-change build. One amendment fell out and is
  folded into §10 R4 — the *empty* row indeed needs no representation, but the **pure base** it would have
  sat on does not disappear with it.
- **P2 — the seam lowering, behind a flag, output compared. AMENDED (2026-08-19): P2 cannot precede P4;
  they are one change.** Implement the lowering and compare woven output per `(payload key × stack)` — P0
  showed the payload key alone merges instances that differ. **Gate:** identical woven bodies on every
  example, which is the proof that the seam can *use* the information P0 showed it has. What the sizing
  measurement found (`docs/effects-v4-p2-sizing.md` §1): the lowering's input must be a *direct-style* body,
  and what reaches the seam today is the already-elaborated one, so running it "beside the existing
  elaboration" needs a monomorphized direct-style body — which is exactly P4's checker change. There is no
  scaffold in between. So P2 and P4 land together, under P2's gate, and R6 is stronger than it states.
- **P3 — the woven re-check. DONE (2026-08-19), gate met.** `monomorphize/channel/WovenRecheck`, wired into
  `WovenValueProcessor` beside `assertNoIdResidue` and mandatory like it. Ground definitional equality with
  no metavariables, no unification and no meta store: a function literal is `Function[parameterType,
  bodyType]`, an application applies a function whose domain is its argument's type and whose codomain is its
  own, a parameter reference has its binder's type, and a bodied value's signature is its body's type.
  **Landed on today's output, where it is a no-op** — it accepts the whole example and integration corpus
  unchanged. Two `used` fixtures had to be corrected: they injected hand-written monomorphic bodies annotating
  a lambda and an applied head with a non-function type, which is exactly what the check is for. **Gate met:**
  `__.test` green, examples byte-identical, and no measurable build-time regression (min-of-three warm
  single-example compiles: 5.43 s with the check, 5.91 s without — the check is below the noise floor of the
  measurement, so the stated budget is "not measurable at one example's granularity").
  One half is deliberately deferred to the lowering: a value **reference**'s agreement with its callee's own
  woven signature is the one rule needing a fact read, which would turn the seam into a callee-first traversal
  — and the `used` fixtures that exist to prove the codegen driver tolerates a *cyclic* injected value show
  that demand is not safe to add on its own. §6's demand direction gives the lowering that read for free, so
  the rule lands with P2 rather than being bolted on ahead of it.
- **P4 — flag day. Held (2026-08-19) pending R9, R10 and R7** — see
  `docs/effects-v4-flag-day-readiness.md`. Signatures stop desugaring to carriers; the checker, ability resolution for effect
  methods and elaboration move together; `EffectSugarDesugarer`'s carrier half, `EffectLifter`,
  `IdNormalizer` and the naming/rendering pair are deleted in the same change. **Gate:** green, examples
  byte-identical, and the diagnostics corpus re-read by hand (R5).
- **P5 — surface cleanup.** `| G` tails, the pinned/open distinction and the alias limitation go; the
  `eliot.carrier` package is removed from the path. **Gate:** green, plus the alias case that prompted
  this document compiling as an ordinary alias.

**Do not start at P1.** P0 is the only phase whose failure is cheap — and it has now run, so P1 is the
next phase to consider, not the first one to start unasked: the decision in §0/§11 is still Robert's.

---

## Appendix A. Provenance

- **A.1** This design was written on 2026-08-19 in response to Robert's question, following the
  investigation of the open-row type-alias limitation
  (`EffectSugarDesugarer.rowErrors(FunctionDefinition)`, `docs/effects-as-rows.md` A.11.13). The
  alias limitation is a symptom and is *not* by itself a reason to do any of this; the reason, if there
  is one, is §3 rule 4 and the table in §0.
- **A.2** The alternative considered and rejected in the same conversation: putting the row on `VPi`
  itself (`VPi(domain, codomain, row)`), Koka-style. It is feasible and does not add a sort, but it
  teaches every unification site, the printer and the `Function` native about rows — putting effects
  *into* the mechanism v4 exists to keep them out of. §4's arrow-with-computation-codomain gets the same
  expressiveness with no change to the Π-former.
- **A.4** **P0 was executed on 2026-08-19** — `docs/effects-v4-p0-spike.md`, with
  `jvm/test/…/EffectsV4SeamGroundnessTest.scala` as its permanent measurement. It measured the tree as it stands,
  by projecting each of today's monomorphic instances into the v4 form (payload key = mono type arguments minus
  the instance's own carriers) and asking whether that key still determines the carrier. It does not for a
  definition called at two stacks, which is what §6's stack-keyed seam answers; and a stored computation's stack
  comes from the base its author pinned, which is what §6's canonical row ⤳ stack rule replaces. Both amendments,
  and R7/R8, are P0's output, not part of the original sketch.
- **A.3** The narrower alternative that does not need any of this, if v4 is not taken: **row aliases** —
  alias the row rather than the row-plus-payload (`type Test = {Writer[List[TestCase]]}`, used as
  `{Test} Unit`), spliced into the ability-constraint list during resolution
  (`ValueResolver.resolveParamConstraints`). It reuses the existing lowering unchanged and closes the
  alias limitation on its own. It is the right change *if v3 stands*; it is wasted work *if v4 lands*.
  Deciding v4 first, either way, is therefore worth more than either change.
