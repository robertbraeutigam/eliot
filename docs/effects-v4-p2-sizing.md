# Effects v4: sizing the flag day (what P2/P4/P5 actually costs)

**Status (2026-08-19): a measurement, not a decision.** `docs/effects-as-channel-v4.md` §10 R2 says "size it
before committing" of the ability-selection relocation, R7 says the stored-computation hoist is "unsized", and
§11 gates P2 on both. This note supplies those numbers, measured against the tree at
`claude/big-bang-effects-v4-m1sfq9` (P0, P1 and P3 landed). It decides nothing — per `docs/effects-as-rows.md`
standing rule 1 the decision is Robert's — and per v4 standing rule 5 the one thing the measurement turned up
that the plan does not say is surfaced here rather than routed around.

## 1. The finding: P2 is not separable from P4

§11 sequences the plan P2 (the lowering, behind a flag, output compared) → P3 (re-check) → P4 (flag day). P2
as written cannot run on this tree, and the reason is structural rather than a matter of effort.

The lowering's input must be a **direct-style** body — that is the whole point: it writes the `flatMap` chains
and the `pure`s. What arrives at the `WovenValue` seam today is the *already elaborated* monadic body, because
v3 elaborates before the checker precisely so the checker validates what elaboration emits. So "run the
lowering beside the existing elaboration and compare woven output" needs a direct-style body that has been
monomorphized — and monomorphizing a direct-style body is exactly the checker change P4 is. There is no
scaffold that produces one: the checker cannot type a direct-style body without knowing effects are out of the
types, and the moment it does, the tree is past the flag day.

The consequence is that R6 ("flag day … does not have a mitigation, only a plan") is stronger than stated:
**P2's comparison gate cannot be met before P4, so P2 and P4 are one change, and its gate is P2's** (identical
woven bodies per `(payload key × stack)` on every example). P3 was landable ahead of them and has landed;
P1 was landable ahead of them and has landed. Everything else moves together.

## 2. What moves together, measured

**Eliot surface** (`stdlib/`, `jvm/`, `lang/`, `examples/`):

| what | count |
| --- | --- |
| `.els` files carrying an effect row | 87 |
| pinned rows (`{… \| T}`) to respell as bare computation types | 19 |
| `~ Effect` / `~ Suspend` machinery constraints to delete | 34 |
| files importing `eliot.carrier` | 43 |
| `Id` / `runId` mentions | 47 |
| carrier machinery modules to retire from the path | `eliot/carrier/Effect.els`, `eliot/carrier/Suspend.els` |

**Corrected 2026-08-19** by `docs/effects-v4-flag-day-readiness.md` §3: of the 34 `~` occurrences, 31 are in code
and only the **9 on `def` heads** go with the flag day. The 22 on `implement` heads sit on the carrier machinery and
the carrier-indexed effect instances — the representation the seam itself emits calls to — and stay, as does the
`eliot.carrier` package (it can only leave *user scope*, which being import-required it already has). Same note, §2:
the 6 example programs that declare their own carrier are the *fake-carrier testing strategy*, which v4 as written
removes without replacement (R9).

**Compiler** — the carrier-aware Scala, 4,313 lines in eleven files:

| file | lines | fate |
| --- | --- | --- |
| `row/RowElaborator` | 1,919 | rewritten as the seam lowering (§6: far smaller, ground inputs) |
| `row/RowChecker` | 570 | derivation + subset check stay; the carrier-binder reading of "declared" goes |
| `core/processor/EffectSugarDesugarer` | 398 | carrier minting deleted; row ⤳ `Computation` desugar replaces it |
| `monomorphize/check/EffectLifter` | 342 | deleted (§7) |
| `monomorphize/channel/IdNormalizer` | 321 | deleted (§7) |
| `monomorphize/check/AbilityResolver` | 234 | effect-method selection relocates to the seam (R2) |
| `monomorphize/check/CarrierKindChecker` | 231 | deleted with the carrier metas it verifies |
| `effect/processor/EffectCarriers` | 110 | deleted (§7) |
| `effect/EffectRowRendering` | 88 | deleted (§7) |
| `effect/EffectCarrierNaming` | 52 | *construction* half survives in `row/CanonicalStack`; the inverse goes |
| `effect/processor/EffectMachinery` | 48 | deleted (§7) |

**`EffectRow` threading**: 21 files read or forward it, of which 8 are pure fact-chain hops
(`FunctionDefinition` → `NamedValue` → `ResolvedValue` → `BlockDesugaredValue` → `MatchDesugaredValue` →
`OperatorResolvedValue` and their two converting processors). Those hops stay — v4 keeps `EffectRow` as
declaration metadata (§7 "Stays") — so the threading is *not* part of the cost; only its **readers** are.

**R2, sized.** Effect-method ability selection is not a separable subsystem: `AbilityResolver` (234 lines) is
one of four post-drain collaborators hooked from `TypeStackLoop.runPostDrainResolution`, and it serves every
ability, not just effect methods. Relocating *effect* methods means the seam gains a second selection entry
point over ground carriers (P0 §4 measured this is a lookup per stack, not a search) while the checker keeps
the resolver for everything else. So R2 is **an addition at the seam, not a move**: budget the new lookup plus
the demand plumbing (§6 "demand direction"), and leave `AbilityResolver` in place.

## 3. What the change buys, in the one place it is easiest to see

The stdlib dischargers are where v3's encoding is most visible, and where v4 is most obviously simpler. Today
every one of them carries a carrier binder that exists only to have somewhere to put the row:

```
def runAbort[G[_], A](obj: {Abort | G} A): G[Option[A]]
def catch[E, G[_] ~ Effect, A](computation: {Throw[E] | G} A, onError: E => {Effect} A): G[A]
def runStateToPair[S, G[_], A](initial: S, p: {State[S] | G} A): G[Pair[A, S]]
```

Under v4 the row is the type and the ambient is the channel's business, so `G` has nothing left to do:

```
def runAbort[A](obj: {Abort} A): Option[A]
def catch[E, A](computation: {Throw[E]} A, onError: E => A): A
def runStateToPair[S, A](initial: S, p: {State[S]} A): Pair[A, S]
```

Nineteen pinned rows and thirty-four machinery constraints disappear the same way. This is the §0 table's
claim made concrete: none of what goes is about *which effects a program performs*.

## 4. R7, R8, Q2 — proposals, for a decision that is not this note's

- **R8 — settled by P1, and recorded.** `row/CanonicalStack` fixes the canonical ability order as the
  transformer nesting of a stored computation: leftmost canonical entry outermost, matching the v3 pinned-row
  order the dischargers are written in. So a stored `{Throw[E], State[S]}` has one semantics, chosen by the
  canonical order rather than by the author. The alternative — putting the order in the type — was rejected as
  a second spelling axis (§4). If that is the wrong trade, it must be revisited **before** anything produces a
  row, because adding it afterwards is the two-spellings trap.
- **Q2 — answered by P1.** Two occurrences of one ability at *different* arguments are two entries, ordered by
  the same canonical key; at *identical* arguments they deduplicate. So `{State[S], State[T]}` is expressible
  and ordered, and `{Console, Console}` is `{Console}`.
- **R7 — still open as of the 2026-08-19 readiness check** (`docs/effects-v4-flag-day-readiness.md` §4), which
  recommends adopting the proposal below as written. **Proposed rule: a stored computation is discharged at its
  canonical base, and a mismatch is a hard error, not a silent lift.** No `hoist`/`mapBase` exists in the tree
  today and no program needs one (P0's S3
  discharges at its canonical base and lifts only the pure result). Adding a per-carrier hoist to the stdlib
  is real work with no current caller; the fail-safe alternative costs nothing and cannot miscompile — a
  consumer whose ambient differs from a stored computation's canonical base gets a diagnostic naming both.
  A hoist can then be added when a program asks for one, which is also when its semantics can be judged.

## 5. Proposed ordering, if the flag day is taken

1. **Surface + core.** `{…} A` desugars to `Computation(Row(…), A)`; the `| T` tail parses but is rejected
   with a message pointing at the bare form; no carrier binder is minted. Everything downstream breaks — this
   step is not independently green and must be developed with 2 and 3.
2. **Checker.** `Computation` in `TypeReference` ⤳ `VComputation`; a call to a rowed callee has its *payload*
   type; `EffectLifter`, `CarrierKindChecker` and the carrier metas go.
3. **Seam lowering + machinery demand + stack-keyed weave key**, with P3's reference-agreement rule (the half
   deferred there) landing as part of it, since the lowering performs the read anyway.
4. **`.els` rewrite** across the 87 files, dischargers first (§3 above is the pattern).
5. **P5 surface cleanup** — the alias case that prompted the whole design compiles as an ordinary alias.

The gate for 1–4 is one gate, and it is P2's: `__.test` green and every example jar `md5sum`-identical, with
the woven bodies compared per `(payload key × stack)`.
