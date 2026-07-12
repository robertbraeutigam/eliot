# Generic Refinement Merges — Removing the `Bool::fold` Dependency

**Status: DONE.** All steps landed and green (2026-07-12). The **whole refinement/meta machinery names no branch
construct** — the durable invariant (§4) is fully met. `fold`'s merge is computed generically by reducing its
`^Meta` companion (channel, Steps 2–3), and the reconcile pass re-encodes branch arms from channel-provided
**join-input edges** it finds by a structural read of the generic companion (Steps 4–5) — `boolFoldFqn` is gone
from both `RefinementChannelProcessor` and `ReconcileProcessor`. The only remaining `Bool::fold` references are the
sanctioned **realization/reduction** sites (`SystemNativesProcessor` reduce, `PostDrainQuoter` branch-select,
`Intrinsics`/`ExpressionCodeGenerator` inline emit), exactly as §1 allows. Continues the bounds-as-refinements work
(`docs/bounds-as-refinements.md`, Step 8 — backend width selection).

**Follow-on cleanup — the `metaOf` intrinsic is deleted, meta is total (2026-07-12, commit `64f5f1ee`).** The
`metaOf(a: Type): Type` intrinsic (below, Step 2 "as built") was the only place a generic `^Meta` companion could
name "the meta of a type parameter". It is gone, replaced by two moves: (1) the channel reduces a generic companion
at the **meta** type arguments (`RefinementChannelProcessor.metaTypeOf` maps each base type arg to its meta type),
so a bare `A` param binds straight to the meta type — no `metaOf(A)` in the signature; a *monomorphic* vessel still
just suffixes its concrete params to `T$Meta`. (2) Meta is **total**: `metaTypeOf(T)` is `T$Meta` if `T` is slotted,
else `Unit`, and `Unit` declares the trivial `Meta[Unit]`, so a merge over untracked arms lands on a real instance
(⊤) instead of a stuck non-existent `T$Meta`. `fold`'s dead `condition: Bool` stays verbatim (unprojected, so no
`Bool$Meta` needed). *Why the channel supplies `Unit` rather than a generated `type T$Meta = Unit` per slotless
type:* that alias is **concrete**, and base types are declared in more than one layer, so per-layer generation
collides ("Has multiple implementations") — a single channel-side rule avoids the clash. `CACHE_VERSION` 13 → 14.
The `metaOf`/`Bool[]`-namespace mentions in the historical Step-2/§Current-state notes below are retained for
provenance but describe deleted mechanism.

**Step 5's second half — RESOLVED by moving all width policy to the backend (2026-07-12, commit `253b7285`).** The
original plan fretted that dropping `widthTransparentLeaves` would regress arithmetic to ⊤/bignum unless natives could
declare *parameter*-metas. That framing was wrong: it assumed the reconcile pass owns conversion *placement*. It does
not need to. The JVM backend already *recovers* each `Int`'s width from its range (`IntRepresentation`); it can equally
*derive* every re-encode from the ranges + descriptors — a call argument widened to its ⊤/bignum parameter boundary, a
`fold` arm re-encoded to the fold node's merged width, the method body widened to the return boundary. So conversion
placement moved into the backend, and the reconcile pass became a **pure meta-stamper**: no `Reconcile` nodes, no
`widthTransparentLeaves`, no join-input edges (the whole structural-companion read of Steps 4–5 is deleted — the
backend, which already emits `Bool::fold` inline, re-encodes the arms from the fold node's recorded merge interval).
Arithmetic's narrow fast path survives with **no** exemption list, because `generateIntrinsic` reads each operand's own
meta directly — with no pass widening operands, there is nothing to exempt. No regression, and `~300` fewer lines.
`CACHE_VERSION` 14 → 15. This closes the last open follow-on; parameter-meta declarations are no longer needed for it.

**The one design correction discovered while landing Step 3 (2026-07-12).** The plan (below, and its §4½) assumed
`reduceInstance(fold^Meta, [Int])` would *stall at a stuck `Meta::join`*, from which the channel would read the two
join inputs and compute the join explicitly. **It does not.** Empirically the auto-derived `Meta[Int$Meta]`
instance **dispatches** — its arguments are `Int$Meta` *values* (not a type-position `Interval`), so value-level
ability dispatch fires (unlike the Step-4c `Numeric[Interval]` case, which was reached in a *type* position). So
`fold^Meta` reduces all the way to `Int$Meta(intervalJoin(range(whenTrue), range(whenFalse)))` and the quote fires
`intervalJoin` to yield the join — **mechanically identical to a transfer companion**. The consequence: the merge
and transfer paths *unified* (the channel just reduces any `^Meta` companion and reads back the result interval;
`mergeViaCompanion` == `applyMetaCompanion` modulo generic type args and the ⊤ condition slot), which is *simpler*
and more cornerstone-aligned (one evaluator, name nothing) — **but** the dissolved `Meta.join` leaves no trace of
*which* arguments were join inputs. That join-input information is exactly what Steps 4–5 need (to reconcile a
branch arm to the merged representation), so it must be recovered by a **separate structural read** of the generic
companion body (recognising the one primitive `Meta::join` there, before monomorphization dispatches it) — *not*
from the reduction. This supersedes §4½ item 2's "stalls at the stuck `Meta::join`" mechanism.

**Landed so far (2026-07-12):**
- *Phase 2 (Steps 2, 3) — the merge goes generic.* **Step 2 auto-derivation:** `MetaConstructorDesugarer` now emits,
  beside the `Int$Meta` meta structure, the field-wise `implement Meta[Int$Meta] { def join(a, b) =
  Int$Meta(intervalJoin(range(a), range(b))) }` — the user declares `Meta` only for their own domain, the compiler
  derives the compound meta-structure instance (each slot joined by its domain's plain join function, `Interval` ⤳
  `intervalJoin`). Unblocked `reduceInstance(fold^Meta, [Int])`. **Step 3 channel switch:** deleted
  `boolFoldFqn`/`walkBranch`/`runJoin`/`applyIntervalInstance`/`metaJoinFqn` from `RefinementChannelProcessor`; a merge
  is now `mergeViaCompanion` (reduce the callee's `^Meta` companion on the arg metas, read back the result interval).
  The `RefinementTable` is behaviour-preserving (a fold node still records the join of its arms), so the reconcile
  pass (still `boolFoldFqn`) is unaffected. lang/jvm/LSP suites green. Commit `90101d3d`.

**Earlier (2026-07-12):**
- *Phase 1 (Steps 1, 6, 7).* The backend consumes `ReconciledMonomorphicValue` (stage B); `IntRepresentation`
  (jvm) decodes an `Interval` meta → machine width, replacing the Eliot `Represent[Interval]` fold; the backend
  reads Int widths from each node's channel meta and honours `Reconcile` nodes for boundary re-encodes (stage X);
  the Phase-3 `RepresentationLowering` pass + `RefinementRepresentation` + both `Represent.els` are **deleted**
  (`Int` is no longer lowered — a bare `Int` descriptor maps to `java.math.BigInteger`, narrow body nodes decode
  from the meta; stage Y). `FactCache.CACHE_VERSION` 11 → 12.
- *Phase 2 foundation (Step 2).* The **`metaOf(a: Type): Type` intrinsic** (`WellKnownTypes.metaOfFQN` +
  `SystemNativesProcessor`; declared in `eliot.compiler.Meta`), the **generalized `MetaTransferDesugarer`**
  (generics kept, every companion param/return typed `metaOf(T)`, structure-producing body for a type-parameter
  return), `fold`'s merge brace in `stdlib/eliot/eliot/lang/Bool.els`, and the plain `intervalJoin` overlay
  function. `fold^Meta[A](t: metaOf(A), …): metaOf(A) = join(t, f)` now **resolves**; arithmetic and fold
  programs build and run. The channel does **not** yet consume `fold^Meta` — it still uses the `boolFoldFqn`
  special-case (kept green). Commits `91920a0a`, `007dfffc`.
- *Remaining (this doc §Current state).* Auto-derive `Meta[Int$Meta]` → repoint the channel onto `fold^Meta`
  (code written) → generic reconcile.

## 1. The premise this corrects

The refinement machinery currently treats `Bool::fold` as **the** way to branch: the channel special-cases
its FQN to compute the join, the reconcile pass special-cases it to merge arms, and its apidoc literally says
it is "the only way to branch on an opaque `Bool`." That premise is false and it is the root of the hardcoding:

- `fold` is **one** data-type eliminator, no more inherent than a user's `foldEither` or `foldList`. The
  language's genuine case-analysis primitives are **value match** (`handleCases` / `PatternMatch`) and **type
  match** (`typeMatch` / `TypeMatch`); every user eliminator desugars down to those. `Bool::fold` is a
  primitive intrinsic only because `Bool` is opaque (platform-independence forbids a base `data Bool`) — it is
  morally a value-match on a two-constructor `Bool`.
- More fundamentally, a **refinement merge is not a syntactic branch at all**. It is wherever the domain's
  `Meta.join` combines the metas of alternative control paths. That can be `fold`, a narrow-armed `match`, or a
  primitive nobody has written yet — the machinery must not enumerate constructs.

### The distinction to preserve

Using `Bool::fold` **by name to realize it** — the backend emitting it as inline branch code
(`ExpressionCodeGenerator.generateBoolIntrinsic`, an `IFEQ` over the taken arm), and the compile-time native
that *reduces* it when its condition is constant (`SystemNativesProcessor`, `PostDrainQuoter`) — is **fine**.
That is the native's realization/reduction, exactly like `nativeAdd` being emitted as `LADD`. What is wrong is
the **refinement/meta logic** naming `fold` to decide "a merge happens here." After this plan, the compiler's
refinement machinery recognizes exactly one primitive — `Meta.join` — and names no branch construct anywhere.

## 2. The principle

**A native declares its meta-behaviour; the compiler reads the declaration, never the opaque body.** A native
that moves refinement information carries a `^Meta` companion (a return-brace, desugared by
`MetaTransferDesugarer`) written in the **domain's** vocabulary:

- a **transfer** — a fresh result meta computed from operand metas: `nativeAdd`'s `{intervalAdd(range(a), range(b))}`;
- a **join** — a merge of alternative metas via the domain's `Meta.join`: `fold`'s `{join(whenTrue, whenFalse)}`.

**Absence of a `^Meta` companion is the sound default, not a special case.** A native with no companion has a
⊤ result (unknown range → bignum layout) and its arguments cross a boundary (reconcile to ⊤). Every IO leaf,
`printLine`, `intToString`, `intLessThanOrEqual` — the vast majority — declare nothing and get ⊤. Omitting a
companion can only ever cost *precision* (a wider representation), never *soundness* — matching the "gaps must
be fail-safe" rule. Only the handful of range-moving natives opt in.

**The compiler recognizes exactly one primitive: `Meta.join`** — the merge method of `ability Meta[D]`, which
the *domain author* defined. It is not a language construct; recognizing it is inherent to having a refinement
domain at all. `fold`'s arms are join-inputs *because `fold^Meta` feeds them to `Meta.join`*, detected the same
way a future selector's would be.

**Join vs transfer is per-edge, not per-value.** A node whose meta is an **input to a `Meta.join`** reconciles
to the join result; every other node reconciles to its consumer's expected meta (⊤ today). A *transformed*
value is still a join-input at the next edge: in `f(if c then (x+1) else 2)`, `x` is transferred at `+1`,
`x+1` is a join-input at the branch, and the branch result is a boundary arg at `f`. The classification is
about the immediate edge, never the value's lineage.

## 3. Plan

### Step 1 — Correct the false premise (apidoc) — DONE (commit 1da6bb75)

Rewrite the two comments that assert `fold` is "the only way to branch": `WellKnownTypes.boolFoldFQN` and
`SystemNativesProcessor`. State that `fold` is *an* eliminator over an opaque `Bool` (value/type match are the
case-analysis primitives; user eliminators desugar to those), and that this FQN is used only to **reduce** and
**emit** `fold`, never as a refinement-merge signal — merges are detected via `Meta.join` (this doc). Give
`fold` a correct `/** */` in `stdlib/eliot/eliot/lang/Bool.els` while here. *Landed:* all three comments
corrected; no behavior change.

### Step 2 — Author `fold`'s `^Meta` companion — FOUNDATION DONE (commits 91920a0a, 007dfffc)

The native's owner (the stdlib author) declares `fold`'s merge behaviour beside `fold`, in Eliot:

```
fold[A](condition: Bool, whenTrue: A, whenFalse: A): A { join(whenTrue, whenFalse) }
```

It lives on the stdlib **runtime track**, co-located with `fold` in `stdlib/eliot/eliot/lang/Bool.els` — a
return brace added to the existing `fold` declaration. `Bool` is an ordinary runtime library type; it is
*borrowed* into the compiler pool (the pool scans the whole runtime track), never re-declared on a compiler
track. The `^Meta` companion is compiler-pool-only **by virtue of the `Qualifier.Meta` namespace it desugars
into** — not because of where its source lives; in the runtime pool the companion is dead (never demanded),
exactly like every `rangeAdd^Meta`. This is the arithmetic precedent verbatim: `rangeAdd`/`rangeSubtract`/
`rangeMultiply` sit on the runtime track in `stdlib/eliot/eliot/lang/Int.els`, and their braces already reach
compiler-pool-only names (`intervalAdd`, the `range` slot) from there — a brace naming `Meta.join` needs no
different home. Those names resolve because their *modules* (`eliot.compiler.Meta`, `eliot.lang.Interval`)
exist in both pools and gain their overlay declarations in the compiler pool; the `fold` vessel imports them
just as the overlay `Interval.els` imports `eliot.compiler.Meta` today. (The compile-time `Either`/`Guard`
bodies live under `eliot-compiler/` for an unrelated reason — they are *self-sufficient compile-time
redefinitions* with real `data`/bodies that must not depend on a sibling runtime target being present; an
inert `^Meta` carrier is nothing like that, so the analogy does not apply here.) The merge *contract* ("the
result's range is the join of the arms' ranges") is **platform-independent** — true on every target — even
though `fold`'s *runtime* is realized per platform; so it belongs in the base once, exactly as the `rangeAdd`
brace is base stdlib while `nativeAdd`'s runtime is jvm-specific.

**Domain-polymorphic vessels — resolved: the compiler derives the compound `Meta`.** The arithmetic vessels
are Int-monomorphic: `rangeAdd(a: Int, b: Int): Int`, whose brace projects the `range` slot and rewraps —
`Int$Meta(intervalAdd(range(a), range(b)))`. `fold` is `fold[A]`, so one generic vessel must join over **A's
meta without naming a slot**: `fold[A](whenTrue: A, whenFalse: A): A { join(whenTrue, whenFalse) }`, whose
companion is `fold^Meta[A](whenTrue: A$Meta, whenFalse: A$Meta): A$Meta = join(whenTrue, whenFalse)` — `join`
over the whole meta-structure `A$Meta`, not a slot. For that to be well-typed, `Meta` must be available on the
**meta-structure** (`Meta[Int$Meta]`), whereas the *user* writes `Meta` only on the **domain** (`Meta[Interval]`).

The resolution (decided 2026-07-12): **the user provides `Meta` for their own domain; the compiler
auto-derives the `Meta` instance for a compound meta-structure field-wise** — `Meta[Int$Meta].join(a, b) =
Int$Meta(join(range(a), range(b)))`, delegating each slot to its own domain's `Meta` (here `Meta[Interval]`).
This is exactly parallel to how `MetaConstructorDesugarer` already auto-derives the meta *constructor* from a
type's fields; the derivation adds the matching *instance*. So "one `Meta` instance per domain" is preserved
for everything a **user** writes — the per-`T$Meta` instances are compiler-generated, not hand-written — and
`fold`'s companion is genuinely generic across every tracked domain (an `A` with no tracked slots derives the
trivial `Meta[Unit]`-shaped pass-through, so its arms are ⊤). `MetaTransferDesugarer` must learn to emit this
**structure-producing** companion shape (the brace *is* the result `A$Meta`, so it is not re-wrapped in the
meta constructor the way a slot-producing arithmetic brace is). This is Phase 2's one genuinely new mechanism;
the rest of Steps 3–5 is deletion of the `Bool::fold` special-casing.

**As built (2026-07-12) — and the two things the design under-specified.** The companion cannot be typed with a
plain `A$Meta` name transform, for a reason the earlier prose glossed: `A$Meta` for a **type parameter** `A` is
not a real type (nothing declares it), and even a **concrete untracked** type has none — `fold`'s `condition:
Bool` has no `Bool$Meta` (only *tracked* types with a `{…}` brace get a `$Meta` structure). So the desugar was
built on a new **`metaOf(a: Type): Type` intrinsic** (`SystemNativesProcessor`: rename the head FQN, suffix
`$Meta`, drop type args; declared in `eliot.compiler.Meta`): *every* companion parameter/return type is
`metaOf(T)`, which always **resolves** (`metaOf` and `T` are ordinary declared names) and **reduces** to `T`'s
`$Meta` structure only when `T` is concrete. The slot-producing *body* still uses the plain `T$Meta` **value
constructor** (e.g. `Int$Meta(intervalAdd(…))`), which exists for a tracked type. Two learnings worth keeping:

- **Type-namespace gotcha.** `metaOf(Bool)` fails: a bare uppercase name in a *value-argument* position resolves
  in the value namespace and misses a `type` with no value constructor. The fix is the "[] = type-namespace
  marker" gotcha — emit `metaOf(Bool[])`, empty type-arg brackets forcing `Bool` into the Type namespace.
  (Prelude `Int` happened to resolve without it; `Bool` — not a prelude, only a local `type` — did not.)
- **The auto-derived `Meta[Int$Meta]` is *required*, not an optimisation.** The channel recognises the merge by
  **reducing** `fold^Meta` (`ReducedBindingClosure.reduceInstance(fold^Meta, [Int])`, §3 as built), which
  *monomorphizes* the companion — so its `join` demands a real `Meta[Int$Meta]` instance and hard-errors ("No
  ability implementation found for `Meta[Int$Meta]`") without one. It cannot be side-stepped by reading the body
  "statically". This auto-derivation is therefore the **one remaining blocker** for the channel switch (below).

### Step 3 — Channel: uniform `^Meta`-driven meta + join-input detection — WRITTEN, blocked on the Step-2 auto-derivation

Delete the fold special-case in `RefinementChannelProcessor` (`walkBranch`/`runJoin`, the `boolFoldFqn`
constant). Every meta-affecting native computes its result meta by evaluating its `^Meta` companion — as
arithmetic already does — with no per-native branch. Detect **join-inputs** by recognizing `Meta.join` in the
companion: the argument positions the companion feeds to `Meta.join` are the join-inputs; record them together
with the join result. This is a one-time static read of each native's (Eliot) `^Meta` body, recognizing the
one domain FQN `Meta.join`, never a construct list.

**As built (`mergeViaCompanion`, written and saved, not yet committed).** The mechanism is **reduce, then
recognize a stuck `Meta.join`** — *not* a static AST read (a static read would be a second, redundant way to
inspect a companion, and the reduction is already how transfers work). At an ordinary call the channel:
(1) a cheap `UnifiedModuleNames` membership test for the callee's `^Meta` name (so a companion-free call costs one
cached lookup); (2) `ReducedBindingClosure.reduceInstance(<callee>^Meta, calleeTypeArgs)` applied to the
arguments' metas — a known `Int` range wrapped `Int$Meta(Interval(..))`, an unknown/non-`Int` argument a `VType`
⊤ placeholder its `Meta.join` cannot fold; (3) `force`. A **merge** reduces to a stuck
`SemValue.VStuckNative(Meta::join, [a, b])` — a *transitively*-reached instance does not dispatch under the
channel's NbE (the Step-4c lesson), so `Meta.join` stalls exactly here — and its two spine arguments are the join
inputs; the interval join is their `runJoin` (the domain's `Meta[Interval]`). A **transfer** reduces to a
concrete meta (not stuck at `Meta.join`) and is not a merge. This drops `boolFoldFqn` and `walkBranch` from the
channel entirely; `runJoin`/`Meta[Interval]` stay. The code is preserved at
`$CLAUDE_JOB_DIR/tmp/RefinementChannelProcessor.mergeViaCompanion.scala`; it was reverted only because
`reduceInstance(fold^Meta, [Int])` hard-errors until `Meta[Int$Meta]` exists.

*Not fully uniform yet (a documented seam).* The **arithmetic** leaves keep their explicit
`nativeAdd → rangeAdd^Meta` mapping (`isArithmeticLeaf`); only the *merge* path went generic. Naming an arithmetic
*leaf* is fine — the invariant forbids naming a *branch construct*, and `nativeAdd` is not one. Re-homing the
arithmetic transfer to a `<callee>^Meta` lookup (so the channel names no leaf either) is a clean follow-up, not
required for the invariant.

**Restriction the detection must state (fail-safe).** Mapping a `Meta.join` argument back to a *caller* arg
position works only when that argument is a projection of the companion's own parameter (`join(range(whenTrue),
range(whenFalse))` → `whenTrue`/`whenFalse`). A companion that computes something non-trivial before `join`
(`join(transfer(a), b)`) has no clean position preimage. Such a companion is **not** a join-input source: it
falls back to ⊤ for the un-mappable positions rather than guessing a mapping — a silent wrong preimage would
violate the "gaps must be fail-safe" rule. `fold`'s companion is the clean case (bare parameters under one
projection), so this restriction costs nothing today; it must be written down before a second join-native lands.

### Step 4 — `RefinementTable` carries generic metas

Change `RefinementTable.NodeInterval(position, min, max)` to carry a **generic `GroundValue` meta** per node,
plus the **join-input edges** (arm position → the join result meta). The channel already builds the
`GroundValue` (`Int$Meta`/`Interval`) internally before decoding to `(min, max)`; store the `GroundValue`.
Adapt consumers: the LSP hover (`TypeHintIndex`) decodes the meta `GroundValue` back to a `[min,max]` for its
value-range hint — the Int-specific decode lives in the consumer, which is the right home for domain knowledge.

**Cache/serialization to confirm first.** The table is deliberately first-order today (`position, min, max`)
*because facts must serialize for the incremental cache*, and equality across instantiations must be
well-defined. A `GroundValue` is a quoted semantic value; before adopting it as the stored meta, verify it
serializes and that structural equality on it is stable across instantiations (two `Interval(0, 5)` from
different call contexts must compare equal). If a raw `GroundValue` does not round-trip, store a serializable
normal form (e.g. the domain author's own encoding) and keep the decode in the consumer. Bump
`FactCache.CACHE_VERSION`.

### Step 5 — Reconcile pass fully generic

Remove `widthTransparentLeaves`, `boolFoldFqn`, and all `Interval`/`GroundValue`-construction from
`ReconcileProcessor`. The rule becomes purely mechanical and names nothing:

> Reconcile each **join-input** (channel-provided) to its join result; reconcile every other call argument and
> the return to its consumer's expected meta (⊤ today). Copy metas through opaquely; compare by structural
> equality.

The pass no longer distinguishes `fold` from any call, no longer excludes arithmetic operands (they reconcile
to ⊤ like any consumed arg — the JVM `long` fast-path is given up on boxed values, recovered generically by
any backend that declares narrow parameter metas), and never constructs or inspects a domain meta.

**This is a deliberate, deferred precision loss — state it as such.** The "recovered generically by any backend
that declares narrow parameter metas" path does **not** exist yet: no native declares narrow *parameter* metas
today (only returns), so post-Step-5 arithmetic operands lay out at ⊤/bignum where the special-case previously
kept a `long`. This is sound (⊤ is always representable) and matches the plan's "precision, never soundness"
stance, but it is a real regression on hot integer code until parameter-meta declarations land. Acceptable
because it trades a hardcoded fast-path for a uniform mechanism; not acceptable to ship silently — call it out
in the Step-5 commit and gate it on `ExamplesIntegrationTest` still *running* correctly (representation-
transparent, so runtime output is unchanged even as widths widen).

### Step 6 — Backend width selection ("the jvm pre-processor") not hardcoded — DONE (2026-07-12)

*Landed* (commits: stage B backend-consumes-reconciled-body; stage X meta-driven widths + Reconcile; stage Y
un-lower + delete). `IntRepresentation` (jvm/asm) decodes an `Interval` meta → `Jvm*` width; the backend reads
every Int width from the node's channel meta (`repInternalNameOf`), threads `expectedResultMeta` for arithmetic/
`fold` results, and emits the width re-encode at explicit `Reconcile` nodes (dropping the implicit
`widenIntArgToBigInteger` / fold-arm conversions). `NativeType` maps a bare `Int` → `java.math.BigInteger` for
boundary descriptors; the `Jvm*` types stay an internal backend vocabulary. Key subtlety found: the reconcile pass
wraps the body top with the (bignum) return type, so a top-level literal's *width must come from its meta, not its
`expressionType`* — a trailing `Reconcile` re-encodes to the boundary. `generateBoolIntrinsic` keeps emitting
`Bool::fold` inline (realization). Verified by the JAR-running `ExamplesIntegrationTest`.

Original design note: `IntRepresentation` (jvm/asm): decode the generic meta (an `Interval` `GroundValue`) → machine width; the
`Jvm*` types become an **internal backend enum**, not Eliot types. `effectiveRepType(node)` drives stack
widths from the node meta; add a `NativeType` `Int → java.math.BigInteger` entry for descriptors (since `Int`
stops being lowered). Lower a `Reconcile` node as `convertRepresentation(rep(source.meta), rep(node.meta))`.
The Int-specific meta decode lives here — the backend is Int/JVM-specific by nature, the correct home for
domain+platform knowledge. `generateBoolIntrinsic` **keeps** emitting `Bool::fold` inline (realization; see
§1).

**Why the decode belongs in the backend (superseding bounds-as-refinements Step 3).** Bounds Step 3 put the
`interval → machine width` decision in Eliot as `Represent.layout`, on the principle "representation derived in
Eliot, not Scala." That principle is right about the *platform-independent* half and wrong about this half. The
sharper line: what stays in Eliot is the domain and its **platform-independent** semantics — the `Meta` lattice,
the `^Meta` transfer/join vessels, the fact that `add`'s range is `intervalAdd` of the operands' ranges (true on
every target). Which *machine width* an `Interval` maps to is **not** platform-independent — `JvmByte`/`JvmShort`
are JVM artifacts, and an ATtiny backend would decode the same interval to different widths. `Represent.layout`
expressed that platform-specific choice *as an Eliot instance that already lived in a jvm compiler overlay*
(`jvm/eliot-compiler/…/Represent.els`) returning a `Type` value — Eliot syntax around knowledge that was jvm's
all along. Deleting it and decoding in `IntRepresentation` puts platform-specific policy in the platform's
backend, where it belongs, while the platform-independent domain logic stays in Eliot. So this is a *correction*
of the layering, not an abandonment of "derive from the range" — the width is still derived from the interval,
just by the consumer that owns the target. **Follow-through:** update `docs/bounds-as-refinements.md` Step 3 and
the `feedback_minimize_scala_decompose_in_eliot` memory to record that representation *policy* splits — domain
semantics in Eliot, target width decode in the backend — rather than reading as an unqualified "representation in
Eliot."

### Step 7 — Deletions (representation policy relocates to the backend) — DONE (2026-07-12)

*Landed* (stage Y). Deleted `RepresentationLowering`, `RefinementRepresentation` (+ its test), and both
`Represent.els` (the empty `jvm/eliot-compiler/` root is gone); `MonomorphicUncurryingProcessor` no longer lowers
(types stay concrete); `CodegenProjection`'s width-collapse key is now the erased carrier
(`erasedCarrier`, since a post-flag-day `Int` is nullary — its width is channel meta, not a type argument);
the dead `intTypeFqn` / `lowerUncurried` / `lowerExpression` / `unambiguousIntervalsByPosition` removed. The
`Bool::fold` FQN still lives only in `SystemNativesProcessor` (reduce), `PostDrainQuoter` (branch-select), and
`ExpressionCodeGenerator`/`Intrinsics` (inline emit) — realization/reduction only, no refinement-merge naming
yet **except** the not-yet-removed channel/reconcile `boolFoldFqn` special-case (Steps 3/5). `CACHE_VERSION`
11 → 12.

Original design note: with the width decode moved into `IntRepresentation` (Step 6), the Eliot-side representation machinery is dead,
not merely duplicated — this is the relocation argued in Step 6, not a cleanup. Delete `Represent.els` ×2 (the
ability + the jvm overlay instance), `RefinementRepresentation`, `RepresentationLowering`, and the
`representationOf` lowering in `MonomorphicUncurryingProcessor`; replace `CodegenProjection`'s
`representationOf`-based width-collapse key with the erased carrier (post-flag-day `Int` type-args are nullary,
so `Int` instances already collapse). Drop the duplicated `boolFoldFqn` constants (channel, reconcile). After
this the *only* references to `Bool::fold` by name are: the compile-time reduction native
(`SystemNativesProcessor`), the compile-time branch-select (`PostDrainQuoter`), and the backend inline emission
(`ExpressionCodeGenerator`/`Intrinsics`) — all **realization or reduction**, none "branch detection." Update
`docs/bounds-as-refinements.md` (Step 3's `Represent` rows and successor map) in the same commit so it does not
still describe `Represent.layout` as the live representation path.

## 4. The durable invariant

- The compiler's **refinement/meta machinery recognizes exactly one primitive: `Meta.join`.** It never names
  `Bool::fold`, `handleCases`, or `typeMatch`.
- `Bool::fold` (and any eliminator) may be named only where the compiler **realizes** it (emits its code) or
  **reduces** it (compile-time constant fold) — never to assume it is "the branch."
- A native's meta-behaviour is **declared in Eliot, beside the native, by its owner**; absence is ⊤ (sound).
- Domain knowledge (what an `Interval` is, how ranges join) lives in the **domain** (`Meta`, the `^Meta`
  vessels) and its **consumers** (the backend's width decode, the LSP's range decode) — never in the generic
  reconcile pass.

## 4½. Current state and the remaining path (2026-07-12)

Phase 1 (Steps 1, 6, 7) and Phase 2 (Steps 2, 3) are **done, committed, green** (see the header). Items 1 and 2
below are landed; the mechanism note on item 2 records the correction (the instance dispatches — no stuck
`Meta::join`). **Item 3 remains** — and its join-input source is *not* the reduction (which dissolves `Meta.join`),
so it needs the structural read described below.

1. **Auto-derive `Meta[Int$Meta]`** — **DONE** (`90101d3d`). `MetaConstructorDesugarer`, which already synthesizes
   the `Int$Meta` structure from `type Int {range: Interval[BigInteger]}`, now also emits the *instance*
   `implement Meta[Int$Meta] { def join(a, b) = Int$Meta(intervalJoin(range(a), range(b))) }`. Copied the
   `DataDefinitionDesugarer.createPatternMatchImpl` shape: a body-less **marker** (arg of the pattern type
   `Int$Meta`, guard `trueReference`) plus the `join` **method**, in `Qualifier.AbilityImplementation("Meta",
   "Int$Meta")` (the meta structure is nullary, so its pattern key is just its name). The body joins **each slot**
   via that slot's domain's *plain* join function (`intervalJoin`, derived as decapitalized domain head + `Join`);
   `intervalJoin` already existed in the overlay. A slot whose domain head is not a simple type application yields
   no instance (fail-safe).
2. **Repoint the channel onto `fold^Meta`** — **DONE** (`90101d3d`), but *not* as the saved `mergeViaCompanion`
   assumed. `reduceInstance(fold^Meta, [Int])` monomorphizes cleanly (thanks to item 1), but the reduction **does
   not stall at a stuck `Meta::join`** — the `Meta[Int$Meta]` instance *dispatches* (its args are `Int$Meta`
   values), so `fold^Meta` reduces to a concrete `Int$Meta(intervalJoin(…))` and the quote yields the join, exactly
   like a transfer. `mergeViaCompanion` therefore just reduces-and-reads-back (no stuck-native detection). The
   channel now names no branch construct. Verified: `FoldJoinProbe` (divergent-range fold) builds/runs;
   `ExamplesIntegrationTest` + lang/jvm/LSP green.
3. **Table join-edges + generic reconcile** (Steps 4–5) — **DONE** (`a9670e7f`). `RefinementTable` gained
   `joinInputs` (per branch-arm position → the merge interval it reconciles to). The channel builds these by a
   **structural read** of the *generic* `fold^Meta` companion body (where `Meta::join` is still the abstract,
   un-dispatched ability method — the reduction dissolves it, so it is *not* the source): peel the leading parameter
   lambdas (`RefinementChannelProcessor.joinInputIndicesOf`/`metaJoinArgParams`/`paramPreimage`), recognise
   `Meta::join(x, y)`, map each bare-parameter (or single-projection) argument back to a value-arg position. Type
   params bind first, so the last `valueArgCount` lambdas are the value params — the offset that maps `fold^Meta[A]`'s
   `{whenTrue, whenFalse}` to arg positions `{1, 2}`, excluding the condition. A non-clean preimage is skipped (⊤,
   fail-safe). `ReconcileProcessor` drops `boolFoldFqn`: every argument reconciles to its join-input target if the
   channel marked it an arm, else to ⊤ — one rule. `widthTransparentLeaves` kept (see the header scope decision).
   `FactCache.CACHE_VERSION` 12 → 13.

The invariant (§4) is fully met: nothing in the channel or reconcile names a branch construct.

## 5. Validation

`ExamplesIntegrationTest` (builds and *runs* JARs — the only check that catches a bad representation
conversion), the full `lang` + `jvm` suites, and the reconcile-pass tests rewritten to the generic
(channel-edge-driven) form: assert that a `Meta.join`-declaring native's arm positions become join-input
reconciles and that no test references `Bool::fold`.
