# Generic Refinement Merges — Removing the `Bool::fold` Dependency

**Status: PLAN.** Continues the bounds-as-refinements work (`docs/bounds-as-refinements.md`, Step 8 —
backend width selection). The representation-reconcile pass exists in a first, *non-generic* form
(`reconcile/processor/ReconcileProcessor`, committed as "stage A"); this plan makes it — and the channel
and backend it sits between — domain- and construct-agnostic, and removes the compiler's dependence on
`Bool::fold` as if it were "the branch."

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

### Step 1 — Correct the false premise (apidoc)

Rewrite the two comments that assert `fold` is "the only way to branch": `WellKnownTypes.boolFoldFQN` and
`SystemNativesProcessor`. State that `fold` is *an* eliminator over an opaque `Bool` (value/type match are the
case-analysis primitives; user eliminators desugar to those), and that this FQN is used only to **reduce** and
**emit** `fold`, never as a refinement-merge signal — merges are detected via `Meta.join` (this doc). Give
`fold` a correct `/** */` in `stdlib/eliot/eliot/lang/Bool.els` while here.

### Step 2 — Author `fold`'s `^Meta` companion

The native's owner (the stdlib author) declares `fold`'s merge behaviour beside `fold`, in Eliot:

```
fold[A](condition: Bool, whenTrue: A, whenFalse: A): A { join(whenTrue, whenFalse) }
```

It lives in the stdlib **compiler track** (`stdlib/eliot-compiler/eliot/lang/Bool.els`, next to a re-declared
`fold`), because the vessel uses compiler-pool `Meta.join` and `^Meta` companions are compiler-pool-only — the
same reason the compile-time `Either`/`Guard` bodies live under `eliot-compiler/`. The merge *contract* ("the
result's range is the join of the arms' ranges") is **platform-independent** — true on every target — even
though `fold`'s *runtime* is realized per platform; so it belongs in the base once, exactly as `rangeAdd^Meta`
is base stdlib while `nativeAdd`'s runtime is jvm-specific.

**New design work — domain-polymorphic vessels.** The arithmetic vessels are Int-monomorphic
(`rangeAdd(a: Int, b: Int): Int`); `fold` is `fold[A]`, so its `^Meta` joins over **A's domain** via
`Meta.join`, which needs a `Meta[…]` instance for A. `A = Int` resolves `Meta[Interval]` and joins; an `A`
with no domain (a `data` type) has ⊤ arms and the companion is a ⊤ pass-through. `MetaTransferDesugarer` (today
transfer-shaped only) must learn to emit a join-shaped, domain-constrained companion.

### Step 3 — Channel: uniform `^Meta`-driven meta + join-input detection

Delete the fold special-case in `RefinementChannelProcessor` (`walkBranch`/`runJoin`, the `boolFoldFqn`
constant). Every meta-affecting native computes its result meta by evaluating its `^Meta` companion — as
arithmetic already does — with no per-native branch. Detect **join-inputs** by recognizing `Meta.join` in the
companion: the argument positions the companion feeds to `Meta.join` are the join-inputs; record them together
with the join result. This is a one-time static read of each native's (Eliot) `^Meta` body, recognizing the
one domain FQN `Meta.join`, never a construct list.

### Step 4 — `RefinementTable` carries generic metas

Change `RefinementTable.NodeInterval(position, min, max)` to carry a **generic `GroundValue` meta** per node,
plus the **join-input edges** (arm position → the join result meta). The channel already builds the
`GroundValue` (`Int$Meta`/`Interval`) internally before decoding to `(min, max)`; store the `GroundValue`.
Adapt consumers: the LSP hover (`TypeHintIndex`) decodes the meta `GroundValue` back to a `[min,max]` for its
value-range hint — the Int-specific decode lives in the consumer, which is the right home for domain knowledge.

### Step 5 — Reconcile pass fully generic

Remove `widthTransparentLeaves`, `boolFoldFqn`, and all `Interval`/`GroundValue`-construction from
`ReconcileProcessor`. The rule becomes purely mechanical and names nothing:

> Reconcile each **join-input** (channel-provided) to its join result; reconcile every other call argument and
> the return to its consumer's expected meta (⊤ today). Copy metas through opaquely; compare by structural
> equality.

The pass no longer distinguishes `fold` from any call, no longer excludes arithmetic operands (they reconcile
to ⊤ like any consumed arg — the JVM `long` fast-path is given up on boxed values, recovered generically by
any backend that declares narrow parameter metas), and never constructs or inspects a domain meta.

### Step 6 — Backend width selection ("the jvm pre-processor") not hardcoded

`IntRepresentation` (jvm/asm): decode the generic meta (an `Interval` `GroundValue`) → machine width; the
`Jvm*` types become an **internal backend enum**, not Eliot types. `effectiveRepType(node)` drives stack
widths from the node meta; add a `NativeType` `Int → java.math.BigInteger` entry for descriptors (since `Int`
stops being lowered). Lower a `Reconcile` node as `convertRepresentation(rep(source.meta), rep(node.meta))`.
The Int-specific meta decode lives here — the backend is Int/JVM-specific by nature, the correct home for
domain+platform knowledge. `generateBoolIntrinsic` **keeps** emitting `Bool::fold` inline (realization; see
§1).

### Step 7 — Deletions & de-duplication

Delete `Represent.els` ×2, `RefinementRepresentation`, `RepresentationLowering`, and the `representationOf`
lowering in `MonomorphicUncurryingProcessor`; replace `CodegenProjection`'s `representationOf`-based
width-collapse key with the erased carrier (post-flag-day `Int` type-args are nullary, so `Int` instances
already collapse). Drop the duplicated `boolFoldFqn` constants (channel, reconcile). After this the *only*
references to `Bool::fold` by name are: the compile-time reduction native (`SystemNativesProcessor`), the
compile-time branch-select (`PostDrainQuoter`), and the backend inline emission
(`ExpressionCodeGenerator`/`Intrinsics`) — all **realization or reduction**, none "branch detection."

## 4. The durable invariant

- The compiler's **refinement/meta machinery recognizes exactly one primitive: `Meta.join`.** It never names
  `Bool::fold`, `handleCases`, or `typeMatch`.
- `Bool::fold` (and any eliminator) may be named only where the compiler **realizes** it (emits its code) or
  **reduces** it (compile-time constant fold) — never to assume it is "the branch."
- A native's meta-behaviour is **declared in Eliot, beside the native, by its owner**; absence is ⊤ (sound).
- Domain knowledge (what an `Interval` is, how ranges join) lives in the **domain** (`Meta`, the `^Meta`
  vessels) and its **consumers** (the backend's width decode, the LSP's range decode) — never in the generic
  reconcile pass.

## 5. Validation

`ExamplesIntegrationTest` (builds and *runs* JARs — the only check that catches a bad representation
conversion), the full `lang` + `jvm` suites, and the reconcile-pass tests rewritten to the generic
(channel-edge-driven) form: assert that a `Meta.join`-declaring native's arm positions become join-input
reconciles and that no test references `Bool::fold`.
