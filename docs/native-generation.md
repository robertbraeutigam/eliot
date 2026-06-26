# Native Generation: Host-Runnable Reduction per Name

Status: **Design resolved; not yet implemented.** This document records the design. The mechanism below — labeled,
total contributor facts selected by a precedence merger — is agreed but not yet built; the only code that has landed
is a *prerequisite* purity fix to `BindingProcessor` (commit `618dad8b`, see "Prerequisite, landed" below). The
problem is latent today — it does not yet mis-compile any program — but the current resolution is a silent,
order-dependent first-registration race that is not fail-safe.

## The problem

During checking, the NbE evaluator reads **one binding per value FQN** — the `NativeBinding` fact
(`lang/.../monomorphize/fact/NativeBinding.scala`). It is the value the checker uses to *reduce* that FQN at
compile time. That fact has **no single owner**: several processors can produce it, and which one wins is decided
by a silent first-registration race.

- `SystemNativesProcessor` (lang) — the compiler-intrinsic host primitives: `Function`, `Type`, `Bool`
  `true`/`false`/`fold`.
- `StdlibNativesProcessor` (stdlib) — the compile-time arithmetic/comparison on `BigInteger`
  (`add`/`subtract`/`min`/`max`/`multiplyMin`/`multiplyMax`/`lessThanOrEqual`/`inc`) and `&&`.
- `DataTypeNativesProcessor` (lang) — inert `VTopDef` for type constructors.
- `MatchNativesProcessor` (lang) — the `match`/`typeMatch` dispatch natives.
- `UserValueNativesProcessor` (lang) — the binding built from a value's own (checking) body.

`SequentialCompilerProcessors` runs *all* of them for a `NativeBinding.Key`, and `registerFact` →
`Deferred.complete` (`IncrementalFactGenerator.scala`) is **first-wins and silent** — a second producer's value is
dropped with no error. So the de-facto rule today is *processor list order*: the compiler natives are sequenced
ahead of `UserValueNativesProcessor`, and `UserValueNativesProcessor` only dodges them at all because it skips
body-less values (`bindsBodylessValues = false`).

The concrete trigger. `BigInteger.add` (and the other arithmetic) is needed at the **type level** to compute
dependent bounds (`Int[add(LMin, RMin), add(LMax, RMax)]`), so the compiler supplies a native reduction. It is
also, eventually, needed at **runtime** (a program that adds values), so a platform layer must supply an
implementation. The moment a target layer gives such an FQN a body, two producers fire for the same
`NativeBinding.Key` and the winner is decided silently by list position. There is no stated rule for which
implementation to read, and the resolution is not fail-safe (a reorder, or a new native processor placed after
`UserValueNativesProcessor`, silently flips the winner — exactly the silent-wrong-typing the project forbids).

## Cornerstone framing

Two cornerstones decide this.

- **Types Are Values (λ\*).** Type-level evaluation is not an abstract "compile-time" notion; it is *literally
  executing code on the machine the compiler runs on*. So the binding the checker reads for an FQN is "how to run
  this FQN on the host."
- **Platform-Independence via Layers.** "Which implementation of a name do I read?" is the same question the layer
  model already answers for every name: *the one belonging to the platform this phase is building for.*

## The reframe: run-able vs emit-only; the host is a platform

The decisive simplification: the compiler can **emit** code it cannot **execute**. The host is a platform — the
actual machine the compiler runs on (the JVM). There are only two kinds of primitive:

- **run-able** — the host can execute it now: integer math, comparison, `&&`, `fold` (the `System`/`Stdlib`
  natives). Type-level evaluation and opaque-body lowering both *run* on the host, so they may only use these.
- **emit-only** — the host can only *produce* it for the target to execute later: `nativeWiden`, the
  `nativeAdd…`/`nativeMultiply…` machine leaves, `intToString`, `println`. The host can never run these.

This boundary **cuts through the target layer, not around it.** Whether a definition "can run on the compiler" has
nothing to do with it being target code — it is whether its transitive primitive base is run-able. `Coerce[Int,Int]`'s
*condition* (`lessThanOrEqual && …`) is run-able, so checking evaluates it; its `nativeWiden` *payload* is emit-only,
so checking never forces it (it rides inside the `some`). Same definition, both kinds, separated by *position*.

So the native reduction and the target body were never competing answers to one question. They are implementations of
one abstract `add` for two execution engines: the **host** (checking) reads the host-runnable realization; **codegen**
reads the emit/runtime realization. The two facts already encode this split: `NativeBinding` = "what the host can run,"
`TransparentBinding` = "what gets emitted." There is no ordering to invent — only a precedence to state.

## The rule

Type-level evaluation is *running code on the host*, so for each name the checker needs that name's **host-runnable
reduction**. Two categories of supplier can offer one:

- **Native suppliers** — compiler/platform-supplied reducers coded directly against the evaluator (System's
  `Function`/`Type`/`Bool`, Stdlib's `BigInteger` arithmetic + `&&`, plus any a platform plugin adds). Host-runnable by
  construction. **Preferred for checking.**
- **User suppliers** — the per-layer body suppliers (stdlib's, jvm's, and any layer in between) that build a binding
  from a value's `.els` body. A user supplier **returns every bodied def** — it neither can nor does decide whether a
  body is host-runnable. **Fallback**, used when no native supplies the name; non-runnability surfaces lazily at the
  use site, never as a producer-side judgment.

Selection is **pure precedence with no conflict resolution**: take the first native value; failing that, the first user
value; failing that, no binding. Nothing has to be ordered or rejected, because uniqueness already holds within each
category:

- native suppliers are **disjoint by construction** (each owns its own names) → at most one native value;
- the user layer-stack has **one implementation per name**, enforced upstream by the layering system
  (`UnifiedModuleValueProcessor` rejects "multiple implementations") → at most one user value.

**A user body coexisting with a native is the normal case, not an override.** `BigInteger.add` has a compile-time
native (for dependent bounds) *and* a runtime body in a platform layer; checking reads the native, codegen reads the
body via `TransparentBinding`. Precedence alone prevents the body from shadowing the native — so there is nothing to
reject. Whether the two *agree* (the native is the spec; the body must conform within the proven bounds) is the
deferred **host/target agreement** obligation (a TODO), not a condition checked here.

**Companion clause.** A name forced in a *must-run* position (type-level eval, or lowering an opaque body) with **no**
value from any supplier is an error — reported lazily, at the use site, when the evaluator gets stuck on an emit-only
leaf (`PostDrainQuoter` "Cannot resolve type."; `RepresentationLowering` "Could not reduce opaque type…"). This is the
Use-Site Verification cornerstone; it is *not* a separate modular analysis.

*(Earlier drafts of this doc proposed "at most one host-runnable implementation per name; two = error." That rule is
**struck**: a user supplier returns every bodied def and cannot identify host-runnability, so "a second host-runnable
implementation" is not an observable condition. Native precedence prevents the shadowing the rule was meant to prevent;
the residual agreement concern is deferred, above.)*

### Stress test

| name | native | user | checking reads | note |
|---|---|---|---|---|
| `add` (BigInteger) | yes | machine `add` (emit-only, body-less ⇒ no user value) | native | native is the only value |
| `add`, U *also* gives a pure-eliot body | yes | composite over native prims | native | **not an override** — native wins checking, U body is codegen; agreement deferred |
| `fitsIn` | — | `lessThanOrEqual && …` | user | host-runnable composite; reduces during checking |
| `Combine[Int,Int]` | — | `min`/`max` composite | user | host-runnable composite |
| `Coerce[Int,Int]` | — | condition host-runnable, `nativeWiden` payload emit-only | user | payload never forced at type level |
| `nativeWiden`, `nativeAdd…` | — | body-less machine leaf (no user value) | none | emit-only; never forced at type level |
| `Int.+` | — | body bottoms in `nativeWiden` (emit-only) | user | signature-only at checking; stuck only if forced; emitted at codegen |
| `opaque type Int = fold(fitsIn…, …)` | — | opaque body host-runnable | user | no native defines `Int`; lowering runs the body |
| `lessThanOrEqual`, `&&`, `fold` | yes | body-less abstract signature (no user value) | native | |
| `println`, `intToString` | — | platform body / machine leaf | user | runtime-only, never forced; companion clause N/A |

## What this is *not* (framings considered and rejected)

Recorded so they are not re-litigated.

- **The host-leaf registry** (this doc's own earlier proposed mechanism). A runtime `Map[ValueFQN, SemValue]`
  assembled at plugin init and consulted to make the host-vs-user decision. Rejected: it is a **side-channel** carrying
  per-FQN semantics *outside* the fact graph — no incremental tracking, a parallel mechanism to the facts. The
  labeled-contributor-facts design keeps every answer in the fact graph; the plugin `Configuration` carries only the
  contributor *roster* (strings), never semantics.
- **"Native-if-present-else-user" as a bare priority.** Once it is restated as a *category precedence* over total facts
  (native suppliers before user suppliers, each category provably single-valued), it is exactly the rule — but as a
  silent tiebreak over a racing single key it was unprincipled. The principle is what makes it legitimate.
- **`opaque` as the marker.** Would force *every* native-implemented function that also gets a runtime body to be
  `opaque def`. Absurd; the classification belongs to the compiler (it has a native), not an annotation.
- **Probing whether a binding fact "exists."** Still rejected as a control-flow signal — and the design *avoids* it:
  contributor facts are **total** (every supplier answers `Some`/`None` for every query, under its own label), so the
  merger reads *values* with `getFactOrAbort`, never tests presence. Absence is a value, not a missing fact; a
  mis-wired supplier aborts loudly instead of silently demoting to a lower-precedence answer.
- **Analysing whether a user body is host-runnable.** Still rejected — and now load-bearing: *because* a user supplier
  returns every def blindly, native+user coexistence is benign and needs no override check (see "The rule").
  Non-runnability is discovered by the evaluator getting stuck at the use site, never computed as a fact.
- **A "two-axis" (platform × stage) theory.** Overbuilt for the single case it covers; did not aid reasoning.
- **Modeling the compiler natives as a third platform.** The target *contributes to* the host run (its type
  definitions, `Combine`/`Coerce` instances, and the ordinary defs they use all execute on the host during checking),
  so there is no clean precedence among base / host / target. The resolution: native suppliers are not a platform —
  they are the host's realization of the run-able leaves; the target contributes to checking simply as *the program
  being checked*.

## Mechanism

**Labeled, total contributor facts + a precedence merger.** Everything stays in the fact graph; nothing is a
side-channel.

1. **Each supplier emits a total, label-discriminated fact** `ContributedBinding(vfqn, label)` — `Some(semValue)` if
   that supplier defines a reduction/body for `vfqn`, `None` otherwise. Distinct labels ⇒ every answer is its own fact
   ⇒ they coexist legally, with no first-registration race. The native suppliers (System, Stdlib, platform reducers)
   and the per-layer user suppliers (stdlib's, jvm's, intermediate layers') are all contributors of this one fact type,
   separated by label and tagged native-vs-user category.
2. **The label set is open and enumerated through `Configuration`.** Dynamic layers mean the contributor set is not
   fixed, so it cannot be a static list of fact types. Each contributing plugin adds its label(s) to a
   `Configuration.Key[Set[String]]` in `configure()`; because all `configure()` complete before
   `initialize(configuration)`, the merger is built in `initialize` already holding the full set, and asks
   `ContributedBinding.Key(vfqn, label)` for each. The config carries only strings (*which* contributors exist), never
   per-FQN semantics — ordinary plugin configuration, not a registry.
3. **One merger owns the evaluator-facing `NativeBinding(vfqn)`.** Pure precedence: first native value → else first
   user value → else abort (no binding; the evaluator stalls at the use site = companion clause). `getFactOrAbort` on
   each contributor fact (all total, so reads always land) makes a missing supplier fail loudly. **No conflict arm:**
   native suppliers are disjoint and the user stack is single-implementation (layering), so each category yields at
   most one value. Native disjointness is a construction invariant, so no ≥2-native guard is added — selection is
   correct under it.
4. **`DataType`/`Match` natives become ordinary native-category contributors** (their own labels), disjoint from the
   rest. The one *existing* overlap must be removed to keep native disjointness real: `SystemNativesProcessor` and
   `DataTypeNativesProcessor` both answer for `NativeBinding(Type)` today (resolved only by list order) — exclude
   `typeFQN` from the `DataType` contributor so each native name has exactly one native supplier.

`TransparentBinding` (codegen) is unchanged: it always reads the value's runtime body, and native suppliers never emit
one.

## Open questions / consequences to accept

- **The emit-leaf set must become an explicit backend declaration.** Today an emit-only leaf (`nativeWiden`) and a
  genuinely abstract (unimplemented) signature are *both* body-less and indistinguishable without the backend saying
  which FQNs it emits. The companion clause and clean codegen both want this line drawn structurally rather than by
  today's implicit FQN-recognition in the backend. This is the one piece the design *forces*.
- **Host/target agreement is unchecked.** When a name has both a native (checking) and a user body (codegen) — the
  `add` case — nothing verifies the body conforms to the native (the native is the spec). Precedence guarantees
  *which* is read in each phase; agreement between them is a separate, currently-unchecked concern worth a TODO.
- **`opaque` host-runnable body over a native (coherence).** An `opaque def` over a native is allowed (checking uses
  the native, codegen the opaque body). For the arithmetic primitives this never arises (their runtime sibling is a
  body-less leaf, not an opaque body), so the lenient reading is fine; it folds into the agreement TODO above.

## Prerequisite, landed

The two `NativeBinding`/`TransparentBinding` producers share `BindingProcessor`, whose `collectBindings` walks a
body's transitive references and fetches each one's `NativeBinding`. It used a **shared mutable** set
(`generating`, a `ConcurrentHashMap`) on the processor instance to break mutual-recursion deadlocks — which made
binding generation impure and interleaving-dependent (it skipped values merely generating concurrently in
unrelated chains). Commit `618dad8b` replaced it with the runtime's per-fiber ancestor chain
(`CompilerIO.activeFactKeys`), which is correctly scoped (only *this* request's ancestors can deadlock) and keeps
the processor a pure function of the facts it reads. The mechanism above should land on this pure base.

## Glossary of current facts/code

- **`ContributedBinding(vfqn, label)`** *(to add)* — one supplier's total answer for a name: `Some(semValue)` or
  `None`, under that supplier's label. The fact the precedence merger reads, one per label.
- **`NativeBinding(vfqn, semValue)`** — the checking-phase (host-runnable) binding the NbE evaluator reads per FQN.
  Made single-owner by this plan: produced only by the merger, from the `ContributedBinding`s.
- **`TransparentBinding(vfqn, semValue)`** — the codegen/lowering binding; identical to `NativeBinding` except it
  keeps `opaque` bodies unfolded. Unchanged by this plan.
- **`OperatorResolvedValue.runtime` / `.checkingRuntime`** — a value's body for lowering vs for checking;
  `checkingRuntime = if (opaque) None else runtime`. What each user supplier reads for its `ContributedBinding`.
- **`Configuration.Key[Set[String]]`** *(to add)* — the contributor-label roster; plugins add their labels in
  `configure()`, the merger reads it in `initialize` to know which `ContributedBinding`s to ask for.
- **`SystemNativesProcessor` / `StdlibNativesProcessor` / `DataTypeNativesProcessor` / `MatchNativesProcessor` /
  `UserValueNativesProcessor`** — the current `NativeBinding` producers (see "The problem"); under this plan they
  become labeled `ContributedBinding` contributors feeding the merger.
- **`CompilerIO.activeFactKeys`** — the runtime's per-fiber ancestor chain of in-progress fact keys; the pure cycle
  guard `collectBindings` now uses.
