# Native Generation: One Host-Runnable Implementation per Name

Status: **Design in progress.** This document records the current state of the design discussion. Nothing of the
resolution mechanism is implemented yet; the only code that has landed is a *prerequisite* purity fix to
`BindingProcessor` (commit `618dad8b`, see "Prerequisite, landed" below). The problem is currently latent — it
does not yet mis-compile any program — but the resolution today is a silent, order-dependent race that is not
fail-safe.

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

This boundary **cuts through the target (U) layer, not around it.** Whether a U definition "can run on the
compiler" has nothing to do with it being U code — it is whether its transitive primitive base is run-able.
`Coerce[Int,Int]`'s *condition* (`lessThanOrEqual && …`) is run-able, so checking evaluates it; its `nativeWiden`
*payload* is emit-only, so checking never forces it (it rides inside the `some`). Same definition, both kinds,
separated by *position*.

So C and U were never competing answers to one question. They are implementations of one abstract `add` for two
execution engines: the **host** (checking) reads the host-runnable realization; **codegen** reads the
emit/runtime realization. The two facts already encode this split: `NativeBinding` = "what the host can run,"
`TransparentBinding` = "what gets emitted." There is no ordering to invent.

## The rule

> Across C (compiler/host) and U (target) combined, a name has **at most one host-runnable implementation**.

- **C is the closed set of host-runnable primitive *leaves*.** Only the compiler can supply a host-runnable leaf —
  U is `.els` text with no host execution semantics except by calling C's primitives. C must therefore be present
  in every build, regardless of target.
- **U = host-runnable *composites* over C's leaves** (`fitsIn`, the `Combine`/`Coerce` instances, the opaque `Int`
  representation body) **+ emit-only leaves** (machine ops) **+ types/abilities.**
- **U may give a C-owned name an *emit-only* sibling** (a body-less backend native — e.g. a machine `add`) even
  though C host-runs it. That is the runtime realization, not an override: checking uses C's native, codegen emits
  the leaf.
- **Two host-runnable implementations of one name = error.** "U gave an eliot body to a host primitive" is the
  override, rejected exactly like the layer merge rejects "multiple implementations."
- **Companion clause:** a name forced in a *must-run* position (type-level eval, or lowering an opaque body) with
  **zero** host-runnable implementations is also an error — reported lazily, at the use site, when the evaluator
  gets stuck on an emit-only leaf (`PostDrainQuoter` "Cannot resolve type."; `RepresentationLowering` "Could not
  reduce opaque type…"). This is the Use-Site Verification cornerstone; it is *not* a separate modular analysis.

Net: type-level eval needs *a host-runnable implementation*; the rule guarantees exactly one; so there is nothing
to order or prefer. The original "which to read for `add`?" stops being a question.

### Stress test

| name | C | U | host-runnable count | verdict |
|---|---|---|---|---|
| `add` (BigInteger) | native | future machine `add` (emit-only, body-less) | 1 (C) | OK — check via native, emit via leaf |
| `add` *if U gave a pure-eliot body* | native | composite over C prims | **2** | **error** — override of a host primitive |
| `fitsIn` | — | `lessThanOrEqual && …` | 1 (U) | OK |
| `Combine[Int,Int]` | — | `min`/`max` composite | 1 (U) | OK |
| `Coerce[Int,Int]` | — | condition host-runnable, `nativeWiden` payload emit-only | 1 (U) | OK — payload never forced at type level |
| `nativeWiden`, `nativeAdd…` | — | body-less machine leaf | 0 (emit-only) | OK — never forced at type level |
| `Int.+` | — | body bottoms in `nativeWiden` (emit-only) | 0 | OK — signature-only at checking; emitted at codegen |
| `opaque type Int = fold(fitsIn…, JvmByte, …)` | — | opaque body host-runnable | 1 (U) | OK — C doesn't define `Int`; lowering runs it |
| `lessThanOrEqual`, `&&`, `fold` | native | body-less abstract signature | 1 (C) | OK |
| `intToString` | — | body-less machine leaf | 0, runtime-only | OK — not type-level; companion clause N/A |

## What this is *not* (framings considered and rejected)

Recorded so they are not re-litigated.

- **"C-if-present-else-U" as a bare priority.** Unprincipled — a tiebreak dressed as a rule.
- **`opaque` as the marker.** Would force *every* C-implemented function that also gets a runtime body to be
  `opaque def`. Absurd; the classification belongs to the compiler (it has a native), not an annotation.
- **A "two-axis" (platform × stage) theory.** Overbuilt for the single case it covers; did not aid reasoning.
- **Modeling C as a third platform on the same axis as the target platforms.** Leads to chaos: the target (U)
  *contributes to* the host run (its type definitions, its `Combine`/`Coerce` instances, and the ordinary defs they
  use all execute on the host during checking), so there is no clean precedence among base / host / target. The
  resolution is that **C is not a third platform** — it is the host's realization of the run-able leaves; the
  target contributes to checking simply as *the program being checked*.
- **Probing whether a `NativeBinding`/`HostNativeBinding` fact "exists."** Violates the fact model: a fact you
  request must be expected to exist (`getFactOrAbort`, or `getFact` + `compileError` on `None`); fact-absence is
  not a control-flow signal. So the host-vs-user decision must read *in-memory data*, never test a fact for
  presence.
- **Analysing whether a U body is host-runnable.** "Does this body bottom out only in run-able leaves" is a
  transitive, whole-graph property — precisely the class of thing Use-Site Verification defers. The user-binding
  producer builds the binding **blindly**; non-run-ability is discovered by the evaluator getting stuck at the use
  site. We never compute it as a fact.

## Mechanism (current direction)

The simplest shape consistent with the fact model: **one processor owns `NativeBinding.Key`**, with the precedence
done *in-process* over in-memory data (a `Map.get`, not a fact-existence probe — which is what makes the "fallback"
legitimate where a cross-fact fallback would not be).

1. A **host-leaf registry** — `Map[ValueFQN, SemValue]` — assembled at plugin init. It spans two modules:
   `SystemNativesProcessor`'s leaves live in `lang`, `StdlibNativesProcessor`'s in `stdlib` (which depends on
   `lang`, not the reverse). So the registry is **plugin-contributed** and read in-memory. This is required, not
   cosmetic: the override check must cover stdlib leaves like `add`, not just System's.
2. The unified `NativeBinding` processor resolves `NativeBinding.Key(fqn)`:
   - `registry.get(fqn)` is a host leaf →
     - if the merged value *also* carries a (checking-visible) body → **override error** (host-leaf ∩ bodied);
     - else → the native.
   - else → the existing user-body logic (build from `checkingRuntime`; inert `VTopDef` for `opaque`; abort →
     `VNeutral` for body-less, leaving `DataType`/`Match`/codegen to handle it).
3. `DataTypeNativesProcessor`/`MatchNativesProcessor` are otherwise disjoint from the user branch (they handle
   body-less FQNs, which the user branch aborts on), so they may stay separate or fold in. **One existing overlap
   must be tidied:** `SystemNativesProcessor` and `DataTypeNativesProcessor` both produce `NativeBinding(Type)`
   today, resolved only by list order (System listed first). Make it explicit — exclude `typeFQN` from
   `DataTypeNativesProcessor`, or fold the type-constructor inert branch into the unified processor.

Properties: registry-first gives the right def **deterministically** (the host realization wins with no reliance
on processor order), and the override is caught **in one place** as a declaration-vs-body intersection — no
probing, no body classification. `TransparentBinding` (codegen) is unchanged: it always reads the value's runtime
body, and the compiler natives never emit a `TransparentBinding`.

## Open questions / consequences to accept

- **The emit-leaf set must become an explicit backend declaration.** Today an emit-only leaf (`nativeWiden`) and a
  genuinely abstract (unimplemented) signature are *both* body-less and indistinguishable without the backend
  saying which FQNs it emits. The companion clause and clean codegen both want this line drawn structurally rather
  than by today's implicit FQN-recognition in the backend. This is the one piece the design *forces*.
- **`opaque` host-runnable body over a C native (coherence).** The proposed check is *lenient* — it triggers on a
  checking-visible body (`checkingRuntime.nonEmpty`), so an `opaque def` over a C native would be allowed (checking
  uses the native, codegen the opaque body). A *strict* reading would reject any body coexisting with a C native.
  For the arithmetic primitives this never arises (their runtime sibling is a body-less leaf, not an opaque body),
  so lenient is recommended. The host/target *agreement* obligation for such a pair — the host native is the
  spec, the target impl must conform within the proven bounds — is a separate, currently-unchecked concern worth a
  TODO.
- **Fold `DataType`/`Match` into the single producer, or keep separate?** Folding yields literally one producer of
  `NativeBinding` (no possible clash); keeping them separate is less churn and they are provably disjoint from the
  user branch once the `typeFQN` overlap is fixed.

## Prerequisite, landed

The two `NativeBinding`/`TransparentBinding` producers share `BindingProcessor`, whose `collectBindings` walks a
body's transitive references and fetches each one's `NativeBinding`. It used a **shared mutable** set
(`generating`, a `ConcurrentHashMap`) on the processor instance to break mutual-recursion deadlocks — which made
binding generation impure and interleaving-dependent (it skipped values merely generating concurrently in
unrelated chains). Commit `618dad8b` replaced it with the runtime's per-fiber ancestor chain
(`CompilerIO.activeFactKeys`), which is correctly scoped (only *this* request's ancestors can deadlock) and keeps
the processor a pure function of the facts it reads. The unified-processor work above should land on this pure
base.

## Glossary of current facts/code

- **`NativeBinding(vfqn, semValue)`** — the checking-phase (host-runnable) binding the NbE evaluator reads per FQN.
  The fact this plan makes single-producer and rule-checked.
- **`TransparentBinding(vfqn, semValue)`** — the codegen/lowering binding; identical to `NativeBinding` except it
  keeps `opaque` bodies unfolded. Unchanged by this plan.
- **`OperatorResolvedValue.runtime` / `.checkingRuntime`** — a value's body for lowering vs for checking;
  `checkingRuntime = if (opaque) None else runtime`. The "has a checking-visible body" test in the override check.
- **`SystemNativesProcessor` / `StdlibNativesProcessor` / `DataTypeNativesProcessor` / `MatchNativesProcessor` /
  `UserValueNativesProcessor`** — the current `NativeBinding` producers (see "The problem").
- **`CompilerIO.activeFactKeys`** — the runtime's per-fiber ancestor chain of in-progress fact keys; the pure cycle
  guard `collectBindings` now uses.
