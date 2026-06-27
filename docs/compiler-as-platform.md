# The Compiler as a Platform: Platform-Scoped Source Unification

Status: **Planned.** Design (this document); implementation not started. Motivating first consumer: the compile-time
`Either` carrier of `docs/effectful-signatures.md` (W1), which needs a *reducing* compile-time implementation
(`foldEither`, `implement Monad/Throw`) available in **every** workspace — including the abstract-only LSP workspace —
without depending on a runtime platform layer (jvm) being linked.

## The idea in one line

**The compiler is a platform, exactly like jvm.** It implements the same abstract base definitions, in the same two
ways a runtime platform does — *mostly Eliot, bottoming out in a few native leaves* — but its implementations are the
ones used for **compile-time evaluation** (type checking / NbE), where jvm's are used for **codegen**. Today the
compiler platform exists but can only be written in **Scala** (the native processors). This plan gives it an **Eliot
source root**, so compile-time logic that *can* be ordinary Eliot (`Either`, `foldEither`, ability instances) *is*.

## Why this is the right model (not a special case)

The split already exists — we are naming it, not inventing it.

- A name can already have **two implementations at one FQN**: `add` has a compile-time reduction (the `stdlib` native
  label in `StdlibNativesProcessor`, `add(2,3)→5` during checking) *and* a runtime one (the jvm arithmetic leaf). The
  `ContributedBinding` / `BindingMergerProcessor` machinery exists precisely to let both coexist: native label wins for
  checking, the runtime body is read by codegen (`TransparentBinding`). "One name, two platform implementations" is the
  established shape.
- `true`/`false`/`fold` are the same: the compiler platform implements them (`SystemNativesProcessor`) for the
  type-level phase; the runtime platform realizes `Bool` for the value-level phase.

So the compiler is *already* a platform whose members are `add`, `fold`, `true`, …. The only thing missing is that its
members can only be **Scala natives** today. `Either` cannot reasonably be a Scala native — it is a plain tagged union
with a `match`-based eliminator and two ability instances, all of which the **one NbE evaluator already runs** for
ordinary `data`. Reimplementing that in Scala would be a parallel evaluator (the anti-pattern the single-evaluator
cornerstone forbids). The fix is to let the compiler platform be implemented in **Eliot**, with native leaves only where
genuinely primitive — *identical in shape to how jvm is mostly-Eliot over a handful of bytecode leaves*.

### Cornerstone framing

This is the **λ\* phase/erasure** distinction made structural: the compile-time (type-level) phase and the runtime
(value-level) phase each get a platform. The abstract base (`lang`/`stdlib`) stays representation-free; **each platform
fills it in for its phase**. The "no `data` in base" rule is untouched — `data Either` lives in the *compiler platform
layer*, a platform layer like jvm, never in the abstract base. It is the **Platform-Independence via Layers** cornerstone
applied to the compiler itself: the compiler "implements the abstract stdlib for the compile-time phase" the same way
jvm implements it for the JVM.

## What blocks it today: platform-blind source unification

The front-end resolves every name against **one pool of all roots**:

```
PathScan (CLI roots + every classpath `eliot/…` resource)
  → ModuleNames / ModuleValue        (per file)
  → UnifiedModuleValue               (per FQN: unify ALL files across ALL roots)
  → resolve → matchdesugar → operator → effect → SaturatedValue   (per FQN)
  → NativeBinding   (checking: ContributedBinding merge; user label reads SaturatedValue.checkingRuntime)
  → TransparentBinding (codegen: reads SaturatedValue.runtime) → used → uncurry → codegen
```

`UnifiedModuleValueProcessor` pools every file for a name and **rejects two concrete definers** outright
(`"Has multiple implementations."`). So a compiler-platform `data Either` and a jvm `data Either` cannot both be concrete
at `eliot.lang.Either` in one pool. And **both `NativeBinding` (checking) and `TransparentBinding` (codegen) read the
same single, platform-blind `SaturatedValue`** — so there is no seam where the compile-time phase could see a different `Either`
than codegen.

To have "two platforms simultaneously" we must make source resolution **platform-scoped**.

## The design: two platforms (`compiler` and `runtime`)

Introduce a **platform** marker as a dimension on the front-end fact keys — effectively a *namespace* the processors
run under. There are exactly two, and **there is no "shared" platform**: because each platform unifies its own
concretes against the base, the **unification result differs per platform**, so there is nothing that can be unified
once and "shared" — a `type Either` resolved with the compiler layer's `data Either` is a different unified value than
the same name resolved with jvm's. The marker is therefore a genuine key dimension, not a shared layer plus overlays.

| platform | files in its pool | drives |
|---|---|---|
| **compiler** | abstract base (`lang`, `stdlib`) + the user's program + the **compiler-platform** module | compile-time evaluation (NbE / type checking) |
| **runtime** | abstract base (`lang`, `stdlib`) + the user's program + the **target-platform** module (jvm) | codegen (`used → uncurry → backend`) |

- The base and the user's program are *physically* visible to both platforms, but they are **unified independently
  within each** — the platform is part of the key, so `(fqn, compiler)` and `(fqn, runtime)` are distinct facts with
  distinct pools. The compiler `Either` and the jvm `Either` never collide; they are parallel implementations of one
  FQN, one per platform.
- Unification is **per platform**: `UnifiedModuleValue.Key(fqn, platform)` pools only the files visible to that
  platform. `"Has multiple implementations."` now only fires when *one* platform has two concrete definers.
- The existing **checking-vs-codegen** split maps onto **compiler-vs-runtime**: the checker's compile-time binding
  resolves in the **compiler** platform (plus the Scala native leaves, which are simply the compiler platform's
  leaves); `TransparentBinding`/`used`/codegen resolve in the **runtime** platform.
- Most names are abstract-only or user code and unify to the same result in both platforms; only platform-provided
  concretes (`Either`, the `Int` representation, …) diverge — and only there does the per-platform pool matter.
- The fact graph is demand-driven, so facts are computed per `(key, platform)` on demand and cached. Processors are
  unchanged beyond carrying the marker — "basically a namespace."

### Mapping onto `ContributedBinding`

The compiler platform becomes a **native-category supplier reading Eliot**, parallel to `StdlibNativesProcessor`
(which reads Scala):

- A new **`compiler`** `ContributedBinding` label, contributed by a new `CompilerNativesProcessor`.
- Unlike `UserValueNativesProcessor` (which reads the *runtime*-platform `SaturatedValue`), `CompilerNativesProcessor`
  reads the **compiler**-platform `SaturatedValue` for the name and emits the evaluable body. It is preferred over the
  `user` label by `BindingMergerProcessor`'s existing native-before-user precedence — giving "compiler reduction wins
  for checking; platform body still used for codegen" with **no change to the merger**.
- The compiler platform's native *leaves* (`add`, `fold`, …) stay where they are (`SystemNativesProcessor` /
  `StdlibNativesProcessor`); they are simply the compiler platform's leaf bottom, just as `nativeAdd*` is jvm's.

## The seam — platform as a key dimension + per-platform roots

Two complementary pieces (both needed, not alternatives):

1. **Platform as a fact-key dimension.** Add a `platform` to the front-end fact keys (`PathScan.Key`,
   `ModuleNames.Key`, `ModuleValue.Key`, `UnifiedModuleValue.Key`, …, `SaturatedValue.Key`). It threads through the
   chain as a namespace; processors are otherwise unchanged. The checker requests `compiler`; `TransparentBinding` /
   `used` / codegen request `runtime`.

2. **Mapping a layer to a platform via resource prefix.** Classpath resources cannot be reliably attributed to their
   originating module (jar/dir/fat-jar), so platform layers ship under a **platform-specific resource prefix**:
   - abstract base (`lang`, `stdlib`) → `eliot/…` (visible to **both** platforms);
   - compiler-platform module → e.g. `eliot-compiler/…` (visible to **compiler** only);
   - target-platform module (jvm) → e.g. `eliot-runtime/…` (visible to **runtime** only).

   `PathScan(path, platform)` then unions the CLI source roots + `eliot/…` (base) + that platform's prefix. This is
   what keeps the compiler platform from seeing jvm's `data Either` (it lives under the runtime prefix) while still
   seeing the abstract base. **Consequence:** the jvm module's resources move out of `eliot/…` into the runtime prefix,
   since they are platform concretes, not base. `lang`/`stdlib` stay at `eliot/…`. (This also removes the
   fat-jar-collapses-same-path-layers hazard the LSP packaging warns about — see
   `[[gotcha_assembly_jar_breaks_layers]]` — because each layer now has a distinct prefix.)

The compiler-platform module must be **always on the classpath** wherever compile-time evaluation runs — the full
driver *and* the LSP server — independent of the chosen target, since type checking always needs it. (Target platforms
like jvm are selected per build; the compiler platform is unconditional.) For a future multi-target setup the runtime
prefix becomes per-target (`eliot-jvm/…`, …) with `runtime` selecting the active one; one target at a time for now.

### Natural codegen exclusion (no special "never emit" plumbing)

`UsedNamesProcessor` walks the **monomorphic runtime graph from `main`**. A name that only ever appears in a
*compile-time* position (a discharged guard signature; see effectful-signatures W2) never enters that graph, so it is
**already** excluded from codegen — we do not need a "compile-time-only / never emit" flag. A compiler-platform name
that *is* also a real runtime type (like `Either`, via `runThrow`) is provided for the runtime platform by the target
platform (jvm) in the ordinary way. The two platforms keep them cleanly apart.

## Work items

### CP1 — Platform-scoped source resolution
Thread `platform` (`compiler` | `runtime`) through the front-end fact chain from `PathScan` to `SaturatedValue`, and
move platform layers to per-platform resource prefixes (base `eliot/…`; compiler `eliot-compiler/…`; jvm
`eliot-runtime/…`) so `PathScan(path, platform)` unions CLI sources + base + that platform's prefix. The jvm module's
resources move under the runtime prefix. Leaf test: a name defined concretely in both the compiler module and the jvm
module resolves with **no** `"Has multiple implementations."` — the `compiler` platform sees one, the `runtime`
platform the other; and a base name resolves identically under both.

### CP2 — The compiler-platform Mill module, always linked
Create a new Mill module (sibling of `jvm`, **depending on `stdlib`** so it has the full abstract stdlib available),
shipping its resources under the compiler prefix. It must be on the classpath of every entry point that type-checks —
the driver and the LSP server — unconditionally (unlike target platforms, which are selected). Initially holds only
CP4's carrier.

### CP3 — `CompilerNativesProcessor` (the `compiler` native label)
A `ContributedBinding` supplier under a new `compiler` label that reads the **compiler**-platform `SaturatedValue` and
emits the evaluable body, contributed into `BindingMergerProcessor` via the existing native roster
(`langNativeLabels`, since it is compiler-owned and always present). Preferred over `user` by the existing precedence.
Leaf test: a name concrete only in the compiler platform reduces during checking; the same name's *runtime* body (if a
platform provides one) is what `TransparentBinding`/codegen uses.

### CP4 — First consumer: the `Either` carrier (effectful-signatures W1)
In the compiler-platform root, in plain Eliot: concrete `data Either = Left | Right`, `foldEither`, and
`implement Monad[Either[String]]` / `implement Throw[String, Either[String]]` (bottoming out in `fold` + the
constructors — the existing native leaves). The abstract `type Either` / `ability Monad` / `ability Throw` stay in
base. jvm keeps its **runtime** `Either` unchanged. This retires the "W1" open question in
`docs/effectful-signatures.md`: the carrier is real, reducing, Eliot, and layer-independent.

## Files (anticipated)

- `source/scan/PathScanner.scala` + `source/scan/PathScan.scala` — CP1 (platform dimension; per-platform prefix union).
- `module/.../UnifiedModuleValueProcessor.scala`, `UnifiedModuleValue.scala`, `ModuleValue`/`ModuleNames` keys,
  `saturate/.../SaturatedValueProcessor.scala` + `SaturatedValue.scala` — CP1 (thread platform).
- `build.mill` + a new compiler-platform module (depends on `stdlib`) + driver/LSP classpath — CP2.
- jvm module resources relocated `eliot/…` → runtime prefix — CP1/CP2.
- `monomorphize/processor/CompilerNativesProcessor.scala` (new) + `monomorphize/fact/ContributedBinding.scala`
  (the `compiler` label, added to `langNativeLabels`) — CP3.
- compiler-platform resources `…/Either.els` (concrete) — CP4; abstract `type Either` + `ability`s added to base
  (`lang`/`stdlib`); jvm `Either.els` (runtime) unchanged but relocated to the runtime prefix.
- `module/fact/WellKnownTypes.scala` — `eitherFQN`/`leftFQN`/`rightFQN` for the W2 discharge to inspect by name.

## Decisions (settled)

1. **Platform is a fact-key dimension** — a namespace threaded through the front-end; processors otherwise unchanged.
2. **No "shared" platform** — exactly `compiler` and `runtime`; each unifies base + user program + its own layer
   independently (per-platform unification, so nothing is shared as a unified artifact).
3. **The compiler platform is a new Mill module depending on `stdlib`** (same level as `jvm`), always on the classpath
   wherever type checking runs (driver + LSP), independent of the selected target.
4. **Identity of `Either`:** the shared FQN `eliot.lang.Either`, with a concrete definer **per platform** (compiler +
   jvm). Structurally identical, resolved independently per platform.

## Open decisions

- **Exact resource-prefix scheme** (single `eliot-runtime/` now vs. per-target `eliot-jvm/…` for a future
  multi-target `runtime`). One target at a time for now, so deferred.
- **How user CLI source threads the platform** — it is visible to both platforms; confirm it needs no marker of its own
  (it is just always in the union for whichever platform is requested).

## Relationship to effectful-signatures

This plan is the foundation `docs/effectful-signatures.md` W1 rides on. Once CP1–CP4 land, effectful-signatures W1 is
"done" by construction (carrier present, reducing, layer-independent), and W2 (the discharge in
`CalculatedReturnResolver`) inspects the resulting `Either[String, Type]` by `leftFQN`/`rightFQN`.
```
