# The Compiler as a Platform: Platform-Scoped Source Unification

Status: **CP1 implemented; CP2–CP4 planned.** The `platform` marker (`compiler | runtime`) is threaded through the
front-end fact chain from `PathScan` to `SaturatedValue`, `PathScanner` selects a per-marker root list, and the
`--compiler-path` / `--runtime-path` CLI options exist (`platform.Platform`, `LangPlugin`). In CP1 the marker defaults
to `runtime` for every existing reader, so behaviour is unchanged; the compiler pool is only exercised by the leaf test
(`module/processor/PlatformScopedUnificationTest`). The blanket classpath scan is *retained* for both markers as the CP1
intermediate (see "What blocks it today" / `PathScanner`) and is retired by CP2 when the compiler-platform module lands.
Motivating first consumer: the compile-time `Either` carrier of `docs/effectful-signatures.md` (W1), which needs a
*reducing* compile-time implementation (`foldEither`, `implement Monad/Throw`) available in **every** workspace —
including the abstract-only LSP workspace — without depending on a runtime platform layer (jvm) being linked.

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
(value-level) phase each resolve against their own platform. The abstract base (`lang`/`stdlib`) stays
representation-free; **each platform fills it in for its phase**. The "no `data` in base" rule is untouched — `data
Either` lives in the *compiler platform layer*, a platform layer like jvm, never in the abstract base. It is the
**Platform-Independence via Layers** cornerstone applied to the compiler itself: the compiler "implements the abstract
stdlib for the compile-time phase" the same way jvm implements it for the JVM.

## What blocks it today: platform-blind source unification

The front-end resolves every name against **one pool of all roots**, discovered two ways:

- `PathScanner`'s `rootPaths` — the CLI `<path>` args (the user's program), resolved on the filesystem.
- a **blanket classpath scan** — `getResources("eliot/" + path)`, which pools *every* classpath resource under `eliot/`.
  Base (`lang`, `stdlib`) **and** jvm both ship their `.els` under `eliot/…`, and `examples` depends on `jvm`
  (`build.mill`), so jvm.jar is on the classpath — meaning base and jvm land in the **same** pool.

```
PathScan (CLI rootPaths + blanket `eliot/…` classpath scan)
  → ModuleNames / ModuleValue        (per file)
  → UnifiedModuleValue               (per FQN: unify ALL files across ALL roots)
  → resolve → matchdesugar → operator → effect → SaturatedValue   (per FQN)
  → NativeBinding   (checking: ContributedBinding merge; user label reads SaturatedValue.checkingRuntime)
  → TransparentBinding (codegen: reads SaturatedValue.runtime) → used → uncurry → codegen
```

`UnifiedModuleValueProcessor` pools every file for a name and **rejects two concrete definers** outright
(`"Has multiple implementations."`). So a compiler-platform `data Either` and a jvm `data Either` cannot both be concrete
at `eliot.lang.Either` in one pool. And **both `NativeBinding` (checking) and `TransparentBinding` (codegen) read the
same single, platform-blind `SaturatedValue`** — so there is no seam where the compile-time phase could see a different
`Either` than codegen. The blanket classpath scan is a development convenience — "find the stdlib without configuring
paths" — and it is exactly what welds base and jvm into one inseparable pool.

To have "two platforms simultaneously" we must make source resolution **platform-scoped**.

## The design: a two-valued phase marker + two explicit source paths

This is deliberately *not* a module/build system, and it deliberately **removes the classpath-scan convenience** rather
than working around it. There is no layer descriptor, no `requires`, no dependency closure. We have two phases and two
explicitly-provided source paths — an honest intermediate on the way to a real build system, with the paths filled
**by hand** for now (in the Mill run / test harness) and computed automatically later.

### The marker

Add a **`platform` marker** to the front-end fact keys — a plain two-valued tag, `compiler | runtime`:

| marker | scans | drives |
|---|---|---|
| **compiler** | the **compiler path** (base — `lang`+`stdlib` — + the compiler platform) | compile-time evaluation (NbE / type checking) |
| **runtime** | the **runtime path** (base + the selected target, jvm, + the user's program) | codegen (`used → uncurry → backend`) |

`CompilerNativesProcessor` reads the **compiler** pool to source each platform concrete's checking-time reduction (the
`add` pattern, below); the **main pipeline** — base, the target, and the user's program — resolves under `runtime`, and
`UserValueNativesProcessor` / `TransparentBinding` / `used` / codegen read that. The marker threads through the chain
(`PathScan.Key`, `ModuleNames.Key`, `ModuleValue.Key`, `UnifiedModuleValue.Key`, …, `SaturatedValue.Key`) as a
namespace; processors are otherwise unchanged.

### Two explicit path lists, filled manually

Replace the single `rootPaths` + blanket `getResources("eliot/…")` discovery with **two explicit, repeatable CLI path
options** — `--compiler-path` and `--runtime-path` (think Java's system path vs. classpath). Each is the **complete**
root list for its phase:

- `--compiler-path` = base (`lang`, `stdlib`) + the **compiler platform** (system-bundled).
- `--runtime-path` = base + the **target** (`jvm`, selected per build) + the **user's program**.

The marker selects which list `PathScanner` scans. **Only base is listed in both**; the compiler platform appears only
in the compiler list, and jvm *and the user's program* only in the runtime list. The user's program does **not** need to
be in the compiler list: the compiler platform supplies the compile-time *reductions* for platform concretes through the
native-label mechanism (the `add` pattern below), so the checker type-checks the program off the runtime pool with those
reductions overlaid — it is never resolved a second time under the compiler marker. For now both lists are **filled by
hand in the Mill run** — each entry points at a module's `resources/eliot` dir — and the eventual build system computes
them. (The compiler path is effectively system-determined, so the user really only configures the runtime path.)

### No relocation, and the symmetric requirement is trivial

Because the lists are explicit, nothing moves and nothing pollutes:

- **No resource-prefix change.** `stdlib` and `jvm` keep their `.els` exactly where they are under `eliot/…`. jvm is
  simply *absent* from the compiler list, so the checker never sees its `Either`; the compiler platform is absent from
  the runtime list, so codegen never sees *its* `Either`. The "symmetric requirement" (keep each platform out of the
  other's pool) is satisfied by *not listing it*, not by a prefix or a relocation.
- **The blanket classpath scan retires** from the driver — it is the workaround we are removing.

### Why this is enough (and validation is unnecessary)

The entry point hands over two complete, explicit lists, so two platforms **cannot** collide in a pool by construction.
There is nothing to validate — no diamonds are expressible, there is no second target, and base is just listed in both. Most names are abstract-only or user code and unify to the same result in both phases; only
platform-provided concretes (`Either`, the `Int` representation, …) diverge, and only there does the marker matter. The
fact graph is demand-driven, so facts are computed per `(key, marker)` on demand and cached.

### Mapping onto `ContributedBinding`

The compiler platform becomes a **native-category supplier reading Eliot**, parallel to `StdlibNativesProcessor`
(which reads Scala):

- A new **`compiler`** `ContributedBinding` label, contributed by a new `CompilerNativesProcessor`.
- Unlike `UserValueNativesProcessor` (which reads the *runtime*-marker `SaturatedValue`), `CompilerNativesProcessor`
  reads the **compiler**-marker `SaturatedValue` for the name and emits the evaluable body. It is preferred over the
  `user` label by `BindingMergerProcessor`'s existing native-before-user precedence — giving "compiler reduction wins
  for checking; platform body still used for codegen" with **no change to the merger**.
- The compiler platform's native *leaves* (`add`, `fold`, …) stay where they are (`SystemNativesProcessor` /
  `StdlibNativesProcessor`); they are simply the compiler platform's leaf bottom, just as `nativeAdd*` is jvm's.

### Natural codegen exclusion (no special "never emit" plumbing)

`UsedNamesProcessor` walks the **monomorphic runtime graph from `main`**. A name that only ever appears in a
*compile-time* position (a discharged guard signature; see effectful-signatures W2) never enters that graph, so it is
**already** excluded from codegen — we do not need a "compile-time-only / never emit" flag. A compiler-platform name
that *is* also a real runtime type (like `Either`, via `runThrow`) is provided for the runtime marker by the target
platform (jvm) in the ordinary way. The two markers keep them cleanly apart.

## Work items

### CP1 — Two explicit source paths + the phase marker — **implemented**
Thread a two-valued `platform` marker (`compiler | runtime`) through the front-end fact chain from `PathScan` to
`SaturatedValue`. Replace `PathScanner`'s single `rootPaths` + blanket `getResources("eliot/…")` scan with **two
explicit root lists** — a compiler path and a runtime path — selected by the marker; add `--compiler-path` /
`--runtime-path` CLI options (`LangPlugin` parser) and fill both **by hand** in the Mill `examples.run` task and the
test harness, each entry pointing at the relevant modules' `resources/eliot` dir. No resource relocation: base is listed
in **both** lists; the compiler platform only in the compiler path; jvm and the user's program only in the runtime path
(the program is checked off the runtime pool, with the compiler platform's compile-time reductions overlaid via the
native label). Leaf test: a name defined concretely in **both** the compiler platform and jvm resolves with **no**
`"Has multiple implementations."` — the `compiler` marker scans one list, `runtime` the other; and a base name resolves
identically under both.

### CP2 — The compiler-platform Mill module, always linked
Create a new Mill module (sibling of `jvm`, **depending on `stdlib`** so it has the full abstract stdlib available),
shipping its `.els` under `resources/eliot/…`. Its resource dir is listed on the **compiler path** of every entry point
that type-checks — the driver and the LSP server — unconditionally (unlike target platforms, which are selected per
build). Initially holds only CP4's carrier.

### CP3 — `CompilerNativesProcessor` (the `compiler` native label)
A `ContributedBinding` supplier under a new `compiler` label that reads the **compiler**-marker `SaturatedValue` and
emits the evaluable body, contributed into `BindingMergerProcessor` via the existing native roster
(`langNativeLabels`, since it is compiler-owned and always present). Preferred over `user` by the existing precedence.
Leaf test: a name concrete only in the compiler platform reduces during checking; the same name's *runtime* body (if a
platform provides one) is what `TransparentBinding`/codegen uses.

### CP4 — First consumer: the `Either` carrier (effectful-signatures W1)
In the compiler-platform root, in plain Eliot: concrete `data Either = Left | Right`, `foldEither`, and
`implement Monad[Either[String]]` / `implement Throw[String, Either[String]]` (bottoming out in `fold` + the
constructors — the existing native leaves). The abstract `type Either` / `ability Monad` / `ability Throw` stay in
base. jvm keeps its **runtime** `Either` unchanged and in place. This retires the "W1" open question in
`docs/effectful-signatures.md`: the carrier is real, reducing, Eliot, and layer-independent.

## Files (anticipated)

- `source/scan/PathScanner.scala` + `source/scan/PathScan.scala` — CP1 (platform marker; two explicit root lists, select
  by marker; retire the blanket classpath scan).
- `plugin/LangPlugin.scala` — CP1 (`--compiler-path` / `--runtime-path` CLI options + their `Configuration.Key`s).
- `build.mill` — CP1/CP2 (the `examples.run` task and the test harness fill both path lists from module `resources/eliot`
  dirs; a new compiler-platform module depending on `stdlib`).
- `module/.../UnifiedModuleValueProcessor.scala`, `UnifiedModuleValue.scala`, `ModuleValue`/`ModuleNames` keys,
  `saturate/.../SaturatedValueProcessor.scala` + `SaturatedValue.scala` — CP1 (thread the marker).
- `monomorphize/processor/CompilerNativesProcessor.scala` (new) + `monomorphize/fact/ContributedBinding.scala`
  (the `compiler` label, added to `langNativeLabels`) — CP3.
- compiler-platform resources `…/Either.els` (concrete) — CP4; abstract `type Either` + `ability`s added to base
  (`lang`/`stdlib`); jvm `Either.els` (runtime) unchanged and in place.
- `module/fact/WellKnownTypes.scala` — `eitherFQN`/`leftFQN`/`rightFQN` for the W2 discharge to inspect by name.

## Decisions (settled)

1. **Platform is a two-valued fact-key marker** (`compiler | runtime`) — a namespace threaded through the front-end;
   processors otherwise unchanged. No closure set, no descriptor.
2. **Two explicit source paths, filled manually** — `--compiler-path` / `--runtime-path`, each the *complete* per-phase
   root list; base listed in both, the compiler platform only in the compiler path, jvm *and the user's program* only in
   the runtime path. The blanket `eliot/…` classpath scan (a development convenience) is **retired** from the driver.
3. **No resource relocation** — `stdlib` and `jvm` keep their `.els` under `eliot/…`; separation is by *which list lists
   them*, never by a prefix. The compiler path is system-determined; in practice the user only configures the runtime
   path.
4. **No validation** — two complete explicit lists are supplied, so two platforms can't collide; base is simply in both,
   and the user's program lives only in the runtime list (checked off the runtime pool, with the compiler platform's
   compile-time reductions overlaid via the native label).
5. **The compiler platform is a new Mill module depending on `stdlib`** (same level as `jvm`), on the compiler path of
   every entry point that type-checks (driver + LSP), independent of the selected target.
6. **Identity of `Either`:** the shared FQN `eliot.lang.Either`, with a concrete definer **per platform** (compiler +
   jvm). Structurally identical, resolved independently per marker.

## Deferred — a real build system

The manual two-path step is an explicit waypoint, not the destination. Two things it leaves open, both to be addressed
when a real build system lands:

- **Packaged distributions.** Filesystem `Path`s can't `resolve` *into* a jar, so a packaged distribution (the LSP dist,
  the generated exe-jar tooling) cannot use plain filesystem paths the way the Mill dev run can. The explicit-path mode
  is the **dev/Mill intermediate**; packaged tooling keeps classpath discovery until the build system replaces it (or we
  add jar-aware root handling). Likewise the test harness, which gets resources from the test classpath today.
- **Computing the paths.** The build system computes the two lists from module dependencies instead of hand-filling them
  in `build.mill`.

If a *second* runtime target or a layer shared by several platforms ever appears (e.g. `arduino-common` feeding both
`arduino-uno` and `arduino-mega`), the flat "one path per platform" map stops expressing it and we generalize — *not
before*. The shape that generalization would take, captured so it isn't re-derived:

- **Layer descriptors** — each layer (a jar/dir) carries a small descriptor (`name`, `requires`); attribution is by
  container root; all layers can stay at `eliot/…`. (Needs separate roots — no fat jar — which we already mandate.)
- **The marker becomes a dependency closure** — the canonical set of layer ids in scope (leaf = request, closure =
  key), so a shared intermediary appears in several closures and abstract diamonds dedup as a set.
- **A validity invariant** — exactly one platform chain per closure and a single consistent base spine; abstract-only
  diamonds allowed; two conflicting platform/base spines banned. A program may be platform-specific (a `requires` edge
  to a platform), which just constrains its legal targets; usually the target is supplied at build time and unnamed.

Crucially, **the marker stays the seam** in every one of these steps — only the source of the per-marker roots changes
(hand-filled CLI paths → build-system-computed paths → descriptor closures) — so none of this is a front-end rewrite,
and the manual two-path start is not a one-way door.

## Open decisions

- **CLI option shape** — repeatable `--compiler-path` / `--runtime-path` vs. a single delimited value; and the exact
  relationship to the existing positional `<path>` arg (likely the user's program, folded into the runtime path).
- **Test harness** — fill the two paths from the on-disk module `resources/eliot` dirs, or keep classpath discovery for
  tests that don't yet exercise the marker split.

## Relationship to effectful-signatures

This plan is the foundation `docs/effectful-signatures.md` W1 rides on. Once CP1–CP4 land, effectful-signatures W1 is
"done" by construction (carrier present, reducing, layer-independent), and W2 (the discharge in
`CalculatedReturnResolver`) inspects the resulting `Either[String, Type]` by `leftFQN`/`rightFQN`.
