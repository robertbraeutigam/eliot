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
(value-level) phase each resolve under their own platform. The abstract base (`lang`/`stdlib`) stays representation-free;
**each platform fills it in for its phase**. The "no `data` in base" rule is untouched — `data Either` lives in the
*compiler platform layer*, a platform layer like jvm, never in the abstract base. It is the **Platform-Independence via
Layers** cornerstone applied to the compiler itself: the compiler "implements the abstract stdlib for the compile-time
phase" the same way jvm implements it for the JVM.

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
same single, platform-blind `SaturatedValue`** — so there is no seam where the compile-time phase could see a different
`Either` than codegen.

To have "two platforms simultaneously" we must make source resolution **platform-scoped**.

## The design: layers, platforms, and the dependency closure

### Layers carry a descriptor; there is no per-platform prefix

A **layer** is a root — a jar or a directory — that carries a **descriptor** at a fixed, well-known resource path
(e.g. `META-INF/eliot-layer.properties`), declaring the layer's **name** and the layers it **requires**:

```
name = jvm
requires = stdlib
```

Every layer keeps its Eliot resources where they already are, under `eliot/…`. There is **no `eliot-compiler/…` /
`eliot-runtime/…` prefix relocation** — the descriptor makes a layer's identity and edges *explicit data* instead of an
implicit naming convention, and nothing physically moves. (An earlier draft of this plan used per-platform prefixes;
descriptors supersede them — see "Why descriptors, not prefixes" below.)

There are **two kinds of layer**:

- **Abstract layers** — signatures only (`type X` with no body, `def` with no body). `lang`, `stdlib`, third-party
  libraries, **and the user's program**. These form the `requires` spine and are platform-neutral.
- **Platform layers** — provide bodies / `data`. `jvm`, the new `compiler` layer, future `arduino-common ← arduino-uno`.
  They *realize* the abstract spine and are what makes a phase concrete.

The **user's program is a layer too**, with its own descriptor. We need it: without one we cannot tell what the program
is written against, and therefore cannot validate it against a chosen platform.

### A platform is a dependency closure; the marker is that closure

The two *phases* — compile-time checking and runtime codegen — each resolve under a **set of layers**: the **dependency
closure** of a requested **platform leaf**, unioned with the always-present program and base.

| phase | requested leaf | closure (the marker) | drives |
|---|---|---|---|
| **checking** | `compiler` | `{program, …libs, stdlib, lang, compiler}` | compile-time evaluation (NbE / type checking) |
| **codegen** | the selected target (`jvm`, later `arduino-uno`, …) | `{program, …libs, stdlib, lang, jvm}` | codegen (`used → uncurry → backend`) |

The **leaf is only the request**; the **closure is the key**. The thing threaded through the front-end fact keys is the
canonical (sorted) **set of layer ids** — the closure — not the leaf label and not a fixed `{compiler, runtime}` enum.
This matters:

- **The leaf underdetermines the pool.** A program may pull in abstract libraries (`collections`, …) that are *not* on
  the platform's chain, so the leaf alone does not name the file set; the closure does.
- **It is the honest key.** `UnifiedModuleValue(fqn, X)` resolves differently across `X` *because* `X` pulls a different
  file set. Making `X` literally be that set gives equal-closures ⇒ shared-facts caching by construction, instead of
  riding on an implicit "ambient program."
- **Abstract diamonds dissolve for free.** A closure is a **set**, so `collections → stdlib` and `program → stdlib`
  (a harmless diamond) dedup to one `stdlib` entry — no special handling.

Both phases of one compile differ only in their platform layer (`…, compiler` vs `…, jvm`), so abstract/user names
resolve twice — once per closure. That double computation is accepted (the fact graph is demand-driven and cached per
key); the closure marker just makes *why* they are distinct keys explicit.

### The structure: program on top, platform spliced deep

The program is **always on top**. Its abstract `requires` does not sit immediately above the platform — it only has to
be **realized somewhere down the target's chain**, which can be many concrete layers deep:

```
myapp            ← program: requires stdlib (abstract); always the top
  │
arduino-uno      ← target (the requested leaf) — platform leaf
  │
arduino-common   ← platform intermediary   ┐ "even more steps beneath"
  │              ←  …more…                  ┘
stdlib           ← JOIN: where myapp's `requires` is realized
  │
lang             ← base
```

A **platform-specific program** — one that uses jvm-specific `data`/functions — simply `requires` a platform layer,
which makes the join *shallow* (the program reaches into the platform itself) and constrains its legal targets:

```
myapp  →  jvm  →  stdlib  →  lang        (requires jvm ⇒ jvm is the only legal target)
```

So **"join depth" is just how far down the target's chain the program's `requires` lands** — at the abstract base for
portable programs (deep, many platform intermediaries), inside the platform for locked programs (shallow). One mechanism,
no branch in the logic. "Named in the program" (a `requires` edge to a platform) and "supplied at compile time" (a leaf
requested by the driver) are the **same mechanism**; they differ only in how much they pin down the target.

### The invariant

A closure is **valid** iff:

1. it contains **exactly one platform chain** (one realization), and
2. all abstract layers in it **agree on the same base spine** (identical layers from the join down to `lang`).

**Abstract-only diamonds are allowed** (a program drawing on two independent abstract libraries that share `stdlib`):
still one platform, still one base — and the merge is **order-independent** (prefer-the-one-concrete, error on two — see
`UnifiedModuleValue`), so the "stack" is purely a *closure/validation* device. We never compute a linear order to merge;
we compute the **set** and check the invariant. What is banned is precisely **two conflicting platform/base spines** in
one closure (e.g. a program that drags in a jvm-locked library while targeting arduino) — which the unifier would
otherwise surface as `"Has multiple implementations."`.

### Why descriptors, not prefixes

Per-layer prefixes (`eliot-jvm/…`) would also separate layers, and would even survive a fat jar (distinct paths).
Descriptors do **not** survive a fat jar — both the descriptors and any redefined `Either.els` collide at one path and
one silently wins. The trade is worth it because **we already mandate separate per-module jars**
(`[[gotcha_assembly_jar_breaks_layers]]`, the LSP `package.sh`), and descriptors buy two things prefixes do not: the
layer DAG becomes explicit *data* (no path-convention encoding, nothing relocates), and a collapsed fat jar is
**detectable** (a root whose descriptor says `name = jvm` but that also contains base files, or two descriptors
collapsing to one) so we can fail loudly instead of silently dropping a layer.

### Mapping onto `ContributedBinding`

The compiler platform becomes a **native-category supplier reading Eliot**, parallel to `StdlibNativesProcessor`
(which reads Scala):

- A new **`compiler`** `ContributedBinding` label, contributed by a new `CompilerNativesProcessor`.
- Unlike `UserValueNativesProcessor` (which reads the *codegen*-closure `SaturatedValue`), `CompilerNativesProcessor`
  reads the **checking**-closure `SaturatedValue` for the name and emits the evaluable body. It is preferred over the
  `user` label by `BindingMergerProcessor`'s existing native-before-user precedence — giving "compiler reduction wins
  for checking; platform body still used for codegen" with **no change to the merger**.
- The compiler platform's native *leaves* (`add`, `fold`, …) stay where they are (`SystemNativesProcessor` /
  `StdlibNativesProcessor`); they are simply the compiler platform's leaf bottom, just as `nativeAdd*` is jvm's.

## The seam — closure as a key dimension + descriptor-driven roots

Two complementary pieces (both needed, not alternatives):

1. **The closure as a fact-key dimension.** Add the closure (canonical sorted set of layer ids) to the front-end fact
   keys (`PathScan.Key`, `ModuleNames.Key`, `ModuleValue.Key`, `UnifiedModuleValue.Key`, …, `SaturatedValue.Key`). It
   threads through the chain as a namespace; processors are otherwise unchanged. The checker requests the `compiler`
   leaf; `TransparentBinding` / `used` / codegen request the selected target leaf — each request expands to a closure,
   and that closure is what is carried.

2. **Descriptor-driven discovery, attribution, and closure.** `PathScanner` no longer assumes a single `eliot/…` pool:
   - **Discover** layers: `getClassLoader.getResources("META-INF/eliot-layer.properties")` returns exactly one URI per
     layer root. Parse each → `(name, requires, rootUri)`, where `rootUri` is the descriptor URI minus its own suffix.
     Build the layer DAG once. (CLI source roots without a descriptor are the program-under-compilation, always unioned;
     a CLI root *with* one is a layer under development — e.g. building the jvm layer from `out/jvm/` in tests.)
   - **Closure**: for a requested leaf, walk `requires` transitively (over discovered layers) and union the
     always-present program + base → the closure set, validated against the invariant.
   - **Attribute & filter**: resolve `eliot/<path>` as today (`getResources`), then keep only URIs whose **container
     root** is in the closure. A pooled resource URI carries its container — `jar:file:/jvm.jar!/eliot/…` → root
     `jar:file:/jvm.jar!/`; `file:/out/jvm/eliot/…` → root `file:/out/jvm/` — recovered by stripping the known
     `eliot/<path>` suffix, the same root the descriptor produced. (The plan's old "cannot be reliably attributed" was
     only ever true for fat jars, which we forbid.)

```scala
// closure(leaf) walks `requires` transitively over the discovered layer DAG + program + base
val rootsInScope = closure(key.platform).map(_.rootUri)
val resourceUris = allMatching.filter(u => rootsInScope.exists(u.startsWith))
```

The `compiler` layer must be **always on the classpath** wherever compile-time evaluation runs — the full driver *and*
the LSP server — independent of the chosen target, since type checking always needs it. (Target platforms like jvm are
selected per build; the compiler platform is unconditional.)

### Natural codegen exclusion (no special "never emit" plumbing)

`UsedNamesProcessor` walks the **monomorphic runtime graph from `main`**. A name that only ever appears in a
*compile-time* position (a discharged guard signature; see effectful-signatures W2) never enters that graph, so it is
**already** excluded from codegen — we do not need a "compile-time-only / never emit" flag. A compiler-platform name
that *is* also a real runtime type (like `Either`, via `runThrow`) is provided for the codegen closure by the target
platform (jvm) in the ordinary way. The two closures keep them cleanly apart.

## Work items

### CP1 — Layer descriptors + dependency-closure resolution
Give every layer a descriptor (`META-INF/eliot-layer.properties`: `name` + `requires`), **including the user program**.
Thread the dependency **closure** (canonical sorted set of layer ids) through the front-end fact chain from `PathScan`
to `SaturatedValue` as the platform marker. `PathScanner` discovers layers by their descriptors, computes the closure
for a requested leaf (+ always-present program + base), validates the invariant (one platform chain; consistent base
spine; abstract diamonds OK), and filters the pooled `eliot/…` resources to the closure's container roots. **No resource
relocation** — `jvm` stays at `eliot/…`, it just gains a descriptor. Leaf test: a name defined concretely in **both** the
compiler layer and the jvm layer resolves with **no** `"Has multiple implementations."` — the `compiler` closure sees
one, the `jvm` closure the other; a base name resolves identically under both closures; and an abstract-only diamond
(`program → lib → stdlib` and `program → stdlib`) resolves once.

### CP2 — The compiler-platform Mill module, always linked
Create a new Mill module (sibling of `jvm`, **depending on `stdlib`** so it has the full abstract stdlib available),
shipping its resources under `eliot/…` with a `compiler` descriptor (`requires = stdlib`). It must be on the classpath
of every entry point that type-checks — the driver and the LSP server — unconditionally (unlike target platforms, which
are selected). Initially holds only CP4's carrier.

### CP3 — `CompilerNativesProcessor` (the `compiler` native label)
A `ContributedBinding` supplier under a new `compiler` label that reads the **checking**-closure `SaturatedValue` and
emits the evaluable body, contributed into `BindingMergerProcessor` via the existing native roster
(`langNativeLabels`, since it is compiler-owned and always present). Preferred over `user` by the existing precedence.
Leaf test: a name concrete only in the compiler layer reduces during checking; the same name's *runtime* body (if a
platform provides one) is what `TransparentBinding`/codegen uses.

### CP4 — First consumer: the `Either` carrier (effectful-signatures W1)
In the compiler-platform layer, in plain Eliot: concrete `data Either = Left | Right`, `foldEither`, and
`implement Monad[Either[String]]` / `implement Throw[String, Either[String]]` (bottoming out in `fold` + the
constructors — the existing native leaves). The abstract `type Either` / `ability Monad` / `ability Throw` stay in
base. jvm keeps its **runtime** `Either` unchanged (and stays at `eliot/…`, now with a `jvm` descriptor). This retires
the "W1" open question in `docs/effectful-signatures.md`: the carrier is real, reducing, Eliot, and layer-independent.

## Files (anticipated)

- `source/scan/PathScanner.scala` + `source/scan/PathScan.scala` — CP1 (closure dimension; descriptor discovery,
  attribution by container root, closure filter).
- `module/.../UnifiedModuleValueProcessor.scala`, `UnifiedModuleValue.scala`, `ModuleValue`/`ModuleNames` keys,
  `saturate/.../SaturatedValueProcessor.scala` + `SaturatedValue.scala` — CP1 (thread the closure marker).
- New `source/.../LayerDescriptor.scala` (+ discovery/closure/invariant) — CP1.
- Descriptors added to each layer root: `lang`, `stdlib`, `jvm`, the new `compiler` module, and the user program's
  source root — CP1/CP2. (No resource relocation.)
- `build.mill` + a new compiler-platform module (depends on `stdlib`) + driver/LSP classpath — CP2.
- `monomorphize/processor/CompilerNativesProcessor.scala` (new) + `monomorphize/fact/ContributedBinding.scala`
  (the `compiler` label, added to `langNativeLabels`) — CP3.
- compiler-platform resources `…/Either.els` (concrete) — CP4; abstract `type Either` + `ability`s added to base
  (`lang`/`stdlib`); jvm `Either.els` (runtime) unchanged.
- `module/fact/WellKnownTypes.scala` — `eitherFQN`/`leftFQN`/`rightFQN` for the W2 discharge to inspect by name.

## Decisions (settled)

1. **The marker is the dependency closure** — the canonical sorted set of layer ids threaded through the front-end as a
   namespace; the requested **leaf is the request, the closure is the key**. Not a fixed `{compiler, runtime}` enum.
2. **Layers carry descriptors, not prefixes** — `name` + `requires` at a fixed resource path; all layers stay at
   `eliot/…`; layer identity is recovered by attributing each resource to its container root. Requires separate roots
   (no fat jar), which we already mandate.
3. **Two layer kinds; the program is a layer** — abstract (signatures only: `lang`, `stdlib`, libraries, the program)
   vs platform (`jvm`, `compiler`, `arduino-*`). The program has its own descriptor so it can be validated.
4. **Validity invariant** — a closure must contain exactly one platform chain and a single consistent base spine;
   abstract-only diamonds are allowed (the merge is order-independent). Two conflicting platform/base spines are banned.
5. **Platform = supplied or named, same mechanism** — the target is usually a leaf requested at compile time (the
   driver's `jvm` arg) and *not* named in the program; a platform-specific program may `require` a platform layer
   directly, which just constrains its legal targets.
6. **The compiler platform is a new Mill module depending on `stdlib`** (same level as `jvm`), always on the classpath
   wherever type checking runs (driver + LSP), independent of the selected target.
7. **Identity of `Either`:** the shared FQN `eliot.lang.Either`, with a concrete definer **per platform** (compiler +
   jvm). Structurally identical, resolved independently per closure.

## Open decisions

- **Exact descriptor format and path** (`META-INF/eliot-layer.properties` vs `eliot/layer.conf`; properties vs a small
  structured format). Minor.
- **How the program's descriptor is provided for CLI source roots** — a well-known file at the source root, and what to
  default to when absent (likely "abstract program requiring the base").
- **How the requested leaf reaches `PathScanner`** — confirm it is the existing CLI target arg for codegen and a fixed
  conventional `compiler` for checking; no per-name marker needed (the program is always in every closure).
- **Whether to detect a collapsed fat jar loudly now or later** — descriptors make it detectable; deciding when to wire
  the check.
- **Multi-target later** — `runtime` becomes per-target (one leaf per target: `jvm`, `arduino-uno`, …) selected per
  build; one target at a time for now.

## Relationship to effectful-signatures

This plan is the foundation `docs/effectful-signatures.md` W1 rides on. Once CP1–CP4 land, effectful-signatures W1 is
"done" by construction (carrier present, reducing, layer-independent), and W2 (the discharge in
`CalculatedReturnResolver`) inspects the resulting `Either[String, Type]` by `leftFQN`/`rightFQN`.
