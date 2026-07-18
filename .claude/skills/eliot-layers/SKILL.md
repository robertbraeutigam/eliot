---
name: eliot-layers
description: Use when adding, moving, splitting, renaming, or deleting Eliot `.els` source files — i.e. deciding which layer (stdlib / jvm / compiler / lang) a `type`, `def`, `data`, `ability`, or `implement` belongs in, and what must be kept abstract or duplicated. Covers the layer/platform model, the two-pool (compiler vs runtime) resolution that decides what must resolve where, the abstract↔concrete merge, signature-merge gotchas, sanctioned duplication, and the verify recipe.
---

# Eliot layers: placing & moving `.els` files

Read the **"Platform-Independence via Layers"** cornerstone in CLAUDE.md first — it owns the *why* (layers =
redefinition not inheritance; merge prefers the concrete). This skill is the *operational* layer: where things
go, what must resolve where, and the mechanical traps.

## The layers (each is a `--path` root shipping an `eliot/` source root; some also ship an `eliot-compiler/` sibling)

A module `eliot.lang.X` maps to `<layer>/eliot/eliot/lang/X.els` (path = `ModuleName.toPath`). The same name
in several layers/roots is **merged**. There is **no standalone `compiler` Mill module** — a layer's compile-time
contribution is a sibling `eliot-compiler/` root next to its `eliot/` root (today `stdlib` — `Either`/`Option`, the
compile-time `AbortCarrier` for return guards, the `Interval` refinement instances — and `lang` — the compile-time
`Id` carrier — ship one).

| Layer / root | Holds | On which pool |
|-------|-------|---------------|
| `lang/eliot` | core compiler (Scala) **and** compiler-owned `.els` (`eliot.compiler.{Meta,Reflect,Type}`, `eliot.compiler.internal.{PatternMatch,TypeMatch}`, the `Eq[Type]` instance, `Bool`'s `true`/`false`, `Id`, `eliot.collection.List`) | **both** |
| `stdlib/eliot` | the platform-independent base: abstract `type`s, body-less `def` signatures, `ability` decls, pure platform-independent bodies/instances | **both** |
| `jvm/eliot` | the runtime platform: concrete `data`, `def` bodies, native leaves, ability `implement`s, carrier cross-lift instances | runtime only (**borrowed** into the compiler pool) |
| `stdlib/eliot-compiler` | the compile-time overlay: `data`/instances the NbE checker must *evaluate at compile time* that **aren't borrowable** — the self-sufficient `Either`/`Option` carriers, the compile-time `AbortCarrier` (reduces `if..else raise` return guards), the `Interval` `Meta`/`Numeric` instances, and the compile-time `Effect`/`Throw` instances | compiler only (**override**) |

## The two pools — the operational key

There is **one** list of source roots (each `--path`, plus the positional program → `LangPlugin.pathKey`). Both
pools derive from it:

- **runtime pool** (codegen + the user program) = every root's `eliot/`.
- **compiler pool** (type checking / NbE) = **the whole runtime pool, *borrowed*** *plus* each root's sibling
  `eliot-compiler/` overlay, which **override-supersedes** the borrowed definition of the same name.

Merge happens **per pool** (`UnifiedModuleValueProcessor`, keyed on `platform`; overlay files carry
`PathScan.overrideFiles`, so an overlay def wins). Two consequences drive every placement decision:

> **Anything in `stdlib` must resolve on BOTH pools.** Because the compiler pool contains the whole runtime track,
> a stdlib reference to a **pure** runtime name (a jvm `data`/fold, a base body) resolves at compile time by
> **borrowing** it — no compiler copy needed. Borrowing fails only for a name that reaches a *runtime-only leaf* (a
> bytecode op) when forced at compile time; that **stalls loudly** (the native-leaf boundary), never silently wrong.

> **A layer's compile-time track must be self-sufficient from base + its own `eliot-compiler/`.** It may borrow the
> program and pure base bodies, but **not** a sibling target (jvm) that might be absent. So a compile-time carrier
> stdlib genuinely needs — the `Either` error monad + its `Effect`/`Throw` instances, the `Option` the guards reduce
> through — lives in `stdlib/eliot-compiler/` and duplicates jvm's runtime copy (sanctioned). Anything pure and
> already on the path (`Pair`, base bodies) is **borrowed**, not duplicated.

## Where does X go?

**Quick heuristic:** *would a backend with a totally different representation (JavaScript — one numeric type;
an 8-bit MCU) need this?* If no, it's platform-specific → `jvm` (or the relevant runtime layer), never stdlib.

| You're adding… | Put it in | Notes |
|---|---|---|
| abstract `type X` (or `type X = alias`) | **stdlib** | so any signature can mention `X` on both pools |
| concrete `data X` (representation + ctor) | **jvm** (runtime); **+ `stdlib/eliot-compiler`** only if the checker needs a *self-sufficient* compile-time carrier (e.g. `Either`) | never stdlib base |
| field accessor / `foldX` eliminator | with the `data` (jvm; + `stdlib/eliot-compiler` for a self-sufficient carrier) | pure ones are **borrowed** at compile time — add a copy only for self-sufficiency, an abstract twin only if a non-borrowable name is referenced |
| pure body, same on every target (composition over abstract/native ops) | **stdlib** | e.g. `catch`, `else`, `updateState`, Function `.` |
| native leaf (backend/compiler supplies the body: bytecode op, arbitrary-precision arith, `printLineInternal`) | **jvm** (runtime) / Scala `*NativesProcessor` (compiler leaves) | the layer "bottom" |
| representation-dependent body (layout/carrier choice) | **jvm** | e.g. `printLine = IO(_ -> printLineInternal(s))` (commits to the jvm `IO` representation) |
| `ability X` | **stdlib** | re-declared (copied) in any layer file hosting an `implement X` — see duplication below |
| `implement X[T]` with a runtime/value payload | with `T`'s `data` (jvm) or `X`'s module | body needs `T`'s ctor, which lives with the `data` |
| pure compile-time-only `implement` (no runtime payload) | **stdlib** | e.g. `Meta[Unit]` in `Unit.els` |
| **body-less** `implement X[T]` (methods are native leaves attached per-platform) | **stdlib** (base) | representation-free *declaration*, so it lives once and both pools borrow it; each platform attaches the leaf (jvm backend + `*NativesProcessor`). e.g. `Eq[String]` in `String.els`, `Compare[BigInteger]`/`Numeric[BigInteger]` — even though value-level |
| effect/carrier `implement` **with an Eliot body** | **jvm** | e.g. the carrier cross-lifts (`Throw[E2, ThrowCarrier[E1, G]] where E1 != E2`) |
| something the compiler must evaluate at compile time, expressible in Eliot | **borrow** it if pure & already on the path; else the owning layer's **`eliot-compiler/`** (base names → `stdlib/eliot-compiler`) | not Scala `SemValue`s — the one NbE evaluator runs it |

### Ability module vs. type module — which of the two allowed homes

An `implement X[T]` may sit with ability `X` *or* with type `T` (the coherence rule above). **Default to the
type's module.** The organizing principle:

- **The ability's module holds the ability declaration + the convenience/feature functions built *on top of*
  it** — the derived combinators every implementation inherits for free (e.g. `Compare`'s `<`/`>`/`min`/`max`
  derive over `lessThanOrEqual`; `Numeric`'s `+`/`-`/`*` operators delegate to `add`/`subtract`/`multiply`).
  These are generic over the ability, not tied to any one instance.
- **A data type's module holds *that type's* implementations of the various abilities it supports** — `Int`'s
  `implement Numeric[Int]`/`Show[Int]`/`Compare[Int]` live in `Int.els`; `BigInteger`'s
  `implement Numeric[BigInteger]`/`Compare[BigInteger]` live in `BigInteger.els`. This keeps each ability module
  instance-agnostic and each type module the one place to see everything that type can do.

Put an instance in the **ability's** module only when it genuinely can't colocate with the type — a
carrier-*generic* instance (`implement[F[_] ~ Suspend] Console[F]`, no single concrete `T`), or an instance for a
bare abstract type declared in another layer.

## The merge, mechanically

For each name, `UnifiedModuleValueProcessor.unifyValues` collects every layer's `ModuleValue` for that pool:
- **>1 concrete** (`runtime.isDefined`) → `"Has multiple implementations."`
- **signatures differ** → `"Has multiple different definitions."`
- else keep the concrete one (or the abstract one if none).

So a layer may **add a body but must not change the signature** — and the concrete def must **repeat the
whole signature**, including fixity/precedence (`infix left …`), because the merge takes the entire
`NamedValue`, not just the body.

### An `implement` can be *split* across layers

An ability implementation's identity is `(ability, type-args pattern + guard)` — not source order or position (see
[[reference_ability_impl_identity]]). So the **same `implement X[T]` may be split across layers** and its members merge
exactly like plain names: put the associated types + abstract method signatures in the base, the method *bodies* in a
platform layer. Both `implement` blocks must spell the **same pattern with the same generic names** (character-exact, so
their keys and signatures match — the usual merge discipline). Per member: `type Assoc = …` concrete in the base +
`type Assoc` abstract in the platform → merges to the base's; `def m(…): Assoc` abstract in the base + `def m(…): Assoc =
body` concrete in the platform → merges to the platform's. `Numeric[Interval[T]]` is the worked example (base owns the
body-less method signatures, jvm owns the endpoint-wise bodies). A **bare** associated-type reference inside a method
signature (`def m(…): Assoc`) auto-resolves to the concrete type (the resolver applies the impl generics) — you do
*not* re-apply the impl generics to it. Two *identical* patterns in one file are the **same** identity → a duplicate-name
error, not two instances; distinct-but-unifiable patterns are what the overlap lint checks.

## Mechanical gotchas

- **`signatureEquality` is core-structural** (`NamedValue.signatureEquality` → `Expression.structuralEquality`
  on `typeStack.signature`), and **binder + generic-parameter names participate** (`FunctionLiteral` compares
  `p.value`; `GenericParameter`/`ArgumentDefinition` compare `.name.value`). To unify an abstract declaration
  with a `data`-generated member you must mirror it **exactly**.
- **Abstract twin of a `data` accessor**: the generated accessor (`DataDefinitionDesugarer`) names its single
  parameter **`obj`** and reuses the data's generic names. So an abstract `def runThrow[E, G[_], A](obj:
  ThrowCarrier[E, G, A]): G[Either[E, A]]` unifies; `(p: …)` or renamed generics will **not**.
- **Sanctioned duplication**: an `implement X` must be colocated with ability `X` (its module) or with the
  target type. Name resolution is per-file, so that file must **re-declare (copy)** `ability X` itself; the
  merge verifies the copies agree. This is correct — do **not** "fix" it by widening the resolver across
  sibling files. (Likewise an `implement` that uses a carrier's ctor must sit with that carrier's `data`.)
- **stdlib's real rule is "no platform *representation*," not "no bodies."** Pure platform-independent bodies
  (`catch`/`else`, `updateState`, `.`) and pure compile-time-only instances belong in stdlib. What stdlib must
  never carry: `data`, native leaves, or representation-dependent bodies.
- **Carrier↔effect import cycles are fine** (no cyclic facts). Two ability `implement`s sharing a method name
  in one module collide their generated lambda classes at JAR time — keep each carrier's instances in its own
  module ([[gotcha_lambda_class_collision_same_module]]). Never bundle layers into one fat/assembly jar — it
  collapses same-path layer resources ([[gotcha_assembly_jar_breaks_layers]]).

## Moving / adding a file — checklist

1. Decide the layer(s) from the table above; split abstract (`stdlib`) vs concrete (`jvm`, + `stdlib/eliot-compiler`
   only for a self-sufficient compile-time carrier) if needed.
2. If a stdlib body references, at compile time, a name that **can't be borrowed** (it reaches a runtime-only leaf)
   and has **no `eliot-compiler` overlay**, add the abstract declaration in stdlib (exact-match signature for any
   `data` member — see gotchas), or an overlay copy. A *pure* referenced name is borrowed and needs nothing.
3. Keep `ability` + its `implement`s colocated; copy the `ability` decl into any file that hosts an instance.
4. If you emptied a file by moving its contents, **delete** it.
5. Centralized Scala FQN sites a *renamed/moved* module may appear in (only if the module name/package changes):
   `module/fact/ModuleName.scala` (`defaultSystemModules`), `module/fact/WellKnownTypes.scala`,
   `effect/processor/EffectMachinery.scala`, jvm `classgen/processor/NativeImplementation.scala`,
   `resolve/.../ValueResolver` (`compilerInternalAbilities`). Pure same-package adds/moves need none of these.

## Verify

- `./mill jvm.test` and `./mill lang.test` (or `./mill __.test`) — runs the whole pipeline.
- **The true two-pool check** (catches compiler-path-only breaks `jvm.test` can miss): build an end-to-end jar
  via the real driver and run it —
  `./mill examples.run jvm exe-jar examples/src/ -m <Module>` then `java -jar target/<Module>.jar`. Pick an
  example exercising the touched names (the `Effects*` programs for the effect layer).

## See also

- CLAUDE.md "Platform-Independence via Layers" (the *why*) and "The compiler is itself a platform."
- Memories: [[project_effect_stdlib_unification]], [[project_library_restructure]],
  [[feedback_minimize_scala_decompose_in_eliot]] (keep Scala thin; representation ≠ type identity),
  [[gotcha_brackets_type_namespace_marker]], [[gotcha_eliot_grammar_case_rules]],
  [[gotcha_lambda_class_collision_same_module]], [[gotcha_assembly_jar_breaks_layers]].
