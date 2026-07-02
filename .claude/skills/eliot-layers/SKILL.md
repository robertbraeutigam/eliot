---
name: eliot-layers
description: Use when adding, moving, splitting, renaming, or deleting Eliot `.els` source files — i.e. deciding which layer (stdlib / jvm / compiler / lang) a `type`, `def`, `data`, `ability`, or `implement` belongs in, and what must be kept abstract or duplicated. Covers the layer/platform model, the two-pool (compiler vs runtime) resolution that decides what must resolve where, the abstract↔concrete merge, signature-merge gotchas, sanctioned duplication, and the verify recipe.
---

# Eliot layers: placing & moving `.els` files

Read the **"Platform-Independence via Layers"** cornerstone in CLAUDE.md first — it owns the *why* (layers =
redefinition not inheritance; merge prefers the concrete). This skill is the *operational* layer: where things
go, what must resolve where, and the mechanical traps.

## The layers (Mill modules, each ships an `eliot/` source root)

A module `eliot.lang.X` maps to `<layer>/eliot/eliot/lang/X.els` (path = `ModuleName.toPath`). The same name
in several layers is **merged**.

| Layer | Holds | On which pool |
|-------|-------|---------------|
| `lang` | core compiler (Scala) **and** compiler-machinery `.els` (`eliot.compiler[.internal].{PatternMatch,TypeMatch,Coerce,Combine,Type}`) | **both** |
| `stdlib` | the platform-independent base: abstract `type`s, body-less `def` signatures, `ability` decls, pure platform-independent bodies/instances | **both** |
| `jvm` | the runtime platform: concrete `data`, `def` bodies, native leaves, ability `implement`s, value-converting instances | runtime only |
| `compiler` | the compile-time platform: `data`/instances the NbE checker must *evaluate at compile time* (e.g. the `Either` guard carrier) | compiler only |

## The two pools — the operational key

The driver (`build.mill` `examples.run` / `apidoc.run`) wires **two independent root pools**:

- **compiler-path** (type checking / NbE) = `lang + stdlib + compiler` — **no jvm**
- **runtime-path** (codegen, and the user program) = `lang + stdlib + jvm`

Merge happens **per pool** (`UnifiedModuleValueProcessor`, keyed on `platform`). Consequence that drives every
placement decision:

> **Anything in `stdlib` must resolve on BOTH pools.** A name a stdlib file *references* must be reachable
> from `lang+stdlib+compiler` as well as `lang+stdlib+jvm`.

That is why, when moving a runtime helper into stdlib, you sometimes must add an **abstract declaration** for
what it references: if the referenced name has a `compiler`-layer twin (e.g. `foldEither`/`foldOption` —
`compiler/.../{Either,Option}.els` define them), it already resolves on both pools via the merged module and
needs nothing. If it has **no** compiler-layer impl (the `ThrowCarrier`/`AbortCarrier`/`StateCarrier` carriers
and their accessors), stdlib must declare it abstractly or the compiler-path resolution fails.

## Where does X go?

**Quick heuristic:** *would a backend with a totally different representation (JavaScript — one numeric type;
an 8-bit MCU) need this?* If no, it's platform-specific → `jvm` (or the relevant runtime layer), never stdlib.

| You're adding… | Put it in | Notes |
|---|---|---|
| abstract `type X` (or `type X = alias`) | **stdlib** | so any signature can mention `X` on both pools |
| concrete `data X` (representation + ctor) | **jvm** (runtime) / **compiler** (if a compile-time carrier) | never stdlib |
| field accessor / `foldX` eliminator | with the `data` (jvm/compiler) | add an abstract twin in stdlib **only** if stdlib code references it (two-pool rule) |
| pure body, same on every target (composition over abstract/native ops, type-level arithmetic) | **stdlib** | e.g. `catch`, `fitsIn`, Function `.` |
| native leaf (backend/compiler supplies the body: bytecode op, arbitrary-precision arith, `printLineInternal`) | **jvm** (runtime) / Scala `*NativesProcessor` (compiler leaves) | the layer "bottom" |
| representation-dependent body (width dispatch, layout choice) | **jvm** | e.g. `Int`'s `+`/`-`/`*` bodies, the `opaque type Int` |
| `ability X` | **stdlib** | re-declared (copied) in any layer file hosting an `implement X` — see duplication below |
| `implement X[T]` with a runtime/value payload | with `T`'s `data` (jvm) or `X`'s module | body needs `T`'s ctor, which lives with the `data` |
| pure **type-level** `implement` (no runtime payload) | **stdlib** | e.g. `Combine[Int,Int]` |
| value-converting `implement` | **jvm** | e.g. `Coerce[Int,Int]` (does `nativeWiden`) |
| something the compiler must evaluate at compile time, expressible in Eliot | **compiler** | not Scala `SemValue`s — the one NbE evaluator runs it |

## The merge, mechanically

For each name, `UnifiedModuleValueProcessor.unifyValues` collects every layer's `ModuleValue` for that pool:
- **>1 concrete** (`runtime.isDefined`) → `"Has multiple implementations."`
- **signatures differ** → `"Has multiple different definitions."`
- else keep the concrete one (or the abstract one if none).

So a layer may **add a body but must not change the signature** — and the concrete def must **repeat the
whole signature**, including fixity/precedence (`infix left …`), because the merge takes the entire
`NamedValue`, not just the body.

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
  (`catch`/`orElse`, `fitsIn`, `.`) and pure type-level instances belong in stdlib. What stdlib must never
  carry: `data`, native leaves, or representation-dependent bodies.
- **Carrier↔effect import cycles are fine** (no cyclic facts). Two ability `implement`s sharing a method name
  in one module collide their generated lambda classes at JAR time — keep each carrier's instances in its own
  module ([[gotcha_lambda_class_collision_same_module]]). Never bundle layers into one fat/assembly jar — it
  collapses same-path layer resources ([[gotcha_assembly_jar_breaks_layers]]).

## Moving / adding a file — checklist

1. Decide the layer(s) from the table above; split abstract (`stdlib`) vs concrete (`jvm`/`compiler`) if needed.
2. If a stdlib body references a name with **no compiler-layer twin**, add the abstract declaration in stdlib
   (exact-match signature for any `data` member — see gotchas).
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
