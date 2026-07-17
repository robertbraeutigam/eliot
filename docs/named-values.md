# `namedValues` — compile-time reflection over top-level values

## Summary

Add one general compiler intrinsic:

```eliot
def namedValues[V](name: String): List[V]
```

It reifies, at compile time, to a residual builder-chain of **every top-level value named
`name` in the current invocation's runtime pool**:

```eliot
namedValues[Test]("spec")   ⤳   prepend(fileA::spec, prepend(fileB::spec, empty))
```

This is the *only* compiler feature the test framework needs; the framework itself (`Test`,
`test`/`describe`/`it`, `shouldBe`, `runTests`) is then pure Eliot. `namedValues` is not
test-specific — it is a reusable reflection primitive (plugin registries, instance tables, …).

It also **retires** the earlier `TestIndex` + synthetic-main ideas: the test entry point is now
ordinary hand-written Eliot — `def main: IO[Unit] = runTests(namedValues[Test]("spec"))` — and a
"test project" is purely a *mount/invocation* concern (which roots the compiler scans), needing no
compiler code beyond this intrinsic.

## Semantics (decided)

- **Keyed on `name`, not type.** Names are lexical (`ModuleNames`, available before the checker);
  type equality in Eliot is *definitional* and requires the evaluator, so a pre-checker type filter
  would be the forbidden "parallel bespoke type-equality mechanism." `V` is therefore **not a
  filter** — it is a *claim*: the emitted `prepend(_, _): List[V]` asserts each element is `V`, and
  the ordinary checker enforces that downstream by definitional equality. A name-matched value that
  does not reduce to `V` is a hard compile error (never a silent drop — fail-safe).
- **`name` must be a compile-time literal `String`.** It drives a compile-time rewrite; a
  non-literal argument is a hard error, not a silent empty list.
- **Scope = the invocation's runtime mount universe.** No lexical scoping of the call site, no scope
  argument. src/test separation is achieved by tests being a *separate invocation* (separate runtime
  mount set) — see `## Scope & invocations`.
- **Deterministic order** — results sorted by canonical `ValueFQN` string.
- **Nullary values only fall out naturally.** A `def spec: Test` has signature `Test`; a
  `def mkSpec(x): Test` has signature `A -> Test`, so `prepend(mkSpec, …): List[Test]` fails the `V`
  claim. No special "exclude functions" logic needed.

## The central new capability: whole-pool enumeration

The core compiler has **no enumeration today** — `SourceMount` exposes only `resolve(path)`, and
computation is demand-driven from `main`. `namedValues` needs "every module in the pool," so this is
the real work. Three facts/processors, bottom-up:

1. **`SourceMount.enumerate(): CompilerIO[Seq[Path]]`** — new method listing every module-relative
   path the mount serves.
   - `FilesystemMount`: walk `root` for `*.els`, map to module paths.
   - Synthetic mounts (jvm `main.els`, LSP `vfs:` overlay): enumerate their computed/overlaid set.
   - **Incremental:** enumeration must record its filesystem reads as scan dependencies (same
     discipline as `PathScan.resolve`) so adding/removing a file invalidates downstream facts. See
     `## Incrementality` — this is a *listing* dependency, the one genuinely new caching cost.

2. **`PoolModules(platform)` fact** — the deduplicated union of all mounts' module names, produced
   by one processor over the `PathScanner` pool's mount list. (Runtime pool for the test use;
   keyed by `platform` so a compiler-time use is possible later.)

3. **`NamedValuesIndex(name, platform)` fact** — for each module in `PoolModules`, read
   `UnifiedModuleNames` (per-module, so layer copies are already merged), keep `Default`-qualifier
   public names equal to `name`, emit `ValueFQN(module, name)`, sort canonically. Reading
   `UnifiedModuleNames` (not per-file `ModuleNames`) dedups layer copies while keeping distinct
   modules distinct.

## The rewrite

`NamedValuesRewriteProcessor` — recognizes `namedValues[V](lit)` by FQN in a **resolved** expression
and rewrites it to the builder chain:

- read the literal `name` (syntactic — no evaluation needed; non-literal → hard error);
- read `NamedValuesIndex(name, Runtime)`;
- emit a fully-resolved expression `prepend(ref(fqn₁), prepend(ref(fqn₂), … empty))`, where
  `prepend`/`empty` resolve to the native `List` builders and each `ref(fqnᵢ)` is a resolved
  `ValueReference` carrying the source position of its declaration (so a downstream `V`-claim error
  points at the offending `def`, not synthesized code).

Slots into the pipeline **after resolution** (FQNs available) and before the checker, so the emitted
references flow through termination → effect → ability → monomorphize normally. Downstream is all
existing machinery: the checker enforces the `V` claim by definitional equality; `used` sees the
references and marks the values live (reachability solves itself — no separate root mechanism);
codegen emits the native builder calls.

## Reduction placement — the one open decision

- **Option A (recommended): a dedicated rewrite processor** producing a residual resolved
  expression. Matches the "purely syntactic rewrite" model, and is sufficient because `namedValues`
  is used only in *runtime* position (in `main`). Simple; does not make the evaluator program-aware.
- **Option B: an evaluator native** (reduce-and-reify through the single `Evaluator`). Required only
  if `namedValues` is ever used in a *compile-time / type* position. Defer (YAGNI for tests); A can
  be upgraded to B later without changing the surface.

Settle this with a small spike: confirm the rewrite can run as a resolved-expression transformation
in the resolve→checker segment. If a compile-time use ever appears, add the native form then.

## Scope & invocations

One runtime pool per invocation → src and test cannot be two runtime universes in one run. Tests are
a **separate `eliotc` invocation** (Mill-style separate project): runtime pool = app roots + test
roots + framework, rooted at a hand-written test `main`. `namedValues` inherits that universe. The
normal build invocation never mounts the test roots, so test code cannot reach the shipped binary.
(Today: a second invocation with different `--path` roots; later: a `.test` project once Eliot has a
build system.)

## Incrementality

Enumeration adds exactly **one** genuinely new caching cost: a *listing* dependency (the set of
files), which — unlike a per-file `stat`/hash content dependency — can only be validated by
re-listing, because nothing tells you a file appeared without looking. It is bounded and movable.

- **It is a directory walk, not a re-parse.** The listing dep is validated by `readdir`/`stat` per
  *directory*; it detects file add/remove and does **not** re-read or re-type any content. The
  existing precise per-file content deps (`stat` → `ModuleNames`) are untouched. Worst case is one
  directory-tree walk per compile, after which downstream stays cached if the set is unchanged.
- **Two dependency kinds, together complete:** the *file set* (a file appeared/disappeared) → the
  coarse listing dep; *value membership within a file* (a file gained/lost the name) → already the
  precise per-file `ModuleNames` dep. Only the first is new.
- **Gate it through the existing `FileStat` fact — no new dependency kind.** `FileStat`
  (`FileStatProcessor` = `File.lastModified()`, keyed by `FileStat.Key(file)`) is the leaf the whole
  incremental scan already hangs off (`FilesystemMount.resolve` for existence, `FileContentReader`
  to skip re-reads). A directory is a `File`, so `enumerate` records a `FileStat.Key(dir)` dep for
  **every directory it walks**. Add/remove/rename bumps that directory's mtime → `FileStat(dir)`
  regenerates → `PoolModules` invalidates → re-walk; otherwise every `FileStat(dir)` is unchanged,
  `PoolModules` stays cached, no `readdir`. Must stat *every* directory in the tree (a nested add
  only bumps its immediate parent's mtime — but that parent is a walked dir, so it is in the dep
  set; a new subdirectory bumps *its* parent's mtime, also in the set). This is the same mtime trust
  level `FileContentReader` already uses for files — no new reliability assumption.
- **Scope to own roots** — enumerate only the test project's roots, never base/stdlib/jvm, so the
  directory-stat dep set stays tiny.
- **Move it out of the compiler (long-term):** once Eliot has a build system, the tool already globs
  the source set and would pass an **explicit module list** to the invocation. Then `PoolModules` is
  a precise, hashable *input* — no compiler-side walk, fully cacheable — and glob incrementality is
  the build tool's job (as in Bazel/Buck/Mill). The compiler-walks-the-roots enumeration is the
  **interim** until then. This reinforces keeping `SourceMount.enumerate` behind `PoolModules` so the
  source of the module set (walk vs. passed-in list) can swap without touching the index or rewrite.

## Fail-safes

- Non-literal `name` → hard error ("`namedValues` requires a literal name").
- Deterministic canonical-FQN order.
- Filter to `Default`-qualifier, public names.
- A name present in the index but not resolvable to a value → surface via `getFactOrError`, never
  drop.
- Empty result → empty `List` (valid). A "zero tests" warning is framework-level, not here.

## Dependencies (NOT part of this feature)

- **Native `List`** — abstract `type List[A]` in base + `empty`/`prepend`/`fold` body-less defs +
  jvm native. The rewrite targets whatever the builders are named. Separate prerequisite
  (`eliot.collection`).
- Test framework and a process exit-code native are out of scope here.

`namedValues` can be unit-tested independently of `List` by asserting the emitted FQN chain / index
contents, before the native `List` lands.

## Test plan

- multi-module fixtures: same-named values across several packages → correct sorted FQN list.
- type-mismatch: a name-matched value not reducing to `V` → checker error, positioned at that `def`.
- non-literal `name` → hard error.
- layer dedup: one module, two layer files defining the name → a single entry.
- distinct modules, same name → distinct entries.
- empty: no matches → empty `List` compiles.
- determinism: stable order across runs.
- whole-pipeline `ProcessorTest` (via `LangProcessors`), plus a leaf test for `SourceMount.enumerate`
  and the index processor.

## Sequencing

1. `SourceMount.enumerate` + `FilesystemMount` walk (+ synthetic-mount impls) with incremental deps.
2. `PoolModules` fact + processor.
3. `NamedValuesIndex` fact + processor.
4. `namedValues` FQN recognition + `NamedValuesRewriteProcessor` (Option A) + fail-safes.
5. Register in `LangProcessors`; wire pipeline placement.
6. Tests.
