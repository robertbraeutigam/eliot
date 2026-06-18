# IDE Type Hints for Incomplete Expressions

## Goal

Allow an IDE client to query "what is the type at position `(uri, line, col)`?"
even when the source contains parse errors or unresolved/ill-typed sub-expressions
elsewhere — and ideally even within a partially-written expression.

This is the core capability that makes hover info, signature help, and
"expected type here" work in an editor. It is a prerequisite for the LSP server
described in `docs/lsp-server.md` (which mostly focuses on driver concerns).

## Key Findings From the Codebase

1. **Position info already pervades the pipeline.** Every expression node is wrapped
   in `Sourced[T]` carrying `(uri, PositionRange, value)` from parser through
   `monomorphize2`. The richest "type at node" information lives in
   `monomorphize2.fact.MonomorphicExpression` (`expressionType: Value` per node).
2. **The fact graph is lazy and cached.** `FactGenerator` keeps a
   `Ref[Map[Key, Deferred[Option[Fact]]]]`
   (`eliotc/.../compiler/FactGenerator.scala:15-40`). Any processor can request any
   fact; uncomputed ones are spawned on demand. There is no global "phase ordering" —
   running "past monomorphize" only requires asking for the right facts.
3. **Errors are the real obstacle, in three places:**
   - `getFactOrAbort` (`CompilerIO.scala:32-36`) short-circuits the entire
     `CompilerIO[T]` whenever an upstream fact is missing.
   - `registerFactIfClear` (`CompilerIO.scala:58-67`) drops the produced fact
     whenever any error has been registered in the current branch — so partial
     facts never reach the cache.
   - `compilerAbort` and exception-style abort flow inside individual processors
     (e.g. the `Evaluator`).

   `recover` (`CompilerIO.scala:90-102`) already exists and is the right primitive —
   it just isn't used widely enough.
4. **Parser already produces partial ASTs.** `ParserResult` keeps `value: Option[A]`
   alongside `currentError`, and the parser performs span-boundary recovery.
   `ASTParser` currently throws away the partial AST (`astResult.value` None ⇒ abort).
   That gate is the first thing to lift.
5. **`visualization.TrackedCompilerProcessor`** already wraps every processor and
   observes every fact request/registration. This is the right hook for an IDE-side
   index — we don't need to instrument processors individually.

## Design Overview

The plan is in four independent layers. Each is useful on its own; the full IDE
experience needs all of them.

### Layer A — Make facts survive errors ("partial facts")

The minimum change to allow type info to flow past broken sub-trees.

1. **Introduce `registerFactAlways`** alongside `registerFactIfClear` in `CompilerIO`.
   It registers the fact unconditionally and is meant for facts that carry their own
   optionality (e.g. an expression where some children are `Unknown`).
2. **Introduce a `Hole` / `Unknown` marker** in the resolved/typed expression ADTs:
   - `resolve.fact.Expression.Unknown(range, reason)` for nodes that resolve couldn't
     pin down.
   - `monomorphize2.fact.MonomorphicExpression.Expression.Unknown(range)` with
     `expressionType = Value.Unknown` for nodes whose type the monomorphizer couldn't
     compute.
   - `Value.Unknown` becomes a bottom-like type that unifies with anything during
     monomorphization but is propagated rather than collapsing the whole expression.
3. **Convert local `getFactOrAbort` calls into `getFact` + `recover`** in the
   per-expression generators: `ValueResolver`, `Evaluator`,
   `MonomorphicTypeCheckProcessor`, `OperatorResolver`, `MatchDesugarer`. Pattern:
   ```scala
   recover(processChild(child))(default = Expression.Unknown(child.range, ...))
   ```
   Module-level / file-level processors keep aborting — we don't want to invent a
   fake module just to type-check one expression.
4. **Update pattern matches** for the new `Unknown` cases everywhere they're
   scrutinised (per CLAUDE.md "Compiler Change Patterns" note). The `used` and
   `uncurry` phases must refuse to operate on expressions containing `Unknown` and
   emit them as errors instead — otherwise the JVM backend would try to codegen holes.
5. **Tests** in `lang/test`: feed snippets with a single broken sub-expression and
   assert that sibling expressions still produce a `MonomorphicExpression` fact whose
   `expressionType` is concrete.

### Layer B — Make the parser tolerant enough to type the *current* expression

Layer A is enough for "broken expression elsewhere"; Layer B is required for the
in-progress expression itself.

1. **Stop aborting on `astResult.value.isEmpty`** in `ASTParser.scala:52-55`.
   Register the partial AST plus all collected `ParserError`s. Files that failed
   entirely become an empty `Module` — still valid for the rest of the pipeline.
2. **Add expression-level recovery points** to the parser. Today recovery happens at
   span boundaries; we need expression-statement boundaries too, so an unfinished
   `let foo = bar.|` still emits a value definition whose body is `Expression.Unknown`
   plus an error.
3. **Synthesize "expected here" placeholders.** When parsing an application argument,
   infix operand, or `match` scrutinee fails, insert an `Expression.Unknown(range)`
   and continue. This makes the AST shape stable enough that resolve/eval/monomorphize
   will produce `MonomorphicExpression` for the surrounding context — which is
   exactly what the IDE needs in order to say "this slot expects an `Int`".

### Layer C — Position-indexed query API

Once Layers A+B exist, we need a way to actually answer "type at (uri, line, col)".

1. **New module `lang/.../ide`** containing `TypeHintIndex`:
   ```scala
   final case class TypeHint(range: PositionRange, tpe: Value, kind: Hint.Kind)
   final class TypeHintIndex private (
       private val byUri: Map[URI, IntervalTree[TypeHint]]
   ) {
     def hintsAt(uri: URI, pos: Position): List[TypeHint] // innermost-first
     def expectedTypeAt(uri: URI, pos: Position): Option[Value]
   }
   ```
   Backed by an interval tree (or just a sorted vector + binary search per file) so
   hover queries are O(log n).
2. **A `TypeHintIndexProcessor`** that subscribes to `MonomorphicValue` facts: when
   one is registered, walk the `Sourced[MonomorphicExpression]` tree and append a
   `TypeHint` per node. Built-in/native values without source positions are skipped.
3. **Reuse `TrackedCompilerProcessor`** as the subscription mechanism. Generalise
   `FactVisualizationTracker` into a `FactObserver` trait with two implementations:
   the existing HTML/Cytoscape one and the new `TypeHintIndexBuilder`.
   `Compiler.scala:60-66` already runs the visualization step at the end of
   compilation; the IDE index goes in the same place but is exposed through a new
   fact (`TypeHintIndexFact`) rather than written to disk.
4. **"Expected type" hints (for in-progress holes).** When the monomorphizer
   encounters `Expression.Unknown` as e.g. the second argument of
   `f : Int -> String -> Bool`, it knows the *expected* type is `String`. Emit a
   `TypeHint(range, String, Kind.Expected)` in addition to (or instead of) the actual
   type. This is what makes "type the current expression" useful.

### Layer D — Driver / entry point for an IDE

The compiler today is one-shot CLI. To make IDE queries cheap:

1. **Add an `ide` sub-command** in `compiler/Main.scala` that:
   - Reads a project root (same as `examples.run`).
   - Builds the full fact graph once, with Layers A+B active.
   - Drops to a small JSON-RPC / stdio loop accepting `hover {uri, line, col}` and
     `reload {uri}` requests.
   - On `reload`, invalidates cached facts whose key references that URI. The
     current `FactGenerator` has no invalidation; the simplest first version throws
     the whole `Ref` away and recompiles. Per-file invalidation is a follow-up that
     overlaps with the incremental fact cache (`IncrementalFactGenerator`).
2. **Plugin wiring.** A new `IdePlugin` (in a new `ide` Mill module) depends on
   `LangPlugin`, installs the `TypeHintIndexProcessor`, and is selected by the `ide`
   command instead of `JvmPlugin`. This is the clean way to keep IDE-only logic out
   of the JVM backend.
3. **LSP layer** (out of scope for this plan, but the JSON-RPC loop is meant to be
   wrapped by a thin LSP shim later — see `docs/lsp-server.md`).

## Suggested Implementation Order

1. **Layer A.1–A.2** (`registerFactAlways`, `Unknown` markers, `Value.Unknown`) —
   small, mechanical, no behaviour change yet.
2. **Layer A.3–A.4** (rewrite `getFactOrAbort` ⇒ `getFact + recover` in
   per-expression processors; update pattern matches; gate the backend). Add tests
   asserting that sibling expressions still type-check.
3. **Layer C.1–C.3** (`TypeHintIndex`, `FactObserver` refactor,
   `TypeHintIndexProcessor`) — gives a working "type at position" for *complete*
   programs.
4. **Layer B** (parser tolerance) — unlocks "type at position" inside the expression
   being edited.
5. **Layer C.4** (expected-type hints) — fed by the `Unknown` nodes that Layers A+B
   produce.
6. **Layer D** (`ide` sub-command, plugin, request loop, naive invalidation).
7. **Per-file fact invalidation** — depends on tagging fact keys with their source
   URI(s); design separately, in concert with the incremental fact cache (`IncrementalFactGenerator`).

## Risks and Open Questions

- **`Value.Unknown` polluting unification.** It must be a true "top of lattice" that
  never causes spurious unification successes elsewhere. Likely needs a dedicated
  case in the monomorphize2 unifier with a "this hint is unreliable" flag propagated
  to downstream `TypeHint`s.
- **Ability resolution past errors.** Ability implementation lookup currently aborts
  hard if no implementation is found. For IDE purposes we want to fall back to the
  ability's declared signature. This needs Layer A treatment specifically inside the
  `implementation` phase.
- **Parser recovery quality.** Span-boundary recovery is coarse; getting useful hints
  inside half-written `let` blocks may need genuine expression-level recovery rules
  and is the riskiest piece of Layer B.
- **Cache invalidation for the `ide` driver.** Throwing the whole graph away on each
  keystroke is fine for small projects but won't scale. Per-URI invalidation requires
  every `CompilerFactKey` to expose its source dependencies — a non-trivial retrofit.
- **Test ergonomics.** Need a new test helper that runs the pipeline with Layer A
  enabled and asserts on the `TypeHintIndex` rather than on final bytecode.

## Files That Will Need to Change (high level)

- `eliotc/.../processor/CompilerIO.scala` — `registerFactAlways`, possibly an
  `Unknown`-aware `getFactOr` helper.
- `eliotc/.../compiler/FactGenerator.scala` — eventually, per-key invalidation hook.
- `eliotc/.../visualization/{TrackedCompilerProcessor,FactVisualizationTracker}.scala`
  — extract `FactObserver` trait.
- `lang/.../resolve/fact/Expression.scala` — `Unknown` case + all matches.
- `lang/.../monomorphize2/fact/{MonomorphicExpression,Value}.scala` — `Unknown` cases
  + unifier change.
- `lang/.../{eval,operator,matchdesugar,implementation,monomorphize2}/processor/*` —
  replace `getFactOrAbort` with recovering variants on per-expression branches.
- `lang/.../ast/parser/ASTParser.scala` and `Parser.scala` — partial AST registration,
  expression-level recovery.
- `lang/.../used/*` and `lang/.../uncurry/*` — error on `Unknown` (block backend).
- New `lang/.../ide/{TypeHint,TypeHintIndex,TypeHintIndexProcessor}.scala`.
- New `ide/` Mill module with `IdePlugin`.
- `eliotc/.../compiler/{Main,Compiler}.scala` — `ide` sub-command and request loop.

## Implicit-generics: propagation, display, and explorability (the deferred W5/W6 work)

The `auto` / implicit-generics feature is built and shipped: input generalization (W1), data-field generalization
(W2), calculated returns (W3), and the limit diagnostics (W4) all work, and every limit that cannot be crossed
**hard-errors at the use site** — it never silently mistypes. (Its own planning doc was retired once that scope
shipped; the design rationale lives in the source comments of the `saturate` package and the `monomorphize`
checker, and the CLAUDE.md "Use-Site Verification" cornerstone.) What remained were two follow-on stages,
*propagation / display polish* and *explorability*, which only pay off once there is IDE integration. They are
therefore parked here, on top of this plan's Layers A/C/D.

Two of the parked items are compiler-*completeness* improvements (the `Combine`-join postpone and transitive
"viral" bounds), not strictly IDE features. They are **fail-safe today** — a hard, actionable error at the use
site, never a wrong type — so they are off the critical path. They are kept here because there is no driver to
prioritize them ahead of the IDE work, and the IDE's by-example presentation is the natural place the same partiality
surfaces early (probing, below). If a concrete program ever needs one of them before the IDE exists, it can be lifted
back out independently; the design notes are preserved below.

### Propagation, ergonomics, display (was W5)

- **Transitive / "viral" bounds through nested records** *(completeness; fail-safe today)*. The saturation oracle
  `SaturatedValueProcessor.inferableInfo`'s W2-growth read is deliberately **non-transitive**:
  `fieldContribution` uses the *raw* arity (`rawInferableInfo`), so a `data Outer(inner: Counter)` does **not**
  grow `Outer` by `Counter`'s bounds — `Outer` stays bound-opaque and `Counter`'s field generalizes only
  per-occurrence (W1). Making bounds propagate transitively means having `fieldContribution` read the *grown* arity
  (`inferableInfo`) and guarding the now-recursive growth lookup against cycles (so a `data` family whose fields
  reference other auto-bounded `data` terminates). Same fail-safe deferral as the multi-constructor-union W2
  follow-up (`Maybe = Nothing | Just(value: Int)`), whose free-binder ambiguity ties it to the calculated-return
  machinery — both are "does not *gain* bound-tracking", never "accepts a wrong type".

- **`Combine`-join postpone (calc-return over a branch join)** *(completeness; fail-safe today)*. A calculated
  return over a `Combine`-joined argument — `double(pick(a, b))` — cannot ground the callee's type args at the call:
  `pick`'s combinable result is resolved only at *drain* (its check records an upper bound), so `double`'s
  instantiation metas are still unsolved when the eager calc-return resolution runs. Today
  `Checker.readMonomorphicReturn`'s not-ground branch reports a specific, actionable error
  (`reportUngroundCalculatedReturn`) instead of leaking the `Type` placeholder into a confusing `Coerce(Type, Int)`
  mismatch. **Making such a call *compile*** means postponing the calc-return resolution into the
  `drainAndResolveLoop` (the same way ability resolution and `resolveUpperBounds` are deferred), so the join grounds
  *first* and the callee's type args become ground before the return is read. The risk that kept it out of W4: the
  deferred upper-bound path leaves the callee's instantiation metas unsolved, so the postpone has to be reordered
  against the combinable-meta / instantiation-meta machinery — real regression risk against the passing `Combine`
  suite, out of proportion for a limit that is already fail-safe.

- **Symbolic `ElaboratedSignature` fallback** for *use-independent* producers — a never-called producer, or tooling
  that wants a principal signature with no concrete driver (the calculated-return path that shipped resolves every
  *reachable* producer concretely via `MonomorphicValue`; this is the use-independent fallback it left out). Check
  the body with the input binders as **neutral** variables, forward-evaluate (`x + x ⤳ Int[add MIN MIN, add MAX
  MAX]`), and quote the symbolic result into the return. Same convergence/limit criterion as the concrete path
  (`Quoter.quote` succeeds ⇒ calculated; stuck ⇒ a reported Limit). Kept strictly off the driven-from-`main`
  compile path — unreachable producers are never monomorphized from `main`, so this is purely the IDE/tooling
  concern of showing a producer's principal calculated return.

- **Readable, stable binder names.** Synthesized binders are currently mechanical (`$Int$0`, `$Counter$1`; see
  `SaturatedValueProcessor.freshName` / `TypePlan.binderName`). Give them readable, stable names for diagnostics
  and hover, and document the explicit-supply / partial-application interactions (bare = tightest calculated;
  explicit annotation = stable published contract widened via `Coerce`).

- **Test:** transitive propagation across two `data` layers and a function; `double(pick(a, b))` compiles to the
  `Combine` join; rendered signatures show calculated returns; hover shows synthesized binders by readable name.

### Explorability: examples, generators, and probing (was W6)

Dependent signatures (`Int[add(MIN,MIN), add(MAX,MAX)]`) are precise but illegible. This item makes them legible
and checkable **by example**, exploiting that whole-program monomorphization already computes worked examples for
free. Everything here reduces to one primitive — *request more `MonomorphicValue` facts and observe* — which the
lazy, unordered, cached fact graph already supports (Key Finding 2 above). Gated behind the implicit-generics
calculated returns (W3) and Layers A (partial facts / `recover`) and C (`TypeHintIndex`) of this plan.

- **Real-usage examples ("all available examples").** Aggregate the existing `MonomorphicValue(fqn, *)` facts and
  show the in→out set at a definition (`double` used as `Int[0,255]→Int[0,510]`, `Int[0,510]→Int[0,1020]`). Free;
  the only change is letting `TypeHintIndex` hold a *set* of hints per definition range. An unused producer has no
  such facts — which is exactly when generators take over.

- **Generators (only for type-position types).** A generator for a type `T` is a finite enumeration of
  representative *bound instantiations*. Primitive for `Int`: a **boundary-focused** canonical set (`Int[0,0]`,
  `Int[0,255]`, `Int[-128,127]`, `Int[0,65535]`, `Int[0,2^31-1]`, `Int[0,2^63-1]`, `Int[0,2^64]`) hitting each
  `Jvm*` tier and the cross-tier edges. Structurally derived for `data`: `Counter[lo,hi]`'s generator is
  `{ Counter[i] | i ∈ Int-generator }` — composed from its components, like deriving `Arbitrary` but at the bound
  level. The set of types needing a generator is derivable: the type-position constructors of every
  `SaturatedValue` signature (a `data` used only as a runtime value needs none). Examples are produced by
  *speculatively requesting* `MonomorphicValue(fqn, generatedArgs)`.

- **Probing ("error on a bad parameter combination").** Each generated (or real) instantiation either monomorphizes
  to an example or *registers an error* — a counterexample. Surface those at the definition ("`double` does not
  type-check for `Int[0,2^63]`: the intermediate overflows `JvmLong` with no wider `Coerce`"). This is
  **lightweight totality testing** of the over-claim that bare-input generalization is total over all bounds: a
  body using a bound-restricted operation (a narrow-only `Coerce`, a fixed-width intermediate, a missing `Combine`
  join) is only *partial*, and probing finds counterexamples that would otherwise surface as a confusing error at
  a future caller. It is the proactive, example-driven twin of the implicit-generics **limits** (the calculated-return
  limits the checker already enforces at the use site) — the same failures, found early by sampling rather than late
  at a use. Honest scope: probing is a counterexample
  *finder*, not a totality *proof* (sampling is incomplete; the sound version is bound-constraint inference, future
  work — see the CLAUDE.md "Use-Site Verification" cornerstone). It must be **sandboxed**: small samples, a
  per-probe step/time budget, and "probe didn't finish" (e.g. hit the recursion limit) reported as *unknown*, never
  a false-positive error.

- **Presentation (creative affordances).** A differential inlay on a bare return (`: Int ⟨[0,255]→[0,510],
  [0,65535]→[0,131070]⟩` — the relationship, not the formula); "view-through" an instantiation (pin a bound, drive
  one speculative monomorphization, feed its per-node types into `TypeHintIndex` so every body node shows its type
  at that width); a totality CodeLens (`7/7 sampled bounds ✓` / `⚠ partial above 2^62` — an empirically-derived
  precondition); a representation-transition view marking where the output crosses a `Jvm*` tier (silent widening
  before it costs memory on a small target).

- **Where:** extends `lang/.../ide` — `TypeHintIndex` → set-per-range; a `GeneratorProcessor` deriving generators
  from `SaturatedValue` signatures; a probing driver issuing budgeted speculative `MonomorphicValue` requests.
  Reuses Layers A/C/D wholesale; the only genuinely new code is generator derivation and probing/aggregation.
- **Test:** a `double` definition with two real call sites shows both; an unused producer shows generated examples;
  a deliberately bound-partial helper yields a counterexample at a representation boundary; a recursive producer's
  probe reports *unknown*, not an error.

This explorability work is also the practical answer to the "Use-Site Verification" trade-off (CLAUDE.md):
generators + probing substitute automated coverage for the modular totality proof the type no longer provides.
