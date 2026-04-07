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
     overlaps with `docs/incremental-compilation.md`.
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
   URI(s); design separately, in concert with `docs/incremental-compilation.md`.

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
