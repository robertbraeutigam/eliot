# LSP Server Plan for Eliot

## Status

| Step | State |
|---|---|
| Incremental compilation | ✅ done (`IncrementalFactGenerator` + persistent fact cache) |
| Persistent compile lifecycle (`CompilationSession`) | ✅ done — the seam a server loops |
| Cancel-restart server loop (`CompilationServer`) | ✅ done — file watching still todo |
| LSP protocol layer + entry point | ⬜ todo |
| Reverse position index | ⬜ todo |
| Diagnostics / Hover / Go-to-Definition / Completion | ⬜ todo |
| Error recovery, virtual file system | ⬜ todo |

The fact-based architecture and pervasive source tracking mean most of the *data* is already there.
The remaining work is making the *lifecycle* interactive (incremental, persistent, error-tolerant)
rather than batch — and the resident-engine lifecycle (incremental compilation, a persistent
`CompilationSession`, and a cancel-restart `CompilationServer`) is now in place.

## Existing Infrastructure

The compiler architecture is well-suited for LSP integration:

- **Source positions everywhere** — `Sourced[T]` wraps every AST node, resolved name, and type
  with `URI + PositionRange`. This maps directly to LSP locations.
- **Fact-based compilation** — `IncrementalFactGenerator` already does lazy, cached, on-demand
  computation. The query model ("give me the type of this value") is very close to what an LSP needs.
- **Rich compilation artifacts** — `ResolvedValue` (name → definition), `MonomorphicValue`
  (type-checked values), `ModuleValue.dictionary` (in-scope names), `CompilerError` (diagnostics with
  positions) — all the data for core LSP features already exists.
- **Plugin architecture** — An LSP plugin could sit alongside `JvmPlugin` without modifying core.

## What Needs Work

### 1. Incremental Compilation — ✅ done

Implemented as `IncrementalFactGenerator`
(`eliotc/.../compiler/IncrementalFactGenerator.scala`) plus a persistent fact cache
(`eliotc/.../compiler/cache/`). It does backward, demand-driven validation: a no-change rebuild does
only leaf `stat`s, and a changed file invalidates just its dependency cone. Key pieces, all built:

- **File change detection** — leaf `FileStat` facts are always regenerated, forming the boundary with
  the external world; everything else is validated against them.
- **Equality / signature cutoff** — a changed leaf whose derived value recomputes *equal* stops
  propagation. This is the big performance win: a body edit that leaves a signature unchanged does not
  cascade.
- **Value-less drilling** — non-serializable `SemValue`-bearing facts (the monomorphize layer) are
  validated *structurally* by drilling their recorded `directDeps` to the leaves, without
  materialising the value. So a no-change run materialises no `SemValue` at all.
- **Persistence** — the cache is written to `.eliot-cache` (Java serialization; a `Path` stand-in;
  non-serializable fact *values* dropped to edges-only while keys/deps are kept), guarded by a
  compiler-classpath fingerprint and a config fingerprint so a stale cache is never served.

(The standalone incremental-compilation design doc has been retired; the design now lives in the
code's doc-comments and commit history.)

### 2. Long-Running Server Mode — 🚧 engine + loop done, watching/protocol remain

The compile lifecycle has been factored out of the one-shot CLI driver into **`CompilationSession`**
(`eliotc/.../compiler/CompilationSession.scala`, commit `16292d61`) — the persistent-generator seam a
server needs:

- `create(targetPlugin, activatedPlugins, configuration, args)` does the run-independent setup once
  (plugin `configure()`, processor-graph collection, cache fingerprints, in-memory cache seeded from
  disk).
- `compileOnce(tracker?)` runs one full compilation against the in-memory `FactCacheData` and folds
  the result back in as the next run's `prior`; `persist()` flushes to disk.
- The **in-memory cache is richer than the on-disk one** — typed `SemValue` facts (with their native
  closures, which are dropped on serialization) survive across runs, so the *monomorphize* layer is
  reused across edits, not just the front end. This is the payoff of keeping the engine resident.
- **Cancel-safe**: the cache is updated as the *last* effect, under a `Mutex`, so a run cancelled by a
  newer edit keeps the previous good cache.
- `CompilationResult` hands back the live generator, so LSP features query facts (`getFact`) against
  the latest result, plus the run's diagnostics.

The CLI now drives `create → compileOnce → persist` with identical batch behavior.

The **cancel-restart server loop** is also built — `CompilationServer`
(`eliotc/.../compiler/CompilationServer.scala`). A single worker loop drives `compileOnce` with
latest-wins semantics:

- `requestCompile` is a non-blocking, coalescing trigger; the worker races the running compile against
  the arrival of a newer request (`IO.race(runOnce, requests.take)`), cancelling and restarting on a
  newer request and committing the result otherwise. It idles until the first request.
- Results are pushed (`onResult` callback, e.g. publish diagnostics) and pullable (`latestResult`,
  e.g. answer a hover from the live generator). The commit is uncancelable, so a finished result is
  published atomically or not at all.
- `start(session, onResult)` runs the worker as a background fiber and, on release, cancels it
  (cancelling any in-flight compile) and flushes the cache to disk for a warm next start.

Still required:

- **File watching** — detect on-disk changes (and, with the VFS below, editor `didChange`) and call
  `requestCompile`. The loop itself is done; this is the trigger source.
- **An LSP protocol layer** — either an existing Scala LSP library (e.g., `lsp4j`, which Metals uses)
  or a lighter JSON-RPC wrapper for message parsing and lifecycle.
- **An entry point** — a new `LspPlugin` used as the session's target, or a standalone driver that
  constructs a `CompilationSession` + `CompilationServer` directly (see *Key Architectural Decision*
  below).

### 3. Reverse Index: Position → Fact — ⬜ todo

The compiler currently works **name → fact** (forward). LSP needs **position → fact** (reverse):

- "What's at line 5, column 12?" → find the `Sourced[ValueFQN]` or `Sourced[Expression]` whose
  range contains that position.
- This requires building a **position index** — a spatial data structure (interval tree or sorted
  list) per file, mapping `PositionRange → Sourced[T]` for all interesting nodes.
- Could be built as a post-processing pass after monomorphic type checking, collecting all `Sourced`
  wrappers from the typed AST.
- Note: `MonomorphicValue` is keyed by `(vfqn, typeArguments)` — a value can have several
  specializations. Hover may want the generic `ResolvedValue` signature, or to pick a representative
  specialization; the index should map a position to the `ValueFQN`, then decide how to present types.

### 4. Error Recovery in Parsing/Type Checking — ⬜ todo

Currently, errors abort the pipeline for that value (via `compilerAbort`). For LSP:

- The **parser** needs error recovery — return a partial AST even when syntax is broken (Parsley
  supports this to some degree).
- **Type checking** should degrade gracefully — provide partial type info even when some expressions
  fail. The `recover()` mechanism in `CompilerIO` is a foundation, but processors need to use it
  more aggressively.
- Goal: always produce *something* useful, even for broken code.

### 5. Virtual File System — ⬜ todo

The compiler reads files from disk via `FileContentReader`. For LSP:

- The server needs to use the **editor's buffer content** (unsaved changes), not the on-disk file.
- Add a `VirtualSourceContent` layer that intercepts `SourceContent` facts with in-memory overrides
  from `textDocument/didChange` notifications.
- This slots naturally into the existing fact system — just a new processor that takes priority over
  `FileContentReader`. It also fits the incremental model: `SourceContent` / `FileStat` are leaf facts
  that are re-read every compile anyway, so they are precisely the override point — a buffer change
  simply makes the leaf report new content, and the dependency cone invalidates normally.

## LSP Feature → Existing Infrastructure Mapping

| LSP Feature | What Exists | What's Missing |
|---|---|---|
| **Diagnostics** | `CompilerError` with positions, messages, descriptions; `CompilationResult.errors` | Server loop + streaming as `publishDiagnostics` |
| **Hover** (type info) | `MonomorphicValue.signature` | Position → value lookup (reverse index) |
| **Go to Definition** | `Sourced[ValueFQN]` in expressions → definition `Sourced[QualifiedName]` in `ResolvedValue` | Reverse index, cross-file navigation |
| **Completion** | `ModuleValue.dictionary` has in-scope names | Partial-name matching, context-aware filtering, triggering on incomplete input |
| **Find References** | `UsedNames` tracks which values are used | Full reverse reference index |
| **Rename** | All references are `Sourced[ValueFQN]` | Collect all occurrences project-wide |
| **Signature Help** | `FunctionDefinition.args` has parameter types | Cursor-position-aware parameter highlighting |
| **Semantic Tokens** | `SourceTokens` + resolved names have all classification info | Token classification mapping to LSP semantic token types |

## Suggested Implementation Order

1. ✅ **Incremental compilation** — done (`IncrementalFactGenerator` + persistent cache).
2. 🚧 **Persistent server mode** — `CompilationSession` lifecycle and the cancel-restart
   `CompilationServer` loop done; remaining: file watching and an entry point.
3. **LSP scaffolding** — new module, `lsp4j` dependency, protocol handling, virtual file system.
4. **Diagnostics** — easiest LSP feature, just stream `CompilationResult.errors` as
   `textDocument/publishDiagnostics`.
5. **Reverse position index** — enables all position-based features.
6. **Hover + Go to Definition** — highest-value features for developers.
7. **Completion** — requires understanding partial/broken input contexts.
8. **Error recovery** — parser and type checker resilience for broken code.
9. **Remaining features** — references, rename, semantic tokens, signature help.

## Module Structure

The incremental generator and the resident compile lifecycle already live in `eliotc`
(`compiler/IncrementalFactGenerator.scala`, `compiler/CompilationSession.scala`,
`compiler/cache/`), so the new module only adds LSP-specific concerns and composes the existing
`CompilationSession`:

```
lsp/          (new module, depends on lang + eliotc)
  ├── server/         LSP protocol handling (lsp4j), the cancel-restart compile loop
  ├── index/          Reverse position index
  └── virtual/        Virtual file system overlay (VirtualSourceContent processor)
```

## Key Architectural Decision

The LSP server needs to decide: **plugin or separate composition?**

- **As a plugin**: Add `LspPlugin` alongside `JvmPlugin`, used as the `CompilationSession` target.
  Reuses the existing `Compiler` orchestration and plugin discovery.
- **As a separate composition**: The LSP server constructs a `CompilationSession` directly (its own
  target driver demanding the facts LSP needs), giving full control over the long-running, per-edit
  lifecycle.

`CompilationSession` is plugin-agnostic — it takes a target plugin and the activated plugins and just
loops the compilation — so either path is now cheap. The separate-composition path is likely better,
since the server's lifecycle (cancel-restart, per-edit, query-after-run) differs fundamentally from a
batch build, and `CompilationSession` already provides exactly the resident, queryable engine it needs.

## Summary

The fact-based architecture and pervasive source tracking mean most of the *data* is already there.
The lifecycle work — making it incremental, persistent, and error-tolerant rather than batch — is now
well underway: incremental compilation, a resident `CompilationSession`, and a cancel-restart
`CompilationServer` loop are built. What remains is wiring that engine to editors — file watching, an
LSP protocol layer (JSON-RPC), a reverse position index, and graceful error recovery.
