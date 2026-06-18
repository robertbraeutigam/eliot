# LSP Server Plan for Eliot

## Existing Infrastructure

The compiler architecture is well-suited for LSP integration:

- **Source positions everywhere** — `Sourced[T]` wraps every AST node, resolved name, and type
  with `URI + PositionRange`. This maps directly to LSP locations.
- **Fact-based compilation** — `FactGenerator` already does lazy, cached, on-demand computation.
  The query model ("give me the type of this value") is very close to what an LSP needs.
- **Rich compilation artifacts** — `ResolvedValue` (name → definition), `MonomorphicValue`
  (type-checked values), `ModuleValue.dictionary` (in-scope names), `CompilerError` (diagnostics with
  positions) — all the data for core LSP features already exists.
- **Plugin architecture** — An LSP plugin could sit alongside `JvmPlugin` without modifying core.

## What Needs Work

### 1. Incremental Compilation (prerequisite — biggest effort)

For interactive use, sub-second feedback on edits is needed. Incremental compilation is now
implemented (`IncrementalFactGenerator` plus a persistent fact cache): a no-change rebuild does only
leaf `stat`s, and a changed file invalidates just its dependency cone. Key pieces:

- **File change detection** via `FileStat` / `UpdateTime` facts.
- **Fact cache invalidation** — when a file changes, invalidate its `SourceContent` and propagate.
  The existing `Deferred`-based caching in `FactGenerator` needs to become persistent across
  compilations and support selective invalidation.
- **Propagation cutoff** — if a function body changes but its type signature doesn't, downstream
  facts shouldn't recompute. This is the big performance win.

### 2. Long-Running Server Mode

Currently `Compiler.runCompiler()` is a one-shot batch process. Required:

- A **persistent `FactGenerator`** that survives across edits (keeps its fact cache).
- **File watching** — detect changes and trigger recompilation of affected facts.
- An **LSP protocol layer** — either use an existing Scala LSP library (e.g., `lsp4j` which Metals
  uses) or a lighter wrapper. This handles JSON-RPC, message parsing, lifecycle.
- A new **`LspPlugin`** (or a standalone server that composes `LangPlugin`'s processors without
  using the plugin system at all).

### 3. Reverse Index: Position → Fact

The compiler currently works **name → fact** (forward). LSP needs **position → fact** (reverse):

- "What's at line 5, column 12?" → find the `Sourced[ValueFQN]` or `Sourced[Expression]` whose
  range contains that position.
- This requires building a **position index** — a spatial data structure (interval tree or sorted
  list) per file, mapping `PositionRange → Sourced[T]` for all interesting nodes.
- Could be built as a post-processing pass after monomorphic type checking, collecting all `Sourced`
  wrappers from the typed AST.

### 4. Error Recovery in Parsing/Type Checking

Currently, errors abort the pipeline for that value (via `compilerAbort`). For LSP:

- The **parser** needs error recovery — return a partial AST even when syntax is broken (Parsley
  supports this to some degree).
- **Type checking** should degrade gracefully — provide partial type info even when some expressions
  fail. The `recover()` mechanism in `CompilerIO` is a foundation, but processors need to use it
  more aggressively.
- Goal: always produce *something* useful, even for broken code.

### 5. Virtual File System

The compiler reads files from disk via `FileContentReader`. For LSP:

- The server needs to use the **editor's buffer content** (unsaved changes), not the on-disk file.
- Add a `VirtualSourceContent` layer that intercepts `SourceContent` facts with in-memory overrides
  from `textDocument/didChange` notifications.
- This slots naturally into the existing fact system — just a new processor that takes priority over
  `FileContentReader`.

## LSP Feature → Existing Infrastructure Mapping

| LSP Feature | What Exists | What's Missing |
|---|---|---|
| **Diagnostics** | `CompilerError` with positions, messages, descriptions | Incremental recompilation, streaming errors |
| **Hover** (type info) | `MonomorphicValue.signature` | Position → value lookup (reverse index) |
| **Go to Definition** | `Sourced[ValueFQN]` in expressions → definition `Sourced[QualifiedName]` in `ResolvedValue` | Reverse index, cross-file navigation |
| **Completion** | `ModuleValue.dictionary` has in-scope names | Partial-name matching, context-aware filtering, triggering on incomplete input |
| **Find References** | `UsedNames` tracks which values are used | Full reverse reference index |
| **Rename** | All references are `Sourced[ValueFQN]` | Collect all occurrences project-wide |
| **Signature Help** | `FunctionDefinition.args` has parameter types | Cursor-position-aware parameter highlighting |
| **Semantic Tokens** | `SourceTokens` + resolved names have all classification info | Token classification mapping to LSP semantic token types |

## Suggested Implementation Order

1. **Incremental compilation** — implement the design doc. This is useful even without LSP.
2. **Persistent server mode** — `FactGenerator` that persists + file watching + recompilation loop.
3. **LSP scaffolding** — new module, `lsp4j` dependency, protocol handling, virtual file system.
4. **Diagnostics** — easiest LSP feature, just stream `CompilerError` as
   `textDocument/publishDiagnostics`.
5. **Reverse position index** — enables all position-based features.
6. **Hover + Go to Definition** — highest-value features for developers.
7. **Completion** — requires understanding partial/broken input contexts.
8. **Error recovery** — parser and type checker resilience for broken code.
9. **Remaining features** — references, rename, semantic tokens, signature help.

## Module Structure

```
lsp/          (new module, depends on lang)
  ├── server/         LSP protocol handling (lsp4j)
  ├── index/          Reverse position index
  ├── virtual/        Virtual file system overlay
  └── incremental/    Persistent FactGenerator with invalidation
```

## Key Architectural Decision

The LSP server needs to decide: **plugin or separate composition?**

- **As a plugin**: Add `LspPlugin` alongside `JvmPlugin`. Reuses the existing `Compiler`
  orchestration. Simpler but constrained by the one-shot batch model.
- **As a separate composition**: The LSP server directly composes `LangPlugin`'s processors into
  its own persistent `FactGenerator`. More flexible, allows full control over the lifecycle. This is
  likely the better path since the server needs fundamentally different lifecycle semantics
  (long-running, incremental, per-edit).

## Summary

The fact-based architecture and pervasive source tracking mean most of the *data* is already there.
The main work is making the *lifecycle* interactive (incremental, persistent, error-tolerant) rather
than batch.
