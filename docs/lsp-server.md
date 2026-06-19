# LSP Server Plan for Eliot

## Status

| Step | State |
|---|---|
| Incremental compilation | ✅ done (`IncrementalFactGenerator` + persistent fact cache) |
| Persistent compile lifecycle (`CompilationSession`) | ✅ done — the seam a server loops |
| Cancel-restart server loop (`CompilationServer`) | ✅ done — file watching still todo |
| Whole-workspace diagnostics driver (`LspPlugin`) | ✅ done — checks every workspace name, no `main` needed |
| LSP protocol layer + entry point (`lsp` module, lsp4j) | ✅ done — `LspMain` stdio loop, verified end-to-end |
| Diagnostics (`publishDiagnostics`) | ✅ done — grouped by URI, clears now-clean files |
| File watching trigger | 🚧 partial — `didChangeWatchedFiles` wired; no internal watcher |
| Reverse position index | ⬜ todo |
| Hover / Go-to-Definition / Completion | ⬜ todo |
| Error recovery, virtual file system (`didChange`) | ⬜ todo |

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

## Server Startup, Editor Integration & Project Model

### Who starts the server

LSP is **client-spawned**: the editor launches the server as a child process and speaks JSON-RPC
over its stdin/stdout. The server is not a daemon the IDE connects to and does not start itself — the
editor owns the lifecycle (`initialize` → `initialized` → … → `shutdown`/`exit`). The deliverable is
therefore a single server binary (`java -jar eliot-lsp.jar`, our `LspMain` over an lsp4j stdio loop),
plus a *thin per-editor adapter* whose only jobs are to know the launch command and that `.els` files
belong to it. The server is identical across editors; only the adapter differs.

### IntelliJ integration (the first target)

IntelliJ has a first-party LSP API (since 2023.2). The historical blocker — it was **Ultimate-only,
paid** — was lifted in late 2025:

- **2025.2**: the LSP API works in IntelliJ IDEA Ultimate even with **no active subscription**.
- **2025.3+**: the separate Community Edition is retired; there is one unified IntelliJ IDEA
  distribution where Ultimate features need a subscription, but **LSP support is available to
  everyone**.

So a plugin built on the native API now reaches essentially all IntelliJ IDEA users for free. The
native path is a small Kotlin/Java plugin declaring `LspServerSupportProvider` + `LspServerDescriptor`
(file type = `.els`, plus the server launch command). Remaining caveats: still **not** in Android
Studio or open-source builds, the implementation is closed-source, and you must build/publish the
plugin (target 2025.2.1+ for the no-subscription behaviour).

The portable alternative is **LSP4IJ** (Red Hat, open source): an LSP client plugin that works on
*all* JetBrains IDEs — every edition, older versions, and Android Studio. It has a **zero-plugin**
mode (the user installs LSP4IJ and points a *user-defined server* at our jar + `*.els`), which is the
fastest route to a working IntelliJ demo, and a plugin mode for one-click install.

We do **not** need the heavyweight fallback — a native PSI language plugin (custom lexer/parser/PSI) —
which would duplicate the compiler's analysis inside IntelliJ's model. LSP is available; the compiler
*is* the language server.

Recommended sequencing (both sit on the identical server, so this does not gate server work):
1. **MVP/demo** → LSP4IJ user-defined server (no IntelliJ plugin code; also covers Android Studio).
   *Built and working* — `./lsp/package.sh` produces the launcher + an importable LSP4IJ template; see
   `lsp/intellij/README.md`. Diagnostics light up in IntelliJ end-to-end.
2. **Shipped plugin** → native LSP API (now free for IntelliJ IDEA) *or* an LSP4IJ-backed plugin if
   Android-Studio / older-IDE reach matters. The native-vs-LSP4IJ choice for the shipped plugin is a
   reach-vs-integration trade-off and is deferred — it does not block the server.

**Packaging constraint — distribute per-module jars, never a fat assembly jar.** The platform-layer
mechanism relies on *multiple files at the same resource path* across classpath roots (e.g.
`eliot/lang/String.els` exists in both the `lang` layer — `type String`, for literal typing — and the
`stdlib` layer — adding `def println`), discovered together via `ClassLoader.getResources`. A fat jar
(`mill lsp.assembly`) collapses same-path entries into one and *silently drops a layer*, so e.g.
`println` becomes "Name not defined." `lsp/package.sh` therefore keeps each module in its own jar on a
`-cp "lib/*"` classpath, which preserves the multiple-resources-per-path semantics. This is a general
consequence of the layered design, not LSP-specific.

### Project model — source roots & classpath without a build file

Eliot has no build file, so the "project model" is implicit and small. The CLI supplies it as:

- **source roots** = positional `<path>...` args → `LangPlugin.pathKey` → `PathScanner(rootPaths)`;
- **stdlib + platform layers** = the **JVM launch classpath** — `PathScanner` reads
  `getClassLoader.getResources("eliot/…")`, so the standard library travels as classpath *resources*,
  not as source directories;
- **compiler plugins/backends** = the JVM launch classpath (`ServiceLoader`);
- **target/output** = `-o` + a target command word.

The consequence for a server launched from the shipped jar: the stdlib and platform layers are
already on its classpath with **zero** configuration, exactly as for the CLI. The only genuinely
project-specific input is *where the user's source is* — and LSP hands that over for free in the
`initialize` handshake as `rootUri` / `workspaceFolders`, which maps straight onto `pathKey`. So the
v1 project model is: **`workspaceFolders` → source roots, bundled classpath → stdlib**, no manifest.

A small **shared CLI/LSP manifest** (e.g. `eliot.toml`) becomes necessary only for multiple source
roots, extra layers (third-party jars adding `CompilerPlugin`s or `eliot/` resources), or a non-default
target. It is deliberately deferred; when it lands it should be read by *both* the CLI and the server,
not special-cased per editor adapter. (Caching reinforces this: `CacheFingerprint.config(args)` and the
compiler-classpath fingerprint already key `.eliot-cache`, so the project model must be stable/normalized
— a declared manifest normalizes naturally, ad-hoc argv does not.)

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
2. ✅ **Persistent server mode engine** — `CompilationSession` lifecycle and the cancel-restart
   `CompilationServer` loop done; remaining trigger source: file watching / editor `didChange`.
3. ✅ **LSP scaffolding** — `lsp` module, `lsp4j` dependency, `LspMain` stdio loop, the
   whole-workspace diagnostics driver (`LspPlugin`), language/document/workspace services.
4. ✅ **Diagnostics** — `CompilationResult.errors` streamed as `textDocument/publishDiagnostics`,
   grouped by URI, clearing now-clean files. Verified end-to-end over real stdio JSON-RPC.
5. **Reverse position index** — enables all position-based features.
6. **Hover + Go to Definition** — highest-value features for developers.
7. **Completion** — requires understanding partial/broken input contexts.
8. **Error recovery** — parser and type checker resilience for broken code.
9. **Remaining features** — references, rename, semantic tokens, signature help.

The first editor adapter is **IntelliJ** (see *Server Startup, Editor Integration & Project Model*):
LSP4IJ user-defined server for the demo, a native-API or LSP4IJ-backed plugin for the shipped build.

## Module Structure

The incremental generator and the resident compile lifecycle already live in `eliotc`
(`compiler/IncrementalFactGenerator.scala`, `compiler/CompilationSession.scala`,
`compiler/cache/`), so the new module only adds LSP-specific concerns and composes the existing
`CompilationSession`:

```
lsp/          (new module, depends on lang + stdlib; lang already depends on eliotc)
  ├── plugin/         LspPlugin — the whole-workspace diagnostics driver (target plugin)
  ├── server/         LSP protocol handling (lsp4j): LspMain stdio loop, language/document services
  ├── index/          Reverse position index            (todo)
  └── virtual/        Virtual file system overlay        (todo: VirtualSourceContent processor)
```

The diagnostics driver (`LspPlugin`) is the one genuinely new compiler-side piece. A batch build is
driven *from `main`* (`UsedNamesProcessor` walks the reachable monomorphized graph); an editor must
instead check **every** name in the workspace, including unreferenced and generic ones. `LspPlugin.run`
therefore walks the filesystem roots for `.els` files, derives each file's `ModuleName` from its
path-relative-to-root, and for every `(ModuleName, QualifiedName)` demands `SaturatedValue` — which
forces tokenize → parse → core → resolve → matchdesugar → operator and surfaces the bulk of
diagnostics **without** use-site instantiation. Deeper type errors inside generic bodies remain
use-site-verified per the cornerstone (they surface where the value is instantiated, future work via
the reverse index / a richer driver), not at the definition.

## Key Architectural Decision — resolved: separate composition

The server constructs a `CompilationSession` **directly** rather than going through CLI arg parsing /
`ServiceLoader` selection: it news up `LspPlugin` (target) + `LangPlugin` + `StdlibPlugin` and passes
them to `CompilationSession.create(targetPlugin, activatedPlugins, configuration, args)`. `LspPlugin` is
its own `CompilerPlugin` whose `initialize` adds no processors (the front end comes from `LangPlugin`)
and whose `run` is the whole-workspace diagnostics driver above. This was chosen over registering
`LspPlugin` as a ServiceLoader target because the server's lifecycle (cancel-restart, per-edit,
query-after-run) differs fundamentally from a batch build, and direct composition keeps the editor
launch path free of the CLI's positional-arg/target-word conventions — the roots come from
`workspaceFolders`, not argv. `JvmPlugin` is intentionally absent: diagnostics need the front end and
the monomorphize layer, never codegen.

## Summary

The fact-based architecture and pervasive source tracking mean most of the *data* is already there.
The lifecycle work — making it incremental, persistent, and error-tolerant rather than batch — is now
well underway: incremental compilation, a resident `CompilationSession`, and a cancel-restart
`CompilationServer` loop are built. What remains is wiring that engine to editors — file watching, an
LSP protocol layer (JSON-RPC), a reverse position index, and graceful error recovery.
