# LSP Server Plan for Eliot

## Status

| Step | State |
|---|---|
| Incremental compilation | ✅ done (`IncrementalFactGenerator` + persistent fact cache) |
| Persistent compile lifecycle (`CompilationSession`) | ✅ done — the seam a server loops |
| Cancel-restart server loop (`CompilationServer`) | ✅ done |
| Whole-workspace diagnostics driver (`LspPlugin`) | ✅ done — checks every workspace name, no `main` needed |
| LSP protocol layer + entry point (`ide/lsp` module, lsp4j) | ✅ done — `LspMain` stdio loop, verified end-to-end |
| Diagnostics (`publishDiagnostics`) | ✅ done — grouped by URI, clears now-clean files |
| IntelliJ editor adapter (LSP4IJ) | ✅ done — `ide/lsp/package.sh` dist + importable template; `ide/lsp/intellij/README.md` |
| File watching trigger | ✅ editor-driven done — server registers `**/*.els` watchers so `didChangeWatchedFiles` actually fires (+ `didSave`); in-process watcher intentionally not added |
| Reverse position index | ✅ done (`ide/lsp/.../index/PositionIndex.scala`), rebuilt per compile |
| Hover / Go-to-Definition | ✅ done — answered from the index; verified end-to-end |
| Completion | ⬜ todo |
| Error recovery, virtual file system (`didChange`) | ⬜ todo |

The fact-based architecture and pervasive source tracking mean most of the *data* is already there.
The resident-engine lifecycle (incremental compilation, a persistent `CompilationSession`, a
cancel-restart `CompilationServer`), the `lsp4j` protocol layer, the whole-workspace diagnostics
driver, the IntelliJ (LSP4IJ) adapter, and now the reverse position index with **hover** and
**go-to-definition** on top of it are built and verified end-to-end. What remains is depth: a virtual
file system for live edits, completion, and error recovery for broken code.

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

### 2. Long-Running Server Mode — ✅ engine + loop + protocol done; internal watcher/VFS remain

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

**The LSP protocol layer and entry point are now built** (`lsp` module, `lsp4j` 0.23.1). `LspMain`
runs an `LSPLauncher` stdio loop; `EliotLanguageServer` reads `workspaceFolders`/`rootUri` on
`initialize` and starts the engine on `initialized`; `EliotCompilationService` is the cats-effect ↔
lsp4j bridge (constructs the `CompilationSession`, holds the `CompilationServer` open via `allocated`,
runs `requestCompile`/`shutdown` on an `IORuntime`); `EliotDiagnostics` maps `CompilerError` →
`publishDiagnostics`. The document services trigger `requestCompile` on `didOpen`/`didSave`, and
`didChangeWatchedFiles` drives on-disk watching. Crucially, that notification is **registration-only**
in LSP (no static `ServerCapabilities` flag exists for it), so on `initialized` the server now sends a
`client/registerCapability` for `workspace/didChangeWatchedFiles` with a `**/*.els` `FileSystemWatcher`
— gated on the client's `didChangeWatchedFiles.dynamicRegistration` capability — which is what makes the
editor actually emit those notifications (`EliotCompilationService.registerFileWatchers`). Verified
end-to-end over real stdio JSON-RPC (diagnostics appear for a broken file and clear on fix). Logs go to
**stderr** (stdout is the protocol channel) via `ide/lsp/resources/log4j2.xml`.

Trigger source is now editor-driven and complete; what remains is live edits:

- **In-process file watching — intentionally not added.** With the watcher registration above, the
  editor's own filesystem watcher already relays *all* on-disk changes, including external ones (`git
  checkout`, codegen, another tool), so a redundant in-process NIO `WatchService` buys little for a
  client-spawned server and carries real cost (recursive subdir registration, debouncing, a background
  fiber lifecycle, and the JDK's slow ~10s polling fallback on macOS). It would only matter for a
  headless/no-editor host or a client lacking watch-registration support — neither a current target.
- **Live (unsaved-buffer) checking** — the Virtual File System below (`textDocument/didChange`);
  `didChange` is currently a deliberate no-op so diagnostics never disagree with an unsaved buffer.

### 3. Reverse Index: Position → Fact — ✅ done

The compiler works **name → fact** (forward); LSP needs **position → fact** (reverse). Built as
`ide/lsp/.../index/PositionIndex.scala`:

- It is rebuilt after every compile in `EliotCompilationService.publishResult`, from the
  `ResolvedValue` facts the compile materialised (`generator.currentFacts()`). The whole-workspace
  driver (`LspPlugin`) demands every workspace name, so every workspace value's resolved form is
  present — no extra walk.
- For each resolved value it collects, from the body (`runtime`) and type signature (`typeStack`),
  every `Sourced[ValueFQN]` — i.e. each value/constructor reference, with the *precise range of the
  referenced name* (also constructor names in `match` patterns). It separately records each value's
  own definition site keyed by `ValueFQN`.
- A lookup (`referenceAt(uri, position)`) returns the **most specific** reference whose range contains
  the position (innermost wins — later `from`, then earlier `to`); `definitionAt`/`hoverAt` compose it
  with the `ValueFQN → ResolvedValue` map. Positions are the compiler's 1-based domain; the protocol
  layer (`LspPositions`, shared with diagnostics) converts to/from LSP's 0-based positions. URIs are
  normalised to a path key so the editor's `file:///…` matches the compiler's `file:/…`.
- Chosen as a **post-compile in-memory index in the LSP layer** rather than a compiler fact: the index
  spans the whole workspace (it must aggregate across values to answer per-file queries), it is cheap
  to rebuild from already-materialised facts, and keeping it out of the fact graph avoids a
  non-serializable, whole-workspace fact. Lookups are synchronous off an `AtomicReference`.
- Note: `MonomorphicValue` is keyed by `(vfqn, typeArguments)` — a value can have several
  specializations. Hover currently shows the generic `ResolvedValue` signature (always available,
  instantiation-independent); presenting a representative specialization is future work.
- Known limitation: references that resolve into the **bundled stdlib / platform layers** (classpath
  `jar:` resources) are not navigable — those `ResolvedValue`s are not workspace facts and their
  definition URIs aren't openable files. `definitionAt` simply returns nothing (fail-safe). Navigation
  among the user's own workspace files works in full, including cross-file.

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

### IntelliJ integration (the first target) — ✅ MVP wired via LSP4IJ

> **Status:** the LSP4IJ user-defined-server path is **built and verified**. `./ide/lsp/package.sh`
> produces the `lsp/dist/` launcher + an importable LSP4IJ template; `ide/lsp/intellij/README.md` is the
> step-by-step setup (install LSP4IJ → import template → open a project with `.els` files →
> diagnostics on save). A native-API or LSP4IJ-backed *shipped plugin* is the deferred next step
> (sequencing below).

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
   *Built and working* — `./ide/lsp/package.sh` produces the launcher + an importable LSP4IJ template; see
   `ide/lsp/intellij/README.md`. Diagnostics light up in IntelliJ end-to-end.
2. **Shipped plugin** → native LSP API (now free for IntelliJ IDEA) *or* an LSP4IJ-backed plugin if
   Android-Studio / older-IDE reach matters. The native-vs-LSP4IJ choice for the shipped plugin is a
   reach-vs-integration trade-off and is deferred — it does not block the server.

**Packaging constraint — distribute per-module jars, never a fat assembly jar.** The platform-layer
mechanism relies on *multiple files at the same resource path* across classpath roots (e.g.
`eliot/lang/String.els` exists in both the `lang` layer — `type String`, for literal typing — and the
`stdlib` layer — adding `def println`), discovered together via `ClassLoader.getResources`. A fat jar
(`mill ide.lsp.assembly`) collapses same-path entries into one and *silently drops a layer*, so e.g.
`println` becomes "Name not defined." `ide/lsp/package.sh` therefore keeps each module in its own jar on a
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
| **Diagnostics** | `CompilerError` with positions, messages, descriptions; `CompilationResult.errors` | ✅ done — streamed as `publishDiagnostics`, grouped by URI, cleared on fix |
| **Hover** (type info) | `ResolvedValue.typeStack` signature | ✅ done — `PositionIndex.hoverAt` renders `name : type` |
| **Go to Definition** | `Sourced[ValueFQN]` in expressions → definition `Sourced[QualifiedName]` in `ResolvedValue` | ✅ done — `PositionIndex.definitionAt`; workspace + cross-file (stdlib jar resources excluded) |
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
5. ✅ **Reverse position index** — `PositionIndex`, rebuilt per compile; enables position-based features.
6. ✅ **Hover + Go to Definition** — answered from the index; verified end-to-end over real stdio JSON-RPC.
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
ide/lsp/      (module ide.lsp, depends on lang + stdlib; lang already depends on eliotc)
  ├── plugin/         LspPlugin — the whole-workspace diagnostics driver (target plugin)
  ├── server/         LSP protocol handling (lsp4j): LspMain stdio loop, language/document services,
  │                   definition/hover handlers, LspPositions (1-based ⇄ 0-based) shared with diagnostics
  ├── index/          PositionIndex — reverse position index (done)
  └── virtual/        Virtual file system overlay            (todo: VirtualSourceContent processor)
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

The fact-based architecture and pervasive source tracking meant most of the *data* was already there;
the work has been making the *lifecycle* interactive rather than batch. That spine is now built and
verified end-to-end:

- **Resident engine** — incremental compilation (`IncrementalFactGenerator` + persistent cache), a
  persistent `CompilationSession`, and a cancel-restart `CompilationServer` loop.
- **LSP server** — the `ide/lsp` module: an `lsp4j` stdio protocol layer (`LspMain`/`EliotLanguageServer`),
  the whole-workspace diagnostics driver (`LspPlugin`), and `publishDiagnostics` that clears on fix.
- **Position features** — a `PositionIndex` (reverse position → fact map) rebuilt per compile, driving
  **go-to-definition** and **hover** over the workspace's resolved values.
- **Editor** — IntelliJ wired via LSP4IJ (`ide/lsp/package.sh` + `ide/lsp/intellij/`); proven over real stdio.

What remains is depth, not spine: a **virtual file system** for live (unsaved-buffer) checking,
**completion**, and **error recovery** in the parser and checker for broken code. (On-disk file watching
is done — the server registers `**/*.els` watchers so the editor relays disk changes; an in-process
watcher is intentionally skipped, see §2.) None of these block the others; the highest-value next step is
the virtual file system (live edits) or completion.
