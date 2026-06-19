# Eliot IntelliJ plugin

A JetBrains plugin that gives `.els` files **syntax highlighting** (via the bundled TextMate grammar)
and **diagnostics** (via the Eliot language server, run through Red Hat's [LSP4IJ](https://plugins.jetbrains.com/plugin/23257-lsp4ij)).
Installing it is all the setup a user needs — no manual TextMate bundle, no language-server template,
no separate JDK. It is the one-click successor to the manual *user-defined server* path in
[`../lsp/intellij/`](../lsp/intellij/README.md) (which remains as a zero-build fallback).

## How it fits together

```
ide/intellij/                              ← self-contained Gradle build (not part of the Mill build)
├── build.gradle.kts                       IntelliJ Platform Gradle Plugin 2.x
├── gradle.properties                      versions (platform, LSP4IJ, plugin)
└── src/main/
    ├── kotlin/com/vanillasource/eliot/intellij/
    │   ├── EliotPlugin.kt                  locates bundled files via the plugin's own install path
    │   ├── EliotLanguageServerFactory.kt   LSP4IJ entry point (server EP)
    │   ├── EliotConnectionProvider.kt      launches the server as a child JVM
    │   └── EliotTextMateBundleProvider.kt  registers the TextMate grammar
    └── resources/META-INF/
        ├── plugin.xml                      server + *.els mapping; depends on LSP4IJ
        └── eliot-textmate.xml              optional: the textmate.bundleProvider EP
```

At **build time**, `prepareSandbox` bundles two things into the plugin distribution as loose files:

- **`<plugin>/server/lib/*.jar`** — the language-server jars produced by [`../lsp/package.sh`](../lsp/package.sh).
  These are the **separate per-module jars** (lang, stdlib, eliotc, lsp + deps), *never a fat jar* — a fat
  jar collapses same-path layer resources (e.g. `String.els` in both `lang` and `stdlib`) and silently
  drops a layer. `package.sh` is the single source of truth for that jar set.
- **`<plugin>/textmate/`** — a copy of [`../textmate/`](../textmate/), the VS Code-layout TextMate bundle.

At **run time**, `EliotConnectionProvider` launches the server out-of-process:

```
<IDE's JBR>/bin/java -cp <plugin>/server/lib/* com.vanillasource.eliot.eliotc.lsp.server.LspMain
```

Running out-of-process is deliberate: it isolates the server's `lsp4j`/`gson`/`log4j`/`cats-effect` from
the IDE's own copies (LSP4IJ ships its own `lsp4j`, so in-process would clash), and the `-cp .../lib/*`
classpath of *separate* jars preserves Eliot's platform-layer resource semantics. The JVM expands the
`/*` wildcard itself, so there is no shell wrapper (works on Windows too), and the IDE's bundled JBR is
the runtime, so no separately installed JDK is required.

The server reads its workspace roots from the LSP `initialize` handshake (`workspaceFolders`/`rootUri`),
which LSP4IJ fills in from the open project — so the open folder *is* the project model, and the standard
library travels inside the bundled jars. No build file or configuration is needed.

## Build & run

Prerequisites: a JDK 21 on `PATH` (for the Gradle build) and the repo's `./mill` (invoked by
`package.sh`). The IntelliJ platform and LSP4IJ are downloaded by Gradle on first build.

```bash
# from ide/intellij/
./gradlew runIde        # launch a sandbox IDE with the plugin (+ LSP4IJ) installed
./gradlew buildPlugin   # produce build/distributions/eliot-<version>.zip
```

`./gradlew runIde` is the quickest way to try it: open (or create) a project containing `.els` files,
and highlighting plus diagnostics (on save / on-disk change) light up.

### Install the built zip into a real IDE

**Settings → Plugins → ⚙ → Install Plugin from Disk…** → pick `build/distributions/eliot-<version>.zip`.
The IDE will offer to install LSP4IJ from the Marketplace (a declared dependency). Restart.

> Re-run `./gradlew buildPlugin` after changing compiler/server code — the server jars are rebuilt from
> source (the `packageServer` task tracks `ide/lsp`, `lang`, `stdlib`, `eliotc` sources as inputs).

## Versions

Pinned in `gradle.properties`: IntelliJ platform `platformVersion`, `lsp4ijVersion`, `pluginSinceBuild`.
LSP4IJ works on all JetBrains IDEs and editions, so the platform baseline can be lowered for wider reach.

## Current capabilities & limits

Inherited from the server (the `ide/lsp` module):

- ✅ **Syntax highlighting** for `.els` (TextMate) and **whole-workspace diagnostics** (every name is
  checked even without a `main`; fixed errors clear once corrected).
- ✅ **Live diagnostics** as you type — the server's virtual-file-system overlay type-checks the unsaved
  buffer, so errors update on every keystroke, not only on save.
- ✅ **Hover**, **Go-to-Definition**, and **Completion** (in-scope names) — answered from the server's
  reverse position index and in-scope-name index. The plugin gets them for free from the server.

## Troubleshooting

- **Everything shows "Name not defined" (e.g. `println`)** — a layer was dropped, which means a fat jar
  snuck in. The plugin must bundle the per-module jars from `package.sh`; never wire in `mill ide.lsp.assembly`.
- **Server doesn't start** — open **Language Servers → Eliot** (the LSP4IJ tool window) and check its
  console; all server logs go to **stderr**. Confirm the bundled `server/lib/` is present in the
  installed plugin directory.
- **No highlighting** — ensure the bundled TextMate plugin is enabled (it is by default); the grammar is
  registered through it.
