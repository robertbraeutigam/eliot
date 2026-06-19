# Running the Eliot language server in IntelliJ (via LSP4IJ)

The Eliot compiler doubles as a language server (`ide/lsp` module, `LspMain`). It speaks LSP over
stdin/stdout and currently provides **diagnostics** (errors/red squiggles) for every `.els` file in
the open project. This guide wires it into any JetBrains IDE — IntelliJ IDEA (any edition), Android
Studio, etc. — using Red Hat's **LSP4IJ** plugin.

> Why LSP4IJ and not IntelliJ's built-in LSP API? LSP4IJ is free, open source, and works on *all*
> JetBrains IDEs and editions with no plugin to build — a `*.els` mapping plus a launch command is
> enough. The native API (free for IntelliJ IDEA users since 2025.2) is an option for a shipped
> plugin later. See `docs/lsp-server.md`.

## 1. Build the server distribution

```bash
./ide/lsp/package.sh
```

This produces `ide/lsp/dist/` (git-ignored):

- `ide/lsp/dist/eliot-lsp` — the launcher script (this is the command IntelliJ runs).
- `ide/lsp/dist/lib/` — the per-module + dependency jars.
- `ide/lsp/dist/lsp4ij-template/template.json` — a ready-to-import LSP4IJ template with the **absolute**
  launcher path already filled in for this checkout.

> **Do not** use `mill ide.lsp.assembly` (a fat jar). Eliot's platform layers keep multiple files at the
> same resource path (e.g. `eliot/lang/String.els` exists in both the `lang` and `stdlib` layers — the
> latter is where `println` lives). A fat jar collapses them into one entry and silently drops a
> layer, so names like `println` stop resolving. `package.sh` keeps the jars separate on purpose.

Re-run `./ide/lsp/package.sh` after changing compiler/server code to refresh the jars.

## 2. Install LSP4IJ

In the IDE: **Settings → Plugins → Marketplace**, search **"LSP4IJ"** (by Red Hat), install, restart.

## 3. Register the Eliot server

Either import the generated template or configure it by hand.

### Option A — import the generated template (fastest)

1. **Settings → Languages & Frameworks → Language Servers**.
2. Click **+ → Import from custom template…** (or *Import template*) and select the folder
   `ide/lsp/dist/lsp4ij-template/`.
3. It pre-fills the **Eliot** server with the launcher command and the `*.els` mapping. Apply.

### Option B — configure manually (New Language Server dialog)

1. **Settings → Languages & Frameworks → Language Servers → + (New Language Server)**.
2. **Server** tab:
   - **Name:** `Eliot`
   - **Command:** `sh -c "/absolute/path/to/eliot/ide/lsp/dist/eliot-lsp"`
     (on Windows use `cmd /c "...\ide\lsp\dist\eliot-lsp"` with an equivalent `.bat`, or run via WSL).
3. **Mappings** tab → **File name patterns** → add `*.els` (Language ID: `eliot`).
4. Apply.

## 4. Use it

Open any project containing `.els` files (the project root becomes the workspace the server checks —
there is no build file; the open folder *is* the project model, and the standard library ships inside
the server's own jars). Errors appear as you **save** (`didSave`) or when files change on disk.

You can watch traffic under **Language Servers → Eliot → (right-click) → … trace** or set the
**Debug** tab's trace level to `verbose`.

## Current capabilities & limits

- ✅ **Diagnostics** for the whole workspace — every `.els` name is checked even without a `main`,
  and fixed errors clear on the next save.
- ⏳ Diagnostics refresh on **save / on-disk change**, not on every keystroke. Live (unsaved-buffer)
  checking awaits the virtual-file-system overlay (`textDocument/didChange`); see `docs/lsp-server.md`.
- ⏳ **Hover / Go-to-Definition / Completion** are not implemented yet (they need the reverse position
  index). Deeper type errors inside generic bodies surface at their use sites, by design.

## Troubleshooting

- **Everything shows "Name not defined" (e.g. `println`)** — you're almost certainly launching a fat
  assembly jar instead of `ide/lsp/dist/eliot-lsp`. Use the launcher; see the warning in step 1.
- **Server doesn't start** — run `ide/lsp/dist/eliot-lsp` in a terminal; it should block waiting for LSP
  input on stdin. Check `java` is on `PATH`. All server logs go to **stderr** (stdout is the protocol
  channel), so the LSP4IJ console's error stream shows them.
- **No diagnostics at all** — confirm the `*.els` mapping is present and the file is inside the open
  project root.
