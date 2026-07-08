#!/usr/bin/env bash
#
# Build a runnable distribution of the Eliot LSP server under ide/lsp/dist/.
#
# This distribution ships CODE ONLY. The server does NOT bundle any Eliot `.els` layer sources: the abstract base,
# the standard library and the platform layers are ordinary dependencies that reach the compiler on the *path* — the
# editor's workspace roots today, downloaded packages once a build system exists — exactly like any other program's
# stdlib. So there is nothing here to keep the per-module `.els` from colliding; the jars carry compiled classes only.
#
# The server is still NOT a `mill ide.lsp.assembly` fat jar: each module jar carries a same-path ServiceLoader file
# (META-INF/services/…CompilerPlugin, naming LangPlugin / StdlibPlugin / JvmPlugin / ApiDocPlugin), and a fat jar would
# collapse those four to one — silently dropping the plugin registrations the compiler CLI discovers via ServiceLoader.
# The per-module split also keeps the bundle honest and staging trivial (one cp per module).
#
# Two output dirs are produced:
#   lib/          — the LSP server's CODE classpath (lang + stdlib + jvm + eliotc + lsp + deps). Run as
#                   `-cp "lib/*"`. These jars carry compiled classes ONLY — no .els (each layer keeps its `.els` in a
#                   separate `eliot/` source root, not `resources/`, so they are never jar-bundled). eliot-jvm.jar is
#                   here for the JVM backend *classes* the "Run main" CLI needs; the jvm module's processors are never
#                   added to the resident server's plugin list, so the server itself emits no bytecode.
#   compiler-lib/ — the one extra jar the JVM backend needs to BUILD a runnable jar that is not already in lib/:
#                   ASM. The "Run main" feature launches the compiler CLI with `-cp "lib/*:compiler-lib/*"`.
#                   eliot-jvm.jar lives in lib/ (not here): a second copy on that combined classpath would duplicate
#                   the backend classes.
#
set -euo pipefail
cd "$(dirname "$0")/../.."   # repo root (script lives at ide/lsp/package.sh)

DIST="ide/lsp/dist"
LIB="$DIST/lib"
COMPILER_LIB="$DIST/compiler-lib"
LAUNCHER="$DIST/eliot-lsp"
rm -rf "$DIST"
mkdir -p "$LIB" "$COMPILER_LIB"

echo "Building module jars..."

# Resolve a module jar's real path via `mill show` (which also forces it to be built) and copy it to a
# stable name. `mill show X.jar` prints a JSON PathRef string ("ref:v0:HASH:/abs/out.jar"); we take the
# path part (from the first '/'). This is more robust than hardcoding out/<m>/jar.dest/out.jar: deleting
# that dir out from under Mill leaves the task "up to date" yet the file gone, whereas `show` materialises.
copy_module_jar() { # <mill-jar-target> <destination-file>
  local path
  path="$(./mill show "$1" 2>/dev/null \
    | python3 -c "import sys, json; s = json.load(sys.stdin); print(s[s.index('/'):])")"
  cp "$path" "$2"
}

# Per-module CODE jars for the server/CLI classpath. eliot-jvm.jar carries the JVM backend *classes* the "Run main" CLI
# needs; the jars hold no .els (layer sources live in a separate `eliot/` source root, not `resources/`, so they are
# never jar-bundled — they reach the compiler on the path as dependencies, not from this distribution). Kept as separate
# jars (not a fat assembly) so the bundle stays honest.
copy_module_jar ide.lsp.jar "$LIB/eliot-lsp.jar"
copy_module_jar lang.jar "$LIB/eliot-lang.jar"
copy_module_jar stdlib.jar "$LIB/eliot-stdlib.jar"
copy_module_jar eliotc.jar "$LIB/eliot-eliotc.jar"
copy_module_jar jvm.jar "$LIB/eliot-jvm.jar"
# eliot-apidoc.jar carries the doc-comment pipeline (ApiDocPlugin + ValueDocProcessor). The resident server activates
# ApiDocPlugin (processors only; its HTML `run` never fires) so hover can read the same doc facts the apidoc site is
# built from. Copied by name for the same reason as the others: upstream module output rides runClasspath as a class
# dir, not a jar, so the wildcard `lib/*` classpath would otherwise miss it.
copy_module_jar apidoc.jar "$LIB/eliot-apidoc.jar"

# Third-party dependency jars (cats-effect, lsp4j, parsley, log4j, asm, ...) from the run classpath.
# Entries are Mill PathRef strings ("qref:v1:HASH:/abs/path.jar"); take the absolute path part.
./mill show ide.lsp.runClasspath 2>/dev/null \
  | python3 -c "import sys, json; [print(p[p.index('/'):]) for p in json.load(sys.stdin) if p.endswith('.jar')]" \
  | while read -r jar; do cp "$jar" "$LIB/"; done

# compiler-lib/ holds only ASM — the one backend dep the "Run main" CLI build needs to emit bytecode. The CLI
# runs `-cp "lib/*:compiler-lib/*"`; eliot-jvm.jar is deliberately NOT copied here (it is in lib/), since a second
# copy of the platform layer on that combined classpath would be a "Has multiple implementations." error.
./mill show jvm.runClasspath 2>/dev/null \
  | python3 -c "import sys, json; [print(p[p.index('/'):]) for p in json.load(sys.stdin) if p.endswith('.jar') and '/asm-' in p]" \
  | while read -r jar; do cp "$jar" "$COMPILER_LIB/"; done

# Stable launcher: a wildcard classpath keeps the per-module jars separate. The launcher carries no layer sources — the
# language client hands the server its workspace roots on `initialize`, and the standard library + platform layers come
# in on that same path as dependencies (see EliotCompilationService).
cat > "$LAUNCHER" <<'EOF'
#!/usr/bin/env bash
# Eliot LSP server launcher. Speaks LSP (JSON-RPC) over stdin/stdout; logs go to stderr.
DIR="$(cd "$(dirname "$0")" && pwd)"
exec java -cp "$DIR/lib/*" com.vanillasource.eliot.eliotc.lsp.server.LspMain "$@"
EOF
chmod +x "$LAUNCHER"

# A ready-to-import LSP4IJ template with the absolute launcher path filled in.
TEMPLATE="$DIST/lsp4ij-template"
mkdir -p "$TEMPLATE"
ABS_LAUNCHER="$(cd "$(dirname "$LAUNCHER")" && pwd)/$(basename "$LAUNCHER")"
cat > "$TEMPLATE/template.json" <<EOF
{
  "id": "eliot",
  "name": "Eliot",
  "programArgs": {
    "default": "sh -c \"$ABS_LAUNCHER\""
  },
  "fileTypeMappings": [
    {
      "fileType": { "name": "Eliot", "patterns": ["*.els"] },
      "languageId": "eliot"
    }
  ]
}
EOF

echo
echo "Built $LAUNCHER ($(ls "$LIB" | wc -l | tr -d ' ') jars in $LIB/)"
echo "Compiler classpath: $(ls "$COMPILER_LIB" | wc -l | tr -d ' ') jars in $COMPILER_LIB/ (asm; jvm backend is in lib/)"
echo "Ready-to-import LSP4IJ template: $TEMPLATE/"
echo "See ide/lsp/intellij/README.md for IntelliJ setup."
