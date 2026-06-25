#!/usr/bin/env bash
#
# Build a runnable distribution of the Eliot LSP server under ide/lsp/dist/.
#
# IMPORTANT — why this is NOT `mill ide.lsp.assembly` (a fat jar):
#   Eliot's platform-layer design relies on *multiple files at the same resource path* being
#   discovered together via ClassLoader.getResources. For example eliot/eliot/lang/String.els exists
#   in BOTH the `lang` layer (declaring `type String`, needed for string-literal typing) and the
#   `stdlib` layer (adding `def println`). PathScanner finds both URLs and the module unifier merges
#   them. A fat assembly jar collapses same-path entries into a single jar entry, silently dropping a
#   layer (e.g. `println` becomes "Name not defined."). Separate per-module jars on the classpath keep
#   each layer's copy, so getResources still returns all of them. Hence: a lib/ of per-module jars.
#
# Two classpath dirs are produced:
#   lib/          — the LSP server's classpath (lang + stdlib + jvm platform layer + eliotc + lsp + deps).
#                   The resident type-checker sees the concrete JVM platform layer (eliot-jvm.jar's .els
#                   resources: the concrete `IO`, `Pair`, `StateT`, ...), so it type-checks platform-specific
#                   code exactly as it will run — Eliot programs may legitimately use platform-only modules.
#                   Only the platform *resources* matter here: the jvm module's processors are codegen-only and
#                   are never added to the server's plugin list, so the resident server emits no bytecode.
#   compiler-lib/ — the one extra jar the JVM backend needs to BUILD a runnable jar that is not already in lib/:
#                   ASM. The "Run main" feature launches the compiler CLI with `-cp "lib/*:compiler-lib/*"`.
#                   eliot-jvm.jar lives in lib/ (not here): the CLI puts both dirs on the classpath, so a second
#                   copy would surface the platform layer twice ("Has multiple implementations." — see the merge
#                   rules in CLAUDE.md).
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

# Per-module jars keep each layer's own resources (the whole point — see header). eliot-jvm.jar carries the
# concrete JVM platform layer (its .els resources: concrete `IO`, `Pair`, `StateT`, ...); with it in lib/, the
# resident server type-checks platform-specific code the same way it will run. As a separate per-module jar its
# same-path .els resources merge with the abstract stdlib's via getResources (see header) instead of colliding.
copy_module_jar ide.lsp.jar "$LIB/eliot-lsp.jar"
copy_module_jar lang.jar "$LIB/eliot-lang.jar"
copy_module_jar stdlib.jar "$LIB/eliot-stdlib.jar"
copy_module_jar eliotc.jar "$LIB/eliot-eliotc.jar"
copy_module_jar jvm.jar "$LIB/eliot-jvm.jar"

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

# Stable launcher: a wildcard classpath keeps the jars separate (layered resources preserved).
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
