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
#   lib/          — the LSP server's classpath (lang + stdlib + eliotc + lsp + deps). This is the
#                   *abstract* workspace the resident type-checker sees; it deliberately omits the JVM
#                   backend, so diagnostics check platform-independently (no concrete Int width, etc.).
#   compiler-lib/ — the extra jars the JVM backend needs to BUILD a runnable jar from a `main`
#                   (eliot-jvm.jar + asm). The "Run main" feature launches the compiler CLI with
#                   `-cp "lib/*:compiler-lib/*"`, adding the concrete jvm platform layer on top of the
#                   server classpath. Kept separate so the resident server's own classpath is unchanged.
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

# Per-module jars keep each layer's own resources (the whole point — see header).
copy_module_jar ide.lsp.jar "$LIB/eliot-lsp.jar"
copy_module_jar lang.jar "$LIB/eliot-lang.jar"
copy_module_jar stdlib.jar "$LIB/eliot-stdlib.jar"
copy_module_jar eliotc.jar "$LIB/eliot-eliotc.jar"

# Third-party dependency jars (cats-effect, lsp4j, parsley, log4j, ...) from the run classpath.
# Entries are Mill PathRef strings ("qref:v1:HASH:/abs/path.jar"); take the absolute path part.
./mill show ide.lsp.runClasspath 2>/dev/null \
  | python3 -c "import sys, json; [print(p[p.index('/'):]) for p in json.load(sys.stdin) if p.endswith('.jar')]" \
  | while read -r jar; do cp "$jar" "$LIB/"; done

# Compiler classpath = the JVM backend module jar + ASM (the only deps not already in lib/). The jvm
# module's own .els resources (the concrete platform layer) live inside eliot-jvm.jar.
copy_module_jar jvm.jar "$COMPILER_LIB/eliot-jvm.jar"
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
echo "Compiler classpath: $(ls "$COMPILER_LIB" | wc -l | tr -d ' ') jars in $COMPILER_LIB/ (jvm backend + asm)"
echo "Ready-to-import LSP4IJ template: $TEMPLATE/"
echo "See ide/lsp/intellij/README.md for IntelliJ setup."
