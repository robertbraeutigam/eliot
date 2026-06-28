#!/usr/bin/env bash
#
# Build a runnable distribution of the Eliot LSP server under ide/lsp/dist/.
#
# Layer .els sources are staged as plain filesystem roots (CP1.5), NOT read from the classpath:
#   Eliot's platform-layer design relies on *multiple files at the same resource path* being discovered together.
#   For example eliot/eliot/lang/String.els exists in BOTH the `lang` layer (declaring `type String`, needed for
#   string-literal typing) and the `stdlib` layer (adding `def println`); PathScanner must find both and let the
#   module unifier merge them. Since CP1.5 the compiler does this by scanning explicit filesystem roots — one per
#   module under eliot-src/ — rather than via ClassLoader.getResources. Keeping each module's resources in its OWN
#   eliot-src/<module> dir (never merged into one) preserves the same-path copies, exactly as separate jars used to.
#   This is also why the server is still NOT a `mill ide.lsp.assembly` fat jar: a fat jar's class layout is fine, but
#   the per-module split keeps the bundle honest and the staging trivial (one cp per module).
#
# Three output dirs are produced:
#   lib/          — the LSP server's CODE classpath (lang + stdlib + jvm + eliotc + lsp + deps). Run as
#                   `-cp "lib/*"`. The jars also still carry their .els resources, but those are now inert: the
#                   resident type-checker reads layer sources from eliot-src/ (below), not the classpath. eliot-jvm.jar
#                   is here for the JVM backend *classes* the "Run main" CLI needs; the jvm module's processors are
#                   never added to the resident server's plugin list, so the server itself emits no bytecode.
#   compiler-lib/ — the one extra jar the JVM backend needs to BUILD a runnable jar that is not already in lib/:
#                   ASM. The "Run main" feature launches the compiler CLI with `-cp "lib/*:compiler-lib/*"`.
#                   eliot-jvm.jar lives in lib/ (not here): a second copy on that combined classpath would duplicate
#                   the backend classes.
#   eliot-src/    — the abstract base (lang+stdlib), the jvm runtime layer, and the compiler platform layer (CP2) as
#                   plain .els source roots, one dir per module (CP1.5). The launcher points the `eliot.layers` system
#                   property here so the server hands them to PathScanner as --compiler-path/--runtime-path (the compiler
#                   layer feeds the compiler path only); the "Run main" CLI passes the same subdirs.
#
set -euo pipefail
cd "$(dirname "$0")/../.."   # repo root (script lives at ide/lsp/package.sh)

DIST="ide/lsp/dist"
LIB="$DIST/lib"
COMPILER_LIB="$DIST/compiler-lib"
ELIOT_SRC="$DIST/eliot-src"
LAUNCHER="$DIST/eliot-lsp"
rm -rf "$DIST"
mkdir -p "$LIB" "$COMPILER_LIB" "$ELIOT_SRC"

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
# needs; its .els resources ride along but are inert now (the resident server reads layer sources from eliot-src/, not
# the classpath — CP1.5). Kept as separate jars (not a fat assembly) so the bundle stays honest; the layered .els
# merge happens across the separate eliot-src/<module> roots staged below, not across these jars.
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

# eliot-src/ holds the layer .els as plain filesystem source roots (CP1.5). Since the classpath scan was removed, the
# server and the "Run main" CLI take the abstract base (lang+stdlib), the jvm layer, and the compiler platform layer (CP2)
# as --compiler-path/--runtime-path directories instead of finding them on the classpath (the compiler layer is on the
# compiler path only). Each module's resources/eliot tree is staged as its OWN root
# under eliot-src/<module>; they are deliberately NOT merged into one dir — same-path files like eliot/lang/String.els
# exist in both lang and stdlib, and keeping them in separate roots lets PathScanner return all copies for the unifier
# to merge (the very reason a fat jar is forbidden — see the header). The launcher points the `eliot.layers` system
# property at this dir; EliotRunConfiguration passes the same subdirs to the CLI build.
for m in lang stdlib jvm compiler; do
  mkdir -p "$ELIOT_SRC/$m"
  cp -r "$m/resources/eliot/." "$ELIOT_SRC/$m/"
done

# Stable launcher: a wildcard classpath keeps the jars separate (layered resources preserved); `eliot.layers` points the
# server at the staged layer sources (CP1.5), found beside lib/ in this same dist.
cat > "$LAUNCHER" <<'EOF'
#!/usr/bin/env bash
# Eliot LSP server launcher. Speaks LSP (JSON-RPC) over stdin/stdout; logs go to stderr.
DIR="$(cd "$(dirname "$0")" && pwd)"
exec java -Deliot.layers="$DIR/eliot-src" -cp "$DIR/lib/*" com.vanillasource.eliot.eliotc.lsp.server.LspMain "$@"
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
echo "Layer sources: $(ls "$ELIOT_SRC" | tr '\n' ' ')in $ELIOT_SRC/ (--compiler-path/--runtime-path roots, CP1.5)"
echo "Ready-to-import LSP4IJ template: $TEMPLATE/"
echo "See ide/lsp/intellij/README.md for IntelliJ setup."
