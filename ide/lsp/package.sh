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
set -euo pipefail
cd "$(dirname "$0")/../.."   # repo root (script lives at ide/lsp/package.sh)

DIST="ide/lsp/dist"
LIB="$DIST/lib"
LAUNCHER="$DIST/eliot-lsp"
rm -rf "$DIST"
mkdir -p "$LIB"

echo "Building module jars..."
./mill ide.lsp.jar lang.jar stdlib.jar eliotc.jar >/dev/null

# Per-module jars keep each layer's own resources (the whole point — see header).
# The lsp module is nested (ide.lsp), so its jar lands under out/ide/lsp/.
cp "out/ide/lsp/jar.dest/out.jar" "$LIB/eliot-lsp.jar"
for module in lang stdlib eliotc; do
  cp "out/$module/jar.dest/out.jar" "$LIB/eliot-$module.jar"
done

# Third-party dependency jars (cats-effect, lsp4j, parsley, log4j, ...) from the run classpath.
# Entries are Mill PathRef strings ("qref:v1:HASH:/abs/path.jar"); take the absolute path part.
./mill show ide.lsp.runClasspath 2>/dev/null \
  | python3 -c "import sys, json; [print(p[p.index('/'):]) for p in json.load(sys.stdin) if p.endswith('.jar')]" \
  | while read -r jar; do cp "$jar" "$LIB/"; done

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
echo "Ready-to-import LSP4IJ template: $TEMPLATE/"
echo "See ide/lsp/intellij/README.md for IntelliJ setup."
