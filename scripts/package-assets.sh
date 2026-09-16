#!/usr/bin/env bash
#
# Build this repository's release assets — one zip per module that ships a compiler plugin, exactly as
# the root `eliot.pkg` declares them:
#
#   eliot-compiler.zip   lang's asset, and the one holding the compiler's entry point: eliotc's jar rides
#                        here (eliotc holds no `.els`, so it cannot be a package and nothing can ever
#                        `dep` it), together with the third-party jars every module shares.
#   eliot-stdlib.zip     stdlib's own jar. StdlibPlugin carries the compile-time natives behind `Int`'s
#                        dependent bounds, so stdlib is a plugin-shipping module like the other two.
#   eliot-jvm.zip        the jvm backend's jar and ASM, the one dependency only it has.
#
# Three properties the build system needs of these, and all three are checked below rather than assumed.
#
#   * Jars at the top level, nothing merged. One asset is one loader's worth of classpath, whether the
#     launcher unions them all (the flat union that ships first) or gives each its own child loader
#     (the decided shape). A fat jar is not an option at any level: each module jar carries a same-path
#     META-INF/services/…CompilerPlugin file, and merging them silently drops plugin registrations.
#   * Disjoint. The launcher fetches every selected module's asset and puts them on one classpath, so a
#     jar in two assets is a second copy of a layer — "Has multiple implementations.", not a waste.
#   * Complete. Every jar the jvm run classpath names lands in exactly one asset, so the three together
#     are a compiler that runs.
#
# A third-party jar rides with the *lowest* asset that needs it, which is what makes the partition a
# rule rather than three hand-written lists: add a dependency to jvm and it lands in eliot-jvm.zip;
# add one to lang and every asset below stops needing to name it.
#
# Usage: scripts/package-assets.sh [output-directory]      (default: out/release)
#
# What it is not: `ide/lsp/package.sh`, which builds a runnable LSP distribution — one combined
# wildcard classpath and a launcher script, IDE scaffolding due to retire when the build system can
# build the LSP. It proves the shape (unmerged per-module jars); it does not produce a release.
set -euo pipefail
cd "$(dirname "$0")/.."

destination="${1:-out/release}"
staging="$(mktemp -d)"
trap 'rm -rf "$staging"' EXIT

# The partition. Each asset names the modules whose own jars it holds, in dependency order; the last
# module of a group is the one whose run classpath is read, since it covers the group.
assetNames=("eliot-compiler.zip" "eliot-stdlib.zip" "eliot-jvm.zip")
assetModules=("eliotc lang" "stdlib" "jvm")

# `mill show <module>.jar` both builds the jar and prints its path as a JSON PathRef
# ("ref:v0:HASH:/abs/out.jar"); take the path part. More robust than hardcoding out/<m>/jar.dest/out.jar,
# which Mill reports as up to date even when the file has been deleted from under it.
moduleJar() {
   ./mill show "$1.jar" |
      python3 -c "import sys, json; s = json.load(sys.stdin); print(s[s.index('/'):])"
}

# The third-party jars on a module's run classpath. Upstream module output rides that classpath as a
# class directory rather than a jar, so filtering to `.jar` leaves exactly the external dependencies.
thirdPartyJars() {
   ./mill show "$1.runClasspath" |
      python3 -c "import sys, json; [print(p[p.index('/'):]) for p in json.load(sys.stdin) if p.endswith('.jar')]"
}

claimed=" "   # basenames already placed, space-delimited: the "lowest asset that needs it" rule

echo "Building release assets into $destination"
mkdir -p "$destination"

for index in "${!assetNames[@]}"; do
   asset="${assetNames[$index]}"
   modules="${assetModules[$index]}"
   content="$staging/${asset%.zip}"
   mkdir -p "$content"

   for module in $modules; do
      cp "$(moduleJar "$module")" "$content/eliot-$module.jar"
      claimed="$claimed eliot-$module.jar "
   done

   lastModule="${modules##* }"
   while read -r jar; do
      case "$claimed" in *" $(basename "$jar") "*) continue ;; esac
      cp "$jar" "$content/"
      claimed="$claimed $(basename "$jar") "
   done < <(thirdPartyJars "$lastModule")

   (cd "$content" && zip --quiet --junk-paths "$staging/$asset" ./*.jar)
   mv "$staging/$asset" "$destination/$asset"
   echo "  $asset — $(ls "$content" | wc -l) jars"
done

# Disjoint: no basename twice across the three assets. `claimed` grows by construction, so this can
# only fail if the rule above is edited into one that overlaps — which is precisely when it should.
duplicates="$(for index in "${!assetNames[@]}"; do
   unzip -Z1 "$destination/${assetNames[$index]}"
done | sort | uniq -d)"
[ -z "$duplicates" ] || { echo "Assets overlap, which is a classpath with two copies of a layer:" >&2
                          echo "$duplicates" >&2; exit 1; }

# Complete: everything the deepest layer's run classpath names is in some asset. A dependency reachable
# only from a module no asset covers would otherwise go missing from the published compiler.
missing="$(while read -r jar; do
   case "$claimed" in *" $(basename "$jar") "*) ;; *) echo "$(basename "$jar")" ;; esac
done < <(thirdPartyJars jvm))"
[ -z "$missing" ] || { echo "Not in any asset:" >&2; echo "$missing" >&2; exit 1; }

echo "Disjoint and complete."
