# The Eliot Build System: Git-Native Packages, Declarative Descriptor, Standard Verbs

Status: **DESIGN** — no implementation yet. Records the design discussion of 2026-07-21.
Fixes the model, the semantics, the descriptor contents, and the descriptor syntax
(`eliot.pkg`).

## The core decision: a descriptor, not build-as-code

The build is described by an **inert data descriptor** at the repo root plus a **fixed verb set**
(`eliot build/run/test …`), Maven-model; it is *not* a program (Gradle/sbt/Mill-style). The
build-as-code trend is a polyglot-tool phenomenon — a general-purpose tool cannot assume a build
model, so it must let users program one. A language-owned tool *defines* the build model, and the
language-owned tools that took this route (Cargo, Go) are the ones their users praise. Rationale:

- **Same verbs on every project.** A build you have never seen is operable and readable.
- **Tools read data without executing it.** The LSP, a mirror, a future `eliot migrate` all parse
  the descriptor directly; no build-tool daemon, no code execution on IDE import (contrast Gradle
  sync). Mechanical ecosystem-wide format migrations stay possible.
- **Opening a project runs no code.** Reproducibility and supply-chain hygiene by construction.
- **Eliot needs less build tool than almost any language.** The compiler already is the build
  engine (demand-driven facts, plugin backends, whole-program compilation from `main`, layer
  assembly via paths). The build tool reduces to: resolve dependencies → assemble roots →
  invoke compiler per configuration → package. Source generation — the classic pressure toward
  build scripts — already has a better home: compiler plugins contribute `SourceMount`s.

Maven's actual failure modes are designed out, not in: plugin configuration is schema-validated
(never stringly-typed blobs), there is no parent-descriptor inheritance and no profiles, and the
escape hatch is scoped Cargo-style — plugins extend *inside* the standard model and can never
redefine verbs or the meaning of the descriptor.

There is no `upload` verb. What "upload" means is what *run* means on that platform: on jvm,
`eliot run` is `java -jar`; on an MCU it is flash + reset + monitor. The verb set stays fixed and
platform-neutral; the platform's backend plugin supplies the meaning of `run` (and `test` —
hardware-in-the-loop later). Precedent: embedded Rust never got `cargo flash`; it got
`runner = probe-rs` under an unchanged `cargo run`.

## Distribution: git-native

A **package is a git repo with the descriptor at its root**. There is no registry and no publish
step — pushing is publishing. This fits Eliot unusually well: whole-program compilation means
there is no intermediate representation and no binary artifact to host — a package *is* its
source tree, and git is already the world's content-addressed source store. Local development is
symmetric: a dependency is a URL or a filesystem path.

### Identity

**The URL is the package identity.** There is no separate name — a name would need a central
place to live, which is the registry we are avoiding (Go ran URL-as-identity at scale). Two
consequences accepted deliberately:

- **URL equality must be defined on day one** (canonical form: lowercase host, strip `.git`, one
  scheme). Swift shipped without this and paid for years; in Eliot the failure is worse — the
  same package resolved twice collides at the FQN merge with a baffling error far from the cause.
- **No package-level namespace claim.** An ownership rule ("only this package defines
  `com.acme.*`") was considered and **rejected**: layers *deliberately* redefine names they do
  not own — that is the platform-implementation mechanism. The compiler's merge already enforces
  what matters (at most one implementation, signatures agree, used ⇒ exists). The build tool's
  contribution is **attribution**: merge errors name the packages (URL + version + module) that
  contributed each colliding definition. Accepted residual: two strangers defining the same name
  with a lexically identical signature silently merge — rare enough to live with; an advisory
  warning can be added later with no descriptor change.

### Versions: branches are compatibility lines, tags are versions

- A **major version is a branch** (`v1`, `v2`): an append-only compatibility contract. Breaking
  changes start a new branch. `git ls-remote` on the branch is update discovery — no registry API.
- **Versions are annotated tags on the branch** (`v1.4`, `v1.5`). The resolver selects tags,
  never branch heads: a moving head names different code on different days, has no
  human-readable version, and gives MVS nothing to order. A commit must be tagged to be
  consumable (no Go-style pseudo-versions).
- **The guarantee has teeth**: `eliot compat-check` diffs exported signatures between the branch
  head and a candidate tag and refuses removals/changes (Elm precedent: computed, not promised).
  Caveat, accepted: use-site verification means a dependency's *body* change can surface new
  obligations at a consumer's call sites under an unchanged signature. This is safe because
  upgrades are explicit (lockfile): latent breakage appears only when the user opts into an
  upgrade, as an ordinary type error at a visible moment — never as silent drift.
- **This guarantee is load-bearing for the layer ecosystem.** The abstract↔concrete merge is
  lexically exact, so a third-party platform layer built against `foo v1.4` still merges when MVS
  selects `foo v1.6` — *only because* signatures are append-only within the branch. Git
  versioning and cross-repo layers are one design, not two features.

### Resolution: Minimal Version Selection

Go's MVS, adopted as-is: a dependency declaration is a **minimum** (never a range, never an upper
bound); resolution takes the transitive closure and picks, per package, the **maximum of the
declared minimums** — the oldest version satisfying everyone. Deterministic without a lockfile,
no silent upgrades (publishing changes nobody's build; only raising a minimum does), no solver.
Ancestry on a release branch gives the version ordering almost for free.

**One major per package per program** (v1 rule): two majors of one package would collide at the
FQN merge, so conflicting-major requirements are a resolver error. Known deferred problem —
Go added coexistence (module/v2 = distinct identity) because ecosystem-wide major migrations are
brutal without it; revisit when it hurts.

### Lockfile

Descriptor = intent ("`foo >= v1.3`"); lockfile = fact. Per resolved dependency: **tag + commit
hash + a content hash of the source tree** (the go.sum lesson: the commit hash pins history; the
content hash detects a rewritten or substituted remote). Since resolution is per build
configuration (below), the lock records **per-configuration resolutions**. With MVS the lock is
nearly redundant for resolution; it survives as the integrity record.

### Location drift and availability — registry-free indirection

Identity answers "same package?"; it does not keep repos findable or alive. Three mechanisms,
none central:

1. **Root-only `replace`** (consumer-controlled): the root project may map URL X → URL Y or a
   local path. Only the root's replaces apply, never a dependency's (Go's rule — keeps resolution
   local). This is also the local-development story.
2. **Mirrors as resolver configuration** (consumer-controlled), never descriptor content. Git is
   content-addressed — a commit hash is a Merkle root — so once the lock pins a hash, *any*
   remote can serve the bytes trustlessly. The design obligation is only negative: do not bake
   "fetch only from the identity URL" into the resolver. A proxy/cache can be run later with zero
   protocol change; the left-pad endgame (immutable proxy + checksum transparency log) stays
   available if the ecosystem ever needs it.
3. **Vanity URLs** (author-controlled, deferrable): identity under a domain the author owns, an
   HTTP response pointing at the current git remote. DNS is the decentralized registry.

## The descriptor

Contents test: **does the resolver or compiler act on it?** Human-facing prose lives in the
README (rendered from git, the Go move); legal text in LICENSE. Deliberately absent: package
name (identity is the URL), the package's own version (the tag carries it — no bump-commit
ritual, no file/tag disagreement), authors, description, and a toolchain-version directive
(the toolchain is a dependency — see below).

What remains:

- **Dependencies**: URL (+ optional module selector into that repo) + minimum version. Scoped:
  base, per-module, test, per-configuration.
- **Modules**: the repo's build modules (the eliot repo itself: `lang`, `stdlib`, `jvm`). Per
  module: name, export flag (dependents mount every exported module's sources; examples/apps/test
  fixtures are internal), its own dependency list. Directories by convention, not configuration.
  **One root descriptor** — per-module descriptor files reintroduce Maven's parent-POM web and
  Go's nested-modules mess, and break the one-parse LSP story.
- **Build configurations** (see below): name, additional dependencies, backend-plugin invocation
  + parameters (main module, artifact kind, output).
- **Plugin binaries** (Maven coordinates — transitional, see below), only in packages that ship
  compiler plugins.

The load-bearing property of the format: **inert data, parseable without the compiler**.
Types-are-values will tempt an Eliot-expression descriptor; resist — that is build-as-code
through the back door, re-coupling every tool to the evaluator.

## Descriptor syntax: `eliot.pkg`

A **custom line-oriented format**, not a general-purpose one (TOML/JSON/YAML):

- **The tool edits the file.** MVS workflows (`eliot get`-style commands raising minimums,
  adding dep lines) machine-edit it, and it lives in git — one clause per line survives
  programmatic edits, diffs, and three-way merges. Nested TOML tables do neither well.
- **The domain is small enough to own.** Seven-ish clause kinds; a tiny grammar makes illegal
  states unrepresentable instead of validating a generic tree after parsing. go.mod proved the
  approach (and is the most readable manifest in the industry for it).
- **It should feel like Eliot** — `--` comments, lowercase keywords, the language's restraint.
  This is still inert data: "parseable without the compiler" is about *no evaluation* and a
  trivially specified grammar, not about borrowing someone else's format. The spec plus the
  resolver library are the reference parser; a TextMate grammar ships in `ide/textmate/`.

Files: **`eliot.pkg`** (repo root) and **`eliot.lock`** (tool-written) — naming consistent with
the `eliot.paths` precedent this system retires.

### Invariants — the format teaches the model

1. **No version operators exist.** Every version token is a minimum (MVS), spelled exactly as
   the git tag (`v1.2`). There is no `>=` to write and no range to express — the syntax cannot
   state what the resolver doesn't do. The one exact pin in the system (plugin jars) rides
   inside the Maven coordinate string, which is exact by nature; the invariant holds without
   exception.
2. **Dependency URLs are scheme-less** (`github.com/x/foo`) — normalization by construction (no
   `https://` vs `ssh://` spellings to unify; transports are resolver config), and it frees `//`
   unambiguously as the **module selector**: `github.com/eliot-lang/eliot//stdlib`. A bare
   `//name` is a sibling module of this repo.
3. **Comments are `--`**, like the language.
4. A file is a sequence of clauses — `keyword args…`, optionally followed by a `{ … }` block of
   sub-clauses. That is the whole grammar. **Unknown clauses are fatal** with an upgrade hint
   ("this descriptor needs a newer eliot") — this is how the format evolves without a
   format-version field (see the toolchain section).

### Clause reference

```
dep github.com/x/foo v1.3                    -- base dep: all exported modules of that repo
dep github.com/eliot-lang/eliot//stdlib v1.2 -- module-selected dep

module lang {                                -- multi-module repos only
  internal                                   -- not exported (default: exported)
  dep //other-module                         -- intra-repo sibling
  dep github.com/x/bar v2                    -- module-scoped dep
  plugin com.vanillasource:eliot-lang:0.5.0  -- ships a compiler plugin (Maven coord, exact)
}

test {                                       -- test scope (also allowed inside module)
  dep github.com/eliot-lang/eliot-test v1
  dep github.com/eliot-lang/eliot//jvm v1    -- the host-runnable platform layer
}

artifact hello {                             -- a build configuration
  dep github.com/eliot-lang/eliot//jvm v1    -- platform layers live HERE, per artifact
  backend {                                  -- params schema-validated by the plugin
    kind exe-jar
    main HelloWorld
  }
}

replace github.com/x/foo with ../foo-local   -- root-only; path or URL
```

- `backend` names no platform: the backend is located among the artifact's deps (the packages
  declaring `plugin`). With exactly one candidate no URL is needed; with several, disambiguate:
  `backend github.com/…//jvm { … }`. Its parameters are free-form keys validated against the
  plugin's declared schema — the typed-plugin-config promise, enforced at parse time.
- **No `module` clause** = the repo is one anonymous exported module with `src/`, `test/`,
  `compiler/` at the root — the zero-config common case.
- Modules of one repo version together (tags are repo-wide): selector lines into the same repo
  at different minimums simply both feed MVS. Per-module versioning does not exist.

### Examples

A minimal library — fully explicit, satisfying the "src resolves against declared deps alone"
lint literally (nothing ambient, including the language itself):

```
dep github.com/eliot-lang/eliot//stdlib v1
dep github.com/somelib/collections v1.3
```

An application targeting two platforms:

```
dep github.com/somelib/sensor-api v2

artifact controller-jvm {
  dep github.com/eliot-lang/eliot//jvm v1
  dep github.com/somelib/sensor-api-jvm v2
  backend { kind exe-jar, main Controller }
}

artifact controller-attiny {
  dep github.com/vendor/eliot-avr v1
  dep github.com/somelib/sensor-api-avr v2
  backend { kind flash-image, main Controller, mcu attiny85 }
}
```

The dogfood — the eliot repo itself: `module lang { plugin … }`, `module stdlib { dep //lang }`,
`module jvm { dep //stdlib, plugin … }`, `module examples { internal, dep //jvm }`.

The lockfile uses the same clause style, machine-written: `lock <url> <tag> <commit>
<tree-hash>` per resolved dependency per artifact, `lock-jar <coord> <sha256>` for plugin
binaries. Its exact format is tool-owned output, not hand-polished here.

## Standard layout: one base plus conditional overlays

Per module, three conventional directories. Only `src/` is mandatory. Libraries and layers are
**not differentiated** — layer-ness is not declared anywhere; it is just what your sources do
(even a pure-`src` package can concretely re-declare foreign abstract names).

| dir | active | runtime pool | compiler pool | exported |
|---|---|---|---|---|
| `src/` | always | yes | yes (borrowed) | per module flag |
| `compiler/` | always | no | yes, as **override** files | **yes** |
| `test/` | test verb only | yes | yes (borrowed) | **no** |

Each conventional name encodes a fixed answer to three orthogonal questions — when it activates,
which pools it mounts into *and how*, whether it ships to dependents. This is why the tracks
cannot be anonymous mill-style submodules: mounting mode is semantic information the tool must
know, and the name is what carries it. Note the two cells that are easy to get wrong: `compiler/`
**is exported** (a downstream program's checking needs your compile-time instances, exactly as
stdlib's overlay serves every program today), and `test/` joins both pools while testing yet
never ships — export is orthogonal to pool membership.

**The compiler–src borrow is kept, necessarily.** In Eliot any ordinary definition can appear in
a type-level position, so the NbE checker routinely evaluates user `src` code; severing the
borrow would break type-level use of your own definitions and force hand-copying every pure body
into `compiler/`. The native-leaf boundary is the existing fail-safe (a borrowed body reaching a
platform leaf stalls loudly, never misevaluates). `compiler/` therefore holds only what it holds
today as `eliot-compiler/`: self-sufficient redefinitions, checking-only instances, compile-time
natives.

**The compiler platform is the one special case** — the only platform *every* build activates,
on every consumer's machine, for every target. So its layer cannot be an opt-in dependency: it
travels with every package unconditionally (the conventional directory), and a library whose
compile track is not self-sufficient is broken for everyone. Two lints the tool owes at the
*author's* build, not some consumer's:

- `src` must resolve against declared dependencies alone;
- the compile track (`src` + `compiler/` across the dependency closure) must resolve **with no
  runtime platform layer present** — the existing self-sufficiency rule, machine-checked at the
  package boundary.

## Platforms: dependencies + a backend call, not a target vocabulary

There is **no "platform" or "target" concept in the descriptor**. A build configuration is:
additional dependencies + a compiler-plugin (backend) invocation + parameters. Platform is
*emergent* from which layer packages the configuration depends on and which backend it calls —
and those arrive together: the jvm package ships both the layer sources and `JvmPlugin` in one
unit today. The backend is identified by the package that ships it, and package identity is
already the URL — no platform-name namespace exists to govern.

- **Per-configuration dependency scoping is load-bearing, not organizational.** Flat-listing
  `eliot-jvm` and `eliot-avr` together would mount both layers into one resolution and the merge
  would correctly explode with "has multiple implementations" on every stdlib name. The
  configuration is the unit of resolution (and of the lockfile). Multi-platform = multiple
  configurations sharing the base, built independently.
- **No discovery, by design.** "Find the platform components" would need the registry we
  rejected. The user names their platform dependencies per configuration (the Cargo
  `[target.'cfg'.dependencies]` shape); the resolver follows declarations. What replaces search
  is a **diagnostic**: after resolving a configuration, every abstract name with no concrete
  provider is reported with attribution — "this configuration is missing a layer implementing
  `foo`'s 14 abstract values" — instead of a deep per-call-site failure.
- **Platform implementations are ordinary packages.** The author can keep one as a module of the
  library's own repo (co-versioned — this is why sub-module dependencies exist) or anyone else
  can ship one from an unrelated repo: layer redefinition has no orphan rule; the global
  at-most-one-implementation check keeps it coherent. Third-party layers stay viable across the
  base library's minor upgrades precisely because of branch compat-checking (above).
- **Test is a configuration** whose backend must be executable on the build host, plus the
  test-scope dependencies (framework + a runnable platform layer — jvm today). "The build system
  chooses" means: run the test configurations whose backend can execute here. It cannot conjure
  platform implementations nobody declared. On-target testing later = a platform plugin making an
  MCU configuration "executable" through the same delegated `test`/`run` verbs.

## Compiler plugins: Maven coordinates (transitional)

Plugins (backends, native contributors) are JVM binaries until the compiler is self-hosted, so a
plugin-shipping package's descriptor names its plugin as **exact-pinned Maven coordinates**. This
is a confined pragmatism, not a model change:

- **Identity/location split, again**: the package's identity stays the git URL; the coordinate is
  merely the *location of a binary* the descriptor at that tag points to. Ordinary users never
  write Maven coordinates; plugin loading flows transparently through the dependency graph.
- **Why not jars-in-git**: no dependency metadata — `JvmPlugin` needs ASM, and bare jars force
  fat/shaded jars, which this project already knows break plugin loading (`META-INF/services`
  collapse; `package.sh` exists to enforce per-module jars). Maven coordinates give the
  transitive closure via POMs, resolved by coursier into separate jars — the discipline already
  required. Building plugins from source collapses into the same thing plus an embedded Scala
  build.
- **Exact pins, not minimums** — plugin jars are toolchain components, outside MVS. The lockfile
  records coordinates + SHA-256 per jar. Maven Central's immutability plus hash-locking covers
  availability and trust; mirrors are resolver config, same shape as git mirrors.
- **v1 conflict policy**: resolve all plugins' coordinates together; surface version conflicts as
  errors. Per-plugin classloader isolation stays in the back pocket, not built speculatively.
- **Marked transitional**: when the compiler is self-hosted, plugins become Eliot source in
  ordinary git packages and this clause retires.

## The toolchain is a dependency

There is **no toolchain-version directive** (`eliot >= 0.x` was considered and dropped). Go and
Cargo carry one (`go 1.21`, `rust-version`) because their toolchain is *ambient* — installed
out-of-band, so the manifest can only document a constraint against something it doesn't
control. Here the toolchain is a package like any other: the eliot repo's modules ship the base
layers as sources and the compiler binaries as exact-pinned `plugin` coordinates. The dependency
line does everything the directive pretended to:

- `dep github.com/eliot-lang/eliot//stdlib v1.5` states the requirement; MVS unifies it across
  the graph; the lockfile pins the outcome — sources *and* compiler jars, since the selected
  tag's descriptor pins its own plugin coordinates. Base sources and compiler binaries cannot
  skew.
- The failure the directive guarded ("package built for a newer compiler dies with parse
  errors") **self-heals** instead: a dependency requiring eliot v1.5 raises the minimum, the
  launcher fetches the v1.5 compiler jars, and the right compiler compiles it. There is no
  "installed compiler" to be too old — only a resolved one.

Bootstrap — something must parse the descriptor and run the resolver before any resolution
exists — is the mill-wrapper split:

- **A dumb committed wrapper pins the launcher**: a small script plus a one-line version pin in
  the repo (mill's `.mill-version` pattern). Not the lockfile — that is tool output,
  per-configuration, and does not exist before the first resolve.
- **Everything smart rides the dependency graph**: lang compiler, backends, base layers,
  exact-pinned via the lockfile. The launcher is only descriptor parser + resolver + artifact
  fetcher + classpath assembler + verb dispatch — boring by design, changing rarely.

Two consequences:

1. **The language falls under the branch-compat contract.** As an ordinary MVS'd dependency, a
   minor tag on the eliot repo's v1 branch must be backward compatible — language and
   base-library evolution within a major is non-breaking, machine-checked by the same
   `compat-check` as everyone else's; a breaking language change is a new major branch.
2. **Descriptor-format evolution** is the one residual the directive used to cover (go.mod's
   `go` line also gates format features): handled by unknown-clause-is-fatal-with-hint, and a
   repo adopting a new clause commits the wrapper/launcher version that understands it —
   self-consistent by construction.

## IDE integration: a project-model query, not BSP

BSP assumes the build server compiles and streams diagnostics — the opposite of the Eliot LSP,
which embeds the compiler in-process (live VFS overlay, unsaved-buffer diagnostics). BSP solves
cross-vendor interop we do not have. The shape is rust-analyzer/gopls: the build tool answers one
question — the **resolved project model** (runtime roots, compiler-overlay roots, dependency
checkout paths, configurations) — via a JSON query or, since tool and LSP share one Scala
codebase, a directly-linked resolver library. The only shared-state care point is a locked
download cache so CLI and LSP do not race. This retires the `eliot.paths` stopgap.

## Deferred / open problems

- **Major-version coexistence** in one program (resolver error today; Go-style identity split if
  ecosystem migrations ever demand it).
- **Availability endgame** — immutable proxy + checksum transparency log, only if the ecosystem
  outgrows lockfile-verified mirrors.
- **Plugin classpath isolation** — if the resolve-together policy bites.
- **On-target `run`/`test` mechanics** — the delegated-verb contract for MCU backends.
