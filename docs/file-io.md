# File I/O and Paths (`eliot.file`) — Design

Status: PROPOSAL (no implementation yet). Driven by the build system's needs (read `eliot.pkg`
per line/character, write the lockfile, scan source trees), but designed as the general-purpose
file API of the stdlib.

## 1. Requirements

From the build system, concretely:

- Address files and directories portably (join, parent, compare, render) — a **Path** algebra.
- Read a text file whole, per line, and per character (the descriptor parser).
- Write a text file whole (the lockfile).
- Scan a directory tree for source files (list, recurse, file-vs-directory).
- Typed, recoverable errors (a missing `eliot.pkg` is an ordinary condition, not a crash).

Generally: fit the effect system (an effect row visible at every filesystem touch), fit the
platform-independence cornerstone (no representation in the base layer; an ATtiny with a flash
filesystem is as legitimate a platform as the JVM), and fit totality (no user-visible iteration
protocol that needs recursion).

## 2. Survey of modern designs

Condensed from a survey of Rust, Go, Zig, OCaml (Eio), Unison, Koka, Effekt, Roc, Haskell,
Java NIO, Python and Swift (swift-system).

| Language | Path type | Read/write in types? | Streaming | Errors | Close guarantee |
|---|---|---|---|---|---|
| Rust | `Path`/`PathBuf` (OS-bytes backed) | No (`File` + `Read`/`Write` traits) | iterators (`lines()`) | `io::Result` | RAII drop (close errors swallowed) |
| Go | plain `string` | interface narrowing (`io.Reader`) | `bufio.Scanner` | `error` + `io.EOF` sentinel | manual `defer` |
| Zig | string slices | No (runtime mode flag) | explicit-buffer readers | error unions | manual `defer` |
| OCaml Eio | (dir **capability**, string) pair | Yes — `source`/`sink` capability types | `Buf_read.lines : Seq` | exceptions | bracket / `Switch` scopes |
| Unison | `FilePath` (wrapped Text) | No (`Handle` + `FileMode`) | `getLine` + EOF loop | `{IO, Exception}` over `Either Failure a` core | `bracket` |
| Koka | normalized `path` struct | No | mostly whole-file | `<fsys, exn>` row — access ≠ failure | handler-scoped |
| Effekt | `String` | No | **files as stream-effect handlers** | `Exception[IOError]` effect | handler `finalize` |
| Roc | opaque `Path` | `Reader` read-only by construction | `Task`-based `read_line!` | precise tag-union errors | platform-managed |
| Haskell | `FilePath = String` → `OsPath` | No (`IOMode` at runtime) | lazy IO (regret) → conduit | exceptions | `withFile`/`bracket` |
| Java NIO | `Path` via `FileSystem` provider | `Reader`/`Writer` split types | `Files.lines : Stream` | exceptions | try-with-resources |
| Python | `pathlib.Path` / `PurePath` | No | file object as line iterator | exceptions | `with` |
| Swift | `FilePath` (typed, platform storage) | No (runtime `AccessMode`) | low-level `read(into:)` | thrown `Errno` | `closeAfter { }` |

Lessons this design adopts:

1. **Separate the pure path algebra from I/O.** Every modern design converged on it (`std::path`
   vs `std::fs`, `PurePath` vs `Path`, `Path` vs `Files`); the ones that mixed them
   (`java.io.File`, whose `delete()` returned `false` with no reason) are the acknowledged
   regrets. → `eliot.file.Path` is pure; all I/O lives behind the effect.
2. **String paths are a decade-long tax.** Haskell's `FilePath = String` (non-UTF-8 filenames
   lose bytes) took ~10 years to fix; Rust/Swift/Haskell-`OsPath` all store OS-native
   representation and convert explicitly at the string boundary. → `Path` is an *abstract base
   type* whose representation is platform-owned — precisely Eliot's layer mechanism — with
   `path(s)`/`Show[Path]` as the explicit conversion boundary.
3. **One path type.** Rust's `Path`/`PathBuf` borrow split is right *for Rust* and a permanent
   ergonomics complaint; a value-semantic language should ship one type (Swift did). Also: bolt
   the good API on late and you keep two forever (Java `File` vs NIO). → one `Path`, now.
4. **A total `Either` core under an effectful convenience surface.** Unison's split —
   builtins return `Either Failure a`, the ergonomic layer raises via an ability — is the right
   layering, and Eliot *already owns it*: `Throw` is realised over `Either`, and
   `runThrow`/`catch` are the total core. So the ability raises directly and no `Either`
   tier is encoded in the API — `runThrow(readFile(p))` is the "try" form, derived for free.
   Koka independently confirms the two-axis row: *access* (`fsys`) and *failure* (`exn`) are
   separate effects → `{FileSystem, Throw[IoError]}`.
5. **EOF must be option-shaped, not error-shaped.** Go's `io.EOF` sentinel and Java's `-1` are
   perennial bug sources; iterator/option-typed termination isn't. → `readLine` yields
   `Option[String]`, `None` = end of file.
6. **Access modes: almost nobody types them, but the ones that do win.** Mainstream ships
   runtime open-flags (`IOMode`, `OpenOptions`) and pays with runtime errors; the typed designs
   get direction-safety either by *doubling the API* (`in_channel`/`out_channel`,
   `Reader`/`Writer`) or by *capability types decided at open* (Eio's a read-only open **is** a
   `source`). Eliot can have the typed guarantee without the doubled API — see §6.
7. **For a total language, iteration lives in the platform.** The viable shapes without user
   recursion: fold-natives, or Effekt-style "the file is a handler for a stream effect".
   → fold-natives now (the `List.foldLeft` precedent); handler-streams noted as future work.
8. **Bracket beats drop.** Rust's close-on-drop silently swallows close errors; effect languages
   naturally bracket (Effekt closes in handler `finalize`). → `withReadFile`/`withWriteFile`
   as the sanctioned handle API; explicit `close` surfaces errors.
9. **Capability scoping (Eio: a path = (directory capability, relative string); WASI preopens)**
   is the one design here that materially improves security posture. Eliot's effect row already
   gives the coarse grain ("touches the filesystem at all"); per-directory capabilities are
   future work (§10) and slot in without breaking this API.

## 3. Shape of the proposal

- New **import-required** package `eliot.file` (deliberately *not* ambient like
  `eliot.effect` — most programs, and most microcontroller targets, never touch a filesystem).
  Two modules: `eliot.file.Path` (pure algebra) and `eliot.file.File` (everything effectful:
  modes, handles, `IoError`, the `FileSystem` ability, the public surface).
- One effect ability **`FileSystem`**, riding `Suspend` exactly like `Console`. (The ability is
  named `FileSystem`, not `File`, because it also covers directories and metadata — and the name
  `File` is taken by the handle type. A row reads `{FileSystem, Throw[IoError]}`.)
- The ability's carrier parameter is bound **`F[_] ~ Throw[IoError]`**: every operation raises
  on the same carrier, directly — no error-encoding tier in the API. Recovery is the ordinary
  `catch`, and `Either`-style results are *derived* (`runThrow(readFile(p))`), not declared.
  No new error machinery — file failures are just `Throw[IoError]`.
- Handles are **`File[M]`** with phantom access-mode markers `Read` / `Write` / `ReadWrite`,
  enforced by compile-evaluated `where` guards (§6). Whole-file and fold ops never expose a
  handle at all and are the primary API.
- A target without a filesystem simply ships no `FileSystem` instance: any `{FileSystem}`
  program fails ability resolution at monomorphization for that target. No capability
  configuration needed — the layer system is the capability.
- The **compiler platform gets no `eliot-compiler/` overlay** for `eliot.file`: compile-time
  code cannot read files (hermetic checking). A `FileSystem` op reached at check time stalls at
  the native leaf — loud, fail-safe.

## 4. `eliot.file.Path` — the pure algebra

```eliot
/**
 * A filesystem path. The representation is platform-owned (which also solves the non-UTF-8
 * filename problem: a platform stores OS-native bytes and only converts at the String boundary).
 */
type Path

/** Parse a string in the platform's path syntax. Total: any string is a path (Rust's stance);
  * lexically normalized on construction (Swift's stance: separators collapsed, no `..` resolution). */
def path(s: String): Path

/** Join: `root / "src" / "Main.els"`. Segment must be a single relative component. */
infix left
def /(base: Path, segment: String): Path

def parent(p: Path): Option[Path]

def fileName(p: Path): Option[String]

def extension(p: Path): Option[String]

def isAbsolute(p: Path): Bool

implement Eq[Path] {
   def equals(a: Path, b: Path): Bool
}

/** Render in platform syntax — the inverse boundary of `path`. */
implement Show[Path] {
   def show(value: Path): String
}
```

All abstract (body-less) in the base layer; the jvm layer supplies natives backed by
`java.nio.file.Path` (same pattern as `List` over `java.util.List`).

Notes:

- **`/` operator**: reads exceptionally well (`root / "eliot.pkg"`), and `eliot.file` being
  import-required keeps it out of ambient scope. If `/` is later wanted for integer division the
  two are distinguishable by module of origin; if that ever gets awkward, the fallback name is
  `child`. Precedence: its own level, tighter than comparison operators.
- **Relative-vs-absolute is *not* in the type** (v1). No mainstream library types it; the only
  shipped fully-typed design (Haskell's `path` package) stayed niche because real paths resist
  lexical classification. If it ever pays for itself, the Eliot-native home is the **refinement
  channel** (a `Path {absolute: Bool}` meta with `where`-preconditions), not a type parameter —
  same trajectory as `Int {range}`. Deliberately deferred.

## 5. Errors: `IoError` + `Throw`

```eliot
/** Why a filesystem operation failed. Representation platform-owned. */
type IoError

def message(e: IoError): String

implement Show[IoError] {
   def show(value: IoError): String = message(value)     // platform-independent body: allowed in base
}
```

jvm: `data IoError(message: String)` — the abstract `message` merges with the generated
data-field accessor, the established `ThrowCarrier.runThrow` pattern. v1 carries a message only;
a structured `kind` (NotFound / PermissionDenied / …, Roc's precise tag unions being the model)
is additive later — worth doing before the build system relies on distinguishing "no
`eliot.pkg` here" from genuine I/O failure, or that distinction gets an `exists` pre-check
instead.

One small stdlib addition to `eliot.effect.Throw`, the general `Either`→`Throw` bridge —
platform instances use it to reflect native results into the effect (§9), and it is generally
useful:

```eliot
/** Reflect an `Either` into the `Throw` effect: yield the `Right`, `raise` the `Left`. */
def orRaise[E, A](e: Either[E, A]): {Throw[E]} A = foldEither(err -> raise(err), a -> a, e)
```

## 6. Access modes on the type level

The question posed: `File[Read | Write | All]` as types, or meta-information? Both are viable in
Eliot; the recommendation is **phantom mode types + `where` guards**, with the refinement
channel deliberately *not* used for modes. The reasoning, then the design:

- An access mode is a **capability of the handle** — part of the API contract you want *visible
  in signatures* (`log: File[Write]` in a `data` field documents and enforces intent) and
  *storable in data structures*. Ordinary types already do both perfectly.
- The refinement channel today is `Int`-shaped, and **channel meta does not yet flow through
  container/data fields** (the open container-propagation problem that also blocks `List` size
  tracking). A `File {mode}` meta would silently stop protecting the moment a handle is stored
  in a record — exactly the kind of gap that must not exist. Phantom parameters ride the
  ordinary type system through every position, today, with zero compiler work.
- The channel's real advantage — branch *joins* (`if(c, openRead(p), openReadWrite(p))` joining
  to `Read`) — is marginal for files and recoverable by explicit narrowing (`asRead`, below).
- What genuinely belongs in the channel later: *value-shaped* path/file refinements
  (absolute/normalized, tracked file size), per §4/§10. Mode is capability-shaped, not
  value-shaped. This is the general split this design proposes: **capabilities → types,
  quantities → channel**.

The design — types are values, so modes are just three abstract markers compared with the
type-level `Eq` that already powers `where E1 != E2`:

```eliot
/** Access-mode markers. Phantom: never constructed, erased at monomorphization. */
type Read
type Write
type ReadWrite

/** Compile-evaluated mode predicates (`Eq[Type]` structural equality, use-site verified). */
def canRead[M]: Bool  = M == Read  || M == ReadWrite
def canWrite[M]: Bool = M == Write || M == ReadWrite

/** An open file handle with access mode `M`. Representation platform-owned. */
type File[M]

/** Explicit narrowing, for storing a `ReadWrite` handle where a narrower mode is expected. */
def asRead[M](f: File[M]): File[Read] where canRead[M]
def asWrite[M](f: File[M]): File[Write] where canWrite[M]
```

Operations then guard instead of doubling the API (the `in_channel`/`out_channel` tax) or
wrapping (no coercions inserted, no runtime cost):

```eliot
def readLine[M](file: File[M]): F[Option[String]] where canRead[M]      // ability method, see §7
```

A `File[ReadWrite]` passes both `readLine` and `writeText` directly; a `File[Write]` at
`readLine` is a *compile* error at the concrete use site — the `where`-precondition mechanism
(`docs/bounds-as-refinements.md` §4.3, `examples/src/WherePrecondition.els`) doing for modes
exactly what it does for integer ranges. This is the use-site-verification cornerstone earning
its keep: no mode lattice, no subtyping, no `Coerce` instances — three markers and two `Bool`
functions.

Alternatives considered and rejected:

- **Mode in the effect row** (`{FileRead}` / `{FileWrite}` abilities): rows say what a
  *function* may do, not what a *handle* permits — a function holding a `File[Write]` and a
  `File[Read]` needs both abilities anyway, and the per-handle guarantee is lost. The row/handle
  split (coarse permission in the row, per-file capability in the value) is the Eio-validated
  synthesis.
- **`Coerce[File[ReadWrite], File[Read]]`** (check-mode auto-narrowing): works, `Coerce` is an
  open extension point — but it inserts wrapper expressions to achieve what a guard checks for
  free, and explicit `asRead` covers the storage case. Can be added later purely additively if
  call-site ergonomics ever demand it.
- **Mode as channel meta**: see above.

Fallback note: if `where` on the guards or the type-level `==` over marker types hits an
unimplemented corner, the same signatures degrade to mode-specific ops (`readLine` on
`File[Read]` only + `asRead`) without changing the model — the guards are ergonomics, the
phantom parameter is the design.

## 7. The `FileSystem` effect

The ability *itself* demands failure capacity of its carrier — its parameter is bound
`F[_] ~ Throw[IoError]` — so every operation raises on the same carrier and the API encodes no
error tier of its own. (Mechanically: an ability header's generic parameters are parsed by the
same binder component as any def's, `~`-constraints included, and are prepended to every
method — so calling any op demands `Throw[IoError]` on the ambient carrier through the
ordinary constraint path, exactly like `catch`'s `G[_] ~ Effect`. A row using the ability
therefore reads `{FileSystem, Throw[IoError]}`, and the bound makes that contract
platform-invariant: no instance can opt out of typed failure.)

The ability methods ARE the public API — the `Console` model. Subject-last parameter order
throughout, per the dot-chain rule.

```eliot
ability FileSystem[F[_] ~ Throw[IoError]] {
   // Whole-file (primary API — no handle, nothing to close, nothing to leak)
   def readFile(path: Path): F[String]
   def writeFile(content: String, path: Path): F[Unit]
   def appendFile(content: String, path: Path): F[Unit]

   // Streaming folds — the platform-owned loops (the `foldLeft` precedent: in a total
   // language every iteration is a native; a file is no different)
   def foldLines[B](initial: B, step: B => String => B, path: Path): F[B]
   def foldCodePoints[B](initial: B, step: B => Int => B, path: Path): F[B]

   // Metadata & directories
   def exists(path: Path): F[Bool]
   def isDirectory(path: Path): F[Bool]
   def listDirectory(path: Path): F[List[Path]]
   /** Every file under `path`, depth-first. A native by necessity: tree descent is
     * unbounded iteration, inexpressible in user Eliot — the build system's source scan. */
   def walk(path: Path): F[List[Path]]
   def createDirectories(path: Path): F[Unit]
   def delete(path: Path): F[Unit]

   // Handles (the low-level tier; prefer with*File / whole-file ops)
   def openRead(path: Path): F[File[Read]]
   def openWrite(path: Path): F[File[Write]]          // create-or-truncate
   def openAppend(path: Path): F[File[Write]]         // create-or-append
   def openReadWrite(path: Path): F[File[ReadWrite]]
   def readLine[M](file: File[M]): F[Option[String]] where canRead[M]       // None = EOF
   def readCodePoint[M](file: File[M]): F[Option[Int]] where canRead[M]     // None = EOF
   def writeText[M](text: String, file: File[M]): F[Unit] where canWrite[M]
   def close[M](file: File[M]): F[Unit]               // close errors deliberately not raised
}
```

Notes:

- **`Either`-style recovery needs no dedicated API** — it falls out of the effect system.
  `Throw` is *realised over* `Either`, so the total core Unison encodes as a separate builtin
  layer is, in Eliot, just discharge:

  ```eliot
  readFile(p) catch (e -> fallback)                 // recover in place
  runThrow(readFile(p))  : {FileSystem} Either[IoError, String]   // the "try" form, derived
  ```

  A build tool aggregating errors instead of failing fast writes `runThrow` at the granularity
  it wants. An earlier draft of this design carried a parallel `try*` method set returning
  `F[Either[IoError, A]]` (Unison's layering, transplanted literally); it was deleted as
  redundant — Eliot's discharge machinery *is* that layer.
- **Per-character = codepoints as range-refined `Int`.** No new `Char` type: a codepoint is
  `Int {Interval(0, 1114111)}` — the refinement channel used exactly as intended (and the
  platform-independence feedback applied: representation derived from the range, a
  microcontroller target packs it as it likes). The `Int`s above should carry that meta on the
  signatures once parameter-position metas are verified; until then the range is documented.
  Decoding (UTF-8/UTF-16 → codepoint) is the platform's job. Binary/byte reads are deferred
  until something needs them (§11) — v1 is a *text* API.
- **`Append` is an open option, not a mode** (the Rust `OpenOptions` lesson): `openAppend`
  yields a plain `File[Write]`.
- **EOF is `None`**, never an error (survey lesson 5).
- **Mode guards sit on the ability methods themselves.** `where` on ability methods is
  syntactically the ordinary def `where` (ability bodies reuse the full def parser) but is an
  unverified corner (§12); the fallback is mode-exact signatures (`writeText` on `File[Write]`
  only) + the explicit `asRead`/`asWrite` narrowing of §6 — same model, slightly noisier call
  sites.
- **Diagnostics caveat**: forgetting `Throw[IoError]` in a row fails soundly but cryptically
  today (the known `Throw`-leak UX at `AbilityResolver`), not with the friendly
  "performs the effect" message `Console`-class leaks get. Pre-existing condition, not new to
  this design; noted as an eventual diagnostics improvement.

The small derived surface (plain defs in the same module — everything else above is already
directly callable):

```eliot
def readLines(path: Path): {FileSystem, Throw[IoError]} List[String] =
   foldLines(empty, acc -> line -> acc.append(line), path)

/** Run `action` on every line, front to back, keeping only its effects (the `foreach` of files). */
def foreachLine(action: String => {Effect} Unit, path: Path): {FileSystem, Throw[IoError]} Unit

/** Open `path`, run `use`, close. The sanctioned handle API. */
def withReadFile[A](use: File[Read] => {Effect} A, path: Path): {FileSystem, Throw[IoError]} A
def withWriteFile[A](use: File[Write] => {Effect} A, path: Path): {FileSystem, Throw[IoError]} A
```

Failure and access are separate row entries (Koka's `<fsys, exn>` validated split), so `catch`
discharges `Throw[IoError]` while `{FileSystem}` keeps riding to `main` like `Console` does.
`foreachLine` and the `with*File` brackets have Eliot bodies built from `foldLines`/handles +
the `Effect` machinery (the `foreach`/`catch` precedents); their exact elaboration through the
effect lifter is an implementation detail to be settled with tests, not part of the design
surface.

## 8. Resource safety (v1 stance)

- The **primary API is handle-free**: `readFile`, `readLines`, `foldLines`, `writeFile` — the
  native opens and closes internally; nothing can leak. This mirrors where every surveyed
  ecosystem actually pushes users (`fs::read_to_string`, `os.ReadFile`, `Path.read_text`).
- Handle code goes through `withReadFile`/`withWriteFile` brackets. v1 guarantees close on
  normal completion; **close-on-`raise` (finalizers) is a known limitation** — a `use` body
  that raises leaks the handle until a general finalizer/`Resource` story exists (the natural
  shape: Effekt-style, the discharger runs the finalizer; needs design, tracked in §10).
  A leaked handle on jvm is eventually collected; on a microcontroller target this matters and
  is the forcing function for the real story.
- `close` deliberately does not raise: surfacing close errors (the Rust drop regret) matters
  for *write* flushing, and the honest v1 answer is that `writeFile`/`appendFile` flush-and-close
  internally and report failures like any other op; explicit-handle writers who care can later
  get a raising `closeFlushed` variant if a real need appears.
- **No linearity yet**: a handle can escape `withReadFile` in a closure or record. Accepted v1
  gap — loud at worst (use-after-close is a runtime `IoError`/`raise`, never silent
  corruption). The planned linearity/uniqueness work (`docs/in-place-mutation.md`) is the
  eventual closer; only experimental Haskell `linear-base` ships this anywhere today.

## 9. Platform layer (jvm) and testing

`jvm/eliot/eliot/file/Path.els` + `File.els`, following the `Console`/`List` patterns exactly:

- Re-declare `type Path`, `type File[M]`, the `FileSystem` ability (merge-verified copies);
  `data IoError(message: String)`.
- Concrete `Path` ops delegate to `private` body-less leaf natives backed by
  `java.nio.file.Path` (erased-generic natives where needed, the `java.util.List` mechanism).
- One instance, generic over the carrier — never pinned to `IO`. The leaf natives still speak
  `Either` (a native cannot `raise`); the instance reflects that into the effect immediately
  with `orRaise`, so the encoding never escapes the platform layer:

```eliot
implement[F[_] ~ Suspend & Throw[IoError] & Effect] FileSystem[F] {
   def readFile(path: Path): F[String] = suspend(_ -> readFileInternal(path)).flatMap(orRaise)
   ...
}

private def readFileInternal(path: Path): Either[IoError, String]      // implemented by the backend
```

- **Implementation question to settle first**: leaf natives that return `Either[IoError, A]` /
  `Option[A]` must construct Eliot data values from backend code. Options: (a) the backend
  emits calls to the generated `Left`/`Right`/`Some`/`None` constructors (jargen knows their
  classes); (b) natives return an opaque platform result interrogated by two further natives
  (`succeeded`/`value`/`error`). (a) is cleaner and likely already reachable; decide at
  implementation.
- `File[M]`'s jvm representation: one erased handle class (buffered reader/writer pair or
  `FileChannel`); `M` is erased like `List`'s element type.

**Testing win to cash in**: because `FileSystem` is an ordinary ability over any `Suspend`
carrier, a *pure in-memory instance* (a `State[Map[Path, String]]`-backed fake, the
`fstest.MapFS`/jimfs payoff) makes build-system logic testable with zero real I/O. Worth
shipping alongside v1 in the test layer.

## 10. Example (build-system flavored)

```eliot
import eliot.file.Path
import eliot.file.File

def descriptor(root: Path): {FileSystem, Throw[IoError]} List[String] =
   readLines(root / "eliot.pkg")

def writeLockfile(content: String, root: Path): {FileSystem, Throw[IoError]} Unit =
   content.writeFile(root / "eliot.lock")

def sources(root: Path): {FileSystem, Throw[IoError]} List[Path] =
   walk(root / "src")            // filter to ".els" once List grows filter (see below)

def main: {Console, FileSystem} Unit =
   report catch (e -> printLine("build failed: " ++ message(e)))

def report: {Console, FileSystem, Throw[IoError]} Unit =
   foreachLine(printLine, path("build.log"))
```

Adjacent stdlib gap this surfaces: `eliot.collection.List` needs the small pure derived helpers
(`filter`, `map`, `contains` — platform-independent bodies over `foldLeft`, base layer) before
the build system can do anything useful with `walk`'s result.

## 11. Deferred / future work

- **Error kinds** on `IoError` (NotFound / PermissionDenied / AlreadyExists / …) — additive;
  wanted by the build system fairly soon (see §5).
- **Binary I/O**: `foldBytes` / byte writes with `Int {Interval(0, 255)}` elements — same
  shape as codepoints, added when a consumer exists.
- **Finalizers / Resource** for close-on-raise (§8) — the general "bracket as effect
  discharge" design.
- **Directory capabilities** (Eio/WASI model): a `Dir` value as scoping capability,
  `openBeneath(dir, rel)`; slots under the same ability later. The effect row already covers
  the coarse grain today.
- **Handler-streams** (Effekt model): a file as a *discharger* of a line-stream effect
  (`{Lines} A` run by a file) — strictly more composable than folds; wants the general
  user-defined-handler story, not file-specific design.
- **Path refinements in the channel** (`absolute`/`normalized` meta + `where` preconditions on
  ops that need them) — the meta-information instinct, applied where it fits (quantities and
  value properties), while modes stay phantom types (capabilities). §6.
- **Seek/random access**, file times, permissions, symlinks, temp files, rename/copy — ordinary
  additive ability growth, none design-blocking.
- `CodePoint`/text utilities probably migrate to a future `eliot.text` alongside the library
  restructure.

## 12. Verification checklist for implementation

Things this design *assumes* and the implementation must confirm (with tests), roughly in risk
order:

1. The ability-header carrier bound `F[_] ~ Throw[IoError]`, end-to-end. Syntax is confirmed
   (`AbilityBlock` parses header generics with the same constrained-binder component as defs
   and prepends them to every method, so call sites demand the constraint through the ordinary
   `~` path); to verify: the synthetic ability marker, instance-conformance checking against a
   constrained header, and that an undeclared `Throw[IoError]` fails at every use (soundly,
   even if cryptically).
2. `where canRead[M]` guards **on ability methods**, with `M` generic and `==` over abstract
   marker types (`Eq[Type]` structural equality — the `where E1 != E2` mechanism). Fallback if
   this corner is unimplemented: mode-exact method signatures + `asRead`/`asWrite` (§6/§7).
3. Instance bodies through the effect machinery: `suspend(...).flatMap(orRaise)` — raising from
   within an implement body on the constrained carrier (the `catch` body is the precedent).
4. Backend construction of `Either`/`Option` results from leaf natives (§9).
5. `foreachLine`/`with*File` bodies through the lifter (build-the-action-chain, `foreach`
   precedent).
6. Erased-generic native for `File[M]` and the `Path` natives (the `List` mechanism, new leaf
   set).
