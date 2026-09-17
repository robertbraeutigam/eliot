# Progress indication (`--progress`) — DESIGN, not built

Status: design draft, 2026-09-17. Nothing here is implemented. §2–§3 are measured constraints; §4 reconciles them with
the Claude Design exploration *"Compiler output design exploration"* (three directions — **1a ledger**, **1b phases**,
**1c quiet** — sharing one premise: output is **append-only**, every line final once printed). That premise is adopted;
what each direction assumed that this engine cannot supply is said where it matters.

## 1. What it is for

The compiler is where the work of a build happens: the build tool only starts compiler jobs, and the compiler decides
on demand what a job consists of — read, check, generate, package, and later run a shell command or flash an MCU.
`--progress` answers the two questions Maven's and Mill's output answers, for that environment:

- **how far along is this run**, and
- **roughly how long until it is done** — coarse is fine, an order of magnitude off is not.

It must stay readable (nothing that scrolls faster than a person reads), stay small (a dozen lines for a build, not a
screenful), and cost an ordinary build nothing: like `--statistics` and `--visualize-facts` it is a `diagnosticKey`, off by default, and out of
the cache identity.

## 2. What was measured

`eliot.build.Launcher` (91 files, the largest program at hand), JDK 21, through a temporary trace in
`IncrementalFactGenerator.regenerate` (reverted).

| run | wall | fact generations | in processors | cache load | cache graph build | cache persist |
|---|---|---|---|---|---|---|
| cold | 15.7 s | 27,181 | 8.9 s | — | ~0.1 s | **4.0 s** |
| one-line body change | 7.8 s | 788 | 2.1 s | 0.6 s | **1.6 s** | **2.1 s** |
| no change | 2.2 s | 478 (all world leaves) | 0.1 s | 0.6 s | 0.1 s | 0.6 s |

Six facts about this engine decide the design.

**F1 — There is no denominator in the run itself.** Computation is demand-driven and depth-first, and nothing runs in
parallel: a requester blocks on the fact it asked for. So *demanded − completed* is the depth of the request stack,
and the Ninja/Bazel ratio `completed / discovered` reads ~100 % from the first millisecond to the last. The pipeline is
also not a sequence of phases: the first completion of a cold build is a `FileStat`, the twelfth is a
`UnifiedModuleNames`, and tokenizing, resolving and checking interleave until the end, all nested under the one root
demand (`GenerateExecutableJar`). A Maven-style "phase 3 of 7" would be fiction.

**F2 — Given a denominator, facts are a good clock.** In the cold trace, after the first fifth of the wall time (few,
expensive facts — tokenizer and parser, under a cold JIT) the raw completion count tracks wall time almost linearly:
32.6 % of facts at 40 % of the time, 59 % at 60 %, 83 % at 80 %. Weighting each fact by its key type's average cost
removes most of the early skew.

**F3 — A third to a half of a run is not fact generation at all.** Cache persistence alone is 25 % of a cold build, and
graph build + persist are 47 % of a one-line incremental one. A percentage driven by facts only would sit at "100 %" for four
seconds. The run has to be modelled as **phases**, of which generating facts is one.

**F4 — The prior graph's change cone is not an estimate.** For the one-line change the static cone (prior facts
reverse-reachable from the changed leaf) holds 4,483 facts; 309 non-leaf facts actually regenerated, and only 110 of
those were in the cone at all — the other ~200 are *value-less* (`SemValue`-bearing) facts, which regenerate because a
regenerating parent needs their value, not because anything under them moved. Simulating a live cone (pruned as facts
recompute equal) gives 3 % → 100 % → 82 % within the first quarter of the run, when `ModuleValue`'s equality cutoff
lands. Counting the cone is exactly the order-of-magnitude error to avoid. Do not build the estimate on it.

**F5 — A live total, measured.** The obvious history-free design is `completed / requested`, with the total growing as
facts are requested — no profile, only the counting the engine already does. On the cold trace it shows 72.7 % at 5 %
of the wall time, 94.6 % at 10 %, 99.8 % at 30 %: requested minus completed is the request chain, at most 31 deep and
9 on average, against 27,158 facts. This is not the sequential engine's fault alone. A fact's dependencies are known
only once the inputs that name them are computed — the graph is *discovered by computing it* — so requests cannot run
far ahead of completions; parallel generation widens the gap from a chain to a frontier, hundreds at best. Estimating
the undiscovered part from the request graph was simulated too (expected subtree size per key type, learned within the
run from completed generations): it wanders between 69 % and 99 % for the whole run, because the remaining work hangs
under the **singletons at the top of the chain** — the root, `UsedNames`, `main`'s `WovenValue` — and a type with one
instance has no completed sample until the run is over.

**F6 — What a run does learn early is its source files.** 87 % of the files a build will ever read are read by 20 % of
the wall time and 98 % by 30 %; module values follow (80 % at 30 %), resolved values trail (69 % at 50 %), monomorphic
values come last (14 % at 50 %). A projection "files seen × facts per file" is therefore stable from about a fifth of
the way in — *if* the ratio comes from somewhere: it is 298 for `Launcher` and 22 for `HelloWorld`, so it is a property
of a project, not of the language.

## 3. The model

### 3.1 A run is a fixed list of phases, one of which is open-ended

| phase | source of the event | how far along *inside* it |
|---|---|---|
| `starting` | `Compiler.runWithConfiguration` entry (plugin discovery, fingerprint) | time only |
| `loading cache` | around `backend.load()` | time only |
| `working` | the fact engine (§3.2) | weighted facts |
| `saving cache` | around `buildCacheData` + `persist` | time only |
| `running` | `session.execute()` | none — the closing line is already printed (§4.6) |

The phase list is the engine's, not a plugin's, so it is the same whatever the pipeline turns out to be. `working` is
deliberately not called "compiling": with a flashing backend most of it may be an upload.

### 3.2 Inside `working`: weighted fact generations, denominated by history

Every fact generation is one unit of work, weighted by the **average self time of its key type** (the key's class
name). Both the weights and the expected number of generations per type come from a **history file**, because F1 says
they cannot come from the run:

- `<target>/.eliot-progress-<configFingerprint>`, beside the cache but **not** discarded with it — it is keyed by the
  configuration only, so the cold build that follows a compiler upgrade still has an estimate. A few kilobytes of text.
- It holds one **profile per run class** — `cold` (no prior cache), `changed` (a world leaf differed), `unchanged` —
  each an EWMA over past runs of that class: per key type `(generations, total self time)`, and the wall time of each
  time-only phase. The class is `cold` or `unchanged` at start and flips to `changed` at the first world leaf whose
  recompute differs (152 ms into the measured run); the percentage is recomputed against the new profile once, early, which is
  the only time it may move backwards.
- The file is written at the end of every `--progress` run, success or failure (a failed run updates weights but not
  expected counts).

Work fraction and ETA, in historical milliseconds `h` so the units are consistent:

```
doneH      = Σ_type  done[type] × avgH[type]   +  the running fact's elapsed, capped at its avgH
expectedH  = Σ_type  max(expected[type], done[type]) × avgH[type]
speed      = liveElapsedInWorking / doneH                 -- today's machine vs. history's, smoothed
eta        = (expectedH − doneH) × speed  +  Σ remaining time-only phases (from the profile, × speed)
fraction   = elapsed / (elapsed + eta)                    -- over the WHOLE run, so saving is in the percentage (F3)
```

Three properties fall out. A project that grew simply makes `done[type]` overtake `expected[type]`, so the percentage slows
instead of lying. A single long fact — a 200 ms class generation today, a 12 s firmware upload tomorrow — keeps the percentage
moving, because the fact being generated is interpolated **in time** against its type's average. And a plugin nobody
told the progress system about is estimated correctly anyway, since a key type needs no description to be weighed.

Expected accuracy: `cold` and `unchanged` repeat closely. `changed` spans a body edit to a signature edit in a core
module, which on the measured program is 7.8 s to ~15 s because the time-only phases dominate — within 2×, corrected
as the run proceeds. That is the honest bound and it meets the requirement; a smarter `changed` estimator is §7's
follow-up, behind a measurement.

### 3.3 The very first build has no ETA

With no history there is no denominator (F1) and no honest way to invent one: mounted source size is no guide, since
only imported modules are ever read. The first build shows counts, elapsed time and the current activity, with `[ --%]`
in the percentage column and no ETA. It happens once per configuration per checkout.

### 3.4 What the user is told is being done

Key types are internal (`UncurriedMonomorphicValue$Key`); what a user recognises is a **verb and a subject**:
*checking `eliot.build.resolve.Resolution`*, *packaging `Launcher.jar`*, *flashing `firmware.hex`*. Plugins supply
that mapping for the key types they own, as data:

```scala
/** How a plugin's facts read to a user. Partial: a key nobody describes is still counted and weighed. */
trait ProgressDescriber {
  def describe(key: CompilerFactKey[?]): Option[ProgressActivity]   // ProgressActivity(verb: String, subject: String)
}
```

contributed through `CompilerPlugin` (`def progressDescriber: Option[ProgressDescriber] = None`). Proposed verbs: lang
— `reading` (`FileStat`, `FileContent`, `PathScan`), `parsing` (tokens, AST, core), `resolving` (module values through
row elaboration), `checking` (both monomorphize tracks and their channel riders), `lowering` (used, uncurry); jvm —
`generating` (`GeneratedModule`), `packaging` (`GenerateExecutableJar`). The subject is a **module or a file, never a
value**: values change 2,500 times a second, modules a few times a second.

The activity shown is the innermost *described* key on the active request chain (`activeFactKeys` already carries it),
**sampled** by the renderer rather than pushed by the engine. Because the engine is sequential, the sample is a
statistical profile: the label shows where the time is going, which is what the user wants to know, and a label that
flickers is a label over cheap work that the sampler mostly misses.

### 3.5 A long-running fact may report its own progress (deferred until one exists)

Time interpolation (§3.2) already keeps the percentage moving through an upload. What it cannot show is *"43 % · 12 kB/s"*.
For that, `CompilationProcess` gains a no-op-by-default `reportProgress(done: Long, total: Long, detail: String)`,
surfaced on `CompilerIO`; the tracker attributes it to the generation that called it and a heartbeat line (§4.2) shows
it while that fact is innermost. It changes no fact and records no dependency. Build it with the first processor that
needs it, not before.

## 4. What is displayed

### 4.1 Append-only lines, never a repaint

The design exploration's premise is taken whole: **every line is final once printed**, so the output is the same bytes
in a terminal, a CI log and a pipe, minus colour. Progress comes from a column that advances and from lines that keep
arriving, not from redrawing. Beyond the reasons the exploration gives, it is the right fit for *this* compiler: it
runs child processes, will run shell commands and flashers, and ends `run` mode by handing the terminal to the user's
program. A repainted region needs one owner of the terminal — replaced `System.out`/`System.err`, a log4j appender
that follows them, a suspend protocol for every child with inherited stdio, terminal-width detection — and an
append-only stream needs none of it: anything else that writes simply lands between two lines. (An earlier draft of
this document proposed a two-line live region; it is withdrawn for exactly that reason.)

What cannot be taken from the mock-ups, by direction:

- **1a's `[n/total]` counter.** There is no total (F1) — "known after resolve" describes a static task graph, which
  this is not. The column stays and holds the estimator's **percentage** instead: `[ 54%]`, and `[ --%]` on a first
  build. **One line per task** goes too: there are 27,000 generations, and the user-sized units are not tasks either —
  "typecheck `sensor.els`" is never *finished* until the run is, because any later demand may instantiate another of
  its values. Lines are therefore **samples and milestones** of one run, not a ledger of completed tasks. What survives
  is the grammar: fixed columns, every line standing alone, slow steps visible, a heartbeat on a long one.
- **1b's phase headings.** Stages interleave from the first millisecond to the last (F1), so a heading with its
  indented details cannot be printed *as the stage ends*. Its content survives as the **closing block**, printed once
  the run is over: where the time went by verb, what was cached, the artefact's budget with the delta since last time.
- **1c is what a fast build looks like anyway.** A run that ends before its first progress line is due prints only the
  closing line, which is 1c's three lines without a mode switch.

### 4.2 The progress line

```
[ 54%] checking    eliot.build.resolve.Resolution                       8.4s   ~7s left
└pct─┘ └verb────┘  └subject, then detail──────────────────────┘   └elapsed┘ └eta────┘
```

Fixed columns, as in 1a, so the eye can run down any one of them. The percentage is §3.2's fraction over the whole run;
verb and subject are §3.4's activity, **sampled at the moment the line is written**; the right-hand columns are wall
time so far and the rounded ETA (1 s steps under 10 s, 5 s under a minute, 15 s above; `finishing` under one second;
absent on a first build).

A line is written when a trigger fires **and at least one second has passed since the last one** — that floor is the
whole of "must not scroll faster than it can be read":

1. the run's phase changed (`loading cache` → working → `saving cache`);
2. the percentage crossed a 10 % boundary;
3. a world leaf was found changed — the run is reclassified `unchanged → changed` (§3.2) and the line says which file,
   which is the exploration's *"why was this rebuilt"* answered with the one thing the engine knows for certain;
4. a single described fact took a second or more — it gets a line of its own with its duration, 1a's *"slow steps are
   visible"*;
5. **heartbeat**: five seconds with no line. It names the innermost described fact and how long it has been running,
   with its sub-progress if it reports any (§3.5). The interval stretches to 15 s after a minute and 60 s after ten, so
   a long upload is a few lines, not hundreds.

That bounds a run at roughly ten milestone lines plus its slow steps and heartbeats, whatever its size.

### 4.3 A whole run

Cold, with history (the header carries the estimate, because the most useful moment for an ETA is before anything has
happened):

```
$ eliot exe-jar -m eliot.build.Launcher
eliot 0.5 · jvm exe-jar · eliot.build.Launcher · full build, about 16s
[  6%] parsing     eliot/build/model/Descriptor.els                     1.0s  ~15s left
[ 11%] resolving   eliot.build.model.Descriptor                         2.1s  ~14s left
[ 20%] checking    eliot.build.git.Git                                  3.4s  ~12s left
[ 31%] checking    eliot.build.resolve.Resolution                       5.0s  ~11s left
   ⋮
[ 68%] generating  eliot.build.Launcher                                10.4s   ~5s left
[ 72%] saving cache                                                    11.1s   ~4s left
[ 90%] saving cache                                                    14.2s   ~1s left
time   parsing 1.5s · resolving 1.9s · checking 3.1s · generating 1.3s · cache 4.1s
ok     Launcher.jar · 412 KB (+88 B) · 27,181 facts, 0 cached · 15.7s
```

One line changed:

```
eliot 0.5 · jvm exe-jar · eliot.build.Launcher · about 2s if nothing changed
[  3%] changed     eliot/build/model/Version.els                        0.8s   ~7s left
[ 24%] generating  eliot.build.model.Version                            2.2s   ~5s left
[ 41%] saving cache                                                     3.9s   ~4s left
[ 80%] saving cache                                                     6.6s   ~1s left
time   generating 1.0s · parsing 0.3s · checking 0.1s · cache 4.3s
ok     Launcher.jar · 412 KB (+0 B) · 788 facts, 27,894 cached · 7.8s
```

Nothing changed — no progress line comes due, so this is 1c:

```
eliot 0.5 · jvm exe-jar · eliot.build.Launcher · about 2s if nothing changed
ok     Launcher.jar · up to date · 28,682 facts cached · 2.2s
```

First build ever, and a long single fact on a future target:

```
eliot 0.5 · jvm exe-jar · eliot.build.Launcher · first build, no estimate yet
[ --%] parsing     eliot/build/model/Descriptor.els      3,902 facts    2.1s

[ 64%] flashing    firmware.hex · 43% · 12.4 kB/s  … 10s               16.0s   ~9s left
```

Failure — the exploration's closing vocabulary, which happens to be true of this compiler (a failed build leaves no
artefact behind):

```
failed 3 errors · first at eliot/build/model/Version.els:56 · 4.1s · nothing written
```

### 4.4 The closing block

- **`time`** — one line, self time by verb from the tracker's per-key-type totals, plus `cache` for the time-only
  phases. It is 1b's per-phase timing and the exploration's *"what took the time"*, moved to the only place it can be
  true. It is also how a user learns that half their incremental build is cache work (§2), which today nothing tells
  them. Omitted when the run printed no progress line.
- **`ok` / `failed`** — always printed. `N facts, M cached` is the exploration's *"caching is stated, not hidden"*,
  read from the generator after the run (`regeneratedKeys`, accepted and carried-forward counts). The clock starts at
  JVM start (`ProcessHandle.current().info().startInstant()`), so the ~40 % of a warm build that precedes `main` is in
  the figure. `failed` repeats the error count and the first error's position so it survives the scroll; the
  diagnostics themselves print exactly as today, above it.
- **The budget** — *"the number an embedded developer came for"*. The target plugin supplies measures of what it
  produced (`CompilerPlugin.progressMeasures`: name, value, unit, optional limit): `jar 412 KB` today, `flash 2.14 KB /
  256 KB · ram 32 B · stack 184 B` on an MCU target. The progress system stores the last values in the history file
  and prints the **delta** — 1b's `Δ last`, for free. One or two measures ride the `ok` line; more become 1b's table
  above it.

### 4.5 Vocabulary, colour, degradation

From the exploration, unchanged: status words are fixed and lowercase — `ok` `warn` `error` `failed` `cached` — and
carry the meaning on their own; colour is decoration and no signal is glyph-only. ANSI roles follow the design
system's terminal kinds: bracket, times and separators *faint*; verb *muted*; subject default; `ok` signal green
(`#2EC27E` under `COLORTERM=truecolor`, else ANSI green); `changed` and `warn` copper/yellow; `error`/`failed` red.
`✓` `·` `…` degrade to `ok` `-` `...` when the locale is not UTF-8. Lines are cut to 100 columns; the subject is what
gives.

Not a terminal, `NO_COLOR`, or `TERM=dumb`: no colour, and each line is prefixed with an absolute timestamp
(`11:42:03 [ 54%] checking …`), as in the mock-up. One deviation: the mock-up drops heartbeats without a tty, and this
keeps them at 30 s — CI is where a silent ten-minute step gets a job killed. Terminal detection only chooses
decoration now, so getting it wrong is harmless (`System.console() != null`, plus `Console.isTerminal` on JDK ≥ 22).

### 4.6 Streams, and `run` mode

Progress lines go to **stderr**. The mock-up puts the build log on stdout so `2>errors.txt` isolates diagnostics; here
`run` mode makes stdout the *compiled program's*, and `eliot run -m Main | jq` has to keep working, which is the
reason Cargo made the same choice. It is D5 below because it is a convention worth deciding once for the whole CLI.
In `run` mode the closing block is printed before `session.execute()`, so the program's output follows a finished
build log. The existing `INFO Generated executable jar` log line is redundant with `ok` and should drop to debug.

### 4.7 Taken later, not now

`--log=json` (one object per line — the same `ProgressSnapshot`s and closing measures, and the natural way for the
build tool to aggregate several compiler jobs); watch mode's timestamped paragraphs (a resident `CompilationSession`
looping `compileOnce` already fits); *"what did it decide about my types"* — effect rows, instance counts, `kept 41 of
58 functions` — which are facts the compiler has but are a report, not progress; and `next:` suggestions.

## 5. Architecture

Everything lives in a new `eliotc/…/progress/` package and follows the `--statistics` precedent: an add-on wrapped
around the processor tree, created only when asked for, with the engine and the processors untouched.

| piece | role |
|---|---|
| `ProgressTracker` | the mutable core: per-key-type `LongAdder`s (generations, self nanos), current phase, the innermost active key, sub-progress. Written by the wrappers, read by the renderer. |
| `ProgressCompilerProcessor` | wraps the **tree root only** (the `wrapTree` position), so it runs once per generation, not once per (key, processor): start/end events, self time by subtracting fact waits exactly as `TimedCompilationProcess` does, the active chain from `activeFactKeys`. |
| `ProgressHistory` | reads/writes the profile file; pure EWMA merge. |
| `ProgressEstimator` | pure: `(profile, tracker snapshot, elapsed) ⇒ ProgressSnapshot(phase, fraction?, eta?, activity, counts)`. All of §3.2's arithmetic, unit-testable with no engine. |
| `ProgressLineWriter` | the one renderer: a fiber waking a few times a second, asking the estimator for a snapshot and applying §4.2's triggers and one-second floor; prints the header and the closing block. The compiler is single-threaded in effect, so it runs on an otherwise idle core. It owns nothing — no stream is replaced, no cursor is moved. |
| `ProgressDescriber`, `progressMeasures` | the two plugin-facing pieces: §3.4's verb/subject mapping, and §4.4's measures of the produced artefact. |

Phase events come from where `PhaseTimings` already brackets the same code (`CompilationSession.create`,
`compileOnce`, `persist`), so `compileOnce` takes an `Option[ProgressTracker]` beside the tracker and statistics it
already takes. `ProgressSnapshot` is writer-independent on purpose: `--log=json` is a second writer over the same
snapshots, and the LSP can map them to `$/progress` work-done notifications, neither touching the model.

**Cost gate.** `--statistics` inflates a build ~20 % because it wraps every processor. This wraps the tree once: two
clock reads and two adder updates per generation plus two clock reads per fact wait, ~10 ms on the 27k-generation
build by arithmetic. The gate for shipping is a measured **< 2 %** on that cold build, flag on vs. off; the flag off
must be byte-for-byte the current code path (`wrap` returns the processor unchanged).

## 6. Work list

1. `ProgressTracker` + `ProgressCompilerProcessor` + phase events; `--progress` prints only the header and the
   `time` / `ok` / `failed` lines of §4.4. Already useful, and the gate: the cost measurement above.
2. `ProgressHistory` + `ProgressEstimator`, tested as pure functions against recorded traces (cold, changed,
   unchanged, grown project, first build).
3. `ProgressLineWriter`: triggers, the one-second floor, heartbeats; tested by feeding it a recorded trace on a
   virtual clock and asserting the exact lines.
4. `ProgressDescriber` for `lang` and `jvm`; the `changed <file>` line.
5. `progressMeasures` for `jvm` (jar size), deltas from the history file; colour and the non-tty form (§4.5).
6. Deferred: `reportProgress` (§3.5) with the first long-running processor; `--log=json`; LSP `$/progress`.

## 7. Open decisions

- **D1 — what bare `eliotc` prints.** The exploration makes quiet (1c) the default and detail opt-in. This document
  only adds `--progress`; whether the `ok` / `failed` line becomes the compiler's ordinary output without the flag is a
  separate, smaller change — it needs no tracker, only the generator's counts.
- **D2 — a better `changed` estimate.** History is within ~2× here. If larger programs show worse, the candidate is a
  **cost-weighted** live cone *plus* the value-less dependencies of its members (F4 shows why the plain cone fails),
  which also needs world leaves validated up front so the cone is known early — an engine ordering change, made only
  on a measurement that history is not enough.
- **D3 — the build tool.** `eliot <package>` runs one compiler per `compiler` line and inherits stdio, so each job
  prints its own header and closing line, which reads naturally one after the other; the tool's own work (resolve, fetch, assets) precedes them
  unannounced. Whether it passes `--progress` by default on a terminal, and whether it adopts the same line
  grammar for its own phases, is that repository's decision; `--log=json` is what it would aggregate from.
- **D4 — a live error count.** Deliberately absent: diagnostics are only known to be *this program's* after the run
  (`currentErrors` filters by reachability), so a live count would show errors that then vanish. For the same reason
  1a's inline `warn` lines are not taken: a diagnostic is printed once, after the run, when it is known to be real.
- **D6 — fewer first builds, and something for the ones left.** §3.2 keys the profile by configuration, so every new
  `-m` is a first build, and keeps it under `<target>`, so every clean is one. Candidate: look a profile up in tiers —
  this configuration, then **this project** (any configuration: per-type costs and F6's facts-per-file ratio carry
  over), then a user-wide one, then a default shipped with the compiler — and below the first tier *project* the
  denominator from what the run has discovered (F6), printing the percentage as rough (`[~30%]`). F5 is why the
  fallback is a looser history and not a live total.
- **D5 — stderr or stdout** for the progress lines (§4.6). Proposed: stderr, because of `run` mode.

Two things the measurements showed that are not this feature's to fix: a one-line body change regenerates **all 44**
`GeneratedModule`s, and spends 3.7 s building and persisting a cache graph in which 788 facts moved.
