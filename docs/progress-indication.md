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

Seven facts about this engine decide the design.

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
graph build + persist are 47 % of a one-line incremental one. A display driven by facts only would sit at "100 %" for four
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

**F7 — "Facts delivered" is a usable count on a warm run too.** Count a fact once, the first time the run *has* it —
generated, accepted from the cache, or proven unchanged by the drill. The drill visits every key of a cached subtree
individually (27,890 memoized checks in the one-line-change trace), so nothing has to be stored about how big a cached
tree is; and because the graph is a DAG with heavy sharing (the stdlib sits under nearly everything), stored per-tree
sizes would count shared facts many times over, where counting distinct keys cannot. Nor do cached facts all arrive in
one jump at the start: validation short-circuits at the first moved dependency, and the rest of a regenerating fact's
inputs are validated when it asks for them. Over the working phase of that trace the count reads 2 % at 20 % of the
time, 24 % at 30 %, 32 % at 40 %, 84 % at 50 %, 97 % at 80 % — lumpy, monotone, never early, and it ends on 27,944 of
which 27,487 came from the cache. The cold trace is F2. What the count does *not* cover is F3: it is complete when
`saving cache` begins, which on that run is the half-way point.

## 3. The model

### 3.1 A run is a fixed list of phases, one of which is open-ended

| phase | source of the event | how far along *inside* it |
|---|---|---|
| `starting` | `Compiler.runWithConfiguration` entry (plugin discovery, fingerprint) | time only |
| `loading cache` | around `backend.load()` | time only |
| `working` | the fact engine (§3.2) | facts delivered |
| `saving cache` | around `buildCacheData` + `persist` | time only |
| `running` | `session.execute()` | none — the closing line is already printed (§4.6) |

The phase list is the engine's, not a plugin's, so it is the same whatever the pipeline turns out to be. `working` is
deliberately not called "compiling": with a flashing backend most of it may be an upload.

### 3.2 Inside `working`: facts delivered, out of the facts the last run delivered

The number shown is a **count of facts**, and it is the same kind of number on every run so that runs can be compared:

- **delivered** — distinct fact keys this run has, counted once each at whichever comes first: its generation ended,
  it was accepted from the cache, or the drill proved it unchanged (F7). A cold build delivers its 27,000 facts by
  generating them, a warm one mostly from the cache; both count to the same total.
- **total** — what the previous run of this configuration ended on, read from the **profile file**
  `<target>/.eliot-progress-<configFingerprint>`. It lives beside the cache but is **not** discarded with it, so the
  cold build after a compiler upgrade still has its total. A few kilobytes of text, written at the end of every
  `--progress` run (a failed run does not update the total). When a program has grown, `delivered` overtakes `total`
  and the total simply follows it — `[28,901/28,901]` and still working is honest, 103 % is not.

A big denominator is what makes this steady: one fact is 0.004 % of it. It needs no run classes and no
reclassification, because cold, changed and unchanged runs all count towards the same figure, and it is 1a's
`[n/total]` column after all — with the total coming from the last run instead of from a task graph.

It costs one hook in the engine, which is the one departure from `--statistics`' "engine untouched": acceptance and
validation never reach a processor, so a wrapper around the processor tree cannot see them. `IncrementalFactGenerator`
takes an optional tracker and tells it three things — generation started (key, for §3.5's activity), generation ended,
fact delivered from cache — and with no tracker the code path is today's.

### 3.3 The very first build: the count alone

With no profile there is no total and no honest way to invent one (F1, F5). The first build shows the count of facts
delivered, the elapsed time and the current activity — regular feedback that the run is alive and moving, which is
most of what a progress display is for — and ends by writing the total the next run will use.

### 3.4 Time, on top of the count (second stage)

A count is not a clock: cached facts cost nothing, generated ones cost milliseconds, and the time-only phases deliver
no facts at all (F3, F7). So the ETA column is fed by a separate, time-based estimate, added once the count is in
place. It needs per-key-type **self time**, which a wrapper around the processor tree's root measures the way
`TimedCompilationProcess` does (but see §3.7 on overlapping waits), and it extends the profile file with one **profile per run class** — `cold` (no prior
cache), `changed` (a world leaf differed), `unchanged` — each an EWMA over past runs of that class: per key type
`(generations, total self time)`, and the wall time of each time-only phase. The class is `cold` or `unchanged` at
start and flips to `changed` at the first world leaf whose recompute differs (152 ms into the measured run).

```
doneH      = Σ_type  generated[type] × avgH[type]   +  each running leaf fact's elapsed, capped at its avgH
expectedH  = Σ_type  max(expected[type], generated[type]) × avgH[type]
speed      = liveElapsedInWorking / doneH               -- today's machine vs. history's, smoothed
eta        = (expectedH − doneH) × speed  +  Σ remaining time-only phases (from the profile, × speed)
```

A single long fact — a 200 ms class generation today, a 12 s firmware upload tomorrow — keeps the ETA falling, because
the fact being generated is interpolated **in time** against its type's average; and a plugin nobody told the progress
system about is estimated correctly anyway, since a key type needs no description to be weighed. Expected accuracy:
`cold` and `unchanged` repeat closely; `changed` spans a body edit to a signature edit in a core module, which on the
measured program is 7.8 s to ~15 s because the time-only phases dominate — within 2×, corrected as the run proceeds.
The same self times give the closing `time` line (§4.4). Whether a percentage is printed as well, and whether it is
the count's or time's, waits until both can be looked at side by side.

### 3.5 What the user is told is being done

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

The tracker keeps the **set of generations in flight** (key, parent, start time — the hook's started/ended events are
all it takes), and the activity is **sampled** from it by the line writer rather than pushed by the engine: the
longest-running in-flight generation that has no in-flight child, walked up its parents to the first described key.
The sample is a statistical profile: the label shows where the time is going, which is what the user wants to know,
and a label that flickers is a label over cheap work that the sampler mostly misses. Today the set is one chain; under
parallel generation it is a tree with several leaves, and the same rule picks the one most worth naming (§3.7).

### 3.6 A long-running fact may report its own progress (deferred until one exists)

Time interpolation (§3.4) already keeps the ETA falling through an upload. What it cannot show is *"43 % · 12 kB/s"*.
For that, `CompilationProcess` gains a no-op-by-default `reportProgress(done: Long, total: Long, detail: String)`,
surfaced on `CompilerIO`; the tracker attributes it to the generation that called it and a heartbeat line (§4.2) shows
it while that fact is the sampled one. It changes no fact and records no dependency. Build it with the first processor that
needs it, not before.

## 4. What is displayed

### 3.7 Under parallel generation

Generation is sequential today only because that is easier to debug; a processor may ask for a list of facts in
parallel at any time, so nothing here may rest on one-at-a-time — and the engine it hooks into is already written for
concurrency (a `Deferred` per key, a fiber per generation).

- **The count and the total do not change at all.** A fact is delivered once whatever the interleaving — the engine's
  per-key `Deferred` and memoized unchanged-check guarantee it — and the tracker is a concurrent set and two adders.
  F5 still holds: parallel requests widen *requested − completed* from a chain to a frontier, not to the program.
- **Activity** is sampled from the in-flight set (§3.5), which is why it is a set and not "the current key".
- **Self time must subtract the *union* of a generation's waits, not their sum.** `TimedCompilationProcess` adds up
  every `getFact` wait, which is right only while waits cannot overlap; ten facts requested in parallel would
  subtract ten overlapping waits and drive the self time negative. The progress wrapper counts outstanding waits and
  accumulates the time during which the count is non-zero. (`--statistics` has the same assumption and would need the
  same change on the day generation goes parallel.)
- **The ETA adapts by itself.** `speed` is live wall time over historical self time, so a run going three ways
  parallel simply measures a speed of about a third; the running-fact interpolation sums over in-flight leaves. What
  degrades is a run whose width changes a lot between its beginning and its end, which the smoothing lags behind.
- **The `time` line becomes CPU time** and can sum to more than the wall time on the `ok` line; it says so
  (`time (cpu)`) once that can happen.
- **Output is unaffected**: only the line writer's fiber prints, from atomically read state.

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

- **1a's `[n/total]` counter is kept, with a different total.** "Known after resolve" describes a static task graph,
  which this is not (F1); the total is the number of facts the previous run delivered (§3.2), and on a first build the
  column holds the running count alone. **One line per task** is what goes: there are 27,000 generations, and the user-sized units are not tasks either —
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
[14,211/27,181] checking    eliot.build.resolve.Resolution                8.4s   ~7s left
└delivered/total┘ └verb───┘ └subject, then detail─────────────────┘  └elapsed┘ └eta────┘
```

Fixed columns, as in 1a, so the eye can run down any one of them. The counter is §3.2's; verb and subject are §3.5's
activity, **sampled at the moment the line is written**; the right-hand columns are wall time so far and, from the
second stage on (§3.4), the rounded ETA (1 s steps under 10 s, 5 s under a minute, 15 s above; `finishing` under one
second; absent on a first build).

A line is written when a trigger fires **and at least one second has passed since the last one** — that floor is the
whole of "must not scroll faster than it can be read":

1. the run's phase changed (`loading cache` → working → `saving cache`);
2. the count crossed another tenth of the total (on a first build: doubled since the last line);
3. a world leaf was found changed — the line says which file,
   which is the exploration's *"why was this rebuilt"* answered with the one thing the engine knows for certain;
4. a single described fact took a second or more — it gets a line of its own with its duration, 1a's *"slow steps are
   visible"*;
5. **heartbeat**: five seconds with no line. It names the sampled activity (§3.5) and how long that fact has been running,
   with its sub-progress if it reports any (§3.6). The interval stretches to 15 s after a minute and 60 s after ten, so
   a long upload is a few lines, not hundreds.

That bounds a run at roughly ten milestone lines plus its slow steps and heartbeats, whatever its size.

### 4.3 A whole run

Cold, with a profile (the header carries the estimate, because the most useful moment for an ETA is before anything
has happened):

```
$ eliot exe-jar -m eliot.build.Launcher
eliot 0.5 · jvm exe-jar · eliot.build.Launcher · full build, about 16s
[   335/27,181] parsing     eliot/build/model/Descriptor.els              1.0s  ~15s left
[ 1,298/27,181] resolving   eliot.build.model.Descriptor                  2.1s  ~14s left
[ 4,279/27,181] checking    eliot.build.git.Git                           3.4s  ~12s left
[ 8,900/27,181] checking    eliot.build.resolve.Resolution                5.0s  ~11s left
   ⋮
[25,621/27,181] generating  eliot.build.Launcher                         10.4s   ~5s left
[27,181/27,181] saving cache                                             11.1s   ~4s left
[27,181/27,181] saving cache                                             14.2s   ~1s left
time   parsing 1.5s · resolving 1.9s · checking 3.1s · generating 1.3s · cache 4.1s
ok     Launcher.jar · 412 KB (+88 B) · 27,181 facts, 0 from cache · 15.7s
```

One line changed — the same total, reached mostly from the cache (F7's curve):

```
eliot 0.5 · jvm exe-jar · eliot.build.Launcher · about 2s if nothing changed
[   597/27,944] changed     eliot/build/model/Version.els                 0.8s   ~7s left
[ 8,950/27,944] generating  eliot.build.model.Version                     2.6s   ~5s left
[27,010/27,944] generating  eliot.build.Launcher                          3.7s   ~4s left
[27,944/27,944] saving cache                                              4.1s   ~4s left
[27,944/27,944] saving cache                                              6.6s   ~1s left
time   generating 1.0s · parsing 0.3s · checking 0.1s · cache 4.3s
ok     Launcher.jar · 412 KB (+0 B) · 27,944 facts, 27,487 from cache · 7.8s
```

Nothing changed — no progress line comes due, so this is 1c:

```
eliot 0.5 · jvm exe-jar · eliot.build.Launcher · about 2s if nothing changed
ok     Launcher.jar · up to date · 27,944 facts, all from cache · 2.2s
```

First build ever, and a long single fact on a future target:

```
eliot 0.5 · jvm exe-jar · eliot.build.Launcher · first build
[ 1,298 facts ] parsing     eliot/build/model/Descriptor.els              2.1s
[ 4,279 facts ] checking    eliot.build.git.Git                           3.4s

[   398/   412] flashing    firmware.hex · 43% · 12.4 kB/s  … 10s        16.0s   ~9s left
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
- **`ok` / `failed`** — always printed. `N facts, M from cache` is the exploration's *"caching is stated, not
  hidden"*, and they are §3.2's own two counters, so the closing line and the last progress line agree. The clock starts at
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
(`11:42:03 [14,211/27,181] checking …`), as in the mock-up. One deviation: the mock-up drops heartbeats without a tty, and this
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

Everything lives in a new `eliotc/…/progress/` package, created only when asked for. Processors are untouched; the
engine gains the one optional hook of §3.2, and the time stage (§3.4) is a `--statistics`-style add-on wrapped around
the processor tree.

| piece | role |
|---|---|
| `ProgressTracker` | the mutable core: the set of delivered keys with its two counters (delivered, from cache), current phase, the set of generations in flight, sub-progress; from the time stage on, per-key-type `LongAdder`s (generations, self nanos). Written by the engine hook and the wrapper, read by the line writer. |
| `ProgressCompilerProcessor` | time stage only: wraps the **tree root only** (the `wrapTree` position), so it runs once per generation, not once per (key, processor): self time by subtracting the union of the generation's fact waits (§3.7). |
| `ProgressProfile` | reads/writes the profile file: the total, then (time stage) the per-class EWMA profiles and the last artefact measures. |
| `ProgressEstimator` | pure: `(profile, tracker snapshot, elapsed) ⇒ ProgressSnapshot(phase, delivered, total?, eta?, activity)`. All of §3.4's arithmetic, unit-testable with no engine. |
| `ProgressLineWriter` | the one renderer: a fiber waking a few times a second, asking the estimator for a snapshot and applying §4.2's triggers and one-second floor; prints the header and the closing block. It wakes a few times a second and reads atomics, so it costs the build nothing measurable. It owns nothing — no stream is replaced, no cursor is moved. |
| `ProgressDescriber`, `progressMeasures` | the two plugin-facing pieces: §3.5's verb/subject mapping, and §4.4's measures of the produced artefact. |

Phase events come from where `PhaseTimings` already brackets the same code (`CompilationSession.create`,
`compileOnce`, `persist`), so `compileOnce` takes an `Option[ProgressTracker]` beside the tracker and statistics it
already takes. `ProgressSnapshot` is writer-independent on purpose: `--log=json` is a second writer over the same
snapshots, and the LSP can map them to `$/progress` work-done notifications, neither touching the model.

**Cost gate.** `--statistics` inflates a build ~20 % because it wraps every processor. The count is one concurrent-set
insert per fact; the time stage wraps the tree once — two clock reads and two adder updates per generation plus two
clock reads per fact wait, ~10 ms on the 27k-generation build by arithmetic. The gate for shipping is a measured **< 2 %** on that cold build, flag on vs. off; with the flag off
the hook is a `None` and `wrap` returns the processor unchanged.

## 6. Work list

Staged so that each step is something a user can already run.

1. **The count.** The engine hook, `ProgressTracker`, phase events, `ProgressLineWriter` with its triggers and
   one-second floor (tested on a virtual clock against a recorded trace, asserting the exact lines). `--progress`
   prints the header, first-build style lines (`[ 4,279 facts ]`, a generic verb) and the `ok` / `failed` line. Gate:
   the cost measurement above.
2. **The total.** `ProgressProfile`; lines become `[n/total]`.
3. **What is being done.** `ProgressDescriber` for `lang` and `jvm`; the `changed <file>` line.
4. **Time.** `ProgressCompilerProcessor`, the per-class profiles, `ProgressEstimator` tested as a pure function
   against recorded traces (cold, changed, unchanged, grown project); the ETA column, the header's estimate, the
   `time` line.
5. `progressMeasures` for `jvm` (jar size), deltas from the profile; colour and the non-tty form (§4.5).
6. Deferred: `reportProgress` (§3.6) with the first long-running processor; `--log=json`; LSP `$/progress`.

## 7. Open decisions

- **D1 — what bare `eliotc` prints.** The exploration makes quiet (1c) the default and detail opt-in. This document
  only adds `--progress`; whether the `ok` / `failed` line becomes the compiler's ordinary output without the flag is a
  separate, smaller change — it needs no tracker, only the generator's counts.
- **D2 — a better `changed` ETA.** History is within ~2× here. If larger programs show worse, the candidate is a
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
  total from what the run has discovered (F6), printed as rough (`[4,279/~27,000]`). Not needed for the first cut:
  a first build showing its count alone (§3.3) is accepted as the starting point. F5 is why the
  fallback is a looser history and not a live total.
- **D5 — stderr or stdout** for the progress lines (§4.6). Proposed: stderr, because of `run` mode.

Two things the measurements showed that are not this feature's to fix: a one-line body change regenerates **all 44**
`GeneratedModule`s, and spends 3.7 s building and persisting a cache graph in which 788 facts moved.
