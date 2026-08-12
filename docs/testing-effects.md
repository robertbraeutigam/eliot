# Testing Strategy: Substituting Effect Implementations

Status: **design note**. The core mechanism *works today* (`examples/src/EffectsFakeCarrier.els`, verified
end-to-end). Three limitations block it from reaching the stdlib effects and from being packaged as a test
framework; each is analysed below with the exact code that causes it and a proposed fix. No compiler change is
made by this note.

## 1. The question

> How do we provide a different implementation for effects in testing, without having to write the production
> code differently?

The instinctive answer — `where` clauses — is the wrong tool, and the reason it is wrong is the reason the right
answer is already sitting in the design.

## 2. The answer: the carrier *is* the injection point

Production code that declares an effect row is already polymorphic in its carrier. This

```eliot
def greet: {Terminal} Unit = {
   val name = read
   write("Hello, " ++ name ++ "!")
}
```

desugars to a definition over a minted carrier binder `F` constrained by `Terminal[F]` (the *Effects Are a Channel*
cornerstone, `EffectSugarDesugarer` minting generic 0). It names no carrier, so it commits to no interpretation.
Which interpretation it gets is decided by *what `F` is instantiated to*, and that is decided by whoever runs it —
in production, the synthesized entry point instantiating `main` to the platform's `IO`; in a test, the test.

So the substitution mechanism is: **the test declares its own carrier and its own instance of the ability for that
carrier.** Production code is untouched, because there was never anything in it to touch.

```eliot
// The test double: an ordinary `data` with an `Effect` instance. Threads Pair(pendingInput, writtenOutput).
data Session[A](runSession: Function[Pair[String, String], Pair[A, Pair[String, String]]])

implement Effect[Session] { ... }

implement Terminal[Session] {
   def write(line: String): Session[Unit] = Session(w -> Pair(unit, Pair(first(w), second(w) ++ line ++ ";")))
   def read: Session[String]              = Session(w -> Pair(first(w), w))
}

// Instantiating `greet` at `Session` and running it yields a plain value to assert on.
def greetSession: Pair[Unit, Pair[String, String]] = runSession(greet)(Pair("Bob", ""))
```

`greetSession` evaluates to `Pair(unit, Pair("Bob", "Hello, Bob!;"))`. No I/O happened, and no line of `greet`
changed. This runs today; `examples/src/EffectsFakeCarrier.els` is the whole program.

Three properties fall out of the existing design rather than being added for testing:

- **The fake cannot cheat.** `Session` has no `Suspend` instance, and `Suspend` is the only route to a native side
  effect. A pure test carrier is *structurally* incapable of performing real I/O, which is the same guard the
  deliberate absence of `Suspend[Id]` already provides.
- **The fake is confined to the test program.** Compilation is whole-program monomorphization from `main`, so a
  test binary is a *different program*. Its instances are not in production's search space at all, and coherence is
  a per-program question.
- **The orphan rule is satisfied for free.** An instance must live in the ability's module or in a module of one of
  its type arguments; the test module declares the test carrier, so `Terminal[Session]` is legally colocated.

### 2.1 Why not `where`

An instance-level `where` guard can only make a candidate *decline*. To stop the library's instance from applying
to a test carrier, the guard would have to be written on **the library's instance**, in the library — which is
changing production code, and worse, it needs a predicate no guard can express ("`F` has no `Suspend` instance",
or "some other `Console` instance exists for `F`"). Guards see type arguments, not the instance environment.

`where` keeps its real job: disambiguating instances whose *patterns* genuinely overlap, as `Dep`'s
`where X1 != X2` separates the reader's native case from its cross-lift. That is orthogonal to effect
substitution.

### 2.2 What is available today without any compiler change

- **A user-defined ability** can be faked exactly as above. This is the full recommended strategy for
  application-owned effects, and it costs nothing.
- **`Dep[X]`** is the supported route when you are willing to state the seam in the signature: declare
  `{Dep[Clock]}` and hand a fake at the discharge site with `provide`. It works now, and it composes. Its cost is
  that it *does* change production signatures, which is precisely what the question asked to avoid — so it is the
  fallback, not the strategy.
- **Swapping the platform layer.** Layers are redefinition and the layer set is chosen on the command line
  (repeatable `--path`), so a test build can drop `jvm/eliot` and put a test layer in its place, redefining `IO`
  and every instance over it. This substitutes *everything at once* with zero production change, which suits
  whole-program integration tests and suits nothing finer. Note it is an alternative to, not a variant of, the
  above: a test layer added *beside* `jvm` would collide at the merge ("Has multiple implementations").

## 3. What blocks this from being the whole story

Three limitations, all established empirically against the current tree. None is a flaw in the strategy; each is a
mechanism that has not been extended to reach it.

### L1 — Catch-all library instances make every stdlib effect ambiguous

```
error: Multiple ability implementations found for ability 'Console' with type arguments [Recorded].
```

The jvm layer implements its real-world effects with a constrained catch-all:

```eliot
implement[F[_] ~ Suspend] Console[F] { ... }
```

Candidate selection is **purely structural**: `AbilityMatcher.matchImpl` unifies the impl's pattern against the
queried type arguments, and `AbilityImplementationProcessor.verifyImplementation` then consults only the `where`
guard. The `~ Suspend` constraint is *not* consulted. So `Console[F]` matches every carrier, a fake
`Console[MyCarrier]` is a second surviving candidate, and the query is `Resolution.Ambiguous`.

The constraint is not ignored forever — it fails later, as an ordinary unsatisfied demand — but ambiguity is
decided first, so the fake never gets there.

This affects every effect a program actually wants to fake: `Console`, `Log`, `FileSystem`, `Process`,
`Environment`.

**Proposed fix — constraint-aware declination.** A candidate whose type-parameter constraints have no
implementation at the matched bindings should return `Verdict.Decline` rather than `Verdict.Keep`. This adds no
surface and no new concept; it makes `~` mean at selection time what it already means at checking time. It also
makes the deliberate absence of `Suspend[Id]` do exactly the job its doc comment claims — excluding real-effect
instances from pure carriers — as a matter of resolution rather than of a later error message.

With it, `Console[Session]`: the jvm candidate needs `Suspend[Session]`, finds none, declines; the fake is the
unique survivor. In production nothing changes, because a real carrier does have `Suspend`.

Implementation risk to weigh: each constraint check is itself an `AbilityImplementation` query, so the demand
graph gains edges (`Console[C]` → `Suspend[C]` → …). Structural recursion down a carrier stack terminates, but the
cycle behaviour of the fact engine on a mutually-constrained pair needs checking before this is written.

### L2 — The pre-monomorphization row verifier false-positives on a nullary return

```
error: This value performs the effect 'Terminal' but does not declare it; add it to its { ... } effect set.
```

The *post*-monomorphization channel gets this right: `EffectAccountingProcessor` gates a contribution by the ride
test — an effect counts only if it is performed on the value's own ambient carrier — and a fake-carrier run is by
construction not on the harness's ambient. The *pre*-monomorphization verifier
(`RowElaborationProcessor.verifyRow`) works from declarations alone, cannot see that the callee was instantiated
at a foreign concrete carrier, and reports the leak whenever `RowChecker.RowResult.decidable` holds.

`decidable` is `declared.nonEmpty || !returnMayCarry`. So the identical harness is accepted or rejected purely on
the shape of its own return type:

```eliot
def transcript: Option[String] = Some(second(runRecorded(greeting)("")))   -- applied return: deferred, compiles
def transcript: String         = second(runRecorded(greeting)(""))        -- nullary return: rejected
```

This is why `greetSession` in the example returns `Pair[Unit, Pair[String, String]]`. That is an accident, not a
design, and a test harness naturally wants to return a `data TestResult(...)` — a nullary concrete type, which
would be rejected.

**Proposed fix**: subsumed by L3's tag. Once the run site is declared, the contribution is cleared at the source
and the return type stops mattering. Absent that, `decidable` would have to be widened to defer whenever a
contribution arrives through a slot whose declared type is headed by a concrete constructor the definition does
not name — deferring to the post-mono channel is fail-safe, since that channel still catches real leaks.

### L3 — A program cannot be passed to a runner, which is what blocks a test framework

```eliot
def transcriptOf(input: String, program: Session[Unit]): Option[String] = ...
def main: {Console} Unit = printLine(transcriptOf("Bob", greet) ...)
--                                                     ^ error: performs the effect 'Terminal' but does not declare it
```

An argument's effects are subtracted from the caller's derived row in exactly two cases
(`RowChecker.valueRow`): the slot is a **pinned row** (`pinnedEntries`), or the callee is a **registered run
boundary** and the slot is parameter 0 (`clearedWhen`). A user's test carrier can be neither:

- pinning requires the ability's canonical `<Ability>Carrier`, which forces *the* interpretation and so is the one
  thing a fake must not use — and `Suspend`-riding abilities like `Console` have no canonical carrier at all;
- `RunBoundaryFunctions` is a plugin-contributed `Configuration` key. Only a compiler plugin can add to it.

So the run must be inlined into the definition that yields the result, and a reusable
`def check(name: String, program: Session[Unit]): TestResult` — the core of any test framework — cannot be
written. This is the limitation that actually matters: the out-of-tree test framework sketched in
`docs/effects-as-rows.md` §A.11.13 gets its `TestCase` bodies as *pinned* rows precisely because this is missing.

**Proposed fix — a user-declarable capture tag.** Let a parameter declare that it *hosts* a computation on the
carrier its type names, rather than receiving it as data. This is not a new concept: it is the same tag the
platform already contributes for `runMain`'s `io: IO[A]` (carrier-recognition source (ii)), made declarable
instead of plugin-only. The `RunBoundaryFunctions` doc is right that shape detection cannot decide this soundly —
but a *declaration* can, which is exactly why the platform declares it.

The cheapest surface reuses the pinned-row syntax with no ability entries, so the whole existing tag pipeline
(`EffectRow.pinnedParameterIndices` → elaborator capture → `RowChecker.pinnedEntries`) applies unchanged:

```eliot
def check(name: String, program: {| Session} Unit): TestResult
```

`{| Session} A` denotes exactly `Session[A]` and differs only in carrying the capture tag. The syntax is a
bikeshed; the requirement is that the tag be *stated at the declaration* rather than inferred from the type.

This fixes L2 as a side effect, and it is what turns section 2 from a technique into a library.

## 4. Recommendation

1. **Adopt the carrier-substitution strategy as the testing story.** It requires nothing of production code, it is
   the direct consequence of effects being rows over an inferable carrier, and it is already demonstrably working.
2. **Fix L1** (constraint-aware declination). Small, local to `AbilityImplementationProcessor`, and it is what
   extends the strategy from application-owned abilities to `Console`/`Log`/`FileSystem`/`Process`/`Environment` —
   i.e. to the effects anyone actually wants to fake.
3. **Fix L3** (a declarable capture tag), which subsumes L2 and is the precondition for an in-tree or out-of-tree
   test framework.
4. Keep **`Dep` + `provide`** documented as the seam-in-the-signature alternative, and **layer swapping** as the
   whole-program integration-test route. Keep **`where`** out of it.

## 5. Evidence

Everything above was established against the tree at the time of writing:

| Claim | How |
| --- | --- |
| A fake carrier interprets a user ability end-to-end | `examples/src/EffectsFakeCarrier.els`, compiled and run; prints `Hello, Bob!;` |
| L1: a fake `Console` instance is ambiguous | a `data Recorded` carrier with `implement Console[Recorded]` ⤳ "Multiple ability implementations found for ability 'Console' with type arguments [Recorded]" |
| L1 scope | `implement[F[_] ~ Suspend] …` in jvm's `Console`, `Log`, `File`, `Process`, `Environment` |
| L1 cause | `AbilityMatcher.matchImpl` matches patterns only; `verifyImplementation` adds only the `where` guard; `paramConstraints` is read by no ability processor |
| L2 | the same program accepted with an `Option[String]` return and rejected with a `String` return |
| L3 | the same program accepted with the run inlined and rejected with the program passed as a `Session[Unit]` parameter |
