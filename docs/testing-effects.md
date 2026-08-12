# Testing Strategy: Substituting Effect Implementations

Status: **design note**. The core mechanism *works today* (`examples/src/EffectsFakeCarrier.els`, verified
end-to-end), and so does a test framework built on it (§3, L3). One limitation (L1) blocks it from reaching the
stdlib effects and one (L2) is an everyday papercut; each is analysed below with the exact code that causes it and
a proposed fix. No compiler change is made by this note.

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

**Proposed fix**: widen `decidable` to defer whenever a contribution arrives through a slot whose declared type is
headed by a concrete constructor the definition does not name. Deferring to the post-mono channel is fail-safe,
since that channel still catches real leaks — it is already the authority on this exact question.

With L3 downgraded (below), this is the friction that actually remains on the everyday path.

### L3 — The fake run and the assertions must be separate definitions

**This does not block a test framework.** The framework already sketched in `docs/effect-row-tails.md` — a
`TestCase` whose body is a pinned `{Throw[AssertionError] | Id} Unit` — works as-is against a fake carrier,
because a pinned slot *is* a captured slot and nothing leaks across it. A complete mini framework (an
`AssertionError`, `assertEquals`, a `TestCase` with a pinned body, a `runCase` folding
`runId(runThrow(body(tc)))`) compiles and runs against the fake `Terminal`, printing `PASS greet` and, with the
expectation changed, `FAIL greet: expected 'Hello, Alice!;' but was 'Hello, Bob!;'`.

The one correction is **where the fake run may sit: outside the pinned body, not inside it.**

```eliot
-- Step 1: run the production code under the fake carrier, yielding a plain value.
def greetTranscript: Pair[Unit, Pair[String, String]] = runSession(greet)(Pair("Bob", ""))

-- Step 2: assert on that value, inside the framework's pinned body.
def greetTest: TestCase = TestCase("greet", assertEquals("Hello, Bob!;", second(second(greetTranscript))))
```

Writing the run *inside* the pinned body instead fails, and instructively:

```
Expected: {Throw[AssertionError] | Id} Session[Type]
Actual:   {Throw[AssertionError] | Id} Unit
```

Inside a pinned region the ambient carrier **is** the pinned stack, and the elaborator writes every
carrier-generic callee at the region's carrier — carriers are rigid and the checker never solves for one, which is
the whole point of the design. So `greet` there is written at `ThrowCarrier[AssertionError, Id]`, not at
`Session`, and `Session` gets pushed into the payload slot. A carrier-generic value can only be instantiated at a
foreign carrier in a region that has no ambient carrier of its own.

What that costs, and it is a real constraint rather than a defect: a test is **run-then-assert**, never
interleaved. You cannot assert part-way through a faked run, or make later fake input depend on an earlier
assertion.

The richer shape — pin the assertion effect *over the fake carrier*, `{Throw[AssertionError] | Session} Unit`, so
a test body is a direct-style script mixing effects and assertions — fails twice, and both failures are worth
recording:

1. `This value performs the effects 'Terminal', 'Transcript' but does not declare them.` Only a pinned row's
   *entries* are subtracted, and a fake's abilities have no canonical `<Ability>Carrier`, so they cannot be
   entries — the tail names the base carrier, not the abilities riding it.
2. `No ability implementation found for ability 'Transcript' with type arguments [{Throw[AssertionError] | Session}].`
   The ability must be implemented for the **whole stack**, not the base. Production `Console` reaches through
   `ThrowCarrier`/`StateCarrier` for free because it rides `Suspend` and every transformer carries a `Suspend`
   lift; a user-defined fake ability rides nothing, so it needs its own lift instance per layer.

**Proposed fix, no longer on the critical path — a user-declarable capture tag.** Let a parameter declare that it
*hosts* a computation on the carrier its type names, rather than receiving it as data. This is the same tag the
platform already contributes for `runMain`'s `io: IO[A]` (carrier-recognition source (ii)), made declarable
instead of plugin-only; the `RunBoundaryFunctions` doc is right that *shape detection* cannot decide this soundly,
but a *declaration* can, which is exactly why the platform declares it. The cheapest surface reuses pinned-row
syntax with no ability entries, so the existing tag pipeline (`EffectRow.pinnedParameterIndices` → elaborator
capture → `RowChecker.pinnedEntries`) applies unchanged:

```eliot
def check(name: String, program: {| Session} Unit): TestResult
```

It would buy a runner that takes a program on a bespoke carrier directly, and it would subsume L2. Neither is
needed to ship a test framework, so this is a later convenience, not a prerequisite.

## 4. Recommendation

1. **Adopt the carrier-substitution strategy as the testing story.** It requires nothing of production code, it is
   the direct consequence of effects being rows over an inferable carrier, and it is already demonstrably working.
2. **Fix L1** (constraint-aware declination). Small, local to `AbilityImplementationProcessor`, and it is what
   extends the strategy from application-owned abilities to `Console`/`Log`/`FileSystem`/`Process`/`Environment` —
   i.e. to the effects anyone actually wants to fake. It is the only blocker.
3. **Fix L2** (widen `decidable` to defer). A papercut, but it sits on the everyday path: it decides whether a
   harness may return the `data TestResult` a test author would naturally write.
4. **L3 needs nothing.** A test framework can be built today on pinned `{… | Id}` bodies; the cost is that a test
   is run-then-assert rather than a direct-style script. Revisit the declarable capture tag only if that shape
   turns out to chafe.
5. Keep **`Dep` + `provide`** documented as the seam-in-the-signature alternative, and **layer swapping** as the
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
| L3 does *not* block a framework | a mini framework (`TestCase` with a pinned `{Throw[AssertionError] \| Id} Unit` body, `assertEquals`, `runCase`) run against the fake `Terminal`: `PASS greet`, and `FAIL greet: expected 'Hello, Alice!;' but was 'Hello, Bob!;'` with the expectation changed |
| L3's actual constraint | the run inside the pinned body ⤳ `Expected: {Throw[AssertionError] \| Id} Session[Type]` / `Actual: {Throw[AssertionError] \| Id} Unit`; moved to its own definition, it compiles |
| pinning over the fake carrier does not help | `{Throw[AssertionError] \| Session} Unit` ⤳ "performs the effects 'Terminal', 'Transcript' but does not declare them" *and* "No ability implementation found for ability 'Transcript' with type arguments [{Throw[AssertionError] \| Session}]" |
