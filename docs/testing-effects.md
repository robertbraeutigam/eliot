# Testing Strategy: Substituting Effect Implementations

Status: **adopted, done**. The core mechanism works for application-owned abilities
(`examples/src/EffectsFakeCarrier.els`) *and*, since constraint-aware declination landed, for the stdlib effects
(`examples/src/EffectsFakeConsole.els` — a fake `Console`, both verified end-to-end), and so does a test framework
built on it: `examples/src/EffectsTestFramework.els` is one, in three definitions, returning the `data TestResult` a
test author would naturally write. Both limitations that were open are fixed (L1, L2, below, each with the code that
caused it); L3 was downgraded to "needs nothing" and is a later convenience only.

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
def greetTranscript: String = second(second(runSession(greet)(Pair("Bob", ""))))
```

`greetTranscript` evaluates to `"Hello, Bob!;"`. No I/O happened, and no line of `greet` changed. This runs today;
`examples/src/EffectsFakeCarrier.els` is the whole program.

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

- **Any ability** can be faked exactly as above — application-owned (`examples/src/EffectsFakeCarrier.els`) or a
  standard-library effect (`examples/src/EffectsFakeConsole.els`). This is the full recommended strategy, and it
  costs nothing. Reaching the stdlib effects is what L1 took; the section header below is kept as written because
  the analysis is what explains the fix.
- **`Dep[X]`** is the supported route when you are willing to state the seam in the signature: declare
  `{Dep[Clock]}` and hand a fake at the discharge site with `provide`. It works now, and it composes. Its cost is
  that it *does* change production signatures, which is precisely what the question asked to avoid — so it is the
  fallback, not the strategy.
- **Swapping the platform layer.** Layers are redefinition and the layer set is chosen on the command line
  (repeatable `--path`), so a test build can drop `jvm/eliot` and put a test layer in its place, redefining `IO`
  and every instance over it. This substitutes *everything at once* with zero production change, which suits
  whole-program integration tests and suits nothing finer. Note it is an alternative to, not a variant of, the
  above: a test layer added *beside* `jvm` would collide at the merge ("Has multiple implementations").

## 3. What stood in the way, and what each fix took

Three limitations, all established empirically against the tree at the time of writing. None is a flaw in the
strategy; each is a mechanism that had not been extended to reach it. **L1 and L2 are fixed** — what each took is
recorded below, since in both cases the second half was not visible until the first half was done.

### L1 — Catch-all library instances made every stdlib effect ambiguous — *fixed*

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

**Fix — constraint-aware declination** (`AbilityImplementationProcessor.constraintsSatisfied`). A candidate whose
type-parameter constraints have no implementation at the matched bindings returns `Verdict.Decline` rather than
`Verdict.Keep`. This adds no surface and no new concept; it makes `~` mean at selection time what it already means
at checking time. It also makes the deliberate absence of `Suspend[Id]` do exactly the job its doc comment claims —
excluding real-effect instances from pure carriers — as a matter of resolution rather than of a later error
message.

With it, `Console[Session]`: the jvm candidate needs `Suspend[Session]`, finds none, declines; the fake is the
unique survivor. In production nothing changes, because a real carrier does have `Suspend`.

Every step of the check is **fail-safe towards keeping**, so it can only remove a candidate that could not have
worked, never create a resolution failure the pattern match alone would not have had. It judges a constraint only
when its ability arguments are known exactly, and answers "satisfied" for an untraced match (the same operand
unreliability the `where` discharge refuses to judge over), a binding that is the defaulted universe, an argument
shape it cannot ground, an unreadable ability arity, and an absent probe. A constraint spells only the arguments
that are not the constrained binder (`F[_] ~ Suspend` is `Suspend[F]`, `G[_] ~ State[S]` is `State[S, G]`), so the
binder is appended exactly when the written arguments are one short of the ability's arity, and any other count is
not judged.

The implementation risk that had to be weighed — each constraint check is itself an `AbilityImplementation` query,
so the demand graph gains edges (`Console[C]` → `Suspend[C]` → …) — is handled where the engine already handles it:
structural recursion down a carrier stack terminates, and a probe that leads back to a resolution already in
progress up the request chain (a mutually-constrained pair) is detected on `activeFactKeys` and answered
"satisfied" rather than demanded. Demanding it would be refused by the engine as a dead-lock and recorded as a
cyclic-demand error, and a candidate must not be declined for a question we chose not to ask.

**The second half: effect accounting assumed instances are colocated with their ability.** With the ambiguity gone,
the fake `Console` resolved and the program then failed with `This value performs the effect 'Console' but does not
declare it` — reported at a value that declares exactly `{Console}`. The post-mono channel derived the performed
ability as `AbilityFQN(ref.moduleName, name)`, reading the ability's module off the *implementation reference*.
The orphan rule admits two placements — the ability's module, or a module of one of its type arguments — and that
reading is correct only for the first. Every stdlib instance takes the first; a fake takes the second, because the
test declares the carrier and not the ability. So `derived` was `Console@<the test's module>` while `declared` was
`Console@eliot.effect.Console`, and the subset check reported an undeclared effect for an effect that *was*
declared. `EffectAccountingProcessor.implementedAbility` now reads the ability off the implementation's own
resolved declaration (`Qualifier.AbilityImplementation(abilityFQN, _)`, the one place the ability's module is
recorded), which is unchanged for every colocated instance.

**What it costs a diagnostic, and what was done about it.** The failing demand moves: `printLine` into a body
pinned to `{Throw[String] | Id}` now fails as `Console` at that row — the jvm instance declining for want of
`Suspend` — where it used to fail as `Suspend[Id]` raised deep inside the stdlib lift that carried it. Failing at
the user's own call is the better position, but the message keyed on the ability being `Suspend` and the argument
being `Id`. `AbilityResolver.sideEffectOnPureCarrier` now reads the *base* of the row instead
(`GroundValueRenderer.baseCarrier`), so any effect demanded on a stack that bottoms out at `Id` keeps the
pure-base explanation rather than degrading to "no implementation found".

### L2 — The pre-monomorphization row verifier charged the harness for the effect it faked — *fixed*

```
error: This value performs the effect 'Terminal' but does not declare it; add it to its { ... } effect set.
```

The *post*-monomorphization channel gets this right: `EffectAccountingProcessor` gates a contribution by the ride
test — an effect counts only if it is performed on the value's own ambient carrier — and a fake-carrier run is by
construction not on the harness's ambient. The *pre*-monomorphization verifier
(`RowElaborationProcessor.verifyRow`) works from declarations alone, cannot see that the callee was instantiated
at a foreign concrete carrier, and reported the leak whenever `RowChecker.RowResult.decidable` held.

`decidable` was `declared.nonEmpty || !returnMayCarry`. So the identical harness was accepted or rejected purely on
the shape of its own return type:

```eliot
def transcript: Option[String] = Some(second(runRecorded(greeting)("")))   -- applied return: deferred, compiled
def transcript: String         = second(runRecorded(greeting)(""))        -- nullary return: rejected
```

That is why `greetSession` in the example used to return `Pair[Unit, Pair[String, String]]` — an accident, not a
design, and one that stood between the strategy and the `data TestResult(...)` a test harness naturally wants to
return.

**Fix — the argument-side undecidability, per row entry** (`RowChecker.fixesCarrier`). The derivation now marks the
entries that arrive at a slot **fixing a foreign concrete carrier**, and `RowResult.leak` subtracts them
(`RowResult.undecided`). This is the mirror of `returnMayCarry`: the return side asks "could my own declared return
be the carrier?", the argument side asks "did this contribution land in someone else's?". Three conditions, each
narrowing to the fake-carrier shape and each fail-safe *towards deciding*, the direction that keeps a diagnostic at
the definition:

- the argument is a **saturated call to a callee with a non-empty declared row** — a carrier-generic computation,
  the only thing a slot can fix the carrier of. An effect that merely *runs* on the way to the slot is a different
  thing: in `size(makeList(readLine))` the pure `makeList` cannot host `readLine`'s effect, so by §1 rule 1 that
  effect ran on the ambient and stays charged;
- the slot's declared type is a **concrete constructor applied to at least one argument**, and not the `Function`
  arrow. A nullary slot (`s: String`) cannot be a carrier at all. A slot headed by one of the callee's *own*
  binders (`step: F[Unit]` in `forever`, every effect-transparent combinator) is the opposite case — that binder is
  instantiated at *this* definition's ambient, which is exactly how an effect rides through a combinator, so it
  stays decided;
- the slot is **not what the argument's own declared payload already is**. This was the half that only became
  visible once the first half was written: `orEmpty(readLine)` hands `readLine`'s declared `Option[String]`
  straight to an `Option[String]` slot, which is a *payload* delivery, and reading it as a carrier fixing silently
  dropped the everyday "I forgot the effect set" diagnostic (`EffectDiagnosticVocabularyTest`,
  `ExamplesIntegrationTest1` both pin it). So the payload must be headed by a *different* concrete constructor,
  leaving the slot able only to be that constructor applied *to* the payload — `Recorded[A]` over a declared
  `Unit`. Note it is the *payload* and not the declared return: a rowed callee's return is `F[X]` by the time the
  row check reads it, `EffectSugarDesugarer` having written its minted carrier binder around the payload, so
  comparing returns would compare every rowed callee's carrier binder and decide nothing. A generic-headed payload
  decides nothing either, and stays charged.

A decided capture — a pinned slot, a platform run boundary — is excluded outright: the declaration already says
exactly what it consumes, and any residual rides on as before.

Deferral is per **row entry** rather than per definition, which the original proposal ("widen `decidable`") would
have made a whole-definition verdict. Two things fall out of that. An unrelated real leak in the same definition is
still reported at the definition, in effect vocabulary. And the bound applies to a definition that *does* declare an
ambient, which the old `declared.nonEmpty` short-circuit got wrong for a *different* ability: a faked `{Console}`
run inside a `{Log}`-declaring harness must not be charged the faked `Console` either.

**What it costs.** Rows are sets of abilities (§Appendix A.5's accepted approximation), so a definition that mixes a
faked run with a real leak of *the same* ability defers that entry and the user gets the post-mono symptom — a
`Type mismatch` at the harness body — instead of the located effect-vocabulary message. The program is still
rejected, which is the property that matters; only the diagnostic's position and wording degrade, and only in that
one mixed shape.

### L3 — The fake run and the assertions must be separate definitions

**This does not block a test framework.** `examples/src/EffectsTestFramework.els` is one — a `transcriptOf` whose
`program: Recorded[Unit]` slot is the seam, a `data TestResult`, an `expect` and a `report` — and it needs nothing of
what follows. The richer framework sketched in `docs/effect-row-tails.md` — a `TestCase` whose body is a pinned
`{Throw[AssertionError] | Id} Unit` — also works as-is against a fake carrier, because a pinned slot *is* a captured
slot and nothing leaks across it: an `AssertionError`, `assertEquals`, a `TestCase` with a pinned body and a
`runCase` folding `runId(runThrow(body(tc)))` compiles and runs against the fake `Terminal`, printing `PASS greet`
and, with the expectation changed, `FAIL greet: expected 'Hello, Alice!;' but was 'Hello, Bob!;'`.

The one correction is **where the fake run may sit: outside the pinned body, not inside it.**

```eliot
-- Step 1: run the production code under the fake carrier, yielding a plain value.
def greetTranscript: String = second(second(runSession(greet)(Pair("Bob", ""))))

-- Step 2: assert on that value, inside the framework's pinned body.
def greetTest: TestCase = TestCase("greet", assertEquals("Hello, Bob!;", greetTranscript))
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

It would buy a runner that takes a program on a bespoke carrier directly. It is not needed to ship a test framework,
so this is a later convenience, not a prerequisite — and with L2 fixed it no longer subsumes anything outstanding.

## 4. Recommendation

1. **Adopt the carrier-substitution strategy as the testing story.** It requires nothing of production code, it is
   the direct consequence of effects being rows over an inferable carrier, and it is already demonstrably working.
2. ~~**Fix L1**~~ **Done** (constraint-aware declination, plus reading an implementation's ability off its own
   declaration in effect accounting). This is what extends the strategy from application-owned abilities to
   `Console`/`Log`/`FileSystem`/`Process`/`Environment` — i.e. to the effects anyone actually wants to fake. It was
   the only blocker.
3. ~~**Fix L2**~~ **Done** (`RowChecker.fixesCarrier`: a contribution that a slot fixes to a foreign concrete
   carrier is deferred, per row entry, to the post-mono channel that already owns the question). It sat on the
   everyday path: it decided whether a harness may return the `data TestResult` a test author would naturally write,
   and it now may — `examples/src/EffectsTestFramework.els`.
4. **L3 needs nothing.** A test framework can be built today on plain values, or on pinned `{… | Id}` bodies; the
   cost is that a test is run-then-assert rather than a direct-style script. Revisit the declarable capture tag only
   if that shape turns out to chafe.
5. Keep **`Dep` + `provide`** documented as the seam-in-the-signature alternative, and **layer swapping** as the
   whole-program integration-test route. Keep **`where`** out of it.

## 5. Evidence

Everything above was established against the tree, the L1 and L2 rows re-checked against their fixes:

| Claim | How |
| --- | --- |
| A fake carrier interprets a user ability end-to-end | `examples/src/EffectsFakeCarrier.els`, compiled and run; prints `Hello, Bob!;` |
| A fake carrier interprets a **stdlib** effect end-to-end | `examples/src/EffectsFakeConsole.els`, compiled and run; prints `Hello, Bob!;` from the transcript, with no real output |
| A test framework on the strategy, returning a nullary `data` | `examples/src/EffectsTestFramework.els`, compiled and run; prints `PASS greet` / `PASS farewell`, with no real output |
| L1 (before): a fake `Console` instance is ambiguous | a `data Recorded` carrier with `implement Console[Recorded]` ⤳ "Multiple ability implementations found for ability 'Console' with type arguments [Recorded]" |
| L1 scope | `implement[F[_] ~ Suspend] …` in jvm's `Console`, `Log`, `File`, `Process`, `Environment` |
| L1 cause | `AbilityMatcher.matchImpl` matches patterns only; `verifyImplementation` added only the `where` guard; `paramConstraints` was read by no ability processor |
| L1 (after): fake resolves, real instance unaffected, nothing over-resolves | `AbilityConstraintDeclinationTest` — the fake interprets untouched production code, the real instance still resolves for a real carrier *in the same program*, and a carrier with no `Console` of its own resolves nothing |
| L2 (before) | the same program accepted with an `Option[String]` return and rejected with a `String` return |
| L2 (after): the harness's return type is free, and a real leak is still caught | `RowCheckerTest` — the fake-carrier harness derives `{Con}` and leaks nothing; an unrelated `{Bee}` in the same definition is still reported; `takeFake(mkFake(readLine))` and `takeOpt(readOpt)` (an effect that merely *runs* on the way to a concrete slot, and a payload delivery) both stay charged |
| L2 (after): the everyday "I forgot the effect set" diagnostic is untouched | `EffectDiagnosticVocabularyTest` / `ExamplesIntegrationTest1` — `def helper: String = printLine(orEmpty(readLine))` still reads "performs the effect 'Console' but does not declare it" |
| L2's residual cost | a harness mixing a faked run with a real leak of the *same* ability is still rejected, but as a post-mono `Type mismatch` at the harness body rather than the located effect message |
| L3 does *not* block a framework | a mini framework (`TestCase` with a pinned `{Throw[AssertionError] \| Id} Unit` body, `assertEquals`, `runCase`) run against the fake `Terminal`: `PASS greet`, and `FAIL greet: expected 'Hello, Alice!;' but was 'Hello, Bob!;'` with the expectation changed |
| L3's actual constraint | the run inside the pinned body ⤳ `Expected: {Throw[AssertionError] \| Id} Session[Type]` / `Actual: {Throw[AssertionError] \| Id} Unit`; moved to its own definition, it compiles |
| pinning over the fake carrier does not help | `{Throw[AssertionError] \| Session} Unit` ⤳ "performs the effects 'Terminal', 'Transcript' but does not declare them" *and* "No ability implementation found for ability 'Transcript' with type arguments [{Throw[AssertionError] \| Session}]" |
