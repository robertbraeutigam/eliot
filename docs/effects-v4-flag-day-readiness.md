# Effects v4: is the flag day ready? (a readiness check, 2026-08-19)

**Status: a check, not a decision.** `docs/effects-as-channel-v4.md` §11 sequences P0 → P1 → P3 → the flag day
(P2+P4, which `docs/effects-v4-p2-sizing.md` §1 showed are one change), and P0, P1 and P3 have landed with their
gates met. This note asks the one question left before the flag day starts — *is everything ready for it?* — and
answers it against the tree at `claude/effects-v4-flag-day-qxhtl9` (`6160f24`), which is green: `./mill __.test`
exits 0, 1,773 tests, 0 failures.

**Verdict: not yet.** Three things stand in the way, and only the third is the one the plan already knows about:

| # | what | kind | cost to clear |
| --- | --- | --- | --- |
| **B1** | v4 deletes the only mechanism a program has to **substitute an effect implementation**, and the design does not notice | design hole | a decision, then design |
| **B2** | §7's deletion list retires the `Effect`/`Suspend` machinery, which *is* the representation the lowering emits | amendment (second of R4's kind) | a paragraph, plus corrected arithmetic |
| **B3** | R7 (the stored-computation hoist) is an open decision explicitly reserved for Robert | decision | adopt the sizing note's proposal |

B1 is the one that matters. B2 and B3 are cheap. Per `docs/effects-as-rows.md` standing rule 1 the decisions are
Robert's, and per v4 standing rule 5 (*stop on conflict*) B1 is surfaced here rather than routed around: it was
found by asking what the flag day's own gate — `__.test` green and every example jar `md5sum`-identical — would
do on day one.

## 1. What *is* ready

Nothing below is in doubt; it is recorded so the gap is not read as a blanket "no".

- **P0** (`docs/effects-v4-p0-spike.md`) landed with `jvm/test/…/EffectsV4SeamGroundnessTest.scala` as its
  permanent measurement: every instance's carrier is ground at the `WovenValue` seam.
- **P1** landed the type-language half: `GroundValue.Row`/`Computation`, `VRow`/`VComputation`, the canonicaliser
  (`monomorphize/fact/CanonicalRow`), the canonical row ⤳ stack rule (`row/CanonicalStack`), the `unify` cases and
  both printers, pinned by `lang/test/…/row/EffectsV4RowAndComputationTest.scala`. Nothing produces either former
  yet, so the tree is unchanged in behaviour — which is exactly the position a flag day wants to start from.
- **P3** landed the woven re-check (`monomorphize/channel/WovenRecheck`), mandatory at the seam beside
  `assertNoIdResidue`, and is a no-op on today's output.
- **R2** is sized (`docs/effects-v4-p2-sizing.md` §2): ability selection is an *addition* at the seam, not a move.
- **R8 and Q2** are settled by P1's canonical form, and recorded there.
- The **surface arithmetic** the sizing note gives checks out on the tree: 121 `.els` files, 43 importing
  `eliot.carrier`, 31 `~ Effect` / `~ Suspend` constraints in code (22 on `implement` heads, 9 on `def` heads).

## 2. B1 — there is no user-reachable interpretation seam left

### 2.1 What today's tree does

`docs/testing-effects.md` is marked **"adopted, done"**, and its mechanism is stated in its own §2 heading:
*the carrier **is** the injection point*. A test declares its own carrier and its own instance of the ability for
that carrier, and the production code — which names no carrier — is instantiated at it:

```eliot
data Recorded[A](runRecorded: Function[String, Pair[A, String]])
implement Effect[Recorded] { … }
implement Console[Recorded] { … }

def transcript: String = second(runRecorded(greeting("Bob"))(""))
```

That is not a document-only claim. Six example programs declare their own carrier and its `Effect` instance, and
they split into two kinds — the distinction matters, because v4 treats them oppositely:

- **Pure discharge** — `EffectsState`, `EffectsOrdering`, `EffectsTestable` declare `data Id[A](runId: A)` only to
  run a `{State}` / `{Abort}` program with no I/O: `runId(runStateToPair("first", swap("second")))`. **v4 deletes
  the need, not the capability**: the canonical base does this, and per the sizing note §3 the discharger returns
  `Pair[A, S]` outright. These three get shorter, and they are evidence *for* v4, not against it.
- **Substituted interpretation** — `EffectsFakeCarrier` (a fake `Terminal[Session]`), `EffectsFakeConsole` and
  `EffectsTestFramework` (a fake `Console[Recorded]`) declare a carrier in order to say **which implementation of
  an ability the program runs on**. There is no v4 spelling for this at all. `__.test` covers it twice over:
  `ExamplesIntegrationTest1` runs those programs end to end ("should report a failed assertion against the fake
  carrier's transcript"), and `jvm/test/…/AbilityConstraintDeclinationTest.scala` is a dedicated class for the L1
  fix that makes a fake `Console` resolve at all.

### 2.2 What v4 does to it

Every leg of the mechanism is removed by the design as written, in three independent ways:

1. **The user cannot name a carrier — by either of the two routes v3 gives.** The routes today are the **pinned
   tail** (`{Throw[E] | Id} A`, `{State[List[String]] | Id} Unit` — concrete pins are used in the test corpus, and
   a `type` alias over a pinned row works) and a plain **carrier-typed slot** (`def transcriptOf(program:
   Recorded[Unit])`, which is what `EffectsTestFramework` uses, since a `Suspend`-riding effect cannot be pinned
   today — `docs/effect-row-tails.md` "Limits", v1). v4 deletes both: §3 rule 3 ("it needs no `| G` tail to say
   so, because there is no carrier to pin to"), §11 P5 ("`| G` tails, the pinned/open distinction … go"), and §2
   tier 3 / §8 for the slot ("never named by a user"; "`IO` stops being reachable from any user or stdlib
   signature"). Note the subtlety on the second route: `Recorded` stays spellable — it is ordinary user `data` —
   but nothing can *produce* a `Recorded[Unit]` any more, because `greet("Bob")` has computation type
   `{Console} Unit` and the row ⤳ stack map is compiler-owned.
2. **The user cannot choose one either.** Under §6 the stack is not a choice but a *function of the row*:
   `row/CanonicalStack.of` puts a `Suspend`-riding row (every `Console` program) on **the platform's run
   carrier**. The run-boundary registry that names that carrier is compiler configuration
   (`row/RunBoundaryFunctions.configKey`, contributed by `JvmPlugin`/`LspPlugin`), not something a program can
   reach. So `greeting : {Console} Unit` lowers onto `IO` and onto nothing else.
3. **The instance the seam selects is therefore always the one written for the platform's stack.**
   `Console[Recorded]` may still be *written* — the ability keeps its `F[_]` parameter — but nothing will ever
   query it. Worse, the thing that made the query unambiguous in the first place, constraint-aware declination on
   `~ Suspend` (`docs/testing-effects.md` L1), is deleted with the machinery abilities (B2).

It is worth being explicit that this is **wider than testing**. `EffectsFakeCarrier`'s `Terminal` is an
*application-owned* ability whose only implementation is on the program's own pure carrier. Under §6 a row with no
canonical carrier rides `Suspend`, so `{Terminal}` lowers onto the platform run carrier and the seam demands
`Terminal[IO]` — an instance the program cannot write, since `IO` is jvm-owned and unreachable from user
signatures (§8). The one escape is for the program to make `Terminal` carrier-*bearing* instead: declare a
`TerminalCarrier`, implement `Effect`/`Suspend` over it and write a discharger — which is the carrier vocabulary
tier 3 forbids users and §7 deletes, done by hand. So the question B1 asks is not only "how do we fake `Console`
in a test": it is **on what terms a program may interpret an effect of its own at all**, and v4 as written answers
"only by using the machinery it removes".

**"Discharge it instead" is not available.** §5 mentions the strategy exactly once, and only about accounting's
argument-side bound: `RowChecker.fixesCarrier` "becomes 'the callee's row was consumed by a discharger'". But the
tree has dischargers only for the five carrier-bearing effects (`Abort`, `Throw`, `State`, `Writer`, `Dep`); the
`Suspend`-riding ones a test actually wants to fake — `Console`, `Log`, `FileSystem`, `Process`, `Environment` —
have none, and by §6's rule they have no carrier for one to be written over. Writing them means user-visible
handlers, which §6 explicitly says v4 does not build ("v4 does *not* require building an effect-handler runtime
or delimited control").

### 2.3 Why this blocks the start, rather than being follow-up work

The flag day's gate is `__.test` green **and** every example jar `md5sum`-identical. Three example programs (and
whatever a user-defined ability would look like) cannot be *expressed* after the change, so they cannot compile to
identical bytecode; the two integration test classes above have no subject left. There is no version of the flag
day that meets its own gate while B1 is open — the gate would have to be amended by hand for precisely the
capability the change removes, which is the shape v4 standing rule 5 exists to catch.

It is also the one place where v4's ledger is wrong in a way that matters. §0's table asks of each deleted
mechanism *"is this about which effects a program performs?"* and answers no. For the carrier it is no — except
here: the carrier is not only the representation, it is **the choice of interpretation**, and that is a user-facing
capability, not plumbing. Removing it is a language decision, not a lowering decision.

### 2.4 The options, and a recommendation

- **(a) Grow user-visible handlers.** Give the `Suspend`-riding abilities canonical carriers so `{Console}` lowers
  to a transformer a user can discharge with a fake handler. Most principled; keeps both the strategy and "no
  carrier in the language". Also the largest scope increase in the whole project — it is the effect-handler
  runtime §6 rules out — and it changes the production lowering of every I/O effect. Not a flag-day-sized step.
- **(b) Drop pinning entirely, and make the seam a term, not a type** (recommended; updated 2026-08-19 after
  Robert's preference to remove pinning if storage survives). **Storage does not need the tail** — that is what
  tier 2 buys, and it is the one place v4 is unambiguously simpler than v3: `data TestCase(name: String, body:
  {Throw[String]} Unit)`, `List[{Console} Unit]`, `f: A => {Console} B` are ordinary types whose stack the seam
  computes from the row (`row/CanonicalStack`, §6). So the `| T` tail, the pinned/open distinction, the
  `<Ability>Carrier` spelling and the "a stored row must be pinned" rule can all go exactly as §7 says.

  What must not go with them is the **choice of interpretation**, and it need not: put it at the *run site*
  instead of in the type — one form that runs a computation at a named base and returns that base's own type:

  ```eliot
  def transcriptOf(program: {Console} Unit): String = second(runRecorded(runAt[Recorded](program))(""))
  ```

  Nothing here is a carrier in a stored or passed type: `program`'s type is base-free as tier 2 wants, and
  `Recorded` appears once, in test code, as an ordinary user `data` type constructor. The mechanism already
  exists — this is precisely a **run boundary** (`row/RunBoundaryFunctions`: "parameter 0 *hosts* a computation on
  a carrier rather than receiving it as data", registered today for `eliot.jvm::runMain`), generalised from a
  platform-registered FQN with a fixed base to one form whose base is a type argument. The seam then weaves the
  argument at the stack `CanonicalStack` builds over **that** base instead of the platform's, and selects
  `Console[Recorded]` there — the same query that runs today, which is why the declination mechanism B2 keeps is
  what makes it unambiguous.

  Cost: one form plus its lowering rule, and the honest restatement that the carrier leaves *types* rather than
  the language outright. Against v3 this is a simplification for users too: the concept a test author has to hold
  is "run this at my carrier", at one call, rather than a representation spelled into a type.

  One price is paid by dropping the tail, and it is **R8**, already recorded and settled by P1: with no pin, the
  author no longer chooses a stored computation's layer order or base — the canonical order does. For a stored
  `{Throw[E], State[S]}` that is the difference between state surviving a raise and not, decided once by the
  canonical key rather than per declaration. That is the trade removing pinning makes; it is not a new risk, but
  it is the one thing the tail said that the row cannot.
- **(c) Drop substituted interpretation.** State that a program cannot choose an ability's implementation, delete
  the three examples, the two test classes and most of `docs/testing-effects.md`, and rewrite the flag-day gate to
  match. Note this is *not* "test with a swapped layer": a test project is **additive** — it sees the subject's
  sources plus its own — so what it can add is more *instances*, and an added instance only bites if something
  selects it. Under v4 nothing would: an added `Console[Recorded]` is never queried, and an added `Console[IO]`
  collides at the merge ("Has multiple implementations."). The whole-layer swap `docs/testing-effects.md` §2.2
  lists is a different thing — replacing `jvm/eliot` wholesale on the `--path` — and it substitutes everything at
  once, so it is an alternative to the strategy, not a version of it.

Whichever is chosen, it must be chosen **before** the flag day starts: (a) and (b) both add a rule the lowering
has to implement, and (c) rewrites the gate the flag day is measured by.

## 3. B2 — the machinery abilities are the representation, not sugar over it

§7 lists for deletion "`Effect` / `Suspend` machinery abilities, `Id` + `Effect[Id]`", and the sizing note counts
"34 `~ Effect` / `~ Suspend` machinery constraints to delete". Both are too broad, in exactly the way §10 R4
already found for `Id`: what leaves is the *language surface*, not the code.

The tree holds **22 `Effect`/`Suspend` instances** — 16 in the layers (`jvm/eliot/eliot/effect/*`,
`jvm/eliot/eliot/jvm/IO.els`, `stdlib/eliot-compiler/…`, `lang/eliot-compiler/…/Id.els`) and 6 in the example
programs of B1. The 16 **are** what the seam lowering emits calls to: `flatMap`, `pure`, `map`, `suspend` at each
ground stack. They are ordinary Eliot, written over carrier generics, and their `~` constraints are not
decoration — they are what lets the bodies typecheck (`implement[E, G[_] ~ Effect] Effect[ThrowCarrier[E, G]]`
calls `pure` and `flatMap` on its base `G`; `implement[F[_] ~ Suspend] Console[F]` calls `suspend` on `F`). Of the
31 constraints in code, **22 sit on `implement` heads — the machinery instances and the carrier-indexed effect
instances the seam selects — and stay**; only the **9 on `def` heads** (the dischargers' `G[_] ~ Effect`,
`foldLeftInternal`, `providedValue`, `runTest`) actually go with the flag day.

Two consequences for the plan, neither expensive:

- §7's deletion table and the sizing table both need the R4 treatment: the machinery leaves the *language* (no
  user or stdlib signature names it, `{Effect}` sugar and the `~` machinery constraints on ordinary `def`s go)
  and stays as the representation's Eliot code.
- P5's "the `eliot.carrier` package is removed from the path" can only mean *removed from user scope* — which it
  already is, being import-required. Removing it from the path would take the platform's own effect library
  with it.

There is one more unlisted item in the same family: **natives with carrier-typed parameters**.
`private def foldLeftInternal[F[_] ~ Effect, A, B](list: List[A], initial: F[B], combine: A => F[B] => F[B]): F[B]`
is a bytecode leaf whose *signature* names a carrier. Under v4 it must be spelled with computation types, and the
lowering must therefore lower **signatures of leaves**, not only bodies — a case §6 does not mention and the
sizing table does not count. It looks mechanical; it should still be on the list before the day starts, since it
lands on the backend's native mapping (`.claude/skills/eliot-jvm-backend`).

## 4. B3 — R7 is still an open decision

`docs/effects-v4-p2-sizing.md` §4 proposes the fail-safe rule — *a stored computation is discharged at its
canonical base, and a mismatch is a hard error naming both, never a silent lift* — and closes with "The decision
is Robert's". Nothing in the tree needs a hoist, so the proposal costs nothing to adopt and cannot miscompile;
adopting it is recommended, and it must be recorded before the lowering is written, because it *is* one of the
lowering's error paths.

## 5. Smaller items, none blocking

- **Q4 is overdue rather than open.** It asks whether a flow grade belongs in the type of a first-class
  computation, and §10 says to settle it "before the canonical form is fixed". P1 fixed the canonical form. Adding
  an entry kind afterwards is §4's two-spellings trap, so Q4 is best answered now, while nothing produces a row.
- **Q1** (does anything besides accounting need a definition-site row certificate) decides whether `RowChecker`
  grows or shrinks in the flag day; it can be answered while writing it.
- **R5** (diagnostics must speak rows and payloads, not `Computation[…]`) is work inside the flag day, and the
  P4 gate already names it.

## 6. What would make it ready

1. A decision on **B1** — (a), (b), (c) or something better — written into `docs/effects-as-channel-v4.md`, with
   the flag-day gate restated to match it.
2. The **B2** amendment folded into §7 and into the sizing tables, plus the native-signature item added to the
   work list.
3. **R7** closed by adopting (or rejecting) the sizing note's proposal, and **Q4** answered.

With those three in place the flag day is a large but ordinary change with a measurable gate, and
`docs/effects-v4-p2-sizing.md` §5 is the order to do it in. Without B1 it is a change that cannot meet its own
gate and that deletes a shipped capability on the way.
