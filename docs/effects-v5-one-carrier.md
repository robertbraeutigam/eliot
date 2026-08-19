# Effects v5: Rows Are Constraints on One Carrier

**Status (2026-08-19): PROPOSAL, and it decides nothing** — per `docs/effects-as-rows.md` standing rule 1 the
decision is Robert's. Written from scratch against seven constraints he stated: *we need effects; unordered
preferred; effects as monadic abilities preferred; a mechanism independent of `monomorphize`, so it can stay
simple; no pinned rows, they are hard to grasp; effects must stay generic — testable with pure carriers; and `IO`
is not special — it is discharged in Eliot too, private to the platform but ordinary from the language's point of
view.*

**The finding, in one line.** Every one of those seven is satisfied by **taking things away from v3**, not by
adding a type former: v3's carrier-as-a-constraint encoding is already the simplest mechanism; what makes it hard
is the four spellings layered on top of it — pinned rows, generic tails, `{Effect}` reuse and the machinery
constraints. Delete those and one rule is left.

**Against v4** (`docs/effects-as-channel-v4.md`): v4 moves the row *into* the type and the carrier *out* of the
language. That buys rule 4 as a theorem, and it costs a flag day, a canonical order that decides semantics an
author used to choose (R8), a hoist a user can now need (R7), row weakening that must not enter `unify` (R11), and
the loss of the interpretation seam (R9). v5 goes the other way: the row **never** enters a type, and the carrier
stays exactly what it already is — an ordinary generic parameter. Every one of R7, R8, R9 and R11 is then not
solved but *absent*, and P0/P1/P3's landed work is unaffected (P1's `Row`/`Computation` formers simply stay
unused).

---

## 1. The whole mechanism

**One rule, in two positions.** A `{…}` row is an unordered set of abilities, and it means something about *this
definition* wherever it appears:

| position | meaning |
| --- | --- |
| **return** | *what I perform* — my ambient carrier has these abilities, and my caller inherits them |
| **parameter** | *what I supply to this argument* — I run it, on my ambient carrier extended by these |

Nothing else. `{}` in a parameter position is the empty supply: "suspended, on my own ambient carrier, nothing
added". `{}` in a return position is "I perform nothing of my own".

```eliot
def greet: {Console} Unit = printLine("hi")                            -- performs Console
def fold[A](c: Bool, whenTrue: {} A, whenFalse: {} A): {} A            -- effect-transparent: both arms are mine
def if[T](condition: Bool, value: {} T): {Abort} T                     -- performs Abort; the arm rides my ambient
def else[A](computation: {Abort} A, fallback: {} A): A                 -- supplies Abort: discharged here
def catch[E, A](computation: {Throw[E]} A, onError: E => {} A): A      -- supplies Throw[E]
def runStateToPair[S, A](initial: S, p: {State[S]} A): Pair[A, S]      -- supplies State[S]
```

Read the last three as English and they are already right: *"I will run this computation, and the `Throw` it needs
comes from me."* That is what a discharger is, and it needs no tail, no base, no stack and no second concept.

**Everything else is ordinary generics.** There is no third kind of slot. If you want to name a carrier — because
you are writing a test, or storing a computation — you write an ordinary higher-kinded type parameter with
ordinary `~` constraints, exactly as you would for any other container:

```eliot
data TestCase[F[_] ~ Throw[String]](body: F[Unit])          -- storing a computation: an ordinary generic type
def transcriptOf(program: Recorded[Unit]): String           -- testing: an ordinary concrete type argument
```

This is the answer to *"where does a stored computation's carrier come from — is it a generic parameter of the
data structure?"*: **yes, and written by you, as ordinary polymorphism.** No new concept is introduced for
storage, and none is needed: `TestCase[F]` is a generic type like `List[A]`, instantiated per use by ordinary
inference and specialised by whole-program monomorphization.

## 2. Why each of the seven constraints is met

- **Effects.** Unchanged from v3: a row on a signature, direct style in bodies, `derived ⊆ declared` verified by
  the channel.
- **Unordered.** A row is a set of ability constraints on one carrier — there is no stack in any type a user
  writes. Where a representation *does* need an order (two dischargers nested), the order is the **nesting of the
  calls in the term**, which is where the semantics is chosen anyway: `runState(s, catch(p, h))` and
  `catch(runState(s, p), h)` differ, visibly, in the code. Nothing canonicalises, so v4's R8 — the canonical order
  silently deciding whether state survives a raise — cannot arise.
- **Monadic abilities.** Unchanged: an effect is an ability over a carrier `F[_]`, its implementation is an
  instance, and discharge is an ordinary Eliot function over a carrier that has the ability.
- **Independent of `monomorphize`.** The mechanism is a **source-to-source desugar before the checker**: it writes
  every carrier type argument, every `flatMap`, every `pure`. Downstream, nothing knows effects exist. That is
  already v3's stated ideal ("the elaborator writes the carrier; the checker never solves for one"), and v5 is
  what makes it true, because the three checker collaborators that survive today exist for the spellings v5
  deletes: `EffectLifter` (a pure term meeting a carrier slot — the desugar writes the `pure`),
  `CarrierKindChecker` (kind-checks carrier metavariables — there are none once every carrier is written), and
  the carrier half of `GuardDischargeResolver`.
- **No pinned rows.** Deleted outright, with the `| T` tail, the open/pinned distinction, the "a stored row must
  be pinned" rule, and the `<Ability>Carrier` convention as a *user-facing* rule.
- **Generic, testable with pure carriers.** A `{Console}` definition is carrier-generic, so a test instantiates it
  at its own pure carrier — the mechanism `docs/testing-effects.md` calls adopted and done, unchanged, and the
  three example programs that use it keep working with no rewrite.
- **`IO` is not special.** It is one carrier with instances, discharged in Eliot. v5 adds no run-boundary concept
  and no platform-owned form; the platform's entry point instantiates `main`'s carrier at `IO` by ordinary
  unification, as it does today.

## 3. What this deletes from the tree

Compared with v3-as-landed, and all of it subtraction rather than replacement:

| what goes | why it existed |
| --- | --- |
| pinned rows, `{… \| T}` tails, "a stored row must be pinned" | a row that must be a *type* needed a concrete stack |
| the open/pinned distinction, `aliasPinnedEffects`, the alias limitation | consequences of the above |
| the `{Effect}` binder-reuse rule | a discharger had to reuse its own binder to spell a slot |
| `~ Effect` / `~ Suspend` on ordinary `def` heads (9 in the tree) | the machinery had to be named to be constrained |
| rule 4's slot trichotomy in `RowElaborator` | three kinds of slot collapse to two, decided syntactically |
| the `<Ability>Carrier` naming convention as a user rule | an ability declares its carrier instead (§5 Q4) |

What **stays**, unchanged: `EffectRow` as declaration metadata; the two verifiers and their vocabulary; the
dischargers; `Inf`; the ability system; `Id` as the pure carrier a fully-discharged computation lands on; the
`eliot.carrier` machinery as *internal* code (v4's R10 finding applies here too — the instances are the
representation, so they stay, and only their user-facing spelling goes).

What **does not happen**: no new type former, no row in any type, no canonical stack, no re-keyed weave, no
relocation of ability selection, no re-check obligation, and **no flag day** — §4.

## 4. How it would land — incrementally, which is the point

v4's single biggest practical objection is R6: the checker, the elaborator and ability resolution must move
together. v5 has no such step, because every intermediate state is expressible in today's language:

1. **`{}` as the parameter-position spelling of "on my ambient carrier"**, accepted alongside `{Effect}`. Pure
   addition; nothing breaks. Migrate the 28 `{Effect}` occurrences — note they are already both positions today
   (`foldLeft(initial: {Effect} B, …): {Effect} B` is "mine" in the argument *and* in the return), which is the
   same two readings v5 gives `{}`.
2. **A tailless row in *parameter* position means "supplied by me"**, i.e. what `{X | G}` means today with `G` the
   signature's own binder. Migrate the 15 pinned rows in code; the generic-tail ones are a one-for-one rewrite
   (`{Abort | G} A` ⤳ `{Abort} A`), and the elaborator's existing derived-discharge-stack rule
   (`carrier(call) = stack(callee.declaredRow ∖ ambient.declaredRow) over ambient`) is *already* this rule.
3. **Concrete pins become ordinary generics.** `data TestCase(body: {Throw[E] | Id} Unit)` ⤳
   `data TestCase[F[_] ~ Throw[E]](body: F[Unit])`, or keep a concrete carrier where one is wanted. Handful of
   sites, each independently green.
4. **Delete what is then unused**: the tail syntax, the pinned/open machinery in `EffectSugarDesugarer`, the
   `{Effect}` reuse rule, and — once the desugar writes every `pure` — `EffectLifter` and `CarrierKindChecker`.

Each step is `__.test`-green and example-jar-identical on its own, which is the gate v3 and v4 both use.

## 5. Costs, risks and open questions — honestly

- **C1 — storing a computation gets more verbose, and shows `F[_]`.** `data TestCase[F[_] ~ Throw[String]](body:
  F[Unit])` is longer than `data TestCase(body: {Throw[String] | Id} Unit)`. The claim is not that it is shorter
  but that it is **not a new concept**: it is the generic parameter the user already writes for any container, and
  it is *inferred* at every use site rather than pinned by the author. Whether that trade is right is Robert's
  call, and it is the one place v5 asks the user for more than v3 does.
- **R1 — the `{}` spelling is a bikeshed with real stakes.** "Suspended on my ambient" is the most common slot in
  the stdlib (`fold`, `if`, `else`'s fallback, `catch`'s handler). If `{}` reads as "no effects" rather than
  "mine", it will mislead. Alternatives: keep `{Effect}`; write the row the arm may perform (`{Abort} T` on `if`'s
  arm, which is what it means today); or a word.
- **R2 — an ability with no carrier of its own cannot be supplied.** `{Console}` in a parameter position asks the
  callee to supply `Console`, and there is no `Console` transformer, so it is a diagnostic — the same limitation
  v3 has ("Suspend-riding effects cannot be pinned"). Testing does not need it (it substitutes the carrier), but
  the message must say which of the two routes to take.
- **R3 — effect-transparency is free, and that must be checked rather than assumed.** `fold[A](c, whenTrue: {} A,
  whenFalse: {} A): {} A` carries whatever its arms carry because all three share one carrier; v4 needed a row
  variable and then row weakening (its R11) for the same thing. This is v5's strongest structural claim and it
  should be verified on `foldLeft` and on `TestSuite.els`'s reflection shape before anything is deleted.
- **Q1 — does the desugar really leave the checker with no effect rule?** Verify by deleting `EffectLifter` and
  `CarrierKindChecker` and seeing what fails; that measurement is the gate for step 4 and can be run cheaply
  today.
- **Q2 — what does an ability declare about its carrier?** The `<Ability>Carrier` convention should become an
  explicit declaration on the ability, so "which representation supplies this effect" is written once, in one
  place, and no phase recognises a carrier by name.
- **Q3 — `Id` and the pure default.** A fully discharged computation in a pure position lands on `Id` today
  (`def port: String = setting("port") else "8080"`). v5 keeps that; it is worth deciding whether `Id` should be
  ordinary stdlib `data` (it is) and nothing more.
- **Q4 — what happens to P1's `Row`/`Computation` formers?** They are inert in the tree and v5 needs neither.
  Leave them dormant (cheap, and they are what v4 would need if v4 is ever revived) or revert them.

## 6. What is *not* claimed

v5 does not make rule 4 a theorem — a rowed slot is still a rule the desugar implements, and the erosion history
in `docs/effects-as-rows.md` Appendix A is a warning about exactly that. What it does instead is reduce the rule
to a **syntactic** one with two cases (a slot has a row or it does not) rather than a semantic classification with
three (rowless / carrier-headed / pinned), which is what actually eroded. That is a smaller claim than v4's, and
it is available without a flag day, without a canonical order, and without giving up the ability to interpret an
effect yourself.

## 7. Aliasing an effectful type — alias the **row**

Yes, and in v5 the natural unit to alias is the row itself, because a row *is* a set of abilities and nothing
else. Naming a set of effects once is also what a user usually wants — far more often than naming one
computation type:

```eliot
type Web = {Console, Log, Throw[HttpError]}

def handle(request: Request): {Web} Response                -- performs all three
def audited[A](action: {Web} A): A                          -- supplies all three: a handler for the set
def retry[A](action: {Web, State[Attempt]} A): {Web} A      -- rows compose by union, unordered
```

Parameterised aliases work the same way, since an entry is an ability applied to arguments:

```eliot
type Fallible[E] = {Throw[E], Log}
def parse(s: String): {Fallible[SyntaxError]} Tree
```

**The mechanics are one rule, in one place.** Rows desugar in `core` to ability constraints written by name
(`F ~ Web`), and names are resolved later; so the only change is that `resolve`, when it meets a row entry naming
a `type` whose body is a row, **splices that row's entries into the constraint list** instead of resolving one
ability (`resolve/processor/ValueResolver.resolveAbilityName`, reached from both `resolveEffectRow` and
`resolveParamConstraints`). Core is untouched, the lowering is untouched, and the channel sees the expanded row,
so accounting and the two verifiers keep working with no new vocabulary. This is the change `docs/effects-as-channel-v4.md`
§A.3 identified as *"the right change if v3 stands"* — it is equally right under v5, since v5 keeps the encoding
A.3 assumes, and it can land **on today's tree, independently of everything else here**.

Two things fall out for free. A row alias naming itself is a cycle in the value-reference graph, which the
recursion gate already rejects (*Total by Default*), so no new cycle check is needed. And a row alias is usable in
both positions of §1 with no extra rule, because it expands before either position is interpreted.

**What is deliberately *not* offered is aliasing the payload with the row** — `type Test = {Writer[List[TestCase]]}
Unit`, the case that provoked v4 (§0 there). That is aliasing a *computation*, which is a carrier-applied type, so
in v5 it is ordinary generics and is written as such:

```eliot
type Test[F[_] ~ Writer[List[TestCase]]] = F[Unit]
```

The reason the sugar cannot quietly do it is mechanical and worth stating, because it is the same reason the
current tree errors: the row desugar runs in `core`, before names resolve, so a definition that merely *names* an
alias (`def testCases: Test`) carries no `{…}` of its own and mints no carrier — today's diagnostic says exactly
that, and it is right ("the open-row lowering mints the shared carrier onto the *alias's own* generic parameters
… the effect is silently dropped"). Under v5 the position rule needs an ambient carrier to attach the row to, and
a bare alias name supplies none. So the honest spellings are the two above: the row alias at each use, or the
explicit generic parameter — and the first is the one to ship.
