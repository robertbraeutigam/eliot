# Effect-Row Tails: Pinned Rows as the One Spelling of a Carrier Stack

Status: **SHIPPED** (parser + core desugar + stored-row rule + stdlib sweep + hover rendering).
**Amended 2026-08-19 by effects-v5 step 2** (`docs/effects-v5-one-carrier.md` §4): the **generic tail** below is no
longer written in the tree. A discharger spells its input as a plain row (`{Throw[E]} A`) and the desugar pins it to
the signature's own carrier, producing the identical type — so everything here still describes what a pinned row
*is* and what it desugars to; only the third usage level (writing `| G` by hand) is gone. A **concrete** base
(`| Id`, `| IO`) is untouched and is still the spelling a stored row must use.

## The problem this solves

An open effect row (`{Throw[E]} A`) is the right tool for *functions*: the carrier generic it
desugars to is re-instantiated at every call and solved by unification, so it stays invisible. A
**stored** effectful value is different in kind — a `data` field, a collection element, a
`namedValues` result must commit to one concrete representation. Before this feature the
commitment had to be spelled in machinery vocabulary (`TestCase[ThrowCarrier[AssertionError, Id]]`),
leaking the carrier names (`ThrowCarrier`, `Id` composition order, partial application) onto
exactly the users the row sugar was hiding them from. The alternative — auto-lifting a carrier
generic onto the data type — just relocated the commitment to whoever first needed the type to be
a concrete *name*, and made the data type generic in tooling (hover, apidoc) besides.

## The design: rows have tails

The flex carrier the open-row sugar mints **is** a row tail that had no syntax. Giving it syntax
unifies the whole surface:

```eliot
{Throw[E]} A          -- open: tail elided, caller picks the carrier (unchanged)
{Throw[E] | Id} A     -- pinned: the canonical Throw layer over the pure base Id
{Throw[E] | G} A      -- generic tail: superseded by effects-v5 step 2 — write `{Throw[E]} A`
```

A **pinned row** is not a constraint but a *concrete type*: the canonical carrier stack realizing
exactly those effects over the base. The desugar is pure type application by the
`<Ability>Carrier` naming convention (the carrier is colocated with its ability, so it resolves
wherever the ability does):

```
{Throw[E], State[S] | Id} A   ⤳   ThrowCarrier[E, StateCarrier[S, Id], A]
```

- **Entries are ordered**: leftmost = outermost = discharged first. The nesting order of a stack
  is semantically observable (state-preserved-on-failure vs not), so pinning makes it a visible,
  user-written choice. Open rows remain unordered constraint sets.
- **No generic parameter is introduced** — everything downstream (merge, checker, accounting,
  codegen) sees exactly what the hand-written carrier spelling produced before.
- A pinned row is one particular *solution* of the corresponding open row's constraints, so
  construction-by-unification just works: a direct-style body's flex carrier unifies with a
  pinned parameter or field type.

### The stored-row rule

A `data` constructor field with an *open* positive row is rejected:

    A stored effect row must be pinned to a base carrier, e.g. `{Throw[Error] | Id} String`.

(The pre-existing carrier-lift lowering is kept only as error recovery so downstream checks still
run.) A pinned field rewrites to the concrete stack and the data type stays non-generic:

```eliot
data TestCase(name: String, body: {Throw[AssertionError] | Id} Unit)
```

`namedValues[TestCase]` works bare, constructors take direct-style bodies, and the ordinary
dischargers consume the field (`runId(runThrow(body(tc)))`).

### No discharge markers

There is no `-E` syntax. A discharger's consumed effect vanishes *structurally* at the
monomorphize-phase residual check (it lands on an inner transformer carrier, absent from the
caller's ambient), so a discharger's return type is just the plain output carrier — no negative
member to spell, and nothing to reject inside a pinned row. (This replaced the earlier `{-E}`
annotation; see the effect section of `.claude/CLAUDE.md`.)

### Stdlib sweep

Every discharger signature spells its *input* as a row and its *output* as the plain carrier (the carrier
data-type names are gone from all `def` signatures). Since effects-v5 step 2 the input row is written **without**
the tail — a parameter row is supplied by the definition, so the desugar pins it to the signature's own carrier and
the type below is what it produces:

```eliot
def runThrow[E, G[_] ~ Effect, A](obj: {Throw[E]} A): G[Either[E, A]]
def catch[E, G[_] ~ Effect, A](computation: {Throw[E]} A, onError: E => {} A): G[A]
def else[G[_] ~ Effect, A](computation: {Abort} A, fallback: {} A): G[A]
def runStateToPair[S, G[_] ~ Effect, A](initial: S, p: {State[S]} A): G[Pair[A, S]]
def provide[X, G[_] ~ Effect, A](x: X, computation: {Dep[X]} A): G[A]
```

The `type XxxCarrier` declarations and the platform layer's `data` + instances are untouched —
they are the representation the pinned row denotes. The raw accessors (`runThrow` & co.) still
merge against the jvm-generated data-field accessors: the pinned row desugars in `core`, *before*
the module merge, to the identical application structure, so `signatureEquality` holds.

### Display form

`GroundValueRenderer` (LSP hover) renders a recognized full carrier application as its pinned
row, flattening nested partial carrier applications into entries:
`ThrowCarrier[String, IO, String]` displays as `{Throw[String] | IO} String`. Checker errors are
spelling-free ("Type mismatch.") so hover was the only leak.

## Progressive disclosure (the usage model)

1. **App code**: bare open rows, nothing changed.
2. **Storing an effectful value**: learn one thing — rows have a base; `| Id` when only pure
   control effects (`Abort`/`Throw`/`State`/`Dep`) remain. Such a value can provably do no I/O
   (`Id` has no `Suspend`), and — since `Inf` has no canonical carrier either — it is total.
3. **Discharger/handler authors**: nothing new to learn since effects-v5 step 2 — a parameter row is what you
   supply (`{Throw[E]} A`), and the stack over your own carrier is what it means.

Level 1 gained one thing since effects-v5 §7: a row can be **named** (`type Web = {Console, Log}`), and the name
is expanded wherever a row entry uses it. That is aliasing the *row*, not the computation, so it introduces no
base, no stack, and nothing from this document — a row alias is not a type.

## Limits and deferred work

- **Suspend-riding effects cannot be pinned** (v1): `Console`/`Log` have no canonical carrier of
  their own, so `{Console | X}` fails loudly at resolve time (no `ConsoleCarrier`). The designed
  extension is (a) an abstract base-layer `type Suspended[A]` aliased per target (jvm:
  `= IO[A]`), the nameable platform base — the same commitment the synthetic entry already makes
  grounding `main` — and (b) treating carrier-less effects left of `|` as *constraints on the
  tail* rather than layers. Neither is needed until a real use case stores platform actions in a
  row-stating field (payload-indifferent generic containers already store effectful functions
  fine).
- The `<Ability>Carrier` convention extends to user-defined effects automatically (colocate a
  carrier `data` with the ability); an effect without one fails at resolve, never silently.
