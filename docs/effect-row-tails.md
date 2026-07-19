# Effect-Row Tails: Pinned Rows as the One Spelling of a Carrier Stack

Status: **SHIPPED** (parser + core desugar + stored-row rule + stdlib sweep + hover rendering).

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
{Throw[E] | G} A      -- generic tail: for discharger/handler authors, G a type parameter
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

### Negatives cannot be pinned

`{-Abort | G} A` is rejected ("Negative effects cannot appear in a pinned effect row.") — a
discharge marker describes a function's behaviour, not a type's shape. Discharger signatures keep
the negatives-only row on their *return* type (`{-Abort} G[A]`), unchanged.

### Stdlib sweep

Every discharger signature now spells its input as a pinned row over its generic base — the
carrier data-type names are gone from all `def` signatures:

```eliot
def runThrow[E, G[_], A](obj: {Throw[E] | G} A): {-Throw[E]} G[Either[E, A]]
def catch[E, G[_] ~ Effect, A](computation: {Throw[E] | G} A, onError: E => A): {-Throw[E]} G[A]
def else[G[_] ~ Effect, A](computation: {Abort | G} A, fallback: G[A]): {-Abort} G[A]
def runStateToPair[S, G[_], A](p: {State[S] | G} A, initial: S): {-State[S]} G[Pair[A, S]]
def provide[X, G[_], A](x: X, computation: {Dep[X] | G} A): {-Dep[X]} G[A]
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
3. **Discharger/handler authors**: generic tails (`{Throw[E] | G} A`), the same notation.

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
