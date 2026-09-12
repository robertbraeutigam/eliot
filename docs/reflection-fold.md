# Reflection Reifies Code, Not Data: `foldNamedValues`

Status: **implemented**, and **carried onto effects v6 (2026-09-09)**. `namedValues` stays, as the `List` sugar
over the fold. The conclusion is unchanged and so is §2's first rule; what changed is the *second* half of the
reason, which was written in carrier vocabulary — see the marked paragraphs.

## 1. The question

Compile-time reflection gathered the program's same-named top-level values into a `List[V]`
(`eliot.compiler.Reflect`). That is enough for a registry of plain data — `PluginRegistry.els` sums an `Int` per
plugin — and not enough for the thing a registry is most wanted for: a **test suite**, where each gathered value is
a computation.

> Would a *fold* be a better generalisation than a `List`?

## 2. Why the `List` cannot work

The load-bearing rule is **§1 rule 4**: `append`/`prepend`'s element slot is a **plain generic**, and a plain
generic is a payload, always. So a gathered computation handed to a list constructor *runs where it is written*
(rule 1), whatever `V` is claimed, and the effect is charged to whoever gathers:

```eliot
def allTranscripts: String =
   namedValues[…]("check").foldLeft("", e -> acc -> acc ++ transcriptOf(e))
```
```
error: This value performs the effect 'Console' but does not declare it
```

Declaring the effect does not help: it makes the *gathering* definition perform it, which is the opposite of what
a registry wants. A list holds what the enumeration already produced, and the point of a test suite is that it has
not run yet.

> **Restated for effects v6 (2026-09-09).** The paragraph this replaced argued the second half from carriers —
> "a stored row must be pinned, and a pinned row fixes the carrier at the point of storage" — and from an
> elaborator writing every carrier-generic callee at the ambient carrier. Neither exists now. The conclusion
> survives in the new vocabulary and is if anything sharper: a stored computation is a **thunk whose operations
> were bound where it was constructed** (`docs/effects.md` §1 rule 3). So putting a test in a list still fixes its
> interpretation at the point of storage, which is exactly the freedom a test needs to keep — it is the *runner*
> that should decide what interprets a test. Rule 4's first bullet is unchanged and still does the work; only its
> old second bullet ("a rowless slot may not receive a computation") has no subject, because such an argument
> simply runs there.

What a `List` therefore reaches is real but narrow: values whose interpretation is fixed where they are built. No
test run on an implementation chosen by the runner.

## 3. The answer: hand back a call, not a value

Reflection should reify the enumeration as **code**. The fold is the fixed, sound shape of that:

```
foldNamedValues(name, initial, combine)  ⤳  combine("Mod::v₁", v₁, combine("Mod::v₂", v₂, initial))
```

Each gathered value is an ordinary **argument**, so it lands in whatever slot `combine` *declares*, and the
ordinary elaboration machinery answers for it with no new mechanism:

- an **empty row** (`test: {} Unit`) supplies nothing, so the gathered value's operations are bound by whatever
  the *caller* declares;
- a **supplied row** (`test: {Throw[E]} Unit`) lets the algebra discharge that entry itself, since an entry the
  algebra's own row does not name is supplied by the slot;
- a **slot with a `with`** (`program: {Console} Unit with recordingConsole`) fixes it to a test double, decided by
  the algebra's signature with nothing written at the call.

Each element is elaborated and monomorphized independently, so gathered values may differ in **row** *and* in
**type** — `combine[V ~ Show](name, v, acc)` gathers heterogeneous values sharing an ability, which one `List[V]`
cannot hold. This is the Use-Site Verification cornerstone applied to reflection: every splice is a use site.

Nothing about effects enters the rewrite. It runs where it always did — after `operator`, before `termination` and
`row` — and emits ordinary code the write reads from declarations alone (`docs/effects.md` §3.2). There is no new
phase, no inference of any binding, and no compiler-side knowledge of what a test is.

## 4. What it generalises

The old rewrite **already was a fold**: `append(append(empty[V], r₁), r₂)` is the enumeration folded at the free
monoid. Generalising removes a hard-coded algebra rather than adding machinery.

- `namedValues[V](name)` is recovered exactly, as the fold at `(prepend, empty[V])` — right-folding with `prepend`
  keeps the index's canonical qualified-name order — so nothing is lost and the sugar stays.
- The converse fails whenever an element is not storable. Fold ⊋ List, properly.

## 5. The three decisions

- **The algebra receives the value's qualified name.** It is free at rewrite time (`pkg.Module::value`) and every
  real consumer wants it: a suite needs labels, a registry wants to report what it loaded.
- **Right fold.** `combine(v₁, combine(v₂, initial))` puts the *rest of the fold* in the slot the algebra may
  declare as a row, which is what buys skipping and fail-fast. A left fold would put the already-accumulated prefix
  there instead.
- **`combine` must be a declared value, not a lambda.** The `row` phase decides run-vs-hand-over from a *callee's
  declaration* (`EffectRow.parameterEffects`, aligned to a signature's parameters); a lambda has none, so its
  parameters are rowless payloads and every gathered computation would run eagerly at the fold. This is a hard
  error, for the same reason the literal-name rule is one: reflection is a syntactic rewrite, not a value.

## 6. What it costs

- The intrinsic's declared signature is a fiction (`[B, C]`, with `C` the algebra): no single Eliot signature can
  state "arity- and row-polymorphic per element". It is replaced before the checker ever sees it, exactly as
  `namedValues` already was.
- The whole emitted chain is attributed to the call's `name` position, so a mismatch in one gathered value is not
  distinguished from another there. The algebra's own declaration is where such a mismatch is read.
- The expansion nests once per gathered value, so a very large suite grows monomorphization work — the same
  property the `append` chain always had.
- A runner must declare the union of the rows its tests perform, unless the algebra supplies and discharges them.
  That is rule 4 doing its job: the fold call site is the user's own code, so the declaration belongs there.
- `namedValues` can still reach itself (`TODO.md`); the fold neither worsens nor fixes it.

## 7. Evidence

Every row was run against the tree when this landed; the rows marked **v5** were argued in carrier vocabulary and
their *evidence* is historical, though the claim above each still holds (§2).

| Claim | How |
| --- | --- |
| A `List` cannot gather a computation | `namedValues[…]("check")` in a pure definition ⤳ "performs the effect 'Console' but does not declare it" |
| …and declaring the effect does not rescue it | **v5**: `Expected: IO[Recorded[Unit]] / Actual: IO[Unit]`. Under v6 the gathering definition simply performs the effect, which is the wrong thing for a registry to do |
| A `{}`-rowed slot leaves gathered tests to the caller's declarations | `NamedValuesIntegrationTest`, and the hand-written expansion `step("A", checkA, step("B", checkB, done))` |
| A supplying slot discharges per test, with rows differing per test | `examples/src/TestSuite.els` — `{Throw[String]}` and `{Console, Throw[String]}` tests in one suite, runner declaring only `{Console}` |
| A slot fixes gathered code to a test double | **v5** as a concrete carrier; under v6 the same thing is a `with` on the slot's type, which `eliot-test`'s `onConsole` does for its doubles at once |
| Heterogeneous element *types* through one algebra | `render[V ~ Show](name, v, acc)` over an `Int` and a `String`, `NamedValuesIntegrationTest` |
| The sugar is unchanged | `PluginRegistry.els` still prints `60`; the collected order is asserted in `NamedValuesIntegrationTest` |
| Every fail-safe is a located error | non-literal name, lambda algebra, under-applied call, bare reference — `NamedValuesRewriteProcessorTest` and `NamedValuesIntegrationTest` |

## 8. Found on the way — **FIXED 2026-09-09**

A row nested in a **type argument** crashed the compiler instead of being rejected:

```eliot
def gathered: List[{Console} Unit] = empty
```
```
IllegalStateException: BlockExpression should not exist after block desugaring
```

The cause is one line up from where it looked: a type argument is parsed by the **value** expression parser (types
are values), where a leading `{` is always a block — so `{Console}` was read as a block juxtaposed with `Unit`, the
effect-row parser never saw it, and `BlockDesugaringProcessor` lowers blocks only in a value's *runtime body*.

It is a located error now (`core/processor/SignatureBlockChecker`), pointing at the brace:

```
A `{ … }` block may not appear in a type. An effect row is written on a definition's return type,
or on a parameter's or field's type — never inside a type argument.
```

**Reported rather than lowered, deliberately.** Desugaring signatures too would make it *compile*: `{Console} Unit`
lowers to `(_ -> Unit)(Console)`, which evaluates to `Unit`, so `List[{Console} Unit]` would silently mean
`List[Unit]` — an effect annotation quietly discarded, which is exactly the failure direction standing rule 8
forbids.
