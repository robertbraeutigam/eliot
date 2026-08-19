# Reflection Reifies Code, Not Data: `foldNamedValues`

Status: **implemented**. `namedValues` stays, as the `List` sugar over the fold.

## 1. The question

Compile-time reflection gathered the program's same-named top-level values into a `List[V]`
(`eliot.compiler.Reflect`). That is enough for a registry of plain data — `PluginRegistry.els` sums an `Int` per
plugin — and not enough for the thing a registry is most wanted for: a **test suite**, where each gathered value is
a computation.

> Would a *fold* be a better generalisation than a `List`?

## 2. Why the `List` cannot work

Two rules of *Effects Are a Channel* meet, and neither is negotiable:

- **§1 rule 3** — open rows never appear in types; only a *pinned* row puts a computation in a type. So the element
  type `V` of a `List[V]` must be a concrete, storable type.
- **§1 rule 4** — `append`/`prepend`'s element slot is a **plain generic**, which is a payload, always. A rowless
  slot may not receive a computation.

So a gathered computation lands in a payload slot whatever `V` is claimed, and the two ways out close on each
other. With the collecting definition declaring nothing, the effect is charged where it is gathered:

```eliot
def allTranscripts: String =
   namedValues[Recorded[Unit]]("check").foldLeft("", e -> acc -> acc ++ transcriptOf(e))
```
```
error: This value performs the effect 'Console' but does not declare it
```

`RowChecker.fixesCarrier` reads *declarations*, and `prepend`'s slot is declared `A`, so the fake-carrier deferral
(`docs/testing-effects.md` L2) cannot see the concrete `[Recorded[Unit]]` written at the call. Declare the effect
to satisfy it, and the definition acquires an ambient carrier — at which point the elaborator writes every
carrier-generic callee at *that* carrier, the gathered values included:

```
error: Type mismatch. Expected: IO[Recorded[Unit]] / Actual: IO[Unit]
```

That is not a gap to close; it is the design working. A list is data, a stored row must be pinned, and a pinned row
fixes the carrier **at the point of storage** — which is exactly the freedom a test needs to keep, since it is the
*runner* that should decide what interprets a test.

What the `List` therefore reaches is real but narrow: values pinned to a concrete stack over `Id`
(`data TestCase(name: String, body: {Throw[E] | Id} Unit)`), i.e. pure tests with pure control effects. No
`{Console}`, no `{File}`, no test run on a carrier chosen by the runner.

## 3. The answer: hand back a call, not a value

Reflection should reify the enumeration as **code**. The fold is the fixed, sound shape of that:

```
foldNamedValues(name, initial, combine)  ⤳  combine("Mod::v₁", v₁, combine("Mod::v₂", v₂, initial))
```

Each gathered value is an ordinary **argument**, so it lands in whatever slot `combine` *declares*, and the
ordinary elaboration machinery answers for it with no new mechanism:

- an **open row** (`test: {Effect} Unit`) runs it on the caller's carrier;
- a **supplied row** (`test: {Throw[E]} Unit`, pinned to the algebra's own carrier by the desugar) lets the
  algebra discharge it —
  the derived discharge stack `stack(callee.declaredRow ∖ ambient.declaredRow) over ambient` is exactly what the
  call site needs;
- a **concrete carrier** (`program: Recorded[Unit]`) fixes it to a test double, and `fixesCarrier` now sees a
  declared slot, which is what it was built to read.

Each element is elaborated and monomorphized independently, so gathered values may differ in **row** *and* in
**type** — `combine[V ~ Show](name, v, acc)` gathers heterogeneous values sharing an ability, which one `List[V]`
cannot hold. This is the Use-Site Verification cornerstone applied to reflection: every splice is a use site.

Nothing about effects enters the rewrite. It runs where it always did — after `operator`, before `termination` and
`row` — and emits ordinary code the elaborator reads through its §3.2 whitelist. There is no new phase, no carrier
inference, and no compiler-side knowledge of what a test is.

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
- **`combine` must be a declared value, not a lambda.** The row elaborator decides capture-vs-run from a *callee's
  declaration* (`EffectRow.parameterEffects` / `pinnedParameterIndices`, aligned to a signature's parameters); a
  lambda has none, so its parameters are rowless payloads and every gathered computation would run eagerly at the
  fold. This is a hard error, for the same reason the literal-name rule is one: reflection is a syntactic rewrite,
  not a value.

## 6. What it costs

- The intrinsic's declared signature is a fiction (`[B, C]`, with `C` the algebra): no single Eliot signature can
  state "arity- and row-polymorphic per element". It is replaced before the checker ever sees it, exactly as
  `namedValues` already was.
- The whole emitted chain is attributed to the call's `name` position, so a mismatch in one gathered value is not
  distinguished from another there. The algebra's own declaration is where such a mismatch is read.
- The expansion nests once per gathered value, so a very large suite grows monomorphization work — the same
  property the `append` chain always had.
- A runner must declare the union of the rows its tests perform, unless the algebra pins and discharges them. That
  is rule 4 doing its job: the fold call site is the user's own code, so the declaration belongs there.
- `namedValues` can still reach itself (`TODO.md`); the fold neither worsens nor fixes it.

## 7. Evidence

Every row was run against the tree.

| Claim | How |
| --- | --- |
| A `List` cannot gather a computation, with nothing declared | `namedValues[Recorded[Unit]]("check")` in a pure definition ⤳ "performs the effect 'Console' but does not declare it" |
| …and cannot with the effect declared either | the same with `{Console}` on the collector ⤳ `Expected: IO[Recorded[Unit]] / Actual: IO[Unit]` |
| What a `List` *can* gather | `namedValues[TestCase]("testCase")` over a `{Throw[E] \| Id}`-pinned body, three modules, compiled and run: `PASS A` / `FAIL B: expected 'x' but was 'y'` |
| An open-row slot runs gathered tests on the caller's carrier | `NamedValuesIntegrationTest`, and the hand-written expansion `step("A", checkA, step("B", checkB, done))` |
| A pinned slot discharges per test, with rows differing per test | `examples/src/TestSuite.els` — `{Throw[String]}` and `{Console, Throw[String]}` tests in one suite, runner declaring only `{Console}` |
| A concrete-carrier slot fixes gathered code to a test double | the fold's `collect(name, program: Recorded[Unit], acc)` in a carrier-free definition, run: `GreetTest::test -> hello;` |
| Heterogeneous element *types* through one algebra | `render[V ~ Show](name, v, acc)` over an `Int` and a `String`, `NamedValuesIntegrationTest` |
| The sugar is unchanged | `PluginRegistry.els` still prints `60`; the collected order is asserted in `NamedValuesIntegrationTest` |
| Every fail-safe is a located error | non-literal name, lambda algebra, under-applied call, bare reference — `NamedValuesRewriteProcessorTest` and `NamedValuesIntegrationTest` |

## 8. Found on the way, not fixed here

An open row nested in a type argument **crashes the compiler** instead of being rejected:

```eliot
def gathered: List[{Console} Unit] = empty
```
```
IllegalStateException: BlockExpression should not exist after block desugaring
```

§1 rule 3 says this can never be a type, so it deserves a located diagnostic ("an open row may not appear in a
type; pin it with `| G`"). It is the first thing someone hits when trying to store an effectful value, and it is
independent of the fold.
