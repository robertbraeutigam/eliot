# List combinators (`eliot.collection`) — Design

Status: IMPLEMENTED (base layer, jvm). Grows `eliot.collection.List` from the three primitives it
shipped with (`empty` / `append` / `foldLeft`) into a usable combinator set, and settles the question
that prompted it: whether grouping needs a `Map`.

## What shipped

- `lang/eliot/eliot/collection/List.els`: `prepend` — the one new *primitive*, declared abstractly
  beside `empty`/`append`/`foldLeft`.
- `stdlib/eliot/eliot/collection/List.els`: `map`, `filter`, `reverse`, `isEmpty`, `find`, `groupBy`
  (+ its private `addToGroup` step) — all **platform-independent bodies** over `foldLeft`, joining
  `foreach` and `Combine[List[A]]` which already lived there.
- `jvm/src/.../NativeImplementation.scala`: the `prepend` native (an `ArrayList` copy + `add(0, …)`,
  mirroring `append`).
- `stdlib/eliot/eliot/lang/Pair.els` + `jvm/eliot/eliot/lang/{Pair,Option}.els`: `pair`, `some`,
  `none` — the abstract value constructors the base layer was missing (see decision 3).
- `lang/src/.../row/RowChecker.scala`: the row-polymorphic-callback fix (see decision 4).
- Tests: `ListIntegrationTest` (11 new cases), `BaseConstructorsIntegrationTest`,
  `EffectCorpus.rowPolymorphicCallbackProgram`.

## The decisions

**1. `groupBy` returns an association list, not a `Map`.** Its type is
`List[Pair[K, List[A]]]`, with keys compared by `Eq[K]`.

The tempting alternative — a native `Map` backed by `java.util.HashMap` — is wrong here for a reason
that has nothing to do with convenience: **a Java map keys on `hashCode`/`equals`, and Eliot `data`
types compile to generated classes with identity semantics.** Grouping by a user-defined key would
then silently produce one group per element while the program's own `Eq[K]` instance says they are
equal. That is a wrong answer, not an error, and the base layer must not have one
(`feedback_gaps_must_be_failsafe`). An association list uses `Eq[K]` by construction and cannot
drift from it.

Two further reasons point the same way. **Ordering**: groups come out in first-occurrence order and
each group keeps its elements' original order, so the same list always groups the same way — a hash
table's iteration order is unspecified, which is a poor fit for a language aiming at reproducible
embedded builds. **Cost**: an association list needs no new abstract type, no per-platform
representation, and no backend surface; it is a `foldLeft` body in the base layer, so every future
platform gets `groupBy` for free.

A `Map` is still worth having eventually — but driven by *lookup*, not by `groupBy`'s return type,
and with its own design questions settled deliberately: which key ability (`Eq` + association,
`Compare` + sorted tree, or a new `Hash`), what ordering it guarantees, and whether a platform may
use a native map at all given the equality-coherence problem above (probably not — it likely ends up
an Eliot `data` structure over `Compare[K]`). When it lands, `groupBy` gains a `toMap` sibling and
nothing here changes.

**2. `prepend` is a primitive, because `reverse` cannot be derived cheaply without it.** A left fold
extends a list at the back, so with only `append` the cheapest `reverse` is a concatenation per
element — `O(n³)` once `Combine`'s own fold is counted. The functional alternative (fold into a
`List[A] => List[A]` and apply it to `empty`) does not survive the effect machinery: `foldLeft`'s
accumulator is `{Effect} B`, so a function-typed `B` meets `Id[List[A] => List[A]]` against
`List[A] => List[A]` and the inner lambda's type cannot be inferred either. `prepend` makes `reverse`
a one-line fold at `O(n²)` — the same cost profile as every other combinator here, since `append`
already copies.

**3. The base layer can now *build* an `Option` and a `Pair`, not only take them apart.** A `def`
name may not be upper-case, so a platform's generated `Some`/`None`/`Pair` constructors are not names
the abstract base can declare; `foldOption`/`foldPair` were declared there but no constructor was, so
`find` and `groupBy` would have had nowhere to live. The fix is the ordinary layer mechanism: a
lower-case abstract `def` in the base (`some`, `none`, `pair`), bound in the platform layer by a
one-line Eliot body (`def some[A](value: A): Option[A] = Some(value)`).

`some`/`none` were in fact *already declared* abstractly and never implemented, so any use of them was
a hard "Function not implemented" — a latent break, now closed and pinned by
`BaseConstructorsIntegrationTest`.

**4. Every callback is effect-transparent.** `map`, `filter`, `find` and `groupBy` all take their
callback with a row in the arrow codomain (`f: A => {Effect} B`), exactly as `foreach` already did.
A pure callback fits — the empty row is a legal row — and costs nothing for the possibility. There is
no short-circuiting anywhere in the language (`&&` does not short-circuit either), so `filter` applies
its predicate to every element and the doc says so. `find` is the one exception, and it falls out
structurally rather than by design: its predicate sits in `foldOption`'s *suspended* `ifNone` arm, so
it stops being applied once something is found, even though the fold still walks the whole list.

Making this work needed a compiler fix, in `RowChecker.parameterEnvironment`. A row-polymorphic slot
declares the row *variable* ρ, whose entries are exactly the ones the pre-monomorphization derivation
cannot name — and the derivation dropped the machinery entry, turning "nothing I can name" into the
claim "nothing". Both questions the derivation answers then came out wrong for every such definition:
`f(e)` read as pure, so §1 rule 1's hoist never happened and the computation was passed inline to a
payload slot (a hard rule-4 error); and the position settled `ρ := {}`, so the whole call was written
at `Id` and an effect landed on a carrier that cannot perform it. Keeping the machinery entry inside
the derivation — and dropping it from the *derived* row at `RowChecker.checkValue`, exactly as
`declaredRow` drops it from the declared one — fixes both with one row rather than a second, parallel
predicate. The verification vocabulary is unchanged: `derived ⊆ declared` is still decided in user
abilities on both sides.

## What is deliberately not here

*(as of the first round; three of these four were reversed — see "Round two" below.)*

- **`Map`** — see decision 1.
- **`head`/`tail`/indexing** — a `List` with no cheap front access has no honest `tail`; revisit with
  the representation, not before.
- **`size`** — the refinement channel's `size` domain for containers is still blocked on container
  propagation (`project_refinement_channel_idea`), and shipping an unrefined `Int` count now would
  have to be re-typed later.
- **`sortBy`** — needs `Compare[K]` and a non-recursive sort, which is a native or a fold over a
  different structure; a separate piece of work.

## Round two — the operations `eliot.build` had to write for itself

Status: IMPLEMENTED. `eliot.build` (the build tool written in Eliot) had accumulated a private
`eliot.build.Lists` and `eliot.build.Strings` holding twenty-odd operations whose own documentation
said "each is a candidate to move [into the standard library] unchanged". They are now in the
standard library and both private modules are deleted.

**Added to `stdlib/eliot/eliot/collection/List.els`** — every one a platform-independent body over
`foldLeft`, no new native: `size`, `singleton`, `head`, `last`, `at`, `tail`, `dropLast`,
`replaceLast`, `any`, `all`, `flatMap`, `flatten`, `sorted`, `insert`, `joined`, `lookupOption`,
`lookup`, `put`. Plus two leaves that are `java.lang.String` calls, `split` and `words` (below).
**Added to `eliot.lang.Option`**: `mapOption`. **Added to `eliot.lang.String`**: `before`, `after`,
`isInteger`, `parseInt`.

### This reverses three of the four "deliberately not here" bullets

Recorded as a reversal, not as a refinement — the bullets above stand as what was decided the first
time round.

- **`head`/`tail`/indexing (`at`) — reversed.** The original objection was honesty: a list with no
  cheap front access has no honest `tail`. That is true and unchanged; what changed is the judgement
  that *omission* is the honest response to it. Every one of these is a `foldLeft` that walks the
  list, which is the same `O(n)`-with-a-copy profile `append` and `Combine` already have — so the
  cost is the API's cost, not a surprise this one operation introduces. They say so in their own
  documentation instead (`at`: "positional access is a walk, not a jump"). Leaving them out did not
  stop anyone from needing them; it only moved the same folds into every program.
- **`size` — reversed.** The original objection was that an unrefined `Int` count "would have to be
  re-typed later". It would not: since `Int` became nullary with its bounds in the **refinement
  channel** (`docs/bounds-as-refinements.md`), a container-size domain would sharpen `size`'s
  *meta-information*, not its signature — `def size[A](list: List[A]): Int` is what it stays. The
  concern was about a world where bounds were type parameters, and that world is gone.
- **`sortBy` — resolved rather than reversed.** What shipped is `sorted[A ~ Compare]` and its step
  `insert`: the "non-recursive sort" the bullet asked for turns out to be an ordinary insertion sort
  carried through the fold, quadratic and documented as such. `sortBy` (sorting by a computed key)
  is still not here.
- **`Map` — unchanged.** Decision 1 stands. `lookupOption`/`lookup`/`put` are association-list
  operations over the shape `groupBy` already answers with; they are what makes that shape usable,
  and they use `Eq[K]` by construction for exactly the reason decision 1 gives.

### `split` and `words` live in `eliot.collection.List`, not in `eliot.lang.String`

They are string operations, and that is where they belong by subject. They cannot go there: name
resolution is per-file and an explicit import is *strict*, so `eliot.lang.String` — which declares
`isEmpty` — may not import `eliot.collection.List`, which declares one too. Rather than rename a
published operation or weaken the shadowing rule, they sit beside `joined`, their exact inverse. The
placement is recorded in both modules' documentation so it reads as a decision rather than an
accident.

### `parseInt` and the totality its guard demands

`parseInt(s): {Abort} Int` is spelled the way `indexOf` is: a total private leaf plus a public guard,
`if(s.isInteger, parseIntInternal(s))`. The leaf **must** be total, and this is the trap worth
recording: `if` is an ordinary strict function (`fold(condition, value, abort)`), so a pure arm is
*built* before the condition selects — a `parseIntInternal` that threw on a malformed numeral threw
before the guard could reject it. It therefore answers zero there, which `parseInt` turns into an
abort, so the sentinel is unreachable through the public name. Both realisations — the JVM leaf and
its compile-time twin in `StringReductions` — implement the same total function against the same
pattern (`[+-]?[0-9]+`, exactly what `java.math.BigInteger` accepts), so a numeral parses identically
while checking and while running.

### `List.flatMap` shadows the `Effect` ability's bind

`eliot.carrier.Effect` declares `flatMap` (and `map`, which `List` has collided with since round
one). A file importing both `eliot.carrier.Effect` and `eliot.collection.List` now resolves a bare
`flatMap` to the list one. This is a compile error at the use site, never a silent miscompile, and
`eliot.carrier` is machinery a program is not meant to import — the integration-test programs that
did so were hand-writing a discharge that `catch` expresses directly, and now use it (which also
removed the import). Recorded because the same shape will recur for any list operation named after a
carrier method.
