# Total Meta Transfers — meta as a shadow logic

Status: **design, partly built.** Four pieces have landed: the R2 check itself
(`monomorphize/channel/MetaTransferAccountingProcessor`, registered but **dormant** — see §9 P1/P2), the
range domain's top (`whole`, §5), the removal of the last *global* top (the generic `Meta[Bound[D]]`,
§5's third amendment) — so every top is a domain top and the only ⊤ left is the machinery's own untotality,
which arming R2 is what removes — and the backend's result-edge re-encode with the **first stated transfer in the
tree** (below). Everything from §6 on — the meta interpretation — is still design.

The gate that stood in front of arming — the backend's missing result-edge re-encode
(`docs/string-length-meta.md` §8, S3), which the first stated transfer tripped — is **closed**, and with it the first
transfer in the tree is stated: `def length(s: String): Int {size(s)}`. What is left before arming is stating the
rest (§P2), not building anything.

A **second domain has since landed** — `type String {size: Interval[BigInteger]}` with its literal seed
(`docs/string-length-meta.md`, S1+S2) — which changes nothing here structurally (a nullary type's meta is a plain
one-slot structure; §2.3's *structural* map is still owed by `List`) but does enlarge what arming R2 costs: every
body-less `String`-returning leaf now produces a meta-carrying type and so joins §P2's list. That is the sequencing
that document argues for and this one accepts: state those transfers once, in its S4, and arm R2 after.

Supersedes the TODO item *"A native that produces a meta-carrying type must state its meta-information"* by
generalising it: the TODO closes one hole, this closes the class of holes it belongs to.

Prior art: the refinement channel (`Int`'s `range` slot, `^Meta` transfer companions, `^Where`
preconditions, `Meta[D]` join) — shipped; see `monomorphize/channel/RefinementChannelProcessor` and the
`project_refinement_channel_idea` / `project_generic_refinement_merges` memories.

---

## 1. The proposal

1. Metas are **total on types** — every type has a meta counterpart.
2. A **native must state** its meta transfer; omitting it means "`Unit`", and it is a compile error if the
   return type's meta is not actually `Unit`.
3. A **non-native derives** its transfer, always; stating one on a non-native is an error.
4. Therefore there is never an "undefined" meta: meta is a **full shadow logic running behind each
   function**.

This is right, and it is what the channel was already heading towards. Today the flow analysis is
intra-procedural, so every call boundary is ⊤, and the only reason arithmetic narrows at all is that
`Numeric[Int]`'s methods happen to be leaves carrying a brace. Precision is currently a property of *where
code sits relative to a native*, which is not a principle. Totality makes it a property of what the code
*is*.

**It should also come out smaller than today.** §7 is the argument for that, and §5 is the one decision
that determines whether it does.

---

## 2. The invariant

> **Closure.** Every named value has a meta transfer.

By induction:

1. **Base.** Every **leaf** — a value with no Eliot body anywhere on the path — states its transfer.
2. **Step.** Every other value derives its transfer from its body, whose calls all have transfers by (1)
   or (2).
3. **Termination.** *Total by Default*: the recursion gate rejects every cycle in the runtime-body
   reference graph, so the derivation walks a DAG.

Point 3 is why Eliot can have this and most languages cannot: the shadow is a homomorphic image of a
program already known to be acyclic. No fixpoint iteration, no widening, no ascending-chain condition —
**except at leaves**, which is exactly where the loops live (`foldLeft`, `forever`) and exactly where a
statement is demanded. The rule and the language's shape agree.

### 2.1 The leaf boundary is the true native leaf

The compiler pool scans *both* mount pools — `PathScanner`, `Platform.Compiler`: "the compiler track sees
the runtime track completely plus its own override overlay". So it reads the `jvm` layer's Eliot bodies
like any other, and derivation walks straight through them. A leaf is therefore what the plain reading
says: **no Eliot body on any mount** — a Scala/bytecode native, the bottom the native-leaf fail-safe
already guards.

Two consequences worth naming:

- **A leaf's transfer is *usually* platform data**, contributed by the layer that owns the native, next to its
  native binding — the same shape as `add` having a per-platform realisation, and the reason a platform leaf
  almost always has a *true, useful* bound to state rather than "don't know" (§5).

  > **Amended by S3: not always, and the example this bullet used is the exception.** It read
  > "`String::length` is `0 .. 2³¹-1` on the JVM and something else on an ATtiny". That was true of the bound
  > this document expected `length` to state, and false of the bound it actually states: `{size(s)}` — the
  > string's own tracked size. The difference is that a *representation* bound is platform data, while a leaf
  > whose result is an exact function of its argument's meta states that function, and a function can be
  > platform-independent. `length`'s is, because S0 made the unit the code point, so the identity holds on
  > every target and the brace belongs in the **base** declaration.
  >
  > The rule to carry forward is therefore sharper than "transfers are platform data": **a transfer lives in
  > the layer that owns the *fact it states*.** Ask what the bound is a property of — a representation
  > (platform: `Process::exitCode`'s `[0, 255]`, `readLine`'s `[0, 2³¹−1]`) or the argument's own meta (base:
  > `length`, and by the same reasoning `combine`'s `size(a) + size(b)`, §6 of `docs/string-length-meta.md`).
  > §8.2's constraint is unaffected and is what makes this safe: a brace is a *concrete* value, so exactly one
  > layer may carry it — and the second kind of transfer is precisely the kind no platform would want to
  > restate.
- **Derivation may pass through a jvm body but must stop at the native under it.** The compiler pool can
  *read* that body; it cannot *run* the bytecode leaf at the bottom (it stalls loudly). So the leaf under
  the jvm body is the one that must state — which is the same rule, applied at the real bottom.

### 2.2 An ability method declaration is not a leaf

`def add(a: T, b: T): T` inside `ability Numeric[T]` is body-less, but it is not a site that states
anything — it resolves to an *implementation*, and the implementation is either bodied (derives) or a
native (states). This is already how it works: `implement Numeric[Int] { def add(a, b): Int {range(a) +
range(b)} }` is a body-less impl method carrying the brace directly.

It matters operationally: of the ~167 body-less `def`s in the tree, the large majority are ability method
declarations. A leaf predicate that misses the distinction turns enforcement into a hundred spurious
errors.

Open question worth deciding separately: may an *ability* declare a transfer its implementations must
satisfy? That would be a stated contract on a non-leaf — useful, but it is a different feature (a
contract), not part of this.

### 2.3 Meta is total on types — but trivially, today

`metaTypeOf` is already total: `T` ⤳ `T$Meta` when `T` declares meta slots, else `Unit`. What it is not is
**structural**: `List[Int]`, `Option[Int]`, `data Counter(n: Int)` all ⤳ `Unit`. Meta does not follow a
value into a container.

The structural map — `meta(D(f₁: T₁, …)) = D$Meta(f₁: meta(T₁), …)`, meta as a functor on the type
structure — is the natural completion and is what the second refinement domain (`List`/`Array` `size`)
needs anyway. **Out of scope here**, because it multiplies the number of leaves forced to state and needs a
decision on recursive `data` that the size domain should drive.

Consequence to accept: meta is lost at every data boundary, exactly as today.

---

## 3. The four rules, precisely

**R1 — Every type has a meta type.** `metaTypeOf(T)` = `T$Meta` if `T` declares slots, else `Unit`;
non-structural. Carrier-transparent: `metaTypeOf(F[A]) = metaTypeOf(A)` — a computation's meta is its
payload's meta.

**R2 — A leaf must state its transfer.** The existing return brace. Omitting it asserts *"this return's
meta type is `Unit`"*; if it is not, that is an error (§4). A leaf with genuinely nothing to say states so
explicitly (§5).

**R3 — A non-leaf derives, and may not state.** A brace on a value with an Eliot body is an error naming
the fix.

**R4 — There is no undefined meta.** ⊤ is stated or structural, never an omission. The channel may still
*lose* precision, but never because nobody said anything.

`where` is untouched: a precondition is a stated contract, never derived.

---

## 4. Checking R2 is a use-site check

For a monomorphic leaf (`String::length: Int`) the check is static at the declaration. For a generic leaf
(`foldLeft[A, B](…): B`) it cannot be: whether `B`'s meta is `Unit` is known only once `B` is ground. So the
normative form of R2's check is **at the use site, post-mono** — if the callee has no transfer and its
ground return's meta type is not `Unit`, error. That is the *Use-Site Verification* cornerstone, not an
exception to it, and it is fail-safe.

The declaration-time check for the monomorphic case is then a nicety that reports at a better position,
not a second mechanism.

Diagnostics need care: the use-site error must point at the **leaf's declaration** while being raised from
a monomorphization — the reporting shape `EffectAccountingProcessor` already needs.

---

## 5. ⊤: the machinery needs no `unknown` form — but the *domain* did need a top

**Settled, and shipped.** The architecture call below held: nothing was added to `Meta` or to the transfer
machinery. The empirical claim attached to it — that no leaf needs a top either — did not, and the leaf that
broke it is the one §P2 already flagged as doubtful. The resolution is a change to the **domain**, exactly
where this section predicted one would go if it were needed:

```eliot
data Bound[T] = Unbounded | Bounded(value: T)
data Interval[T](start: Bound[T], end: Bound[T])
type Int {range: Interval[BigInteger]}
```

An `Unbounded` endpoint absorbs through `Numeric[Bound[T]]`, and the domain's top is `whole` — the interval
open at both ends. (This shipped in two steps: the top was first a *second* `Bound` wrapped around the slot,
and collapsed into the endpoints once those could express an open end. See the second amendment below.)

> **Amended: `Interval` now has unbounded endpoints too.** The paragraph below argued for a wrapper at the
> slot *instead of* an unbounded endpoint per side, on the grounds that the case that seemed to need them —
> a size — can state `[0, platform max]`. That ground does not hold, and the objection is the layers
> cornerstone: **the base layer may not assume a platform maximum.** Non-negativity of a size is
> platform-independent; its maximum is a property of a representation. With closed-only endpoints an
> abstract declaration has to choose between inventing a maximum it cannot know and stating `Unbounded`,
> which throws away the `≥ 0` it *does* know. So `Interval`'s endpoints are each a `Bound[T]`
> (`data Interval[T](start: Bound[T], end: Bound[T])`), an unbounded endpoint's direction is read from its
> position, and the base states `atLeast(0)` while a platform narrows it to `closed(0, platformMax)`.
> This is the change §5's own last paragraph anticipated — a change to `Interval`, not to `Bound` or
> `Meta` — reached by a different motivation than the §5.2 one it expected.
>
> `Bound` is **kept** at the slot ~~for now~~ — **superseded, see the second amendment below.**
>
> Precision, on the arithmetic: `add` and `subtract` stay **exact** on a half-open interval, because each
> pairs endpoints so an absorbing `Unbounded` lands in the position meaning the infinity it came from, and
> the start/end pairing rules out the indeterminate `∞ − ∞`. `multiply` **widens** to the whole line, since
> its corner products are compared after the multiplication, when a product has lost the position that gave
> its infinity a direction. Sound, not tight — the same trade `Numeric[Bound[T]]` already documents for a
> zero operand, and recovering it needs a signed endpoint view local to that one method.

> **Amended again: the slot-level `Bound` is gone.** With `whole` available, the wrapper added no
> expressiveness — `Bounded(whole)` and `Unbounded` denoted the same thing — so the range domain is now
> `Interval[BigInteger]`, and so is the size domain, each topped by `whole`. The amendment above deferred
> this on two grounds; both are answered rather than waived:
>
> - **Two spellings of the top.** This was the real blocker, and collapsing is what *fixes* it. There is now
>   exactly one structure meaning "nothing bounds this", so a leaf that states the top and a `multiply` that
>   widened into it agree under `ReconcileProcessor.metaByPosition`'s structural comparison instead of
>   cancelling each other to ⊤. Keeping the wrapper was what created the second spelling.
> - **A domain that is not an interval.** Still true, and ~~still served: `Bound[D]` and its `Meta[Bound[D]]`
>   join stay in the tree as the generic top for such a domain~~ — **superseded, see the third amendment.**
>   What changed is that no *shipped* domain is of that kind — both are intervals — so neither pays for a
>   wrapper it does not need.

> **Amended a third time: the generic top is gone, and there is now no global top at all.** The
> `implement[T ~ Meta[T]] Meta[Bound[T]]` instance kept above — `Bound[D]` as the ready-made top of any
> domain `D` — has been deleted (`stdlib/eliot-compiler/eliot/lang/Bound.els`). It was dead code, but it was
> not *only* dead code: it was the last place the machinery offered a top of its own, and keeping it around
> contradicted the thing this document is for.
>
> The principle, stated positively: **every top is a domain top.** A domain says what "nothing bounds this"
> means in its own vocabulary — an interval says `whole` — and the machinery has no opinion, no `top` member
> on `Meta`, and no wrapper that manufactures one. A domain with no interval structure will get a top the
> same way `Interval` did: by growing one, in itself. That is a strictly better position than a generic
> wrapper, for the reason the second amendment already found the hard way — a wrapper is a *second spelling*
> of a top, and `ReconcileProcessor.metaByPosition` compares verdicts structurally, so two spellings of the
> same top read as disagreement and cancel to ⊤. A generic top does not just fail to help a future domain; it
> would hand that domain the bug the range domain had to be rescued from.
>
> `Bound` itself stays, unchanged and load-bearing: it is `Interval`'s endpoint, the thing that makes a range
> half-open. Only its second, aspirational job is gone — the base declaration now says so.
>
> **What this does *not* remove**, and the distinction is the whole of §3's R4: the channel still yields ⊤ at
> a boundary it cannot see through (a companion-free callee, a parameter, a lambda interior —
> `RefinementChannelProcessor`, spelled `None`). That is not a top the *domain* supports; it is the
> machinery's untotality, and it is removed by making the machinery total — R2 armed for leaves (§P2), R3,
> then the interpretation for non-leaves (§P4) — not by deleting a value. Deleting the generic top narrows
> "the meta machinery has a top" to exactly that one honest residue, which is what makes §P2's next step
> legible.
>
> Verified by the byte-identity oracle: all 40 compiling examples produce class content identical to the
> pre-change build, as an unexercised instance must.
>
> Verified by the byte-identity oracle: all 40 compiling examples produce class content identical to the
> pre-change build, since a stated top and a half-open range already decoded to the same ⊤ layout
> (`IntRepresentation.intervalBounds`). The two decoders lost one nesting level; nothing else downstream moved.

The original argument for a slot-level wrapper only, kept because the rest of it still holds: it is a
wrapper at the slot rather than an unbounded endpoint per side: endpoint-level infinities carry more
precision (a half-open `[0, ∞)`), but `Interval` is a runtime user-facing type, and the case that seemed to
need them — a size — states `[0, platform max]`, which is closed *and* keeps its non-negativity. Only a
leaf whose bound is **exponential in its argument's meta** genuinely escapes a closed interval, and there
is exactly one.

This is what R4 asks for. Absence stops being overloaded: a stated top says *"nothing bounds this"*, and
absence goes back to meaning only *"nobody has computed this yet"*. A stated top and an absent meta still
agree on **layout** (both are a bignum — `IntRepresentation.intervalBounds` decodes an open endpoint to
`None`), which is why nothing downstream moved: all 39 example jars were byte-identical across the change,
and all 40 again across the collapse.

The original argument, kept because its shape is still right:

I argued earlier that `Meta[D]` needed a `top`, then that a machinery-level `unknown` transfer form was
needed for `foldLeft`. **Both withdrawn.** Walking the leaves that seemed to need one:

| leaf | honest transfer | needs |
|---|---|---|
| `String::length` | ~~`0 .. platform max`~~ — the parenthesised "better" answer is the one that shipped: **`{size(s)}`**, the string's own size meta, which now exists | nothing |
| `Process::exitCode` | `0 .. 255` | nothing |
| `String::parseIntInternal` | ~~the platform int range~~ — **`Unbounded`** | the domain top (above) |
| `Id::runId`, `Effect::pure` | identity on the payload's meta | nothing |
| `Function::apply`, `Effect::flatMap` | the function argument's transfer, applied | **higher-order** |
| `PatternMatch::handleCases` | join over the cases' transfers | **higher-order** |
| `List::foldLeft` | `combine`'s transfer iterated over `list`'s size meta | **higher-order + §5.2** |

The structural reason there is no gap: **on a real target every meta-carrying type has a representation,
hence a bound.** An `Int`-returning native returns a platform `Int`; an ADC read returns an ADC's width. A
platform leaf that cannot state a true bound is a leaf whose platform has not decided its representation,
which is a different bug. So R2 can be enforced with no escape hatch, and the *absence* of an escape hatch
is what makes it extract the information it exists to extract.

**Where that reasoning fails, and it fails exactly once.** The premise holds for a leaf that *returns* a
representation. It does not hold for one whose result is a **function of another value's meta**, because the
composition can outrun any representation the result is stored in. `parseIntInternal` is
`new BigInteger(String)` — the numeral read at full precision — so its honest bound is `±(10ⁿ − 1)` for an
argument of `n` code points. Two things go wrong at once: `10ⁿ` is not writable (`Numeric` is
add/subtract/multiply, no exponentiation), and at the representation limit `n = 2³¹−1` the honest answer is a
two-billion-digit `BigInteger` — finite, sound, and roughly 890 MB to materialise. A bound that hangs the
compiler is worse than a wide one, so the leaf states `Unbounded` and the exponent never runs.

Adding a `power` native would make the tight cases (`"42"` ⤳ `[-99, 99]`, a `where`-bounded field) genuinely
precise, and it is cheap — its own transfer is endpoint-wise `power` through `Numeric[Interval[T]]`, the same
shape `add` already has. It is a **precision follow-on, not a prerequisite**, and it is only safe *given* a
domain top to saturate into.

`Interval` may still gain unbounded endpoints if its own semantics want them — that is a change to
`Interval`, not to `Bound` or `Meta`, and §5.2 is the only thing that would motivate it. **It has, and
§5.2 was not what motivated it** — the platform-maximum objection above was. The prediction about the
*shape* of the change held exactly: `Bound` and `Meta` were untouched.

### 5.1 Unbounded iteration cannot reach the interpretation

A leaf is **summarized** by its stated transfer, never executed. Only *derived* code is interpreted, and
derived code is acyclic by the recursion gate. So a native's loop is invisible to the interpretation by
construction — `foreverInternal(thunk: Function[Unit, Unit]): Unit` is a leaf, its transfer is `Unit`, and
the interpretation never sees the `while(true)` under it. Robert's observation holds and is load-bearing:
an unbounded loop returns `Unit`, so it can carry no meta.

It holds by construction for `forever` as declared, not as a theorem. I proposed making it one — *an `{Inf}`
leaf may not return a meta-carrying type* — and that rule is **dropped**. It buys nothing and costs something:

- It buys nothing because the guarantee does not come from the return type. It comes from the first sentence
  of this section: a leaf is **summarized, never executed**, so its internal loop is invisible to the
  interpretation whatever it returns. Unboundedness of a loop and the truth of a stated return bound are
  orthogonal.
- It costs something because a blocking native is honestly `{Inf}` and honestly bounded. An ADC or UART read
  that spins until data arrives returns `0 .. 1023`; the rule would forbid stating it, for no gain.

The real guard is the **budget** (§5.2), and it is not redundant with this rule — it is the only thing
standing where iteration actually enters, which is a transfer that *invites* it by calling `iterate`. That is
`foldLeft`'s transfer, never `forever`'s.

Unbounded iteration can therefore only enter the interpretation if we *invite* it — which is exactly what
the fold does.

### 5.2 The fold: executing it is right, and it needs a budget, not an ∞

`list.foldLeft(0, _ -> count -> count + 1)` under B: bind `count` to its meta, evaluate `count + 1`'s
transfer, iterate. The information is all there and B is the only option that can use it — A cannot, since
iterating needs `combine`'s transfer applied to a value the interpretation learns at reduction time, and
A has no way to reach a lambda argument's transfer (§6.1).

Three cases, and the third is the whole question:

1. **Singleton size** `[n, n]` — iterate `n` times, exact.
2. **Size interval** `[lo, hi]` — the result is "after `lo` iterations, or `lo+1`, …, or `hi`": run to `hi`,
   joining each iterate from `lo` on. Exact, and terminates.
3. **Size honestly bounded but large** — `readLines` returns a `java.util.List`, so its size is truthfully
   `0 .. 2³¹-1`. Finite. Not iterable at compile time.

So: **finite ≠ computable at compile time**, and the machinery needs a *budget*, not an ∞. When the budget
is exhausted the interpretation yields absence — sound ("I know nothing" is always true), reportable, and
never wrong.

This is what preserves R4 rather than breaking it. Absence stops meaning *"nobody stated anything"* — a
modelling gap, which R4 forbids — and starts meaning *"the interpretation did not converge within
budget"*, a resource outcome. Different in kind, and only the first was ever the hole.

The alternative to a budget is a **widening operator** — join the iterates until stable — and *that* is
what would need unbounded endpoints: `[0,0] → [0,1] → [0,2] → …` has no fixed point without ∞. So the
choice is exactly: budget ⇒ no unbounded needed; convergence-by-widening ⇒ unbounded needed. **Recommend
the budget.** It is simpler, it is honest, and Eliot deliberately does not have widening machinery — the
recursion gate was supposed to make it unnecessary, and at every point except this one it does.

One constraint on how the iteration is expressed: **the channel must not name `foldLeft`.** The shipped
invariant is that nothing in the channel or reconcile names a leaf or a branch construct — a transfer is
recognised only as a transfer. So bounded iteration must live in the *transfer language* as a primitive
(`iterate(n, f, x)`, where `n` is a size meta) that `foldLeft`'s own stated transfer calls. One meta-language
leaf, not a channel special case.

All of which still waits on the size domain: until `List` carries a size, `foldLeft`'s transfer has no `n`
to iterate over, the reduction stalls, and the result is absence — soundly, and with no `unknown` form
needed even in the interim (a stalled transfer already drops to absence today).

The real lesson from the table: **the load-bearing problem is higher-order transfers, not ⊤.** Which is §6.

---

## 6. The architecture decision: twin program, or meta interpretation

This is the fork that decides whether the result is simpler than today or merely bigger.

### 6.1 Option A — emit a shadow twin

Derive, in `core` alongside the existing meta desugarers, a `f$Meta` **Eliot** twin of every value:
literals become `integerLiteral$Meta(n)`, calls become `f$Meta(args$Meta)`, lambdas become lambdas over
metas, rows are stripped. The twin is compiled by the ordinary pipeline and reduced by the one NbE
evaluator; the channel's `metaViaCompanion` stays the only recognition point, and simply always hits.

Attractive: pure name transform, no lookup, reuses every front-end phase (operator resolution, `match`
desugaring) for free.

Two problems, one fatal:

- It roughly **doubles the compiler pool**, against a TODO list that already carries three "compilation is
  slow" entries. Mitigable (a static "return meta is `Unit` ⇒ never force the twin" fast path, plus
  demand-driven facts), but not free.
- **There is no node-level correspondence between the twin and the runtime body.** The twin is derived from
  source; the channel walks monomorphized bodies. At a call with a lambda argument, the channel holds a
  `MonomorphicExpression` lambda node and cannot synthesise that lambda's twin from it — so every
  higher-order transfer in the §5 table degrades to unknown, and the channel keeps its whole existing case
  analysis *plus* a second program. That is bigger, not simpler.

### 6.2 Option B — interpret the body under a meta interpretation (recommended)

Do not emit a second program. Extend the channel's existing walk so that a node's meta is computed by
walking the *actual* body with:

- parameters bound to the caller's argument metas,
- **leaves substituted by their stated transfer** (reduced through NbE, exactly as today),
- non-leaves recursively interpreted, memoized on `(vfqn, typeArgs, argMetas)`.

Node correspondence is then free — you are walking the very body whose layout the representation pass
stamps. Higher-order falls out: a lambda node's meta is a *closure over metas*, built during the same
walk and applied when `apply`/`flatMap`/`handleCases` calls it. That single property is what unblocks
every row of the §5 table, including the fold — iterating `combine`'s transfer requires reaching a lambda
argument's transfer, which is precisely what A cannot do.

**Decided: B.**

Costs, honestly: it is per-call-site rather than a compiled summary (memoization is doing real work), and
the interpretation is a Scala walk rather than emitted Eliot. The latter is *not* a "parallel generator"
violation — the channel already owns this walk; this extends it rather than adding a second mechanism. And
the stated transfers themselves stay Eliot, reduced by the one evaluator, so the single-evaluator
cornerstone is intact.

Termination is still the recursion gate's (§2), now applied directly rather than through a derived
program.

**Recommendation: B.** A is the more elegant transform in isolation; B is the smaller *system*, and it is
the only one of the two that makes the channel shrink.

---

## 7. Why this ends up simpler than today

Under B, these disappear:

- The `UnifiedModuleNames` **membership test** for a companion (`metaViaCompanion`, `hasWhereCompanion`'s
  sibling) — with totality there is nothing to test.
- The **"no companion ⇒ ⊤" branch** and with it the notion that a call site's precision depends on whether
  someone happened to write a brace.
- `MetaTransferDesugarer`'s **two-mode signature rule** (a monomorphic vessel suffixes `Int` ⤳ `Int$Meta`; a
  generic one keeps its signature verbatim and is reduced at meta type arguments). One convention replaces
  it, and the `T$Meta` name-suffix hack survives only where it belongs — as the *meta structure's own*
  generated name.
- The **α special case**: an integer literal stops being a distinct arm in `walkFlow` (it is a call to
  `integerLiteral`, whose transfer is stated like any other leaf's).

And one entanglement gets untied, which is the biggest structural win:

> Today one walk decides both *what the meta is* and *what layout is permitted*. They are different
> questions with different answers.

`walkFlow` currently skips lambda interiors and treats parameters as ⊤ — but those are **representation**
policy (the shipped boundary rule: narrow ints widen back to ⊤ at every call/ctor/typeMatch/function-value
edge, because a narrow heap box fails a reader's `CHECKCAST`), enforced inside the *meta* computation
because there is only one walk. Under B they separate: the interpretation computes metas everywhere,
including through lambdas and parameters; the representation side keeps its conservative policy about what
may be *stamped*. Fewer rules, each in one place, and the `CHECKCAST` hazard stops being something the
meta logic has to know about.

**This separation is also the thing most likely to be got wrong.** Letting a call-site parameter meta
narrow a callee's interior means either call-site-sensitive layout or splitting monomorphization on metas
— and the latter is explicitly forbidden by the shipped invariant *"refinements are invisible to monomorph
instantiation identity"*. The interpretation's output is consumed at the **call node in the caller**;
inside the callee, layout stays joined/conservative. Get that wrong and it arrives as a
`ClassCastException`, not a type error.

---

## 8. Remaining difficulties

**8.1 Ability-impl transfers collide.** `metaCompanionFqn` keeps only `callee.name.name`, which *strips*
the `AbilityImplementation` qualifier — so `Numeric[Int]::add`'s transfer is a plain `add^Meta` in
`eliot.lang.Int`. Safe today only because no other `add` is declared there; under totality it collides.
Fix before anything else: name a transfer in the **callee's own qualifier namespace** (mirroring `$Where`),
which preserves impl identity and lets `Qualifier.Meta` retire. Dispatch itself is already solved — the
channel sees the resolved *instance* FQN post-mono.

**8.2 Braces do not merge.** A brace desugars to a companion *with a body*, i.e. a concrete value; if two
layers carried the brace, the merge rejects it with "Has multiple implementations". So R2 must say where
the brace lives — with the **native**, i.e. in the layer that owns the leaf — and a brace must not be part
of `signatureEquality` (the merge is lexical; requiring character-exact repetition across layers would be
a new failure mode for no gain). A body-supplying layer may not carry one: a special case of R3.

**8.3 `foldLeft` narrows nothing until the size domain lands.** `def size[A](list: List[A]): Int =
list.foldLeft(0, _ -> count -> count + 1)` is the first thing anyone will try; its transfer stalls for want
of an `n` (§5.2) and yields absence. Correct, but say so in the user-facing docs or the first report will
be "you said meta was total".

**8.4 The interpretation needs a budget on two axes.** Fold iteration (§5.2) is one. The other is
depth/fan-out: the interpretation recurses into callee bodies, and a diamond-shaped call DAG re-interpreted
per path is exponential even though each path is acyclic. Memoization on `(vfqn, typeArgs, argMetas)`
handles the common case but distinct argument metas defeat it. Both budgets must fail the same way —
absence, never a wrong answer.

**8.5 Memoization key growth.** That same cache key is `(vfqn, typeArgs, argMetas)`, and metas are
arbitrary domain values, so distinct call sites with distinct literal ranges are distinct entries. Bounded
by call-site count, but it interacts with the existing cache-serialization complaints — measure with
`--statistics`, diffing a run with and without the flag (the flag itself inflates ~20%).

**8.6 The `Meta`/metavariable name collision.** `SemValue.VMeta`, `MetaStore`, `TypeStackLoop.returnMeta`
are **unification metavariables**, nothing to do with refinement meta. New code in `monomorphize/` will
read ambiguously. "Meta" is the user-facing word for the refinement side (`ability Meta[D]`), so the
metavariable side is the better rename target — worth doing before this lands more of the other.

---

## 9. Staging

**P1 — the leaf predicate.** "Does this unified value have an Eliot body anywhere on the path?", computed
after the layer merge, excluding ability method declarations (§2.2). Nothing consumes it yet; publish the
list and eyeball it.

**P1/P2 mechanism — LANDED (not yet armed).** The leaf predicate turned out to be exactly the body test the
mono fact already carries — `MonomorphicValue.runtime.isEmpty` (no need for the two-track/`NativeBinding`
detection §2/§3 sketch). The R2 check rides each `MonomorphicValue` as
`monomorphize/channel/MetaTransferAccountingProcessor` (on the `EffectAccountingProcessor` template): a
body-less value whose **declared** return head is a concrete meta-carrying type and which declares no `^Meta`
companion is reported at the value. A **type-parameter return head** (`foldLeftInternal : F[B]`, `runId : A`)
is exempt — the meta is forwarded, not originated, so it is the §6 higher-order case, not this one. Proven
against the real stdlib: armed, it fired on exactly `String::length`, `indexOfInternal`, `parseIntInternal`,
and `Process::exitCode`/jvm `outcomeExitCode`; folds, carrier returns, bodied values, and the brace-carrying
arithmetic leaves all pass. Registered but **undemanded** (dormant) — arming is a one-line `getFactOrAbort`
precondition in `WovenValueProcessor`. `String::length` has since stated its transfer (S3), so it would no
longer fire there — the first item to leave that list by being answered rather than exempted.

**P2 prerequisite — the domain top — LANDED.** The range domain is `Interval[BigInteger]`, topped by `whole` (§5), so
every leaf below now has a transfer it can honestly state. This was P2's one blocker.

**P2 prerequisite — no global top — LANDED.** The generic `Meta[Bound[D]]` top is deleted (§5, third amendment):
tops are domain-owned, the machinery has none, and the only ⊤ left is the channel's own untotality — which is
what arming removes rather than something to state around.

**P2 prerequisite — the result-edge re-encode (S3) — LANDED, and it was the real gate.**
`docs/string-length-meta.md` §8: stating a transfer on a leaf emitted as a *generated native static method* pinned a
narrow meta on the call node while the method's return descriptor stayed the ⊤ bignum, and the class verifier
rejected it (`VerifyError: 'java/math/BigInteger' is not assignable to 'java/lang/Byte'`). It had never fired
because no leaf stated one; `String::length` — the first entry in the table below — fired it immediately. So S3
(`ExpressionCodeGenerator.convertResultFromBoundary`, the mirror of `generateArgumentToBignum`) landed *with* that
first stated transfer, as this entry required, and `String::length` is off the list below. The arithmetic leaves were
invisible to it and remain so: they are inline intrinsics, which already emit at the node's own width — the one shape
the re-encode skips.

**What that settles beyond one leaf.** The re-encode is a function of the node's meta against the one ⊤ boundary
width, not of which callee is being invoked, so it applies to every non-intrinsic application at once — the
over-applied spine, the `Function.apply` bridge and the two `match` calls included. Every leaf P2 still owes a
transfer therefore inherits a backend that already carries it, and this class of `VerifyError` cannot recur per-leaf.
Verified inert where nothing is stated: with the backend change and no brace, all 40 compiling examples were
byte-identical to the pre-change build.

**P2 — arm it + the missing statements (remaining).** Wire the precondition, then state the transfers the
armed check demands. Their bounds are **platform data**, so by the platform-independence cornerstone they
belong in the platform layer, not the base (the brace desugars to a separate `^Meta` companion, so a
jvm-layer brace over a base-abstract declaration merges cleanly with no `signatureEquality` change):

| leaf | stated transfer |
|---|---|
| ~~`String::length`~~ | **stated (S3)**: `{size(s)}` — the string's own size, exact, and better than the `[0, 2³¹−1]` this table first proposed. It is also the one row that belongs in the **base** rather than a platform layer, because since the unit is the code point the identity holds on every target — there is no platform datum in it |
| `String::indexOfInternal` | `Bounded([-1, 2³¹−1])` — `-1` is the not-found answer |
| `Process::exitCode`, jvm `outcomeExitCode` | `Bounded([0, 255])` |
| `String::parseIntInternal` | `Unbounded` — full-precision `BigInteger`, and its honest bound is exponential in its argument's size (§5) |

Plus the generic leaves of §5, which are the higher-order rows and wait on P4. **This is what closes the
original TODO.**

Sequencing note (`docs/string-length-meta.md` §7): declaring `String` meta-carrying pulls every body-less
`String`-returning leaf into this list — roughly a dozen more, all of them statable as
`Bounded([0, platform max])`. Landing the `String` domain *before* arming means stating them once instead of
twice.

**P3 — R3 enforcement.** Error on a brace over a bodied value. Should be a no-op on today's tree
(`Numeric[Int]`, `fold`, `integerLiteral` are all leaves) — a good sign and a good test.

**P4 — the interpretation (§6.2).** The naming fix of 8.1 first, then the walk, then the separation of §7.

**P5 — precision follow-ons.** Structural meta types (§2.3); `where` demanded with known argument metas at
nested calls, which P4 makes possible for the first time.

Each stage verified by the fast example sweep + byte-identity compare
(`reference_verification_harness_recipes`).

---

## 10. Non-goals

- Structural/functorial meta types — the second refinement domain's project.
- Derived `where` preconditions; `where` stays a stated contract.
- Meta-driven monomorphization — refinements stay invisible to instantiation identity.
- A second evaluator. Stated transfers are Eliot on the one NbE evaluator, or this is not worth having.

---

## 11. Decisions needed before code

1. ~~Option B over Option A~~ — **decided: B** (§6.2).
2. ~~A machinery-level `unknown` form~~ — **dropped, and the counterexample arrived** (§5). The machinery
   still has no `unknown` form and `Meta` is untouched, which is the half that mattered. But `parseIntInternal`
   *was* the real counterexample this decision reserved the right to be revisited for, so the **domain** grew a
   top. It first grew one as a slot-level `Bound` wrapper and now spells it `whole`, the both-ends-open interval
   (§5). **Settled and shipped** — and finished off by the third amendment: the generic `Meta[Bound[D]]` top is
   deleted, so *every* top is now a domain top and the machinery has none.
3. **Budget, not widening** (§5.2) — still the recommendation, but the framing has changed. §5.2's argument was
   *budget ⇒ no ∞ needed; widening ⇒ ∞ needed*, and ∞ now exists in the domain, so widening is no longer
   impossible — an ascending chain terminates at `whole`. That is a **removed one-way door**, not a reason
   to switch: the budget is still simpler and more honest, and Eliot deliberately has no widening machinery.
4. ~~An `{Inf}` leaf may not return a meta-carrying type~~ — **dropped** (§5.1). It buys nothing (a leaf is
   summarized, never executed, so its loop never reaches the interpretation whatever it returns) and would
   forbid a legitimate blocking native that is honestly `{Inf}` and honestly bounded.
5. **Brace placement**: with the native, excluded from `signatureEquality` (§8.2).
6. **Scope of v1**: P2 alone, or P2 + P4.
7. **May an ability declare a transfer its impls must satisfy?** (§2.2) — probably yes eventually, but as a
   contract feature, not part of this.
8. **A `power` native** (§5) — worth having for precision once `String` size lands, and safe only now that
   the domain has a top to saturate into. Not a prerequisite for anything.
