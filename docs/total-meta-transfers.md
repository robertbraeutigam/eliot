# Total Meta Transfers — meta as a shadow logic

Status: **design proposal**, no code. Supersedes the TODO item *"A native that produces a meta-carrying
type must state its meta-information"* by generalising it: the TODO closes one hole, this closes the class
of holes it belongs to.

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

- **A leaf's transfer is platform data**, contributed by the layer that owns the native, next to its native
  binding. `String::length` is `0 .. 2³¹-1` on the JVM and something else on an ATtiny. That is not a wart;
  it is the same shape as `add` having a per-platform realisation, and it means a platform leaf almost
  always has a *true, useful* bound to state rather than "don't know" (§5).
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

## 5. ⊤: the machinery needs neither a domain `top` nor an `unknown` form

I argued earlier that `Meta[D]` needed a `top`, then that a machinery-level `unknown` transfer form was
needed for `foldLeft`. **Both withdrawn.** Walking the leaves that seemed to need one:

| leaf | honest transfer | needs |
|---|---|---|
| `String::length` | `0 .. platform max` (better: the string's own size meta, once it exists) | nothing |
| `Process::exitCode` | `0 .. 255` | nothing |
| `String::parseIntInternal` | the platform int range | nothing |
| `Id::runId`, `Effect::pure` | identity on the payload's meta | nothing |
| `Function::apply`, `Effect::flatMap` | the function argument's transfer, applied | **higher-order** |
| `PatternMatch::handleCases` | join over the cases' transfers | **higher-order** |
| `List::foldLeft` | `combine`'s transfer iterated over `list`'s size meta | **higher-order + §5.2** |

The structural reason there is no gap: **on a real target every meta-carrying type has a representation,
hence a bound.** An `Int`-returning native returns a platform `Int`; an ADC read returns an ADC's width. A
platform leaf that cannot state a true bound is a leaf whose platform has not decided its representation,
which is a different bug. So R2 can be enforced with no escape hatch, and the *absence* of an escape hatch
is what makes it extract the information it exists to extract.

`Interval` may still gain unbounded endpoints if its own semantics want them — that is a change to
`Interval`, not to `Meta`, and §5.2 is the only thing that would motivate it.

### 5.1 Unbounded iteration cannot reach the interpretation

A leaf is **summarized** by its stated transfer, never executed. Only *derived* code is interpreted, and
derived code is acyclic by the recursion gate. So a native's loop is invisible to the interpretation by
construction — `foreverInternal(thunk: Function[Unit, Unit]): Unit` is a leaf, its transfer is `Unit`, and
the interpretation never sees the `while(true)` under it. Robert's observation holds and is load-bearing:
an unbounded loop returns `Unit`, so it can carry no meta.

It holds by construction for `forever` as declared, not as a theorem. Worth making it one: **an `{Inf}`
leaf may not return a meta-carrying type.** Cheap to check, and it turns "true of today's `forever`" into
"true of every future unbounded native".

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

**P2 — R2 enforcement + the missing statements.** Error on a leaf whose ground return meta type is not
`Unit` and which states nothing; then fix what it uncovers — `String::length`, `indexOfInternal`,
`parseIntInternal`, `Process::exitCode`, jvm `outcomeExitCode`, and the generic leaves of §5.
**This alone closes the original TODO**, and is worth shipping even if nothing else follows.

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
2. ~~A machinery-level `unknown` form~~ — **dropped** (§5): no leaf appears to need it, and a stalled
   transfer already yields absence soundly. Revisit only if P2 turns up a real counterexample.
3. **Budget, not widening** (§5.2) — confirm; it is what lets the machinery stay free of ∞.
4. **An `{Inf}` leaf may not return a meta-carrying type** (§5.1) — turns a property of today's `forever`
   into an enforced rule.
5. **Brace placement**: with the native, excluded from `signatureEquality` (§8.2).
6. **Scope of v1**: P2 alone, or P2 + P4.
7. **May an ability declare a transfer its impls must satisfy?** (§2.2) — probably yes eventually, but as a
   contract feature, not part of this.
