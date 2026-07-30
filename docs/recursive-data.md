# Recursive Data: The Fold Is a Derived Eliminator

Status: **DESIGN ONLY** — no implementation. Nothing in this document is built yet.

## The problem this solves

Recursive `data` already declares, constructs and pattern-matches. All three of these compile today:

```eliot
data Tree[A] = Leaf | Node(left: Tree[A], value: A, right: Tree[A])   -- self-recursive
data Expr = Lit(text: String) | Block(body: Stmt)                     -- mutually recursive
data Stmt = Print(what: Expr) | Empty
data Rose[A](value: A, children: List[Rose[A]])                       -- nested under a container
```

`StrictPositivityChecker` blesses them by design ("positivity is *not* about making any fold total —
folds are platform natives, trusted to terminate"), and a one-layer `match` on any of them works.

Nothing can *consume* them. A hand-written walk is rejected by the no-recursion gate, and the
diagnostic points at a door that does not exist:

```
Value 'foldTree' is defined recursively.
  Use a platform loop primitive instead: 'fold' for a bounded loop, 'forever' for an endless one.
```

There is no platform loop primitive for a user's `Tree`, and there structurally cannot be one — a
platform cannot ship a native for a type the user has not written yet. So today a user can build an
arbitrarily rich recursive structure and then do nothing whatsoever with it.

This is not a gap in the *Total by Default* cornerstone. It is a **missing derivation**: `match` is
already the one-layer eliminator, and what is absent is its fixed point. Every recursive type in the
stdlib already has that fixed point **hand-written** — `Bool`'s `fold`, `foldOption`, `foldEither`,
`List`'s `foldLeft` — and the whole `docs/collections.md` combinator set is ordinary non-recursive
Eliot layered over `foldLeft`. The design below derives what has been written by hand four times.

## The design: the catamorphism is `PatternMatch` with recursive fields replaced

`PatternMatch` already solves the hard part — an ability whose method signature varies per
implementation:

```eliot
ability PatternMatch[T] {
   type Cases[R]                                     -- abstract shape, per implementation
   def handleCases[R](value: T, cases: Cases[R]): R   -- one uniform signature
}
```

`DataDefinitionDesugarer` generates, per data type, a body-less `Cases` type-def plus a
`handleCases` whose *own* signature inlines the concrete Church-encoded shape that
`eliminatorHandlerType` builds:

```
Cases[R]  =  Function[H₀ -> H₁ -> … -> Hₙ -> R, R]        -- one handler Hᵢ per constructor
Hᵢ        =  field₁ -> field₂ -> … -> R                   -- or Function[Unit, R] when nullary
```

Conformance is alias-transparent, so the abstract declaration and the concrete substitution agree.
The recursor is the same construction with **one substitution**:

```eliot
ability Recursive[T] {
   /** `PatternMatch.Cases[R]` with every recursive occurrence of `T` replaced by `Function[Unit, R]`. */
   type Algebra[R]

   /** Fold `value` bottom-up, applying `algebra`. */
   def fold[R](value: T, algebra: Algebra[R]): R
}
```

> **The derivation rule, in full:** in `eliminatorHandlerType`, a field whose type is the data type
> itself becomes `Function[Unit, R]`; every other field keeps its declared type.

For `data Tree[A] = Leaf | Node(left: Tree[A], value: A, right: Tree[A])`:

| | handler for `Leaf` | handler for `Node` |
|---|---|---|
| `PatternMatch.Cases[R]` | `Unit => R` | `Tree[A] -> A -> Tree[A] -> R` |
| `Recursive.Algebra[R]`  | `Unit => R` | `(Unit => R) -> A -> (Unit => R) -> R` |

Nothing new is needed to express that: `eliminatorHandlerType` **already** emits
`Function[Unit, R]` for nullary constructors, so the substituted expression is one the generator
already builds.

### Recursive positions are thunks, not values

Handing the algebra `Function[Unit, R]` rather than `R` is what makes the fold usable rather than
merely correct. The algebra decides *whether* to descend; a thunk that is never called is a subtree
that is never walked.

- `find`, `any`, `all`, `member` become **O(path)**, not O(n). `size`, `depth`, `sum` stay O(n)
  because they genuinely are.
- **Totality survives.** A user may only call the thunks the fold handed them, each standing for a
  structurally smaller subtree; they cannot fabricate one for a larger tree, so they cannot diverge.
  Calling one twice costs more work and still terminates.
- **The strictness rules hold.** `Function[Unit, R]` is an honestly declared function, not a
  suspension conjured by the compiler — *effects-as-rows* rule 1 (strict in every plain position)
  and rule 2 (suspension is declared) both stand, and no effect row is involved.

This subsumes what `tailRecM`'s `Either[A, B]` signals: per-child control is strictly more
expressive than one continue/stop bit, and it keeps `Either` out of the signature.

### What is generated, per data type

A fourth sibling of `createPatternMatchImpl` / `createTypeMatch` in `DataDefinitionDesugarer`,
emitting the same three shapes those two already emit, plus two conveniences:

1. **implementation marker** — `AbilityImplementation("Recursive", implKey)`, body-less signature
   vessel carrying the default `true` guard, exactly as the `PatternMatch` marker does.
2. **`Algebra`** — body-less type-def over `R: Type`.
3. **`fold`** — the concrete substituted algebra type inlined in its own signature; **abstract**, the
   backend supplies the body.
4. **`foldBound`** — the meta-data projection the refinement computation needs (see below): the
   structure's own recursion bound. Length for a list, **height** for a tree.
5. **strict positional wrapper** — `foldTree(ifLeaf, ifNode, t)`, derived non-recursive Eliot that
   forces every thunk immediately. Users never write a Church-encoded algebra, exactly as they never
   write `handleCases(scrutinee)(casesLambda)` — `matchdesugar` hides that today.

### Where the recursion lives: a native, like `handleCases`

`fold` is abstract and the **backend generates its body**, which is precisely where `handleCases`
already comes from: `DataClassGenerator` emits it as a virtual instance method on each data class,
Church-encoded, dispatching per constructor. `Node.fold` recurses into its children's `fold` and
applies the handler; `Leaf.fold` returns the base arm.

So the recursion sits in **generated bytecode, where every loop in Eliot already lives**. There is no
`fix`, no `letrec`, and — decisively — **no exemption from the recursion gate**. The *Total by
Default* cornerstone is untouched, and nothing here needs sign-off.

This also buys stack safety: because the backend owns the body, it may use an explicit work stack, so
a deep tree does not consume JVM stack depth. A recursive Eliot definition could never have promised
that.

The cost is that each backend implements the emission. That is the same cost `handleCases` and
`typeMatch` already pay, in the same file, following the same pattern — incremental work, not new
machinery.

## Traversals: one generated `fold` restricts nothing

The catamorphism fixes the order of the **recursion**, not the order of the **result**. The recursion
is always bottom-up depth-first; what gets built is entirely the algebra's choice. Depth-first in all
three orders, one line each, over the strict wrapper:

```eliot
def inOrder[A](t: Tree[A]): List[A]   = t.foldTree(empty, l -> v -> r -> l ++ singleton(v) ++ r)
def preOrder[A](t: Tree[A]): List[A]  = t.foldTree(empty, l -> v -> r -> singleton(v) ++ l ++ r)
def postOrder[A](t: Tree[A]): List[A] = t.foldTree(empty, l -> v -> r -> l ++ r ++ singleton(v))
```

Breadth-first is *not* available at `R := List[A]` — at a `Node` you hold two fully folded subtrees
and the level structure is gone. It **is** a cata at a richer carrier: fold to a list of levels and
merge pairwise.

```eliot
def levels[A](t: Tree[A]): List[List[A]] =
   t.foldTree(empty, l -> v -> r -> prepend(mergeLevels(l, r), singleton(v)))

def levelOrder[A](t: Tree[A]): List[A] = t.levels.flatten
```

`mergeLevels` zips two level-lists with `++`, keeping the longer tail — a `List` fold, and it wants a
`zipLongest` in the stdlib to read well.

Generally, **`R` may itself be a function**: fold to `R := Queue -> List[A]` for the accumulator-passing
queue-based breadth-first walk, or to `R := List[A] -> List[A]` for difference lists. Catamorphisms are
universal — every structurally recursive function over the type is a cata, possibly at a higher-order
carrier. Higher-order `R` allocates, which is free on the JVM and worth watching on an ATtiny.

### Traversal order is a choice, so it is never derived

`fold` is canonical and unique, so it is derived. A **traversal order** is not: in-order, pre-order
and level-order are all defensible, so they stay ordinary named functions. Likewise `filter`, which
has no canonical meaning on a tree (deleting an interior node), stays library code over `fold` —
exactly as `List.filter` is a fold today. `Functor.map` *is* canonical by parametricity over a
strictly-positive parameter, so it is derivable later.

Ability coherence makes this concrete: implementations must be unique with no overlap, so depth-first
and breadth-first **cannot both** be `Foldable[Tree[A], A]`. If a second instance is genuinely wanted,
the wrapper is cheap:

```eliot
data BreadthFirst[A](tree: Tree[A])
implement[A] Foldable[BreadthFirst[A], A] { ... }   -- over levelOrder
```

`Traversable` is **not** part of this design. `List.map` already declares `f: A => {Effect} B`, so
effectful mapping works in direct style and rows already do `traverse`'s job.

### Effects need nothing from the ability

Instantiate `R := G[B]`: the algebra receives already-folded computations as children and `flatMap`s
them itself, so `fold` stays pure and effect-transparent. This is `foldLeftInternal`'s design
generalized — the ability carries no effect row, and therefore never interacts with the effect
machinery at all.

## Meta-data: run the transfer function, bounded by the structure

The result's refinement cannot be stated axiomatically. The grade design in `TODO.md` can write

```
def fold[T, A](ls: List[T], init: A, step: F[A]): {cycles: ls.size.end * step.cycles + 7} F[A]
```

because cycles compose by a **fixed** algebra (`seq (+)`, `branch (interval hull)`, registered per
platform), so `n * c + 7` is closed-form. The accumulator's refinement has no such law: it composes
by the **user's step function** — `acc + 1`, `acc * 2`, `max(acc, item)` — which is arbitrary Eliot.
That is the one place "axiomatic on natives" cannot reach, and the value is obtainable only by
applying the code.

Applying it needs **no new engine**. `Interval + Interval` is ordinary Eliot — `Interval[T]` closes
under the arithmetic operators via `implement[T ~ Numeric[T] & Compare[T]] Numeric[Interval[T]]`, so
that `[0, 1] + [1, 2]` computes `[1, 3]` on `Interval[BigInteger]` — and the single NbE evaluator
already runs it, precisely as *Types Are Values* says it should. (Widening-based abstract interpretation *would* be a second engine. Concrete iteration is
not.)

### The iteration

With `ρ₀` the refinement of `init` and `Φ(ρ)` the refinement of `step`'s result given `acc : ρ`:

```
ρ_{k+1} = ρ_k ⊔ Φ(ρ_k)        until a fixed point, or N times, whichever comes first
```

`N` is the structure's own recursion bound. The join at each step is required rather than taking
`Φ^N(ρ₀)` alone, because the fold may run *any* prefix in `[0, N]` — which is `branch` in the grade
vocabulary, and is also what keeps the result correct for a **short-circuiting** thunked fold.

**No widening operator is needed**, precisely because `N` is finite and known; widening exists only to
force convergence against an unbounded bound. The interval obtained is exact.

Convergence is frequently immediate, so this is not uniformly O(N):

| step | iterations |
|---|---|
| `acc -> item -> max(acc, item)` | 1 — fixed point at once |
| `acc -> item -> item` | 1 |
| `acc -> item -> acc && p(item)` | 1 |
| `acc -> item -> acc + 1` | N — grows every step |

Only genuinely *accumulating* folds cost N applications of the meta-data code. At microcontroller
scale (≤ 255, ≤ 65535) that is affordable; at 2³¹ it is not.

### The bound is itself meta-data, and for a tree it is height

For a list the bound is length. For a tree the node arm applies once **per level**, joining across
siblings, so the bound is **height**. Uniformly:

> Every `Recursive` type must carry the bound of its own cata as meta-data — length for a list,
> height for a tree.

Hence the derived `foldBound` projection above: the refinement computation is unrunnable without it,
and deriving it per data type keeps this uniform instead of per-type special-casing.

### When the bound is unbounded

If `N` is `∞` or too large to play, either

1. the result refinement is **declared** and verified in one step — `ρ₀ ⊆ ρ_decl` and
   `Φ(ρ_decl) ⊆ ρ_decl`, an induction rather than an iteration, decidable and cheap; or
2. it widens to `⊤` (unbounded `Int`).

Both are **sound and fail-safe**: a wider result proves fewer things downstream, so the consequence is
more errors, never fewer. Nothing silently accepts wrong typing.

The canonical cases avoid iteration entirely: `List.size` is a native whose result range is a
*projection* of its input's size meta-data (`{range: ls.size}`) per the §4.2 projection discipline.
Iteration is the inference path for **user-written** folds that declare nothing, such as
`list.foldLeft(0, acc -> _ -> acc + 1)`.

Verification lands post-monomorphization in the existing refinement channel
(`monomorphize/channel/RefinementChannelProcessor`), at ground instantiations where the bound and the
algebra are both concrete — the same use-site stance `EffectAccountingProcessor` takes for effects.

### Prerequisite: sizes

`List` carrying `size: Interval` is the container-propagation work already noted as blocking the
refinement channel's size domain, and it needs the size algebra on the container operations:

| operation | size |
|---|---|
| `empty` | `[0,0]` |
| `append` / `prepend` | `n + 1` |
| `map` | `n` exactly |
| `filter` | `[0, n]` |
| `flatten` | multiplies |

Until that lands every fold's bound is `∞` and the fallback is always taken — so sizes really are the
gate, matching the **ranges → sizes → grades** ladder.

## Rejected alternatives

- **One generic `cata` over a derived base functor.** `ability Recursive[T] { type Base[_]; def
  project(t: T): Base[T] }` plus a single hand-written recursive
  `cata(alg, t) = alg(map(x -> cata(alg, x), project(t)))` concentrates all recursion in exactly one
  definition, and everything generated (`data TreeF[R]`, `implement Functor[TreeF]`, `project`) is
  non-recursive. **Rejected** because that one definition still needs an exemption from the recursion
  gate, and exemptions are not on the table. The native-`fold` route needs none and additionally
  gives stack safety.
- **User-written recursion behind a structural-descent checker** (Agda/Idris style). More expressive,
  and where sized types eventually point, but a **reversal** of a decided cornerstone ("no
  `fix`/`letrec` — full stop") requiring measures, a checker, and sign-off. It would also dilute
  `Inf`, whose whole meaning is that it is the *only* opt-out.
- **`Foldable` as the primitive.** It is element-oriented and lossy: a `Tree[A]` folded through it
  yields elements in some order, not shape, so `depth` is not expressible. It also only applies at
  kind `* -> *`, while the eliminator applies to every data type, parameterized or not.
- **`foldLeft` as the primitive.** Left-accumulating, element-oriented and linear. There is no
  canonical "left" for a tree.
- **Deriving `unfold` / anamorphisms.** The dual is *not* cata-derivable and *not* total — producing
  can diverge where consuming cannot. That belongs to `Inf`, deliberately outside this design.

## Open questions

1. **Mutual recursion.** `Expr`/`Stmt` compiles today and is silently unusable. Its recursor is a
   *pair* of eliminators, each taking both algebras, so it needs either a multi-parameter
   `Recursive[T₁, T₂]` or one instance derived per SCC of the data-reference graph. The SCC
   computation already exists in `RecursionChecker`. This is the hardest sub-case.
2. **`List` is not `data`.** It is a native `java.util.List`, so there are no constructors to derive
   from and `foldLeftInternal` stays a native. A hand-written `Recursive[List[A]]` instance is
   perfectly possible, so generic cata code can still cover it; it simply is not *derived*.
3. **Container-nested recursion.** `data Rose[A](value: A, children: List[Rose[A]])` recurses *through*
   `List`, so reaching the children needs `List`'s own `map`. This wants the derived `Functor` (or an
   equivalent) first, and is a later step than direct recursive fields.
4. **A row-carrying ability method is currently classified as an effect** — `ability Foldable[T, A]`
   with a `{Effect}` row in a method yields *"This value performs the effect 'Foldable'"*. Judged a
   compiler bug, and it must be fixed before `Foldable`/`Functor` can carry rows. It is **not** on
   this design's critical path: `Recursive` carries no rows.
5. **No structural short-circuit beyond the thunks.** The thunked algebra removes forced O(n) for
   searches, but there is no way to abandon a fold *mid-arm* other than declining to force. If that
   is ever wanted it is its own primitive, not a change to `fold`.

## Invariants this design must keep

- **No recursion in Eliot, ever** — `fold`'s body is backend-generated, and no definition is exempt
  from the recursion gate. If a future step wants an exemption, that is a cornerstone reversal
  needing sign-off, not a refinement.
- **One evaluator.** The refinement iteration runs the *existing* NbE evaluator over ordinary
  `Interval` code. No widening lattice, no second abstract interpreter, no parallel "meta-data
  interpreter".
- **Derive what is unique; never derive a choice.** The eliminator and (later) `map` are canonical.
  Traversal order, `filter`, and `Foldable` instances are choices and stay in user code.
- **Meta-data is declared or computed, never guessed.** When the bound is finite the interval is
  exact; when it is not, the result is a declared invariant verified in one step, or `⊤`. Widening
  toward a *wider* result is the only sanctioned imprecision, because it can only produce more errors,
  never fewer.
