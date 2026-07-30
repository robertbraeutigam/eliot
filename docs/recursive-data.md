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
already the one-layer eliminator, and what is absent is its fixed point.

The evidence is that the stdlib hand-writes an eliminator for **every** `data` type it owns — `Bool`'s
`fold`, `foldOption`, `foldEither`, `foldPair` — each a near-identical `match` over its own constructors,
duplicated again in the `eliot-compiler/` overlay. Those are all *non*-recursive, so they are merely
tedious; a recursive type is where the same shape stops being writable at all. (`List`'s `foldLeft` is a
different animal — a native container's fold, not a `data` eliminator, and it stays a native. The whole
`docs/collections.md` combinator set is then ordinary non-recursive Eliot layered over it.)

So the derivation both removes existing duplication and makes the currently impossible case possible.

## The design: the recursor is `PatternMatch` with recursive fields expanded

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
The recursor is the same construction under **one rewrite of the handler fields**:

```eliot
ability Recursive[T] {
   /** `PatternMatch.Cases[R]`, with every recursive field expanded to that field *plus* its suspended fold. */
   type Algebra[R]

   /** Fold `value` bottom-up, applying `algebra`. */
   def fold[R](value: T, algebra: Algebra[R]): R
}
```

> **The derivation rule, in full:** in `eliminatorHandlerType`, a field whose type is the data type
> itself becomes **two** handler arguments — the field unchanged, then `Function[Unit, R]`. Every other
> field keeps its declared type, unchanged and alone.

For `data Tree[A] = Leaf | Node(left: Tree[A], value: A, right: Tree[A])`:

| | handler for `Leaf` | handler for `Node` |
|---|---|---|
| `PatternMatch.Cases[R]` | `Unit => R` | `Tree[A] -> A -> Tree[A] -> R` |
| `Recursive.Algebra[R]`  | `Unit => R` | `Tree[A] -> (Unit => R) -> A -> Tree[A] -> (Unit => R) -> R` |

Nothing new is needed to express that: `eliminatorHandlerType` **already** emits `Function[Unit, R]`
for nullary constructors, so both substituted pieces are expressions the generator already builds.

### Why the original field, and not just the thunk

Handing back the field itself makes this a **paramorphism** rather than a plain catamorphism, and the
distinction is load-bearing rather than decorative. Consider inserting into a search tree: the arm needs
the *untouched* sibling subtree to rebuild with, and a catamorphism only ever yields folded results — so
reconstructing the untouched side means folding it, which is O(n) per insert and destroys the point.

It costs nothing to provide. The constructor already holds `left` and `right`, so the generated `fold`
has both in hand and passes them for free. **A catamorphism is just the paramorphism that ignores the
originals**, which is precisely what the simple `foldTree` wrapper below does.

The tempting simplification — "`para` is derivable from `cata`, so derive the smaller thing" — is true
mathematically and wrong operationally: the derivation costs a full rebuild of every subtree the algebra
wanted to keep. Do not narrow this back to cata.

### Recursive positions are suspended, not forced

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

### Select a thunk; never force one inside a conditional

A **trap**, and the one thing most likely to silently undo the property above. A *pure* arm of
`fold`/`if` is built strictly — only an *effectful* arm is genuinely suspended
([[gotcha_if_guard_builds_both_arms]]). So a conditional over already-forced thunks walks both subtrees:

```eliot
fold(value <= v, goL(unit), goR(unit))     -- WRONG: applies both, walks the whole tree
fold(value <= v, goL, goR)(unit)           -- right: selects a function, applies one
```

Both arms of the correct form are *function values*, so building them is free and only the selected one
is ever applied. Every conditional descent therefore ends in `(unit)`, outside the `fold`. Without this
idiom the O(path) behaviour disappears with no diagnostic, which makes it worth stating in the derived
wrappers' documentation rather than leaving to folklore.

### What is generated, per data type

A fourth sibling of `createPatternMatchImpl` / `createTypeMatch` in `DataDefinitionDesugarer`,
emitting the same three shapes those two already emit, plus two conveniences:

1. **implementation marker** — `AbilityImplementation("Recursive", implKey)`, body-less signature
   vessel carrying the default `true` guard, exactly as the `PatternMatch` marker does.
2. **`Algebra`** — body-less type-def over `R: Type`.
3. **`fold`** — the concrete substituted algebra type inlined in its own signature; **abstract**, the
   backend supplies the body.
4. **`depth`** — the derived nesting-depth meta-data the refinement computation needs as its iteration
   bound (see below): a per-constructor refinement rule, plus the projection that reads it. Derived,
   never user-declared.
5. **two positional wrappers**, derived non-recursive Eliot, so users never write a Church-encoded
   algebra — exactly as they never write `handleCases(scrutinee)(casesLambda)`, which `matchdesugar`
   hides today:

   ```eliot
   -- the common case: originals dropped, thunks forced. A plain catamorphism.
   def foldTree[A, B](ifLeaf: {Effect} B, ifNode: B => A => B => {Effect} B, t: Tree[A]): {Effect} B

   -- the general case: each recursive field as (original, suspended fold).
   def foldTreeWith[A, B](
      ifLeaf: {Effect} B,
      ifNode: Tree[A] => (Unit => B) => A => Tree[A] => (Unit => B) => {Effect} B,
      t: Tree[A]
   ): {Effect} B
   ```

   **The arms are rows, not plain types.** `{Effect} B` on every arm result is what `Bool.fold`,
   `foldOption` and `foldEither` already do — `IfDemo.els` documents `Bool.fold`'s arms as *"both arms are
   suspended (`{Effect} A`), so only the selected one ever runs"*. (`foldPair` is the outlier: pure, with
   its single arm spelled `Function[A, Function[B, C]]`. See *Migrating the existing folds* below.) Rows
   and thunks are **complementary**: rows give *effect*-laziness on the arms, thunks give
   *evaluation*-laziness on recursive positions. A pure arm in a row slot is still built strictly
   ([[gotcha_if_guard_builds_both_arms]]), which is exactly why the thunks cannot be replaced by rows.

   Nullary arms are `Function[Unit, R]` inside the Church-encoded `Algebra`, but the wrapper exposes them
   as row-typed values (`ifLeaf: {Effect} B`) and passes `_ -> ifLeaf` inward — what the `match` desugar
   does today.

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

## A worked example: a fully pure search tree

The whole point of the derivation is that this is all a user writes — no recursion, no natives, and **no
meta-data annotations at all**. Everything below is derived from the declaration or computed from a body;
the leaf-trust concession discussed later is needed only for platform containers like `List`.

```eliot
import eliot.collection.List

/** An unbalanced binary search tree. */
data Tree[A] = Leaf | Node(left: Tree[A], value: A, right: Tree[A])

def empty[A]: Tree[A] = Leaf

def single[A](value: A): Tree[A] = Node(empty, value, empty)

/** `t` with `value` inserted in order. Rebuilds only the path descended; the untouched side is reused. */
def insert[A ~ Compare](value: A, t: Tree[A]): Tree[A] =
   t.foldTreeWith(
      single(value),
      origL -> goL -> v -> origR -> goR ->
         fold(value <= v, _ -> Node(goL(unit), v, origR),
                          _ -> Node(origL, v, goR(unit)))(unit)
   )

/** Whether `t` holds `value`. Visits one path, not the whole tree. */
def contains[A ~ Compare](value: A, t: Tree[A]): Bool =
   t.foldTreeWith(
      false,
      _ -> goL -> v -> _ -> goR ->
         fold(value <= v, fold(v <= value, _ -> true, goL), goR)(unit)
   )

def size[A](t: Tree[A]): Int = t.foldTree(0, l -> _ -> r -> l + r + 1)

def height[A](t: Tree[A]): Int = t.foldTree(0, l -> _ -> r -> max(l, r) + 1)

def inOrder[A](t: Tree[A]): List[A] = t.foldTree(empty, l -> v -> r -> l ++ singleton(v) ++ r)
```

`insert` and `contains` reach for `foldTreeWith` because they descend one path and keep the other side
untouched — the paramorphism and the selection idiom together are what make them O(path). Everything
that genuinely visits every node uses the plain `foldTree`.

What the compiler derives from the declaration, spelled as if hand-written:

```eliot
data Tree[A] {depth: Interval[BigInteger]}                    -- the meta-data channel

def Leaf[A]: Tree[A] {[0, 0]}
def Node[A](left: Tree[A], value: A, right: Tree[A]): Tree[A] {max(depth(left), depth(right)) + 1}
```

And what it computes from the bodies, with no annotation written and none permitted:

| definition | computed meta |
|---|---|
| `empty` | `{[0, 0]}` — directly from `Leaf` |
| `single` | `{[1, 1]}` — from `Node(Leaf, v, Leaf)` |
| `insert` | `{[0, depth(t) + 1]}` — the iteration: `Φ` rebuilds a `Node`, so depth grows by at most one level |
| `height` | `Int {[0, depth(t)]}` |
| `size` | `Int {[0, 2^depth(t) − 1]}` |

**Open naming question.** The derived `depth` projection occupies the name `depth` in `Tree`'s namespace,
exactly as `range(a)` does for `Int` — which is why the runtime function above is called `height`. Either
the derived channel takes a distinguished name or users avoid it; this needs deciding before
implementation.

## Migrating the existing folds

The derivation applies to **every** `data` type, not only recursive ones. `Option`, `Either` and `Pair`
have no recursive fields, so nothing matches the rewrite: no originals, no thunks, `Algebra[R]` ≡
`Cases[R]`, and the derived `fold` ≡ `handleCases`. For them the derivation is plain case analysis — and
that is precisely the shape of three of the four hand-written folds this design was motivated by.

Better still, the derived name is already the right one. `fold<TypeName>` ⇒ `foldOption`, arms from
constructor names ⇒ `ifNone`/`ifSome`. So migration is a **deletion**, not a rewrite:

```eliot
-- jvm/eliot/eliot/lang/Option.els AND stdlib/eliot-compiler/eliot/lang/Option.els both hold this
-- identical body today; both are deleted, since the derived wrapper *is* foldOption.
def foldOption[A, B](ifNone: {Effect} B, ifSome: A => {Effect} B, o: Option[A]): {Effect} B = o match {
   case None -> ifNone
   case Some(v) -> ifSome(v)
}
```

Leaving a hand-written body beside the derived one is a hard error — *"Has multiple implementations."*

What **stays** is the abstract declaration in `stdlib`: base-layer code calls `foldOption` (`orElse`,
`mapOption`, `List.head`) and the base has only `type Option[A]`, with no constructors to derive from. So
the win on an existing type is two bodies, not three. On a genuinely recursive type it is total, because
today the body cannot be written at all.

**The delicate part is the merge, and it applies only to migration.** Because layer merging is lexical
([[gotcha_arrow_alias_not_in_data_or_merge]]), a derived signature must be *character-identical* to the
abstract declaration it replaces. Against `foldOption` the generator must emit `{Effect} B` rows (not
plain `B`), the `=>` arrow alias (`Function[A, {Effect} B]` would not match textually), the result generic
named `B` rather than `R`, and the parameters `ifNone`, `ifSome`, `o` in that order with the subject last.

And the four existing eliminators **do not agree with each other**, so no single convention matches them
all as written:

| | arms | spelling | name |
|---|---|---|---|
| `Bool.fold` | rows, plus `{ join(whenTrue, whenFalse) }` | values (nullary constructors) | `fold`, not `foldBool` |
| `foldOption` | rows | `A => {Effect} B` | conventional |
| `foldEither` | rows | `E => {Effect} B` | conventional |
| `foldPair` | **none — pure** | `Function[A, Function[B, C]]`, one curried arm | conventional |

So migration means **normalising these declarations onto the generated convention**, not matching each
one. Adding rows to `foldPair` is source-compatible (a row slot is strictly more permissive than a plain
one, so existing call sites keep compiling), but it *is* a base-layer edit and `Bool.fold`'s name is a
deliberate exception. Migrate them deliberately, one at a time, rather than as a side effect of landing
the feature. A new user type such as `Tree` has no abstract declaration to match and is unaffected.

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

### The non-recursive half already ships: join the arms

`Bool.fold` is the degenerate eliminator, and it *already* carries its meta-data:

```eliot
def fold[A](condition: Bool, whenTrue: {Effect} A, whenFalse: {Effect} A): {Effect} A { join(whenTrue, whenFalse) }
```

`join` is exactly the `branch` operation — the hull of the arms. So the derived fold's meta-data splits
cleanly in two:

- **case analysis** contributes `join(arm₁, …, armₙ)`, and needs no iteration whatever. A non-recursive
  type is *entirely* this case: the derived `foldOption`'s meta is `join(ifNone, ifSome)`, full stop.
- **recursion** contributes the iteration below, and only where a constructor actually has recursive
  fields.

That the shipped `Bool.fold` already spells the first half in exactly this vocabulary is the strongest
available evidence the derivation is writing down an existing pattern rather than inventing one.

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

### The bound has one role and two sources

The bound plays a single role — how many times `Φ` nests — but it comes from **the same place the
fold's implementation comes from**, and there are two of those.

**A user-declared `data` ⇒ derived from its constructors.** The recursion is visible in the
declaration, so the bound is its **nesting depth**, produced by the same substitution that produces
the fold:

```
constructor with no recursive fields   ⤳   [0,0]
constructor with recursive fields      ⤳   max(those fields' depths) + 1
```

That is an ordinary constructor refinement rule, propagated by the existing channel through
constructor application, so `Node(Leaf, x, Leaf)` gets depth `[1,1]` exactly — no fold involved and no
circularity, because the channel propagates through *constructors* and only the fold's *result* needs
the iteration.

**A platform-native container ⇒ an axiomatic projection stated by the platform.** `List` is a native
`java.util.List` (an `ArrayList` on the JVM). It has **no constructors**, so there is no nesting depth
to derive and nothing to substitute over: its bound is `size`, declared beside the native exactly as
its cycles cost is, and `foldLeftInternal` applies `Φ` **sequentially** `size` times rather than per
level.

This is not two mechanisms. It is the same layering the whole language uses — a `fold` body comes from
the backend for a `data` type and from `foldLeftInternal` for `List`, and **the bound follows the
implementation.** Derived fold, derived bound; native fold, platform-axiomatic bound.

**Trust is confined to leaves.** A native's size transfer *cannot* be derived — nothing in Eliot can
see that `ArrayList.add` appends exactly one element — so `append: n + 1` is **declared and trusted**,
on the base-layer body-less `def`, since no Eliot body exists on any platform. That is the same trust
already extended to a native's effect row (nothing verifies `printLine` touches the console) and to its
cycle count (AVR datasheet numbers), and it is the grade design's existing rule: *axiomatic on natives,
checked contracts on bodied defs*.

Above a leaf the rule inverts: a **bodied** definition's meta-data is computed from the declarations
below it and never asserted. An unchecked assertion on a definition whose body is available is the real
unsoundness, because the body can contradict it — that is what
[[feedback_gaps_must_be_failsafe]] rules out, not the leaf axiom.

### Branching needs no meta-datum: Φ's arity supplies it

One bound suffices even though a tree branches, because the recursive arm applies `Φ` once per
recursive field by construction:

| user fold | `Φ` | after `H` iterations |
|---|---|---|
| `depth` | `max(r, r) + 1` | `[0, H]` |
| `size`  | `r + r + 1`     | `s(h) = 2·s(h−1) + 1` ⇒ `[0, 2^H − 1]` |

`2^H − 1` is exactly the tight node count for a tree of height `H`. So the same single bound yields
the correct interval for both folds, and no branching factor is tracked anywhere.

Depth-derived bounds are **sound but not always tight**: a degenerate tree of height 8 holding 8 nodes
still reports `size : [0, 255]`, because depth alone cannot distinguish it from a full one. That is the
right direction to err (a wider result proves fewer things), and if a type ever carried *both* depth
and node-count meta-data the two bounds would simply intersect. Not required for this design.

### The fold consults no type-specific meta-data

It iterates whatever the refinement channel **already holds for `R`'s type**: `R := Int` ⇒ the `range`
channel; `R := List[A]` ⇒ the `size` channel once that exists; `R := String` ⇒ nothing tracked, so
trivially `⊤`. Both inputs come from the channel as well — `ρ₀` is its refinement of `init`, `Φ` its
refinement of the step body.

The fold therefore contributes exactly two things, **the iteration and the bound**, and introduces no
meta-data vocabulary of its own. It must stay a *consumer* of existing channels; a fold-specific
notion of meta-data would be a second analysis by the back door.

### The bound must be declarable and projectable on parameters

Otherwise the mechanism is unreachable in real code: a literal `Node(Leaf, x, Leaf)` has an exact
depth, but `def sum(t: Tree[Int]): Int` does not, so every fold over a parameter is `⊤`. Whichever of
the two sources supplies it — derived depth or a native's `size` — the bound must be

- **declarable** on a parameter — `t: Tree[Int] {depth: [0,8]}`, checked at the call site like any
  other refinement; and
- **projectable** — `t.depth`, exactly as the grade design already writes `ls.size.end`.

Keep this distinct from a user-written `def depth(t: Tree[A]): Int = t.foldTree(0, l -> v -> r ->
max(l(unit), r(unit)) + 1)`. That is an ordinary fold whose result range `[0, H]` comes *out of* this
mechanism; it does not feed it. The two agreeing is a consistency check, not an input.

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
refinement channel's size domain. Because `List` has no constructors, every entry below is **declared
and trusted** on the base-layer body-less `def`, not derived — the axiomatic source of the previous
sections:

| operation | size |
|---|---|
| `empty` | `[0,0]` |
| `append` / `prepend` | `n + 1` |
| `map` | `n` exactly |
| `filter` | `[0, n]` |
| `flatten` | multiplies |

Until that lands every fold's bound is `∞` and the fallback is always taken — so sizes really are the
gate, matching the **ranges → sizes → grades** ladder.

The honest cost of trusting these: a wrong `append: n + 1` silently yields wrong bounds, with no
diagnostic. That is the same exposure the platform already carries for effect rows and cycle counts, and
it is bounded the same way — the surface is a handful of container operations per platform, auditable in
one file, and testable by generators and probing rather than by proof (the *Use-Site Verification*
cornerstone's stance on totality).

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
   equivalent) first, and is a later step than direct recursive fields. It also breaks the two clean
   properties above: the depth rule (`max(recursive fields) + 1`) has to reach *through* the container
   to find the recursive occurrence, and the branching factor is the child list's **length** rather
   than a fixed constructor arity — so unlike a `Tree`, `Φ`'s arity no longer supplies branching on its
   own and the container's `size` meta-data is needed too. The same container-propagation dependency as
   everything else on the ladder.
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
- **The eliminator stays a paramorphism.** Each recursive field yields the original *and* its suspended
  fold. Narrowing to a catamorphism because `para` is cata-derivable is a real regression, not a
  simplification: it forces a full rebuild of every subtree the algebra meant to keep, so `insert` goes
  from O(path) to O(n). The originals cost nothing — the constructor already holds them.
- **Conditional descent selects a thunk, never forces one.** `fold(c, goL, goR)(unit)`, not
  `fold(c, goL(unit), goR(unit))`, because a pure arm is built strictly. This is the difference between
  O(path) and O(n) and it fails silently, so it belongs in the derived wrappers' own documentation.
- **Meta-data is declared or computed, never guessed.** When the bound is finite the interval is
  exact; when it is not, the result is a declared invariant verified in one step, or `⊤`. Widening
  toward a *wider* result is the only sanctioned imprecision, because it can only produce more errors,
  never fewer.
- **The bound follows the implementation.** A derived fold gets a derived bound (nesting depth, from the
  constructors); a native fold gets an axiomatic one (`List`'s `size`, declared on the body-less base
  `def` — there are no constructors to derive from, and a native's transfer is not derivable in
  principle).
- **Trust is confined to leaves.** A body-less `def` backed by a native may *declare* its meta-data
  transfer, exactly as it declares its effect row and its cycle cost. A **bodied** definition may not:
  its meta-data is computed from what it calls. Moving an assertion above a leaf, where a body could
  contradict it, is the unsoundness to refuse.
- **The fold owns no meta-data vocabulary.** It iterates whatever channel `R`'s type already has, and
  contributes only the iteration and the bound. A fold-specific meta-datum would be a second analysis
  by the back door.
