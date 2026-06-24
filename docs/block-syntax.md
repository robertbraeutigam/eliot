# Block Expressions: `val` Bindings and Statement Sequencing

## Goal

Give Eliot a readable block form so multi-step bodies stop being hand-nested `flatMap` chains.
Two surface features:

- **`val` bindings** — a local name bound to a value: `val name = expr`.
- **Statement sequencing** — write effectful steps one after another and have them threaded
  through the carrier automatically.

Today this:

```
def swap(next: String): {State[String]} String =
   flatMap(getState, old -> flatMap(putState(next), ignored -> pure(old)))
```

should be writable as this:

```
def swap(next: String): {State[String]} String = {
  val old = getState
  putState(next)
  old
}
```

## The core idea: one construct, not two

`val` and "statement" are the **same** construct. A block is a right-fold of immediately-applied
lambdas, and a bare statement is just a `val` whose binder is discarded:

```
⟦ val x = e ; rest ⟧ = (x -> ⟦rest⟧)(e)        -- named binding
⟦ e ; rest ⟧         = (_ -> ⟦rest⟧)(e)         -- statement = val with an ignored binder
⟦ result ⟧           = result                    -- final expression = the block's value
```

So a block of `n` items lowers to `n` nested `FunctionApplication(FunctionLiteral(...), rhs)` nodes
terminating in the result expression. This is the only lowering rule; everything else is reused.

Two consequences fall out of this lowering **for free**:

- **Effect threading.** The lowering produces a tower of immediately-applied lambdas, so the
  existing auto-lift (`effect.processor.DirectStyleDesugarer`) rewrites every
  effectful-argument-in-pure-position into `Monad.flatMap` (or `Applicative.map` when the
  continuation is pure). `(_ -> rest)(println(x))` becomes `flatMap(println(x), _ -> rest)`
  automatically. There is no bind arrow (`<-`): `val x = e` binds the *carried result* when `e` is
  effectful and the plain value when it is not, decided by `x`'s use-site type exactly as every
  other argument position already is.

- **Termination.** Every lowered node is an immediately-applied, non-recursive lambda; a binder's
  scope never covers its own right-hand side. So a block cannot express recursion, forward
  references, or mutual recursion — structurally. `termination.RecursionChecker` walks
  `ValueReference`s; block binders become `ParameterReference` leaves it already ignores. No checker
  change.

A block is an ordinary **expression** (usable as a lambda body, a `match`-arm body, or an argument),
not a `def`-only construct, so `def f: T = { … }` is just the common case.

## Surface syntax

### Blocks and `val`

```
val name = expr            -- binding
val name: Type = expr      -- binding with an optional type annotation
expr                       -- statement (value discarded unless it is the final expression)
```

A binding reuses the existing lambda-parameter shape (`LambdaParameterDefinition`: a name with an
optional `: type`). `val` is a reserved keyword (added to `token.TokenParser.hardKeywords`; confirmed
no `.els` uses `val` as an identifier). A block must end in an expression (the result); a block
ending in a binding, or an empty block, is an error.

### Statement separation: newlines, never `;`, by fixity

There are **no semicolons**. The block is **over-separated** at parse time — split on *every*
newline — and a later pass **merges** adjacent lines back together when the line break sits inside a
single expression. The merge decision uses the *declared fixity* of the parts on each side of the
break, so user-defined operators (symbolic *or* alphanumeric) join lines exactly as built-ins do.
For two lines one newline apart, **merge iff** the **last part of the upper line** has fixity
`Infix` or `Prefix` (it demands a right operand), **or** the **first part of the lower line** has
fixity `Infix` or `Postfix` (it demands a left operand). Otherwise the break is a statement
boundary. A **blank line never merges** (the force-separate override); `( … )` is the force-join.

This gets every case right:

```
foo                 -- '.' first on the lower line is Infix → merge
  .bar              --   leading-dot chaining: foo.bar.baz
  .baz

a                   -- 'or' first on the lower line is Infix → merge (alphanumeric infix)
  or b

base +              -- '+' last on the upper line is Infix → merge
  adjustment

foo                 -- both Application → no merge → two statements
bar

flag                -- '!' first on the lower line is Prefix (not Infix/Postfix) → no merge:
! other             --   two statements `flag` and `!other`, correctly
```

### Why fixity, and why a standalone pass

Fixity in Eliot is a **declared, semantic property of a name, not a lexical one**: `infix left def
or(...)` makes the ordinary alphanumeric `or` an infix operator, and `.` is an ordinary infix
(`Function.els`: `infix left below apply def .[A,B](a, f) = f(a)`). So a purely lexical newline rule
(e.g. "a `Symbol` is an operator") is unsound — `or` is alphanumeric, and `+` is only an operator
once declared.

But fixity does **not** require the operator resolver. It is carried as a static field from
`FunctionDefinition` → `core.NamedValue` → `resolve.ResolvedValue` → `MatchDesugaredValue`
(`ValueResolver` copies it through). So once a part is name-resolved to an FQN, its fixity is one
`getFact(ResolvedValue.Key(fqn)).fixity` away — and the merge only ever inspects the *boundary*
parts of each line, never the interior. The whole decision is therefore a small standalone pass that
operates on the flatexpressions opaquely and leaves the actual operator resolution to the operator
resolver, which stays untouched.

## Implementation

### 1. The node and how each layer treats it

`Expression.BlockExpression(lines: Seq[BlockLine])`, where a `BlockLine` is
`(binder: Option[(name, optType)], expr: <FlatExpression of that source line>)`. The parser splits on
every newline (the only line-awareness in the whole feature: a line is the maximal atom run on one
source line, tracked via `Sourced.range`), recognises a leading `val name [: type] =` as the
binder, and takes the rest of the line as `expr`. No merging at parse time.

The node lives in the **ast / core / resolve** expression models and is gone before matchdesugar. In
`core` (`CoreExpressionConverter`) the handling is structural: convert each line's `expr`, carry
binders. `EffectSugarDesugarer`'s `collectEffects` / `rewrite` get a `BlockExpression` case so a
`{…}` in a `val x: {Console} T` annotation is still desugared.

### 2. Resolve — scoping only

`ValueResolver` resolves each line's `expr` and pushes each `binder` name into the existing
`ValueResolverScope` so subsequent lines resolve references to it (the same mechanism used for lambda
and function parameters). Scoping is per-line and positional — it needs no statement boundaries. A
reference to a binder from within its own (later-merged) RHS resolves to that local but lands outside
its lambda at lowering, where it is the hard error **"`x` is referenced in its own definition"**.
(Decided: a `val` RHS does *not* shadow an outer same-named binding; self-reference is an error,
matching the no-recursion stance.)

### 3. `BlockDesugaringProcessor` — merge and lower (the only real new logic)

A standalone processor `ResolvedValue → BlockDesugaredValue`, inserted **after resolve and before
matchdesugar** (which is rewired to consume `BlockDesugaredValue`). It is deliberately **out of the
operator resolver**: it cannot run before name resolution (it looks fixity up by FQN, so the merge
agrees with how the operator resolver will treat the same names) and the operator resolver needs no
change. Per block:

1. **Merge** adjacent lines one newline apart by the boundary-fixity rule above — fixity from
   `getFact(ResolvedValue.Key(fqn)).fixity` for a `ValueReference` boundary part, else `Application`.
   Chain-merge until stable; never merge across a blank line or into a `val` line.
2. **Lower** the merged lines into nested immediately-applied lambdas: the first statement after a
   binder is its RHS (`(name -> rest)(rhs)`), other statements are bare (`(_ -> rest)(s)`), the last
   statement overall is the result. Error if a binder occurs in its own merged RHS.

Output is ordinary lambdas wrapping the still-unresolved merged `FlatExpression`s. matchdesugar, the
operator resolver, effect and termination then run unchanged — the operator resolver resolves each
merged flatexpression like any other.

### Blast radius

`BlockExpression` in three expression models (`ast` / `core` / `resolve`) plus their `Show` /
traversal / structural-equality; the block parser; `val` in `TokenParser.hardKeywords`;
`CoreExpressionConverter` (structural recursion); `EffectSugarDesugarer` (two cases); `ValueResolver`
(binder scope layer); the new `BlockDesugaringProcessor` + `BlockDesugaredValue` fact; and rewiring
`MatchDesugaringProcessor`'s input key. **Nothing in the operator resolver, effect, monomorphize, or
termination.**

### Alternative considered: desugar before resolve

Running the merge/lower *before* `ValueResolver` would turn each `val` into a plain lambda, so the
scope layer in resolve would be unnecessary (lambdas already scope correctly). The cost is that
fixity would have to be looked up by *unresolved* name, which can disagree with resolution in
pathological cases (two imported `+`s of different fixity). It is sound only if Eliot guarantees
fixity is unique per operator name globally; absent that rule, the post-resolve placement above is
the sound default.

## Tests

Whole-pipeline via `ProcessorTest(LangProcessors()*)`, single-line asserts:

1. **Parse:** a block parses to `BlockExpression` with one over-separated `BlockLine` per source line.
2. **Lowering:** a block lowers to nested immediately-applied lambdas (assert the resolved body
   shape).
3. **Effect threading end-to-end:** `swap` in block form compiles and yields the same monomorphized
   result as the hand-written `flatMap` nest.
4. **`val` binds the carried result:** `val line = readLine` then `println(line)` type-checks.
5. **Termination:** a block passes `RecursionCheckProcessor`; a value self-referential through a
   block is still flagged.
6. **Self-reference error:** `val x = f x` reports "referenced in its own definition".
7. **Errors:** a block ending in a `val`, and an empty block.

Merge tests (`BlockDesugaringProcessor`):

8. **Leading-dot chaining:** `foo`↵`.bar`↵`.baz` merges to one expression `foo.bar.baz`.
9. **Trailing operator:** `base +`↵`adjustment` merges.
10. **Alphanumeric infix:** `a`↵`or b` merges (declared `infix`), not two statements.
11. **Prefix starts a statement:** `flag`↵`! other` does not merge — two statements `flag` and
    `!other`.
12. **Plain sequence:** `getState`↵`putState(next)`↵`old` is three statements (all `Application`).
13. **Blank-line override:** a blank line is never merged, even where fixity would otherwise join.
14. **No merge into a `val`:** a line is never absorbed into a following `val` binding.

## Decisions and deferred

- **Self-reference in a `val` RHS is an error**, not shadowing of an outer binding (decided).
- **Discarded pure statement** (a non-final statement with no effect) is dead code under purity;
  harmless, so a candidate lint warning, not an error, and not required for the first cut.
- **Error recovery inside a half-written block** (for IDE hints) follows the general parser-recovery
  work in `docs/ide-type-hints.md`, not this change.
