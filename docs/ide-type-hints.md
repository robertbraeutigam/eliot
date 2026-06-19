# IDE Type Hints for Incomplete Expressions

## Goal

Let an IDE answer "what is the type at `(uri, line, col)`?" even while the source has parse
errors or ill-typed sub-expressions — including inside the expression being edited. This is what
makes hover, signature help, and "expected type here" work on in-progress code.

## Current state

The `ide/lsp` server provides diagnostics, go-to-definition, hover, in-scope completion, and a
"Run main" code lens over live (unsaved) buffers.

Concrete-type hover is driven by `TypeHintIndex` (`ide/lsp/.../index/`): it maps a source position
to the concrete monomorphic type each node was checked at — the per-node `expressionType:
GroundValue` carried by `MonomorphicValue` facts. It is rebuilt after every compile from
`generator.currentFacts()`; hover prefers it and falls back to the referenced value's declared
signature.

This defines the remaining work: **a node is typed only when its whole monomorphization cone
compiles without error**, because a fact that hits an error is never materialised, so it never
reaches `currentFacts()`. Making facts survive errors is therefore *sufficient* — the existing
index surfaces the recovered hints with no new wiring. Hover is empty (never wrong) on a broken
buffer.

## The blocker

Errors stop facts from being produced in three places:

- `getFactOrAbort` (`CompilerIO.scala`) short-circuits the whole `CompilerIO[T]` when an upstream
  fact is missing.
- `registerFactIfClear` (`CompilerIO.scala`) drops the produced fact if any error was registered in
  the current branch, so partial facts never reach the cache.
- `compilerAbort` / exception-style abort inside processors (e.g. the `Evaluator`).

`recover` (`CompilerIO.scala`) already exists and is the right primitive; it just isn't used widely
enough.

The parser already keeps partial ASTs (`ParserResult.value: Option[A]` alongside `currentError`)
and does span-boundary recovery (`Parser.recovering*`), but `ASTParser` discards the partial AST
when `value` is empty.

The live-buffer overlay (`ide/lsp/.../virtual/`) already type-checks the current unsaved buffer;
what is missing is tolerating its *broken* syntax rather than dropping the whole file's facts.

## Layer A — make facts survive errors (partial facts)

1. Add `registerFactAlways` alongside `registerFactIfClear` in `CompilerIO`, for facts that carry
   their own optionality (children may be `Unknown`).
2. Add an `Unknown` marker:
   - `resolve.fact.Expression.Unknown(range, reason)` for nodes resolve can't pin down.
   - `monomorphize.fact.MonomorphicExpression.Expression.Unknown(range)` with `expressionType =
     GroundValue.Unknown` for nodes the monomorphizer can't type.
   - `GroundValue.Unknown` is a bottom-like type: it unifies with anything but propagates rather
     than collapsing the enclosing expression.
3. Convert local `getFactOrAbort` to `getFact + recover` in the per-expression processors
   (`ValueResolver`, the `Evaluator` / `monomorphize` checker, `OperatorResolver`, `MatchDesugarer`):
   ```scala
   recover(processChild(child))(default = Expression.Unknown(child.range, ...))
   ```
   Module/file-level processors keep aborting — don't invent a fake module to type one expression.
4. Update all pattern matches for the new `Unknown` cases. `used` and `uncurry` must refuse to
   operate on expressions containing `Unknown` and emit them as errors, so the backend never
   codegens a hole.

## Layer B — make the parser tolerant of the current expression

1. Stop aborting on `astResult.value.isEmpty` in `ASTParser`: register the partial AST plus the
   collected `ParserError`s. A file that fails entirely becomes an empty `Module`, still valid
   downstream.
2. Add expression-level recovery points (today recovery is at span boundaries only), so an
   unfinished `let foo = bar.|` still yields a value definition whose body is `Expression.Unknown`
   plus an error.
3. Synthesize "expected here" placeholders: when an application argument, infix operand, or `match`
   scrutinee fails to parse, insert `Expression.Unknown(range)` and continue, so resolve/monomorphize
   still produce `MonomorphicExpression` for the surrounding context.

## Expected-type hints

Once Layers A/B produce `Unknown` nodes: when the monomorphizer meets `Expression.Unknown` as e.g.
the second argument of `f : Int -> String -> Bool`, the expected type (`String`) is known — emit it
for the hole's range. `TypeHintIndex` needs to carry an expected-vs-actual flavour on a hint; no new
processor or driver wiring.

## Order

1. Layer A.1–A.2 (`registerFactAlways`, `Unknown` markers, `GroundValue.Unknown`) — mechanical, no
   behaviour change.
2. Layer A.3–A.4 (rewrite the per-expression processors; update matches; gate
   `used`/`uncurry`/backend on `Unknown`). Test that sibling expressions still type-check, and that
   hover still produces a hint on a buffer with one broken sub-expression.
3. Layer B (parser tolerance) — types the expression being edited.
4. Expected-type hints — add the expected/actual flavour to `TypeHintIndex`.

## Risks

- `GroundValue.Unknown` must be a true top-of-lattice that never causes spurious unification
  successes; it likely needs a dedicated case in the `monomorphize` unifier with an "unreliable"
  flag, so downstream hints are marked low-confidence rather than shown as a wrong type.
- Ability resolution aborts hard when no implementation is found; for IDE use it should fall back to
  the ability's declared signature (Layer A treatment inside the `ability` phase).
- Span-boundary recovery is coarse; useful hints inside half-written blocks need genuine
  expression-level recovery rules — the riskiest part of Layer B.
- Need a test helper that drives the index pipeline (or `CompilationSession`) over a broken snippet
  and asserts on `TypeHintIndex` rather than on bytecode.

## Files

- `eliotc/.../processor/CompilerIO.scala` — `registerFactAlways` (and maybe an `Unknown`-aware
  `getFactOr`).
- `lang/.../resolve/fact/Expression.scala` — `Unknown` case + matches.
- `lang/.../monomorphize/fact/{MonomorphicExpression.scala, GroundValue.scala}` — `Unknown` cases +
  unifier change.
- `lang/.../{operator,matchdesugar,ability,monomorphize}/...` — `getFactOrAbort` → recovering
  variants on per-expression branches.
- `lang/.../ast/parser/{ASTParser,Parser}.scala` — partial AST registration, expression-level
  recovery.
- `lang/.../used/*`, `lang/.../uncurry/*` — error on `Unknown`.
- `ide/lsp/.../index/TypeHintIndex.scala` — expected/actual flavour for expected-type hints.

## Deferred follow-on: implicit-generics propagation & explorability

Separate from error recovery, gated on Layer A (partial facts) and the existing `TypeHintIndex`. The
`auto`/implicit-generics feature itself is shipped (input and data-field generalization, calculated
returns, limit diagnostics; any limit that can't be crossed hard-errors at the use site).

Propagation & display:

- **Transitive bounds through nested records.** Make `SaturatedValueProcessor.inferableInfo`'s
  `fieldContribution` read the grown arity (`inferableInfo`) instead of the raw arity, guarding the
  recursive lookup against cycles, so `data Outer(inner: Counter)` grows `Outer` by `Counter`'s
  bounds.
- **Calc-return over a `Combine` join.** `double(pick(a, b))` can't ground the callee's type args at
  the call because the join resolves only at drain; postpone calc-return resolution into
  `drainAndResolveLoop` so the join grounds first. (Today `Checker.readMonomorphicReturn` hard-errors
  via `reportUngroundCalculatedReturn`.)
- **Symbolic `ElaboratedSignature` fallback** for never-called producers: check the body with input
  binders neutral, forward-evaluate (`x + x ⤳ Int[add MIN MIN, add MAX MAX]`), quote the symbolic
  return. Kept off the from-`main` compile path.
- **Readable, stable binder names** instead of `$Int$0`/`$Counter$1`
  (`SaturatedValueProcessor.freshName` / `TypePlan.binderName`).

Explorability (legible dependent signatures, by example):

- **Real-usage examples.** Aggregate `MonomorphicValue(fqn, *)` facts and show the in→out set at a
  definition; needs `TypeHintIndex` to hold a set of hints per range.
- **Generators (type-position types only).** A finite enumeration of representative bound
  instantiations — a boundary-focused canonical set for `Int` hitting each `Jvm*` tier and the
  cross-tier edges, structurally derived for `data`. Produce examples by speculatively requesting
  `MonomorphicValue(fqn, generatedArgs)`.
- **Probing.** Each generated/real instantiation either monomorphizes (example) or registers an
  error (counterexample) to surface at the definition. Sandboxed: small samples, per-probe step/time
  budget, "didn't finish" reported as unknown, never a false error. A counterexample finder, not a
  totality proof.
- **Presentation ideas.** A differential inlay on a bare return, "view-through" a pinned
  instantiation, a totality CodeLens, a representation-transition view marking `Jvm*` tier crossings.
- **Where.** Extends `ide/lsp/.../index` — `TypeHintIndex` → set-per-range; a generator deriver from
  `SaturatedValue` signatures; a probing driver issuing budgeted speculative `MonomorphicValue`
  requests.
