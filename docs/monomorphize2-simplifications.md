# monomorphize2: simplifications and stock-algorithm evaluation

Architectural commentary on the `monomorphize2` design as it stands after the polytype-instantiation fix (solver instantiation rule, extractor `vr.typeArgs` fold, processor `ShortUniqueIdentifiers`-replayed walk). This document is **analysis, not guidance** — it does not propose changes that have been agreed to. For operational rules, see `SKILL.md`.

## What we have, named in stock terms

The current `monomorphize2` is essentially **constraint-based Hindley-Milner type inference** with two non-stock extras:

1. **βη-aware unification** — the solver's beta-reduction and polytype instantiation cases are HM's `Inst`/`Gen` rules made concrete on ORE. Robinson unification with explicit β-reduction.
2. **Evaluation fallback** — `evalAndCompare` handles type equality modulo computation, which HM proper doesn't need but a language with type-level functions does.

The closest textbook label is "HM constraint generation + first-order unification + βη + a normalization fallback". There's no famous single algorithm with exactly this shape because most languages with this requirement go all the way to NbE.

## Simplifications, ranked by leverage / risk

### 1. Replace the `ShortUniqueIdentifiers` replay with an annotated body — high leverage, low risk

This is the biggest tidy-up available. The processor's `walkBody` exists *only* to recover the per-position fresh-var names the extractor allocated, by replaying the same `generateNext` calls in the same order. That's fragile (silently breaks on extractor walk-order changes), it requires `advanceForExpression` to mirror extraction exactly, and it forces the processor to know intimate details of extraction.

The information the processor really wants is: "for this ORE node, what was the assumed-type expression at extraction time?" The extractor *already computes* that — it constructs `funcType = Function(?argTypeVar)(?retTypeVar)` in the FA case and passes it down. It just throws it away.

**Simplification**: have `TypeCheckState` carry a side-table — `Map[NodeId, OperatorResolvedExpression]` keyed by some node identity (cheapest: the `Sourced` position; most robust: a synthetic id assigned during extraction). At each FA/FL in extraction, store the assumed type for the current node. The processor then reads it directly:

```scala
case ValueReference(vfqn, _) =>
  val assumedOre = endState.nodeAssumedTypes(nodeIdOf(source))
  val resolved   = solution.resolveExpressionValue(Evaluator.evaluate(assumedOre))
  ...
```

What this kills:
- All of `advanceForExpression`.
- The "match the extractor's `generateNext` order exactly" invariant.
- The "the parent's `assumedType` *is* the result type by the `assumedType := retTypeVar` invariant" coupling — that becomes a property of the recorded data, not an implicit contract.
- The need to keep `WalkIO = StateT[CompilerIO, ShortUniqueIdentifiers, _]` in the processor at all.
- One full chunk of the SKILL document's "diagnostics" section (the "wrong concrete type" / "drift out of sync" entries become impossible).

What it adds: a `Map[…, ORE]` field in `TypeCheckState` and a small write at FA/FL/FL-with-annotation cases. Maybe 30 lines of extractor code, deletes 80 lines of processor code.

The `MonomorphicTypeCheckProcessor.scala:107` ValueReference case becomes 3 lines instead of the current carefully-staged FA-driven walk. The whole `WalkIO` monad goes away. **This is the simplification worth actually doing.**

### 2. Drop the dual roles of `ParameterReference` — medium leverage, medium risk

`ParameterReference` in ORE has two meanings inside the type checker: a real user parameter, and a unification variable. The skill calls this out as a "dual role", but it's a real source of subtle bugs (occurs check confuses the two; the `Solution` map mixes them).

A separate `UnificationVar` constructor in ORE — *only used inside `monomorphize2`* — would make the code easier to reason about. The solver could distinguish "bind this metavariable" from "rename this parameter" without string-suffix conventions like `$`.

**Cost**: adding an ORE node violates rule "Do not try to add a new ORE node, just because it would be convenient in `monomorphize2`" from the SKILL document. The skill explicitly bans this. So this is more of an architectural debt observation than a recommendation.

If that ban were ever lifted, the right move would probably be to introduce a `MetaVar` ORE constructor scoped to the type-checker and have `ConstraintExtract` and `ConstraintSolver` use it instead of `ParameterReference`. The current `ShortUniqueIdentifiers` `$`-suffix scheme is exactly the kind of "string-typed" workaround that vanishes.

### 3. Fold the type-stack walk and the body walk into one — low leverage, low risk

The constraint extractor walks the type-stack levels and *then* the body. The two walks share almost all their code (`collectConstraints` is called for both). The split exists because the type-stack walk provides the assumedType for the body walk.

This isn't really a simplification — it's already nearly one walk. But noting that the type-stack walks could be eliminated entirely if signatures were always pre-evaluated (Phase 3 already does `Evaluator.evaluate(resolvedValue.typeStack.map(_.signature))`). The constraints from the type-stack walks essentially re-derive the kind structure, which the eval package already knows.

If you trusted the evaluator's view of the signature, the type-stack walk could be replaced with: "evaluate the signature; that's the body's expected type." No constraint generation needed for the type stack. The kind-checking constraints from the type-stack walks (`C$ := Type, A$ := Type, …`) aren't actually doing useful work — the evaluator has already kind-checked the signature.

**Risk**: the type-stack walks might be doing something subtle — e.g., they ensure that fresh type-parameter names get into the `Solution` so the body walk can reference them. Specifically, this is how `?A -> BigInteger` ends up in the Solution for `def f[A](a: A): A` with `typeArgs = [BigInteger]`. So removing them is more involved than it first looks.

Probably not worth it on its own, but worth thinking about if constraint extraction is ever revisited.

### 4. Replace the constraint solver with bidirectional checking — high leverage, high risk

Stock algorithm: **Dunfield-Krishnaswami "Complete and Easy" bidirectional type checking** (ICFP 2013). It handles polytypes (impredicative or predicative) without a separate solving phase. Each node is in either *check* mode (given an expected type) or *synthesize* mode (produces a type). Polytype instantiation happens at value-reference sites in synthesize mode. Unification is local and incremental.

The processor's current `buildExpression` walk *already* runs in something close to check mode — it carries `assumedType` top-down. So the processor is half-bidirectional already; what it lacks is the synthesis half (it gets that from the solver via the replay).

A full bidirectional rewrite would:
- Eliminate `ConstraintSolver` entirely.
- Eliminate `ConstraintExtract`'s constraint-emission role; constraint extraction becomes "unify locally as you walk".
- Merge the three-phase pipeline into one phase: walk the body once, in check or synthesize mode, producing a `MonomorphicExpression` directly.

**Why not now**: the existing design is starting to converge on something that handles dependent types (the `evalAndCompare` fallback). Bidirectional HM doesn't naturally accommodate type-level computation. The right stock algorithm for *that* direction is **NbE-based elaboration** (Lean / Idris / smalltt style), which is a much bigger change.

Also: the current architecture's three-phase split (extract → solve → substitute) is clear and debuggable, even if the substitution phase is more complex than it should be. Bidirectional collapses everything into one walk, which is harder to debug and harder to add features to.

### 5. Replace the structural solver with NbE-based unification — very high leverage, very high risk

Stock algorithm: **Coquand's algorithm** for type checking with dependent types, modernized as **Normalization by Evaluation** in tools like Lean 4, Idris 2, Agda, smalltt.

The shape:
- The evaluator becomes the canonical normalizer. It must support **metavariables** (unification placeholders) as first-class `ExpressionValue` nodes.
- "Unify" on two `ExpressionValue`s: normalize both to weak head NF, compare structurally, recurse on children. When a metavariable meets a concrete value, solve it (with **pattern unification** restrictions à la Miller).
- All beta-reduction, η-expansion, and polytype instantiation happen inside the evaluator. The "solver" disappears as a separate component; it becomes just `unify(eval(left), eval(right))`.

This is **the** right architecture for a language with dependent types and type-level computation. It's what Eliot is heading toward, based on the `evalAndCompare` fallback hint.

**Why not now**:
- Requires the `eval` package to learn about metavariables, which is a substantial change to a stable component.
- Collapses the very-clean ORE-vs-Value separation that the SKILL document treats as foundational.
- Multi-month rewrite.

If/when the dependent-type story actually matures, this is where to go. Until then, the current hybrid is a sensible halfway house.

### 6. Reuse `eval` package's substitution machinery — very small leverage

The solver uses `OperatorResolvedExpression.substitute` for beta-reduction and instantiation; the `eval` package has `ExpressionValue.substitute`. They are mostly parallel implementations of capture-avoiding substitution at different representation levels.

This isn't really fixable without breaking the ORE/Value world separation. The duplication is a consequence of having two worlds, which is itself a deliberate design decision. Worth noting only as evidence that the two-worlds design *does* impose a cost.

## Recommendation

**Do simplification 1 when there's appetite for it.** Annotate the ORE in `TypeCheckState` with a `Map[NodeId, ORE-assumed-type]` during extraction; have the processor read from it instead of replaying `ShortUniqueIdentifiers`. This:

- Removes the most fragile coupling in the current code (~80 lines of processor walk + the `WalkIO` monad).
- Doesn't change any algorithmic behavior — it's purely a refactor of how info flows from extract to processor.
- Makes the SKILL document substantially shorter and easier to follow.
- Doesn't preempt any of the bigger architectural moves (2 / 4 / 5) — those would still be options later.

**Don't do 2 / 3 / 4 / 5 yet.** They're either banned by current rules, not really simplifications in disguise, or correct directions but premature given that monomorphize2 is still stabilizing on basic test cases.

**The dependent-type direction is real.** When the language genuinely needs the eval-based equality fallback for non-trivial cases, the right move will be to bite the bullet on simplification 5 (NbE-based elaboration with metavariables in the eval package). That's what Lean / Idris / Agda all do, and it's the only known stock algorithm that handles this cleanly. Until then, the current hybrid is fine.
