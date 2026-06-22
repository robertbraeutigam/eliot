# Monomorphization keying: codegen-relevant dedup + non-convergence backstop

## Problem

`MonomorphicValue.Key` is `(vfqn, typeArguments: Seq[GroundValue])` with `GroundValue` compared
by universal equals (`monomorphize/fact/MonomorphicValue.scala:29,37`,
`monomorphize/fact/GroundValue.scala:88`). So two instantiations that differ only in a
compile-time-only index produce **distinct** specializations even when the generated code is
identical. This explodes in two ways:

1. **Size-indexed recursion.** A recursive value over a size-bounded structure
   (`sum : List[A,N] -> ...`, `map`) recurses internally as `f[N] -> f[N-1] -> …`. Every step is a
   new key ⇒ O(N) (or unbounded) specializations. `RepresentationLowering` only dedups widths in
   `uncurry`, *after* the cost is paid.
2. **Compile-time data through type args.** Any structure threaded through type arguments for
   resource tracking / analytics / proofs (a `Map`, a measure, reified statistics) multiplies the
   key the same way.

There is also **no evaluator guard** (`eval/Evaluator.scala` `applyValue`/`unfoldTopDef`): recursion
is stopped only by *stuckness* (a neutral argument becomes a residual, never unfolds); a recursion
forced on a concrete argument unfolds without limit. The only existing guard is the
calculated-return FQN-cycle detector (`check/Checker.scala:851-860`), which hard-errors and is
correct only for calculated returns.

## Key insight — where to cut

`N` is **codegen-irrelevant** (erased at runtime — `map`'s body is a loop over a runtime list,
`N`-agnostic) but **type-relevant** (it appears in `map`'s signature and in calculated returns a
caller reads via `signature.deepReturnType`, `check/Checker.scala:859`). The same parameter is
phantom for *code* and load-bearing for *types* ⇒ **one key cannot serve both**.

Therefore:

- **Keep the `monomorphize` / type-checking key full** (exact `typeArguments`). Signatures and
  calculated returns stay exact. The checker is unchanged.
- **Dedup on a codegen-relevant projection at the codegen boundary** (`used` → `uncurry` →
  backend). That is where the explosion lives: checking a recursive call against an *explicit*
  return type does **not** demand the callee's `MonomorphicValue` (recursive *calculated* returns
  are already rejected at `Checker.scala:856`), so the per-`N` blow-up is driven by the `used`
  traversal (`used/UsedNamesProcessor.scala:34`), not the checker. Collapse there and `f[100]`,
  `f[99]`, … fold to one codegen node via a back-edge.

Soundness rule throughout: **never merge two instances whose generated code differs.** Collapse only
provably-phantom positions. Imprecision (failing to collapse) is caught loudly by the backstop, never
a silent miscompile.

## The three categories, and which can actually explode

- **Phantom** (`Rec[N]`, size index, proofs): erased, no runtime form. Collapse by dropping from the
  key.
- **Representation-determining** (`Int[auto MIN, auto MAX]` picking a width): the bounds *do*
  influence code, but this category is **self-limiting** — there are finitely many machine
  representations, and `representationOf` collapses every bound to one of them. `Int[0,100]` ≡
  `Int[0,50]` (both byte), `Int[0,100000]` distinct. The whole space is bounded by the program's
  finite type set (barring unbounded *type-level* recursion, a separate pathology). Dispatch is
  likewise bounded by the finite number of ability instances.
- **Reified value** (a compile-time value materialized into the body — `PostDrainQuoter` Stage 1/2 —
  e.g. usage statistics printed at runtime): each distinct value is genuinely different code, and the
  count is bounded only by *how many distinct values the program produces* — which, via recursion, is
  **unbounded and non-collapsible** (we need all the values).

Sharp claim: **only reified value-multiplicity is unboundedly explosive; every type-role
multiplicity is bounded by the program's finite type structure.** So the reified case is the one and
only place where collapse is impossible — and it gets a different disposition (below).

## Per-parameter dispositions

Each type-parameter position of a definition gets exactly one disposition:

| class | disposition | bound |
|---|---|---|
| phantom | **collapse-erase** — drop from key, no runtime form | n/a |
| representation | **collapse-to-representation** — key on `representationOf` (width class) | finitely many reps |
| dispatch | **specialize** | finitely many instances |
| reified, recursion-**invariant** | **specialize** | finite call sites (modulo transitive multiplicity) |
| reified, recursion-**variant** | **demote-to-runtime** — drop from key, *retain as a runtime value parameter*; call sites pass their (constant) value | one body |

**Demotion = the reified twin of erasure.** Both keep a per-iteration-varying quantity at runtime
instead of specializing per value; they differ only in whether the quantity must physically exist at
runtime (phantom: erased; reified: a runtime parameter). Demoting a recursion-variant reified
parameter is trivially sound — it is *literally an ordinary recursive value parameter* the compiler
had chosen to bake in; un-baking it is correct. Mechanically it is the same key-collapse as a
phantom, plus one bit: the dropped position survives as a runtime argument in the monomorphic
signature. **The multiplicity is real and all the values matter — but it belongs in the data (N
call-site constants, one body), not the code (N bodies).**

This is the **Swift / .NET model**: runtime-parameterize by default, specialize as an optimization,
with the trigger here being a code-size budget. (Rust/C++/MLton monomorphize-always and accept the
bloat; Rust's escape is manual `dyn`.)

## Where each decision runs — three layers (the cost model stays out of NbE)

Two distinct "unrollings" must not be conflated:

- **Breadth** — the *number* of `MonomorphicValue` facts (`f[100], f[99], …`). This is the fact
  graph going wide; each individual body checks and terminates cleanly (the recursive argument is a
  neutral runtime value ⇒ the body's `match` is stuck ⇒ NbE does **not** unroll). The cost model and
  demotion live here, **entirely at the fact layer, outside NbE.**
- **Depth** — a *single* NbE `force` unrolling a genuinely recursive *type-level* function
  (`Vec[1000000]`). This is inside the evaluator.

```
PRE-NbE  (static, on resolved body + call graph — no evaluation):
  relevance class + recursion-variance  ->  per-parameter disposition
  recursion-variance = "is param p passed a non-identity value on a back-edge?"
  (a syntactic check over the call-graph SCCs — REUSES the Rec/Inf cycle detector)
        │  decides collapse-erase / collapse-rep / specialize / demote up front,
        │  with NO cost computation — the unbounded shape is detected syntactically
        ▼
DURING mono, FACT LAYER  (observes breadth; not inside NbE):
  online budget / active-fact-chain counter  ->  safety net for *transitive* multiplicity
  (a recursion-invariant reified p whose caller is itself instantiated many ways)
  on trip: demote-or-error
        ▼
INSIDE NbE  (separate concern, unrelated to the cost model):
  dumb step/depth limit on unfoldTopDef  ->  guards genuine type-level recursion
  trips => nontermination ERROR (never a demotion); = the Rec/Inf termination effect's job
```

This dissolves the chicken-and-egg ("need the cost before monomorphizing, but the cost needs
monomorphizing"): you never compute the count. The only unbounded source — a reified parameter that
varies around a recursive cycle — is a **shape** caught syntactically *before* NbE; everything else
is type-bounded; the online budget mops up transitive multiplicity. NbE keeps only a step-limit
safety guard, which is needed regardless and is the termination effect's eventual responsibility.

## Deliverable 0 — characterization test suite (FIRST STEP)

Before changing anything, pin down empirically **how the compiler reacts today** to each scenario:
does it compile, error, or hang, and **how many versions does it unroll?** This is the baseline the
fixes are validated against (counts go N→1 where collapse/demote should happen, stay N where the
instances are genuinely distinct), and it doubles as discovery — it reveals which scenarios are even
expressible today (recursion is not yet a built feature) and which hang.

**Harness.** A new `ProcessorTest` subclass (e.g. `MonomorphizationVersioningTest` in
`lang/test/.../monomorphize/`) wiring the pipeline through `MonomorphicTypeCheckProcessor` and, for
end-to-end breadth, `used` + `uncurry`. `runGenerator(source, trigger, imports)` returns all
materialized facts; trigger the codegen root so the full reachable breadth materialises, then count:

```scala
def versionCounts(facts: Map[CompilerFactKey[?], CompilerFact]): Map[ValueFQN, Int] =
  facts.values.collect { case mv: MonomorphicValue => mv }.groupBy(_.vfqn).view.mapValues(_.size).toMap
// (mirror with UncurriedMonomorphicValue for the post-uncurry codegen-unit count)
```

Wrap each compile in `IO.timeout(…)` so a divergent scenario fails the test instead of hanging the
suite; treat a timeout as the documented "diverges today" outcome. The `intStubContent`
(`type Int[auto MIN: BigInteger, auto MAX: BigInteger]`) makes the bounds scenarios directly
writable. A scenario that is not yet expressible is written as the *intended* program and marked
pending/ignored with a note — recording the gap is part of the baseline.

**Scenarios** (each asserts the current count + a comment with the target count):

| # | scenario | today (characterize) | target |
|---|---|---|---|
| S1 | recursion over a size-bounded structure (`sum`/`map`, Cat 1 phantom) | explosion / hang | 1 |
| S2 | two call sites `Int[0,100]` & `Int[0,50]` (Cat 2, same width) | 2 | 1 |
| S3 | `Int[0,100]` & `Int[0,100000]` (Cat 2, different width) | 2 | 2 (must stay) |
| S4 | reified value at K finite call sites (Cat 3, recursion-invariant) | K | K (correct) |
| S5 | reified value varying per recursion (Cat 3, recursion-variant) | unbounded / hang | demote → 1 |
| S6 | two types, same representation, different ability impl (dispatch) | 2 | 2 (must stay — soundness) |
| S7 | divergent recursion (no base case) | hang (timeout) | backstop error |
| S8 | recursion on a runtime value, invariant type (control) | 1 | 1 (regression guard) |

S3, S6, S8 are the **must-not-over-merge** guards; S1, S2, S5 are the wins; S7 motivates the
backstop. The suite is the acceptance gate for Deliverables A and B.

## Deliverable A — non-convergence backstop

A safety net that converts a runaway specialization into a diagnostic (today a divergent recursion
hangs / OOMs). Independent of B; valuable on its own and the trip-wire that feeds demotion's online
path.

- **Where:** the codegen driver — `used/UsedNamesProcessor.scala`. Optionally mirror at the producer
  (`monomorphize/processor/MonomorphicTypeCheckProcessor.scala`).
- **Mechanism:** reuse the active-fact chain (`processor/CompilerIO.scala:42` `activeFactKeys`,
  populated by `compiler/IncrementalFactGenerator.scala:50`). Count *nested* entries on the chain
  sharing the same `vfqn` (generalizing the FQN match at `Checker.scala:852` from "any repeat =
  error" to "more than K nested repeats"). Past a generous threshold (configurable; default e.g.
  500) emit a specific error and `abort`, modelled on `reportRecursiveCalculatedReturn`
  (`Checker.scala:862`):

  > Monomorphization of '<name>' is not converging: specialized <N> levels deep with differing type
  > arguments. Argument(s) <diff of consecutive keys> change on every step — make them compile-time
  > only (phantom), bound the recursion, or demote them to runtime.

- Post-B, legitimate recursion collapses (phantom) or demotes (reified) and never trips; the backstop
  then only fires on genuinely divergent type-level recursion or analysis-missed positions. It is the
  fail-safe behind the relevance/variance analysis (cf. gaps-must-be-failsafe).

## Deliverable B — codegen-relevant projection + dispositions

### B1. Relevance + recursion-variance analysis (per `vfqn`, static, pre-NbE)

On the resolved/saturated body + call graph — no evaluation. Classify each type-parameter position:

- **R1 reified** — appears in **value position** in the body (`check/PostDrainQuoter.scala` Stage 1/2).
- **R2 dispatched** — drives an ability-instance selection.
- **R3 representation** — changes the machine representation (what `representationOf` consumes).
- **recursion-variant** — passed a non-identity value on a recursive back-edge (call-graph SCC check,
  shared with the `Rec`/`Inf` cycle detector).

Default disposition follows the table above. **R4 is deliberately NOT a rule**: merely appearing in
the signature does not make a parameter codegen-relevant — that is the `map: List[A,N]->List[B,N]`
case, type-relevant but code-phantom, exactly what we must collapse. Start maximally conservative
(only obvious phantoms collapsed); the backstop covers imprecision.

Carry the result on `SaturatedValue` (forward onto the existing fact per lean-fact-flow) —
`MonomorphicTypeCheckProcessor` already reads `SaturatedValue.Key(vfqn)`
(`MonomorphicTypeCheckProcessor.scala:18`), so it is available where keys are built.

### B2. Projection function

`codegenProject(vfqn, fullArgs): Seq[GroundValue]` — deterministic, shared by all demand sites:

- **collapse-erase / demote** positions: dropped from the key (demote additionally retains the value
  as a runtime parameter — see B3);
- **representation** positions: canonicalized via `RepresentationLowering.representationOf` so
  width-equivalent bounds collapse, **while preserving the nominal head** for dispatch (do not key
  purely on the bare representation — see soundness checkpoints);
- **specialize** positions: kept verbatim.

### B3. Wire through the codegen pipeline (checker untouched)

- `used/UsedNamesProcessor.scala`: dedup the traversal on `(vfqn, codegenProject(args))`; expand a
  **representative** `MonomorphicValue` for the body. For **demoted** positions, splice the dropped
  value back as a runtime argument (the demoted value flows from data, not the type key).
- `uncurry`: key `UncurriedMonomorphicValue` on the projected args
  (`uncurry/fact/UncurriedMonomorphicValue.scala:39`,
  `uncurry/processor/MonomorphicUncurryingProcessor.scala:22-23`) and rewrite call-target references
  in bodies to projected keys (passing demoted values as ordinary arguments).
- Backend: one class/method per projected key falls out of the above.

`MonomorphicValue` / `Checker` / `TypeStackLoop` are unchanged — they keep producing exact,
full-args facts for type reasoning.

### B4. Demote policy (open knob)

Demotion trades code for **RAM/ROM** (the value now lives at runtime) — exactly the cost a
resource-bounded target wants *explicit and provable* (cf. the `TODO.md` "guaranteed to fit all
resources" goal). So demotion must be **visible, never silent**. Recommended default:
**auto-demote-with-a-diagnostic**; on strict embedded targets prefer **error-and-require-an-explicit
annotation** (force the user to choose), i.e. a per-target policy or per-parameter annotation.
*Decision pending.*

### Soundness checkpoints (verify before relying on a collapse)

1. **Dispatch on bounds.** If a bound-indexed ability instance can exist
   (`implement Foo[Int[0,100]]` distinct from `Foo[Int[0,50]]`), canonicalizing bounds to a width
   class would merge distinct dispatch ⇒ treat such a bound as **R2-relevant**. Confirm whether
   bound-indexed instances are admissible.
2. **Opaque types sharing a representation.** Two distinct opaque types both lowering to `JvmByte`
   but with different abilities must not merge ⇒ projection preserves the **nominal head**, never
   keys purely on `representationOf`'s output.
3. **Reified params (R1) always kept or demoted, never silently merged.** A bounded reified family
   stays distinct; an unbounded one demotes; neither collapses to a single baked-in constant.

## Validation

Deliverable 0's suite is the gate. After A+B the counts must move: S1/S2 → 1, S5 → demote→1, S3/S6/S8
unchanged, S7 → backstop error (not hang). Plus full suite green and a jvm end-to-end check (generated
class count) for a recursive `sum`/`map`.

## Order of work

0. **Characterization test suite** (Deliverable 0) — baseline + discovery; the acceptance gate.
1. **Backstop** (Deliverable A) — independent, immediately useful, de-risks B and feeds demotion.
2. **B1** relevance + recursion-variance analysis (start maximally conservative).
3. **B2/B3** projection + wiring through `used`/`uncurry`/backend, including the demote disposition.
4. Resolve **B4** policy; tighten B1 as the suite/benchmarks justify; the backstop covers residual
   imprecision.

## Cross-refs

`TODO.md` → *Compiler architecture & tooling*. Grew out of the recursion-as-effect / `Rec[N]`
discussion: `Rec`'s size index is the first instance of a codegen-phantom type argument, and "erase
`N` before mono" is the special case of this general keying fix — so the recursion/termination
processor never has to strip anything itself, and its cycle detector is reused by B1's
recursion-variance check.
