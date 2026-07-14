# Type levels as named values (`TypeLevel`) — dissolve the type-stack loop

**Status:** PLAN (2026-07-14, not yet implemented). Supersedes `return-position-unification.md` §4 ("Stage 2")
— its Attempt-1 findings section is the evidence record for this plan; Stage 1 (`calculatedReturn` removal) is
landed on master and unaffected. The Stage-2 WIP branch (`wip/return-position-unification-stage2`) is **not
merged**; only two pieces are salvaged (Step 0).

## 0. Principle (Robert, 2026-07-14)

> The unit of execution on every platform is a **named value**, not an expression. The NbE `Evaluator` is the
> *kernel*, not the platform: an expression runs by becoming (part of) a value's **body**. "Types are values" must
> therefore hold *representationally*, not just semantically — the type of a value is a named value one
> **TypeLevel** up, compiled by the same pipeline as every other value.

Why this principle was missing, in one paragraph (full evidence: `return-position-unification.md` §4 Attempt-1
findings, and the 2026-07-14 architecture review): only a named value passes through saturation → the effect
phase → check-mode elaboration (`EffectLifter`) → carrier pinning → ability resolution → per-instantiation match
reduction. A signature slot gets none of that on *any* track — it gets only `evalExpr`/`renormalize`, the bare
β/δ+native-refire kernel. Match reduction in particular *cannot* be added to the kernel: `MatchNativesProcessor.stuck`
mints `VNeutral(Reserved(Match), …)`, which drops the native reducer — there is no FQN left to re-fire, and adding a
dispatch rule to `renormalize` would duplicate the match native (the second-evaluator anti-pattern). Three guard
attempts failed by re-implementing platform features inside the checker one at a time and all bottomed out at this
boundary. `orError` needed zero checker machinery for one reason only: it is a named value.

## 1. The model

Named-value identity gains a **TypeLevel** dimension, parallel to `Qualifier`:

- **TypeLevel 0** is the runtime body. It runs on the **runtime** track.
- **TypeLevel n ≥ 1** is a type expression. It runs on the **compiler** track.
- Every expression has a TypeLevel+1 expression — its type — except the literal `Type`, which is the top.
- The value at level *n* has **body** = the level-*n* expression and **signature** = the level-*n+1* expression.

Numbering (shifts today's `TypeStack` indexing by one — adopt this convention everywhere):

| TypeLevel | Content                          | Today's representation            | Track    |
|-----------|----------------------------------|-----------------------------------|----------|
| 0         | runtime body                     | `NamedValue.runtime`              | runtime  |
| 1         | the signature                    | `TypeStack.levels(0)` (signature) | compiler |
| n ≥ 2     | kind levels                      | `TypeStack.levels(n-1)`           | compiler |
| top       | literal `Type`                   | implicit                          | —        |

The payoff: **`TypeStackLoop` is revealed as not primitive.** "Check level *n*'s expression against the evaluation
of level *n+1*" is the ordinary "check a value's body against its signature," applied recursively up a finite
tower. So the bespoke stack walker dissolves into ordinary per-value monomorphization demands; the fact engine
supplies ordering, caching, and the recursion guard. A level-1 body containing `if..else..raise` is elaborated by
`EffectLifter` *in body context* (the B1 class of `= Type` kind-unify fights structurally cannot arise — a body
never faces that unify), its matches desugar and reduce through the existing `PatternMatch`/`TypeMatch` rails, its
effect row is genuinely verified by the effect phase, and its per-instantiation reduction fires matches — all
existing, green machinery.

## 2. What remains — per-level *policy*, not special cases

Three things do not dissolve. They relocate into declared policy; name them now so no implementer rediscovers
them as "bugs."

### 2.1 Two access modes, opposite authority — never merge them

| Mode | When | Authority | Mechanism (exists today) |
|---|---|---|---|
| **inline** (δ-inline the body, live local metas) | adjacent levels mutually constrained — a level-1 body with holes (calculated return: under-applied constructor, `arityShortfall`) | **body**: level 0 solves level 1's holes | `installReturnMeta` deferral + `readMonomorphicReturn` back-edge |
| **fact** (read `CompilerMonomorphicValue((v, n), groundArgs)`) | ground per-instantiation reads — callers, effectful level-1 values | **payload**: `Right(t)` becomes level 0's *expected* type; `Left(msg)` rejects | cross-track back-edge + `dischargeGuardedReturn`/`GuardChannel` |

The first guard attempt produced a **silent-accept soundness bug** by merging these (the body solved the return
meta while the `Right(t)` payload was discarded — `def g[COND]: if(COND, String[]) else raise(…) = 5` typed as
`Int`). The payload-mismatch fixture in Step 0 pins this forever.

### 2.2 The level-≥1 platform contract

The compiler platform *defines* the type levels' effect channel, exactly as `main` pins the runtime ambient
carrier to `IO`:

- Declared row bound: a level-≥1 body may perform at most `{Throw[String]}` (`Abort` internally dischargeable —
  `if..else` discharges it). Pure levels have row `{}` ⊆ the bound, trivially.
- Carrier: `{Throw[String]}` pins to compile-time `Either[String]` **via the declared-binder rail**
  (`Track.Compiler.pinCarriers` off `paramConstraints` — green in `CompilerAbilityResolutionTest` Increment D).
  The fragile *inferred*-meta sweep of the WIP (`pinInferredReturnCarriers`/`pinGuardCarrierToEither`) is not needed
  and not carried forward: the level value's signature *declares* the row.
- Boundary read: `Right(t)` ⟹ `t`, `Left(msg)` ⟹ error at the concrete instantiation, via the existing
  `GuardChannel` protocol. Two small, named sites — a contract, not machinery.

### 2.3 Per-level check policy

- `RecursionCheckProcessor` applies at **level 0 only** (today's "body only, never the signature"). Level-≥1
  self-reference in `data` is legitimate covariance, governed by `StrictPositivityChecker`. Without this stated
  policy, exploding signatures into bodies would flag every recursive `data Tree`.
- The effect phase verifies level-≥1 rows against the §2.2 contract — a *new, free* user-facing check.
- `MarkerGuardSignature` / the `where`-clause ability guards are a **different feature** and stay untouched.

## 3. Implementation order

Each step lands green on the full suite before the next starts. Checker-touching steps must **net-delete** checker
lines (see §4).

### Step 0 — red tests + salvage (small; lands first)

- **No revert is needed**: Stage-2 attempt 1 was never merged — master carries none of it (verified 2026-07-14;
  the entire attempt is the single branch commit `77c2ed43`). "Reverting the current attempt" = not merging it,
  plus deleting the three reference branches (`wip/return-position-unification-stage2`, `wip/if-else-guard-idiom`,
  `failed/if-else-guard`) **once Step B is green** — by then their only unique content (the fixtures, the old
  `docs/if-else-guard-idiom-wip.md` assessment) is superseded by committed tests and this plan. See Appendix A for
  the branch-only inventory.
- Cherry-pick from `wip/return-position-unification-stage2`, as two plain commits (do **not** merge the branch):
  1. `EffectLifter.underApplied`: `case VType => 0 < arity` — a guard-independent lifter correctness fix (any pure
     type flowing into a carrier slot).
  2. `stdlib/eliot-compiler/eliot/effect/Abort.els` — the compile-time `AbortCarrier` overlay (missing sibling of
     the `Either`/`Option` overlays).
- Land the goal fixtures as tests, `ignore`d/pending until Step B flips them:
  - TRUE: `def greeting[COND: Bool]: if(COND, String[]) else raise("greeting unavailable") = "hello"`,
    `main = printLine(greeting[true])` → runs, prints `hello`.
  - FALSE: `greeting[false]` → build error containing `greeting unavailable`.
  - Bare reject: `def unavailable: raise("not available") = …` → build error.
  - **Helper-guard-by-name**: a user-written `def nonEmpty[A: Type, N: Int]: {Throw[String]} Type = …` referenced
    *by name* in a return slot — syntactically indistinguishable from a type application; kills any temptation to
    scan syntax.
  - **Payload mismatch** (the §2.1 soundness pin): `def g[COND]: if(COND, String[]) else raise(…) = 5` must FAIL
    (body `Int` vs accepted payload `String`).

Every prior attempt stayed "green" while drifting because the goal was never encoded as a test.

### Step A — the naming (green; zero consumer changes)

Deliverable: type expressions have names and run on the platform, verified equivalent to today.

- Add `typeLevel: Int = 0` to the boundary fact keys: `SaturatedValue.Key(vfqn, platform)` and
  `CompilerMonomorphicValue.Key(vfqn, typeArguments)` (`MonomorphicValue.Key` stays level-0-only — level ≥ 1 is
  compiler-track by definition). A proper key dimension, **not** a mangled synthetic FQN — level identity is
  structural, not stringly (the `handleCases^PatternMatch#N` precedent is for front-end-minted values, which this
  is not — yet; see Step E).
- New derivation processor answering `SaturatedValue.Key((v, n≥1), Compiler)` from the host's `SaturatedValue`:
  body = `TypeStack.levels(n-1)`, signature = `TypeStack.levels(n)` — or, at the top, `Type` with the §2.2
  contract row on the codomain; generics = the host's. Demand-driven: a level nobody demands costs nothing, so
  **every** value has level values *available* with no syntactic discrimination and no pure-case tax.
- `CompilerMonomorphicTypeCheckProcessor` handles the new keys with the existing `TypeStackLoop`/`Track.Compiler`
  unchanged (a level value is just a small ordinary value).
- **Equivalence tests**: for existing green fixtures (pure signatures; the `orError` combinator forms), demand
  `CompilerMonomorphicValue((v, 1), groundArgs)` and assert its reduced return equals what `evalExpr` produces in
  today's walk. Divergence is a finding, not a test bug — the fact-mode value is the intended authority.
- `CACHE_VERSION` bump.

### Step B — effectful returns read the level-1 fact (the red tests go green)

- In the settle/read for a return that is neither ground-`Type`-kinded nor under-applied (three-way structural
  dispatch: ground → ordinary; `arityShortfall` → calculated, inline mode; otherwise → fact mode), demand
  `CompilerMonomorphicValue((v, 1), groundArgs)` and discharge per §2.2. Unground args defer via the existing
  return-meta (Use-Site Verification). Reporting sites preserved: reject at the definition for a direct
  monomorphize, at the use reference for a caller.
- Stage the flip: keep the combinator path (`dischargeGuardedSignature` + precompute-merge) until the fact-mode
  path passes `GuardSignatureIntegrationTest`, then switch and delete the guard reliance on
  `CompilerNativesProcessor`'s nullary precompute (the precompute itself stays for other compile-time natives).
- Un-`ignore` the Step-0 fixtures. Delete on master what the flip obsoletes: `sawGuardReturn`/`recordGuardReturn`,
  `isGuardCarrier`, the in-place `dischargeGuardedSignature` peeling (`GuardProbe` goes with it if nothing else
  uses it). The never-merged Stage-2 branch machinery (settle split, `isGuardKind`, `stuckEffectfulHeads`,
  `resolveEffectfulReturn`/`settleEffectfulReturn`, `reduceEffectfulGuardReturn`, `evalSemExpr`,
  `pinInferredReturnCarriers`, `effectCarrierMetaIds`) is simply never merged.

### Step C — kind checks become ordinary level checks

- Level *n* is checked as an ordinary value against `eval(level n+1)` — the same recursive demand, so the
  per-level `= Type` kind-unify in `walkTypeStack` and its carve-outs go: delete `flattenReturnToType` (a level-1
  value's *signature* carries the kind; nothing to flatten), and `TypeStackLoop` shrinks to a driver over "check
  level 0's body against the level-1 read" (or disappears into `Checker`).
- This is the step with the widest blast radius on the check ladder — it gets its own regression pass over the
  W3/W4 implicit-generics and ability-guard suites before landing.

### Step D — calculated returns re-documented, not rewritten

- The inline access mode *is* the calculated-return mechanism; `readMonomorphicReturn` stays. Write the §2.1
  authority table into the code docs at `CalculatedReturnResolver` so the two modes are never re-unified.

### Step E (optional; only if the carriage hurts) — front-end explosion

- Move `TypeLevel` into core `NamedValue` identity and delete the `TypeStack` carriage; the Step-A derivation
  moves from the saturate boundary into core desugaring. Mechanical by this point, because nothing downstream
  reads the stack anymore. Skip it if the front-end carriage isn't causing friction — the semantic explosion
  (Steps A–D) is what buys the simplification.

### Cleanup (after Step B is green; was `return-position-unification.md` §5)

- Delete `eliot.lang.Guard` (`when`/`orError`), both layers (`stdlib/eliot/…` and `stdlib/eliot-compiler/…`).
- Rewrite `GuardSignatureIntegrationTest` to the `if..else..raise` / bare-`raise` forms.
- Update `Expression.scala` return-type-parser doc comments and `ASTParserTest` cases referencing `when`/`orError`.

## 4. Guardrails (the stop rules)

- **Routing, not capability.** If a change teaches `Checker`, `renormalize`, or `walkTypeStack` any new
  level-specific behaviour, stop — the fix is to route the expression through the platform as a value, never to
  grow the kernel. Every checker-touching PR in this plan must net-delete checker lines.
- **Never merge the two access modes** (§2.1). Body authority and payload authority are opposite; the
  payload-mismatch fixture is the tripwire.
- **Fail-safe, never silent.** A level that cannot be resolved errors loudly (the `PostDrainQuoter` "Cannot
  resolve type." convention); no `Type` fallback, no discarded verdicts.

## 5. Risks

- **Step A equivalence gap.** The Attempt-1 gotcha (elaborated eval carries an extra explicit type-arg meta that
  leaves `fold` natives stuck) showed raw-vs-elaborated evaluation can diverge. Fact mode runs the full mono
  pipeline (metas solved before reduction), so it should not reproduce it — the equivalence tests exist to prove
  that, per fixture, before any consumer switches.
- **Step B changes how the working `orError` path resolves.** Staged flip (both paths live until the new one is
  green) — same mitigation the Stage-2 draft prescribed.
- **Cross-track cycles.** Level facts re-enter the compiler track from a runtime check; the existing
  `activeFactKeys` recursion guard covers the back-edge — extend it to the level keys and add a cycle test.
- **Cost.** Per-instantiation level facts add fact-engine overhead, but `walkTypeStack` already evaluates every
  signature per instantiation; fact mode adds caching *across* callers. Measure at Step B; no optimization before
  measurement.
- **Step C blast radius.** The check ladder is the most delicate code in the compiler (the Stage-1 notes chose a
  transient flatten precisely to avoid touching it). Step C is deliberately sequenced after B so the guard
  pressure is already off, and it can pause indefinitely without leaving anything half-migrated.

## Appendix — demolition schedule

Anchors are master as of 2026-07-14 (`2930ee01`). Line numbers drift; identifiers are the authority.

### A. The current attempt: branch-only, never merged (deleted with the branches after Step B)

Everything below exists **only** on `wip/return-position-unification-stage2` (`77c2ed43`) and is disposed of by
never merging (Step 0). Listed so nobody "rescues" a piece of it later:

- `CalculatedReturnResolver`: the "Effectful-return cross-track back-edge (Stage 2)" section — `stuckEffectfulHeads`
  (hard-coded `{flatMap, map, pure}`), `isStuckEffectfulReturn`, `resolveEffectfulReturn`,
  `readCompilerVerdict(Ground)`, `settleEffectfulReturn`, `deferReturnToBody`.
- `Checker`: `isGuardKind` (the `= Type` kind-unify carve-out for carrier-meta-headed returns),
  `pinGuardCarrierToEither`, `evalSemExpr` and its call into the read path.
- `Track`: the `settleReturnPosition` 2→3-tuple widening (`bodyCheckSig`/`publishSig` split), the compiler-track
  lenient `freshMeta` body-check branch, `pinInferredReturnCarriers`/`pinMetaToEither` (the inferred-meta sweep).
- `TypeStackLoop`: `reduceEffectfulGuardReturn`, `reduceGuardSubValues`, the elaborated-level capture (`levelExprs`).
- `Unifier.effectCarrierMetaIds`.
- **Salvaged** (the only two pieces, via Step-0 cherry-picks): `EffectLifter.underApplied` `case VType => 0 < arity`;
  `stdlib/eliot-compiler/eliot/effect/Abort.els`.

### B. Master code removed, per step

**Dies at Step B** (the fact-mode flip):

| Site | Anchor | Replaced by |
|---|---|---|
| `sawGuardReturn` flag + `recordGuardReturn` | `CheckState.scala:63,69`; set `Checker.scala:241`; read `TypeStackLoop.scala:90` | the three-way structural dispatch needs no recorded state |
| `isGuardCarrier` + the kind-ladder guard arm (`case VType =>`) | `CalculatedReturnResolver.scala:263`; `Checker.scala:238` | fact-mode read; a level-1 body never faces the `= Type` unify |
| `dischargeGuardedSignature` (VPi peel via probe) | `CalculatedReturnResolver.scala:321` | `CompilerMonomorphicValue((v,1), groundArgs)` read |
| `Marker.GuardProbe` reserved neutral | `SemValue.scala:102`, minted `CalculatedReturnResolver.scala:329` | goes with the peel (its only user — verified) |
| runtime `Track.settleGuardedReturn` guard arm | `Track.scala:86` | settle reads the level-1 fact |
| compiler `Track.settleGuardedReturn` publish-undischarged arm | `Track.scala:126` | level values publish through their own mono fact |
| guards' reliance on the nullary precompute-merge | `CompilerNativesProcessor.scala:88-91` (`performsAbility` → `CompilerMonomorphicValue(vfqn, ∅)`) | per-instantiation level-1 read. The precompute itself **stays** for other compile-time natives; after Cleanup, re-audit whether it still has users |

**Dies at Step C** (kind checks become level checks):

| Site | Anchor | Replaced by |
|---|---|---|
| `flattenReturnToType` transient | `TypeStackLoop.scala:247` | a level value's *signature* carries the kind — nothing to flatten |
| the per-level `= Type` kind-unify walk | `TypeStackLoop.walkTypeStack` | level *n* checked as an ordinary value against `eval(level n+1)`; `TypeStackLoop` shrinks to a driver or dissolves into `Checker` |

**Dies at Cleanup** (after Step B is green):

| Site | Anchor |
|---|---|
| `eliot.lang.Guard` module (`when`/`orError`) | `stdlib/eliot/eliot/lang/Guard.els` + `stdlib/eliot-compiler/eliot/lang/Guard.els` |
| `when`/`orError` fixtures — **18 files** reference `orError` today (`grep -rl orError lang/test jvm/test examples/src`) | `GuardSignatureIntegrationTest` (rewrites to `if..else..raise`/bare `raise`), `MonomorphicTypeCheckTest` W2b/G1 blocks (`:1017-1131`), `CompilerAbilityResolutionTest`, `ASTParserTest`, `ExamplesIntegrationTest1-3`, `FullIntegrationTest`, `WhereOnDefIntegrationTest`, `TerminationIntegrationTest`, plus incidental tokenizer/module/resolve/operator/matchdesugar/ability fixtures — most rewrite mechanically |
| parser doc examples using `A when (…) orError "…"` | `ast/fact/Expression.scala:248-249,284` (docs only — the infix-in-type-position parser support itself stays; `if..else..raise` needs it) |

### C. Explicitly KEPT (do not "clean up" these)

| Site | Anchor | Why |
|---|---|---|
| `dischargeGuardedReturn` + `extractGuardMessage` | `CalculatedReturnResolver.scala:280,298` | the single `Right`/`Left` boundary read (§2.2); callers at `Checker.scala:465,707` remain as the boundary sites |
| `GuardChannel` | `monomorphize/check/GuardChannel.scala` | shared protocol with the ability `where`-guards |
| declared-binder carrier pin | `Track.scala:169` (`throwCarrierErrorType`), `:188` (`pinCarrierToEither`) | the rail the §2.2 contract rides |
| calculated-return inline mode, whole mechanism | `CalculatedReturnResolver`: `arityShortfall:84`, `isCalculatedReturn(Expr)`, `installReturnMeta:63`, `resolveCalculatedReturn:126`, `resolveCompleteCalculatedReturn:151`, `readMonomorphicReturn(Ground):175,220`, the `reportUnground`/`reportRecursive` errors | Step D: this *is* the inline access mode (§2.1) |
| `MarkerGuardSignature` + ability-guard interpreter | `monomorphize/check/MarkerGuardSignature.scala`; `AbilityImplementationProcessor.scala:169-253` | different feature (`where` ability guards), untouched |
