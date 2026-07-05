# Ability Implementation Guards

Status: **Stage 0 landed** (2026-07-05); Stages 1-6 planned. Revised 2026-07-05: the coherence stance is
settled (use-site exactly-one-survivor is the rule; definition-time overlap is a conservative lint), the
marker-body decision is added, the blanket `Eq` rule is dropped, and two further clients — the `IntArith`
width-dispatch family and a guarded `Coerce` — are folded in. Guards are thereby not a one-off fix for
`Throw` but the language's general "computed instance applicability" primitive. Stage 0 (the `Eq` ability +
`Eq[Type]` leaf) is implemented — see §5 for what shipped and the two naming/method decisions it settled.

## 1. Motivation

Eliot models typed errors as the `Throw[E, F[_]]` effect ability, discharged through a monad-transformer
carrier `ThrowCarrier[E, G, A]`. Two *distinct* error types in one effect row (`{Throw[NetError],
Throw[ParseError]}`) force the carrier to nest — `ThrowCarrier[NetError, ThrowCarrier[ParseError, IO]]` —
because each error needs its own `Either[E, A]` slot. Making that stack resolve requires two carrier
instances:

- **native** `Throw[E, ThrowCarrier[E, G]]` — the base case: the layer that *owns* error type `E`
  discharges into its own slot (`pure(Left(err))`), requires only `G ~ Effect`.
- **lift** `Throw[E2, ThrowCarrier[E1, G]]` — the inductive step: a *foreign* error type `E2` is
  delegated inward (`map(Right, raise(err))`), requires `G ~ Throw[E2]`.

Both are needed (the native terminates the recursion; the lift routes a non-owned error to its owner —
see [the Dep contrast](#appendix-why-depadepb-does-not-need-this)). But they **structurally overlap** on
the diagonal `E1 = E2`, and the definition-time overlap check (`ModuleAbilityOverlapCheckProcessor` via
`AbilityMatcher.patternsOverlap`) rejects overlapping same-module instances. That rejection is the sole
blocker to multiple distinct `Throw[E]` in one row (verified: adding the lift today produces
`Overlapping ability implementation: 'Throw' overlaps …`).

Rather than resolve overlap by a specificity ordering (a bespoke meta-rule outside the language, whose
silent most-specific-wins failure mode conflicts with fail-safe design, and which can never express
computed conditions), we make instances **genuinely disjoint** with a boolean guard — the lift applies
only `where E1 != E2`. Instance applicability stays *inside the language*, an ordinary type-level
computation. The uniqueness requirement is unchanged as a semantic rule — at every concrete type
combination that manifests, exactly one instance applies — but *where it is checked* moves to the use
site (§3), which is the same Sound-Not-Modular stance the rest of the language takes.

Three clients on day one:

1. **`Throw` self-lift** (this section) — an equality guard, `where E1 != E2`.
2. **`IntArith`** (§4.1) — the jvm arithmetic width dispatch as a range-guarded family, `where
   fitsIn[…]`.
3. **Guarded `Coerce`** (§4.2, exploratory) — retiring the hand-rolled Option-decline protocol.

## 2. Design

### 2.1 The `where` guard clause

An ability implementation may carry an optional guard after its head:

```eliot
implement[E1, E2, G[_] ~ Throw[E2] & Effect] Throw[E2, ThrowCarrier[E1, G]]
      where E1 != E2 {
   def raise[A](err: E2): ThrowCarrier[E1, G, A] = ThrowCarrier(map(a -> Right(a), raise(err)))
}
```

`where <expr>` holds a compiler-evaluated expression of type `{Throw[String]} Bool`. At each concrete use
site it evaluates to one of three outcomes:

1. `true`  → the instance applies;
2. `false` → the instance **declines** (dropped from candidates; resolution falls through — e.g. to the
   native instance on the diagonal);
3. `error(msg)` (a raised `Throw[String]`) → **hard compile error** carrying `msg`.

`false` is a *soft* decline, distinct from the *hard* error — on the diagonal we need the native
instance to win cleanly, not an error from the lift. The `Throw[String]` channel is retained so a guard
that does real computation can emit a legible diagnostic instead of a confusing downstream "no matching
instance"; do **not** use it for the diagonal (that is the valid native case).

Keyword `where` is a soft keyword (lowercase identifier in a unique position — no tokenizer change,
`Primitives.identifierWith`). `provided`/`if`/`guard` are alternatives; `where` chosen to avoid clashing
with the `when`/`orError`/`error` guard combinators.

**Parse boundary**: the guard expression must stop at the body's `{`. Application is parens-based, so
`{` cannot continue `E1 != E2` — but a guard that is *itself* a block expression would be ambiguous;
reject that (or require parentheses). Needs an explicit test.

### 2.2 `Eq` ability — not a bespoke `typeEquals`

Type-level code is ordinary code, so "are these two equal" must be the same `==` everything else uses,
not a special leaf. We introduce:

```eliot
ability Eq[A] {
   def ==(a: A, b: A): Bool
}
// != derived as not(a == b)
```

`==`/`!=` are the general equality operators, for values **and** — because types are values — for types.
They are independently valuable (Eliot currently has no user-facing equality at all); only the `Eq[Type]`
leaf is attributable to guards. They need **infix precedence declarations**
(`InfixPrecedenceResolver` relations) — deciding their placement relative to existing operators is a
Stage-0 task.

**The `Eq` constraint is demand-driven, not a blanket rule.** A generic parameter is not of kind `Type` —
it has whatever type it has (`MIN`/`MAX` in `Int[MIN: BigInteger, MAX: BigInteger]` are `BigInteger`; the
`Throw` error slot `E` is `Type`). Type-checking a guard body imposes `Eq[T]` on exactly the parameters
the guard compares, via ordinary ability constraint propagation — `E1 != E2` demands `Eq[Type]` because
`E1`/`E2` are `Type`-typed; a bounds guard would demand `Eq[BigInteger]`. There is **no** "every generic
parameter's type must implement `Eq`" well-formedness rule: the blanket version would demand `Eq` on
higher-kinded parameters (`G[_]` — equality on `Type -> Type` function values) and retroactively burden
every existing generic definition for no benefit.

**The `Eq[Type]` leaf**:

- Deep/structural: same head constructor and all arguments equal, recursing through the arguments.
  Walking a value of unknown depth is a loop, which user code cannot express (no recursion), so it
  bottoms out in a **native leaf** — like `fold`/`add`. Presented as an ordinary `Eq` instance; only its
  implementation is primitive. Since everything is forced/normalised first, structural comparison of
  normal forms *is* definitional equality — the cornerstone's one notion of type equality, read back as
  a `Bool`.
- **Pure comparison only — never through the `Unifier`.** Unification solves metas as a side effect; an
  equality *test* must never mutate the meta store. Mirror `Unifier.groundEquals`-style pure structural
  compare, do not call `unify`/`unifyForced`.
- **Concrete-only semantics suffice.** Use-site arguments are always ground (§3.1), so the leaf only
  needs: `true` on structurally identical concrete normal forms, `false` on differing concrete forms,
  and — defensively — **stuck** (`VStuckNative`) on anything non-concrete, which hard-errors at
  read-back rather than answering wrongly (fail-safe). The richer symbolic semantics (reflexivity on
  identical neutrals, stuck on distinct metas) are needed only by the *optional* definition-time
  disjointness proof (§3.2) and can be added if and when that lands.
- **Placement**: `ability Eq` abstract in the base stdlib; the `Eq[Type]` instance is
  **compiler-pool-only** (type values are erased at runtime — a runtime `==` on types is
  unimplementable). The `Eq[Type]` instance must itself be **unguarded** — a guard on it would demand its
  own resolution (same fact key → engine cycle error). Base value instances (`Eq[Int]`, `Eq[String]`,
  `Eq[BigInteger]`) as needed (these can have runtime bodies); `Eq` for `data` derivable later.

### 2.3 The guard rides the marker's return type

`implement` has **no dedicated AST node** — it desugars at parse time into a synthetic *marker*
`FunctionDefinition` whose argument types encode the head pattern and whose signature `matchImpl` matches
against. Rather than thread a new `guard` field through `FunctionDefinition → NamedValue → ResolvedValue
→ OperatorResolvedValue`, **the guard becomes the marker's return type**:

```
// synthesized marker for the guarded lift (schematic):
def Throw[E1, E2, G](arg0: E2, arg1: ThrowCarrier[E1, G]): E1 != E2
//  pattern stays in the argument types ^^^^^^^^^^^^^^^^   guard is the return slot ^^^^
```

**The marker's current body must be retired.** The synthesized marker today is not an empty vessel: it
has return type `pattern.head` and body `arg0` (`ast/fact/ImplementBlock.scala:55-63`) — a trivially
well-typed identity. Co-opting the return slot interacts with the landed W2b machinery:
`dischargeGuardedSignature` treats a *stuck guard with a body* by installing a return meta the body then
solves (`CalculatedReturnResolver.scala:299`) — a marker keeping body `arg0` would have the body fight
the guard, and at concrete discharge `arg0 : P0` would be checked against `true`. Decision: markers
become **body-less** (return slot = guard expression, default the literal `true` when no `where` clause;
body = `None`), uniformly for all markers — pure signature vessels. Verified safe: nothing consumes the
marker's body or old return — `AbilityMatcher.extractFunctionArgs` drops the return before matching,
`ImplementationMarkerUtils` reads only the *first argument's* type constructor, and
`AbilityResolver.abilityArity` reads the *ability* marker (`Qualifier.Ability`), not the implement
marker. The body retirement is the one behaviour change to watch in Stage 1 (full suite).

Consequences:

- **No new field threading.** The pattern stays in the argument types (what `matchImpl` unifies), so
  matching is unchanged; the guard rides the existing `typeDefinition`/`typeStack`/signature slot through
  every front-end phase untouched.
- **No new evaluator.** A `{Throw[String]}` computation in a return-type slot of a runtime-pool value,
  discharged at check time via compiler-reduced natives, is the *already landed* effectful-signatures
  bridge (W2b / compiler-as-platform increment F: `CalculatedReturnResolver.isGuardCarrier` /
  `dischargeGuardedReturn`, `CompilerNativesProcessor` → reduced `Leaf`). The guard inherits a working
  evaluation path; the only novelty is reading a `Bool` payload instead of a type, and invoking the
  discharge from resolution (§3.1, spike 1).
- **The error path is free.** A guard that `error("…")`s is `Left(msg)`; the discharge already emits it
  as a compile error with the author's message.

## 3. Coherence: use-site exactly-one-survivor is the rule

The semantic coherence rule for guarded instances: **at every manifest instantiation, after guard
filtering, exactly one candidate survives**. Zero survivors is the existing "No ability implementation
found" error; two or more the existing "Multiple ability implementations" error
(`AbilityImplementationProcessor`, `deduplicated match` L36-55). This is the Use-Site Verification
cornerstone applied to instance coherence: every instantiation that actually manifests is fully checked,
only the modular per-definition certificate is given up, and no incorrect program compiles.

**Why disjointness is not checked statically.** Guard disjointness is `∀x. ¬(g₁(x) ∧ g₂(x))` over
unbounded domains (all types, all `BigInteger` bounds). Totality does not help — the universal
quantifier is what is undecidable. A solver for the arithmetic fragment would be a second, parallel
decision procedure outside the one evaluator (cornerstone-suspect) and still only partial. Note that an
earlier draft's rule — "an incomparable guarded overlap (neither guard provably false) still errors" —
would reject the range-partitioned families of §4 outright (both `fitsIn` guards are stuck under the
MGU): definition-time *must* defer what it cannot decide, never error on it.

**Trade-off (explicit).** A library can ship guarded instances whose conflict or gap only surfaces at a
user's concrete use site — "a type error not of their making." This is the same trade the language
already makes for calculated returns and `infer` params, with the same mitigations: author-side coverage
through tests/probing, and the IDE surfacing the problem at the definition. A boundary-probing lint can
be layered on later as *tooling*; it never needs to be semantics.

### 3.1 Use-site discharge (the primary, semantic hook)

`AbilityImplementationProcessor.verifyImplementation` (L126-149) loads the marker signature and calls
`AbilityMatcher.matchImpl(markerSig, queryArgs)`, which returns `Some(implTypeArgs)` — the impl's own
type params (`E1/E2/G`) bound to concrete `GroundValue`s in declaration order — or `None`. After a
`Some`, discharge the marker's return under that binding and:

- `Right(true)`  → keep the candidate;
- `Right(false)` → return `Seq.empty` (decline);
- `Left(msg)`    → emit `compilerError` with `msg`;
- anything else  → internal error (cannot happen at a concrete site, see below).

Both `verifyImplementation` call sites (the normal path and `handleDefaultImplementation`) go through
this one filter, and the use-time constraint resolver routes back through the same `generateFact` — one
hook. The ambiguity check then only sees surviving candidates, so native and lift never both reach it.

**Ground-args guarantee.** `AbilityResolver.tryResolveOne` quotes type arguments to `GroundValue` before
calling `resolveAbility` and leaves the reference pending otherwise (`AbilityResolver.scala:107-137`), and
`matchImpl` takes `Seq[GroundValue]`. Guards therefore always evaluate over fully concrete arguments —
the stuck case cannot arise at use sites.

**Fail-safe binding caveat.** `attemptMatch` prefers structurally-traced bindings
(`tracePatternMetas`) but falls back to `metaToGround`, which collapses complex bindings to
`GroundValue.Type` (`AbilityMatcher.scala:289-303`). A guard must **never** be evaluated over a fallback
binding — `Type`-collapsed values could compare equal wrongly. If any parameter the guard mentions is
untraced, hard-error (internal), never evaluate.

### 3.2 Definition-time overlap becomes a conservative lint

`ModuleAbilityOverlapCheckProcessor` keeps its job as *early author feedback*, demoted from soundness
mechanism to conservative lint with three outcomes:

- **provably overlapping** — patterns unify and both markers are unguarded (return-slot literal `true`)
  → error, exactly today's behaviour;
- **provably disjoint** — some guard provably `false` under the MGU → no error;
- **undecided** — anything stuck → **defer to use sites**, silently.

The minimal correct implementation is the first and third arm only: patterns unify AND both unguarded →
error; any explicit `where` on either side → defer. This preserves today's behaviour for all existing
code and requires **no symbolic guard evaluation at all** — which is what removes the reflexive/stuck
`Eq` semantics, the MGU-binding evaluation, and the `paramMetas` threading from the critical path. The
second arm (proving the `Throw` diagonal disjoint via `Eq` reflexivity under the MGU, so the guarded
lift gets a definition-time clean bill) is an *optional later enhancement*, feedback-only.

Precedent: cross-module overlap is *already* detected only at call time (see the processor's own doc
comment) — guarded same-module pairs simply join that class.

## 4. Further clients

### 4.1 `IntArith` — the jvm width dispatch as a guarded family

The jvm layer's `dispatchAdd`/`dispatchSubtract`/`dispatchMultiply`
(`jvm/eliot/eliot/lang/Int.els:118-155`) are ordered `fold(fitsIn[…], …)` decision trees — the same
substrate as guards (a compile-time `Bool` over type-level values selecting among alternatives), minus
the resolver coupling. Reformulated with explicit disjoint conditions:

```eliot
ability IntArith[Cmin: BigInteger, Cmax: BigInteger] {
   def addOp(a: Int[Cmin, Cmax], b: Int[Cmin, Cmax]): Int[add(Cmin, Cmin), add(Cmax, Cmax)]
   def subtractOp(a: Int[Cmin, Cmax], b: Int[Cmin, Cmax]): Int[subtract(Cmin, Cmax), subtract(Cmax, Cmin)]
   def multiplyOp(a: Int[Cmin, Cmax], b: Int[Cmin, Cmax]): Int[multiplyMin(Cmin, Cmax, Cmin, Cmax), multiplyMax(Cmin, Cmax, Cmin, Cmax)]
}

implement[Cmin: BigInteger, Cmax: BigInteger] IntArith[Cmin, Cmax]
      where fitsIn[-128, 127, Cmin, Cmax] { ... }                                             // byte
implement[Cmin: BigInteger, Cmax: BigInteger] IntArith[Cmin, Cmax]
      where fitsIn[-32768, 32767, Cmin, Cmax] && not(fitsIn[-128, 127, Cmin, Cmax]) { ... }   // short
// ... int, long, big: five instances, disjoint by explicit complements
```

Five instances (one per operand width), each body making the small binary result-width choice (a
`fold`, as today); the jvm `+`/`-`/`*` keep their widen-then-call shape, now calling the ability method.
The ordered chain's implicit smallest-first priority becomes explicit disjoint complements.

- **No `Eq` needed**: `fitsIn`/`&&`/`not`/`add` are already compiler-evaluable (they run in the
  `opaque type Int` body and the return-bound arithmetic today). This client depends only on Stages 1-3 —
  so it can land *before* Stage 0 and de-risk the shared machinery.
- **All-metas pattern**: disjointness is carried entirely by the guards; `matchImpl` handles bare-meta
  patterns (they match anything). Binding `BigInteger`-typed params through ability matching is
  established practice — the `Combine` instance (`stdlib/eliot/eliot/lang/Int.els:66`) does exactly this,
  and bounds sit structurally inside `Int[..]` patterns so they take the faithful trace path (§3.1).
- **What it buys over the fold**: a coverage gap or overlap in the conditions is a hard error at the
  manifest instantiation, whereas a wrong fold predicate silently selects a wrong-width leaf (the
  `nativeAdd*` signatures do not encode width — only the name does, resolved by the backend).
- **Watch items**: (a) the same-module lambda-class collision gotcha — five instances sharing method
  names in one module; bodies must stay lambda-free (they are: direct native calls + `fold`) or that
  backend bug must be fixed first; (b) resolution cost — every distinct bounds pair is a distinct cached
  ability query where the fold reduced inline during monomorphization; measure on the examples suite;
  (c) **scope** — this replaces the *internal* dispatch only. The abstract stdlib `+` stays a plain
  `def`; making user-facing `+` an ability (open Num-style overloading) is a separate language decision,
  not part of this.

### 4.2 Guarded `Coerce` — retiring the Option-decline protocol (exploratory)

The jvm `Coerce[Int, Int]` instance (`jvm/eliot/eliot/lang/Int.els:75`) is a hand-rolled guard: the
instance always pattern-matches, the *body* returns `Option` where `none` means "does not apply", and the
checker special-cases the protocol (`RefinementSolver.tryCoerce` evaluates `coerce` and splices the
`some` payload). Post-guards:

```eliot
implement[Smin: BigInteger, Smax: BigInteger, Tmin: BigInteger, Tmax: BigInteger]
      Coerce[Int[Smin, Smax], Int[Tmin, Tmax]]
      where lessThanOrEqual(Tmin, Smin) && lessThanOrEqual(Smax, Tmax) {
   def coerce(value: Int[Smin, Smax]): Int[Tmin, Tmax] = nativeWiden(value)
}
```

Total body, no `Option`; decline is instance non-applicability, uniform with every other ability; the
`error(msg)` channel is free for a real diagnostic (`"Int[0,1000] does not fit in Int[0,255]"`). Retires
one bespoke checker protocol.

**Caveat before committing**: the checker currently distinguishes "instance exists but bounds fail" from
"no `Coerce` instance for this type pair at all"; folding decline into non-applicability merges them —
the refinement solver's error reporting and the deferred-coercion reconciliation
(`reconcileRefinements`) must still produce the right message at the right position. A follow-up client,
not part of the initial plan. Also a range guard, so it requires §3's use-site stance.

## 5. Implementation plan (staged, each independently verifiable)

Ordering note: **Stage 0 gates only the `Throw` client** (the only equality guard). Stages 1-3 are the
shared machinery, exercisable by the `IntArith` client alone — landing Stage 5 before Stages 0/4 is a
valid de-risking order.

**Stage 0 — `Eq` ability + `Eq[Type]` leaf** (for the `Throw` client). **LANDED.**
- `ability Eq[A] { def equals(a: A, b: A): Bool }` abstract in the base (`stdlib/.../Eq.els`); `==`/`!=`
  are **top-level `infix` operators** delegating to `equals` — *not* ability methods, because
  `AbilityBlock` drops fixity/precedence when desugaring an ability method (so an `infix def ==` inside
  the ability would silently lose its fixity). This resolves the open "method naming" question: named
  method `equals` + operators. `!=` is `fold(equals(a, b), false, true)`. Precedence: `==`/`!=` are
  `infix left`, `== above &&` (binds tighter than logical-and), `!= at ==`; arithmetic-vs-comparison
  precedence deferred until a runtime `Eq[Int]` makes it exercisable.
- `Eq[Type]` instance + its `typeEquals` leaf live in `compiler/.../Eq.els` (compiler-pool-only — types
  are erased at runtime), with the ability re-declared there per the layer-merge duplication rule. The
  leaf is `typeEquals(a: Type, b: Type): Bool`, a Scala native in `SystemNativesProcessor` (well-known
  `WellKnownTypes.typeEqualsFQN`): **concrete-only** structural normal-form compare (mirrors
  `Unifier.groundEquals`, never through the `Unifier`), defensively stuck (`VStuckNative`) on non-concrete
  input; itself unguarded. Base value instances (`Eq[Int]`, `Eq[String]`, `Eq[BigInteger]`) **deferred** —
  not needed by the `Throw` client, and each needs a backend comparison native.
- ~~Enforce "a generic parameter's type must implement `Eq`"~~ — dropped; the constraint is
  demand-driven from guard bodies (§2.2).
- **Verified:** `EqTypeLeafTest` (leaf reduces `Int==Int`→true, `Int==String`→false, `Int[0,5]` vs
  `Int[0,10]`→false, stuck on a meta); `EqOperatorResolutionTest` (compiler track: `==`/`!=` at `A=Type`
  resolve the `Eq[Type]` instance through the first-order `[A ~ Eq]` constraint, inline to `equals`, `==`
  above `&&`); `EqTypeIntegrationTest` (the shipped `stdlib/.../Eq.els` parses/merges into a real build).
  Note: `Eq[Type]` fully fires only in a **type/signature position** on the **compiler track** — a runtime
  value using `==` on types cannot resolve the compiler-pool instance and correctly fails (confirmed), so
  the concrete end-to-end firing arrives with the Stage-4 guard.

**Stage 1 — marker synthesis: guard as return slot, body retired.**
- Parse `where <expr>` in `ast/fact/ImplementBlock.scala` (between the head-pattern parse and the body),
  as a value expression, soft keyword via `identifierWith("where")`; test the `{` parse boundary (§2.1).
- Marker: return slot = guard expression (default the literal `true`), **body = `None`**, uniformly for
  all markers (§2.3).
- Type-check the guard against `{Throw[String]} Bool` (reuse body checking) — malformed guards are
  ordinary errors.
- Test: `implement … where <expr> { }` parses/round-trips; **full suite** green (the body retirement is
  the behaviour change to watch).

**Stage 2 — use-site discharge + 3-way interpretation.**
- In `verifyImplementation`, after `matchImpl` returns `Some(implTypeArgs)`, discharge the marker return
  and keep/decline/error per §3.1; refuse (internal error) if any guard-mentioned parameter's binding
  came from the `metaToGround` fallback rather than the structural trace.
- Preferred route (spike 1): demand the marker's **compiler-track monomorphization** at `implTypeArgs`
  and read its deep return — the `readMonomorphicReturnGround` pattern
  (`CalculatedReturnResolver.scala:187`) — rather than hand-assembling a `CheckIO` context (carrier
  pinning, `Env.bind`) inside the ability processor. Known blocker: compiler-track re-entry is not yet
  threaded (NOTE at `CalculatedReturnResolver.scala:183`), already on the books as a follow-up increment.
- Test (hand-built guarded marker): `true` keeps, `false` declines, `error(msg)` surfaces `msg`.

**Stage 3 — definition-time overlap becomes the conservative lint (§3.2).**
- `patternsOverlap` reports overlap only when the patterns unify AND both markers are unguarded
  (return-slot literal `true`); any explicit guard on either side → defer.
- Test: native + guarded-lift no longer errors; two unguarded overlapping impls still error (existing
  behaviour); two *guarded* overlapping impls compile at definition and produce "Multiple ability
  implementations" at a concrete use site.
- (Optional, later, feedback-only: evaluate guards under the MGU to prove the diagonal disjoint —
  requires the symbolic `Eq` semantics.)

**Stage 4 — the `Throw` client.**
- Add `where E1 != E2` to the jvm `ThrowCarrier` `Throw` lift instance; restore the native instance.
- `examples/src/EffectsTwoThrows.els` compiles and runs; add it and a degenerate same-type-stack case
  (`ThrowCarrier[E, ThrowCarrier[E, IO]]` — the native must win the outer slot deterministically) to
  `ExamplesIntegrationTest`; re-run the full suite + `EffectsTwoDeps` for regressions.

**Stage 5 — the `IntArith` client** (§4.1; may precede Stages 0/4).
- Replace `dispatchAdd`/`dispatchSubtract`/`dispatchMultiply` with the five-instance guarded family;
  `+`/`-`/`*` call the ability methods.
- Test: examples suite green; a deliberately-introduced guard gap errors at the instantiation (not
  silently mis-selects); compilation-time comparison on an arithmetic-heavy example.

**Stage 6 — guarded `Coerce`** (§4.2; exploratory, after the caveat is resolved).

## 6. Grounded hook points

| Concern | File / method |
|---|---|
| Parse `where`; marker synthesis (current return `pattern.head` + body `arg0` to retire) | `lang/.../ast/fact/ImplementBlock.scala` (marker at L55-63); `ast/fact/Primitives.scala:136` `identifierWith` |
| Marker return type / signature | `ast/fact/FunctionDefinition.scala` `typeDefinition` (+ `signatureEquality` L35-40) → `core/fact/NamedValue.scala` `typeStack` → `resolve/fact/ResolvedValue.scala` → `operator/fact/OperatorResolvedValue.scala` |
| Use-site filter; ambiguity | `lang/.../ability/processor/AbilityImplementationProcessor.scala` `verifyImplementation` (L126-149); `deduplicated match` (L36-55); `handleDefaultImplementation` (second filter call site) |
| Ground-args guarantee | `lang/.../monomorphize/check/AbilityResolver.scala` `tryResolveOne` (L107-137, quotes to `GroundValue` or leaves pending) |
| Binding trace vs lossy fallback | `lang/.../ability/util/AbilityMatcher.scala` `attemptMatch`/`tracePatternMetas` (L209-253) vs `metaToGround` (L289-303, collapses to `GroundValue.Type` — must not feed a guard) |
| Definition-time lint | `AbilityMatcher.patternsOverlap` (L43-51), `attemptOverlap` (L274-287); `ability/processor/ModuleAbilityOverlapCheckProcessor.scala` (doc comment: cross-module overlap already call-time-only) |
| Guard discharge (reuse) | `lang/.../monomorphize/check/CalculatedReturnResolver.scala` `dischargeGuardedReturn` (L240-251), `extractGuardMessage` (L257-261), `dischargeGuardedSignature` stuck-guard-with-body meta (L299 — why marker bodies must go); compiler-track re-entry NOTE (L183), `readMonomorphicReturnGround` (L187) — the preferred Stage-2 route |
| `Eq[Type]` leaf | `monomorphize/processor/SystemNativesProcessor.scala` `systemReduction`; `Evaluator.trueValue`/`falseValue` (Evaluator.scala:222); pure compare mirrors `Unifier.groundEquals` (never `unify` — meta-solving side effects) |
| Operator precedence for `==`/`!=` | `operator/processor/InfixPrecedenceResolver.scala` (declared relations) |
| `IntArith` client | `jvm/eliot/eliot/lang/Int.els` `dispatchAdd`/`dispatchSubtract`/`dispatchMultiply` (L118-155); `BigInteger`-param matching precedent: `Combine` instance `stdlib/eliot/eliot/lang/Int.els:66` |
| `Coerce` client | `jvm/eliot/eliot/lang/Int.els:75`; `monomorphize/refine/RefinementSolver.scala` `tryCoerce`/`buildCoercedExpr`/`reconcileRefinements` |

## 7. Risks & spikes (do these first)

1. **Marker-return discharge as a resolution step** (the crux). Confirm the guard can be discharged from
   `verifyImplementation` per instantiation. Preferred shape: compiler-track monomorphization of the
   marker + deep-return read (§5 Stage 2), which reuses the whole checker instead of hand-assembling
   carrier pinning + bindings; the compiler-track re-entry threading is the known prerequisite.
   Prototype against a hand-built guarded marker before the rest.
2. **Marker body retirement.** Uniformly body-less markers with a `true`/guard return slot — confirm no
   consumer regression (the three known consumers are verified clean, §2.3; the full suite is the
   backstop) and no interaction with the W2b stuck-guard-with-body deferral.
3. **Re-entrant ability resolution.** Evaluating a guard resolves `Eq[Type]` (or nothing, for `fitsIn`
   guards) *during* another ability's resolution. Not a cycle (different fact keys; `Eq[Type]` is
   unguarded by requirement), but confirm nested resolution on the compiler track; the engine's cycle
   detection is the backstop.
4. **`signaturesCompatible` / `AbilityImplementationCheckProcessor`.** `peelPattern` drops returns, so
   the co-opted return should not perturb the method-signature compatibility check — but
   `setupEvaluation` now walks the guard's value references when evaluating the marker signature
   (`collectBindings`); confirm they classify cleanly (ability methods → `AbstractAbility` meta, bodied
   defs → `Body`).
5. **Same-module lambda-class collision** (Stage 5): five `IntArith` instances sharing method names in
   one module — keep bodies lambda-free or fix the backend bug first.
6. **Parse boundary** for `where <expr> {` (§2.1).

## 8. Resolved & open questions

Resolved this revision:

- **Coherence stance**: use-site exactly-one-survivor is the semantic rule; definition-time overlap is a
  conservative lint that must never reject an undecided pair (§3). Recorded as a decision, not a
  simplification — it is what makes computed (range) guards possible at all.
- **Marker shape**: uniform — every marker's return slot holds the guard (default literal `true`),
  bodies retired (§2.3).
- **`Eq` placement**: abstract in base; `Eq[Type]` compiler-pool-only, unguarded (§2.2).
- **No blanket `Eq` rule**: demand-driven from guard bodies (§2.2).

Still open:

- Keyword: `where` vs `provided`/`if`/`guard`.
- `==`/`!=` precedence placement; `Eq` method naming (`==` operator vs a named `equals`).
- Whether Stage 5 (`IntArith`) lands before Stages 0/4 (`Throw`) as the de-risking order.
- Whether/when to pursue the optional definition-time disjointness proof and a probe-based coverage lint
  (tooling, not semantics).

## Appendix: why `Dep[A]`/`Dep[B]` does not need this

`Dep[X, F]` is a reader (`dependency: F[X]`); multiple dependencies resolve on the **same** flat carrier
by type-keyed dispatch — no `DepCarrier`, no stacking, no base+lift template pair, so distinct concrete
`Dep[Database]`/`Dep[Topic]` never overlap. Only effects that **represent** their payload and therefore
stack as transformers (`Throw`, and `State` at two distinct state types) need the self-lift and hence the
guard. Cross-lifts (effect A through a *different* carrier B — `State`-through-`Abort`) do not overlap
because the carrier shapes differ.
