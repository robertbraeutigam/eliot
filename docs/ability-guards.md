# Ability Implementation Guards

Status: **Stages 0–6 landed** (Stage 6: 2026-07-06), plus the **use-site diagnostics follow-up** (§5a,
2026-07-06): resolution outcomes ride the `AbilityImplementation` fact and every failed *demand* — no
applicable instance, a guard `error(msg)` rejection, an ambiguity — is reported at the demanding use-site
reference, not at the ability/marker declaration. Revised 2026-07-05: the coherence stance is
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
- **Placement**: `ability Eq` abstract in the base stdlib; the `Eq[Type]` instance lives in the **`lang`
  base layer** (`lang/.../Eq.els`), so it resolves on *both* source paths — a guard is discharged on the
  queried platform's track (§5 Stage 4), so a runtime-layer guarded instance needs `Eq[Type]` resolvable
  under the runtime pool too. The comparison still only *reduces* at compile time (`typeEquals` is a
  compiler-supplied native; types are erased at runtime). The `Eq[Type]` instance must itself be
  **unguarded** — a guard on it would demand its own resolution (same fact key → engine cycle error). Base
  value instances (`Eq[Int]`, `Eq[String]`, `Eq[BigInteger]`) as needed (these can have runtime bodies);
  `Eq` for `data` derivable later.

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
- `Right(false)` → decline (dropped from the candidates);
- `Left(msg)`    → a `Rejected(msg)` outcome, reported at each demanding use site (§5a);
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
- `Eq[Type]` instance + its `typeEquals` leaf live in `lang/.../Eq.els` (the base layer, visible on both
  source paths — which Stage 4's queried-platform discharge relies on; the leaf still only reduces at
  compile time), with the ability re-declared there per the layer-merge duplication rule. The
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

**Stage 1 — marker synthesis: guard as return slot, body retired. LANDED (2026-07-05).**
- Parse `where <expr>` in `ast/fact/ImplementBlock.scala` (between the head-pattern parse and the body),
  soft keyword via `identifierWith("where")`. Parsed with **`Expression.typeRunParser`** (the return-type
  parser), which gives the `{` parse boundary of §2.1 *for free* — `{` is not a type-atom start, so the
  greedy type-run ends there — and lets an infix guard like `E1 != E2` read without parentheses. Tested.
- Marker: return slot = guard expression, **body = `None`**, **uniformly for all markers** — the
  `ImplementBlock` marker *and* the two `data`-generated markers (`DataDefinitionDesugarer`'s
  `PatternMatch`/`TypeMatch` impl markers). The default (unguarded) guard is the literal `true`, written as
  the **module-qualified** reference `eliot.lang.Bool::true` (`Expression.trueReference`, shared by both
  synthesis sites) so it resolves in every module *without an import* (the resolver's `module::name` path
  looks up by FQN, bypassing import scope).
- Guard type-checking against `{Throw[String]} Bool` is **deferred to the use-site discharge (Stage 2)** —
  markers are never monomorphized (not reached from `main`), so there is no definition-time check hook, and
  deferring to the use site is exactly the Sound-Not-Modular stance (§3). The default `true` is trivially
  well-typed; no real guarded marker exists until Stage 4.
- Test: `implement … where <expr> { }` parses/round-trips (return slot = guard, body-less, guard stops at
  `{`); **full suite green**.

Two things the body retirement + `true`-return surfaced (both fixed here, neither anticipated verbatim by
the plan's spikes):
- **`AbilityImplementationCheckProcessor.checkSignatures` must skip the marker.** The marker (local name ==
  ability name) is in both `abilityMethods` and `implMethods`, so `checkSignatures` paired the ability
  marker (return = first generic → the pattern) against the impl marker (return now `true`) and
  `signaturesCompatible` unified the guard against the pattern → spurious "Signature … does not match".
  Fixed by excluding the marker from the *signature*-compat pairing only (its pattern is already validated
  at the same use site by `matchImpl`); completeness/extras keep the marker on both sides. Spike 4 assumed
  "peelPattern drops returns" sufficed, but the marker is *also* passed as a method sig (peeled to body,
  keeping the return) — that gap.
- **`AbilityMatcher.classifyValueRef` must not abort on an unproduced reference.** It looks up
  `OperatorResolvedValue` in the (default `Runtime`) platform; the old marker return referenced only
  `Type`-qualified names (classified without the fact), but the new `true` is `Default`-qualified. In a
  compiler-pool-only match the Runtime lookup misses and it aborted "Can not evaluate referenced value."
  Fixed by classifying an unproduced reference as a stuck leaf (`bodylessBinding`, mirroring the body-less
  case) instead of aborting — the marker's guard return is dropped by `extractFunctionArgs` before
  evaluation anyway, so matching never needs its value.

  Test-harness consequence: every synthesized `implement`/`data` marker now depends on `eliot.lang.Bool`
  being in the pool (real builds always have it). `ProcessorTest.systemImports` registers `Bool` (loadable,
  not auto-imported); custom-source-set tests that build their own pools (`CompilerAbilityResolutionTest`,
  `PlatformScopedAbilityResolutionTest`, `MonomorphicTypeCheckProcessorTest`, `JvmClassGeneratorProcessorTest`)
  register a `Bool` stub the same way.

**Stage 2 — use-site discharge + 3-way interpretation. LANDED (2026-07-05).**
- `AbilityImplementationProcessor.verifyImplementation` now discharges the guard after a `matchImpl` `Some`
  (`dischargeGuard`). An **unguarded** marker (return slot is exactly the default `eliot.lang.Bool::true`
  reference — detected via `SignatureView.of(markerSig).returnType`) is kept verbatim, *without* monomorphizing
  the marker (the pre-guards behaviour + the efficiency win — only real guards pay). A **real** guard is
  discharged by requesting `CompilerMonomorphicValue.Key(markerVfqn, groundArgs)` — the marker on the
  **compiler track**, always, regardless of the queried `platform`, since guards are compile-time computations —
  and reading `signature.deepReturnType` (§3.1's "deep-return read", as a *direct fact request* from the ability
  processor, not a `CheckIO` back-edge, so the `CalculatedReturnResolver.scala:183` compiler-track re-entry
  blocker never applies). `interpretGuard` maps the reduced verdict: `Bool` `true` / `Right(true)` → keep,
  `Bool` `false` / `Right(false)` → decline (`Seq.empty` → resolution falls through), `Left(msg)` →
  `compilerError(msg) >> abort` (hard). Both `Bool` shapes are accepted — a body-less `Bool::true`/`false`
  reference (`GroundValue.Structure` head) and a native-reduced `GroundValue.Direct(Boolean)`.
- The kind check accepts a `Bool` guard return: `CalculatedReturnResolver.isGuardCarrier` now recognises a
  `boolFQN`-headed *inferred type* (a `Bool`-typed value in a type position) in addition to the W2b `eitherFQN`
  carrier. A *normal* `Bool` return type still infers kind `Type`, so this only fires for a guard.
- **Fail-safe (`metaToGround` caveat, §3.1):** `AbilityMatcher.matchImpl` now returns a `Match(groundArgs,
  allTraced)`; `allTraced` is false when any leading param's binding fell back to the lossy `metaToGround`
  (`GroundValue.Type`-collapsing) path rather than the faithful `tracePatternMetas`. A real guard over an
  untraced binding is an internal error, not a silent (possibly-wrong) verdict. (Conservative: refuses on *any*
  untraced param, not only guard-mentioned ones — safe, and `false` never arises for the planned clients whose
  params are all structural.) A stuck / unrecognised verdict is likewise an internal error, never a silent keep.
- **Verified:** `AbilityGuardDischargeTest` (compiler-pool, self-contained `Bool`+`Either`, no effect machinery)
  — unguarded keeps without discharge; `where fold(false,false,true)` keeps; `where false` declines (→ "No
  ability implementation found"); `where Right(true)`/`Right(false)` keep/decline via the payload; `where
  Left("…")` is a hard error carrying the message. Full suite green + `HelloWorld` builds & runs.
- **Stage-4 watch item** (surfaced here): the discharge reads the *reduced signature return*. That reduces
  cleanly for constructor / native-direct guards (`true`/`false`/`fold`/`Left`/`Right`). An **ability-dispatched**
  guard (`E1 != E2` via `Eq[Type]::equals`) must fully reduce *in the signature-return position* — but
  `PostDrainQuoter.quoteSem` renormalises the SemValue with only the marker's own binding cache + drain-resolved
  `implBindings`, which do **not** include an ability method's transitive impl body unless it was resolved during
  the marker's own check. If `E1 != E2` stays stuck, the discharge fail-safe-errors (never miscompiles), so
  Stage 4 must ensure the `Eq[Type]` guard reduces (e.g. apply `abilityResolutions` to the signature SemValue
  before read-back, mirroring `resolveAbilityRefs` for bodies), or route the guard through the body-reduction
  (`reduceSourced`) path instead of the signature-return read.

**Stage 3 — definition-time overlap becomes the conservative lint (§3.2). LANDED (2026-07-05).**
- The overlap scan now reports overlap only when the patterns structurally overlap AND *both* markers are
  unguarded; any explicit `where` guard on either side → defer silently to the use site. The "unguarded"
  test (return-slot is exactly the default `eliot.lang.Bool::true` reference) was **lifted out of
  `AbilityImplementationProcessor` into `AbilityMatcher.isUnguarded`** — one shared definition, so the
  Stage-2 discharge (skip discharge for an unguarded marker) and the Stage-3 lint (defer a guarded pair)
  can't drift. `AbilityMatcher.patternsOverlap` stays a **purely structural** predicate (unchanged); the
  guard-deferral policy lives in `ModuleAbilityOverlapCheckProcessor.reportOverlaps` (the lint owner),
  which decides "report only if `isUnguarded(sigA) && isUnguarded(sigB)`". This is the minimal §3.2 arm
  set (unguarded-overlap → error; any guard → defer) — no symbolic guard evaluation, no MGU-binding
  evaluation, no `Eq` reflexivity on the critical path.
- **Verified:** `AbilityGuardOverlapLintTest` (compiler-pool, self-contained `Bool`+`Function`, mirrors
  `AbilityGuardDischargeTest`) — triggering the overlap-check fact directly: two unguarded overlapping
  impls → 2 "Overlapping" errors (pre-guards behaviour preserved); native (unguarded) + guarded → 0
  errors; two guarded → 0 errors. The third §3.2 consequence end-to-end: two guarded impls whose guards
  both reduce to `true` pass the definition-time lint yet produce "Multiple ability implementations" at the
  concrete use site (`AbilityImplementation.Key`). Full `lang` (832) + `jvm` (173) suites green.
- (Optional, later, feedback-only: evaluate guards under the MGU to prove the diagonal disjoint —
  requires the symbolic `Eq` semantics.)

**Stage 4 — the `Throw` client. LANDED (2026-07-05).**
- Added the guarded lift `implement[E1, E2, G[_] ~ Throw[E2] & Effect] Throw[E2, ThrowCarrier[E1, G]] where E1 != E2`
  to `jvm/.../effect/Throw.els` beside the native `Throw[E, ThrowCarrier[E, G]]` (`import eliot.lang.Eq` for `!=`).
- The Stage-2 discharge design (monomorphize the marker on the *compiler* track, always) did **not** survive contact
  with a runtime-layer marker; three problems surfaced and were fixed, all reusable by any guard client:
  1. **Marker lives only in the queried pool.** The `Throw` marker is in the `jvm` (runtime) layer, absent from the
     compiler path, so a compiler-track `CompilerMonomorphicValue` for it "Could not find" it. Fix:
     `AbilityImplementationProcessor.dischargeGuard` now monomorphizes the marker on the **queried `platform`'s track**
     (`MonomorphicValue` on runtime, `CompilerMonomorphicValue` on compiler) — the pool the impl was found in. The
     guard is still compile-time, but reduces on either track (`Eq[Type]` is in the base layer on both paths; its
     `typeEquals` leaf is a platform-agnostic native).
  2. **Higher-kinded pattern-arg types fail the kind check.** A marker's argument types encode the ability head, so a
     carrier pattern (`arg1: ThrowCarrier[E1, G]`, kind `Type -> Type`) is not kind `Type` and the full checker rejects
     it. Fix: `MarkerGuardSignature.strippedForGuard` (applied in both mono processors) drops the pattern-argument
     arrows before monomorphizing a marker, leaving *binders + guard return* — the only part the guard needs. Safe:
     markers are monomorphized *only* for guard discharge.
  3. **The ability-dispatched guard stays stuck at signature read-back** (the Stage-2 watch item, confirmed). `E1 != E2`
     unfolds to `fold(equals(E1,E2), false, true)`, and `equals` (the `Eq` method) is reached only *inside* `!=`'s body,
     so it never surfaces as a directly-collected ability ref — the type-stack walk inlines `!=`'s *generic* native and
     `equals` stays a stuck native. Fix (`TypeStackLoop`, gated on a body-less guarded signature): reduce each bodied
     sub-value the guard calls (`!=`) per-instantiation on the compiler track (`ReducedBindingClosure.reduceInstance` →
     `CompilerMonomorphicValue(!=, [Type]).reduced`, `equals` already resolved to `Eq[Type]::equals` with its
     `typeEquals` leaf folded), then **re-evaluate the guard return** against the concrete binder env with those reduced
     bindings seeded, so the guard collapses to a concrete `Bool` the discharge reads. Neither the plan's first
     suggestion (apply `abilityResolutions` to the signature SemValue) alone sufficed — `equals` is never *collected* —
     so this is the "route the guard through a reduced form" alternative, done as a targeted re-evaluation.
- **Verified:** `examples/src/EffectsTwoThrows.els` compiles and runs (`http 503`); `ExamplesIntegrationTest2` gains
  the off-diagonal lift (two distinct error types, both catch orders), the degenerate diagonal (two same-typed throws
  → the lift declines via `where String != String`, the native wins with no ambiguity), and `EffectsThrow`/`EffectsTwoDeps`
  regress clean. Full `lang` (832) + `jvm` (176) suites green; `HelloWorld` builds & runs.

**Stage 5 — the `IntArith` client (§4.1). LANDED (2026-07-05).**
- Replaced `dispatchAdd`/`dispatchSubtract`/`dispatchMultiply` in `jvm/.../lang/Int.els` with `ability
  IntArith[Cmin, Cmax] { addOp; subtractOp; multiplyOp }` and **five range-guarded instances** (byte/short/int/
  long/big). Each guard is the explicit disjoint complement of the ordered chain's implicit priority —
  `fitsIn[thisWidth, Cmin, Cmax] && not(fitsIn[nextSmaller, Cmin, Cmax])` — with the byte instance unguarded-by-
  complement (`fitsIn[byte]`) and the big instance the pure `not(fitsIn[long])`. Each method body keeps the inner
  *result*-width `fold` (unchanged from the old dispatch). `+`/`-`/`*` now call `addOp`/`subtractOp`/`multiplyOp`
  (a use-site-deferred ability call — no `IntArith` constraint on `+`, resolved at each concrete instantiation).
- Needed one base addition: **`def not(b: Bool): Bool = fold(b, false, true)`** in `lang/.../lang/Bool.els`
  (platform-independent; where `fold`/`true`/`false` are in scope). No `Eq` — the guards use only the
  already-compiler-evaluable `fitsIn`/`&&`/`not`/`lessThanOrEqual` natives, so they reduce through the *standard*
  signature evaluation (the Stage-4 ability-dispatched machinery is not engaged: the guards' sub-values carry no
  ground type arguments, so `reduceGuardSubValues` yields nothing and the ordinary checkSig reduction discharges them).
- **Verified:** `Arithmetic`/`Ranges` and the whole examples suite compile and run unchanged; new
  `ExamplesIntegrationTest4` cases: a four-width ladder (byte/short/int/long selected in one program) and the
  headline gap-error — a user `Widen` family covering only the byte range, used at `Int[0,100000]`, hard-errors
  "No ability implementation found for ability 'Widen'" at the manifest instantiation (no silent mis-select).
  Full `lang` (832) + `jvm` (178) suites green. Watch items resolved: lambda-free bodies → no same-module lambda
  collision; resolution cost is fine on the suite. (Orthogonal pre-existing find, *not* Stage 5: the
  `JvmBigInteger` representation overflows for values beyond `long` — the big-operand instance and any
  long→bignum promotion are codegen-untested; unchanged by this refactor.)

**Stage 6 — guarded `Coerce` (§4.2). LANDED (2026-07-06).**
- Retired the `Option`-decline protocol. The `Coerce` ability now returns `To` directly
  (`lang/.../compiler/Coerce.els`: `def coerce(value: From): To`), and the jvm instance is guarded:
  `implement[…] Coerce[Int[Smin,Smax], Int[Tmin,Tmax]] where lessThanOrEqual(Tmin, Smin) &&
  lessThanOrEqual(Smax, Tmax) { def coerce(value) = nativeWiden(value) }` — total body, no `some`/`none`.
- **The caveat, resolved.** "No applicable instance" for `Coerce` must be a *soft* "Type mismatch." at the use
  site, not the hard "No ability implementation found" a user ability (or an `IntArith` coverage gap) gets. The
  hard error is produced by the *producer* (`AbilityImplementationProcessor.handleMissingImplementation`), and
  the checker's coercion probe (`RefinementSolver`) triggers it via `getFactIfProduced`. Fix at the time: that
  producer *declined* (aborted with no error) when the ability was `Coerce` (`isCoerce`, keyed on
  `WellKnownTypes.coerceFQN`) and there was no default — `Coerce` is checker-inserted and its non-applicability
  is a normal answer. Every other ability still hard-errored on no-match, so the `IntArith` gap behaviour was
  unchanged. **Superseded by §5a**: the producer no longer errors for *any* ability — it registers the
  resolution outcome, probes read a failed outcome as a decline, and demands report at the use site — so the
  name-keyed `isCoerce` carve-out is deleted.
- `RefinementSolver.coercionPayload` unifies the impl signature against `VPi(actual, _ => expected)` (was
  `Option[expected]`) and returns the evaluated body (`nativeWiden(marker)`) directly as the splice payload; the
  guard having resolved the instance *is* the existence proof, so there is no `some`/`none` to interpret.
- Verified: full `lang` (832) + `jvm` (179) suites green; the narrowing-rejection tests
  (`MonomorphicTypeCheckTest`: `Int[0,3] = 5`, `Byte = 5000`, `Int[0,3] = integerLiteral[5]`) still report the
  clean single "Type mismatch." (the caveat holds); real-layer widening of both a parameter and a literal works.
  Stub-fidelity fix along the way: the two `ProcessorTest` `Coerce` stubs were under-annotated
  (`implement[Smin, …]` with no `: BigInteger`); the `Option` body tolerated the kind default (`Type`) but the
  guard's `lessThanOrEqual` — which needs `BigInteger` — surfaced it as a spurious `Type → BigInteger` coercion,
  so the stubs now match the real instance's `[Smin: BigInteger, …]`.

**All six stages (0–6) are landed; the ability-guards plan is complete.**

**§5a — Follow-up: use-site diagnostics (outcome-carrying resolution). LANDED (2026-07-06).**

Review findings after Stage 6: the resolution errors were anchored in library code — "No ability
implementation found" at the *ability declaration*, a guard `error(msg)` at the *marker* — and a plain
cross-constructor mismatch (`String` where `Int[..]` expected) emitted a spurious "The type parameter
'String, Int' does not implement ability 'Coerce'." against the shipped `Coerce.els` (the check
processor's structural-no-match error, which the `isCoerce` carve-out never covered). Root cause: the
*producers* reported resolution failures, but only the *demander* holds the use-site position — and for a
checker-inserted probe a failure is not an error at all. Fix, in one move:

- **The `AbilityImplementation` fact carries a `Resolution` outcome** — `Resolved(implFQN, implTypeArgs)` /
  `NoImplementation` / `Rejected(messages)` / `Ambiguous` — and the producer registers it *without erroring*
  (`AbilityImplementationProcessor` aggregates per-candidate `Keep`/`Decline`/`Reject` verdicts; the
  check processor's structural-no-match arm no longer errors either). An **absent** fact now always means an
  upstream error already reported at its own definition.
- **Demands report at the use site**: the checker's saturation pass (`AbilityResolver.reportFailedDemand`)
  holds the demanding `Sourced` reference; on a ground-argument demand whose fact carries a failed outcome it
  reports there — "No ability implementation found …" / the guard's own `Rejected` messages / "Multiple
  ability implementations …" — and aborts the value's monomorphization (the §3 hard error at the manifest
  instantiation). **Probes stay silent**: `Coerce`/`Combine`/the lift arms read a failed outcome as a decline,
  which *deletes the `isCoerce` carve-out* and fixes the spurious cross-constructor `Coerce` diagnostic
  (regression: `MonomorphicTypeCheckTest` "single mismatch for a cross-constructor mismatch").
- **A marker's guard survives to its published signature**: the runtime track's W2b signature discharge is
  skipped for ability-implementation markers (`Track.Runtime.settleGuardedReturn` via the now-public
  `MarkerGuardSignature.isMarker`) — previously a runtime-pool guard's `Left(msg)` hard-errored *inside the
  marker's own monomorphization*, anchored at the marker; now the verdict reaches `interpretGuard`, rides the
  fact as `Rejected`, and is reported at the demanding reference. (The compiler track already left marker
  signatures undischarged, which is why the Stage-2 unit test saw the verdict.)
- **One guard-verdict protocol**: the `Right`/`Left` payload-position convention and the rejection fallback
  message are single-sourced in `monomorphize/check/GuardChannel`, shared by the W2b signature discharge
  (`CalculatedReturnResolver`) and the ability-guard verdict interpreter.
- Also retired: the `handleDefaultImplementation` internal-error corner (a default whose every sibling
  declined is now an ordinary `NoImplementation`), and the "The type parameter … does not implement ability"
  message (folded into the demander's "No ability implementation found for ability 'X' with type arguments
  […]." at the use site).
- **Test-harness consequence**: a unit scenario that monomorphizes a value at a *ground* stub carrier must
  supply instances for the abilities it demands (a failed ground demand is now reported in-generation, which
  suppresses the value's fact) — the effect-lift and carrier-bookkeeping stubs gained trivial
  `implement …[IO]` instances delegating to an abstract helper.

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
- **`Eq` placement**: abstract in base; the `Eq[Type]` instance in the `lang` base layer (both source
  paths — the queried-platform discharge needs it under the runtime pool too), unguarded (§2.2).
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
