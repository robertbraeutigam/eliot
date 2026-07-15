# Signature unification — one execution path for signatures

**Status: PLANNED (2026-07-15).** Supersedes and **replaces** `docs/signature-split.md` (deleted). That plan's Steps
0–8 + 10 are LANDED on master: every named value splits at birth into `v@Runtime` + `v@Signature` twins, the signature
twin monomorphizes on the compiler track (`CompilerMonomorphicValue(v@Signature, args)`), the payoff feature (inline
`if..else..raise` / bare `raise` guards) works end-to-end, and `when`/`orError` is retired. Its Step 9 — the checker
deletion sweep — did **not** realize: the shipped Steps 5–7 chose a *gated flip* (the twin read runs *beside* a retained
in-place walk), under which every deletion target is load-bearing. This plan is the deferred rewrite that un-gates the
flip, so the in-place walk and the guard machinery become genuinely unreferenced and delete. Durable evidence from the
old plan is absorbed in §9.

## 0. Goal — read this first

**One execution path.** After this plan, there is exactly one way a signature is computed: its signature twin's
monomorphization, which is an **ordinary body monomorphization** (the same check → elaborate → drain → read-back
pipeline every body takes), and exactly one way a signature is consumed: **read the twin's fact and settle at the
read** (discharge a guard verdict, install a W3 return meta, or pass through). No in-place signature walk, no guard
state flag, no track-switched return settling, no marker re-evaluation, no guard-scoped reduction machinery.

The measure of success (the stop rule): `monomorphize/check` gets **smaller and simpler** — the §5 deletion ledger
must realize, and nothing on the §6 survivors list may grow a new signature-specific branch. If a phase can only stay
green by adding a new gate, stop and re-derive here.

What this is *not*: a feature. Every user-visible behaviour (guards, calculated returns, ability `where` guards,
effectful signatures) already works. This is the payment of the debt the gated flip took on — the second,
special-cased signature path whose existence is close to the second-evaluator anti-pattern the cornerstone forbids.

## 1. What the gated flip left standing — the second paths, precisely

Each of these is a place where signature execution takes a path a body would not:

- **(a) The twin's own mono is the old walk behind a fact key.** `TypeStackLoop(signatureOnly = true)` runs
  `walkTypeStack` (kind-check levels + shallow `evalExpr`), sets `bodyToCheck = None`, and then *patches* the shallow
  result: `reduceGuardSubValues` / `reduceResolvedImpls` pre-compose per-instantiation reduced bindings (with the
  `ReducedBindingClosure.reduceInstance(recursive = true)` flag and the `absorbLeadingArgs` wrapper), and
  `quoteSignature` deep-reduces the peeled return (`reduceSemExprToGround`) — gated on `signatureOnly && sawGuard &&
  !isMarker`. That is in-walk kernel-reduction re-composition: the signature is *not* reduced by the ordinary
  `Track.Compiler.readBackBody` (`reduceSourced`) path bodies take.
- **(b) The value mono keeps an in-place fallback arm.** `establishSignature` walks in place whenever the twin fact is
  absent (W3 declined, marker excluded, twin errored) or the application is partial. Every signature-error path is
  therefore *computable twice*.
- **(c) The return settle is a track switch + a state flag.** `Track.settleReturnPosition` (calc / guard / pass-through)
  with `Track.Runtime.settleGuardedReturn` → `dischargeGuardedSignature`, driven by `CheckState.sawGuardReturn` (set by
  the kind-check carve, threaded through `TypeStackLoop`).
- **(d) Markers have their own path.** `AbilityImplementationProcessor.readGuardVerdict` monomorphizes the
  **Runtime-role** marker on the queried platform's track; the marker is excluded from both flips, and its guard is
  reduced by the Stage-4 workaround (`reduceGuardSubValues` without absorb + `reevaluateGuardReturn` re-evaluating the
  raw signature return in the kernel).
- **(e) W3 has three shapes.** The twin *declines* (`signatureOnly && isCalc → abort`); the value mono walks in place
  with `flattenReturnToType` (the kind-correct placeholder) + `installReturnMeta`; callers take the
  `readMonomorphicReturn` back-edge. Two of the three are signature-special.
- **(f) The callee flip carries three gates** (`Checker.flippedCalleeSignature`): `signatureOnly` (acyclicity —
  structural, must stay), marker exclusion (deletable once (d) is fixed), and full-application + ground-args (the
  inference boundary — stays, see §6).
- **(g) The front-end dual slot.** `NamedValue.runtime`/`signature`, the runtime twin's inert `.signature` placeholder,
  and `signatureEquality` as a bespoke comparator — threading, not semantics, but it is the reason every front-end
  phase has an own-signature arm.

## 2. The corrected account — why the ledger failed, and the good news

The old plan's §6 recorded the load-bearing proof imprecisely, and the correction matters because it *shrinks* the
rewrite:

**The flip already fires for guarded values.** An inline guard adds no binder (its carrier arises from `else`/`raise`
instantiation, not from `paramConstraints`), so `greeting[COND]`'s runtime mono at key `[true]` is fully applied and
ground — `establishSignature` takes the flip arm and injects the twin's verdict (`Right(String)` / `Left(msg)`). The
"guards are partially applied, so the flip is gated off" explanation was wrong. What the no-op experiment (6 reddened
tests) actually proved: `Track.Runtime.settleGuardedReturn → dischargeGuardedSignature` is **the discharge-at-the-read
site for the value's own twin read** — `dischargeGuardedSignature` peels the re-inflated injected signature and
discharges `Right(t) ⤳ t` / `Left(msg) ⤳ author error`. It is not an in-place-walk dependency; it is the read-boundary
discharge wearing the old Track-switch clothes.

Consequence: the guard consumer side needs a **relocation, not new machinery** — a lean, uniform "settle at the read"
(one helper, role- and track-agnostic) replaces the switch, the flag, and the stuck-guard-defer arm. The genuinely hard
work is confined to the *producer* side (making the twin's mono an ordinary body mono, §4.1–4.2) and to markers/W3
(§4.3–4.4).

## 3. The target model

### 3.1 The twin's mono is an ordinary body mono (producer)

`CompilerMonomorphicValue(v@Signature, args)` is produced exactly like any compiler-track value, with the signature
expression's *arrow chain* as the body:

1. **Binders**: `SignatureView.of(sig).binders` — check each declared binder kind against `VType` (ordinary checks,
   replacing the old kind-level walk), then bind the ground `args` in ρ/Γ (`bindTypeStackParam`, the existing flip-arm
   code). The derived kind is never minted or stored — it exists only as this per-binder discipline.
2. **Body**: the binder-stripped arrow chain (`parameters → returnType`, `SignatureView.toExpression` without binders),
   checked by **one ordinary `checker.check(body, VType)`**. The shared resolution ladder already covers every return
   shape (§3.3). Elaboration (effect lift), ability drain, `pinCarriers` (CP-D + the inferred-return pin) run exactly
   as they do for any compiler-track body — they already do today; only the check's entry shape changes.
3. **Read-back**: `Track.Compiler.readBackBody` (`reduceSourced`) — the *ordinary* compile-time reduction, upgraded
   once for everyone by §3.4 — reduces the checked body. The published `signature` is that ground: a plain type for an
   ordinary signature, `Right(t)`/`Left(msg)` for a guard, a `Bool`/`Right(Bool)`/`Left` verdict for a marker, an
   **under-applied constructor head** for W3 (§3.5). No `quoteSignature` special path, no `peelSignatureBinders`, no
   `sawGuard` gate, no marker/W3 exclusion: **every signature twin produces**.

Markers keep their one input normalization: `MarkerGuardSignature.strippedForGuard` (pattern-argument arrows are not
kind-checkable; the verdict depends only on binders) — applied where it is applied today, in the processor, now only
for `Signature`-role marker keys.

### 3.2 Every consumer reads the twin and settles at the read

- **The value's own mono** (both tracks, `Runtime`-role keys): the twin fact is **mandatory** — read it, re-inflate
  (`Evaluator.groundToSemPi`), bind binders (the existing flip arm), then run the one **read-settle**:
  - ground return `Right`/`Left`-headed ⟹ discharge (`dischargeGuardedReturn`: payload / author error at the
    established site);
  - ground return with arity shortfall (`isCalculatedReturn`) ⟹ W3: a bodied value gets `installReturnMeta` (the body
    solves it); a body-less value gets the W4 "must state its return type explicitly" error (relocated from
    `failOnAbstractCalculatedReturn`);
  - otherwise pass through.
  A twin that failed to produce ⟹ **abort** (`getFactOrAbort` semantics: the twin already reported the signature's
  errors; no second walk, no double reporting). The in-place arm of `establishSignature`, `walkTypeStack`,
  `flattenReturnToType`, `Track.settleReturnPosition`/`settleGuardedReturn`, `dischargeGuardedSignature`, and
  `CheckState.sawGuardReturn` all delete.
- **Callee references** (`Checker.inferValueReference`): the flip stays gated on *full application + ground args* —
  that gate is the inference boundary, not a defect (§6) — and the read-side settles exactly as today
  (`resolveCompleteCalculatedReturn` → `dischargeGuardedReturn`, already in the right order). The **marker exclusion
  deletes**. The `signatureOnly` gate **stays** (acyclicity, §4.6). The non-ground fallback stays as *symbolic
  evaluation of the callee's signature expression* — `evalExpr` + `peelLams` metas — which is the one NbE evaluator
  doing inference, not a second path.
- **Markers** (`AbilityImplementationProcessor`): the verdict is **one read** —
  `CompilerMonomorphicValue(marker@Signature, matched.groundArgs).signature` (deep return) — replacing the
  platform-switched Runtime-role marker mono. Signature twins are compiler-track by construction, and the compiler
  pool borrows the entire runtime track, so a runtime-layer instance's marker twin resolves there (§4.3 verifies the
  guard's leaves). `reevaluateGuardReturn`, the marker arms in both mono processors, and `readGuardVerdict`'s platform
  switch delete. The `interpretGuard` verdict protocol (`GuardChannel`) is untouched.

### 3.3 One return-position discipline, stateless, in the shared ladder

A signature's return position may legitimately be, where `Type` is expected:

1. an ordinary type (kind `Type`) — the default;
2. a **guard**: a carrier-typed *value* (`Either[..]`-headed, `Bool`-headed, or an effect-carrier-meta-headed
   application) denoting a computation that yields a type or rejects — the existing `isGuardCarrier` recognition;
3. a **W3 hole**: an under-applied omittable constructor head (`Counter`, bare `Int`) whose missing arguments the body
   computes — the existing `isCalculatedReturn` recognition, newly used as a ladder acceptance (today it is a pre-walk
   detection that triggers `flattenReturnToType`).

These three acceptances live in **one place** — the shared check-mode resolution ladder (`resolveGuardedLadder`),
which both return boundaries and argument slots already front — as *stateless* arms: accept and move on, no
`recordGuardReturn`, no flag, no downstream reader. The producer's reduction is shape-agnostic (an ordinary signature
reduces to its type, a guard to its verdict, a W3 return stays under-applied); the consumer's read-settle recognizes
the shapes on the *ground* form. This is the honest residue of "signatures are values": a signature's return has a
three-shape discipline no ordinary body position has, expressed once, statelessly, at the boundary that checks it.

### 3.4 The keystone mechanism: stuck-driven escalation in compile-time read-back

The reason (a)'s guard machinery exists: the ordinary read-back closes dependencies over **raw one-hop**
`NativeBinding`s, and a stacked carrier (`AbortCarrier` over `Either[String]`) leaves an impl body's *nested* base
ability call abstract — the reduction bottoms out at a stuck `match`. Step 7 compensated with eager, guard-scoped,
recursive pre-composition (`reduceGuardSubValues(recursive = true)` + `absorbLeadingArgs`); an earlier global attempt
(reducing every dependency of every instance) broke 6 examples by demanding monos at *defaulted* instantiations (a
`Compare` dependency at a defaulted `Type` argument).

The replacement is **escalation on stuck, in the ordinary read-back loop**:

1. Evaluate the checked expression with the current bindings (raw one-hop, exactly today's `reduceSourced`).
2. If the result quotes — done (the common case pays nothing).
3. If it is stuck: collect the expression's value references **with their ground type arguments** (the
   `SemExpression.ValueReference` nodes carry them — today's `collectValueRefs`), fetch each one's
   *reduced-at-its-instantiation* form (`CompilerMonomorphicValue(ref, groundArgs).reduced`, closed over raw deps),
   merge into the binding lookup, re-evaluate. Loop until it quotes or no new binding was added.
4. Still stuck ⟹ the existing fail-safes, unchanged: a compiler-track *function* body falls back to the structural
   quote (legitimate — runtime parameters stay neutral); a signature return hard-errors ("Cannot resolve type.").

Why this avoids the old regression: escalation only ever demands a mono for a reference **that actually blocks the
result**, at the **concrete arguments the expression itself carries** — the `Compare`-at-defaulted-`Type` collateral
was a product of *eager* recursion into every dependency. Why it terminates: each round adds bindings for a strictly
growing subset of the expression's finite reference set; the per-reference demand is a fact demand (memoized, and
`activeFactKeys`-guarded against re-entry). Why recursion disappears: each fetched `CompilerMonomorphicValue.reduced`
was itself produced by a read-back that ran this same loop, so it is already resolved at its own instantiation — the
recursion moves into the fact graph, where it is cached and cycle-guarded, instead of being re-implemented as a
closure-composition flag.

This deletes `ReducedBindingClosure`'s `recursive` flag, `absorbLeadingArgs`, `reduceGuardSubValues`,
`reduceResolvedImpls`, and the guard-scoped binding maps in `TypeStackLoop`'s quoter assembly — and it upgrades
*every* compile-time reduction (markers, future stacked-carrier user code), not just guards.

### 3.5 W3 flows through the twin (the hole is representable today)

The twin for a W3 value no longer declines: the ladder accepts the under-applied return (§3.3 arm 3), and the
read-back publishes it as-is. **This is representable with no fact-shape change**: an under-applied W3 head is an
abstract type constructor — a body-less `VTopDef` — and `Quoter.quote` already grounds `VTopDef(fqn, None, spine)` to
`GroundValue.Structure(fqn, args)` at *any* arity (verified; the "Cannot quote unapplied top-level definition" arm is
for *bodied* top-defs only). The arity knowledge stays where it is (`SaturatedValue.inferableArity`), and the
consumer-side recognition (`isCalculatedReturn` via `arityShortfall`) works on the re-inflated form unchanged.

The two existing W3 reads are untouched: the value's own body solves the installed return meta
(`installReturnMeta` + `failOnUndeterminedCalculatedReturn`), and callers read the *solved* return off the value mono
(`readMonomorphicReturn`, own-track, `activeFactKeys`-guarded). What changes is only that the signature's *shape*
(arrows + hole) now comes from the twin like everyone else's — `flattenReturnToType` and the twin's decline delete,
and `isCalculatedReturnExpr` (the ORE-form pre-detection) becomes unnecessary.

## 4. The difficult parts, in depth

### 4.1 Escalation correctness (the keystone risk)

Open verification from Step 7's evidence: the S7 notes claim an impl reduced at its concrete instantiation still
carried an *abstract* nested base-ability call — which contradicts the "each CMV's read-back resolves its own refs"
argument above (the impl's own drain should have resolved `pure` at `AbortCarrier[Either[String]]`). Hypothesis: the
S7 fetches reduced some dependencies at *generic or partially-defaulted* instantiations (the raw `NativeBinding`
route), where the nested resolution genuinely cannot happen — escalation's fetch-at-the-expression's-own-ground-args
discipline is exactly what fixes that. **Phase A must prove this on the stacked-carrier fixture before anything else
lands**; the fallback design, if the hypothesis fails, is an explicit `deep` mode on the escalation fetch (close
fetched bindings over reduced-at-instantiation deps) — still shape-agnostic and stuck-driven, never guard-gated.

**PHASE A OUTCOME (landed): the pure one-hop hypothesis is FALSE; the `deep`-mode fallback is needed and shipped.**
One-hop escalation over the expression's own top-level ground-arg refs reduces the *inline* `if..else..raise` guard
(`greeting[COND]`) — both the satisfied `Right(t)` and the rejected `Left(msg)` — because the guard's combinators
(`else`/`if`/`raise`) *are* the top-level refs, so each is fetched reduced at its own concrete stacked-carrier
instantiation. It does **not** reduce a guard written through a *user function* (`guardOr[A](cond, value) =
if(cond, value) else raise(...)`, used as `String[] |> guardOr(COND)`): there the guard combinators are nested inside
`guardOr`'s body, `guardOr[String].reduced` is a **structural function** read-back (its impl-dispatch deps close over
its runtime value parameters `cond`/`value`, so `reduceSourced` correctly keeps it structural — the `VLam` exclusion),
and applying it to the guard's concrete args leaves those nested deps stuck (`flatMap((o => match(o)), match(abort))`).
The refs are not in the guard expression, so the fixed-expression escalation loop cannot reach them. The `deep` fetch
(`ReducedBindingClosure.reduceInstance(_, _, deep = true)`, the relocated Step-7 `recursive`) resolves it by closing
the fetched binding's dependencies over their own reduced-at-instantiation forms. So `ReducedBindingClosure`'s `deep`
parameter **survives** (it is *the* escalation fetch mode, no longer the guard-gated `recursive`); §5's "delete
`ReducedBindingClosure`'s `recursive` parameter" is superseded — it is renamed and repurposed, not removed. The
regression the eager-global attempt caused stays avoided: `deep` is invoked only by the *stuck-driven* escalation fetch
at the expression's own ground args (the marker-guard reader keeps its one-hop `deep = false`), so no dependency is
demanded at a defaulted `Type`. Verified green: the `if..else..raise` inline forms, the bare `raise`, the piped
user-function guard (satisfied + rejected), the `MIN > 0` / `N < 10` compile-time-comparison guards, the ability-guard
marker suite, and the S7 regression set (Arithmetic 14 / Ranges 21 / Intervals / WherePrecondition 100).

Also verify: escalation must not re-fire on legitimately-structural compiler bodies (`foldEither` — a function over
neutral runtime parameters). The trigger must be "quote failed AND the failure is a stuck top-def/match/native head",
not "quote failed" alone (a `VLam`/param-neutral result goes straight to the structural fallback as today).

### 4.2 The twin's body check — entry shape and coverage

`walkTypeStack` today checks two levels (derived kind against `Type`, signature against the kind). The body-model
replaces this with per-binder kind checks + one body check against `VType` — same coverage, different factoring.
Verify no coverage is lost: (i) binder kinds are still checked well-formed; (ii) an arrow chain's parameter positions
are still checked (they are — `Function[A, B]`'s argument slots check against `Type` through the ordinary spine,
which is where the ladder's three acceptance arms fire for return positions); (iii) `levelExprs`' one remaining job —
ability-reference discovery in type positions — is subsumed by collecting refs from the checked body (the value
mono's `abilityRefs` collection already unions body + levels; for the twin the body *is* the signature).

Also verify the arrow chain evaluates/quotes as before: `Function[A, guard]` evaluates to `VPi`, quotes to the
`Function` `Structure` — same ground as today's walk produces (Step 5's equivalence result de-risks this).

### 4.3 Markers move to the compiler track

A runtime-layer guarded instance (the `Throw` self-lift, `where E1 != E2`) discharges its marker on the *runtime*
track today. Its twin monos on the compiler track. Two things to verify:

- **Pool reach**: the compiler pool scans the whole runtime track, so the marker's `SaturatedValue(marker@Signature,
  Platform.Compiler)` exists; and the guard's reduction leaves (`Eq[Type]`'s `equals` native) are platform-agnostic
  natives contributed on both platforms. A `where` guard that (illegitimately) depended on a runtime-only native would
  newly stall — **loudly** (the native-leaf boundary error), which is correct: a guard is a compile-time computation
  by definition. Add a regression test for the self-lift before flipping the consumer.
- **Verdict parity**: the marker twin's reduced return must reproduce today's verdicts (`true`/`false` in both `Bool`
  representations, `Right(bool)`, `Left(msg)`) — `interpretGuard` is untouched, so this is a producer-equivalence
  check on the ability-guard suite.

The strip stays: a marker twin's mono checks the *stripped* view (binders + guard return). This is marker-feature
normalization at one site, not a signature-special checker branch.

### 4.4 Twin-mandatory semantics (error propagation)

Today a twin failure silently falls back to the in-place walk, which re-discovers (and re-reports?) the same errors.
Under twin-mandatory, the value mono aborts on a missing twin — per the design rules, absence after an upstream
report is the correct decline. Verify: (i) the twin's errors carry the signature's source positions (they do — the
walk attributes to the signature today, and the twin checks the same expressions); (ii) no diagnostic is *lost* for
the LSP (whole-workspace diagnostics demand every value's mono; the twin's errors surface through the same
`CompilationResult`); (iii) no diagnostic is *duplicated* (the runtime twin and its signature twin at the same args
both being demanded must yield one report — the fact cache already dedups the twin's own production).

### 4.5 The stuck-guard-defer arm can die — verify the premise

`dischargeGuardedSignature`'s `isGuard && hasBody ⟹ fresh return meta` arm served the in-place model, where a value's
own mono could meet a guard *stuck on abstract binders*. Under twin-mandatory, a value mono only ever reads verdicts
produced at **its own ground arguments** — a guard at ground args always decides (its condition is a closed
compile-time computation). Sweep for counterexamples before deleting: an ability-`where` guard over a type an
instance leaves generic? (No — marker discharge happens at matched ground args.) A guard referencing an abstract
native? (Stalls loudly in the twin — correct.) If a legitimate stuck-at-ground case surfaces, the fail-safe is that
the twin's read-back hard-errors — never a silent acceptance — and the fix is a deliberate design decision, not a
quiet re-add of the defer arm.

### 4.6 Acyclicity, re-argued

The demand DAG changes in two ways: every value mono now *hard*-depends on its twin (previously soft), and marker/W3
twins newly produce. The argument stays three-part: (1) a value mono reads only its **own** twin at depth 1, and a
twin's mono checks no separate body and issues no further twin demand — `Checker(signatureOnly)` still gates the
callee flip off inside twin monos (the load-bearing structural gate: self-in-signature `Function`/`Type` would
otherwise demand themselves); (2) callee flips fire only in body checks, whose callee twin bottoms per (1); (3) the
reference DAG is finite and recursion-free; `activeFactKeys` remains the backstop for the W3 caller back-edge and for
escalation's fact demands. New in this plan: escalation adds `CMV(ref, args)` demands *during read-back* — these are
ordinary body-mono demands (depth-1 twin reads inside, per (1)) and are `activeFactKeys`-guarded, so the argument
composes. Write this into the code where the old argument lives (`TypeStackLoop.Result`'s successor).

### 4.7 Full-arity mono keys (delete `instantiateRemaining`)

The always-flip arm binds `binders.zip(typeArguments)` — it assumes every mono key carries an argument per binder.
Verify this invariant holds (callers append implicit metas for every peeled binder; unsolved phantoms default to
`Type` and quote into the key), then make it an assertion and delete `instantiateRemaining`. If a genuine
partial-arity key source exists, surface it in Phase C and decide explicitly — do not keep the arm "just in case".

### 4.8 Performance

Escalation is pay-on-stuck; twin-mandatory turns one `getFactIfProduced` + fallback walk into one `getFactOrAbort` —
strictly less work on the happy path. Marker/W3 twins add monos (small). Re-measure the Step-6 workload
(`EffectsMulti` + full examples + `ide.lsp` suite) at Phase C's gate; the budget is the same "within startup noise"
bar.

## 5. Deletion ledger

The measure of the plan. Everything here deletes; each item names its replacement.

**`TypeStackLoop`** (becomes two small, renamed units — the twin mono and the value mono):
- `walkTypeStack` + the `levels` construction + `levelExprs` plumbing ⟹ per-binder kind checks + one body check (§3.1).
- `establishSignature`'s in-place arm + `instantiateRemaining` ⟹ twin-mandatory read (§3.2, §4.7).
- `flattenReturnToType` + the `isCalc` pre-detection (`returnExprOf` / `isCalculatedReturnExpr`) ⟹ the ladder's W3
  acceptance arm + read-side `isCalculatedReturn` (§3.3, §3.5).
- `failOnAbstractCalculatedReturn` ⟹ the W4 error at the read-settle (§3.2).
- `quoteSignature` + `peelSignatureBinders` + the `guardMarker`/`inlineGuard` gates + `reduceGuardSubValues` +
  `reduceResolvedImpls` + `absorbLeadingArgs` + `reevaluateGuardReturn` + `collectValueRefs` (the in-walk composition)
  ⟹ ordinary `readBackBody` + escalation (§3.4).
- the guard-scoped quoter lookup maps (`guardSubBindings`, `reducedImplBindings`) ⟹ escalation's binding loop.

**`CheckState`**: `sawGuardReturn` + `recordGuardReturn` ⟹ nothing (the acceptances are stateless).

**`Track`**: `settleReturnPosition` + `settleGuardedReturn` (both impls) ⟹ the one read-settle helper. `Track` keeps
only the genuine platform strategy: `platform`, `pinCarriers`, `implBindings`, `readBackBody`.

**`CalculatedReturnResolver`**: `dischargeGuardedSignature` (the peel/rebuild + the stuck-guard-defer arm) ⟹ the
read-settle (which reuses `dischargeGuardedReturn` on the leaf); `isCalculatedReturnExpr` ⟹ the SemValue form only.

**Processors**: both mono processors' twin-read fallback arms and marker exclusions; ~~`ReducedBindingClosure`'s
`recursive` parameter~~ **(superseded — Phase A: the one-hop hypothesis failed on the piped user-function guard, so
this survives, renamed `deep`, as the escalation fetch mode; see §4.1 PHASE A OUTCOME)**;
`AbilityImplementationProcessor.readGuardVerdict`'s platform switch (⟹ one twin read) and the Runtime-role marker monos
it demanded.

**Tests**: `SignatureTwinMonoTest`'s equivalence framing (there is no second computation to be equivalent *to*) ⟹
direct assertions on twin facts.

**Front-end (Phase F, severable)**: the `NamedValue.runtime`/`signature` dual slot, the runtime twin's placeholder
`.signature`, `signatureEquality` as a bespoke comparator, and every phase's own-signature arm ⟹ one body per fact;
binder names forwarded on the runtime twin's fact (resolve scope, `recordAmbientCarriers`); signature-consuming reads
(callee fallback evaluation, LSP hover/`TypeHintIndex`, apidoc headers, ability conformance) ⟹ the sig twin's fact;
the sig-twin merge rule (all-agree) *is* the relocated `signatureEquality`.

## 6. What explicitly survives — the sanctioned residue

So nobody "cleans up" past the design. Each is the minimal expression of a real feature, not a second path:

- **`SignatureView`** and the derived-kind discipline (the kind is never minted or stored).
- **The three return-position acceptance arms** (§3.3) — stateless, in the one shared ladder.
- **`dischargeGuardedReturn` + `GuardChannel`** — the one verdict-discharge protocol, at every read (own-twin read,
  callee reads, `interpretGuard`).
- **`installReturnMeta` / `failOnUndeterminedCalculatedReturn` / `readMonomorphicReturn` + `activeFactKeys`** — the W3
  body-solve and the caller back-edge (body-computes-signature is an inversion by design).
- **The `signatureOnly` callee-flip gate** — structural acyclicity (§4.6): a twin's own walk must resolve callees
  symbolically, never demand a twin fact.
- **The full-application + ground-args gate on the callee flip, and the symbolic `evalExpr` fallback** — a reference
  whose arguments are still inference metas *cannot* be served by a ground fact; evaluating the callee's signature
  expression and peeling metas is the one NbE evaluator doing bidirectional inference. This is the inference boundary,
  permanent by design. (An under-applied or non-ground *guard* callee correctly stays stuck there and is settled at a
  later read — Use-Site Verification.)
- **`MarkerGuardSignature`** (recognition + strip) and `AbilityImplementationProcessor`'s verdict interpretation — the
  ability-guards feature, with its own reject-vs-decline semantics.
- **`Track`'s platform hooks** (`pinCarriers`, `implBindings`, `readBackBody`) — genuinely two platforms.
- **`CompilerNativesProcessor`'s precompute-and-merge** — genuine compile-time natives (no guard reliance since the
  old Step 10).
- **`EffectLifter`, `CarrierKindChecker`, `AbilityResolver`, `PostDrainQuoter`'s staging gate** — shared machinery,
  untouched.
- **S8's results**: sig twins exempt from the two user-facing effect diagnostics (the derived-row exemption); the
  recursion check's structural no-op on twins; alias cycles caught on the runtime twin.

## 7. Execution phases

Each phase is one committable unit: compiles, full suite + all examples + `ide.lsp` green at its gate. Ordering is
load-bearing: the producer must be whole before any consumer loses its fallback, and the in-place walk is deleted
*last*, only when nothing references it.

- **Phase A — escalation replaces the guard-scoped reduction. [LANDED.]** Built the stuck-driven escalation loop in
  the compile-time read-back (§3.4, in `PostDrainQuoter.reduceWithEscalation`, shared by `reduceSourced` and
  `reduceSemExprToGround`) and routed the *existing* twin guard path through it: `quoteSignature`'s deep-reduce now
  consults the escalated evaluation instead of the pre-composed `guardSubBindings`/`reducedImplBindings` maps. Deleted
  `reduceGuardSubValues`' inline-guard call, `reduceResolvedImpls`, and `absorbLeadingArgs` from `TypeStackLoop` (the
  absorb-in-ignore-lambdas moved into the escalation as `PostDrainQuoter.absorbTypeArgs`). **§4.1's one-hop hypothesis
  was disproven on the piped user-function guard** (see §4.1 PHASE A OUTCOME): `ReducedBindingClosure.recursive` was
  therefore **not** deleted — it is renamed `deep` and repurposed as the (stuck-driven, non-guard-gated) escalation
  fetch mode; the marker Stage-4 path (`reduceGuardSubValues` raw + `reevaluateGuardReturn`) keeps its one-hop
  `deep = false` and is otherwise untouched until Phase D.
  *Gate — met:* full `lang.test` (233) + `jvm.test` (283) + `ide.lsp.test` green; guard suite green
  (`GuardSignatureIntegrationTest` 8/8, `AbilityGuardDischargeTest`, `CompilerAbilityResolutionTest`,
  `SignatureTwinMonoTest`, `CompilerAbortCarrierTest`); all examples build and run; S7 regression set
  (Arithmetic/Intervals/Ranges/WherePrecondition) explicitly re-verified.

- **Phase B — the twin's mono becomes an ordinary body mono.** Rebuild the `signatureOnly` mode per §3.1: binder kind
  checks + arrow-chain body check via the shared ladder (add the stateless W3-hole acceptance arm; the guard arm
  already exists) + ordinary `readBackBody`. Twins now produce for **every** value — W3 (under-applied ground, §3.5)
  and markers (stripped view) included; the twin's W3 decline deletes. Consumers are *not* yet flipped (the value
  mono's fallback still runs where it ran). `CACHE_VERSION` bump (twin fact content changes for W3/marker keys).
  *Gate:* twin facts assert directly (`SignatureTwinMonoTest` successor): ordinary / generic / ability-constrained /
  guarded / W3 / marker fixtures produce the expected ground; full suite green.

- **Phase C — consumers read unconditionally; the walk deletes.** The value mono goes twin-mandatory with the uniform
  read-settle (§3.2); `Track.settleReturnPosition`/`settleGuardedReturn`, `dischargeGuardedSignature`,
  `sawGuardReturn`/`recordGuardReturn`, `flattenReturnToType`, `failOnAbstractCalculatedReturn` (relocated),
  `establishSignature`'s in-place arm, `instantiateRemaining` (§4.7), `walkTypeStack` + `levelExprs`, and
  `quoteSignature`/`peelSignatureBinders` delete. The callee flip's marker exclusion deletes. Verify §4.4's error
  semantics and §4.5's premise; re-measure §4.8; write §4.6's acyclicity note into the code.
  *Gate:* full suite + examples + `ide.lsp` green; `monomorphize/check` net line count decisively negative — **this is
  the goal-2 gate the old plan failed; if it does not hold here, stop**.

- **Phase D — markers via the twin read.** `AbilityImplementationProcessor.readGuardVerdict` becomes the one
  compiler-track twin read; the Stage-4 workaround (`reevaluateGuardReturn`, the raw `reduceGuardSubValues`, and
  `collectValueRefs` if unreferenced) and the Runtime-role marker monos delete. §4.3's self-lift regression test lands
  *before* the flip. (D depends on B's marker twins; it can land before or after C — whichever keeps the walk's last
  references contained. If C precedes D, the walk's deletion moves here.)
  *Gate:* ability-guards suite (self-lift, IntArith width dispatch, guarded Coerce) + full suite green.

- **Phase E — front-end single-bodying (severable).** One body per front-end fact (§5's last block): the runtime twin
  drops its placeholder `.signature` (binder names forwarded), the sig twin *is* the signature, `signatureEquality`
  collapses into the sig-twin all-agree merge, and each phase's own-signature arm deletes. Consumers sweep: resolver
  binder scope, matchdesugar/operator own-signature paths, effect-phase reads (`dischargedEffects` relocates to the
  sig twin), saturation, the callee-fallback signature read, LSP hover / `TypeHintIndex` / apidoc, ability
  conformance. `CACHE_VERSION` bump. This phase is wide but mechanical, independently valuable, and droppable without
  weakening A–D.
  *Gate:* zero behaviour change; full suite + examples + `ide.lsp` green; `HelloWorld` builds and runs.

## 8. Guardrails (stop rules)

- **The §5 ledger must realize.** A phase that can only stay green by adding a signature-specific gate to
  `Checker`/`Track`/the read-back is going the wrong way — stop and re-derive against §3.
- **No guard-scoped anything in read-back.** Escalation is shape-agnostic and stuck-driven; the moment a fix wants a
  `sawGuard`-like flag or a per-feature binding map, it is the old design growing back.
- **One evaluator, one pipeline.** The twin's mono is the ordinary pipeline; no kernel reduction re-composition, no
  deep-`renormalize` read-back patching, no carrier synthesis in a walk (§9's first bullet lists what was tried).
- **The kind is never minted or stored.** Binary role, per-binder discipline, no level towers.
- **Gaps must be fail-safe.** A signature that does not reduce hard-errors ("Cannot resolve type."); a rejection is
  always reported (`GuardChannel.fallbackRejectionMessage`); twin absence aborts; never a silent `Type`, never silent
  acceptance.
- **Do not delete the survivors** (§6) — in particular the `signatureOnly` acyclicity gate and the symbolic
  non-ground callee fallback, which superficially look like leftovers of the old design and are not.

## 9. Evidence record (absorbed from the retired signature-split plan)

- **The deep-reduction wall** (Attempt 1, `wip/return-position-unification-stage2`): reducing the guard tower in place
  bottoms at `VNeutral(Reserved(Match))`; `renormalize` cannot re-fire a stuck match; match reduction lives only in
  the full pipeline. Forbidden moves all tried there: sub-value binding composition, deep-`renormalize` read-back,
  carrier synthesis in the walk. This is *the* reason the signature must be an ordinary body — and why §3.1 is the
  heart of this plan.
- **The gated-flip outcome** (Steps 5–7, landed): the twin read works and is behaviourally equivalent on the common
  case (Step 5's equivalence test), the callee flip needs the `signatureOnly` acyclicity gate (`Cyclic fact demand:
  Function@Signature ← Function@Signature` without it), overhead is within JVM-startup noise (~3% wall on
  `EffectsMulti`), and W3-as-"twin declines" sidesteps a twin↔runtime self-cycle. All carried forward except the
  decline (replaced by the §3.5 hole, which creates no cycle — the twin never reads the value mono).
- **The corrected 6-test account** (§2): the flip fires for guarded values; `Track.Runtime.settleGuardedReturn` is the
  read-boundary discharge, not an in-place-walk dependency. The old §6's "partially applied" explanation was wrong —
  recorded here because it halves the perceived size of the consumer-side work.
- **The S7 crux mechanisms**: the inferred-carrier pin (`pinInferredReturnCarriers` → `pinMetaToEither`; the `{Abort}`
  layer solves structurally, only base metas pin), the effect-carrier-meta arm of `isGuardCarrier` (the kind check
  meets `?G[?A]` before the pin), and the stacked-carrier wall (why raw one-hop closures don't reduce nested base
  abilities — §3.4/§4.1 own this now). The checked expression, not the raw ORE, is what reduces (it carries the
  effect lift's `pure`).
- **The global-recursive-reduction regression** (S7): eagerly reducing every dependency per-instantiation broke 6
  examples via demands at defaulted instantiations — the boundary condition escalation's blocking-refs-only,
  expression-carried-args discipline is designed around.
- **Step 10's literal fix** (`1a4a328d`): signature literals stay `BigInteger` (`signatureContext` in
  `CoreExpressionConverter`) — a prerequisite the inline guard forms depend on; do not regress it in Phase E's
  front-end sweep.
- **Reject-site convention** (green tests): a guard's `Left(msg)` reports at the guard *definition* for a direct
  monomorphize and at the *use reference* for a caller. The read-settle preserves this (the own-twin read attributes
  to the value's name; the callee read to the reference).
- **Step A's fate** (retired TypeLevel plan): scaffolding born without consumers gets deleted wholesale — hence the
  producer-first-but-consumed-in-the-same-arc phase ordering (B produces, C/D consume, nothing lands unread).
