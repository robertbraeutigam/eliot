# Discharge-Aware Effect Accounting

Status: **Steps 0–4 SHIPPED; Step 5 INVESTIGATED — not clean, limitation documented + conservative behaviour
locked (all 2026-07-08); Steps 6–7 remain plan.** Captures why an effectful
function whose internal `if..else` (or any handler) *fully discharges* an effect is nonetheless forced to
declare that effect, and a staged plan to fix it. The fix keeps the one-carrier model and monomorphization as
the sound backstop; it only makes the cheap, definition-local effect check *precise* instead of
over-approximating.

**Shipped slice (Steps 0–2):** the negative effect-set syntax `{…, -E}`, the `dischargedEffects` field threaded
`FunctionDefinition → NamedValue → ResolvedValue → BlockDesugaredValue → MatchDesugaredValue →
OperatorResolvedValue` (resolved to `AbilityFQN` in the resolve phase, part of `NamedValue.signatureEquality`),
the shallow direct-call subtraction in `EffectUsageCollector`, and the five body-discharger annotations. The
symptom is fixed end-to-end: `IfDemo.demo` and `examples/DischargeDemo.els` now declare only `{Console}`; the
`M5run`-style real leak stays rejected (`EffectDischargeAccountingTest` + `LeakDemo` smoke). One deliberate
deviation from the plan is recorded under Step 1 below (the abstract-accessor trio).

## 0. The symptom

```eliot
def demo(flag: Bool): {Console} Unit = {
   printLine(if(flag, "A") else "B")   // every `if` has a matching `else`
}
def main: IO[Unit] = demo(true)
```

→ `error: This value performs the effect 'Abort' but does not declare it` (reported at `demo`).

Yet the `if..else` is fully discharged. Verified cold-cache ground truth:

| case | signature | body / use | result |
|------|-----------|-----------|--------|
| P1 | `def demo(f): String = if(f,"A") else "B"` | pure return | ✅ passes |
| M2 | `def demo(f): {Console} Unit = printLine(if(f,"A") else "B")`, `demo(true)` | used | ❌ "performs Abort" |
| Smoking | `def demo(f): IO[Unit] = { printLine(if(f,"A") else "B") }`, used | concrete carrier | ✅ compiles + runs |
| M5run | `demo` really calls `abort`, `{Console,Abort}`, used in `IO` w/o `else` | real leak | ❌ `No ability implementation found for 'Abort' with type arguments [IO]` |

The first three show the `if..else` *does* discharge — `demo` genuinely performs only `Console`. M5run shows a
*real* undischarged abort is caught at monomorphization. So the effect system is **sound**; the `{Abort}` that
`demo` is forced to declare is a **false positive**, and because callers propagate a callee's declared effects,
the phantom `Abort` spreads to every abstract caller of `demo`.

## 1. Root cause

Two facts in the definition-local `effect` phase combine into the false positive:

- **`EffectUsageCollector.collectApplication`** computes a body's `used` effects by *unioning the effects of
  every argument* and reading each callee's own effects. For `printLine(else(if(flag,"A"), "B"))` it recurses
  into the `else` argument and counts `if`'s `{Abort}` (because `if`'s *result* is effectful). It has **no
  notion that `else` discharges its `AbortCarrier` argument.** So `used = {Console, Abort}`.
- **`DeclaredEffectChecker.verify` (line 28)** skips the `used ⊆ declared` check entirely when the carrier set
  is empty — i.e. for a *pure* return (`P1`) and for a *concrete* carrier (`main : IO[Unit]`). It only runs for
  an abstract `{…}` carrier.

So the same over-counted `{Abort}` is silently ignored for `P1`/`main`, but rejected for the one honest,
platform-independent `{Console}` form. The over-approximation is deliberately *conservative* (over-counting only
makes `used ⊆ declared` harder, so it never lets a real effect through) — the cost is false positives.

Monomorphization is the real, sound gate: it instantiates `demo`'s carrier to `IO` (the internal `AbortCarrier`
lives and dies inside the sub-expression), and only a *genuinely* undischarged abort reaching `IO` fails
(M5run). **The single-carrier model is not the obstacle** — the Smoking case proves a local, discharged
`AbortCarrier` inside an `IO` body already works. Only the syntactic walk is wrong.

## 2. Design decisions

1. **Never guess — always gated.** Subtraction happens only where a discharge is *asserted*: a declared
   negative effect on a signature, or a summary *inferred from a body*. We never subtract from a raw
   `XxxCarrier[G]→G` shape match (that heuristic is flaky on arbitrary user code and can turn a false positive
   into an unsound false negative).
2. **One representation, two producers.** Every value carries a `dischargedEffects` summary. For a **primitive**
   (native/abstract body, nothing to analyse) it is *declared*; for a **user function** it is *inferred from
   the body*. The collector reads it uniformly, so a user's `orDefault` and the built-in `else` are
   indistinguishable to callers.
3. **Two collector depths.** *Shallow* (subtract at a direct discharger call) needs no new machinery and already
   fixes `demo`. *Deep* (parameter/let effects tracked by origin — "provenance") is what lets discharge compose
   through user-defined handlers and let-bindings.
4. **Monomorphize stays the sound backstop.** The definition-local check only ever becomes *more permissive*; a
   real leak (M5run) must stay rejected. Every step re-verifies this. This matches the use-site-verification
   cornerstone: the def-local check is a friendly early lint, not the soundness gate.
5. **Threading template = `opaque`.** A new fact field rides the exact chain `opaque` already travels:
   `FunctionDefinition`(ast) → `NamedValue`(core) → `ResolvedValue`(resolve) → `MatchDesugaredValue` →
   `BlockDesugaredValue` → `OperatorResolvedValue`, read by `CalleeSignatures.infoFor`.

**Discharger primitives** (stdlib, explicit-carrier `XxxCarrier[…,G,A] → G[…]`): `else`, `runAbort` (→`Abort`);
`catch`, `runThrow` (→`Throw`); `runStateToPair`, `runStateToValue`, `runStateToFinalState`, `runStateCarrier`
(→`State`). **All eight are annotated** as of Step 4 — the five with hand-written bodies directly, and the three
raw `data`-field accessors (`runAbort`, `runThrow`, `runStateCarrier`) via the layer-merge union of discharge
annotations (Step 4a), which lets the abstract declaration's `{-E}` ride onto the generated concrete accessor.

## 3. Syntax: negative members in the effect set

Discharge is declared **where effects already live — the `{…}` set** — as a *negative* member:

```eliot
def logOrDefault(x: {Abort} String, key: String): {Console, -Abort} Unit   -- adds Console, discharges Abort
def else[G[_] ~ Effect, A](computation: AbortCarrier[G, A], fallback: G[A]): {-Abort} G[A]
def catch[E, G[_] ~ Effect, A](computation: ThrowCarrier[E, G, A], onError: E => A): {-Throw[E]} G[A]
```

**Desugar rule — partition the set:**

- **Positive members** behave exactly as today: they constrain the one shared carrier `F` and the `{…} A`
  position rewrites to `F[A]` (`EffectSugarDesugarer`).
- **Negative members** are recorded in the value's `dischargedEffects` and contribute **no** carrier
  constraint.
- **A negatives-only set introduces no carrier `F` at all** — there is nothing positive to carry — so it
  passes its inner type through unchanged and only records the discharge. This is what lets the
  explicit-carrier primitives annotate their real return: `{-Abort} G[A]` desugars to `G[A]` (unchanged) plus
  `dischargedEffects = {Abort}`. `else`'s body is untouched.

**Ability granularity.** `-Throw[E]` records discharge of the ability `Throw`; the type argument is carried for
readability and future per-argument precision but the definition-local check is ability-level (matching the
existing `DeclaredEffectChecker` design, which leaves type-argument precision — `Throw[String]` vs `Throw[E2]`
— to monomorphization).

**Declared discharge must be verified.** A `-E` the body does not actually perform would be unsound (callers
would skip a real effect). The declaration is checked against the inferred summary (Step 3) where a body
exists, and monomorphization remains the backstop (a falsely-declared `-E` that leaves a real effect is caught
when it reaches a concrete carrier). Abstract primitives (`runAbort`) have no body; their discharge is
axiomatic (they are the accessor that reifies the effect away).

## 4. Steps

### Step 0 — Regression harness (first)

- **Goal:** lock current + target behaviour deterministically, avoiding the on-disk `target/.eliot-cache` that
  makes ad-hoc CLI probing order-dependent.
- **Where:** processor-level tests `extends ProcessorTest(LangProcessors()*)` (in-memory, no cache) in
  `lang/test/.../effect/`. Add 2–3 end-to-end `examples/` compiles with unique module names as smoke tests.
- **Cases:** `demo:{Console}` (must pass after Step 2); `M5run` real-leak (must *stay* rejected at
  monomorphize); `P1` pure (stays green); `{State,Abort}` mixed (Step 4).
- **Done when:** the harness reproduces today's failures and encodes the target outcomes.
- **Shipped:** `EffectDischargeAccountingTest` (processor-level, in-memory, no cache) locks subtraction-passes,
  undischarged-still-rejected, partial-discharge-keeps-sibling, and pure-stays-green; `examples/DischargeDemo.els`
  and the de-workaround'd `examples/IfDemo.els` are the end-to-end smoke compiles.

### Step 1 — Negative effect-set syntax + marker plumbing

- **Parser:** `effectfulTypeParser` entries accept an optional leading `-`. `Expression.EffectfulType` carries
  signed members (e.g. split `positive: Seq[AbilityConstraint]` / `negative: Seq[AbilityConstraint]`).
- **Desugar:** `EffectSugarDesugarer` partitions (Section 3): positives → carrier constraints + wrap;
  negatives → a new `dischargedEffects` on `FunctionDefinition`; negatives-only set → no carrier, pass-through.
- **AST field:** add `dischargedEffects: Seq[Sourced[QualifiedName]] = Seq.empty` to `FunctionDefinition`
  (mirrors `opaque`/`doc`: optional-with-default, so data-desugared/synthetic functions are unaffected).
  Decide `signatureEquality` inclusion — **include it**, so an abstract stdlib signature and its jvm impl can't
  silently disagree about discharge (layer-merge safety).
- **Thread** the field through the 5 downstream facts + the `.copy` in each producing processor
  (`CoreProcessor`, `ValueResolver`, matchdesugar, block, `OperatorResolverProcessor`); resolve the ability
  name to `AbilityFQN` where ability constraints resolve.
- **Mark primitives:** annotate the stdlib dischargers (Section 2). **As initially shipped (Steps 0–2)** only the
  **five body-dischargers** (`else`, `catch`, `runStateToPair`, `runStateToValue`, `runStateToFinalState`) were
  annotated — each lives in one layer only, so its annotation merges cleanly. The **three raw accessors**
  (`runAbort`, `runThrow`, `runStateCarrier`) were left un-annotated because their jvm implementation is the
  *generated* `data`-field accessor of `AbortCarrier`/`ThrowCarrier`/`StateCarrier`, which cannot carry a negative
  member — a `{-E}` on the abstract stdlib side would either be dropped at merge or, with `dischargedEffects` in
  `signatureEquality`, **fail the merge**. This conservative gap was **closed in Step 4a** by the layer-merge union
  (below): all three are now annotated and their discharge rides onto the concrete accessor.
- **Verify:** `OperatorResolvedValue` for `else` carries `dischargedEffects = {Abort}`; no behaviour change yet.

### Step 2 — Shallow subtraction: direct invocation (fixes the symptom)

- **`CalleeSignatures.CalleeInfo`** gains `dischargedEffects: Set[AbilityFQN]` (read from Step 1).
- **`EffectUsageCollector.collectApplication`**, `ValueReference` branch:
  `usedEffects = ownEffects ∪ (argUsage.usedEffects \ info.dischargedEffects)`. Subtracting from the whole
  arg-union is sound: the non-computation args (`fallback`, `initial`, `onError`) ride the inner carrier `G`
  and cannot carry the discharged ability.
- **Verify (milestone, independently shippable) — DONE:** `demo:{Console}` compiles (`IfDemo`,
  `DischargeDemo`); `P1`/pure still passes; a genuinely undischarged abort under `{Console}` **still rejected**
  by the def-local subset check (`LeakDemo`: "performs the effect 'Abort' but does not declare it"), and a real
  leak reaching a concrete carrier **still rejected at monomorphize** — the subtraction fires only at an actual
  discharger call, never at a bare `if`.

### Step 3 — Extend to user-defined dischargers (inference)

- **Goal:** `def orDefault(x:{Abort} A, d:A) = x else d` is inferred to discharge `Abort`, with no annotation,
  so its callers subtract automatically.
- **Provenance in the collector (the shared enabler for Steps 3 & 5):** today a `ParameterReference`
  contributes only an `effectful` boolean, not *which* effects. Enrich `Usage` so a parameter reference
  contributes its **declared effects tagged by origin**; a discharger removes the tagged occurrence. An effect
  *survives* to the result iff some undischarged occurrence remains — this is what makes double-use (Step 4)
  sound.
- **Summary fact:** `EffectDischargeSummary(vfqn, platform)`, produced in/beside the effect phase:
  `declared` if the value has negative members (Step 1), else `inferred` = the parameters' effects that did not
  survive the body. `CalleeSignatures.infoFor` reads this fact (unifying declared + inferred). **Termination:**
  Eliot has no recursion, so the runtime-body call graph is a DAG — "summarise `f` from its callees'
  summaries" bottoms out (a payoff of the totality cornerstone).
- **Ordering:** this adds a cross-value read *within* the effect layer (a caller needs its callee's summary);
  model it as a proper fact dependency, single-owner supplier (no processor cycle).
- **Verify:** `printLine(readSetting("k") orDefault "def")` in a `{Console}` fn compiles; a non-discharging
  passthrough `def run(x:{Abort} A) = x` still forces `{Abort}` (its Abort survives).

**SHIPPED (2026-07-08).** As built:
- **Provenance is effect-level, not per-occurrence-tagged.** `Usage.survivingParamEffects: Set[AbilityFQN]` — the
  parameter-originated effects still reaching a subtree's result undischarged. A `ParameterReference` seeds its carrier
  binder's declared effects (`EffectCarriers.carrierHead` finds the binder); a discharger call subtracts its
  `dischargedEffects` from the arg-union's survivors; every other node unions its children (over-approximating survival,
  so discharge is never *over*-inferred — the only unsound direction). A set is sufficient because "an effect survives
  iff some undischarged occurrence remains" = "the effect is in the union of the subtrees' survivor sets" — which
  already makes the **double-use** case (`pair(x else "d", x)` → Abort survives via the second, undischarged `x`)
  correct, so Step 4's double-use falls out here. Per-*origin* tagging is only needed to make **let-binders** behave
  like parameters (Step 5); deferred until then.
- **`EffectDischargeSummary(vfqn, platform)`** fact + `EffectDischargeSummaryProcessor` (off `RecursionCheckedValue`):
  `dischargedEffects = declared (`{…, -E}`) ∪ inferred (`paramEffects − survivingParamEffects`)`. Single-owner
  recursion — it reads back callee summaries via `CalleeSignatures.infoFor` — with **no `activeFactKeys` guard needed**:
  a summary is only computed for a recursion-checked value, and those form a DAG (a recursive callee produces no
  `RecursionCheckedValue`, hence no summary, read conservatively as no discharge). Kept **separate from the effect
  check** so a caller can read a handler's discharge even when the handler's *own* body fails a check.
- **Only channel-1 (arg-union) subtraction happens; the callee's own `effectAbilities` are never reduced by its
  discharge.** Sound: if a handler's result genuinely carries the effect (its return type still declares `{E}`), that
  fresh occurrence must reach the caller — and it does, via `effectAbilities`. A handler that *fully* discharges must
  therefore have a **non-`{E}` (pure/residual) return** — which trips the "declared pure but performs effects"
  fail-safe. So a fully-discharging user handler's *summary* is inferred and its callers subtract (verified at the
  processor level: `EffectDischargeAccountingTest` "let a caller subtract a user handler's inferred discharge"), but its
  own **end-to-end compile awaits Step 6** (fail-safe reconciliation). The passthrough — return `{Abort} A` unchanged,
  carrier-headed, no fail-safe — compiles today and correctly infers *no* discharge (`PassthruLeak` smoke: a `{Console}`
  caller is still rejected for the undischarged `Abort`).

### Step 4 — Double / mixed effects correctness

- **Keep-one, drop-one:** a `{State, Abort}` computation discharged with `runAbort` only → stays `{State}`
  (real pattern: `runStateToPair(runAbort(p), s0)` in `State.els`).
- **Independent siblings:** one statement discharges its Abort, a sibling still aborts → block stays `{Abort}`
  (subtree scoping from Step 2 already gives this; add the test).
- **Double-use of a parameter:** `pair(x else "d", x)` with `x:{Abort} A` → Abort **survives** (one occurrence
  undischarged). *Already correct from Step 3's set-union survival* (the undischarged `x` keeps Abort in the union);
  Step 4 just adds the explicit test.
- **`Inf` is never discharged:** confirm no discharger lists `Inf` and that `Inf` still propagates (the subset
  check is also the `Inf` carrier — a regression here breaks the totality opt-out).

**SHIPPED (2026-07-08).** Mostly verification — the four properties were already correct from the set-subtraction
mechanism of Steps 2–3 — plus one real change (Step 4a) so the *keep-one/drop-one* example works with the raw
accessors it names:
- **Step 4a — trio resolved via a layer-merge union of discharge annotations.** `dischargedEffects` is dropped from
  `NamedValue.signatureEquality` (a generated `data`-field accessor legitimately can't spell it) and instead unioned
  across all co-located declarations in `UnifiedModuleValueProcessor` — the annotated abstract declaration's `{-E}`
  rides onto the chosen concrete implementation; two layers with *different non-empty* discharge sets is a hard
  "Layers disagree on the discharged effects" error. `runAbort`/`runThrow`/`runStateCarrier` are now annotated
  (`runStateCarrier`'s function return needs parens: `{-State[S]} (S => G[Pair[A, S]])`). Verified end-to-end
  (`TrioDischarge`: `def tryIt[G[_] ~ Effect]: G[Option[String]] = runAbort(aborting)` compiles + runs — before Step 4a
  it was rejected for the undischarged `Abort`). So `runStateToPair(runAbort(p), s0)` now discharges *both* orders
  precisely.
- **Keep-one/drop-one, independent siblings, double-use** all follow from set subtraction being at ability granularity
  and per-subtree; **`Inf` is never discharged** because no discharger annotates it and inference only ever *removes*
  what a discharger discharges (a `{Inf}` parameter's `Inf` survives any non-`Inf` discharge). Locked in
  `EffectDischargeAccountingTest`'s "double / mixed effects (Step 4)" cases.

### Step 5 — Let-binds (investigate → implement if clean)

- **Problem:** the collector attaches a `let`'s bound-value effects at the *let node* (immediately-applied
  lambda), detached from the binder's occurrences, so `{ val y = aborting(); y else "d" }` cannot subtract.
- **Approach:** the Step-3 provenance already tags a *parameter's* effects; a `let` binder is the same shape
  (`(y -> body)(arg)`). Propagate the bound value's effects to the binder's occurrences as a tagged origin;
  reuse the existing `stored` carrier-typed-binder rule.
- **Risk:** interaction with the checker's own bind/`pure` elaboration and deliberate `stored` storage; direct
  style (`aborting() else "d"`, and what `if..else` desugars to) already avoids this. If the fix is not clean,
  **document the limitation** and stop — direct style covers the real cases.
- **Verify:** let-bound discharge compiles, or a written-down limitation + a test asserting the conservative
  (still-sound) behaviour.

**INVESTIGATED (2026-07-08) — not clean; limitation documented, conservative behaviour locked.** The fix
would need far more than the def-local accounting tweak the plan sketched, because the blocker is **not** in
the accounting — it is in the **checker's own let-bind elaboration**, one phase later. Ground truth
(`examples/`, cold cache): `{ val x = mightAbort(f); printLine(x else "default") }` fails, and *keeps* failing
even when `demo` is hand-annotated `{Console, Abort}` to bypass the def-local check entirely — with a fresh,
worse error at monomorphization:

```
error: Type mismatch.  Expected: AbortCarrier(IO, String)  Actual: String   (at `x else "default"`)
```

**Why.** A `val x = e` lowers to the immediately-applied lambda `(x -> body)(e)`. The checker's
`typeImmediateLambda` (monomorphize/check/Checker.scala) sees the carrier-headed argument, **sequences it with
`Effect.flatMap`**, and binds `x` to the action's *payload* type `T'` (`String`), **not** the carrier
`AbortCarrier[IO, String]` (EffectLifter's bind-lift, docs/effect-lift-in-checker.md). So inside the body `x :
String` — the carrier has already been unwrapped by the bind, and a later discharger (`else`, which needs an
`AbortCarrier`) can no longer act on it. The effect is therefore *bound* (propagated), never *discharged*;
`demo` genuinely still performs `Abort`. Direct style (`mightAbort(f) else "default"`, and what `if..else`
lowers to) avoids this because the carrier value is a *direct* argument to the discharger — no intervening
bind — and it compiles + runs (verified).

Making the let form work would require the checker to make a **use-directed** bind-vs-store decision (look
ahead: does a later occurrence of `x` feed a discharger? if so, *store* the carrier instead of binding it) —
or the user to annotate the binder with the carrier type (the existing `stored` path), which is impractical:
`AbortCarrier` is documented plumbing that application code never names, and spelling its `G` would commit to a
platform. Either is a substantial change well outside discharge accounting, with its own soundness subtleties
(unused binder, value-param aliasing at one ability). Per the plan's escape clause, we **stop here**.

Crucially, the current behaviour is already **fail-safe**, not a silent gap: the def-local accounting
*conservatively rejects* the let form with the clear def-site message *"performs the effect 'Abort' but does
not declare it"* — it never silently drops the effect. Making the accounting pass would only relocate that
error to the confusing use-site type-mismatch above (strictly worse diagnostics), so leaving it conservative is
also the better UX. Locked by two tests in `EffectDischargeAccountingTest` under **"let-binds (Step 5)"**: the
subset check still rejects a let-bound-then-discharged effect (contrast the direct form, which passes), and a
handler that discharges only through a `let` infers *no* discharge (conservative). If let-bound discharge is
ever wanted, it is a checker feature (use-directed store), tracked here — not an accounting one.

### Step 6 — Fail-safe & interaction audit

- Reconcile the **"declared pure but performs effects"** fail-safe (`EffectCheckProcessor`, the
  `usage.effectful` flag) with subtraction: a fully-discharged body must not be flagged; a genuinely effectful
  one still must.
- Confirm apidoc/LSP hover effect rendering (if any) reflects the now-honest sets.
- Re-run the whole `examples/` suite + `__.test`.

### Step 7 — Docs & skill updates

- Fold this doc's outcome into `CLAUDE.md`'s `effect` phase description; update the **`eliot-code`** skill's
  effect chapter (discharge no longer forces the ambient row; `{…, -E}` syntax); note the negative member in
  whichever reference covers stdlib effect authoring.

## 5. Sequencing & shippability

```
Step 0 ─► Step 1 ─► Step 2  ✦ shippable: fixes the symptom, stays sound
                        │
                        ▼
                 Step 3 (builds provenance) ─► Step 4 ─► Step 5 (optional)
                        │                          │
                        └────────► Step 6 ◄────────┘ ─► Step 7
```

- **Steps 0–2 are a complete, shippable slice**: honest `{Console}`, `{…, -E}` syntax in place, soundness
  intact. Everything after is the "no annotation ever, even for user-defined handlers" generalisation.
- **Steps 3–5 share one enabler** (collector provenance); build it once in Step 3.
- **Hard gate at every step:** M5run (real leak) must stay rejected — the line between "more precise" and
  "unsound."

## 6. Alternatives considered (and why not)

- **Shape detection** (subtract wherever a param is `XxxCarrier[G]` and the return is `G`): rejected — flaky on
  arbitrary user code; a wrong subtraction is an unsound false negative.
- **Drop the def-local check, rely only on monomorphization:** loses def-site diagnostics, dead-code checking,
  and the modular effect interface abstract callers propagate from.
- **Full row-polymorphic effect types with a subtraction operator** (`{Abort|ρ}A → {ρ}A`): the principled
  endpoint, and `{…, -E}` is a lightweight step toward it, but it requires row unification in `unify` (which the
  cornerstone keeps as pure definitional equality). Deferred; the accounting fix delivers the same result for
  the cases that occur without that cost.
