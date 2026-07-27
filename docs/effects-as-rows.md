# Effects as Rows, v3: Declared Suspension + a Written Carrier

**Status (2026-07-27).** The design is **implemented** and the v2 machinery is being deleted against it.
`A.11.1`–`A.11.6` are done: the elaborator **writes the carrier**, `Bool.fold` **declares its
suspension**, **every plain slot is strict** (§1 rule 1, finally implemented), and the per-definition row
check is **unbounded** with the post-mono `DeclaredPureChecker` deleted as subsumed. Gate: full
`__.test` green, 37/40 examples compiling (`PluginA`/`B`/`C` predate this work), every program
byte-identical in output and class content.

**Next: A.11.7** (delete the v2 bridge), then A.11.8–A.11.10. The live plan is **Appendix A.11**; it
replaces every earlier plan (§8 and the plans inside A.9/A.10 are historical). **A.9.4 owns the method**
— arm-liveness tracing, the differential probe, the byte-identity oracle, the tracer gotchas — and is
reused by every remaining step.

**One open decision**: `foldOption`'s suspension (A.11.5-R) — a reversal of the settled signature list,
not a refinement, so it needs sign-off rather than a judgement call in flight.

**Standing rule for this document: §§1–7 state the decision; §8 and the appendices record what happened
to it.** An appendix that changes a decision must say so in §§1–7, never amend the rule in place. (This
rule exists because A.8.6 amended §1 rule 1 in place and a *reversal* consequently read as a refinement
for six days — see A.10.)

**One-sentence summary.** Suspension is *declared* in signatures instead of inferred from genericity, and
the carrier is *written* by the elaborator instead of solved by the checker; effect elaboration then
becomes a syntax-directed **desugar phase** before checking, effects verify as a **row channel** beside
the type (the same architectural move as the Int-bounds refinement channel), and the NbE checker holds
one local rule and no effect decisions.

Successor to `docs/effects-as-channel.md` (v2), whose remaining checker machinery — the bridge, the
ladders, the `Id` apparatus — is live underneath until A.11.7/A.11.8 retire it.

## 0. Why revisit v2

v2 (uniform carriers) is internally coherent; this section records why it is a local optimum.

**Measured cost.** The effects-as-channel effort grew `monomorphize/check/` by ~1,185 lines (+30%),
`Checker.scala` alone by +581, plus ~900 lines of carrier/`Id` machinery. Of ~2,750 effect-related lines
in the checker layer, ~36% decided *where* binds go, ~29% existed to *prevent carrier metas from being
wrongly unified* (theft) — and the two were entangled: nearly every insertion arm was ordered to dodge
theft.

**Bug record.** Of 15 fix commits in the v2 window, the four highest-impact (including both dot-operator
bugs) are one failure: a carrier metavariable captured by first-contact unification, because nothing
distinguishes a carrier constructor from a data constructor in the unifier. The guard family was
documented as uncompletable. Three more were the `Id`-transparency tax.

**The premises v2 is optimal under** — v3 revisits all three:

1. **Carriers are ordinary types in one unsorted unification space.** Consequence: the theft class, the
   join solver, the pinned tags, the ladder orderings.
2. **Suspension is inferred from genericity per instantiation.** The `choose`/`pick` pair proves this is
   undecidable from signatures: identical callee shapes, and the bind decision is made by the *sibling
   argument's type*. This is implicit evaluation-order polymorphism — whether `readLine` executes at a
   call site depends invisibly on how generic the callee is.
3. **The carrier is a type argument the checker *solves*** (found late — A.10 — and the deepest of the
   three). Everything expensive is downstream of one line pair in `Checker` that mints a fresh meta in
   carrier position at every effectful reference: flex-flex `?F[X] ~ ?G[Y]` (which pattern unification
   cannot decompose), the theft class, `Id`-as-bottom with its normalizer, and mode undecidability.

Two observations seal it: `CarrierJoin` (`Id` = bottom, one non-`Id` winner, conflict = mismatch,
unsolved = `Id`) **is row unification** rebuilt inside carriers — v2's rejection of a row calculus
predates that evidence, and v3 *consolidates* scattered representations rather than adding one. And the
dot operator (`def .[A,B](a: A, f: A => B): B`) puts every chain's subject into a bare generic slot, so
the most common operator in the language permanently routed through the most fragile machinery. Under v3
it is effect-irrelevant by construction.

## 1. The user model (three rules)

1. **Effects run where they are written.** An effectful expression in any plain position performs its
   effects there; they join the enclosing definition's row. Strict call-by-value in **every** plain
   position, including a slot typed by a bare generic: `choose(readLine, readLine)` runs both reads. The
   only exceptions are the two *declared* ones below.
2. **Suspension is declared.** A parameter that must *not* run its argument declares an open row:
   `whenTrue: {G} A` receives the computation unrun. `if[T](c: Bool, value: {Abort} T)` already spells
   this — v3 makes the syntax mean what it looks like it means. Pure arguments fit suspended slots
   (`{} ⊆` anything).
3. **Pinned means captured** (unchanged from v2). `{Throw[E] | G} A` is a reified computation — an
   ordinary type, usable in `data` fields, discharger parameters, `List[TestCase]`. Open rows never
   appear in types; pinned rows are the only place a type contains a computation.

Rule 1 is implemented as of A.11.5 (it was withdrawn at bare-generic slots by A.8.6 for six days; A.10
reinstated it and A.11.5b removed the deferral).

Consequences the user sees:

- `something.foldLeft(f, z)` with `something : {Console} List[T]` **just works with zero declaration on
  `foldLeft`**: the effects run, the `List[T]` payload flows, `Console` joins the caller's row. The
  collections/data library stays effect-oblivious.
- Evaluation order is readable from signatures: a suspended slot says "may not run / may run later / may
  run repeatedly"; everything else runs exactly once, here.
- Rows remain the only effect surface (`def main: {Console} Unit`); diagnostics stay in payload/row
  vocabulary.

## 2. The checking model: two channels beside each other

Checking a runtime term yields a **payload type** (the existing NbE judgment, which never sees an effect)
and a **row** (a second output, exactly as an `Int`'s range lives in the refinement channel beside the
type, not inside it).

- **Row constraints are set-shaped**: union for sequencing, inclusion for boundaries
  (`derived ⊆ declared`). Row variables solve by union — **commutative and order-independent**, so no
  argument-order or sibling-order sensitivity can exist. There is nothing to steal: a row variable
  unifies only with rows, never with a type meta.
- **Rows attach to arrows** as latent rows (`A => {Console} Unit`). An unannotated signature arrow reads
  as a fresh row variable, making higher-order functions row-polymorphic by default.
- **Where it runs**: per definition, post-operator-resolution, reading only declared information. This is
  decidable *now* precisely because discharge is visible syntactically — a pinned-slot capture removes
  the argument's row by declaration, and a call needing effects the ambient does not provide runs on its
  own discharge stack (§3). The reason the old pre-mono `effect/` phase failed — it could not see
  structural discharge — is gone.
- **The `Effect` machinery marker** (`action: A => {Effect} Unit`) reads as "the ambient row variable of
  the enclosing signature".

**Two verifiers, one vocabulary.** The pre-mono row check (`RowElaborationProcessor.verifyRow`) reports
what declarations settle, at the definition, before anything downstream runs. The post-mono
`EffectAccountingProcessor` (`derived ⊆ declared` at ground mono keys, codegen precondition) remains the
unconditional fail-safe, per the use-site-verification cornerstone. Both emit the same message.

The pre-mono check is bounded exactly twice, by what declarations genuinely cannot settle (A.11.6):
**coverage** — an unknown callee may leave the derivation incomplete; and **decidability** — a definition
declaring no ambient whose declared return could *itself* be the carrier (an applied `Box[String]`,
`IO[Unit]`, or a generic-headed return) is the constructor-class shape, settled only by the
instantiation. Everything else is enforced, including a pure-returning definition that performs an
effect (the diagnostic the deleted `DeclaredPureChecker` used to voice post-mono, now earlier and naming
the effect).

## 3. Elaboration: a desugar, not a checker mode

A phase (`row/RowElaborator`, run by `RowElaborationProcessor` between the recursion gate and saturation)
rewrites each definition into **fully explicit monadic core Eliot** — the same shape v2's checker
*output* had, so monomorphization, ability resolution, `used`/`uncurry`, the jvm backend, `runMain` and
the synthetic main are unchanged consumers:

- A definition with declared row `{Console} Unit` gets its carrier binder (`F[_] ~ Console`) from the
  existing `EffectSugarDesugarer`.
- Every effectful call in a strict position becomes a `flatMap` chain (`$row$N` binders); `val x =
  <effectful>` binds.
- A suspended-slot argument passes as its carrier-typed computation, unrun; a pinned-slot argument is
  captured whole.
- **Pure code is untouched.** A definition with an empty row elaborates to itself — no `Id`, no wrapper,
  nothing to erase.

### 3.1 The elaborator writes the carrier

*(Decided A.10, implemented A.11.4. This is the change that made the rest of v3 implementable.)*

The ambient carrier at any point in a definition is a **syntactic** function of that definition's own
minted binder, so the elaborator computes it as a **term** and writes it:

```
printLine("hi")        ⟶   printLine[F]("hi")
flatMap(k, readLine)   ⟶   flatMap[F](k, readLine[F])
```

Every carrier position is then **rigid** — the definition's own ρ-bound binder or a written stack over it
— and the base binds once at the platform entry point, so both tracks (`IO` on jvm, `Either[String, _]`
on the compile track) work without the elaborator knowing which. Mechanically it costs nothing new:
`ValueReference.typeArgs` already threads explicit type arguments to the checker,
`EffectSugarDesugarer` mints the carrier as generic parameter **0**, and
`CarrierKindChecker.recordCarrierMetas` drops binders already supplied — so writing the carrier creates
**no carrier meta at all**.

Three rules complete it:

- **A region's carrier has three states** (`RegionCarrier`): `Absent`; `Spelled` — writable from the
  definition's own declaration (its minted binder `F`, the pinned stack its return declares minus the
  payload, a platform run carrier); and `Unspelled` — a carrier exists but is expressible only in a
  *callee's* binders (chiefly the interior of a pinned capture). All three place identically; only
  `Spelled` writes.
- **The carrier is written only where the mechanism reaches**: at a call whose callee's **first** binder
  is a declared carrier the post-argument result rides. First-binder is the *limit of the mechanism*, not
  a heuristic — `typeArgs` applies positionally, so writing binder *k* means writing `0..k-1`, which are
  payload types the elaborator cannot name. It costs nothing in practice (the desugar *prepends* the
  minted carrier; an ability method's own ability parameter leads); a hand-written discharger placing its
  carrier later (`catch[E, G[_] ~ Effect, A]`) keeps it inferred.
- **The discharge stack is derived, with no special case for the dot operator** (Robert's decision,
  A.11.4c): `carrier(call) = stack(callee.declaredRow ∖ ambient.declaredRow) over ambient`. A callee
  needing more than the ambient provides cannot be running on it, so it runs on the canonical stack of
  that difference — `rename` needing `{State[String]}` under a `{Console}` ambient carries
  `StateCarrier[String, F]`, does not perform on the ambient, and passes as data rather than hoisting.
  This is what makes a dot-chained discharger need no rule of its own; the rejected alternatives were
  inlining `.` and keeping a `.`-only deferral.
  - **The filter that makes it work, corpus-forced**: an effect the ambient does not declare is *not*
    automatically dischargeable. `Suspend`-riding effects (`Console`, `Log`, `Inf`) have no
    `<Ability>Carrier` at all and are provided by the *base*. Dischargeability is read off **the
    universe's own pinned rows** — an effect is dischargeable in this body iff a discharger for it is
    among the names the body reaches — which needs no lookup, since probing for a carrier type that does
    not exist is itself a hard compile error.

`Id` is written the same way: at a pure boundary the region is written `Id`, its arms come out
`pure[Id]`, and `runId` is written beside it. It is then **honestly well-typed** rather than
well-typed-modulo-normalization, which is what A.8.12's blocked slice lacked.

### 3.2 The whitelist (the anti-accretion guardrail, binding on every future change)

The elaborator may consult exactly these facts: a callee's declared parameter types and return type (slot
carrier-headedness, carrier-codomain arrows, atomic-vs-applied shape), its declared row and carrier
binders, its pinned metadata (`EffectRow.pinnedParameterIndices` / `returnPinnedEffects`), the
run-boundary registry, and one level of type-alias expansion inside those signatures. A decision that
cannot be made from the whitelist is a **design gap to close in the declarations** — never approximated
by a new syntactic rule. In particular, a rule that inspects a *sibling argument's expression shape* is
prohibited: that is inference, not desugaring. (An elaborator-local join over the callee's *declared*
parameter shapes is inside the whitelist; a sibling-expression rule is not.)

The fail-safe direction is built in: a missing rewrite leaves direct-style code the checker rejects
loudly; a wrong rewrite silently changes when an effect runs.

**This is not v1's weaver**: v1 erased and tried to *reconstruct* placement post-mono with no signal; v3
never erases — the signal is in signatures, read before checking.

## 4. What is deleted, what stays, what is added

> **A.11.0 is the authoritative, current inventory and A.11 the ordered roadmap.** One correction to the
> original promise: the checker does not end with *zero* effect code but with **one order-free local
> rule** — a pure term meeting a **rigid** carrier-headed expected type is `pure`-lifted (the default
> ladder's existing arm: no metas, no ordering, no lattice).

**Deleted from the checker layer.** `EffectLifter`'s arms; `UniformCarrierChecker`; the `carrier/`
package (`Carrier`, `CarrierJoin`, `UniformLadder`); `IdNormalizer` + `stripIdMachinery`;
`CarrierKindChecker`'s carrier-role seeding; and inside `Checker` the ladders' effect arms, the Phase A/B
deferral with its obligations, the pinning mechanisms, and the slot routers. Landed so far (A.8.8–A.8.10,
A.11.6): the unreachable arms, **every bind the checker used to insert**, the manufactured `Id` head, and
`DeclaredPureChecker` (subsumed pre-mono). Remaining: A.11.7 (bridge) and A.11.8 (obligations, the `Id`
apparatus, the carrier side table).

**Stays.** `EffectRow` and its pinned metadata (consumed by the desugar instead of the checker); pinned
surface and semantics; the dischargers and the `eliot.carrier`/`eliot.effect` packages;
`EffectAccountingProcessor` + `MonomorphicValue.ambientCarriers`;
`EffectRowRendering`/`GroundValueRenderer`; `RunBoundaryFunction`; the `Inf` story; the compile-track
`Either` discharge; `Id` as ordinary `data` with no `Suspend[Id]`.

**Added.** `row/RowChecker` (derivation + subset check per definition) and `row/RowElaborator` (the one
elaboration owner, writing carriers), in their own package with their own phase.

## 5. Interactions

- **Compile-time track.** Elaborated output is ordinary core Eliot; the compile track already evaluates
  explicit monadic code (the `Either` discharge for effectful signatures, `Effect[Id]`). It keeps v2's
  mid-spine default ladder by design — the compile-track boundary.
- **Monomorphization.** Mono keys keep today's shapes; use-site verification unchanged.
- **Diagnostics.** Row errors are per-definition at the user's own location, which fixes the
  `Suspend`-at-`Id` mislocation for free and replaces the cryptic `AbilityResolver` path for
  `State`/`Throw`/`Abort` leaks. Payload mismatches stay in payload vocabulary, since user slots never
  hold carriers.
- **LSP.** Hover reads payload type and rows from the row facts. Note the LSP's synthesized
  per-module monomorphization wrapper must *name* the platform run boundary (`runMain`), exactly as the
  jvm entry point does — inlining its body makes the wrapper read as a `Unit` definition performing the
  wrapped module's effects.
- **Known v2 limitations, updated.** The **val-bound discharge limitation is gone** (A.11.6): a `val`
  binding an effectful call binds the reified computation, because the call carries its own discharge
  stack, so the discharger reaches it through the binder. Open rows on by-value parameters flipped from
  *rejected* to *meaningful* (suspension). Still standing: a handler whose effects enter via a declared
  carrier-typed parameter must return a carrier-headed type (that carrier is caller-chosen and can never
  default to `Id`); and `Suspend`-riding effects cannot be pinned (the `Suspended` extension remains
  future work).

## 6. Stdlib and semantics migration

**The semantic break is real, and it is the price of the design.** An effectful argument at *any* plain
slot, bare-generic included, runs at the call site. Today's behaviour there was v2's, where the mode fell
out of how generic the callee happened to be — implicit evaluation-order polymorphism, invisible in the
signature. Removing it is the point of rule 2.

Because the failure mode is **behavioural, not a type error** — a lazy combinator silently becoming
strict — the corpus audit was a *gate before the flip*, not closeout work. **It ran (A.11.3-R) and the
break was measured at exactly one signature**: 1,047 deferral sites, 974 handing over a plain value, 43
already strict, 21 carrier-typed values passed as data, and 6 `Bool.fold` branch slots — the whole break.

Signature changes:

- `def fold[A](condition: Bool, whenTrue: {Effect} A, whenFalse: {Effect} A): {Effect} A` — **converted**
  (A.11.5a), with `import eliot.carrier.Effect` in `Bool.els`. The `{Effect}` spelling is load-bearing:
  it desugars to one shared inferable `F[_]` with `F ~ Effect` at binder **0**, and moving the carrier to
  a later binder misfeeds the `{ join(…) }` refinement's positionally-applied `Meta` companion.
- `if[T]` — **textually unchanged** (`value: {Abort} T` now *means* suspended). `Abort.else`'s
  `fallback: G[A]` was already declared-suspended; `foldEither`/`foldPair` take lambdas and are lazy by
  construction.
- `def foldOption[A, B](ifNone: {Effect} B, …)` — **did not convert; open decision** (A.11.5-R). Both
  spellings are refuted by the corpus, and nothing in it needs the conversion.

Every laziness-requiring signature must declare suspension; a combinator that forgets it becomes strict.

## 7. What this preserves of the cornerstones

Types-are-values: intact — carriers remain ordinary type constructors in elaborated code and pinned
types; the one evaluator and definitional equality untouched; no kind or sort is added to the type
language (the value/computation separation lives in the judgment's second channel, like refinement
ranges). Use-site verification: intact (accounting at ground keys). Total-by-default: intact (`Inf` is a
row entry). Platform layers: intact (elaboration is layer-agnostic; `IO` stays jvm-owned). "Effects are a
channel" is *restored to full strength*: v2 conceded the carrier as the checker's internal
representation; v3 makes the internal representation match the slogan — rows never become types, and the
only carrier-typed code is code the desugar wrote or the user pinned.

## 8. Migration plan — HISTORICAL (R1–R6)

> **Read A.11 for what to do next.** This is the record of the R1–R6 staging; the detail lives in the
> appendices it points to.

- **R1 — spike (2026-07-25).** A standalone row checker in test sources, 12 green cases through the real
  pipeline. Findings: the derivation rule is one line (A.1/A.2), the suspension surface already parses,
  and pinned-slot *entries* must be recorded beside the position tag.
- **R2 — surface (2026-07-25).** `EffectRow.returnPinnedEffects`/`pinnedParameterEffects` recorded at the
  desugar in declared (= discharge) order; `returnPinned`/`pinnedParameterIndices` became derived views,
  so no consumer changed. Consequence, fail-safe: a pinned entry's ability name now resolves like an
  open-row entry's.
- **R3 — row check, shadow (2026-07-25).** Promoted to `row/RowChecker`, swept over the real corpus
  against live v2: **zero disagreements** after two triaged findings (A.7).
- **R4 — elaboration desugar, shadow (2026-07-26).** `row/RowElaborator`, twin-verified against
  hand-written monadic twins (30 shapes) and then **shadow-compiled** end to end via fact injection, with
  behavioural identity as the oracle.
- **R5 — flip (2026-07-26).** Elaboration became a real phase; the row verification was wired. Running
  over the whole corpus exposed the mode question that A.8 records, resolved first by A.8.6's deferral
  (later reversed) and finally by A.10/A.11's written carrier.
- **R6 — closeout.** Folded into A.11.9/A.11.10.

## 9. Open questions

1. **`foldOption`'s suspension** — the one signature that did not convert (A.11.5-R). Three ways forward:
   build the lambda-binder purity rule the second spelling needs, convert when a shape actually requires
   it, or accept the strict `ifNone` permanently and say so in its apidoc. Needs sign-off; it is a
   reversal of the settled list.
2. Whether the post-mono accounting verifier can eventually retire, now that the pre-mono check is
   unbounded — not before experience says so.
3. The fate of `Checker`'s non-effect Phase-A/B remnants — whether spine inference simplifies further
   once slots are effect-free.

*(Answered and folded into the design: `Id` at discharge is fully syntax-directed (A.4); one latent row
per arrow (A.5); production rows are multisets of (ability, type-args) (A.6).)*

---

# Appendix A. The record

Everything below is history and operational detail. The decisions live in §§1–7.

## A.1 The derivation rules (live spec of `RowChecker`)

A `Row` is a set of effect-ability entries (production: multiset of (ability, type-args) — A.6).
Judgments are per definition, over the operator-resolved body, reading only *declared* information:

- **value-of**: `row(literal) = row(λ) = row(under-applied ref) = ∅`; `row(saturated call f(a₁…aₙ)) =
  riding(f) ∪ ⋃ᵢ contrib(aᵢ)`; `row(applied λ)` (the block/`val` desugar) `= row(bound arg) ∪ row(body)`.
- **riding** (A.11.6, the derivation half of §3.1's discharge-stack rule): `riding(f) = declared(f) ∖
  capturedByStack(f)`, where `capturedByStack(f) = (declared(f) ∖ ambient) ∩ dischargeable`. Effects the
  ambient does not provide and a discharger in scope can consume land in that call's own carrier stack,
  for a consumer to discharge — not on this definition's row. **This must stay the mirror of
  `RowElaborator.carrierAt`**: a verifier that counted what the elaborator had just routed elsewhere
  reports a leak for correct code (it did, for `Blocks` and `EffectsState`). A capture nothing discharges
  is still rejected — by the checker, against the declared return — and post-mono accounting, whose
  *ride test* against the ground ambient carrier this mirrors, remains the unconditional verifier.
- **contrib** at slot *i*: `contrib(aᵢ) = (row(aᵢ) ∪ latent(aᵢ)) ∖ pinnedEntries(f, i)` — the subtraction
  applies only when slot *i* is pinned; every non-pinned slot, strict *or suspended*, contributes
  identically.
- **latent**: `latent(λx.e) = row(e)`; `latent(under-applied ref f) = declared(f)`; else `∅`.
- **declared**: open-row return entries ∪ the effects constrained on the signature's carrier binders
  (machinery excluded); an effect-ability method's contribution is its own ability.
- **check**: `row(peeled body) ⊆ declared`, reported at the definition, bounded by coverage and
  decidability (§2).

`RowElaborator.performs` reads the same derivation against the **region's** row, not the definition's:
inside a pinned capture the region carrier provides that slot's pinned entries, and inside a
run-boundary argument it provides everything. Without that widening an effectful argument inside a
`catch` capture stops binding.

## A.2 Suspension is row-neutral

Whether a slot is strict (bind now) or declared-suspended (pass the computation) changes only *when* the
effect runs — elaboration's business — never whether the caller must declare it. The *only* slot mode
that touches derivation is pinned capture, as subtraction. Consequence: slot modes matter to the
elaborator alone.

## A.3 Worked shapes (each is a green `RowCheckerTest` case)

`use(items)` with an effect-oblivious `use` (the `foldLeft`-chain and dot-chain spine) → derived
`{Con}`; `choose(readLine, readLine)` and `pick(readLine, pure)` → identical derivations by the one
strict rule; `def leaky: Str = readLine` → leak located at `leaky`; `catchX(failing, h)` under a pure
return → `∅`, discharge-to-pure with no `Id` in the row story; an effectful handler joins its latent row;
partial discharge subtracts only what is pinned; a nested `{X, Y | G}` stack subtracts both entries;
`forever(…)` rides the union like any entry and leaks when omitted; `{ val x = readLine … }` sequences
through the applied-lambda desugar.

## A.4–A.6 Settled details

- **`Id` at discharge.** Rows never mention `Id`. It appears only in *elaboration*, inside a declared
  discharge region under a pure residual: the capture boundary is the pinned argument position, the
  stack's layers and their order come verbatim from the declared pinned spelling, and the base carrier is
  the enclosing region's — `Id` exactly when the residual row is empty, with `runId` at that boundary.
- **Latent rows.** One latent row per arrow; a function-valued argument's latent row joins the receiving
  call conservatively ("the callee may run it"), and calling a function-typed parameter contributes that
  parameter's declared arrow row, with `{Effect}` denoting the enclosing signature's ambient variable.
- **Production deltas.** Rows are multisets of (ability, type-args) — a name-set collapses `{Throw[A],
  Throw[B]}`; discharge consumes by type-arg match, and two same-ability entries with a non-pinning
  handler have no canonical order and stay a diagnostic asking for a pin. First-order abilities (`Show`)
  are distinguished from effect abilities by the missing HKT binder. Only the *outer* layer of a carrier
  stack is payload-applied (an inner layer's base is its last argument) — shape knowledge that belongs at
  the desugar, not at a consumer.

## A.7 The R3 shadow-sweep triage (both findings live)

1. **The nominal-run return** (`def main: IO[Unit]`): a definition returning the platform's concrete run
   carrier is the *nominal run* spelling of a boundary, where the concrete carrier captures the whole
   row. The row checker reads the run-carrier head off the **registered run boundary's own first
   parameter** (`runMain(io: IO[A])` ⇒ `IO`), never guessed from a name.
2. **The accessor merge dropped row metadata — a genuine latent defect, fixed in the layer merge.** An
   abstract discharger signature carries the pinned entries; its concrete twin is a `data` *accessor*
   whose synthesized definition carries an empty `EffectRow`, and the merge's body-preference discarded
   the abstract twin's row. `UnifiedModuleValueProcessor` now merges row metadata fieldwise.

## A.8 The R5 flip and the deletion slices (2026-07-26/27)

### A.8.1 The seam (live)

`OperatorResolvedValue` → `NamedValuesRewrittenValue` → `RecursionCheckedValue` → **`RowElaboratedValue`**
→ `SaturatedValue` → …

`RowElaboratedValue` carries the `OperatorResolvedValue` with only its body rewritten, which is why the
*sideways* reads later phases perform (a callee's signature, fixity, effect row) keep reading
`OperatorResolvedValue`; only `SaturatedValueProcessor` was repointed. Elaboration is placed after the
recursion gate, which walks the *user's* reference graph before any machinery call is spliced in.

**The universe is built by demand, not guessed**: `RowChecker.Universe.onMiss` reports every name
consulted but absent, the processor fetches exactly those and repeats until a round misses nothing new.
Guessing would silently fall back to unknown-callee approximations, and a wrong slot mode changes *when*
an effect runs — which no later phase catches.

**Position fidelity**: `assemble` returns the **original** nodes when nothing changed. Rebuilding an
equal spine re-attributes it to per-argument positions, silently moving every diagnostic anchored at a
call (and duplicating LSP hover hints).

### A.8.2 The rules the corpus forced (1–3 live, 4 completed, 5 deleted)

1. **A bare `[F[_]]` is not a carrier.** `EffectCarriers.declaredCarrierBinders` asks which binders a
   signature *declares* as carriers: ability-constrained (`[G[_] ~ Effect]`, every `{E}`-minted binder),
   the base of a declared **pinned** row (deliberately unconstrained, so nothing else marks it), or — for
   an ability method — its ability's own binder. `Console` and a constructor-class `Container` are the
   same shape; what separates them is the *use site*.
2. **`runId` only at a declared discharge** (superseded in scope by A.11.5a's pure-boundary rule, which
   subsumes it): a merely carrier-*returning* call has discharged nothing, so the desugar writes nothing
   and unification decides.
3. **A concrete carrier-typed parameter is data.** `implement Effect[IO]`'s own `fa: IO[A]` is ordinary
   data being taken apart. A parameter holds a computation only when its declared type is headed by one
   of the definition's **own** carrier binders, or is a pinned stack. Symmetrically a `pure`-lift
   requires the node to be **definitely pure**, and only an *atomic* declared type says "payload".
4. **Hoist iff the argument performs or discharges** — the row test plus the declared discharge clause.
5. **Relayed slot modes** — deleted, never committed. See A.8.3.

### A.8.3 Why rule 5 was a red flag (the lesson, kept)

The relay rule named nothing — no FQN check, no `.`-specific branch — yet it existed because `a.f(b)` is
`.(a, f(b))`, and it handled depth 1 only. **A rule invented so that one idiom elaborates is a rule
shaped by that idiom, whatever its stated generality**, and it was plainly inference: it propagated a
mode across a generic binder through a higher-order argument, which is what the *checker* does with
types. The rule was deleted at A.8.6 — but the *pattern* survived, as A.10 later measured: every
landing after it shipped "corollaries the corpus forced".

### A.8.4–A.8.5 The design question, and the option nobody had

The question was: can the desugar decide slot mode for positions flowing through a callee's generic
binders, from declarations alone? Three options were weighed — **(a) declare it** (fails at `.`, whose
`A` can forward to any slot of any callee), **(b) stage it** (give elaboration some type information),
**(c) bound it** (accept those shapes do not elaborate). A.8.6 chose (b).

**All three presuppose there is a mode to decide there.** With a *written* carrier there is none — an
effectful call hoists, a pinned or carrier-typed value passes as data, both declared — so the question
dissolves (A.10). That was the fourth option, and it also revives (a) for any residual case, since (a)
failed *only* at `.` and `.` needs no relay rule under written carriers.

### A.8.6 Bounded staging — REVERSED by A.10

> The decision to let the desugar defer instantiation-decided positions. It withdrew §1 rule 1 at
> bare-generic slots and was recorded as a *refinement*, which it was not. Its justification (protecting
> dot-chained discharge) is measured false once the carrier is written (A.10), and the deferral it
> introduced is precisely what kept the carrier inferred and the whole v2 machinery alive: **a deferred
> position is one the elaborator writes nothing at, so it cannot write the carrier there either.**
> Removed at A.11.5b.

Four corollaries it forced are **live guardrails**, independent of the deferral:

1. **Payload-by-construction binders.** A block binder bound by an elaborator-*inserted* `flatMap` holds
   the computation's payload by construction, so a reference to it is definitely pure. Elaborator-owned
   information, not inference — without it the checker first-contact-unifies the payload meta with the
   carrier (the State-under-`Id` miscompile).
2. **An inserted rewrite must be fully discharged or rolled back whole.** Hoisting requires a
   classifiable core; otherwise the machinery the elaborator writes puts an unclassifiable node into a
   carrier-typed position the checker then commits wrongly (a `ClassCastException`, an `Abort`-stack
   double-wrap).
3. **Pinned captures never boundary-wrap.** Pinned means captured: a pure actual does not lift into a
   pinned slot.
4. **Untouched code keeps its original nodes, transitively** (see A.8.1's position fidelity).

### A.8.7 The post-drain resolver — live, and slated for deletion at A.11.8

To finish the deferred positions, a resolver runs at **quiescence** rather than mid-spine:
`monomorphize/check/ModeResolver`, driven from `TypeStackLoop`'s post-drain fixpoint (drain → modes →
abilities per round), classifying each obligation against the *solved* store and splicing
`RowElaborator`'s own rewrites with a fueled restart. Mid-spine resolution was rejected because
resolution would run while other metas are half-solved, making *ordering* a correctness concern — the
shape of all four of v2's highest-impact bugs.

Its **guardrails outlived it and still bind**: a component may read solved metas and splice rewrites; it
may never run inside unification, never retract a solution, never grow an ordering arm. A shape that
genuinely needs mid-drain resolution is a **stop-and-redecide signal**, not a license for a mid-flight
arm.

Three landing corollaries, each still a live fact about the machinery: the splice must `pure`-wrap a
*payload* core read off the solved spine type (else a bare-generic tail is stolen by the carrier
codomain); a suspension-holding spine must wrap **no** mid-spine binds (the flex-core `map` default
reorders effects — `andThen(printLine("trying"), abort)` printed nothing); and a deferred `let` is its
own obligation (a post-drain adoption under a plain `let` silently drops the effect).

With the deferral gone (A.11.5b), this whole path is what A.11.8 deletes.

### A.8.8–A.8.10 Deletion slices 1–3 (2026-07-27)

All three were **evidence-driven, not eyeballed** — see A.9.4 for the method.

- **Slice 1 (544 net lines): the arms the resolver made unreachable.** `EffectLifter.tryBindLift` (all
  four call sites zero on both tracks — the desugar writes every bind now), `tryIdDefault` (all three
  zero), the `allowBindLift` flag and the arms it selected (an unreconcilable carrier is now a committed
  mismatch — the fail-safe direction), **two of the four pinning mechanisms** (`recordRowArgumentPins`
  recorded 98 pins and `applyPendingCarrierPins` applied **0**, the eager pin having already pinned the
  slot), the compiler-track Phase-B remnant, and the zero-caller uniform code.
- **Slice 2: the checker inserts zero binds.** Traced at **outcome** granularity, the whole gate's
  runtime node-inserting decisions were only ~236 against routers firing 5.9k/1.6k/964 times. Every
  surviving bind came from a position the desugar had **correctly deferred** — the dot operator
  (`printLine(x.field)` = `printLine(.(x, field))`, whose return is its own bare binder) and
  declaration-generic slots a sibling had already rigidified (`"Hello, " ++ readLine` solves `A := String`
  from the left operand). Fix: two more suspension sites feeding the existing queue —
  `isDeclarationGeneric` reads the **unforced** domain, and a payload slot with a carrier-headed actual
  suspends instead of binding. The *only* bind producer left in the checker afterwards is the
  immediately-applied-lambda `let` rule (`bindWrap` direct), which A.11.8 inherits.
  - **The corollary that is still load-bearing**: classify by the **carrier-role tag off the unforced
    type**, never the forced shape. Calling any non-ambient `VTopDef` spine result a payload made a
    discharge stack (`AbortCarrier[IO, String]`) get `pure`-wrapped → re-hoisted → wrapped, forever
    ("mode resolution did not converge"). Recognizing `AbortCarrier` *by name* would have been
    prohibited; the elaboration-threaded tag is what the cornerstone sanctions.
- **Slice 3: the checker stops manufacturing `Id`.** Not an elaboration decision but an **encoding**: v2
  wrapped every pure judgment (`T` → `Id[T]`, term → `pure@Effect[Id](term)`) so slot arms could split a
  carrier unconditionally, and `IdNormalizer` erased it again. The trace showed ~95% identity
  (`idWrap` 6,641 vs `purePayloadPass` 4,504 — each pair literally `runId(pure@Id(x))`; 756 of 793
  distinct return-boundary sites lifted into `Id` itself). The bridge now **classifies** instead of
  wrapping (`ActualForm` = `Carried` / `IdCarried` / `Pure`), and a pure term gets no node unless the
  position is carrier-**headed**.
  - **The fail-safe the re-trace caught (do not lose it):** a computation whose carrier meta is already
    *solved to `Id`* is pure to `Carrier.split` but carried to `effectCarrierSplit`; passing it through
    unchanged puts an `Id[..]` value in a `?G[..]` slot and the join (for which `Id` is no contribution)
    does not object. The carrier-slot arm resolves the actual's carrier first and pure-lifts when it is
    bottom. The first cut without this looked like an improvement (`carrierPureLift` 39 → 1) — hence
    **measure twice, before and after**.

### A.8.11–A.8.12 Slice 4: measured, attempted, and cancelled

**What the bridge still did (A.8.11).** A differential probe (plain unification run speculatively at
every bridge site, meta solutions diffed) over 11,844 entries: 10,188 agree, 146 differ only in flex-flex
solve *orientation*, 4 error-divergences from a unit suite, and **1,506 genuinely differ with a
carrier-role meta**. Those 1,506 all say one thing — ordinary pattern unification cannot decompose a
**flex-flex application** (`solveMeta` tries injectivity only against a rigid applied rhs, else
postpones), so `?F[X] ~ ?G[Y]` strands. So carrier-safe unification is exactly two rules: **carrier
decomposition**, and **`Id` is bottom** (never *commit* a flex carrier meta — plain unify would decompose
`Id[T] ~ ?G[T]` to `?G := Id`, the premature-commitment bug class).

**Why it is cancelled.** Both rules exist only because the carrier is a *metavariable*. With the carrier
written (§3.1) every carrier position is rigid, `unifySpines` handles it, and the `Unifier` is not
touched at all — which also dissolves the cornerstone sign-off question a role-keyed decomposition arm
would have needed.

**Slice 4a, attempted and reverted (A.8.12) — the lesson survives.** Moving the checker's boundary
`pure`/`runId` insertions out to a source-level splice failed because `catch`'s declared return is its
own binder `G[A]`; at a pure use `G := Id`, so the discharge-to-pure arm wrapped the body in `runId`,
giving it type `T` where the declaration said `Id[T]` — well-typed **only modulo the downstream
`Id`-normalizer**. A checker-inserted node may rely on that; **a source-level splice cannot** (the
restarted check re-derives and reports "Expected: String / Actual: String", the renderer erasing the
`Id`). A.10 then showed the blocker was *the inference, not the node*: with the carrier written,
`catch[String, Id] : Id[String]` and `runId` is an honest projection. The same blocker reappeared in
argument-slot form at A.11.5a and dissolved the same way.

## A.9 Method and gotchas (reuse these)

*(The rest of the original handover — tree state, the `unify` guardrail decision, the 4a-after-4b
ordering, the 4b inventory — is superseded: 4b is cancelled and A.11 owns the plan.)*

### A.9.4 The method

- **Arm-liveness tracing, not inspection.** A temporary `ArmTrace` object (env-gated on `ELIOT_ARM_TRACE`
  = the dump directory; a JVM shutdown hook writes one file per process) with a `fire(arm, sample)` call
  on every arm under consideration. Run the whole gate — `./mill __.test` **and** a compile of all 40
  examples — then aggregate. Only delete zero-fire arms.
- **Trace at outcome granularity, not entry count.** A router's entry count is mostly *routing*; what
  matters is which arm *decided* what.
- **The differential probe.** Where the question is "does this machinery still differ from the ordinary
  path?", run the ordinary path speculatively on the *pre* state and diff the resulting meta solutions
  plus the error count. Two counting cautions: a postponement is not an error, so `agree` over-counts
  where the plain path would have deferred; and a flex-flex solve recorded in the opposite direction
  shows up as `differ` while being semantically identical — read the samples, not just the counts.
- **Measure twice, before and after** (slice 3's first cut looked like an improvement while dropping a
  fail-safe).
- **The byte-identity oracle.** Build a baseline in a `git worktree` at the pre-change commit, compile
  all examples in both trees, and compare (1) **program output** and (2) **class content unzipped from
  the jars** — jars themselves are not byte-reproducible (timestamps), and `$row$N` binder numbering can
  legitimately rename lambda classes. Byte-identity is a safety oracle, not a hard gate.
- **Diffing error sets.** Some sessions (the LSP compile) emit hundreds of pre-existing internal-error
  messages; diff the error set against a stashed baseline rather than reading them.

**Tracer gotchas** (each cost time at least once): the env-gated object plus shutdown hook does propagate
into mill's forked test JVMs, but mill **prefixes every forwarded line with a worker id**, so
`grep '^SPLICE'` finds nothing — never anchor a grep. Sample keys must carry the range **end** as well as
the start (elaborator-generated nodes reuse an argument's `Sourced`, so start-only keys conflate distinct
nodes) and, for an argument, its spine **head**. And `target/.eliot-cache` must be deleted before every
run or the pipeline replays facts and the trace comes back empty.

### A.9.6 Gotchas banked from the slice-4a attempt

- **A source-level splice must type-check; a checker-inserted node need only survive Id-normalization.**
  Distrust any plan that moves an insertion out of the checker without first removing the `Id` slop it
  relies on.
- **`MonomorphicTypeCheckTest.effectLiftImports`' `Effect` and `State` stubs reference `IO` without
  importing it.** Latent today; any change that demands more of the stub modules surfaces it as "Name not
  defined." anchored at `IO`.
- **Any harness whose snippet can discharge to a pure value needs an `eliot.lang.Id` stub.** A
  checker-inserted `runId` carries its `ValueFQN` and never goes through name resolution; a source-level
  one does. `SystemImport("Id", "type Id[A]\ndef runId[A](obj: Id[A]): A")` suffices for name-only
  harnesses; one that needs the instance wants the concrete `data Id[A](runId: A)` + `implement
  Effect[Id]`.
- **`Sourced` identity is the splice's only handle.** `spliceResolvedModes` matches targets by reference
  identity against the body in `resolvedValue.runtime`. Rebuilding an untouched subtree breaks it.

## A.10 The stock-take, and the written-carrier spike (2026-07-27)

Two slices in a row were blocked by the substrate rather than by effort, so the question asked was not
"how do we do 4b" but "is the deletion working at all, and would any decision — including reversing an
earlier one — greatly simplify this?"

**The framing, which is the whole lesson.** There were two premises with very different histories.
**Slot mode** (§1 rules 1–2) was decided at v3's founding and *never implemented*: A.8.6 withdrew it six
days later. **The carrier as a checker-solved type argument** was genuinely unexamined — by v2's fork and
by v3 §0 alike. The connection between them is mechanical: **a deferred position is one the elaborator
writes nothing at, so it cannot write the carrier there either.** A.8.6's "small local concession" is
exactly what kept the carrier inferred and the whole v2 machinery alive.

**Why the reversal read as a refinement** (worth remembering, because the next one will look the same):
it was recorded as *narrowing a cost*; its justification was *protecting a shipped idiom* (dot-chained
discharge), later measured false; and A.8.4's three options all presupposed there was a mode to decide.

**The measurement.** Effect machinery under `lang/src` (`check` + `carrier` + `row`): 3,996 pre-v2 →
5,585 pre-v3 → 6,895 at the stock-take. Across the whole v3 window `check/` was **+38** and the total
**+1,310** — v3 had added a second effect system beside the first. The qualitative symptom: every landing
since A.8.6 shipped "corollaries the corpus forced" (4 + 3 + 1 + 1), which is the A.8.3 pattern with the
rule deleted and the pattern kept.

**The spike (green).** Env-gated switches forced the bridge off and the deferral off. Bridge off, the
examples went 36/40 → 33/40: exactly four regress (`Concat`/`EffectsMulti`/`EffectsTwoDeps` with
"Higher-kinded type parameter mismatch" — the flex-flex decomposition, now user-visible; `EffectsThrow`
with an ability-selection failure inside the stdlib's own `Throw.els`). **Bonus: `IfDemo`, a
long-standing baseline failure, compiles with the bridge off — the bridge causes it.** Those four plus
three more, hand-written in the target form (explicit carrier at every effectful call and inserted
combinator, `Id`/`runId` written), **all compile bridge-off *and* deferral-off with zero obligations,
zero inserted nodes, and output byte-identical to baseline**: `XConcat`, `XTwoDeps` (the dot operator +
nested `DepCarrier`), `XMulti`, `XThrow` (stdlib `catch` + guarded instance), `XPureA` (A.8.12's
blocker), `XPureB` (`foldLeft` accumulator), `XState` (nested `StateCarrier` + `Console` riding + `Id`
discharge + the dot-chained discharger).

**What the spike did not prove**, of which one item is still open:

1. It validated the target *form*, not the elaborator. The discharge-stack instantiation was
   syntax-directed (landed as A.11.4c); `foldLeft[String, F[String]]` + `pure[F]("")` was the open
   accumulator question — **resolved at A.11.2-R: build neither mechanism.**
2. **No partial rollout** — bridge-off breaks the stdlib's own `runStateToValue` body, so the flip is
   whole-program (done at A.11.5).
3. **Untested then, and still worth watching**: `Inf`; the compile-time track's `Either` carriers and
   `CalculatedReturnResolver`; guarded instances beyond `Throw`'s; two distinct `State[S]` layers (for
   which the stdlib has no lift instance in any case). The spike's `main` used the nominal-run `IO[Unit]`
   form throughout, not a row.

# Appendix A.11. The roadmap: from this tree to the end state

The whole remaining path, including what a plan written from inside the checker keeps missing: flags,
experiment scaffolding, the v2 artefacts outside `check/`, the test suites that pin dead machinery, and
the docs. Each step lands green on its own and is verified by the A.9.4 method.

**Done: A.11.1 – A.11.6. Next: A.11.7.**

## A.11.0 The end state, as an inventory

**What must exist.** `EffectSugarDesugarer` (rows → carrier binder + pinned metadata); `row/RowElaborator`
(the one elaboration owner, *writing* carrier expressions); `row/RowChecker` (`derived ⊆ declared` per
definition); `EffectRow` and its pinned metadata; `EffectCarriers` / `EffectMachinery`;
`EffectRowRendering` / `EffectCarrierNaming` / `GroundValueRenderer`; `RunBoundaryFunction`;
`channel/EffectAccountingProcessor` + `MonomorphicValue.ambientCarriers`; the `WovenValue` codegen seam;
`AbilityResolver`; the `eliot.carrier`/`eliot.effect` packages and their dischargers; `Id` as ordinary
`data`, honestly written with `runId` beside it; the compile-track `Either` discharge. In the checker,
exactly **one** effect rule: *a pure term meeting a rigid carrier-headed expected type is `pure`-lifted.*

**What must not exist**, grouped as the roadmap deletes them (line counts as measured at the stock-take):

| group | items | lines |
| --- | --- | ---: |
| the v2 bridge | `monomorphize/carrier/` (`Carrier` 82, `CarrierJoin` 124, `UniformLadder` 188), `check/UniformCarrierChecker` 289, and in `check/Checker`: `routeArgumentSlot`, `uniformPayloadSlot`, `uniformCaptureSlot`, `uniformCarrierSlot`, `uniformArgumentSlot`, `payloadFitsDomain`, `uniformPayloadOf`, `singleLayerCarrierDomain`, `eagerRowPinIntoDomain`, `findCarrierLayerSlots`, `uniformReturnBoundary`/`uniformReturnRoutable` | ~933 |
| the obligation/resolver path | `check/ModeResolver` 213; `CheckState.modeObligations`/`letObligations` + `ModeObligation`/`LetObligation` + `recordModeObligation`; `Checker.genericArgSlot`/`defaultArgSlot` deferral arms + `resolveDeferredSlot` + `SlotOutcome.Deferred`/`Suspended`; `TypeStackLoop`'s splice-and-restart, its fuel, and `processIO`'s `Either` return; `RowElaborator.spliceResolvedModes` | ~350 |
| the `Id` apparatus | `channel/IdNormalizer` 308, `PostDrainQuoter.stripIdMachinery`, `WovenValueProcessor.assertNoIdResidue` | ~340 |
| the carrier side table | `Unifier.carrierRoles` / `isEffectCarrier` / `CarrierRole`; `CarrierKindChecker`'s carrier-role *seeding* (its kind checking stays); `EffectLifter`'s remainder beyond the one pure-lift arm and the node builders | ~400 |
| ~~A.8.6's uncertainty~~ | **DONE at A.11.6**: `uncertain`/`Derivation.deferred` and `DeclaredPureChecker` deleted; two boundings remain by design (coverage, decidability), and `capturedByStack` was added to the derivation | −150 |
| flags & experiment scaffolding | `CompilationSession.compileOnce(seedFacts)` (a production API added only for the R4 shadow compile); `jvm/test/.../RowElaborationShadowCompileTest`; the shadow half of `jvm/test/.../RowShadowSweepTest` | ~750 |

**Arithmetic to hold the work to.** `check/` was 5,219 at the stock-take and is **5,097** after A.11.6;
the remaining deletions remove ≈950 more, landing it at ≈4,150 against the **pre-v2 baseline of 3,996**.
The machinery as a whole goes 6,895 → ≈5,200: below the pre-v3 5,585, above pre-v2, and the difference is
`row/` — a phase that did not exist, now holding the work the machinery used to do. **Do not claim a net
reduction against pre-v2.**

**Exit criteria, all mechanically checkable:**

- `grep -rin "uniformCarrier\|CarrierJoin\|UniformLadder\|IdNormalizer\|ModeObligation\|seedFacts" lang/src jvm/src eliotc/src` → empty.
- No `lang/src/.../monomorphize/carrier/`.
- No env-var, system-property, CLI or constructor gate anywhere in the effect path — one code path only.
- `check/` at or below 3,996 lines.
- Full gate green (871 test targets across lang/jvm/eliotc/LSP is the current baseline), and **37 of 40
  examples compile** (`IfDemo` included — the bridge causes its failure;
  it already compiles as of A.11.4).
- `docs/effects-as-channel.md` retired; the CLAUDE.md cornerstone describes rows and written carriers.

## A.11.1–A.11.3 Done: scaffolding, the accumulator decision, the corpus and the audit

- **A.11.1** deleted the dead v2 spike (`monomorphize/spike/UniformCarrierSpike` + test, 622 lines) and
  four stale scaladoc references to a `uniformCarrier` gate removed weeks earlier.
- **A.11.2-R — the accumulator question: build neither mechanism.** An effectful `combine` makes
  `foldLeft`'s payload generic carrier-headed through a *sibling*, so a pure `initial` must lift. Decider:
  the slot occurs **exactly once** in the whole corpus (`List.foreach`, where the stdlib already writes
  `pure(unit)` by hand), so no library code depends on inferring it. A pure `initial` under an effectful
  `combine` is a type error the user fixes with `pure(0)` — and the elaborator writes the carrier at that
  `pure` like any other call. Signatures unchanged; §1's effect-oblivious collections stand; both
  rejected options stay buildable if this is ever measured to bite.
- **A.11.3a** extracted the shared corpus to `jvm/test/.../EffectCorpus.scala` (`combinedProgram` +
  `infProgram`), depending on nothing, so A.11.9 can delete the shadow harnesses without taking it along.
- **A.11.3-R — the mandatory pre-flip semantic-break audit, run.** Method: an env-gated recorder on the
  deferral branch of `elaborateCall` logging (definition, callee#slot, declared slot shape, argument
  spine head, four verdicts), dumped at JVM exit, over the whole gate plus all 40 examples; instrument
  reverted. **1,047 distinct deferral sites; 974 hand over a plain value.** Of the 73 carrier-valued: 43
  genuinely strict (`.` 22 including the 2 dot-chained discharges, `==` 8, `++` 1, `show` 1, `putState`
  1, 10 in hand-written test combinators) where hoisting is already the behaviour; 21 carrier-typed
  *values* inside the carrier machinery that rule 1 passes as data; and **6 = `Bool.fold`'s two branch
  slots — the entire §6 break.** The numbers held exactly at the flip.

## A.11.4 The elaborator writes the carrier — done

Landed in three commits, gate green, **37/40 examples (was 36)** with all 36 baseline programs
byte-identical in output *and class content*; `IfDemo` compiles for the first time, which is an exit
criterion.

The design is §3.1; what the corpus forced during landing:

- A discharging call's residual is **not** `Id` merely because it discharges (`Effect[AbortCarrier[G]]`'s
  own `flatMap` calls `runAbort(fa)`, whose carrier is plainly `G`) — `Id` belongs only at the two
  boundaries the elaborator inserts `runId` at.
- The dischargeability filter of §3.1 (a naive row difference killed all 40 examples with "Could not find
  `ConsoleCarrier^Type`", because the synthesized entry captures a `{Console}` value on `IO`).
- `CarrierBookkeepingTest` pins the effect: `def echo: {Console} Unit = printLine("x")` mints **zero**
  carrier metas where it minted one.

**Caveat recorded and since fixed:** `IfDemo` compiled but silently *dropped* its effectful `Bool.fold`
arms, because they sat at bare-generic slots and `fold`'s generic-headed return was not carrier-valued.
A.11.5's declared suspension is exactly that fix.

## A.11.5 Convert the stdlib and jvm layers — done

Whole-program, in one step (no partial rollout: bridge-off breaks the stdlib's own `runStateToValue`).
Order was forced: **the signature change first, then the deferral removal** — `Bool.if` is
`fold(condition, value, abort)`, so strict slots before `fold` declares suspension make `if` abort
unconditionally.

**A.11.5a — `fold` declares its suspension, and a pure boundary names `Id`.** Three supporting fixes,
each a *symmetry restored* rather than a rule added, and each forced by a measured failure:

1. **The pure-boundary `Id` default** (`RowElaborator.pureBoundaryRegion`): at a value position with no
   region carrier, a call whose result rides its callee's own carrier binder *and* whose binder declares
   no user effect runs on `Id` (region written `Id` ⇒ arms `pure[Id]`, node `runId`). It **subsumes** the
   old `discharges` test. **An ability method is excluded outright, machinery or not** — its carrier
   comes from instance resolution, which is what makes `def f: Box[String] = wrap(s)` and
   `def e: Either[String,String] = pure("hello")` the same shape, and neither of them this one.
2. **`UniformLadder`**: a payload slot that *declares* `Id[T]` (`runId`'s own `obj`) takes it as data
   instead of projecting — A.8.12's blocker in argument-slot form, dissolving exactly as predicted.
3. **`NbeEvaluator`**: a written type argument now reaches native leaves, and the native's own declared
   `paramType` decides whether to apply it (type parameter ⇒ any type argument; value parameter ⇒ only a
   ground constant, e.g. `integerLiteral[128]`). **Unsure ⇒ not applied**: a dropped argument leaves the
   native unapplied and loud, a wrongly applied one computes the wrong answer silently. Do *not* give
   `fold`'s native a carrier parameter — the write is not guaranteed (regions can be Absent/Unspelled),
   so a 4-arity native would misreduce silently.

**Binder order is load-bearing**: spelling `fold[A, G[_] ~ Effect]` (carrier second, so the elaborator
leaves it inferred) breaks the `{ join(…) }` refinement, whose `Meta` companion is applied positionally.
Second independent reason the `{Effect}` spelling is right.

**A.11.5b — the deferral is gone.** `elaborateCall`'s generic-headed-slot branch and `genericHeaded` were
deleted with **nothing replacing them**: A.11.4c's derived discharge stack already makes a dot-chained
discharger's argument carry a *different* carrier, so it does not perform on the ambient and passes as
data. Gate: every program's output *and every class file* byte-identical to the A.11.5a build — removing
the deferral changed no compiled program.

**`foldOption` did not convert** — §9 open question 1. Both spellings are refuted by the corpus: as
specified (`ifNone: {Effect} B`, strict `ifSome`) it breaks `Effect[AbortCarrier[G]]`'s `flatMap`, which
passes an effectful `a -> runAbort(f(a))`; with `ifSome: A => {Effect} B` (arrow spelling —
`Function[A, {Effect} B]` fails the lexical layer merge) the machinery types but the ordinary pure use
`o.foldOption("<none>", s -> s)` stops, because the elaborator cannot prove a **lambda binder** pure and
so leaves `A = F[B]`.

**A rule written then withdrawn, keep the shape**: `writeCarrier`'s `ridesAmbient` — write the ambient
only where the callee *performs* on it, so an effect-transparent combinator inherits its carrier from its
arguments. It fixes a genuine miscommitment and the example corpus stayed green, but it breaks
`Path.extension` (`fold` under a pure `Option[Path]` return; only `FileIoIntegrationTest` covers it):
unwritten, the carrier meta never resolves and the body quotes as `$bad-apply(Option(String))`.

## A.11.6 Unbound the row check; `DeclaredPureChecker` deleted — done

**What landed.** `RowChecker`'s `uncertain` row, `Derivation.deferred` and `genericHeadedSlot` are gone:
with every plain slot strict, a rowed argument at a bare-generic slot joins the caller's row certainly.
`verifyRow` lost the "only a definition that declares an ambient" bounding, so a pure-returning
definition that performs an effect is reported pre-mono, at its own definition, naming the effect.
`check/DeclaredPureChecker` (105 lines) and its wiring are deleted, along with `TypeStackLoop`'s
`residualBody` parameter, which existed only to feed it.

**The decision on `DeclaredPureChecker`, measured rather than assumed.** Its domain is exactly what the
row check now enforces; its discharge-awareness is structural here rather than a committed-mismatch gate;
and its message named no effect where the row check names it. Both curated diagnostics were checked
first: `def echo: String = printLine(readLine)` is now issued by the row check, earlier and better, and
the effectful-lambda-under-a-rigid-pure-codomain case never came from it at all (it is an ordinary
`Type mismatch.`). Deleting it can lose no program: it only ever *replaced* a message on a value whose
mono had already failed.

**The corpus forced one addition** — the derivation had to learn the rule §3.1 gave the elaborator. See
A.1's `riding` clause: without it, `Blocks` and `EffectsState` (both dot-chained discharge) read as
leaks, because the pinned-slot subtraction cannot see through `.` and the discharger is a *sibling*
argument the whitelist forbids inspecting.

**Two boundings remain, and they are a different kind** (§2): coverage, and decidability — a no-ambient
definition whose return may itself carry is the constructor-class shape
(`AbilityImplementationCheckProcessorTest` proves it), which is the same exemption `DeclaredPureChecker`
applied to an applied return.

**A documented limitation disappeared**: val-bound discharge works (verified on output, both branches;
pinned in `CatchShapeMatrixTest`'s Group F). CLAUDE.md was corrected in the same commit.

**One synthesized source had to be corrected, not worked around**: the LSP's per-module monomorphization
wrapper inlined `runMain`'s body, which now reads as a `Unit` definition performing the user's effects.
It names `runMain` — as the jvm entry deliberately does, for a taggable capture slot — and `LspPlugin`
registers the boundary. Symptom if this regresses: every TypeHint index test returns empty.

**Gate:** full `__.test` green, 37/40 examples, every program's output and class content byte-identical
to the A.11.5 build. `check/` 5,219 → 5,097.

## A.11.7 Delete the bridge

`monomorphize/carrier/`, `UniformCarrierChecker`, and the routers in `Checker`. Trace first and delete
only zero-fire arms — after A.11.4/A.11.5 the whole group should be cold, and **any arm that still fires
is a missing elaborator rule, i.e. a stop-and-redecide signal**, not a reason to keep the arm.

Keep exactly one thing: the pure-lift rule (the default ladder's existing pure-wrap arm against a
**rigid** expected type) plus the `pureWrapNode`/`runIdNode` builders, which move to `row/` as the
elaborator's node constructors. Note A.11.5a left one live bridge behaviour to preserve or relocate: a
payload slot declaring `Id[T]` takes it as data.

## A.11.8 Delete the obligation path, the `Id` apparatus, and the carrier side table

In this order, each possible only after A.11.7:

1. **Obligations** — `ModeResolver`, `CheckState`'s obligation vectors, the `Deferred`/`Suspended`
   outcomes, `resolveDeferredSlot`, `TypeStackLoop`'s splice-and-restart and fuel, `processIO`'s `Either`
   return, and `RowElaborator.spliceResolvedModes`. `TypeStackLoop` returns to a plain post-drain
   fixpoint.
2. **`Id`** — `IdNormalizer`, `stripIdMachinery`, `assertNoIdResidue`. Record *why* this is safe: `Id` is
   no longer an encoding the checker manufactures and a normalizer erases; it is written where it belongs
   and typed honestly. `Id` remains ordinary `data` with no `Suspend[Id]` — the soundness guard is
   unchanged.
3. **The carrier side table** — `Unifier.carrierRoles`/`isEffectCarrier`/`CarrierRole` and
   `CarrierKindChecker`'s carrier-role seeding, once nothing seeds or reads them. `CarrierKindChecker`'s
   HKT kind seeding and post-drain verification are a separate concern and stay. `unify/CarrierRoleTest`
   goes with the table.

This is also where the cornerstone guardrail is honoured by *not* acting: the `Unifier` gains nothing.

## A.11.9 Remove the experiment scaffolding and fix the test suites

- **`seedFacts`**: `CompilationSession.compileOnce`'s optional parameter exists only for the R4 shadow
  compile. Remove it with the shadow harness — a production API kept alive by one test is exactly the
  residue this section exists to catch.
- **Delete**: `RowElaborationShadowCompileTest`, and the shadow half of `RowShadowSweepTest` (its corpus
  moved to `EffectCorpus` at A.11.3a).
- **Delete with their machinery**: `carrier/CarrierMechanismTest`, `check/UniformCarrierCheckerTest`,
  `check/CarrierBookkeepingTest`, `check/EffectLifterTest`, `channel/IdNormalizerTest`,
  `unify/CarrierRoleTest`.
- **Rewrite**: the lift group of `MonomorphicTypeCheckTest` (its generic-slot shapes assert the deferred
  v2 spellings, which stop existing) and `RowElaboratorTest`'s twins (explicit carrier args).
- **Rename and keep**: `jvm/.../UniformCarrierCompileTest` (244) and `UniformCarrierConditionalTest`
  (101) are v2-*named* but are behaviour gates over the real base layer. Rename to something the end
  state can justify and keep every program.

## A.11.10 Docs closeout

- **CLAUDE.md**: rewrite the *Effects Are a Channel (Uniform Carriers)* cornerstone. The uniform/`Id`-headed
  judgment invariant, the carrier-meta join, and "any new consumer must Id-normalize first" all stop
  being true; what replaces them is: rows are the surface, the elaborator *writes* the carrier, carriers
  are never metavariables, `Id` is written where a discharge lands on a pure boundary, and the checker
  holds one pure-lift rule. (The val-bound-discharge limitation and the two-verifier split were already
  corrected at A.11.6.)
- **`.claude/skills/eliot-monomorphize/SKILL.md`**: drop the bridge/ladder/join description.
- **`docs/effects-as-channel.md`** (2,002 lines): retire it — reduce to a short historical note pointing
  here, or delete it and let git hold the history.
- **This document**: fold the appendices into the design sections where they are settled, keeping the
  standing rule from the top.
- **`IfDemo`**: it compiles again as of A.11.4; confirm its output is what the example intends.
