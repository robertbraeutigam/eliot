# Effects as Rows, v3: Declared Suspension + a Written Carrier

**Status (2026-07-27).** The design is **implemented** and the v2 machinery is being deleted against it.
`A.11.1`–`A.11.6` are done: the elaborator **writes the carrier**, `Bool.fold` **declares its
suspension**, **every plain slot is strict** (§1 rule 1, finally implemented), and the per-definition row
check is **unbounded** with the post-mono `DeclaredPureChecker` deleted as subsumed. Gate: full
`__.test` green, 37/40 examples compiling (`PluginA`/`B`/`C` predate this work), every program
byte-identical in output and class content.

**A.11.7 is BLOCKED, and its own method is what blocked it.** The bridge was traced and then switched
off part by part over the whole gate: it is **not cold** — 43 test failures and 5 further examples
without it, and the failures are silent miscompiles, not diagnostics. **A.11.7-R** records the
part-by-part map and the two previously-closed decisions that have to be reopened before any of the
~933 lines can go. The live plan is **Appendix A.11**; it replaces every earlier plan (§8 and the plans
inside A.9/A.10 are historical). **A.9.4 owns the method** — arm-liveness tracing, the differential
probe, the byte-identity oracle, the tracer gotchas — and is reused by every remaining step.

**All three A.11.7-R open decisions are now closed by §1 rule 4** (2026-07-27, Robert). Rule 4 —
*an effect passes through a position if and only if that position declares it* — is stated in §1, and it
**reverses A.11.2-R** ("build neither mechanism"): `.`, `foldLeft`'s `initial` and `foldOption`'s
`ifNone` declare `{Effect}`. With it, `foldOption` converts and **A.10's cancellation of slice 4b
stands** — the one remaining flex-flex shape is itself a rule-4 violation (§6.1), so the `Unifier` still
gains nothing. §6.1 is the correction inventory; A.11.7-R's candidate rule is explicitly *not* adopted,
because it approximates rule 4 instead of declaring it.

**Standing rule 1 — where decisions live: §§1–7 state the decision; §8 and the appendices record what
happened to it.** An appendix that changes a decision must say so in §§1–7, never amend the rule in
place. (This rule exists because A.8.6 amended §1 rule 1 in place and a *reversal* consequently read as
a refinement for six days — see A.10.)

**Standing rule 2 — stop on conflict, do not route around it.** If §1's rules appear to conflict with
each other, with the tree, or with a measurement — or if a step finds itself *narrowing*, *bounding*,
*deferring*, *approximating*, or *exempting* one of them — **stop work immediately and surface it.** Do
not land the workaround and record it as a corollary. This applies however local the concession looks;
every entry in the rule-4 erosion table below was locally reasonable and each cost more than the rule it
bought out. The tells to stop on, in the vocabulary this project has actually used: "bounded staging",
"the corpus forced", "narrowed to ~nothing", "a small local concession", "keeps the shipped idiom", "it
usually holds". A conflict is a decision for Robert, not a judgement call in flight.

**One-sentence summary.** Suspension is *declared* in signatures instead of inferred from genericity, and
the carrier is *written* by the elaborator instead of solved by the checker; effect elaboration then
becomes a syntax-directed **desugar phase** before checking, effects verify as a **row channel** beside
the type (the same architectural move as the Int-bounds refinement channel), and the NbE checker holds
one local rule and no effect decisions.

Successor to `docs/effects-as-channel.md` (v2), whose remaining checker machinery — the bridge, the
ladders, the obligation path — is live underneath until A.11.7/A.11.8 retire it. The `Id` *erasure* is
not on that list: `Id` is the value of the empty row and stays (§1 rule 4).

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

## 1. The user model (four rules)

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
4. **An effect passes through a position if and only if that position declares it.** *(Rule 4 is the
   invariant the other three rest on. It is stated last because it was found last, and it outranks every
   convenience below it.)*
   - A **plain generic is a payload, always.** `A`, `B`, `T` in `def .[A, B](a: A, f: A => B): B`,
     `def ++[T ~ Combine[T]](left: T, right: T): T`, `def foldLeft[A, B](initial: B, …): B` can never be
     instantiated at a computation. A function that transports effects says so: `f: A => {Effect} B`
     returning `{Effect} B`, `initial: {Effect} B`.
   - A **rowless slot may not receive a computation.** Not a carrier-headed value, not a pinned capture,
     not a value whose declared row is non-empty. This is a hard error naming the slot, never a silent
     re-route.
   - `{Effect}` is a **row variable**, not a fixed carrier. `ρ := {}` is an ordinary instantiation, so
     `dependency.url` (ρ = `{}`) and `items.foreach(x -> printLine(x))` (ρ = `{Console}`) go through the
     *same* declaration of `.`.
   - **`Id` is the value of the empty row, and that is allowed** (decided 2026-07-28, Robert, after the
     A.11.7-S measurement): a row-polymorphic definition instantiated at `ρ := {}` may be written at
     `Id` and erased by monomorphization. Rule 4 constrains *declarations*, not the representation of
     the empty row. **Deleting `Id` was never a decision** — it was a plan item inherited from v2's
     critique of the `Id`-headed *encoding*, which is a different thing: what v2 was faulted for, and
     what A.8.10 already fixed, is the checker **manufacturing** a carrier head on pure judgments so
     slot arms could split unconditionally, then normalizing it away. Written `Id` with an honest type
     is not that, and A.8.12's blocker does not apply to it.

   Everything the elaborator must decide is then decided by declarations, per call, order-free: a call's
   result kind and row are its declared return instantiated from the declared types and rows of the
   arguments given.

Rule 1 is implemented as of A.11.5 (it was withdrawn at bare-generic slots by A.8.6 for six days; A.10
reinstated it and A.11.5b removed the deferral).

**Rule 4 has been agreed and then worked around at least four times. Its erosion is the single cause of
every stall recorded in Appendix A.** Recorded so it cannot read as new:

| # | where | how rule 4 was worked around | cost |
| --- | --- | --- | --- |
| 1 | A.8.6 | bare-generic slots exempted from rule 1 — mode "belongs to the instantiation" | six days; `ModeResolver`, obligations, splice-restart (~350 lines), reversed by A.10 |
| 2 | A.11.2-R | declined `{Effect}` on `foldLeft`/`foldOption` — kept an undeclared effect position for ergonomics | the elaborator cannot hoist at a generic-return callee **at all**; the payload router stays alive (A.11.7-R) |
| 3 | A.11.4c | the derived discharge stack routes a computation through `.`'s **rowless** `A` "as data" | 5 `State`-family miscompiles; makes "a generic is a payload" false, so it cannot be assumed |
| 4 | A.11.5-R | `foldOption` left with a strict `ifNone` because both declared spellings failed | a silent lazy-branch failure mode, still open |

The A.11.7-R candidate rule (*let `declaredPayloadResult` accept a generic head*) is the same move a
fifth time: it **approximates** rule 4 in the elaborator instead of **declaring** it in the signature. It
is not to be adopted in that form.

**Rule 4 outranks the tree.** Where code, a stdlib signature, an example, or a test conflicts with it,
the rule wins and the artefact is wrong. A test that pins a rule-4 violation is a defect in the test, to
be corrected, not evidence about the design. The known conflicts to correct are inventoried in §6.

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
- **The discharge stack is derived** (A.11.4c): `carrier(call) = stack(callee.declaredRow ∖
  ambient.declaredRow) over ambient`. A callee needing more than the ambient provides cannot be running
  on it, so it runs on the canonical stack of that difference — `rename` needing `{State[String]}` under
  a `{Console}` ambient carries `StateCarrier[String, F]` and does not perform on the ambient. This is
  what lets a `val` bind a dischargeable computation as data (§5).
  - **Amended by §1 rule 4 (2026-07-27).** A.11.4c additionally let that computation *pass as data
    through a rowless slot*, which is how a dot-chained discharger kept working without a rule of its
    own. That is erosion #3 in the §1 table and the cause of the 5 `State`-family miscompiles A.11.7-R
    measured: it makes "a plain generic is a payload" false, so nothing downstream may assume it. Under
    rule 4 the derivation above **stays**; delivering the result to a *rowless* slot is a hard error, and
    the affected call sites take the direct call instead (§6.1). A.11.4c's rejected alternatives —
    inlining `.`, and a `.`-only deferral — stay rejected: `.` declares `f: A => {Effect} B` and needs no
    special case in either direction.
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
package (`Carrier`, `CarrierJoin`, `UniformLadder`); `CarrierKindChecker`'s carrier-role seeding; and
inside `Checker` the ladders' effect arms, the Phase A/B deferral with its obligations, the pinning
mechanisms, and the slot routers. Landed so far (A.8.8–A.8.10, A.11.6): the unreachable arms, **every
bind the checker used to insert**, the manufactured `Id` head, and `DeclaredPureChecker` (subsumed
pre-mono). Remaining: A.11.7 (bridge) and A.11.8 (obligations, the carrier side table).

**The `Id` erasure stays** (decided 2026-07-28 — §1 rule 4, third bullet). `IdNormalizer`,
`PostDrainQuoter.stripIdMachinery` and `WovenValueProcessor.assertNoIdResidue` were listed here for
deletion on the assumption that nothing would write `Id` once the checker stopped manufacturing it.
A.11.7-S measured otherwise: `Id` is the value of the empty row, a row-polymorphic definition
instantiated at `ρ := {}` is written at `Id`, and something must erase it before codegen. What was
deleted is the **encoding** (A.8.10: manufacturing a carrier head on pure judgments); what stays is the
erasure of honestly-written `Id`. `assertNoIdResidue` in particular keeps its value — it is the proof
that erasure is complete, and it is *more* load-bearing now that `Id` is written deliberately.

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
- `def foldOption[A, B](ifNone: {Effect} B, …)` — **converts** (rule 4). A.11.5-R left it open because
  both spellings were refuted by the tree; under rule 4 that is a defect in the tree, not evidence about
  the signature, and the refutations are entries in the correction inventory below.

Every laziness-requiring signature must declare suspension; a combinator that forgets it becomes strict.

### 6.1 Rule-4 correction inventory

Rule 4 outranks the tree, so these are **defects to correct**, not constraints on the design. Measured
(A.11.3-R for the dot sites, `grep` over `stdlib/`, `jvm/`, `examples/` for the rest); the list is the
whole known cost.

**Signatures that must declare what they transport** — each currently lets an effect through a rowless
position:

- `def .[A, B](a: A, f: A => {Effect} B): {Effect} B` — the subject stays a plain, strict slot; the
  transported row is declared on `f` and comes back out of the dot.
- `foldLeft`'s `initial: {Effect} B` and `foldOption`'s `ifNone: {Effect} B` — the two A.11.2-R declined.

**Call sites that pass a computation through a rowless slot** — 7 lines, each rewritten to the direct
call, where the callee's own declared slot (pinned, or carrier-typed) already has the right mode:

| site | now | correction |
| --- | --- | --- |
| `examples/src/Blocks.els:33` | `rename("after").runStateToPair("before")` | `runStateToPair("before", rename("after"))` |
| `examples/src/EffectsState.els:22` | `swap("second").runStateToPair("first")` | direct call |
| `stdlib/…/effect/State.els:69,76` | `runStateToPair(initial, p).map(first/second)` | `map(first, runStateToPair(initial, p))` |
| `stdlib/…/effect/Writer.els:54,60` | `runWriterToPair(p).map(first/second)` | direct call |
| `stdlib/…/collection/List.els:26` | `acc.flatMap(_ -> action(e))` | `flatMap(_ -> action(e), acc)` |

Plus two scaladoc examples in `carrier/Effect.els` (`readLine.flatMap(…)`, `readLine.map(…)`) that teach
the disallowed form. The infix dischargers are **not** affected: `catch` (`Throw.els:77 infix left`) and
`else` (`Abort.els:76 infix right`) resolve straight to a direct call with the computation at its pinned
slot, so `x catch (e -> …)` and `host else "localhost"` are untouched.

**Declarations storing an open carrier**: `jvm/test/…/TerminationIntegrationTest.scala:206`'s
`data Box[F[_]](action: F[Unit])` — already illegal under rule 3 (a stored row must be pinned), and the
last source of the flex-flex `?F[X] ~ ?G[Y]` that A.11.7-R found still alive. Correct the shape; do not
reopen A.10's cancellation of slice 4b to accommodate it.

**Tests pinning a rule-4 violation are wrong and get corrected** — known: three `RowElaboratorTest` twins
(including the one named "defer a call with a generic-headed return — A.8.6"), `MonomorphicTypeCheckTest`'s
"pass an effectful eliminator branch through unsequenced", and the 5 `State`-family shapes A.11.7-R
measured, which fail *because* of erosion 3 in the §1 table.

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
- **Switch it off, part by part** (A.11.7-R). Where the question is the blunter "is this load-bearing at
  all?", an env-gated bypass is cheaper *and* stronger evidence than a differential probe: it answers in
  behaviour rather than in meta solutions. Gate each part **separately**, never only all-at-once — the
  all-off run said "43 failures, delete nothing", while the per-part runs said the carrier router costs
  1 test and the payload router 36, which is the whole finding. Cost is one gate + one examples sweep
  per part (~6 min each).
- **A firing arm is a question, not a verdict.** Having localized a live part, do not stop at "it is
  needed": find the *one* arm that decides, and ask what the elaborator would have had to know. That is
  what turned "the payload router is load-bearing" into "`declaredPayloadResult` refuses a generic head".
- **Measure twice, before and after** (slice 3's first cut looked like an improvement while dropping a
  fail-safe).
- **The byte-identity oracle.** Build a baseline in a `git worktree` at the pre-change commit, compile
  all examples in both trees, and compare (1) **program output** and (2) **class content unzipped from
  the jars** — jars themselves are not byte-reproducible (timestamps), and `$row$N` binder numbering can
  legitimately rename lambda classes. Byte-identity is a safety oracle, not a hard gate. For repeated
  sweeps a worktree is not needed: drive `Main` directly off `./mill show examples.runClasspath` with the
  three `--path` layer roots, and store per-example `md5sum` of every unzipped class under a label, so
  any two labels diff in one loop. **`Concat`, `Effects`, `EffectsMulti` and `IfDemo` read stdin**, so
  their *output* differs between a run with a tty and one without (a timeout versus `readLine`
  returning null) with identical bytecode — for those four, class content is the only sound comparison.
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

**Done: A.11.1 – A.11.6. A.11.7 is measured and BLOCKED — see A.11.7-R.**

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
| ~~the `Id` apparatus~~ | **CANCELLED 2026-07-28** (§1 rule 4, third bullet; A.11.7-S). `channel/IdNormalizer` 308, `PostDrainQuoter.stripIdMachinery` and `WovenValueProcessor.assertNoIdResidue` **stay**: `Id` is the value of the empty row and is written deliberately, so its erasure is required, and `assertNoIdResidue` is the proof that erasure is complete | 0 |
| the carrier side table | `Unifier.carrierRoles` / `isEffectCarrier` / `CarrierRole`; `CarrierKindChecker`'s carrier-role *seeding* (its kind checking stays); `EffectLifter`'s remainder beyond the one pure-lift arm and the node builders | ~400 |
| ~~A.8.6's uncertainty~~ | **DONE at A.11.6**: `uncertain`/`Derivation.deferred` and `DeclaredPureChecker` deleted; two boundings remain by design (coverage, decidability), and `capturedByStack` was added to the derivation | −150 |
| flags & experiment scaffolding | `CompilationSession.compileOnce(seedFacts)` (a production API added only for the R4 shadow compile); `jvm/test/.../RowElaborationShadowCompileTest`; the shadow half of `jvm/test/.../RowShadowSweepTest` | ~750 |

**Arithmetic to hold the work to — RESTATED 2026-07-28**, because cancelling the `Id` group changes it
and A.11.7-S said to restate it rather than let the old number stand. `check/` was 5,219 at the
stock-take and is **5,097** after A.11.6; the remaining deletions (bridge ~933, obligations ~350, carrier
side table ~400, less what is not in `check/`) remove ≈950 more, landing `check/` at **≈4,150** against
the pre-v2 baseline of 3,996 — unchanged, since the `Id` apparatus never lived in `check/`. The
**machinery total** does change: 6,895 → **≈5,540**, not ≈5,200, because `IdNormalizer` and its two
helpers (~340) stay. That is still below the pre-v3 5,585, and above pre-v2 3,996; the difference is
`row/`, a phase that did not exist. **Do not claim a net reduction against pre-v2**, and do not quote the
≈5,200 figure — it assumed a deletion that is now cancelled.

**Exit criteria, all mechanically checkable:**

- `grep -rin "uniformCarrier\|CarrierJoin\|UniformLadder\|ModeObligation\|seedFacts" lang/src jvm/src eliotc/src` → empty.
  (`IdNormalizer` was in this list until 2026-07-28 and is **not** an exit criterion — see §1 rule 4.)
- No `lang/src/.../monomorphize/carrier/`.
- No env-var, system-property, CLI or constructor gate anywhere in the effect path — one code path only.
- `check/` at or below 3,996 lines. (The machinery total lands at ≈5,540, not ≈5,200.)
- Full gate green (871 test targets across lang/jvm/eliotc/LSP is the current baseline), and **37 of 40
  examples compile** (`IfDemo` included — the bridge causes its failure;
  it already compiles as of A.11.4).
- `docs/effects-as-channel.md` retired; the CLAUDE.md cornerstone describes rows and written carriers.

## A.11.1–A.11.3 Done: scaffolding, the accumulator decision, the corpus and the audit

- **A.11.1** deleted the dead v2 spike (`monomorphize/spike/UniformCarrierSpike` + test, 622 lines) and
  four stale scaladoc references to a `uniformCarrier` gate removed weeks earlier.
- **A.11.2-R — REVERSED by §1 rule 4 (2026-07-27).** It is erosion #2 in the §1 table: declining to
  declare kept an *undeclared* effect position alive, and the cost was not the accumulator's ergonomics
  but the elaborator's inability to hoist at a generic-return callee at all — hence the payload router,
  hence the obligation path (A.11.7-R). `foldLeft`'s `initial` and `foldOption`'s `ifNone` declare
  `{Effect}`; `pure(0)` is *not* the user's fix, since that is itself a computation at a rowless slot.
  The original reasoning is kept below because its decider was true and is instructive: a true premise
  ("the slot occurs once") does not license exempting a rule.
  **A.11.2-R as recorded on 2026-07-27, superseded:** An effectful `combine` makes
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

**This ran, and it returned the stop-and-redecide: the group is not cold. See A.11.7-R** for the
part-by-part measurement and the two decisions that have to be reopened first. Nothing below this
paragraph has been executed.

Keep exactly one thing: the pure-lift rule (the default ladder's existing pure-wrap arm against a
**rigid** expected type) plus the `pureWrapNode`/`runIdNode` builders, which move to `row/` as the
elaborator's node constructors. Note A.11.5a left one live bridge behaviour to preserve or relocate: a
payload slot declaring `Id[T]` takes it as data.

## A.11.7-R The bridge is not cold — the measurement, and what it blocks on

A.11.7 said to trace first and delete only zero-fire arms, and that **an arm that still fires is a
missing elaborator rule, i.e. a stop-and-redecide signal**. The trace was run, and then a stronger
experiment: switch the bridge off, part by part, over the whole gate (871 targets) *and* all 40
examples. Everything below is measured; the instrument was env-gated and has been reverted, and the
tree is back to a green gate with class content byte-identical to the pre-experiment build.

**Entry counts are not the measurement.** With everything on, the routers fire constantly — 4,833
payload-domain routes, 1,542 carrier-domain, 5,123 uniform return boundaries — but almost all of that
is *routing*: the whole gate's node-producing outcomes total ≈260 (`return/pureIntoCarrier` 213,
`PureLift@CarrierSlot` 38, `carrierSlot/pureWrap` 6, `return/dischargeToPure` 4, `PayloadUnwrap` 1,
`nullaryMismatchRunId` 1). This is the same trap A.8.9 named; the deciding question is not how often an
arm runs but whether the default ladder would reach the same result.

**The part-by-part map** (test failures out of a 0-failure baseline; examples out of 37/40):

| bridge part switched off | test failures | examples |
| --- | ---: | --- |
| *baseline — everything on* | 0 | 37/40 |
| the carrier-domain router (`uniformCarrierSlot`) | 1 | 37/40 |
| the uniform return boundary | 8 | 36/40 (−`Concat`) |
| the capture join + eager row pin | 15 | 36/40 (−`EffectsThrow`) |
| the payload-domain router (`uniformPayloadSlot`) | 36 | 32/40 |
| all four | 43 | 32/40 |

**What each live part is actually doing.**

1. **The payload router is the load-bearing one, and its live job is a single arm**:
   `payloadSlot/suspendHoist` — a carrier-headed actual at a plain payload slot *suspends* so the
   post-drain `ModeResolver` can hoist it. Those are the sites the A.11.3-R audit counted as "43
   genuinely strict, where hoisting is ALREADY the behaviour" (`.` 22, `==` 8, `++`, `show`,
   `putState`, …) — but the audit did not ask *who* hoists them, and the answer is the checker, not the
   elaborator. `RowElaborator` declines: hoisting needs a classifiable core, and `declaredPayloadResult`
   returns false for a return headed by one of the callee's own binders, which is exactly `.`'s `B`,
   `++`'s `A`, `identity`'s `A`. Without the router the effect is *dropped silently* —
   `printLine(dependency.url)` prints `null`, `EffectsMulti` prints `null` — not diagnosed.
2. **The carrier router** is live for exactly one shape: an effectful actual into a data constructor's
   carrier-typed field (`Box(forever(printLine("boxed")))`). Plain unification cannot decompose the
   flex-flex application `?F[Unit] ~ ?F'[Unit]` and postpones, so the loop never runs. That is
   **A.8.11 rule 1 (carrier decomposition)**, alive and load-bearing.
3. **The capture join + eager row pin** is `catch`/`provide`/`runThrow` against a guarded instance —
   without it, "No ability implementation found for ability 'Throw' with type arguments [String, IO]",
   and LSP hover renders `{Throw[IO[String]] | IO} String`. Rule 1 again, plus the row-directed pin.
4. **The return boundary** costs 8 tests and `Concat`.

**The candidate elaborator rule was built and measured, not just proposed.** Two lines, both in
`RowElaborator`: `declaredPayloadResult` accepts a `ParameterReference` head — a call whose declared
return is one of its own *value* binders is a payload, which is what strictness makes true, since every
plain-slot argument is a payload after hoisting — and `definitelyPure`'s `ValueReference` arm gains the
guard `!carrierHeaded(remaining, declaredCarrierBinders(orv))`, without which a `printLine` call reads
as pure. It works as intended: `++` flips to `elab/hoist=true` and `Concat`'s `payloadSlot/suspendHoist`
leaves the trace entirely. With it, payload-router-off improves from 5 regressed examples to 2
(`EffectsThrow`, `IfDemo`), and the examples stay 37/40 with the router on.

**But the rule as spelled costs 16 test failures.** They are three groups, and only one is a real
defect:

- **7 — one cascade.** A single compile inside `FullIntegrationTest`'s shared session fails, and every
  later test in that session dies with `ClassNotFoundException: main` ("type as its payload and run as
  the bare type" ×4, "fail the build with the author message" ×3). One root cause, not seven.
- **4 — suites pinning the spelling the rule replaces**: three `RowElaboratorTest` twins (the
  generic-eliminator shapes, incl. the one literally named "defer a call with a generic-headed return —
  A.8.6") and `MonomorphicTypeCheckTest`'s "pass an effectful eliminator branch through unsequenced".
  These assert the deferral; if the rule lands they are rewritten, not fixed.
- **5 — genuine `State`-family miscompiles** (state threading, `runStateToValue`, the whole-list
  `State` instance). This is the real cost and the thing to understand before adopting anything.

So the rule is the right area and is not yet the right rule.

**What this means for the plan.**

- **The step ordering in A.11.0/A.11.8 is not achievable**: the bridge *routes into* the obligation path
  (`payloadSlot/suspendHoist`, `capture/doomedSuspend`). The bridge and the obligation path are one
  mechanism and are one deletion, not two.
- **Two closed decisions have to be reopened before any of it can go**, and both are reversals:
  - **A.11.2-R, "build neither mechanism."** Its decider was that `foldLeft`'s `initial` occurs exactly
    once in the corpus, so no library code needs the inference. That is still true and is beside the
    point: the mechanism is needed not for the accumulator but for the elaborator to hoist at a
    generic-return callee at all — the `.`/`++`/`identity` family, 43 sites the audit already counted.
  - **A.10's cancellation of slice 4b.** It was cancelled because a written carrier makes every carrier
    position rigid, so flex-flex `?F[X] ~ ?G[Y]` would not arise. Measured: it still arises — a data
    constructor's `F[_]` field and a pinned capture both produce it. Carrier decomposition needs a home
    (A.9.2's open `unify`-guardrail question, verbatim).
- **The pattern is A.10's, one level down.** A.10 found that A.8.6's local concession was what kept the
  carrier inferred and the whole v2 machinery alive. Here A.11.2-R's "neither mechanism" is what keeps
  the *hoist* in the checker, and therefore the payload router, and therefore the obligation path. The
  cost is again paid downstream rather than at the decision.

**State of the tree at this handover.** No compiler code changed. The instrument — an env-gated
`ArmTrace` object, a four-part `ELIOT_NO_BRIDGE` switch threaded through `uniformReturnRoutable` /
`routeArgumentSlot` / `uniformCaptureSlot` / `eagerRowPinIntoDomain`, and the `ELIOT_GENERIC_PAYLOAD`
probe — is **reverted**; there is no gate anywhere in the effect path, as A.11.0's exit criteria
require. Gate re-verified after the revert: `./mill __.test` **0 failures**, **37/40** examples
(`PluginA`/`B`/`C` predate A.11.4), and **class content byte-identical** for all 37 against the
pre-experiment build. The measured line counts are unchanged from A.11.6 (`check/` 5,097).

**RESOLVED by §1 rule 4 (2026-07-27, Robert), which supersedes the three branches below.** Both
reopenings A.11.7-R asked for are answered at once, and neither by the mechanism it proposed:
**A.11.2-R is reversed** (`.`, `foldLeft`, `foldOption` declare `{Effect}`, so a generic-return callee is
a declared payload and the elaborator hoists — the candidate two-line rule is *not* adopted, since it
approximates rule 4 rather than declaring it); and **A.10's cancellation of 4b stands**, because the one
flex-flex shape still alive is `data Box[F[_]]`, itself a rule-4/rule-3 violation to correct (§6.1). The
5 `State`-family failures the candidate rule cost are not evidence against it — they are erosion #3
(A.11.4c) failing, exactly as §1 predicts. Branches 1–3 below are kept as the measurement that led here.

**Where to resume, per branch of the decision (historical).**

1. *If A.11.2-R is reopened and the elaborator takes the hoist*: start from the two-line candidate
   above, and start by understanding the 5 `State` miscompiles — that group, not the pinning suites, is
   what says whether "a generic-headed return is a payload" is true or merely usually true. The `.`,
   `==`, `++`, `show`, `putState` sites from A.11.3-R are the acceptance corpus; `payloadSlot/
   suspendHoist` reaching zero over the whole gate is the mechanical exit test. Only then does the
   payload router — and with it the obligation path, in one deletion — become dead.
2. *If A.10's 4b is reopened*: the scope is much smaller than the ~930 lines it was originally costed
   at, because only two shapes now need it — a data constructor's `F[_]` field and a pinned capture.
   The two rules are still exactly A.8.11's: carrier decomposition of a flex-flex application, and `Id`
   as bottom (never *commit* a flex carrier meta). A.9.2's arguments on where they live stand unamended.
3. *If neither is reopened*: A.11.7–A.11.8 do not proceed, and A.11.0's arithmetic (`check/` → ≈4,150)
   is not reachable. Say so explicitly rather than letting the roadmap read as merely unfinished.

## A.11.7-S The `.` spike: rule 4 is structurally fine, and `ρ := {}` is a forced decision

Run 2026-07-28, one line of `.els` changed, reverted; tree restored (37/40 examples, class content
byte-identical to the pre-spike build, 0 diffs). Baselines measured first, not assumed: **871/871 tests,
37/40 examples** (`PluginA`/`B`/`C`).

**The change**: `infix left below apply def .[A, B](a: A, f: A => {Effect} B): {Effect} B = f(a)` plus
`import eliot.carrier.Effect` in `Function.els`. Nothing else.

**Result 1 — `.` carries the row structurally.** It parses, desugars (`{Effect}` mints `F[_] ~ Effect` at
binder 0, so `writeCarrier`'s first-binder condition is met), its own body still type-checks, and 23/40
examples still compile, effectful ones among them. There is no architectural rejection: `.` is an
ordinary row-polymorphic signature. This is a materially easier conversion than `fold`'s at A.11.5 — one
import, one line, and no interaction with type-level evaluation (measured: `.` has 50 value-level uses
across 20 library `.els` files and **zero** in `where` guards or type positions).

**Result 2 — a syntax defect, independent of this work.** `Function[A, {Effect} B]` does *not* parse as a
row: inside a bracketed type argument the `{…}` reads as a **block expression** and the compiler dies with
`IllegalStateException: BlockExpression should not exist after block desugaring` while resolving
`eliot.lang.Function..` — an internal error, not a diagnostic. The `=>` spelling (`A => {Effect} B`, what
`foreach` and `foreachLine` already use) works. The row sugar is position-restricted and fails loudly but
wrongly; worth a real diagnostic regardless of this plan.

**Result 3 — every remaining failure is one cause: `ρ := {}`.** 14 regressed examples and 40 regressed
tests, and all of them have the same signature — the elaborator writes `.[F]` (the ambient) at every dot
inside an effectful region, so `.`'s `f: A => F[B]` slot demands a carrier-returning function and a
**pure** one does not fit:

```
Expected: Database -> {Dep[Database], Dep[Topic] | IO} String     (EffectsTwoDeps:10)
Actual:   Database -> String
Expected: List(Int) -> Int                                        (PluginRegistry:16 — the same thing
Actual:   List(Int) -> Int                                         with `Id` erased by the renderer)
```

Distribution: **12 of the 14 examples are pure dots**; the other 2 (`Blocks:33`, `EffectsState:22`) are
the dot-discharge sites §6.1 already lists for correction. All 40 test failures are in `jvm.test` — the
suites that use the real stdlib; `lang` (816-\*) and `eliotc`/LSP (871-\*) stayed fully green, though
that signal is weak, since their universes stub `.` rather than reading `Function.els`.

**What the spike did *not* show.** The prediction on record was that the 22 `.` sites would leave the
deferral trace and `payloadSlot/suspendHoist` would fall. That was **not** observed and could not be:
`ρ := {}` fails earlier. The prediction is neither confirmed nor refuted — it is untestable until the
decision below is made.

**The forced decision (standing rule 2 — surfaced, not taken).** §1 rule 4's third bullet says `ρ := {}`
must produce no node. With `.` carrying the row, a pure dot leaves exactly two possibilities, and there
is no third:

1. **Beta-reduce at `ρ := {}`** — emit `f(a)`, never call `.`. Satisfies rule 4 exactly. But it is
   available only where the callee's body *is* the application; a genuinely row-polymorphic library
   function (`foreach` called with a pure action) cannot be beta-reduced and its own `pure`/`flatMap`
   land at `G := Id` regardless.
2. **Allow `Id` at `ρ := {}`** and let monomorphization erase it — i.e. rule 4's third bullet narrows to
   *"no `Id` node at a call site the elaborator writes"*, and A.11.8's "delete the `Id` apparatus"
   narrows to *"delete the checker-manufactured `Id` encoding"*, keeping written `Id` and its erasure.

Because of the `foreach` case, (1) does not remove the need for (2); it only removes it at `.`.

**DECIDED 2026-07-28, Robert: option (2) — allow `Id` at `ρ := {}` and let monomorphization erase it.**
With the clarification that settles the framing: *deleting `Id` was never a decision at all.* It entered
the plan as an inference from v2's critique of the `Id`-headed **encoding** — the checker manufacturing a
carrier head on pure judgments so slot arms could split unconditionally — which A.8.10 already removed.
Written `Id` with an honest type is a different thing and is allowed wherever it helps the
implementation. §1 rule 4's third bullet is restated accordingly (it constrains declarations, not the
representation of the empty row), the `Id` group is struck from A.11.0's deletion table, `IdNormalizer`
is removed from the exit criteria, and A.11.0's arithmetic is restated: machinery lands at **≈5,540**,
not ≈5,200; `check/` is unaffected at ≈4,150, since the `Id` apparatus never lived there.

## A.11.8 Delete the obligation path and the carrier side table

In this order, each possible only after A.11.7 (with which step 1 is **one deletion**, not two — the
bridge routes into the obligation path; A.11.7-R):

1. **Obligations** — `ModeResolver`, `CheckState`'s obligation vectors, the `Deferred`/`Suspended`
   outcomes, `resolveDeferredSlot`, `TypeStackLoop`'s splice-and-restart and fuel, `processIO`'s `Either`
   return, and `RowElaborator.spliceResolvedModes`. `TypeStackLoop` returns to a plain post-drain
   fixpoint.
2. ~~**`Id`**~~ — **cancelled 2026-07-28** (§1 rule 4). `IdNormalizer`, `stripIdMachinery` and
   `assertNoIdResidue` **stay**: `Id` is the value of the empty row, written deliberately by the
   elaborator, so something must erase it before codegen and `assertNoIdResidue` is the proof that the
   erasure is complete. `Id` remains ordinary `data` with no `Suspend[Id]` — the soundness guard is
   unchanged. What was deleted, at A.8.10, is the checker-*manufactured* `Id` head; that is the thing v2
   was faulted for, and it is already gone.
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
  `check/CarrierBookkeepingTest`, `check/EffectLifterTest`, `unify/CarrierRoleTest`.
  **`channel/IdNormalizerTest` stays** — its machinery does (§1 rule 4); if anything it needs *adding* to,
  since `Id` is now written on purpose rather than manufactured.
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
