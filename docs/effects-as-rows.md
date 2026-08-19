# Effects as Rows, v3: Declared Suspension + a Written Carrier

**Status (2026-07-28): the design is LANDED and this document is its closeout.** Every step of the A.11
roadmap is done, including A.11.10 (this pass). The elaborator **writes the carrier**, **every plain slot
is strict** (§1 rule 1), suspension is **declared** (rule 2), and **§1 rule 4 holds and is enforced** —
every position classifies from its declaration, a computation may not reach a rowless slot, and `ρ := {}`
runs at `Id`. The v2 machinery is deleted, not disabled: no bridge, no carrier join, no obligation path,
no carrier side table, no experiment scaffolding. **`check/` is 3,873 lines, below the pre-v2 baseline of
3,996** — A.11.0's arithmetic target, met. The per-definition row check is unbounded and
`DeclaredPureChecker` is deleted as subsumed.

**Gate at closeout**: `__.test` green (1,487 tests), 37/40 examples — `IfDemo` compiles again *because*
the bridge is gone, and its output was verified at A.11.10; `PluginA`/`B`/`C` predate this work and are
unrelated. Every program is unchanged in output and class content against the pre-work baseline.

**What is still open**: `PluginA`/`B`/`C`, never diagnosed, failing before this work started. (The
pinned-`data`-field gap the closeout carried out of A.11.7-Y was **fixed** immediately after — A.11.11.)

**How to read this document.** §§1–7 state the design and are self-contained; §8 and Appendix A are the
record of what happened to it, in chronological order, and are history — read them for *why* a rule has
the shape it has, or for the method (A.9.4), never for the current rule. A.11 is the roadmap that landed
the design; A.11.Z was its handover.

**How the decisions closed.** A.11.7 stopped because the bridge was measured *not cold* (A.11.7-R:
43 test failures and 5 further examples without it, as silent miscompiles). Its three open decisions are
all now closed, none by judgement in flight:

- **§1 rule 4** (2026-07-27, Robert) — *an effect passes through a position if and only if that position
  declares it* — reverses **A.11.2-R** so `.`, `foldLeft`'s `initial` and `foldOption` declare `{Effect}`,
  and leaves **A.10's cancellation of slice 4b standing**, since the one remaining flex-flex shape is
  itself a rule-4 violation (§6.1). A.11.7-R's candidate rule is explicitly *not* adopted: it
  approximates rule 4 instead of declaring it.
- **`ρ := {}`** (2026-07-28, Robert, after the A.11.7-S spike) — `Id` is the value of the empty row and
  stays; deleting it was never a decision, only an inference from v2's critique of the `Id`-headed
  *encoding*, which A.8.10 already removed.
- **`foldOption`** — closed by measurement, not decision (A.11.7-U): it converts, and A.11.5-R's
  surviving refutation is A.11.7-T step 1, not a missing rule.

§6.1 is the correction inventory the rule outranked: 4 signatures, 7 call sites, one illegal `data` shape,
and the tests that pinned the violations — **all cleared** (A.11.7-T step 3, A.11.7-X). §6.1-A is the
instrumented conformance audit that sized the gap at 28 shapes and is kept because its predicate *became*
the enforcement, so a green gate is that audit reporting zero.

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

Successor to `docs/effects-as-channel.md` (v2), whose checker machinery — the bridge, the ladders, the
obligation path, the carrier side table — A.11.7/A.11.8 deleted; that document is now a retired signpost.
The `Id` *erasure* was never on the deletion list: `Id` is the value of the empty row and stays (§1
rule 4).

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
2. **Suspension is declared, and a row is not a carrier.** A parameter that must *not* run its argument
   declares an open row: `whenTrue: {G} A` receives the computation unrun. `if[T](c: Bool, value:
   {Abort} T)` already spells this — v3 makes the syntax mean what it looks like it means.

   A **row** position means *"a value or a computation"*, because the empty row is a legal row: a pure
   argument fits (`{} ⊆` anything) and is lifted. A **carrier-typed** position — `x: G[A]`, `IO[A]`, a
   pinned stack — means *"a computation on this carrier"*, and a plain `A` is simply not one of its
   values: a type error, never a lift. *(Decided 2026-07-28, Robert. Until then every `~ Effect`-
   constrained carrier slot lifted a pure actual, which made the **calling convention depend on a
   constraint the callee declares for its own body's sake**: adding `~ Effect` to `def hold[G[_]](x:
   G[String])` silently changed what callers could pass, and without it the same call died in the quoter
   with "contains unresolved variable" rather than reporting a mismatch. That is exactly the
   "mode falls out of how generic the callee happens to be" implicitness §0 indicts.)*

   The two are **the same shape** — an open row desugars to `F[A]` — so the distinction is read from the
   **row tag** the desugar records (`EffectRow.parameterEffects`, source (i)), never from the type. This
   is the same tag-not-shape discipline rule 4's capture rule uses.

   **`{}` denotes the signature's own carrier — "on my own ambient carrier, nothing added".** This is the
   empty row, and it is the *written* spelling of the position above: `fallback: {} A`, `whenTrue: {} A`,
   `f: A => {} B`. When a definition already binds exactly one `Effect`-constrained carrier (`G[_] ~
   Effect`, as every discharger does), its rows reuse *that* binder instead of minting a second, and the
   row's entries join its constraints. So `fallback: {} A` in `else` **is** `G[A]`, now carrying the row
   tag: the same type, saying "a value or a computation". Two or more such binders are ambiguous and mint
   as before. Without this the rule above would have forced `host else pure("localhost")` on every
   discharge, pushing `eliot.carrier` — machinery the language deliberately hides — into user code.

   `{}` is what the tree writes since effects-v5 step 1 (`docs/effects-v5-one-carrier.md` §4). The older
   spelling `{Effect}` names that machinery ability explicitly and is exactly what `{}` desugars *into*
   — same carrier, same `F ~ Effect` constraint, same row tag — so the two are interchangeable and both
   parse; `{}` is preferred because a definition saying "on my own carrier" should not have to name, or
   import, the machinery. The synthesized constraint resolves at its fixed FQN (`eliot.carrier.Effect`),
   so writing `{}` needs no import.
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
   - `{}` (formerly `{Effect}`) is a **row variable**, not a fixed carrier. `ρ := {}` is an ordinary instantiation, so
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

   - A **carrier-headed slot captures**, and that is the *whole* rule for how a slot is filled *(decided
     2026-07-28, Robert — the unified reading of A.11.7-Y)*. A slot whose declared type is headed by a
     carrier holds the computation, however that carrier is named: a pinned row's stack, one of the
     callee's own carrier binders, the concrete `Id`, or a platform run carrier (`data
     Box(action: IO[Unit])`). A slot that is *rowless* is strict, per the bullet above. There is no third
     kind of slot and no name-keyed exemption — the four namings are one predicate, not four arms.

   Everything the elaborator must decide is then decided by declarations, per call, order-free: a call's
   result kind and row are its declared return instantiated from the declared types and rows of the
   arguments given, and **every type argument a declaration determines is written down** (§3.1).

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

The pre-mono check is bounded exactly three times, by what declarations genuinely cannot settle (A.11.6,
extended once): **coverage** — an unknown callee may leave the derivation incomplete; **decidability** — a
definition declaring no ambient whose declared return could *itself* be the carrier (an applied
`Box[String]`, `IO[Unit]`, or a generic-headed return) is the constructor-class shape, settled only by the
instantiation; and, per row entry, a contribution delivered to a slot that **fixes a foreign concrete
carrier** (`RowChecker.fixesCarrier`) — a carrier-generic callee handed to a `Recorded[A]` slot performs
in *that* carrier, not on this definition's ambient, which is the whole of the fake-carrier testing
strategy (`docs/testing-effects.md` L2). All three defer to accounting's ride test, which decides them
exactly. Everything else is enforced, including a pure-returning definition that performs an effect (the
diagnostic the deleted `DeclaredPureChecker` used to voice post-mono, now earlier and naming the effect).

## 3. Elaboration: a desugar, not a checker mode

A phase (`row/RowElaborator`, run by `RowElaborationProcessor` between the recursion gate and saturation)
rewrites each definition into **fully explicit monadic core Eliot** — the same shape v2's checker
*output* had, so monomorphization, ability resolution, `used`/`uncurry`, the jvm backend, `runMain` and
the synthetic main are unchanged consumers:

- A definition with declared row `{Console} Unit` gets its carrier binder (`F[_] ~ Console`) from the
  existing `EffectSugarDesugarer`.
- Every effectful call in a strict position becomes a `flatMap` chain (`$row$N` binders); `val x =
  <effectful>` binds.
- A suspended-slot argument passes as its carrier-typed computation, unrun; a **carrier-headed** slot —
  pinned row, callee carrier binder, `Id`, or a platform run carrier — captures its argument whole (§1
  rule 4).
- **Pure code is untouched.** A definition with an empty row and no discharge elaborates to itself — no
  wrapper, nothing to erase. The one exception is a pure definition that *discharges*: its region is
  written at `Id`, with `runId` beside it (§3.1, last paragraph), which the `IdNormalizer` then erases.

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
- **The elaborator writes every type argument a declaration determines**, as a leading prefix of the
  callee's binders. Two sources determine one today: the **region** supplies the carrier, and a **pinned
  parameter** supplies its row's ability arguments, instantiated from the captured argument's own
  declared row (`catch`'s `{Throw[E] | G} A` against `bad`'s declared `{Throw[String]}` gives
  `E := String`). Writing stops at the first binder nothing determines — `typeArgs` applies positionally,
  so a prefix is all that can be written, and a binder the declarations leave open stays inferred.
  *(Decided 2026-07-28, Robert. Until then only the carrier was written, and only when it was binder 0;
  that restriction is why `catch[E, G[_] ~ Effect, A]` kept everything inferred and why its `E` had to be
  pinned inside the checker instead — A.11.7-Y shape 3.)*
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
run-boundary registry, one level of type-alias expansion inside those signatures, and **the derived row of
an argument at a position whose classification that argument settles**. A decision that
cannot be made from the whitelist is a **design gap to close in the declarations** — never approximated
by a new syntactic rule. In particular, a rule that inspects a *sibling argument's expression shape* is
prohibited: that is inference, not desugaring. (An elaborator-local join over the callee's *declared*
parameter shapes is inside the whitelist; a sibling-expression rule is not.)

**The argument-row clause** (2026-07-29, Robert, entering A′) makes explicit what the list had left
unsaid rather than forbidden, and the boundary it keeps is the one that was always the point. Deriving the
row of *this* position is not a sibling rule: the derivation itself reads only declarations
(`RowChecker` walks a callee's declared row and pinned metadata), and the position is one the callee's own
signature nominated by mentioning a carrier binder there. The elaborator had in fact depended on this
since R5 — `performs`, the hoisting test, is exactly this derivation — so the clause regularises a
dependency rather than opening one. What stays prohibited is unchanged: consulting a *different* argument
to decide this one.

The rule it unblocked is §1 rule 4's third bullet: `ρ := {}` is settled by what the determining positions
*do*, not by what kind of value they hold. Both readings are needed and neither subsumes the other — a
slot filled by something that already is a computation is caught by the kind (its row derives empty,
because the row variable `{Effect}` is machinery and names nothing), and a payload whose *evaluation*
performs is caught by the row. The conjunction can only withhold `ρ := {}`, never grant it, which is the
fail-safe direction: a withheld empty row costs a `pure` wrap, a wrongly granted one puts an effect on a
carrier that cannot perform it.

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
package (`Carrier`, `CarrierJoin`, `UniformLadder`); the carrier *role* record; and inside `Checker` the
ladders' effect arms, the Phase A/B deferral with its obligations, the pinning mechanisms, and the slot
routers. **All of it has landed** (A.8.8–A.8.10, A.11.6, A.11.7, A.11.8): the unreachable arms, **every
bind the checker used to insert**, the manufactured `Id` head, `DeclaredPureChecker` (subsumed pre-mono),
the whole bridge, the obligation path, and the carrier role — which A.11.8-3 found was never a fact of its
own, only the higher-kinded *kind* record under another name. What stays of that record is the kind
seeding and its post-drain verification (as planned), plus its two compile-track readers, since the inline
guard's carrier is still inferred there (§8).

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
`EffectRowRendering`/`GroundValueRenderer`; the platform run boundaries (A.11.9 retired the
`RunBoundaryFunction` *fact*, whose last demander went with the bridge — what stays is the plugin
configuration key, read by the row phase alone: `row/RunBoundaryFunctions`); the `Inf` story; the
compile-track `Either` discharge; `Id` as ordinary `data` with no `Suspend[Id]`.

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
- `if[T]` — **textually unchanged** (`value: {Abort} T` now *means* suspended); `foldPair` takes a lambda
  whose codomain the audit never saw at a computation.
- **`Abort.else` and `Throw.catch` — their recovery slots became rows** (2026-07-28, §1 rule 2):
  `fallback: {Effect} A` and `onError: E => {Effect} A`, over the unchanged `G[_] ~ Effect` binder they
  already bound. The *types* are unchanged — `{Effect}` denotes that same `G` — so the emitted code and
  every call site are unchanged; what changed is that the slots now *declare* that they accept a value as
  well as a computation, which is what keeps `host else "localhost"` and `bad catch (e -> "fallback")`
  working once a bare `G[A]` stops lifting. This was previously listed as "already declared-suspended",
  which was true of the elaborator's behaviour and not of the declaration.
- `def foldOption[A, B](ifNone: {Effect} B, …)` — **converts** (rule 4). A.11.5-R left it open because
  both spellings were refuted by the tree; under rule 4 that is a defect in the tree, not evidence about
  the signature, and the refutations are entries in the correction inventory below.
- `foldEither` — **converts too** (`onLeft: E => {Effect} B`, `onRight: A => {Effect} B`, return
  `{Effect} B`). The line below excusing it as "lazy by construction" was wrong: rule 4 is about what a
  position *declares*, not about when it runs, and the conformance audit (§6.1-A) found `Throw`/`Abort`
  handing it handlers that return computations.
- `foldLeft` needs **both** its `initial` and its accumulator rowed (`combine: A => B => {Effect} B`) —
  measured at A.11.7-T step 1, where `initial` alone leaves `foreach`'s `acc` an undeclared computation.
  Its jvm primitive `foldLeftInternal` then spells the carrier with an explicit `[F[_] ~ Effect]` binder
  (`initial: F[B]`, `combine: A => F[B] => F[B]`), which the `{Effect}` sugar cannot express in an arrow
  domain; the emitted native is unchanged, being generic and erased.

Every laziness-requiring signature must declare suspension; a combinator that forgets it becomes strict.

### 6.1 Rule-4 correction inventory

Rule 4 outranks the tree, so these are **defects to correct**, not constraints on the design.

**Conformance: the rule HOLDS as of 2026-07-28** (A.11.7-T steps 1–3). Every position classifies from its
declaration, the four signatures below declare what they transport, delivering a computation to a rowless
slot is a hard error, and the §6.1-A audit's 28 shapes are all gone — measured by the enforcement itself,
which is that audit's predicate turned into a diagnostic. The second half of the rule — *and only into* —
was already true: the elaborator writes a carrier only where a declaration puts one (A.11.4), and both
verifiers check `derived ⊆ declared`.

The original inventory below was assembled by `grep` and by A.11.3-R's deferral audit, and claimed to be
"the whole known cost". **§6.1-A supersedes that claim**: an instrumented sweep found two things it
missed — `foldEither`, and the fact that converting `.`'s `f` slot leaves a *computation subject*
(`p.runStateToValue(…)`) a violation on the `a: A` slot.

**Signatures that must declare what they transport** — **all four converted at A.11.7-T step 3**; the
final spellings, including what the conversion itself measured, are listed there:

- `def .[A, B](a: A, f: A => {Effect} B): {Effect} B` — the subject stays a plain, strict slot; the
  transported row is declared on `f` and comes back out of the dot.
- `foldLeft` and `foldOption` — the two A.11.2-R declined — plus `foldEither`, which §6.1-A added.
  `foldLeft` needed more than its `initial`: its **accumulator** is rowed too, and its jvm primitive
  carries an explicit `[F[_] ~ Effect]` binder.

**Call sites that pass a computation through a rowless slot** — 7 lines, each rewritten to the direct
call, where the callee's own declared slot (pinned, or carrier-typed) already has the right mode:

| site | now | correction |
| --- | --- | --- |
| `examples/src/Blocks.els:33` | `rename("after").runStateToPair("before")` | `runStateToPair("before", rename("after"))` |
| `examples/src/EffectsState.els:22` | `swap("second").runStateToPair("first")` | direct call |
| `stdlib/…/effect/State.els:69,76` | `runStateToPair(initial, p).map(first/second)` | `map(first, runStateToPair(initial, p))` |
| `stdlib/…/effect/Writer.els:54,60` | `runWriterToPair(p).map(first/second)` | direct call |
| `stdlib/…/collection/List.els:26` | `acc.flatMap(_ -> action(e))` | `flatMap(_ -> action(e), acc)` |

**Six of the seven, and both scaladoc examples, are corrected as of A.11.7-T step 1** — step 1 makes
`.`'s slots live, so they stopped compiling (or, at `State`/`Writer`, silently miscompiled) and had to
be corrected there rather than waiting for step 2's diagnostic.

**`List.els:26` could not be corrected on its own and waited for step 3**, where `foreach` became
`list.foldLeft(pure(unit), e -> _ -> action(e))` — the dot disappears with the nested `flatMap`.
Rewriting it alone made `foreach` print nothing (`ListIntegrationTest`, `FileIoIntegrationTest` and two
`ExamplesIntegrationTest2` State programs): `flatMap`'s `fa: F[A]` is a *declared-suspended* slot, and
the accumulator handed to it binds `foldLeft`'s `combine: Function[A, Function[B, B]]`, whose domain
rule 4 reads — correctly — as a payload, so it is `pure`-wrapped. The dot spelling merely hides that:
the defect is `foldLeft`'s signature, and the site's correction belongs with it.

The two scaladoc examples in `carrier/Effect.els` (`readLine.flatMap(…)`, `readLine.map(…)`) taught the
disallowed form and now show the direct call. The infix dischargers are **not** affected: `catch`
(`Throw.els:77 infix left`) and `else` (`Abort.els:76 infix right`) resolve straight to a direct call
with the computation at its pinned slot, so `x catch (e -> …)` and `host else "localhost"` are
untouched.

**Declarations storing an open carrier**: `jvm/test/…/TerminationIntegrationTest.scala`'s
`data Box[F[_]](action: F[Unit])` — already illegal under rule 3 (a stored row must be pinned), and the
last source of the flex-flex `?F[X] ~ ?G[Y]` that A.11.7-R found still alive. Step 2's enforcement is a
rule about *call arguments* and never reached it (no call routes through a field declaration), so it was
**corrected as a fixture at A.11.7-X** — the shape is now the concrete `data Box(action: IO[Unit])`, which
is what rule 3 asks for and what made the carrier router's last shape disappear. Rule 3 still has no
*check* of its own for a `data` field declaring an open row; adding one is optional hardening, and A.10's
cancellation of slice 4b stays cancelled either way.

**Tests pinning a rule-4 violation are wrong and get corrected** — known: three `RowElaboratorTest` twins
(including the one named "defer a call with a generic-headed return — A.8.6"), `MonomorphicTypeCheckTest`'s
"pass an effectful eliminator branch through unsequenced", and the 5 `State`-family shapes A.11.7-R
measured, which fail *because* of erosion 3 in the §1 table.

### 6.1-A The rule-4 conformance audit (2026-07-28) — the complete measured gap

*Method* (A.9.4): a temporary env-gated `RowAudit` recorded, at every elaborated call, each **plain
generic of the callee instantiated at a computation** — read off the arguments *after* hoisting, so an
argument that performs and is run here is not counted (that is rule 1 working). This is the exact
predicate step 2 turns into a hard error, and it is strictly wider than "a rowless slot receives a
computation": it also catches an arrow slot `f: A => B` given a function whose *codomain* is a
computation, which is how most of `.`'s traffic violates the rule. Instrument reverted; tree unchanged.

*Coverage*: the whole gate (871/871 green while instrumented) plus all 40 examples. **28 distinct shapes
over the gate, 4 over the examples** (a subset). By callee:

| callee | shapes | what instantiates the generic at a computation |
| --- | --- | --- |
| `.` — subject slot `a: A` | 9 | 5 discharge stacks (`ThrowCarrier`/`StateCarrier`/two `DepCarrier`/`IO`), `readLine`, `flatMap[IO]`, a nested `.`, `swap` |
| `.` — function slot `f: Function[A, B]` | 7 | `flatMap`, `foldLeft`, `foreach`, `provide`, `runStateToPair`, `runStateToValue`, `runThrow` |
| `foldEither` | 3 | `onError`, `err -> printLine(…)`, `err -> pure(Left(err))` |
| `foldOption` | 2 | `pure`, a suspended `fallback` |
| test-local combinators (`pipe`, `|>`, `weird`) | 5 | `readLine`, `flatMap`, an effectful lambda |
| `foldLeft` | 1 | `pure(unit)` at `initial` |
| `++` | 1 | a nested `++` whose own `T` is a computation |

Three corrections to the inventory above, all forced by the measurement:

- **`foldEither` joins the signature list.** §6.1 excused it as "takes lambdas and is lazy by
  construction" — but rule 4 is about *declaring*, not about laziness, and `Throw`/`Abort` hand it
  handlers that return computations. It needs `onLeft: E => {Effect} B`, `onRight: A => {Effect} B`,
  return `{Effect} B`, exactly like `foldOption`.
- **Converting `.` does not clear `.`.** §6.1's spelling keeps the subject a plain, strict slot, which
  is right — but it means the 9 subject-slot shapes stay violations and must each become a direct call
  (or hoist, where the subject performs). Two of them were the `Blocks`/`EffectsState` sites already
  corrected; the rest live in jvm test programs and are defects in those fixtures.
- **The test-local combinators are fixtures pinning the old model** and are corrected with the rest, per
  the paragraph above.

Nothing in the measured set is outside rule 4's reach, and nothing needs a new mechanism: every entry is
a signature that must declare what it transports, or a call site that must stop routing a computation
through a rowless one.

**All 28 are cleared as of A.11.7-T step 3 (2026-07-28)**, and the audit needs no re-run to say so: its
predicate *is* the enforcement (`RowElaborator.violations`), which now aborts any value that trips it, so
a green gate and 37/40 examples are the audit reporting zero. The prediction it could not make held too —
converting `.` did not clear `.`: the nine subject-slot shapes each had to become a direct call.

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

1. ~~**`foldOption`'s suspension**~~ — **CLOSED 2026-07-28 by measurement (A.11.7-U), not by a decision.**
   It converts, in the full rule-4 spelling (`ifNone: {Effect} B`, `ifSome: A => {Effect} B`, return
   `{Effect} B`). A.11.5-R's two refutations both dissolve: the first was measured against a spelling
   that declared only *one* of the two positions, and the second — "the elaborator cannot prove a lambda
   binder pure" — is not a missing *rule* but the missing *instantiation*, i.e. A.11.7-T step 1. §6 is
   correct as it stands; there is nothing here to sign off.
2. Whether the post-mono accounting verifier can eventually retire, now that the pre-mono check is
   unbounded — not before experience says so.
3. The fate of `Checker`'s remaining slot deferral — it is now the **compile track's** mid-spine decision
   only (§8), reached by the `Either` guard discharge; whether spine inference simplifies further once
   that track is revisited is untouched by this work.
4. ~~**A pinned `data` field still hoists**~~ — **FIXED 2026-07-28** (A.11.11 below). The data-level pass
   now rewrites *open* rows only, so a pinned field is still spelled as a row when
   `DataDefinitionDesugarer` splits the data and the per-function pass tags the constructor and accessor.

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
  identically. A non-pinned slot that **fixes a foreign concrete carrier** (`fixesCarrier(f, i, aᵢ)`:
  `aᵢ` is a saturated call to a rowed callee, and the slot is a concrete constructor — not `Function`, not
  one of `f`'s own binders — headed differently from that callee's declared *payload*) still
  contributes, but its entries are recorded as *undecided* and drop out of the leak: they are performed in
  the slot's carrier, not on this definition's ambient (§2's third bounding).
- **latent**: `latent(λx.e) = row(e)`; `latent(under-applied ref f) = declared(f)`; else `∅`.
- **declared**: open-row return entries ∪ the effects constrained on the signature's carrier binders
  (machinery excluded); an effect-ability method's contribution is its own ability.
- **check**: `row(peeled body) ∖ undecided ⊆ declared`, reported at the definition, bounded by coverage,
  decidability and carrier-fixing slots (§2).

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
   the base of a declared **pinned** row (deliberately unconstrained, so nothing else marks it), or a
   *machinery* ability's method (`Effect`'s `pure`/`flatMap`, `Suspend`'s `suspend` — the only ability
   methods that cannot say so with a row, since machinery is filtered out of every row by design).

   **An ordinary ability method had a fourth clause, and it was wrong** (fixed 2026-07-30): its ability's
   own binder counted, whatever the ability. The stated justification was that `Console` and a
   constructor-class `Container` are the same shape and the *use site* separates them — but the use site
   is never consulted by the row derivation, which read `unwrap(b)` as performing the effect `Container`
   and rejected `def unboxed(b: Box[String]): String = unwrap(b)` with no spelling that could fix it.
   **An ability is not an effect by nature; a method performs an effect because it declares one**, with a
   row on its return exactly as any other definition does (`def printLine(s: String): {Console} Unit`).
   Those rows desugar onto the *ability's own* binder (`EffectSugarDesugarer.abilityMethodCarrier` — the
   same reuse rule an `Effect`-constrained binder gets, reading a different declaration), so the
   constraint clause above answers for them and `RowChecker.calleeContribution` lost its ability arm
   entirely: every callee, ability method or not, contributes its declared row. A method declaring no row
   performs nothing, which is exactly what a constructor class is.
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
nodes) and, for an argument, its spine **head**. And the cache (`target/.eliot-*`) must be deleted before
every run or the pipeline replays facts and the trace comes back empty.

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
   `GuardDischargeResolver` (then `CalculatedReturnResolver`); guarded instances beyond `Throw`'s; two distinct `State[S]` layers (for
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
| the carrier side table | **REDUCED 2026-07-28** (A.11.8-3): `CarrierRole` and the `effectCarrier` flag are gone — they were the higher-kinded *kind* record under another name — leaving `Unifier.higherKindedMetas` and two derived projections, which the **compiler track's** inline guard reads (§8). `EffectLifter`'s remainder beyond the one pure-lift arm and the node builders stays with it | −31 |
| ~~A.8.6's uncertainty~~ | **DONE at A.11.6**: `uncertain`/`Derivation.deferred` and `DeclaredPureChecker` deleted; two boundings remain by design (coverage, decidability), and `capturedByStack` was added to the derivation | −150 |
| flags & experiment scaffolding | **DONE at A.11.9**: `CompilationSession.compileOnce(seedFacts)` (a production API added only for the R4 shadow compile), `RowElaborationShadowCompileTest`, and *all* of `RowShadowSweepTest` — plus the `RunBoundaryFunction` fact and processor, whose demander went with the bridge. `EffectCorpus` stays, with `EffectCorpusIntegrationTest` as its consumer | −285 |

**Arithmetic to hold the work to — RESTATED 2026-07-28**, because cancelling the `Id` group changes it
and A.11.7-S said to restate it rather than let the old number stand. `check/` was 5,219 at the
stock-take and is **5,097** after A.11.6; the remaining deletions (bridge ~933, obligations ~350, carrier
side table ~400, less what is not in `check/`) remove ≈950 more, landing `check/` at **≈4,150** against
the pre-v2 baseline of 3,996 — unchanged, since the `Id` apparatus never lived in `check/`. The
**machinery total** does change: 6,895 → **≈5,540**, not ≈5,200, because `IdNormalizer` and its two
helpers (~340) stay. That is still below the pre-v3 5,585, and above pre-v2 3,996; the difference is
`row/`, a phase that did not exist. **Do not claim a net reduction against pre-v2**, and do not quote the
≈5,200 figure — it assumed a deletion that is now cancelled.

**Measured at the end of A.11.8 (2026-07-28)**: `check/` **3,873** — the target is met with room (−277
against ≈4,150, and below the pre-v2 3,996). The **machinery total is 6,091** (`check/` 3,873 + `carrier/`
0 + `row/` 2,218), *above* the ≈5,540 projection, and the whole difference is `row/`: it was **1,268** at
the stock-take and the projection implicitly assumed it would stay there. It grew to 2,218 as the
elaborator took over decisions the checker used to make — the design working, not drift, and the reason
the `check/` number is the one to hold the work to.

**Exit criteria, all mechanically checkable:**

- `grep -rin "uniformCarrier\|CarrierJoin\|UniformLadder\|ModeObligation\|seedFacts" lang/src jvm/src eliotc/src` → **empty as of A.11.9**.
  (`IdNormalizer` was in this list until 2026-07-28 and is **not** an exit criterion — see §1 rule 4.)
- No `lang/src/.../monomorphize/carrier/`.
- No env-var, system-property, CLI or constructor gate anywhere in the effect path — one code path only.
- `check/` at or below 3,996 lines. (The machinery total lands at ≈5,540, not ≈5,200.)
- Full gate green (871 test targets across lang/jvm/eliotc/LSP is the current baseline), and **37 of 40
  examples compile** (`IfDemo` included — the bridge causes its failure;
  it already compiles as of A.11.4).
- `docs/effects-as-channel.md` retired; the CLAUDE.md cornerstone describes rows and written carriers.

**All six verified at A.11.10 (2026-07-28)**: the grep is empty, `monomorphize/carrier/` does not exist,
there is no `getenv`/`getProperty` anywhere under `row/`, `monomorphize/` or `effect/`, `check/` is 3,873,
the gate is green with 37/40 examples, and the v2 document is a signpost while the cornerstone is
rewritten (A.11.10-1).

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

> **Read order for the A.11.7 group.** `A.11.7` is the original step. `A.11.7-R` is why it stopped,
> `A.11.7-S` is the spike that answered the question it stopped on, and **`A.11.7-T` is the work to do
> now** — implementing §1 rule 4, which is what makes the deletion below possible. Do T, then this.

`monomorphize/carrier/`, `UniformCarrierChecker`, and the routers in `Checker`. Trace first and delete
only zero-fire arms — after A.11.4/A.11.5 the whole group should be cold, and **any arm that still fires
is a missing elaborator rule, i.e. a stop-and-redecide signal**, not a reason to keep the arm.

**This ran, and it returned the stop-and-redecide: the group is not cold. See A.11.7-R** for the
part-by-part measurement and the two decisions that have to be reopened first — both since closed by §1
rule 4. Nothing below this paragraph has been executed; it becomes executable after A.11.7-T.

Keep exactly one thing: the pure-lift rule (the default ladder's existing pure-wrap arm against a
**rigid** expected type) plus the `pureWrapNode`/`runIdNode` builders, which move to `row/` as the
elaborator's node constructors. Note A.11.5a left one live bridge behaviour to preserve or relocate: a
payload slot declaring `Id[T]` takes it as data.

### A.11.7-V The measurement after rule 4 (2026-07-28) — and the one arm that is still not cold

Re-ran A.11.7-R's part-by-part bypass (A.9.4's method; scaffolding env-gated and reverted, tree green
throughout). **Rule 4 bought most of it**, and the remaining live surface is one arm:

| part switched off | A.11.7-R (before rule 4) | now | what still fires |
| --- | --- | --- | --- |
| carrier router | 1 test / 37 examples | **1 / 37** | `data Box[F[_]](action: F[Unit])` — §6.1's illegal stored open row |
| uniform return boundary | 8 / 36 | **1 / 37** | one `MonomorphicTypeCheckTest` HKT-carrier-to-`Id` shape |
| payload router | 36 / 32 | **11 / 37** | the `catch`/`else` non-identity-handler pin (`eagerRowPinIntoDomain`) |
| **the whole deferral path** | — | **0 / 37, class content byte-identical** | *nothing* |

**The obligation path (A.11.8 step 1) has exactly one live producer, and the exit test fails.** With every
deferral site off the gate is untouched — *except* `uniformPayloadSlot`'s hoist suspension, which is
`payloadSlot/suspendHoist` itself: switching that one arm off costs **5 tests**. So A.11.7's exit test —
`payloadSlot/suspendHoist` reaching zero — does **not** hold, and by this section's own standing rule that
is a **stop-and-redecide**, not an arm to keep. A deletion attempt was built and **reverted**: two of the
five are programs that must *not* compile and did (`EffectDiagnosticVocabularyTest`'s missing
`State`-over-`Throw` cross-lift), which is a fail-safe regression, not a spelling difference.

**The missing rule, stated as precisely as the corpus states it.** The clearest shape is
`def echo: {Console} Unit = printLine(pure("lifted"))` (`ExamplesIntegrationTest1`): `pure(…)` is a
**computation value with an empty row** — carrier-headed by declaration, but it neither *performs* nor
*discharges*, and `RowElaborator.strictArgument` hoists only on `performs || discharges` (deliberately:
binding a `pure(x)` runs a computation that has nothing to run). So the elaborator passes it through and
the checker must bind it. Under §1 rule 4's second bullet — *a rowless slot may not receive a computation,
not a carrier-headed value, not a pinned capture* — that program is **illegal**, and the enforcement built
at A.11.7-T step 2 does not catch it: `recordRowlessSlots` fires on a callee's *plain generic*
instantiated at a computation (the §6.1-A predicate), and `printLine(s: String)`'s slot is concrete.

Two ways to close it, and the choice is Robert's because each changes what compiles:

1. **Widen the enforcement to rule 4's stated scope** — a computation at *any* rowless slot, concrete
   included. `printLine(pure("lifted"))` becomes an error naming the fix (`pure` is not needed there),
   and the five shapes become fixture corrections like §6.1's.
2. **Hoist a carrier-valued argument regardless of its row** — drop `strictArgument`'s
   `performs || discharges` gate. Nothing is rejected, but a `pure(x)` at a plain slot gains a
   `flatMap`/`pure` round trip that erases only if `Id`-normalization reaches it.

Option 1 is what the rule as written says; option 2 is what the tree does today, via the checker.

**Option 2 is refused (2026-07-28, Robert): `printLine(pure("string"))` must never be legal.** And the
carrier it lifts into is *not* `Id` — measured from the emitted bytecode of
`def echo: {Console} Unit = printLine(pure("lifted"))`:

```
IO.pure("lifted")  →  IO.flatMap(λ, ·)          + one lambda class
```

The hoist writes `flatMap` on the **region's** carrier and `pure`'s carrier unifies with it, so this is a
real allocation and two `IO` calls for a string the caller already held; `Id` would appear only if the
enclosing definition were pure. The tree allows it today, with a test pinning it
(`ExamplesIntegrationTest1`, "should bind author-written machinery flowing into a pure slot").

### A.11.7-W Option 1 measured — and it lands on the rule 3 / rule 4 boundary

Built and reverted: `recordRowlessSlots` extended so a slot that *cannot host a computation* — not one of
the callee's carrier binders, not `Id`, not a platform run carrier, not an arrow ending in one, not a
pinned or run-boundary position — reports an argument whose kind is `Carrier`. **84 test failures, 30
violation sites across five callees. Exactly one is the target shape.** The other four are all one thing:

| callee | sites | what it is |
| --- | --- | --- |
| `handleCases` | 12 | the `match` desugar's eliminator, `value: T` — matching *on* a `ThrowCarrier`/`AbortCarrier` value |
| `AbortCarrier` / `ThrowCarrier` | 12 | the carrier `data` constructors, field `G[Either[E, A]]` |
| `runId` | 5 | the `RowElaboratorTest` twin harness — its `Id` stub does not match `WellKnownTypes.idFQN`, so the exemption misses (harness artifact, but it shows the exemption is name-keyed) |
| `printLine` | 1 | `printLine(pure("lifted"))` — the shape this is all about |

**The collision is between §1 rule 3 and §1 rule 4, and it is real.** Rule 3: *pinned means captured — a
reified computation is an ordinary type, usable in `data` fields, discharger parameters, `List[TestCase]`*.
Rule 4: *a rowless slot may not receive a computation*. A concrete carrier stack value is **both** — a
computation by its type and ordinary data by rule 3 — and `RowElaborator.expressionKind` answers `Carrier`
for both. So constructing a `ThrowCarrier` and matching on one, which rule 3 explicitly blesses, read as
rule-4 violations.

Two of the four also have a narrower cause worth separating out: the carrier `data` constructors are
rejected only because their field is headed by the **data type's own higher-kinded binder** (`G[_]`, which
carries no `~ Effect` constraint), and "an applied binder head is unclassifiable — the constructor class"
is already the established reading in `typeKind`. Extending the *slot* exemption the same way is not a new
rule and clears 12 of the 30. `handleCases` is not covered by it: its scrutinee slot is a **bare** binder,
which rule 4 calls a payload, and the scrutinee is carrier-typed data.

### A.11.7-X Rule 4 unqualified, decided and landed (2026-07-28)

**Decision (Robert): rule 4 is unqualified — `printLine(pure("string"))` is never legal.** Of the two
readings, the unqualified one is also the simpler to implement, because its exemptions are not new rules
but existing ones restated. It is landed, and **A.11.7's exit test now passes: `payloadSlot/suspendHoist`
is ZERO over the whole gate and all 40 examples** (it was 5 shapes). Gate green, 37/40 examples, every
program's output *and* class content unchanged.

**What "rowless" is, decided positively rather than by exclusion.** A carrier is always *applied* to its
payload, so **any applied head may be a carrier** and this pass says nothing about it — `{G} A`, `runId`'s
`Id[A]`, `runMain`'s `IO[A]`, a user's own identity carrier, the carrier `data` types' own
`G[Either[E, A]]` field. What remains is what rule 4 can positively call rowless: a **nullary concrete
type** (`printLine`'s `String`) and a **bare generic**, which rule 4 calls a payload outright. Deciding it
this way is what keeps the rule off names — recognising carriers by FQN misses a user-declared one (the
shadow corpus declares its own `data Id[A]`), and "has an `Effect` instance" is prohibited outright.

Two exemptions, each an existing rule speaking:

- a **pinned parameter** passed on is passing *data* — §1 rule 3 verbatim, and what keeps a carrier's own
  generated accessor legal (destructuring its pinned `obj` hands it to the `match` eliminator's plain slot);
- an **applied binder head** is the constructor class, already how `typeKind` reads a return.

**One elaborator gap this surfaced, and it was a real defect.** A discharge nested inside an argument —
`printLine(foldEither(e -> e, s -> s, runThrow(bad)))` — is hoisted while that argument is elaborated,
leaving the argument itself a `flatMap` even though the *original* expression neither performs (the
capture consumed its row) nor is headed by a discharger. Rule 1 places a bind at the enclosing **region**,
not at the innermost call, so `strictArgument` now also hoists a node that is itself an inserted bind
(`sequences`) — the chain keeps rising until it reaches a position that can hold a computation. Without it
the checker was rescuing these, which is what kept `suspendHoist` alive.

**Corrections, all §6.1-shaped**: `ExamplesIntegrationTest1`'s "bind author-written machinery flowing into
a pure slot" becomes a *rejection* test (it pinned the violation), and
`TerminationIntegrationTest`'s `data Box[F[_]](action: F[Unit])` — §6.1's illegal stored open row, and the
last shape needing the arm — becomes the concrete `data Box(action: IO[Unit])`.

**A.11.7 + A.11.8 are now unblocked**: with `suspendHoist` at zero the obligation path has no live producer
at all, which the `defer` bypass already measured as byte-identical.

### A.11.7-Y The three blockers, diagnosed (2026-07-28)

A.11.8-2 left A.11.7 blocked on three shapes and named each only by its failing test. This section
**diagnoses all three** — what the elaborator writes, what the bridge decides instead, and what the
missing rule is — and re-runs the measurement on the post-A.11.8-1 tree, including the whole bridge at
once. Instrument env-gated (`ELIOT_NO_CARRIER_ROUTER` / `ELIOT_NO_RETURN_BOUNDARY` /
`ELIOT_NO_PAYLOAD_ROUTER`, plus a one-line elaboration dump) and **reverted**; tree unchanged and green.

**The measurement, re-confirmed.** Baseline `__.test` 1,539 tests / 0 failures; 37/40 examples.

| part switched off | tests | examples | what fires |
| --- | --- | --- | --- |
| carrier router | **1** | 37, class content identical | `TerminationIntegrationTest` "an `Inf` action stored in data then run through its accessor" |
| uniform return boundary | **1** | 37, class content identical | `MonomorphicTypeCheckTest:537` — the unconstrained-`[F[_]]` stub |
| payload router | **7** | 37, class content identical | `catch` with a **non-identity** handler (`CatchShapeMatrixTest` group B ×6, `ExamplesIntegrationTest2` ×1) |
| **all three at once** | **9** | **37, every class file byte-identical to baseline** | the union, no interaction |

**The example corpus does not need the bridge at all.** With the whole bridge off, all 40 examples
compile exactly as before (37 OK, the same `PluginA`/`B`/`C` failures) and the md5 of every unzipped
class file is identical. The bridge's entire live surface is those **9 test shapes**, which is what
makes a shape-by-shape diagnosis affordable.

#### 1. The carrier router — rule 3 is not implemented on the *delivery* side, and the tree miscompiles

```eliot
data Box(action: IO[Unit])
def runBox(b: Box): IO[Unit] = action(b)
def main: IO[Unit] = runBox(Box(forever(printLine("boxed"))))
```

What the elaborator writes (dumped from `RowElaborationProcessor`, `printLine` variant for brevity):

```
flatMap[IO]($row$2 -> runBox($row$2),
  flatMap[IO]($row$1 -> pure[IO](Box($row$1)), printLine[IO]("boxed")))
```

It **hoists** the computation out of the field and stores `pure(unit)` in the `Box`. The carrier router
then `pure`-wraps the hoisted payload back into `IO` (`tryPureWrap`), so the program type-checks —
without it the slot reports `Expected: IO(IO(Unit)) / Actual: IO(Unit)`.

**This is a silent miscompile on the shipped tree, not a typing gap.** Measured, bridge on:

```eliot
def main: IO[Unit] = {
   val b = Box(printLine("constructing"))
   printLine("before")
   runBox(b)
}
```

prints `constructing` then `before`, and `runBox(b)` prints **nothing**. Under §1 rule 3 — *a pinned or
concrete carrier type is an ordinary type, usable in `data` fields* — it must print `before` then
`constructing`. The covering test passes for the wrong reason: the `forever` loop runs at construction
time, so the accessor is never what loops.

**Mechanical cause.** `elaborateCall` captures at exactly three kinds of slot — a
`pinnedParameterIndices` index, a slot headed by one of the *callee's own* carrier binders, and a slot
declaring the concrete `Id`. A `data` field is none of them:

- the **concrete** spelling `IO[Unit]` is a platform **run carrier** — recognition source (ii), the
  `RunBoundaryFunction` registry — which the elaborator already consults for *regions*
  (`runCarrierHead`) but never for *slots*;
- the **pinned** spelling `{Abort | IO} String` **loses its tag before the constructor exists**:
  `CoreProcessor:40` runs `EffectSugarDesugarer.desugar(DataDefinition)` first, collapsing the row to a
  carrier stack, and only then does `DataDefinitionDesugarer` build the constructor's
  `FunctionDefinition` — so `declaredEffectRow` sees no `EffectfulType` and records no
  `pinnedParameterEffects`. Measured: `data Holder(computation: {Abort | IO} String)` with
  `Holder(risky)` fails **loudly** with `Expected: {Abort | IO} String / Actual: String`, the hoist
  having delivered the payload.
- and a `Console`-performing computation cannot be pinned at all (no canonical carrier, the v1
  limitation), so `IO[Unit]` is the *only* spelling for the shipped test's own shape.

**The missing rule, stated:** *a slot whose declared type is a concrete carrier — a platform run carrier,
or the carrier stack a pinned row spells — hosts the computation, so it captures like a pinned slot.*
Both recognitions are the sanctioned tagged ones (sources (i) and (ii)); neither is a name or shape
guess. Landing it also requires restoring the pinned tag on `data` constructors (desugar order, or carry
the entries through `DataDefinitionDesugarer`).

**Why it needs a decision**: it changes behaviour that currently ships. Storing an effect in a `data`
field starts meaning *store*, not *run here*, so the ordering above flips and the `Box` test loops
through its accessor instead of at construction.

#### 2. The uniform return boundary — the checker inventing a carrier for a binder nothing declares one for

```eliot
def id[F[_]](x: F[String]): F[String] = x
def someString: String
def f: String = id(someString)
```

The only shape in the whole gate, and no example. `F[_]` carries no `~ Effect` constraint, so it is not a
declared carrier: the elaborator writes nothing. The bridge then does two things the written-carrier
model removed everywhere else — `CarrierKindChecker` flags the HKT binder as a carrier *unfiltered*, so
the carrier router `pure`-wraps the argument into `?F`, and the return boundary defaults `?F := Id` and
inserts `runId`. With the arm off it is "Higher-kinded type parameter mismatch".

**The honest spelling already works and was measured, with the arm off**:

```eliot
def f: String = runId(id[Id](Id(someString)))    // prints "hello", arm on and arm off
```

So the option is: **delete the arm; the shape becomes a type error and the test becomes a rejection test
plus the explicit spelling above.** The alternative — keep an `Id` default for unconstrained HKT
binders — is the checker deciding a carrier no declaration mentions, i.e. the premise A.10 reversed.

#### 3. The payload router — `catch`'s `E` has nothing to determine it

```eliot
def bad: {Throw[String]} String = raise("boom")
def main: IO[Unit] = printLine(bad catch (err -> "fallback"))
```

With the router off: `No ability implementation found for ability 'Throw' with type arguments
[String, {Throw[Type] | IO}]` — finding-7's junk-ground. Nothing determines `catch[E, G[_] ~ Effect, A]`'s
`E`, so it grounds to `Type` and the guarded `where E1 != E2` lift is selected instead of the native. An
*identity* handler pins `E := A` through itself, which is exactly why only group B fires. The bridge's
answer is `eagerRowPinIntoDomain`: read the actual's row constraints off `CheckState.metaConstraints` and
pin the domain's error slot before the capturing unify.

**Measured fix — write it in the source instead**:

```eliot
def main: IO[Unit] = printLine(catch[String](bad, err -> "fallback"))   // compiles and prints "fallback"
```

with the payload router **off**. So the missing rule is the natural continuation of A.11.4: *the
elaborator writes not only the carrier but the pinned row's own ability type arguments, instantiated from
the captured argument's declared row* — `catch`'s `{Throw[E] | G} A` against `bad`'s declared
`{Throw[String]}` gives `E := String`. It reads declarations only (§3.2-legal), and `E` sits at binder 0,
so A.11.4b's first-binder limit is not in the way; the write is a legal prefix.

#### What this changes about the plan

Each shape is a *missing elaborator rule*, exactly as A.11.7's standing rule predicted — and two of the
three are the same rule family as A.11.4 ("the elaborator writes what the declaration says"), not new
mechanism. Shape 1 is additionally a **fail-safe defect in the shipped tree**
(`feedback_gaps_must_be_failsafe`): the bridge is hiding a misplacement, not compensating for a missing
type. None of the three is deferrable into the deletion, and none is a judgement call in flight.

#### Decided (2026-07-28, Robert): one rule, not three

All three take the elaborator-side option, and the argument for them is that they are **one rule**, now
recorded in §1 rule 4 (last bullet) and §3.1:

> A slot's declared type decides how it is filled, and the elaborator writes down every type argument a
> declaration determines.

- **Shape 1 removes cases rather than adding one.** `elaborateCall` states "the declared slot type is
  carrier-headed" three times, once per way the carrier is named (pinned index, callee binder, concrete
  `Id`). Admitting the platform run carrier makes it four namings *and one predicate* — and it deletes a
  name-keyed exemption (the `Id` arm keys on `WellKnownTypes.idFQN`, which A.11.7-W already caught
  misfiring on a test harness's own `Id` stub).
- **Shape 3 generalizes `writeCarrier`** from "the carrier, if it is binder 0" to "the determined
  prefix". Same mechanism, same source, same function — and it dissolves the first-binder limit at
  exactly the call A.11.4b recorded it against.
- **Shape 2 is then a consequence, not a decision**: with nothing else needing the return boundary,
  keeping it would mean keeping the unfiltered HKT carrier flagging, the join, the `Id` finalize and the
  `runId` insertion for one lang stub whose honest spelling already compiles.

**Order of work** (chosen for descending confidence): shape 3 first — a pure generalization of a
function that exists, with a measured target form. Then shape 1, **spike first**: the current behaviour
is measured wrong, but the fix is unproven and its riskier half is the desugar-order change that restores
the pinned tag on `data` constructors, not the elaborator arm. Then shape 2 as deletion, then A.11.7.

#### Shape 3 landed (2026-07-28) — the payload router is cold

`writeCarrier` became `writeTypeArguments`: it walks the callee's binders and writes the leading run each
of which some declaration determines, stopping at the first it does not. Two sources —
`ridesFirstBinder` + `carrierAt` (the region's carrier, unchanged) and the new `pinnedDetermination` (a
pinned parameter's row entries matched against the captured argument's own declared row, read via
`argumentRow` from the argument callee's `declaredEntries`). `catch` now comes out `catch[String](bad,
h)`; `else` keeps `else[F]` because its carrier *is* binder 0.

**`G` is deliberately not written**, and this is the rule rather than a bound: a discharger's carrier
binder is the pinned row's **residual**, which the capture decides — the region's carrier is merely a
good guess for it (right when the discharged effect is exactly the argument's row minus the region's,
wrong under a nested discharge). A guess is not a determination, so the prefix stops. `else`'s binder-0
carrier is written by the *first* source, exactly as before, so nothing that worked changes.

**Measured**: full gate 1,539/1,539 green, 37/40 examples, every class file byte-identical to the
pre-step build — and with the **payload router switched off**, still 1,539/1,539 and 37/40 byte-identical
(it was 7 failures before this step). The router's last live job is gone.

#### Shape 1 landed (2026-07-28) — the carrier router is cold, and the miscompile is fixed

`idHeaded` became `concreteCarrierHead`: a slot whose declared type is a **concrete carrier applied to
its payload** — `Id[A]`, or a platform run carrier off the run-boundary registry (`IO[Unit]`) — hosts the
computation, so `elaborateCall` captures the argument with the region spelled as that carrier instead of
hoisting it. `Id` stops being a special case and becomes one of the namings.

**The behaviour this corrects**, now pinned by a test in `TerminationIntegrationTest`:

```eliot
data Box(action: IO[Unit])
def runBox(b: Box): IO[Unit] = action(b)
def main: IO[Unit] = { val b = Box(printLine("stored")); printLine("before"); runBox(b) }
```

was `stored` / `before` with the accessor's run printing nothing, and is now `before` / `stored`. The
neighbouring `forever`-in-a-`Box` test passed either way — it loops at construction just as happily —
which is why an ordering assertion goes in beside it.

**Measured**: full gate 1,539/1,539, 37/40 examples, every class file byte-identical to the pre-step
build; with the **carrier router switched off**, still 1,539/1,539 and 37/40 byte-identical (it was 1
failure). Both routers are now cold.

#### Separated out: a **pinned** `data` field still does not capture

The other half of shape 1's diagnosis is a distinct defect and does **not** block the deletion, because
it fails *loudly* and failed identically before this work:

```eliot
data Holder(computation: {Abort | IO} String)
def main: IO[Unit] = { val h = Holder(risky) … }     // Expected: {Abort | IO} String / Actual: String
```

The pinned row is the *documented* way to store a computation (§1 rule 3; `docs/effect-row-tails.md`
requires stored rows to be pinned), and `{Abort | IO} String` is not reachable by
`concreteCarrierHead` — `AbortCarrier` is neither `Id` nor a run carrier. The route is the pinned
**tag**, and the constructor does not have one: `CoreProcessor` runs
`EffectSugarDesugarer.desugar(DataDefinition)` first, which collapses the row to a carrier stack, and
only then does `DataDefinitionDesugarer` build the constructor, so the per-function
`declaredEffectRow` sees no `EffectfulType` and records no `pinnedParameterEffects`.

The fix is a desugar-order change — leave *pinned* rows for the per-function pass and rewrite only the
open rows at the data level, which is what mints the data type's carrier — with its own blast radius
(`DataDefinitionDesugarer` would then see an `EffectfulType` in a field type). It is deliberately not
bundled into the bridge deletion.

#### A.11.7 landed (2026-07-28) — the bridge is deleted

With both routers cold and the return boundary's one shape converted, the whole v2 bridge went:
`monomorphize/carrier/` (`Carrier`, `CarrierJoin`, `UniformLadder`), `check/UniformCarrierChecker`, and
in `Checker` the `uniformChecker` collaborator, `checkAgainst`'s uniform route with
`uniformReturnBoundary`/`uniformReturnRoutable`/`uniformValueReturn`/`uniformPlainValueType`/
`unifiesDefinitionally`, `routeArgumentSlot` with `uniformPayloadSlot`/`uniformCaptureSlot`/
`uniformCarrierSlot`/`uniformArgumentSlot`/`uniformPayloadOf`/`payloadFitsDomain`/
`singleLayerCarrierDomain`/`eagerRowPinIntoDomain`/`findCarrierLayerSlots`, and `calleePinnedParams`
with the `pinned` flag threaded through the spine loop. `checkAgainst` and `checkArgumentSlot` now go
straight to the single ladder on both tracks. Tests deleted with their machinery:
`carrier/CarrierMechanismTest`, `check/UniformCarrierCheckerTest`.

**`check/` is 4,622 → 3,879 — below the pre-v2 baseline of 3,996**, which was A.11.0's arithmetic and
the number to check.

**One consequence worth keeping**: `calleePinnedParams` was the last **demander** of the
`RunBoundaryFunction` *fact*. The pipeline never needed it — `LangProcessors` hands the run-boundary set
to `RowElaborationProcessor` directly from the plugin configuration — but two test harnesses
(`RowShadowSweepTest`, `RowElaborationShadowCompileTest`) reconstructed their `RowChecker.Universe` by
collecting that fact out of the demanded universe, so with no demander they silently swept with *no*
run boundaries and reported a false `Console` leak on `main`. Both now read
`session.effectiveConfiguration.getOrElse(RunBoundaryFunction.configKey, …)`, the same source
`LangPlugin` uses. The fact and its processor are now produced-but-never-demanded and should be retired
in A.11.9, with `configKey` rehomed.

**Gate**: `__.test` **1,489/1,489 green** (1,539 before, minus the 52 tests of the two deleted suites,
plus 2 new ones), 37/40 examples, and **every class file byte-identical to the baseline taken before any
of this work**. The exit greps `uniformCarrier|CarrierJoin|UniformLadder|ModeObligation` are empty;
`IdNormalizer` stays by §1 rule 4 and `seedFacts` goes with A.11.9's scaffolding.

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

## A.11.7-U The `foldOption` measurement: it converts, and its blocker is A.11.7-T step 1

Run 2026-07-28, three declarations changed and reverted; tree restored (37/40, class content
byte-identical to baseline, 0 diffs). §9 open question 1 is closed by this measurement rather than by a
decision.

**The change**: the full rule-4 spelling in all three layers (abstract `stdlib`, concrete `jvm`, compile
overlay `stdlib/eliot-compiler`), kept lexically identical because the layer merge is lexical —
`def foldOption[A, B](ifNone: {Effect} B, ifSome: A => {Effect} B, o: Option[A]): {Effect} B` plus
`import eliot.carrier.Effect`.

**Result: 35/40 examples and 9 test failures — and all 11 are one cause.**

```
printLine(testAllowed.foldOption("DENIED", s -> s))          EffectsTestable:21
  Expected: Option(String) -> String
  Actual:   Option(IO(String)) -> IO(String)
```

`ifNone` pure-lifts fine (`B := String`). The lambda `s -> s` sits at the declared carrier-codomain slot
`A => F[B]`, and the elaborator cannot show its body — a bare binder reference — is pure, so it does not
`pure`-wrap it; unification then solves `A := IO[String]` and the actual `Option[String]` no longer fits.
The other shapes are the same class one step out: `p -> p.second` (a dot, whose result
`declaredPayloadResult` refuses today) and `Path.extension` in `FileIoIntegrationTest`. Where the body
*is* classifiable the conversion is silent — `jvm/…/Abort.els:24`'s `foldOption(None, a -> Some(f(a)), o)`
never moved.

**Why this closes the question rather than answering it.** A.11.5-R read this as needing a new
"lambda-binder purity rule". It is not a new rule: **rule 4 already decides it.** A lambda at a slot
declared `A => {Effect} B` binds `A`, which is a plain generic and therefore a payload — so a reference
to that binder *is* pure, by declaration. Supplying that fact is exactly A.11.7-T step 1 (instantiate the
callee's declared shape from the arguments), and `RowElaborator` already carries the binder bookkeeping
(`withBinder(name, holdsCarrier, isPayload)`) to receive it.

**Consequence for A.11.7-T**: `foldOption` is not a separate step and needs no separate decision. It is a
*test case* for step 1 — convert it in step 3 alongside `.` and `foldLeft`, and take
`EffectsTestable:21`, `EffectsOrdering:34/36` and `FileIoIntegrationTest`'s extension shape as the
acceptance shapes for the lambda-binder half of step 1.

## A.11.7-T Implement §1 rule 4 — the work that unblocks the deletion

Everything from A.11.0 onward is a *deletion* step, and each presupposes that the elaborator can classify
every position from declarations. That is what rule 4 buys, and this section is how it is bought. It is
the only remaining step that writes code rather than removing it.

**The three steps are ordered, and the order is load-bearing.** Step 2 before step 3, or the §6.1 sites
get silently re-routed instead of surfacing as errors — the failure mode this whole effort exists to stop.

### Step 1 — per-call instantiation of the callee's declared return

`RowElaborator` gains one rule: *a call's result kind and row are its declared return, instantiated from
the declared types and rows of the arguments supplied.* One step, local, order-free, no unification, no
metas; whitelist-legal by §3.2 ("an elaborator-local join over the callee's declared parameter shapes").

This replaces `declaredPayloadResult`'s blanket `false` on a `ParameterReference` head. It is **not**
A.11.7-R's candidate rule, which said "a generic head is a payload" — that approximates rule 4; this
*instantiates* it, and the difference shows exactly where the candidate cost 5 `State` miscompiles.

It has **two halves**, and A.11.7-U measured the second one into existence: call results (`.`'s `B`,
`++`'s `T`), and **lambda binders** — a lambda at a slot declared `A => {Effect} B` binds `A`, a plain
generic, so by rule 4 a reference to that binder is a payload and its body `pure`-wraps.
`RowElaborator.withBinder(name, holdsCarrier, isPayload)` already carries the bookkeeping to receive it.

- *Acceptance*: `.`, `++`, `identity` classify correctly at every A.11.3-R site; for the binder half,
  `EffectsTestable:21`, `EffectsOrdering:34/36` and `FileIoIntegrationTest`'s extension shape (A.11.7-U);
  the `RowElaboratorTest` twins that assert the A.8.6 spellings are rewritten (they pin a rule-4
  violation — §6.1).
- *Risk*: none foreseen. This is reading declarations the elaborator already reads.

**LANDED (2026-07-28). Gate green — full `__.test`, 37/40 examples, every program's output and class
content unchanged except the two dot-discharge sites §6.1 corrects.**

- `RowElaborator.callResultKind` is the one function: a call's result is its declared return, classified
  against the callee's carrier binders and, where it is headed by a plain generic, against
  `instantiation` — the kinds the arguments occupying the *determining* positions supply (a bare-binder
  slot `a: A`, an arrow codomain `f: A => B`). `Kind` is `Payload | Carrier | Unknown`;
  `declaredPayloadResult`'s blanket `false` on a `ParameterReference` head is gone, and
  `calleeCarrierValued` / `definitelyPure` / `coreClassified` all read the same function. A binder no
  parameter determines stays `Unknown`, which is what keeps the constructor class (`wrap(s) : F[A]`)
  out — the distinction A.11.7-R's candidate rule could not make.
- The **binder half** (A.11.7-U): `withLambdaBinders` classifies a lambda's own binders from the declared
  domains of the slot it occupies. An atomic non-carrier domain — a plain generic `A`, a concrete
  `String` — is a payload, so `s -> s` at `A => {Effect} B` is provably pure; a carrier-headed domain
  holds a computation; an applied one (possibly a concrete carrier stack) stays unclassified.
- **Corollary the corpus forced — hoisting *changes* the instantiation.** `choose(readLine, readLine)`
  has computations at both slots before the rewrite and payload binders at both after it, so the core
  call's own kind flips carrier→payload and it must be `pure`-wrapped as the chain's innermost
  continuation. The kind is therefore re-read off `finalArgs`, with each `$row$N` registered
  payload-by-construction. Without it `choose` and `printLine(identity(readLine))` both failed to
  monomorphize ("Type mismatch, readLine"). The resulting spelling — `flatMap`, `flatMap`, `pure` — is
  §1 rule 1's canonical example compiled exactly as the rule states it.
- **`assemble` now rebuilds a spine keeping each original application node's own position**
  (`rebuiltSpine`), instead of `applyChain`'s per-argument attribution. This was not cosmetic: with the
  newly `pure`-wrapped `catch` handler the `catch` spine gets rebuilt, and re-attribution both duplicated
  a hover hint onto the subject's range (two `TypeHintIndexCompileTest` cases) *and* silently broke
  `foreach` and three `State` programs, which printed nothing. The standing comment in `assemble` — that
  rebuilding "silently moves every diagnostic anchored at a call" — understated it; position identity is
  load-bearing downstream, and rewriting one argument must move nothing else.
- **§6.1 corrections applied in the same step**, because step 1 makes `.`'s slots live and the affected
  sites then *hard-fail* rather than waiting for step 2: `State.els:69,76` and `Writer.els:54,60`
  (`runStateToPair(initial, p).map(first)` ⤳ `map(first, runStateToPair(initial, p))` — otherwise the
  discharge is hoisted and `map` meets the payload: `Expected: Id2(Pair(Type, String)) / Actual:
  Pair(String, String)`), `Blocks.els:33`, `EffectsState.els:22`, and the two `carrier/Effect.els`
  scaladoc examples that taught the disallowed dot form. **`List.els:26` is the exception and stays as
  it is** — see §6.1: correcting it needs `foldLeft`'s signature, which is step 3.
- **Tests corrected, both §6.1-listed**: `MonomorphicTypeCheckTest`'s "pass an effectful eliminator
  branch through unsequenced" becomes "run both branches of a payload-slot eliminator, then lift the
  chosen payload"; `RowElaboratorTest`'s "defer a call with a generic-headed return (A.8.6)" becomes
  "lift a generic-headed return its arguments instantiate at a payload".
- **What step 3 must answer, measured here.** `foldLeft`'s conversion needs more than §6.1's
  `initial: {Effect} B`: `foreach`'s accumulator binds `combine: Function[A, Function[B, B]]`, so the
  binder half reads `acc` as a payload — correctly, by rule 4 — which turns the existing violation
  (`B := F[Unit]`) from a deferral the checker patched up into a `pure(acc)` at `flatMap`'s suspended
  slot. So the *accumulator* must be rowed too (`combine: A => B => {Effect} B`), and the jvm primitive
  `foldLeftInternal` then holds `F[B]` at a slot rule 4 reads as a plain generic. That primitive is
  generic and erased, so an explicit `[F[_] ~ Effect]` binder spelling (`initial: F[B]`,
  `combine: A => F[B] => F[B]`) declares it without changing the emitted native — the `{Effect}` sugar
  cannot spell a carrier in an arrow *domain*.

### Step 2 — enforce "no computation at a rowless slot"

A hard error naming the slot, when an argument's declared row is non-empty, or its type is
carrier-headed or a pinned capture, at a slot whose declaration carries no row. This is what makes rule 4
a rule rather than a convention, and it is what makes the §6.1 corrections *appear* rather than change
behaviour silently.

- *Acceptance*: the 7 call sites in §6.1 fail to compile with the new diagnostic, and compile again once
  rewritten to the direct call; `data Box[F[_]](action: F[Unit])` (`TerminationIntegrationTest:206`) is
  rejected and rewritten to a pinned row.
- *Risk*: the diagnostic must name the direct-call fix, or it reads as the language losing a feature. The
  infix dischargers are unaffected and the message should say so.

**LANDED (2026-07-28), together with step 3** — separately it cannot be: the enforcement is what makes
§6.1-A's 28 shapes *stop compiling*, and step 3's signatures are what make them legal again.

- `RowElaborator.Elaborated(body, violations)` carries both out of **one** pass:
  `recordRowlessSlots` runs over the arguments each call ends up with, so the predicate is read *after*
  hoisting exactly as §6.1-A specifies. `RowElaborationProcessor.verifyRowlessSlots` reports every
  violation at its own argument and **aborts** the value — a re-routed computation is a miscompile, not a
  mistyping. The help text names the direct-call fix and says the infix dischargers are unaffected.
- **Measured on its own, before step 3**: 83 test failures and 6 further examples, *every one* of them a
  rule-4 violation on the four signatures step 3 converts (`foldOption` in `Abort.els`, `foldEither` in
  `Throw.els`, `foldLeft` and `.` in `List.els`/`File.els`). Nothing else surfaced — the audit's numbers
  held.

### Step 3 — `writeCarrier` gates on the instantiated row; `ρ := {}` goes to `Id`

`writeCarrier` writes the ambient only when step 1 says the instantiated row is non-empty; at `ρ := {}`
the call is written at `Id` and `IdNormalizer` erases it (§1 rule 4, third bullet). Then convert the
signatures of §6.1 — `.`, `foldLeft`, `foldOption`'s `ifNone`/`ifSome`, and `foldEither` (added by the
§6.1-A audit) — using the `=>` spelling, never `Function[A, {Effect} B]` (A.11.7-S: it parses as a
*block*). `foldLeft` needs its **accumulator** rowed as well as its `initial`, and its jvm primitive an
explicit `[F[_] ~ Effect]` binder (measured at step 1; §6).

- *Acceptance*: the 14 examples A.11.7-S regressed compile again; examples back to **37/40** with class
  content compared against a baseline label; `./mill __.test` back to **871/871**; and the §6.1-A audit,
  re-run, reports **zero** — that is what "the rule holds" means mechanically.
- *Risk*: the one genuinely new behaviour. A.11.5's withdrawn `ridesAmbient` rule belongs here if
  anywhere — re-evaluate it under instantiation, and re-check `Path.extension` (`FileIoIntegrationTest`
  is its only cover).

**LANDED (2026-07-28). Gate green — full `__.test`, 37/40 examples (`PluginA`/`B`/`C` predate A.11.4), and
all 37 program outputs byte-identical to the pre-step build.** The four signatures now declare what they
transport, and no rule-4 violation survives anywhere in the gate or the examples.

*Signatures* (`=>` spelling throughout, never `Function[A, {Effect} B]` — A.11.7-S):

- `def .[A, B](a: A, f: A => {Effect} B): {Effect} B` — the subject stays plain and strict.
- `def foldOption[A, B](ifNone: {Effect} B, ifSome: A => {Effect} B, o: Option[A]): {Effect} B`.
- `def foldEither[E, A, B](ifLeft: E => {Effect} B, ifRight: A => {Effect} B, e: Either[E, A]): {Effect} B`
  (added by the §6.1-A audit).
- `def foldLeft[A, B](initial: {Effect} B, combine: A => B => {Effect} B, list: List[A]): {Effect} B`,
  over `private def foldLeftInternal[F[_] ~ Effect, A, B](list: List[A], initial: F[B], combine: A => F[B] => F[B]): F[B]`
  — the explicit binder spelling the `{Effect}` sugar cannot give an arrow *domain*, exactly as step 1
  measured. The emitted native is unchanged (generic, erased). `foreach` becomes
  `list.foldLeft(pure(unit), e -> _ -> action(e))`, which is §6.1's `List.els:26` correction.

*The empty row, and what it cost* (§1 rule 4's third bullet, in the `Id` form Robert decided):

- **`emptyRowCall`** reads the row a call instantiates off the arguments at the positions that *mention*
  the carrier — a carrier-headed slot, and an arrow slot whose codomain is carrier-headed. All plain
  values ⇒ the row is empty ⇒ the call runs at `Id` (region, written type argument, and inserted `pure`s)
  and its result is projected back with `runId` (`projectId`). Three exclusions, each because the row is
  not that call's to decide: a callee **declaring an effect** of its own, an **ability method** (instance
  resolution chooses), and a **discharger** (the capture's residual chooses — `host else "localhost"` has
  a pure fallback at a carrier-headed slot and is emphatically not the empty row). A reference already
  carrying explicit type arguments is left alone (`writableReference`), so hand-monadic code is never
  double-projected. **This is A.11.5's withdrawn `ridesAmbient`, re-evaluated as the doc asked** — and it
  works here because it *writes* `Id` where the old rule wrote nothing; `Path.extension` compiles and its
  `FileIoIntegrationTest` cover passes.
- **A call at the empty row is a payload to everything outside it** (`callResultKind` downgrades a
  declared `Carrier` result when `emptyRowCall` holds), or the row would leak outwards:
  `show(lines.foldLeft(0, e -> acc -> add(acc, 1)))` would otherwise read as delivering a computation to
  `show`'s rowless slot — a violation of the *caller's* making. `declaredResultKind` keeps the
  pre-projection answer for the projection's own decision.
- **A pure function at a carrier-codomain slot lifts pointwise** (`etaPureLift`): `f: A => {Effect} B`
  given `url` becomes `x -> pure(url(x))`, because a lift under an arrow has nowhere else to go. It is
  built as an eta-expansion and then handed to `elaborateLambdaForced`, the same path a hand-written
  lambda takes, so a body that is already a computation passes through instead of double-wrapping. In
  practice only `.` reaches it — every other converted slot is filled by a lambda.
- **A slot declaring the concrete `Id` carrier** (`runId`'s `obj: Id[A]`) is a capture, not a strict slot:
  nothing hoists out of it. Without that, `foreach(printLine, runId(runStateToFinalState(…)))` hoisted the
  discharge onto the ambient `IO` and demanded it run there.

*Three defects found by the conversion, each a latent bug the new signatures merely exposed:*

1. **Variable capture in the `=>` alias expansion.** `asArrowLike` substituted the alias's binders one at
   a time; `=>` binds `A` and `B`, so expanding `B => {Effect} B` substituted `A := B` and then rewrote
   that very `B` when `B := F[B]` followed — reading `A => B => {Effect} B` as
   `A => {Effect} B => {Effect} B` and classifying `foldLeft`'s accumulator as a computation. The
   substitution is now simultaneous, staged through fresh names.
2. **`instantiation` let a later position erase an earlier one** (`acc ++ …`). One position reading
   `add(e, acc)` — an ability method outside the universe, hence `Unknown` — wiped what `initial: B ← 0`
   had settled. Positions now combine through `joinKind` (`Unknown` loses, `Carrier` wins), which is also
   what makes the rule order-free.
3. **`definitelyPure` could not classify a *called* function-typed parameter.** `f(a)` with
   `f: Function[A, Either[String, B]]` is plainly a value, and saying otherwise left it unlifted at a
   carrier position — which rolls the whole rewrite back and broke `Effect[Either[String]]`'s own
   `flatMap`, i.e. every compile-track guard.

*Two further declaration reads the conversion forced, both stated as rules:*

- **A bare plain-generic return no argument determines is a payload** (`typeKind`), by rule 4 itself: an
  *applied* binder head (`wrap(s) : F[A]`) stays `Unknown`, which is what keeps the constructor class out.
  Without this a data accessor referenced unapplied (`logic.content`) was unclassifiable and its dot ran
  on the ambient carrier.
- **A lambda argument's binders are classified during the row read, not only during its elaboration**
  (`underArrow`). `foldEither(err -> err, v -> v, e)` is read before its handlers are elaborated, so
  `err` would otherwise come out unclassified and the fold ran on `IO`.

*Measured cost, recorded rather than smoothed over:* class content differs for 18 of the 37 examples while
every program's **output** is identical. The `.` conversion means a pure dot whose function is not already
a lambda emits an eta-expansion lambda class — `DotOperator` goes from 23 to 25 classes. That is the price
of `.` transporting a row through one declaration; the alternative (beta-reducing `.` at `ρ := {}`) was
A.11.7-S's option 1, which A.11.4c had already rejected as a `.`-specific special case.

*Test fixtures corrected*, all of them §6.1-A's nine `.`-subject-slot shapes — a dot-chained discharger
(`p.runStateToPair(s0)`, `comp.provide(x).provide(y)`, `bad.runThrow.foldEither(…)`) delivers a computation
to `.`'s rowless `A` and is now an error, rewritten to the direct call: `UniformCarrierCompileTest` (2),
`ExamplesIntegrationTest1` (2), `ExamplesIntegrationTest2` (3), `ExamplesIntegrationTest3` (1). One
`RowElaboratorTest` twin was re-spelled: a pure callback leaves the callee's row empty, so the call runs at
`Id` and projects with `runId` — the twin now says so.

### Then, and only then: the exit test for A.11.7

`payloadSlot/suspendHoist` reaching **zero** over the whole gate is the mechanical signal that the
payload router is dead. A.11.7 and A.11.8 step 1 are **one deletion** (the bridge routes into the
obligation path — A.11.7-R).

### The measurement loop

A.9.4 owns the method. The harness A.11.7-S left is the fast path and should be reused: compile all 40
examples by direct `java -cp` (classpath from `./mill show examples.runClasspath`) with
`--path lang/eliot --path stdlib/eliot --path jvm/eliot` **appended** — `build.mill`'s `examples.run`
adds those, `Main` does not, so a direct invocation without them fails all 40 on "Could not find path
eliot/…". A full sweep is ~3 minutes against ~45 through `./mill examples.run`. Delete the cache
(`target/.eliot-*`) between runs, and never pipe a long mill run through `tail` — it buffers and
progress is invisible.

### Known-unknowns to clear before the deletion, not during it

Neither is a blocker today; both are the shape that becomes one in flight (standing rule 2):

- **The compile track.** §8 keeps `checkAgainstDefault`/`defaultArgSlot` permanently by design, while
  A.11.8 step 1 deletes the *deferral arms* of `genericArgSlot`/`defaultArgSlot`. Those read as different
  things; confirm it rather than assume it.
- **`PluginA`/`B`/`C`.** Failing since before A.11.4, never diagnosed, and A.11.0's exit criterion
  *accepts* them by asking for 37/40. Triage once, so an undiagnosed failure is not carried into the
  criterion that declares the work finished.

## A.11.Z HANDOVER — resume here (2026-07-28)

The single entry point for picking this up. A.9 was the previous one and is now historical; **A.9.4 still
owns the method** and this section only adds to it.

### A.11.Z.1 Tree state and gate baseline

`./mill __.test` **green, 1,487 tests / 0 failures**. **37/40 examples** — `PluginA`/`B`/`C` fail and have
failed since before A.11.4; they are undiagnosed and A.11.0's exit criterion accepts them. Every program's
**output and class content are unchanged** from the pre-A.11.7-T build. Sizes: `check/` **3,873** (below
the pre-v2 baseline 3,996 — A.11.0's arithmetic target), no `carrier/`, `row/` **2,218**, `unify/` 554.

**Done**: A.11.1–A.11.6, A.11.7-T steps 1–3, A.11.7-X, A.11.7-Y, **A.11.7** (the bridge is deleted),
**A.11.8** (step 1 obligations, step 2 cancelled, step 3 the side table) and **A.11.9** (scaffolding and
suites). **§1 rule 4 holds and is
enforced.** Every position classifies from its declaration; a computation may not reach a rowless slot;
`ρ := {}` runs at `Id`. There is **no runtime-track deferral left anywhere** and no runtime-track carrier
metavariable; only the compile track's Phase B and its inferred inline-guard carrier remain (§8).

**A.11.10 is done too** (2026-07-28, A.11.10-1), so **the roadmap is complete** and this handover is
history. What is left is not part of it: the separated **pinned-`data`-field gap** (A.11.7-Y, and §9 item
4) — a `data Holder(computation: {Abort | IO} String)` still hoists, because `CoreProcessor` desugars the
row at the `DataDefinition` before `DataDefinitionDesugarer` builds the constructor, so the constructor
never gets the pinned tag — and the undiagnosed `PluginA`/`B`/`C`, which the exit criterion accepts.

### A.11.Z.2 What is next

> **Superseded 2026-07-28: A.11.7 and A.11.8 are done** — see
> [A.11.7-Y](#a117-y-the-three-blockers-diagnosed-2026-07-28), which diagnosed the three shapes below,
> recorded Robert's unified decision on them, and landed all of it plus the deletion, and A.11.8-3 for the
> side table. What remains is **A.11.9** (scaffolding and suites — now also the `RunBoundaryFunction`
> fact, whose last demander went with the bridge), **A.11.10** (docs closeout), and the
> **pinned-`data`-field gap** A.11.7-Y separated out. The section below is kept for the measurement
> history.

**A.11.7 — delete the bridge.** A.11.8 step 1 is **done** (A.11.8-1) and did not need it: the two deferral
producers inside the bridge were dead and were deleted from within it, so the coupling A.11.7-R described
is severed and the two steps are independent after all. What is left is the bridge proper, and it is
**not** free — three shapes still fire, and by A.11.7's standing rule each is a missing elaborator rule,
i.e. a stop-and-redecide needing a decision, not an arm to keep.

The part-by-part bypass (A.9.4's method; `ELIOT_NO_BRIDGE`, scaffolding reverted). The last row is
**historical** — that path no longer exists:

| part switched off | A.11.7-R | after rule 4 (A.11.7-V) | **now** | what still fires |
| --- | --- | --- | --- | --- |
| `payloadSlot/suspendHoist` alone | — | 5 | **0** | *nothing* — A.11.7's exit test |
| carrier router | 1 / 37 | 1 / 37 | **1 / 37** | `TerminationIntegrationTest` "loop endlessly" |
| uniform return boundary | 8 / 36 | 1 / 37 | **1 / 37** | `MonomorphicTypeCheckTest:540` — "Higher-kinded type parameter mismatch" at a pure return |
| payload router | 36 / 32 | 11 / 37 | **7 / 37** | the `catch`/`else` non-identity-handler pin (`eagerRowPinIntoDomain`) |
| every deferral site | — | 0 (stale) | **7 / 37** | `GuardSignatureIntegrationTest` — the **compile-track** guard discharge |

**The last row is what A.11.8-1 resolved, and the way it resolved matters.** A.11.7-V reported the whole
deferral path cold; that run missed a site, so the honest reading became "switching *every* deferral site
off costs 7 tests". Measuring at **production** granularity instead of by bypass (a counter on each site
that can create a deferral — see A.11.8-1) split those 7 cleanly: they are **all compile-track**, from two
positions in `Either.els:21`, and the runtime track produces **zero** deferrals of any kind. The runtime
half was therefore not "cold at one arm" but absent, and it is now deleted; the compile-track half stays
by §8. **A.11.7-T's first known-unknown — whether the compile-track half can go — is closed: it cannot,
and it should not.**

The two routers have exactly one shape each. Per A.11.7's standing rule those are still *questions* — a
firing arm is a missing elaborator rule — but one shape each, not a class. **A.11.7 cannot proceed until
those three are decided** (A.11.8-2).

**All three are now diagnosed — see [A.11.7-Y](#a117-y-the-three-blockers-diagnosed-2026-07-28)**, which
re-measures on this tree (including the whole bridge at once: **9 tests, 0 examples, every class file
byte-identical**), gives each shape its repro, states the missing rule, and — for shapes 2 and 3 — shows
the replacement spelling compiling *with the arm off*. Headline: **shape 1 is a silent miscompile in the
shipped tree**, not a typing gap. What is still open is the decision on each, not the analysis.

### A.11.Z.3 The deletion recipe — EXECUTED (A.11.8 step 1), kept for its two traps

This was the edit list for the obligation-path deletion, written from an attempt that had been built and
reverted. **It has now been carried out** (A.11.8-1), with one correction: the recipe deletes the *whole*
path, runtime and compile-track alike, and the compile track's Phase B had to be kept — so
`SlotOutcome.Deferred`, `resolveDeferredSlot`, `assembleSpine` and `rebuildChain` **survived**. The rest
went as written. It is retained for **trap 2**, which is a standing fact about this tree, and for trap 1,
which is a Scala editing hazard worth remembering.

- **`Checker.scala`** — `checkAgainstDefault` calls `resolveGuardedLadder` directly (drop the `Resolved`
  unwrap and its throw); `inferSpineApplications` ends `} yield built` (drop `hadDeferred`,
  `resolveDeferredSlot`, `assembleSpine`); delete the `SlotOutcome` sum type and collapse
  `CheckIO[SlotOutcome]` → `CheckIO[SemExpression]`, `SlotRecord.outcome` → `slotExpr: SemExpression`;
  delete `resolveDeferredSlot`, `assembleSpine`, `rebuildChain`, `genericArgSlot`, `isDeclarationGeneric`
  (`checkArgumentSlot` routes straight to `routeArgumentSlot`); drop the deferral arms of `defaultArgSlot`,
  `uniformPayloadSlot` (with `actualCarrier`) and `uniformCaptureSlot` (keep `doomed` as the `joinRoutable`
  guard); delete the `modeResolver` collaborator and the `recordLetObligation` site.
- **`CheckState.scala`** — the two obligation vectors, their scaladoc, `recordModeObligation`,
  `recordLetObligation`, `ModeObligation`, `LetObligation`.
- **`TypeStackLoop.scala`** — `processWithState` becomes `processIO(...).run(CheckState.initial)` (the
  `attempt`/fuel loop goes); `processIO`/`processValueMono`/`drainAndBuildQuoter` drop their `Either`;
  `runPostDrainResolution` returns `Unit` and its saturation becomes a plain ability fixpoint; delete
  `spliceRewrite` and the twin's "requested a mode-resolution restart" abort.
- **Delete** `ModeResolver.scala` and `RowElaborator.spliceResolvedModes`.
- **Then the scaladoc**: `EffectLifter`, `UniformCarrierChecker`, `CheckState`, `Checker` all describe the
  resolver in prose.

**Two traps, each of which cost time:**

1. Removing a `match` scrutinee's body leaves an **orphan `}`**, so the class closes early and every later
   method reports `Not found: force` / `Not found: lifter`. The error location is nowhere near the edit.
2. **The compiler-track arm of `resolveDeferredSlot` is a genuine §8 keeper on this tree** — switching it
   off costs the guard suite. An earlier run said otherwise and was stale; A.11.7-T's first known-unknown
   is therefore still open, and this is exactly the shape it warned about.

### A.11.Z.4 Method, added to A.9.4

- **Measure at *production* granularity, not by bypass.** A bypass probe answers "what breaks if this is
  off", which conflates every site it gates and tells you nothing about *which* one fired. A counter on
  each site that can produce the thing (here: each site that can create a deferral or an obligation), run
  over the whole gate **and** all 40 examples, answers "what fires, where, how often" — and that is what
  split A.11.Z.2's ambiguous "7 tests" into "zero on the runtime track, 14 on the compile track at two
  known source positions". It is also cheaper: one instrumented run instead of one run per switch.
- **A counter object is only initialised when something calls it.** The examples sweep printed nothing at
  first and that read as "no data" rather than "no fires" — add an unconditional `hit` at a site that
  always runs (the `Checker` constructor: 2,627 instantiations over 40 examples), so silence becomes
  evidence.
- **Gate every site of a mechanism, or the measurement lies.** The first `defer` probe missed
  `uniformPayloadSlot`'s deferral and reported the path cold while its *only* live producer was still on.
  The deletion then disagreed with the probe, which is how it was caught.
- **A deletion that makes something compile is a regression.** Two of that attempt's five failures were
  programs that must *not* compile and did. Check the direction of every failure, not just the count.
- The fast example sweep drives `com.vanillasource.eliot.eliotc.compiler.Main` (**not** `…eliotc.Main`)
  off `./mill show examples.runClasspath` with the three `--path` roots appended.
- `FullIntegrationTest`'s shared session returns the **previous** test's output when a compile fails, so a
  wrong-output assertion is usually a compile error in disguise — reproduce the shape standalone.

### A.11.Z.5 After the deletion

A.11.7 (the bridge), A.11.8 (obligations, the carrier side table) and A.11.9 (scaffolding and suites) are
**done** — and so is A.11.10 (docs closeout, A.11.10-1), which completes the roadmap; what is left is only
the separated pinned-`data`-field gap. **A.11.0's arithmetic is the number to check, and `check/` met it**: 3,873
against the ≈4,150 target and the pre-v2 baseline of 3,996. The machinery total landed at 6,091, above the
≈5,540 projection, because `row/` grew from 1,268 to 2,218 as the elaborator absorbed what the checker
used to decide; do not claim a net reduction against pre-v2, since that difference *is* `row/`, a phase
that did not exist.

## A.11.8 Delete the obligation path and the carrier side table

1. **Obligations** — **DONE 2026-07-28** (below). `ModeResolver`, `CheckState`'s obligation vectors, the
   `Suspended` outcome, every runtime deferral producer, `TypeStackLoop`'s splice-and-restart and fuel,
   `processIO`'s `Either` return, and `RowElaborator.spliceResolvedModes`. `TypeStackLoop` is a plain
   post-drain fixpoint again.
2. ~~**`Id`**~~ — **cancelled 2026-07-28** (§1 rule 4). `IdNormalizer`, `stripIdMachinery` and
   `assertNoIdResidue` **stay**: `Id` is the value of the empty row, written deliberately by the
   elaborator, so something must erase it before codegen and `assertNoIdResidue` is the proof that the
   erasure is complete. `Id` remains ordinary `data` with no `Suspend[Id]` — the soundness guard is
   unchanged. What was deleted, at A.8.10, is the checker-*manufactured* `Id` head; that is the thing v2
   was faulted for, and it is already gone.
3. **The carrier side table** — **DONE 2026-07-28** (A.11.8-3 below), and the measurement changed what the
   step *is*. The `effectCarrier` flag was written at one call site together with the carrier kind, so it
   never carried information the kind record did not: `CarrierRole` collapses into
   `Unifier.higherKindedMetas` and the queries become derived projections of it. The flag's readers are
   **not** dead — all three are the compiler track's inline guard (§8 by design); on the runtime track the
   arm is decision-free, which is A.11.4 confirmed independently. `CarrierKindChecker`'s HKT kind seeding
   and post-drain verification stay, as planned. `unify/CarrierRoleTest` → `HigherKindedMetaTest`.

This is also where the cornerstone guardrail is honoured by *not* acting: the `Unifier` gains nothing.

### A.11.8-1 Step 1 landed, and it did **not** need A.11.7 first (2026-07-28)

A.11.Z.2 carried A.11.7-R's reading that A.11.7 and step 1 are **one deletion**, because "the bridge
routes into the obligation path". Measured on this tree, that coupling is one-directional and already
severed: the bridge *had* two deferral producers, and both were dead, so they were deleted **from inside
the bridge** and the rest of the bridge stayed. Step 1 landed on its own.

**The measurement** (A.9.4's method, at production granularity — a counter on every site that can create a
deferral or an obligation, `ELIOT_DEFER_TRACE`-gated, dumped at JVM exit, scaffolding reverted). Whole
gate (`__.test`, 871 targets) **plus** all 40 examples, 2,627 `Checker` instantiations:

| producer | gate | examples |
| --- | --- | --- |
| `genericArgSlot` (declaration-generic slot) | 0 | 0 |
| `uniformPayloadSlot` hoist suspension (`payloadSlot/suspendHoist`) | 0 | 0 |
| `uniformCaptureSlot` doomed suspension | 0 | 0 |
| `defaultArgSlot`, **runtime** track | 0 | 0 |
| `recordLetObligation` | 0 | 0 |
| `defaultArgSlot`, **compiler** track | **14** (2 sites) | 0 |

The one live producer is the compile track, at exactly two positions in the compile-time `Either`
(`Either.els:21`) — the guard discharge the `GuardSignatureIntegrationTest` suite exercises, which §8
keeps by design. So the runtime half is not merely "cold at the arm A.11.7 names": **no runtime-track
deferral exists at all** since rule 4, which is what rule 4 says — every position classifies from its
declaration, so there is nothing left for an instantiation to decide.

**Deleted**: `ModeResolver` (213); `CheckState.modeObligations`/`letObligations` + their recorders and the
`ModeObligation`/`LetObligation` types; `SlotOutcome.Suspended`; `genericArgSlot` + `isDeclarationGeneric`
(`checkArgumentSlot` routes straight to `routeArgumentSlot`); `uniformPayloadSlot`'s suspend arm and its
`actualCarrier` probe; `uniformCaptureSlot`'s doomed suspend arm (`doomed` **stays** — it is the
`joinRoutable` guard); the `let`-obligation recording; `TypeStackLoop`'s splice-and-restart, its fuel, the
`Either` return threaded through `processIO`/`processValueMono`/`drainAndBuildQuoter`, the signature
twin's "requested a mode-resolution restart" abort, and `resolveModesAndAbilitiesToFixedPoint` (now
`resolveAbilitiesToFixedPoint`, a plain ability fixpoint); `RowElaborator.spliceResolvedModes` +
`maxRowBinderIndex`. Net **−572** lines. `check/` 5,097 → **4,622**, `row/` 2,223 → **2,126**, machinery
7,728 → **7,156**.

**Kept, deliberately**: `SlotOutcome.Deferred`, `resolveDeferredSlot`, `assembleSpine`/`rebuildChain` —
the compile track's Phase B, the §8 boundary. `resolveDeferredSlot` **lost its platform split**: the
runtime arm was the obligation recording, so what remains is one behaviour (adopt a bare-flex domain,
else run the ladder) for both tracks. A runtime shape reaching it — none does — gets exactly what it got
before the A.8.7 obligations existed, a unification decision, never a silent accept.

**Gate**: `./mill __.test` 871/871 green; 37/40 examples (`PluginA`/`B`/`C` unchanged); all 37 jars
**byte-identical in class content** to the pre-deletion build.

### A.11.8-2 What A.11.7 still blocks on — three shapes, unchanged by this step

The bridge itself is untouched, and A.11.Z.2's part-by-part table still stands (this deletion removed only
arms that fired zero times, so it cannot have changed what the bridge decides): the **carrier router** (1
test — `TerminationIntegrationTest` "loop endlessly"), the **uniform return boundary** (1 test —
`MonomorphicTypeCheckTest:540`, "Higher-kinded type parameter mismatch" at a pure return) and the
**payload router** (7 tests — the `catch`/`else` non-identity-handler pin, `eagerRowPinIntoDomain`). By
A.11.7's standing rule each firing arm is a **missing elaborator rule**, i.e. a stop-and-redecide, and
those three decisions are still open.

> **Closed 2026-07-28** — Robert decided all three as **one rule** (§1 rule 4's last bullet, §3.1) and the
> bridge is deleted. See [A.11.7-Y](#a117-y-the-three-blockers-diagnosed-2026-07-28).

### A.11.8-3 Step 3 landed — the flag was never an independent fact (2026-07-28)

**The step's precondition ("once nothing seeds or reads them") is only half met, and the measurement says
which half.** Three production sites read the `effectCarrier` flag; each was counted at outcome
granularity and then bypassed individually over the whole gate **and** all 40 examples (A.9.4's method,
`ELIOT_CARRIER_PROBE` / `ELIOT_CARRIER_OFF`, scaffolding reverted):

| reader | reads (gate) | bypassed ⇒ |
| --- | --- | --- |
| `EffectLifter.effectCarrierSplit`, meta-headed arm | 733 runtime / 41 compiler | 3 `GuardSignatureIntegrationTest` + its own 2 unit tests; **examples 37/40, every class file byte-identical** |
| `GuardDischargeResolver.isGuardCarrier` | 11 | 5 `GuardSignatureIntegrationTest` |
| `Track.Compiler.pinInferredReturnCarriers` | 11 call sites, 18 metas pinned | 4 `GuardSignatureIntegrationTest` |

**So the flag's entire live surface is the compiler track's inline guard** — `if..else..raise`, whose
carrier is introduced through instantiated combinators with no declared binder to key off. That is the §8
boundary the plan keeps by design, so the table cannot be deleted outright; the honest reading of the
step's own "once" clause is that this half stays. The runtime track's 733 reads are **routing, not
decisions**: with the arm off the whole example corpus compiles to byte-identical classes. That is
independent confirmation of A.11.4 — since the elaborator writes the carrier, a runtime-track carrier is
never a metavariable.

**What the measurement did settle is that there is no "carrier role" to record at all.** `CarrierRole`'s
two fields were written at **one** call site, unconditionally together (`CarrierKindChecker`'s
`case _: VPi`), so `isEffectCarrier(id)` was never anything but "this meta was peeled from a higher-kinded
binder" — the fact the kind aspect already records. The side table therefore collapses to the kind map the
plan preserves:

- `Unifier.carrierRoles: Map[Int, CarrierRole]` → `higherKindedMetas: Map[Int, (SemValue, Sourced[String])]`;
  `CarrierRole` and `updateCarrier` are gone.
- `recordCarrierKind` + `recordEffectCarrier` → one `recordHigherKindedMeta` (also on `CheckState`).
- `isEffectCarrier` → `isHigherKindedMeta`, `effectCarrierMetaIds` → `higherKindedMetaIds` — derived
  projections, each documented at the reader that keys on it and why a higher-kinded meta is the only
  metavariable shape that can stand for a carrier.
- `unify/CarrierRoleTest` → `unify/HigherKindedMetaTest` (A.11.9 listed it for deletion *with the table*;
  the table survives in reduced form, so it is rewritten to what survives — the two cases asserting the
  flag as a distinct fact stop existing). `CarrierBookkeepingTest`'s probe counts higher-kinded metas; its
  A.11.4 acceptance assertion (**zero** carrier metas for `def echo: {Console} Unit`) is unchanged.

**Gate**: `./mill __.test` green — 1,488 tests (1,489 − the one case that stopped existing), 0 failures;
37/40 examples; every program's output **and class content** byte-identical to the pre-step build.
`check/` 3,879 → **3,873**, `unify/` 554.

**What A.11.8 still owes**: nothing — step 1 is done, step 2 was cancelled (§1 rule 4), and step 3 is as
far as it goes while the compile track keeps its inferred carrier. If the compile track is ever converted
(§8), `higherKindedMetas` loses its last non-kind reader and the projections go with it.

## A.11.9 Remove the experiment scaffolding and fix the test suites

**DONE 2026-07-28** — see [A.11.9-1](#a119-1-what-landed-2026-07-28) for what landed and the two findings.

- **`seedFacts`**: `CompilationSession.compileOnce`'s optional parameter exists only for the R4 shadow
  compile. Remove it with the shadow harness — a production API kept alive by one test is exactly the
  residue this section exists to catch.
- **Delete**: `RowElaborationShadowCompileTest`, and the shadow half of `RowShadowSweepTest` (its corpus
  moved to `EffectCorpus` at A.11.3a).
- **Delete with their machinery**: `carrier/CarrierMechanismTest`, `check/UniformCarrierCheckerTest` —
  both **done** at A.11.7. `check/CarrierBookkeepingTest`, `check/EffectLifterTest` and
  `unify/CarrierRoleTest` were listed here too, but their machinery did **not** all go: the probe's
  A.11.4 acceptance (zero carrier metas for `def echo`) and the lifter's surviving pure-wrap/`bindWrap`
  arms are live behaviour, and the role test became `unify/HigherKindedMetaTest` (A.11.8-3). Keep them;
  re-check at A.11.10 that each still asserts something the end state has. **Re-checked at A.11.10 — all
  three do**: `HigherKindedMetaTest` covers the reduced record and its one live handle (the compiler
  track's inline-guard carrier); `CarrierBookkeepingTest` covers ambient-carrier recording *and* the
  A.11.4 acceptance (no carrier meta is minted for an ability method, because the elaborator wrote it);
  `EffectLifterTest` covers exactly what survives — `effectCarrierSplit`, the two doomed-postponement
  probes, `tryPureWrap` (including the two non-wrap cases that keep it from double-wrapping) and
  `bindWrap` for the `let` rule. No case asserts deleted machinery.
  **`channel/IdNormalizerTest` stays** — its machinery does (§1 rule 4); if anything it needs *adding* to,
  since `Id` is now written on purpose rather than manufactured.
- **Rewrite**: the lift group of `MonomorphicTypeCheckTest` (its generic-slot shapes assert the deferred
  v2 spellings, which stop existing) and `RowElaboratorTest`'s twins (explicit carrier args).
- **Rename and keep**: `jvm/.../UniformCarrierCompileTest` (244) and `UniformCarrierConditionalTest`
  (101) are v2-*named* but are behaviour gates over the real base layer. Rename to something the end
  state can justify and keep every program.

### A.11.9-1 What landed (2026-07-28)

Gate: `./mill __.test` green, **1,487 tests / 0 failures** (1,488 − 3 shadow cases + 2 new); 37/40
examples; every program's output **and class content** byte-identical to the A.11.8 build.

- **`seedFacts` is gone** from `CompilationSession.compileOnce`, with its only caller. `grep -rin
  seedFacts lang/src jvm/src eliotc/src ide` → empty, closing that exit criterion.
- **Deleted**: `RowElaborationShadowCompileTest` (the R4 fact-injection experiment) and
  `RowShadowSweepTest` — *all* of it, not half: its assertion was "the standalone row checker agrees with
  v2 on every demanded definition", and neither side of that comparison exists any more (v2 is deleted;
  the row check is wired into the pipeline as a codegen precondition, A.11.6, so a leak fails the compile
  before any sweep could see it). `FullIntegrationTest.runJar` went back to being private — it had been
  lifted to the companion for the shadow compile.
- **`EffectCorpus` kept, and given a real consumer.** Its own doc says it is a fixture that must outlive
  the harnesses, but deleting both would have left it unreferenced. `EffectCorpusIntegrationTest` now
  compiles and runs it through `FullIntegrationTest` and **pins the output**, which is strictly more than
  the shadow compile asserted: that experiment compared run B against run A, so what the corpus *prints*
  was never written down. (Measured, not guessed — the third line is `unparseable`, from `parsed`'s own
  `raise`, where a plausible guess is `boom`.)
- **`RunBoundaryFunction` retired.** The fact and its processor had no demander left (the bridge was the
  reader); what the pipeline actually uses is the *configuration key*, which the row phase reads. It moved
  to `row/RunBoundaryFunctions` — a plain object next to its consumer, no longer pretending to be a fact
  — and `LangProcessors` lost the tag processor.
- **Renamed, every program kept**: `UniformCarrierCompileTest` → **`EffectShapeCompileTest`**,
  `UniformCarrierConditionalTest` → **`CarrierSlotCompileTest`**. Both headers now say what the programs
  gate rather than which mechanism they were written against; the per-program comments keep the mechanism
  history, which is *why* each shape is there.
- **The lift group of `MonomorphicTypeCheckTest` and the `RowElaboratorTest` twins** needed no assertion
  changes — A.11.7-T had already moved them to the rule-4 spellings. What was stale was the *reasoning*:
  the group header and four cases still explained themselves in terms of A.8.6 deferral, mode obligations
  and the A.8.7 resolver. Rewritten to the rule that decides them now, with the case names re-pointed at
  the shape rather than at "deferred".
- **A latent harness defect fixed**: `MonomorphicTypeCheckTest`'s `Effect` stub declares its instances at
  the stub `IO` without importing `eliot.jvm.IO` (A.8.12 spotted this and left it, since nothing demanded
  the module). It does now.
- **Measured and reverted**: giving that stub `.` the real `stdlib` signature (`f: A => {Effect} B`). With
  `import eliot.carrier.Effect` added to the stub module, the name `.` stops resolving in the snippets
  ("Name not defined." at `readLine.f`) — a property of that stub universe, not of the language (the real
  declaration compiles everywhere, and the rowed function slot is covered by the jvm suites and the
  `DotOperator` example). The comment now records the attempt so it is not silently retried.

## A.11.10 Docs closeout

**DONE 2026-07-28.** The plan, and what each item turned into:

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

### A.11.10-1 What landed, and the one prediction that was wrong

- **The cornerstone is rewritten** and renamed *Effects Are a Channel (Rows In, the Carrier Written)*. It
  now states the four user rules, the written carrier and its three completing rules, the anti-accretion
  whitelist as binding on future changes, the one pure-lift rule left in the checker, and the two
  verifiers. CLAUDE.md's pipeline list gained the missing **`row` phase** as an entry of its own (it had
  described elaboration as living in the checker), the `monomorphize` entry lost the auto-lift/bridge
  description, and the `effect` entry's stale `EffectLifter.tryIdDefault` sentence became the elaborator
  writing `Id`/`runId`.
- **One planned item was measured wrong and NOT done: "any new consumer must Id-normalize first" does not
  stop being true.** The plan item was written before the `ρ := {}` decision (2026-07-28) that *kept*
  `Id`: because the elaborator now writes `Id` and `runId` deliberately, a consumer of `MonomorphicValue`
  or of a mid-mono `SemExpression` still sees them, and `IdNormalizer` still erases at the `WovenValue`
  seam. The tax is unchanged and stays documented in both the cornerstone and the skill; what *did* stop
  being true is the *encoding* it used to pay for (a manufactured `Id` head on every pure judgment).
- **The skill** lost the `carrier/` package, `UniformCarrierChecker` and `DeclaredPureChecker` from its
  file map, lost the Phase-A/B narrative (deferral is now described as the compile track's decision), and
  had its carrier-bookkeeping section rewritten around `Unifier.higherKindedMetas`. Its "effect channel"
  section and two anti-patterns were re-pointed: *making a bind/`pure`/`Id` decision inside the checker*
  and *re-introducing carrier inference* are now the things to reject in review — the exact inverse of the
  v2-era anti-pattern that told the reader elaboration belongs in check mode.
- **`docs/effects-as-channel.md` is retired to a signpost**: a §-by-§ map from every v2 section to its live
  successor here, what v2 got right and kept, and what it got wrong. The map exists because ~80 scaladoc
  comments still cite `effects-as-channel §N`; those citations now land on something true instead of on a
  superseded design, and no mass comment sweep was needed. The full text is in git
  (`git show c9dcd53b:docs/effects-as-channel.md`).
- **This document**: the status banner is now a closeout, §6.1/§6.1-A read as settled rather than pending,
  §3's "pure code is untouched" no longer contradicts written `Id`, §4's `RunBoundaryFunction` entry
  reflects A.11.9's retirement to a configuration key, and §9 carries the pinned-`data`-field gap as an
  open question rather than leaving it only in the appendix. The appendices stay intact as the record —
  standing rule 1 makes them the *history* of the decisions, not a second statement of them, so folding
  meant making §§1–7 stand alone, not deleting what happened.
- **`IfDemo` is confirmed**: it compiles and runs, and its output is what the file's comments describe —
  the constant-`true` section takes every "then" branch, `sign(true)` prints `+`, `describe(false, true)`
  prints `second`, and the runtime section follows stdin (`yes` ⟹ the true branches, anything else ⟹ the
  false branches, including `3. chain: second` and `6. plain else`). Both `demo`'s six forms and
  `foldForms`' three run correctly, so the `{Abort}`-discharge-through-`else` and the suspended `fold`
  arms behave as the example teaches.

## A.11.11 The pinned-`data`-field gap, closed (2026-07-28)

Not part of the A.11 roadmap — the defect A.11.7-Y separated out and the closeout carried as §9 item 4.
Fixed the way that item predicted, in `EffectSugarDesugarer`:

**The defect.** `CoreProcessor` desugars a `data` definition's field rows *before* `DataDefinitionDesugarer`
splits the data into functions, and that pass collapsed **both** row forms. For an *open* field row the order
is required — the recovery lift mints a carrier generic, and only the un-split definition can give it a home
on the *type* (splitting first leaves `F` bound on the value constructor alone, an unbound free variable at
every use). For a **pinned** field row it is exactly wrong: a pinned row introduces no generic, so it has
nothing to place on the type, but it *is* a carrier-stack capture position — and the tag recording that
(`pinnedParameterEffects` on the value constructor, `returnPinnedEffects` on the accessor) is computed per
*function*, from a position still spelled as an `EffectfulType`. Collapsing it first erased the spelling
before any function existed, so both came out untagged and the elaborator hoisted the stored computation
onto the ambient carrier instead of capturing it.

**The fix.** The data-level pass rewrites open rows only: it gates on `openRows.isEmpty` (was `rows.isEmpty`)
and passes `rewritePinned = false`, a `rewrite` mode that leaves a pinned node standing while still
descending into its parts, so no *open* row escapes to the per-function pass. Everything else is unchanged —
including the recovery path for the illegal open field row, which still reports "A stored effect row must be
pinned to a base carrier."

**Measured.** Before: `data Holder(computation: {Abort | IO} Unit)` + `Holder(if(true, printLine("stored")))`
failed with `Expected: {Abort | IO} Unit / Actual: IO(Unit)` — fail-safe, but the shape was unusable. After:
it compiles, and prints `before` then `stored`, i.e. the stored effect runs at the accessor, not at
construction (§1 rule 3). Also verified: generic data (`data Task[E](name: String, run: {Throw[E] | IO}
String)`, the pinned entry naming the data's own binder), a pinned field at index 1, and a multi-constructor
data destructured through `match`. Gate `__.test` **871/871**, 37/40 examples, and **every example's class
content byte-identical** to the pre-fix build.

**Adjacent limitations this measurement mapped, both pre-existing and neither introduced here** (verified by
reproducing them on the pre-fix tree, and with no `data` involved):

- a **pure** actual at a pinned slot is rejected (`Task("ok", "value")` ⟹ `Expected: {Throw[String] | IO}
  String / Actual: String`). That is R5 corollary 3 — pinned captures deliberately never boundary-wrap, which
  is what preserves the curated val-bound-discharge diagnostic. §1 rule 2's "pure arguments fit" is about
  *suspended* (open-row) slots, not pinned ones, so there is no rule conflict here — but it is the natural
  next question if stored computations get more use.
- an **inline effectful call** at a pinned slot is rejected unless it goes through a *declared* row:
  `Task(raise("boom"))` reports "performs the effect 'Throw' but does not declare it", while
  `def bad: {Throw[String]} String = raise("boom")` + `Task(bad)` compiles and runs. The same happens with no
  data at all (`printLine(raise("boom") catch (…))` fails identically on the pre-fix tree), because the
  elaborator instantiates a pinned row's ability arguments from the argument's **declared** row (§3.1), and
  `raise[E]`'s declared row is its own binder `{Throw[E]}`.

## A.11.12 A row is not a carrier — the pure-actual rule (2026-07-28)

**Decided by Robert, implemented the same day.** Recorded in §1 rule 2 (the decision) and §6 (the two
signature changes); this section is the record of how it was measured and what it cost.

**The question.** A pure actual was accepted at *some* carrier-headed slots and not others: `fold(true,
"a", "b")` and `hold("value")` (a `G[_] ~ Effect` parameter) lifted it, `data Box(value: IO[String])`
lifted it, and a pinned `{Abort | IO} String` rejected it. Robert's reading — *`{Console} A` says there is
a **potential** effect, so a pure value belongs; `G[A]` is a **type**, and "which `G`, and what if it has
no `pure`?" has no answer the caller gave* — makes the pinned rejection correct and the two lifts wrong.

**The measurement that settled it.** The acceptance was *constraint-dependent*:

```
def hold[G[_]](x: G[String], label: String): String = label
hold("value", "built")   ⟹ "Cannot resolve type — contains unresolved variable"

def hold[G[_] ~ Effect](x: G[String], label: String): String = label
hold("value", "built")   ⟹ compiles, with an inserted pure
```

`~ Effect` is declared for the *callee's body's* sake, so the calling convention turned on something the
signature does not say — the §0 implicitness this design exists to remove — and its failure mode when
absent was a quoter crash rather than a type error.

**What was built.**

1. **`{Effect}` reuses the signature's own carrier** (`EffectSugarDesugarer`): a definition binding exactly
   one `Effect`-constrained higher-kinded binder collapses its rows onto *that* binder, merging the row's
   entries into its constraints; none, or more than one, mints as before. Measured first: **no signature in
   the tree mixed the two spellings**, so this changed the meaning of nothing that existed. It is a desugar
   rule, not surface — Robert's constraint was "no new syntax".
2. **`else`/`catch` re-spelled** to `fallback: {Effect} A` / `onError: E => {Effect} A`. Same types, same
   emitted code; the slots now *declare* what they always meant.
3. **The lift revoked in the elaborator** at every carrier-typed slot that is not row-tagged: the
   carrier-binder arm, the carrier-codomain handler arm (which now elaborates such a lambda *naturally*
   instead of forcing a `pure`), and the concrete-carrier arm.
4. **Two diagnostics, not one.** `RowElaborator.Violation` now carries its own help text, because the two
   directions read oppositely: a computation at a rowless slot (rule 4) and a plain value at a carrier-typed
   one (rule 2). The second is reported *by the elaborator* rather than left to the checker, whose one
   surviving pure-lift arm fires on any rigid carrier-headed expectation and would have accepted
   `Box("pure")` on the ambient `IO` alone.

**What was deliberately *not* done.** `EffectLifter.tryPureWrap` stays. Disabling it was measured
(env-gated, reverted): **3 lang unit cases + 7 jvm shapes** across four suites still depend on it, so it
remains the checker's one order-free local rule exactly as §4 says. The rule above is enforced where the
declaration is known — in the desugar — and the checker's arm is the fallback for what the elaborator does
not classify.

**Fixtures corrected** (§1: a test pinning a rule violation is a defect in the test): `RowElaboratorTest`'s
stub discharger now mirrors the real one (`handler: Str => {Effect} A` over `G[_] ~ Effect`) — 7 twins were
pinning the old lift — and `ExamplesIntegrationTest2`'s "a pure value into a generic effect-carrier
parameter should auto-lift via pure" became a **rejection** test, joined by a new one showing the row
spelling accepting a pure *and* an effectful argument at the identical position. Five `CoreProcessorTest`
cases pin the reuse rule (reuse, signature-identity with the hand-written `G[A]`, constraint merging, and
both decline cases).

**Gate**: `__.test` 871/871, and **all 40 examples byte-identical in class content** to the pre-change
build — the re-spelling is transparent to every existing call site.

**Known residue, unchanged by this**: a `data` field has no row spelling (rule 3 requires a stored row to be
pinned, and pinned is concrete), so storing a *pure* value in a computation field must be written
`Box(pure(x))`, which needs `import eliot.carrier.Effect`. Declaring the field at its payload type is
usually the better answer.

## A.11.13 A pinned row spelled as a type alias (2026-08-01)

Two defects, both the tail of A.11.12's sibling change (`a669f530`, "read a pinned return as a carrier"),
both found by an out-of-tree project — a test framework whose suite type is written once as an alias:

```eliot
type Test = {Writer[List[TestCase]] | Id} Unit

infix none below should def in(testCase: TestCase, body: {Throw[AssertionError] | Id} Unit):
   {Writer[List[TestCase]] | Id} Unit = tell(append(empty, TestCase(...)))

def testCases: Test = { "shouldBe" should "…" in { … }  ...  }
```

**1. The alias hid the pinned tag from `topRegionCarrier`.** A `type X = …` is a `FunctionDefinition`
whose *return position is the kind `Type`* and whose **body** is the type it denotes, so
`pinnedRowEntries(function.typeDefinition)` — which reads the return position — recorded nothing for it,
and `def testCases: Test` therefore got `RegionCarrier.Absent`. Meanwhile every statement in the block is a
saturated call to `in`, whose pinned return `a669f530` had just started reading as `Kind.Carrier`. Caller
and callee disagreed about the one carrier they share: the definition read pure, so its block tail took the
`Id` boundary and came out `runId`-wrapped, while each statement was a computation on the `WriterCarrier`
stack the alias names. The symptom is the one [[gotcha_parameter_kind_needs_declared_type]] warns about —
`Expected:` and `Actual:` rendering **identically** (`{Writer[List(TestCase)] | Id} Unit` both sides),
because the renderer erases the `Id` the mismatch is made of.

The tag is now recorded for a type-level definition from its body — `EffectRow.aliasPinnedEffects`, kept
**apart** from `returnPinnedEffects` so an alias is never mistaken for a value returning a computation —
and `topRegionCarrier` reads it through `expandAlias`, the *one level of type-alias expansion* §3.2 already
grants (the same expansion `asArrowLike` was doing inline for `=>`; it is now one shared helper).

**2. A reified pinned computation could not be stored.** With a pinned-returning call classified
`Kind.Carrier`, rule 4's rowless-slot check rejected `append(steps, one)` for
`def one: {State[S] | Id} Unit` — which is exactly the `List[TestCase]` use §1 rule 3 *sanctions*: a pinned
row is a reified computation **and** an ordinary type, so handing one on is handing on data. The exemption
already existed for a pinned *parameter* (`isCapturedValue`) and now covers the saturated call too;
it also had to move out of the first arm, since the second — "it lands on the type parameter `A`" — was
reporting the very same value one message later.

**Now closed (A.11.13-A).** The "Not done" gap below is fixed: `declaredResultKind` and the pre-mono
verifier now read the pinned tag through the alias too. There turned out to be **two** consumers of the tag
that the definition-side fix (`topRegionCarrier`) had left behind, and a callee spelling its pinned return
through an alias needs both:

1. **Elaboration** (`RowElaborator.declaredResultKind` → `returnsPinnedAlias`): a saturated call to such a
   callee is now classified `Kind.Carrier`, matching a direct pinned return, so it is not `pure`-wrapped at
   the carrier position it in fact returns. This is what lets a *block sequence* calls whose own return is
   the alias (the call-site twin of A.11.13's aliased-definition block).
2. **Verification** (`RowChecker.pinnedReturnEntries` → `aliasPinnedReturnEntries`): such a return is a
   declared capture, so the alias's pinned abilities now count as *declared* — otherwise the pre-mono
   `derived ⊆ declared` check reported the captured effect as an undeclared leak at the definition. Only the
   ability identity is read (one universe lookup, no substitution), and `pinnedRowEntries` records only a
   *top-level* pinned body, so a nested pinned row (an arrow codomain) never leaks in.

Both mirror how `topRegionCarrier` already reads the tag; the post-mono `EffectAccountingProcessor` needs no
change, because such a definition's carrier is written concrete (`StateCarrier[…]`) and so is not an
open-row value it checks. Guarded by one new `ExamplesIntegrationTest2` case (a block sequencing calls whose
pinned return is spelled by an alias) and three `CoreProcessorTest` cases for the open-row-alias rejection
below.

**Superseded — was: Not done, and fail-safe.** `declaredResultKind` read only the *syntactic* pinned tag, so
a callee whose pinned return was spelled through an alias read as a payload and was `pure`-wrapped at a
carrier position — a hard checker error, never a silent re-route. (Kept for the record; closed by
A.11.13-A above.)

**A.11.13-A also fail-safes the open-row alias.** An *open* row in a type-alias body
(`type Susp = {Suspend} Unit`) lowers its carrier onto the alias's own generics, which a definition naming
the alias cannot reach — the effect was silently dropped (or misreported as a leak). Only a pinned row is a
type (§1 rule 3), so this is now rejected at core (`EffectSugarDesugarer.rowErrors(FunctionDefinition)`,
mirroring the `data`-field open-row rule), pointing the author at pinning or declaring the effect on the
definition.

**Gate**: `__.test` green, including two new `ExamplesIntegrationTest2` cases (the aliased block, and the
stored pinned computation), and all **39** example jars byte-identical to the pre-change build — jars being
reproducible since 2026-07-31, the comparison is now an `md5sum` of the jar itself. Cold-cache compile time
is unchanged (2.46–2.55 s over three runs either side), so the added universe lookup costs nothing
measurable.
