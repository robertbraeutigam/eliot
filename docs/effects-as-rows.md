# Effects as Rows, v3: Declared Suspension + a Desugared Elaboration

Status: **DESIGN — not landed.** Successor direction to `docs/effects-as-channel.md` (v2, the live
implementation). Decision pending a spike (§8 R1). v2 stays the live, green default throughout; nothing
in this document changes behaviour until the migration plan (§8) is executed.

**One-sentence summary.** Make suspension *declared* in signatures instead of inferred from genericity;
then effect elaboration (where `flatMap`/`pure`/thunks go) becomes a syntax-directed **desugar phase**
that runs before checking, effects check as a **row channel** beside the type (the same architectural
move as the Int-bounds refinement channel), and the NbE checker returns to checking only — with no
effect machinery in it at all.

## 0. Why revisit v2

v2 (uniform carriers) is internally coherent and its engineering discipline held; this section records
the evidence that it is a local optimum — optimal *given* two premises, both of which this design
revisits.

**The measured cost.** The effects-as-channel effort grew `monomorphize/check/` by ~1,185 lines (+30%),
`Checker.scala` alone by +581 (+58%; ~63% of its 1,575 lines are now effect/carrier work), plus ~900
lines of new carrier/Id machinery (`UniformCarrierChecker`, `CarrierJoin`, `UniformLadder`,
`IdNormalizer`). Of the ~2,750 effect-related lines in the checker layer, ~36% decides *where* binds
go, ~29% exists to *prevent carrier metas from being wrongly unified* (theft), and the two are
entangled: nearly every insertion arm is ordered the way it is to dodge theft.

**The bug record.** Of the 15 fix commits in the v2 window, the four highest-user-impact ones
(`2a821837`, `5864f95f`, `ba208c48`, `9c1a3f29` — including both dot-operator bugs) are one failure:
a carrier metavariable captured by ordinary first-contact unification, because nothing distinguishes a
carrier constructor from a data constructor in the unifier. The guard family is documented as
uncompletable (`project_compound_state_effect_lift_fix`), and the latest theft fix landed five days
after the join solver "closed" the class. A further 3 fixes were the Id-transparency tax (every
`MonomorphicValue`/`SemExpression` consumer must Id-normalize first).

**The two premises v2 is optimal under:**

1. **Carriers are ordinary types inside the one unsorted unification space.** `IO` and `List` are the
   same kind of thing to the unifier (`CarrierKindChecker` flags *every* `Type -> Type` binder as a
   carrier, deliberately unfiltered — the "tag" is kind arity, not a sort). Consequence: the theft bug
   class, the join solver, the pinned tags, the ladder orderings.
2. **Suspension is inferred from genericity per instantiation.** A generic slot receives the suspended
   action; a concrete slot runs it. The proof this is genuinely undecidable from signatures is the
   `choose`/`pick` test pair (`MonomorphicTypeCheckTest` "pass an effectful eliminator branch through
   unsequenced" vs "lift a deferred flex slot once a later argument rigidifies it"): identical callee
   shapes, and the bind decision is made by the *sibling argument's type*. This is implicit
   evaluation-order polymorphism — whether `readLine` *executes* at a call site depends invisibly on
   how generic the callee is. It is also why v1's post-mono weaver was unrecoverable, and why v2 had to
   fuse elaboration into check-mode.

An audit of all eight elaboration decision sites found exactly **one** is irreducibly
instantiation-dependent — the generic-slot suspension decision (premise 2). Everything else either
dies with a carrier/value separation (the theft class, the equal-arity arm, the spurious-HKT-flag
fallback) or is decidable from declared signatures (ride-up-vs-bind via `occursInValue`, the `let`
rule). Remove premise 2 by *declaration*, and premise 1 can be removed too, because the checker no
longer needs carriers at all.

Two further observations seal it:

- `CarrierJoin` (Id = bottom, one non-`Id` winner, conflict = mismatch, unsolved = `Id`) **is row
  unification**, rebuilt inside the carrier representation. Together with `EffectRow`'s pinned tags,
  `ambientCarriers`, and `metaConstraints`, v2 already contains a de-facto row calculus — scattered.
  v2's §10 rejection of a row calculus ("a fourth representation") predates this evidence: v3
  *consolidates* four scattered representations into one, it does not add a fifth.
- The dot operator (`def .[A,B](a: A, f: A => B): B`) puts every chain's subject into a bare generic
  slot — so the most common operator in the language permanently routes through the most fragile
  machinery (Phase-B deferred slots, theft-prone rigidified unification). Under v3 the dot operator is
  effect-irrelevant by construction.

## 1. The user model (three rules)

1. **Effects run where they are written.** An effectful expression in any plain position performs its
   effects at that position; they join the enclosing definition's row. Strict call-by-value, always,
   regardless of the callee's genericity. `pick(readLine, "x")` and `choose(readLine, readLine)` both
   run their reads at the call site.
2. **Suspension is declared.** A parameter that must *not* run its argument declares an open row:
   `whenTrue: {G} A` receives the computation unrun. `if[T](c: Bool, value: {Abort} T)` already spells
   this — v3 makes the syntax mean what it looks like it means. Pure arguments fit suspended slots
   (`{} ⊆` anything).
3. **Pinned means captured** (unchanged from v2). `{Throw[E] | G} A` is a reified computation — an
   ordinary type, usable in `data` fields, discharger parameters, `List[TestCase]`. Open rows never
   appear in types; pinned rows are the only place a type contains a computation.

Consequences the user sees:

- `something.foldLeft(f, z)` with `something : {Console} List[T]` **just works with zero declaration
  on `foldLeft`**: the effects run, the `List[T]` payload flows, `Console` joins the caller's row. The
  entire collections/data library is effect-oblivious.
- Evaluation order is readable from signatures: a suspended slot says "may not run / may run later /
  may run repeatedly"; everything else runs exactly once, here.
- Rows remain the only effect surface (`def main: {Console} Unit`); diagnostics stay in payload/row
  vocabulary. Nothing else about the surface changes.

## 2. The checking model: two channels beside each other

Checking a runtime term yields a **payload type** (the existing NbE judgment — which never sees an
effect) and a **row** (a second output, exactly as an `Int`'s range lives in the refinement channel
beside the type, not inside it).

- **Row constraints are set-shaped**: union for sequencing, inclusion for boundaries
  (`derived ⊆ declared` at a definition with a declared row). Row variables (from
  effect-polymorphic signatures) solve by union — **commutative and order-independent**, so no
  argument-order or sibling-order sensitivity can exist. There is nothing to steal: a row variable
  unifies only with rows, never with a type meta.
- **Rows attach to arrows** as latent rows (`A => {Console} Unit` — existing surface). An unannotated
  arrow in a signature defaults to a fresh row variable, making higher-order functions
  row-polymorphic by default (`map(f: A => B, …)` accepts an effectful `f`; the latent row threads to
  `map`'s own row). This defaulting is a mechanical sugar rewrite, not a checker decision.
- **Where it runs**: per definition, post-operator-resolution (the row of each callee is read off its
  resolved signature; suspension/pinned modes likewise). Note this is decidable *now* precisely
  because discharge is visible syntactically: a pinned-slot capture removes the argument's row from
  the caller's derivation by declaration — the reason the old pre-mono `effect/` phase failed (it
  could not see structural discharge) is gone, because capture is declared, not discovered.
- **The `Effect` machinery marker** (`action: A => {Effect} Unit`) reads as "the ambient row variable
  of the enclosing signature" — the same meaning it has today, expressed as row-variable sharing.

The **post-mono accounting verifier stays** (`EffectAccountingProcessor`, `derived ⊆ declared` at
ground mono keys, codegen precondition). The per-definition row check gives early, well-located
diagnostics; accounting remains the exactness fail-safe at the use site, per the use-site-verification
cornerstone and the gaps-must-be-fail-safe rule. If experience shows full redundancy, retiring one is
a later, separate decision.

## 3. Elaboration: a desugar, not a checker mode

A new phase (evolving `EffectSugarDesugarer`; runs with the other desugars, after operator resolution
and the row check) rewrites each definition into **fully explicit monadic core Eliot** — the same
shape v2's checker *output* has today, so monomorphization, ability resolution (`Effect[IO]`),
`used`/`uncurry`, the jvm backend, `runMain`, and the synthetic main are unchanged consumers:

- A definition with declared row `{Console} Unit` gets its carrier binder (`F[_] ~ Console`) exactly
  as today's desugar mints it.
- Every effectful call in a strict position becomes a `flatMap` chain (`$eff$N` binders — reuse v2's
  `wrapBinds` conventions as the spec); `val x = <effectful>` binds, as today.
- A suspended-slot argument is passed as its carrier-typed computation, unrun.
- A pinned-slot argument is captured whole (unchanged).
- **Pure code is untouched.** A definition with an empty row elaborates to itself — no `Id`, no
  wrapper, nothing to erase. The Id-uniform apparatus (`IdNormalizer`, `stripIdMachinery`,
  `assertNoIdResidue`, the per-consumer normalization tax) has nothing to exist for.

Every placement decision reads *declared* information (slot modes, callee rows) — no types needed, no
instantiation-dependence, hence a desugar. The checker then checks the elaborated program as ordinary
code: `flatMap` is an application like any other. **This is not v1's weaver**: v1 erased and tried to
*reconstruct* placement post-mono with no signal; v3 never erases — the signal is in signatures, read
before checking.

**The `Id` residue question** (discharge under a pure return: `def sign(f: Bool): String =
if(f, "+") else "-"` — the discharge region needs *some* carrier). Recommended resolution: the desugar
knows the residual row is empty, so it instantiates the region's base carrier to `Id` and inserts
`runId` at the boundary — statically placed, syntax-directed, only inside declared discharge regions,
never around ordinary pure code. A small boundary erasure at codegen (a trivialized `IdNormalizer`)
removes it. `Id` keeps its "no `Suspend[Id]`" safety property. To be verified in the spike (§8 R1).

## 4. What is deleted, what stays, what is added

**Deleted (checker layer, ≈2,000+ lines):** `EffectLifter` (433) with all arms
(`mustLiftBeforeUnify`, `tryBindLift`, `tryPureWrap`, `tryIdDefault`); `UniformCarrierChecker` (414);
the `carrier/` package (404: `Carrier`, `CarrierJoin`, `UniformLadder`); `IdNormalizer` (308) +
`PostDrainQuoter.stripIdMachinery` (or trivialized per §3); `DeclaredPureChecker` (subsumed — 
"declared pure but performs an effect" becomes an ordinary row-subset failure with a good location);
`CarrierKindChecker`'s carrier-role seeding; and inside `Checker.scala` the ladders
(`resolveLadder`/`resolveFailureLadder`), Phase A/B deferral (`SlotOutcome.Deferred`,
`resolveDeferredSlot`, `sequenceBeforeUnify`, `deferredGenericDefault`), all four pinning mechanisms
(`eagerRowPinIntoDomain`, `recordRowArgumentPins`, `findCarrierLayerSlots`,
`applyPendingCarrierPins`), the slot routers (`uniformPayloadSlot`/`uniformCaptureSlot`/
`uniformCarrierSlot`/`payloadFitsDomain`), and the effect branch of `typeImmediateLambda` — the
checker ends with zero effect code, below the pre-v2 baseline.

**Stays:** `EffectRow` facts and the pinned tags (`returnPinned`/`pinnedParameterIndices` — consumed
by the desugar instead of the checker); pinned-row surface and semantics; the dischargers and the
`eliot.carrier`/`eliot.effect` packages; `EffectAccountingProcessor` + `MonomorphicValue.ambientCarriers`
(fail-safe verifier); `EffectRowRendering`/`GroundValueRenderer` (pinned stacks still render as rows);
`RunBoundaryFunction`; the `Inf` story (an ordinary row entry, unchanged); the compile-track `Either`
discharge (§5).

**Added:** the row checker (row inference + subset check per definition; rows are sets — est. 300–500
lines, own package, e.g. `lang/.../row/`); the completed elaboration desugar (est. 300–400, comparable
to `matchdesugar`); arrow row-defaulting in the sugar phase.

## 5. Interactions

- **Compile-time track (v2 §8).** Elaborated output is ordinary core Eliot; the compile track already
  evaluates explicit monadic code (the `Either` discharge for effectful signatures, `Effect[Id]`).
  Expectation: both tracks consume the elaborated form with no special-casing; the type language and
  signature evaluation stay carrier-free as before. Verify in the spike.
- **Monomorphization.** Mono keys keep today's shapes (suspended slots still instantiate generics at
  carrier types, `fold`'s `A := AbortCarrier[G, T]` via `if`); use-site verification unchanged.
- **Diagnostics.** Row errors are per-definition with the user's own location — this fixes the
  `Suspend`-at-`Id` mislocation (v2 §10 item 16) for free, and replaces the cryptic
  `AbilityResolver` path for `State`/`Throw`/`Abort` leaks with a located row message. Payload
  mismatches stay in payload vocabulary naturally, since user slots never hold carriers.
- **LSP.** Hover reads (payload type, declared/derived row) from the row facts — earlier than mono,
  no Id-normalization needed; the `TypeHintIndex` normalization requirement disappears.
- **Known v2 limitations carried or improved:** the `val`-bound-binder discharge limitation keeps its
  rule (capture needs the expression at the pinned slot, or a pinned-annotated binder) but gains an
  explainable diagnostic ("effects already ran at the `val`; pin the binder's type to capture"); open
  rows on by-value parameters flip from *rejected* to *meaningful* (suspension); Suspend-riding
  effects still cannot be pinned (the `Suspended` extension remains future work).

## 6. Stdlib and semantics migration

Signature changes (small, enumerable):

- `fold[A](c: Bool, whenTrue: {G} A, whenFalse: {G} A): {G} A` — declared-suspended arms.
- `if[T]` — **textually unchanged** (`value: {Abort} T` now *means* suspended).
- Lazy combinators (`orElse` fallbacks, any future `&&`/`||`): declare suspension; dischargers,
  `Effect`/`Suspend`, `printLine`/`readLine`, `Inf.forever`: unchanged.

**Semantic break (the one real cost):** an effectful argument at a plain generic slot now runs at the
call site — code relying on implicit suspension-via-genericity changes behaviour. Audit expectation:
almost all real suspension flows through `fold`/`if` (declared) and dischargers (pinned); direct
reliance is rare. The migration must grep the corpus (stdlib, examples, eliot-test) for effectful
arguments meeting bare generic slots and classify each.

## 7. What this preserves of the cornerstones

Types-are-values: intact — carriers remain ordinary type constructors in elaborated code and pinned
types; the one evaluator and definitional equality untouched; no kind/sort is added to the *type
language* (the value/computation separation lives in the judgment's second channel, like refinement
ranges). Use-site verification: intact (accounting at ground keys). Total-by-default: intact (`Inf`
is a row entry). Platform layers: intact (elaboration is layer-agnostic; `IO` stays jvm-owned).
"Effects are a channel" is *restored to full strength*: v2 conceded the carrier as the checker's
internal representation; v3 makes the internal representation match the slogan — rows never become
types, and the only carrier-typed code is code the desugar wrote or the user pinned.

## 8. Migration plan (each step lands green on its own; v2 stays live until R5)

- **R1 — spike (no wiring):** a standalone row checker over one resolved file + the `Id`-residue and
  compile-track questions of §3/§5 answered on paper against the spike. Exit: the row rules written as
  inference rules; `sign`/`catch`/`foldLeft`-chain/`choose`/`pick` worked examples.
- **R2 — surface:** arrow row-defaulting; open rows on by-value parameters accepted with the
  suspension meaning (parser/core only — no behaviour change while v2's desugar still runs).
- **R3 — row check, shadow:** the per-definition row checker runs report-only beside v2 on the whole
  corpus; disagreements triaged (each is either a v2 bug or a v3 rule gap).
- **R4 — elaboration desugar, shadow:** desugar output compiled on a second track; byte-identity as a
  safety oracle where the output should match v2's elaboration (it is an oracle, not a hard gate).
- **R5 — flip:** mono consumes elaborated facts; the checker's effect machinery goes cold; delete in
  slices (per-slice gate: lang + jvm tests, HelloWorld, eliot-test).
- **R6 — closeout:** stdlib signature updates (§6), CLAUDE.md cornerstone rewrite, skills/memory
  sweep, v2 doc marked superseded.

## 9. Open questions

1. §3's `Id`-at-discharge recommendation — confirm in R1 that boundary-`runId` insertion is fully
   syntax-directed for nested/multi-effect discharge stacks.
2. Row-variable scoping for local lambdas and `let`-bound functions (fresh per arrow vs shared with
   the ambient) — R1.
3. Whether accounting can eventually retire (post-R5 experience) — not before.
4. Multi-instance rows (`{Throw[A], Throw[B]}`) — v2's rule (no inferred discharge order; pin to
   choose) carries over; restate in row terms in R1.
5. The exact fate of `Checker.scala`'s non-effect Phase-A/B remnants — whether spine inference
   simplifies further once slots are effect-free.
