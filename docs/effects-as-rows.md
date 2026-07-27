# Effects as Rows, v3: Declared Suspension + a Desugared Elaboration

Status: **R1–R4 COMPLETE; R5 FLIP LANDED under the A.8.6 resolution, row verification wired, and the
A.8.7 RESOLVER IMPLEMENTED AND LIVE (post-drain resolution at quiescence; runtime generic-slot modes are
now suspended obligations, classified at quiescence, with splice-and-restart) — 2026-07-26, full gate
green. Deletion slices 1–3 landed (A.8.8/A.8.9/A.8.10, 2026-07-27): the checker inserts zero binds and
no longer manufactures `Id`. Next: slice 4, the carrier-safe *unification* routers.** Successor direction to
`docs/effects-as-channel.md` (v2, whose remaining checker machinery — the ladders, the concrete-slot
arms — stays live underneath until the deletion slices retire it against the resolver). The row
checker (`lang/.../row/RowChecker`) sweeps the real corpus with zero v2 disagreements (R3), verifies
`derived ⊆ declared` per definition in the pipeline (bounded per A.8.6), and the
elaboration desugar (`lang/.../row/RowElaborator`) is twin-verified, **shadow-compiled end to end**
(R4, `RowElaborationShadowCompileTest`), and wired as a real phase (`RowElaborationProcessor`).

**R5 wired the desugar into the pipeline as a real phase and ran it over the whole corpus** — which
exposed what twin shapes could not: for positions that flow through a callee's **generic binders**, the
fact that decides slot mode and lift placement is an *instantiation*, not a declaration (the full record
is Appendix A.8). **The resolution, decided 2026-07-26, is bounded staging (A.8.6)**: the desugar decides
every declaration-decided position and **explicitly defers** the instantiation-decided ones — it never
guesses. Deferred positions are finished by the checker: today by v2's live machinery, at end state by
the **A.8.7 resolver** — a post-drain peer that classifies each deferred obligation against the
quiescent meta store and splices the desugar's own rewrite. Progress detail per step: §8.

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
   effects at that position; they join the enclosing definition's row. Strict call-by-value wherever the
   callee's signature gives the slot a shape. *(Amended by A.8.6: at a slot typed by a **bare generic**
   the signature is silent, and the mode is the use site's instantiation — a computation flowing to a
   discharger's pinned parameter through `.` stays captured; `pick(readLine, "x")` still runs its read
   at the call site once the sibling argument fixes the instantiation. Either way the row joins the
   caller's row — mode never changes the row, only where the binds go.)*
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
instantiation-dependence, hence a desugar — **for every position whose mode the callee's own signature
spells** (amended by A.8.6; the R5 corpus run proved that is not every position). The checker then
checks the elaborated program as ordinary code: `flatMap` is an application like any other. **This is
not v1's weaver**: v1 erased and tried to *reconstruct* placement post-mono with no signal; v3 never
erases — the signal is in signatures, read before checking.

**The whitelist (the anti-accretion guardrail, binding on every future change).** The elaborator may
consult exactly these facts and nothing else: a callee's declared parameter types and return type (slot
carrier-headedness, carrier-codomain arrows, atomic-vs-applied shape), its declared row and carrier
binders (`EffectCarriers.declaredCarrierBinders`), its pinned metadata
(`EffectRow.pinnedParameterIndices` / `returnPinnedEffects`), the run-boundary registry, and one level
of type-alias expansion inside those signatures. A decision that cannot be made from the whitelist is
**deferred — the elaborator writes nothing** — never approximated by a new syntactic rule. In
particular, a rule that inspects a *sibling argument's expression shape* to decide a slot's mode is
prohibited: that is inference, and inference lives in the resolver (A.8.6), not the desugar. The
fail-safe direction is built in: a missing rewrite leaves direct-style code the checker either
elaborates (transition) or rejects loudly; a wrong rewrite silently changes when an effect runs.

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
`PostDrainQuoter.stripIdMachinery` (or trivialized per §3); `DeclaredPureChecker` (**corrected by the
A.8.6 landing: NOT subsumable pre-mono** — a no-ambient definition's carrier-ability call may be a
constructor-class use (`def f: Box[String] = wrap(s)`, `Container[Box]`), indistinguishable from a leak
without the instantiation, so the row check does not enforce there and `DeclaredPureChecker` — which is
mono-failure-triggered, hence instantiation-informed — stays until the resolver era);
`CarrierKindChecker`'s carrier-role seeding; and inside `Checker.scala` the ladders
(`resolveLadder`/`resolveFailureLadder`), Phase A/B deferral (`SlotOutcome.Deferred`,
`resolveDeferredSlot`, `sequenceBeforeUnify`, `deferredGenericDefault`), all four pinning mechanisms
(`eagerRowPinIntoDomain`, `recordRowArgumentPins`, `findCarrierLayerSlots`,
`applyPendingCarrierPins`), the slot routers (`uniformPayloadSlot`/`uniformCaptureSlot`/
`uniformCarrierSlot`/`payloadFitsDomain`), and the effect branch of `typeImmediateLambda` — the
checker ends with zero effect code, below the pre-v2 baseline.

**Slices 1 and 2 of that list have landed (A.8.8/A.8.9, 2026-07-27)**, driven by an arm-liveness
trace over the whole gate rather than by inspection. Slice 1 (unreachable arms): `tryBindLift`,
`tryIdDefault`, the `allowBindLift` flag with the ladder arms it selected,
`sequenceBeforeUnify`/`deferredGenericDefault`, two of the four pinning mechanisms
(`recordRowArgumentPins`/`applyPendingCarrierPins` — recorded 98 pins, applied 0) and the zero-caller
uniform methods. Slice 2 (a live decision moved, then deleted): the checker no longer inserts a
single bind on the runtime track — the bridge's whole `UniformSlotOutcome`/`Bound` path,
`SlotOutcome.Bound` with `suspendBoundSlot`, the spine's bind fold and `EffectLifter.wrapBinds` are
gone, the elaboration decision having moved to the desugar and the post-drain resolver. Slice 3
(A.8.10, an *encoding* removed): the checker no longer manufactures an `Id` head for pure judgments —
`intoCarrierHeaded`/`intoCarrierHeadedTerm` and the uniform-carrier judgment invariant are deleted, and
the bridge classifies a term's form instead, so ~95% of the machinery (a wrap the `Id`-normalizer
existed to erase again) is simply not written. What remains of the list is carrier-safe *unification*,
which the row channel must absorb next — see A.8.10's closing paragraph.

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

**Semantic break — narrowed to nearly nothing by A.8.6:** the original plan made an effectful argument
at a plain generic slot run at the call site unconditionally. Under bounded staging a *bare generic*
slot's mode is instead resolved from its instantiation — which is v2's behaviour — so dot-chained
discharge, `foreach`, `provide` and the `choose`/`pick` pair all keep their current semantics, and the
planned corpus audit for implicit-suspension reliance is moot. The break that remains is only at slots
with a *declared concrete* payload type: an effectful argument there always runs at the call site
(hoisted by the desugar), which is also what v2 does. Effectively no user-visible semantics change.

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

- **R1 — spike (no wiring): DONE (2026-07-25).** The standalone row checker lives in test sources
  (`lang/test/src/com/vanillasource/eliot/eliotc/row/RowCheckerSpike.scala` + `RowCheckerSpikeTest.scala`,
  12 green cases through the real pipeline to `OperatorResolvedValue`), the row rules and worked examples
  are Appendix A, and the §9 questions 1/2/4 are answered there. Headline findings: the derivation rule is
  **one line** (suspension is row-neutral — only pinned slots differ, by subtraction); the suspension
  surface **already parses** (open rows on by-value parameters populate `EffectRow.parameterEffects`
  today); and R2 must additionally record **pinned-slot entries** beside the position tag.
- **R2 — surface: DONE (2026-07-25), re-scoped by the R1 findings.** What R2 turned out to be:
  (i) **pinned-slot entries recorded in `EffectRow`** — `returnPinnedEffects` / `pinnedParameterEffects`
  (entries in declared = discharge order, not deduplicated), populated by `EffectSugarDesugarer` at the
  one point that knows; the position-only `returnPinned`/`pinnedParameterIndices` became derived views,
  so every existing consumer (`Checker.calleePinnedParams`) is untouched and all fact-chain hops ride the
  existing `map`/`traverse`. The spike's name-based `<Ability>Carrier` inversion was deleted and its
  nested-stack test passes off the recorded entries — the R2 acceptance criterion. One consequence,
  fail-safe by direction: a pinned entry's ability name now *resolves* like an open-row entry's, so
  pinning an ability that is not in scope errors loudly (in real code the carrier is colocated with its
  ability, so one resolves iff the other does). (ii) The **suspension surface needed no work**: open rows
  on by-value parameters already parse and populate `parameterEffects` (R1 finding); their v3 *meaning*
  activates with the R4 desugar. (iii) **Arrow row-defaulting moved to R3**: it is a *reading* rule of
  the row checker (an unannotated signature arrow reads as a fresh row variable), not a stored fact —
  nothing to land ahead of its consumer. Gate: lang 1042, jvm 293, HelloWorld, eliot-test 11/11.
- **R3 — row check, shadow: DONE (2026-07-25).** The spike graduated to production code —
  `lang/src/com/vanillasource/eliot/eliotc/row/RowChecker.scala`, still **unwired** (consumed only by its
  unit suite `RowCheckerTest`, 16 cases, and the shadow sweep) — with FQN-based rows, first-order-ability
  detection, suspended-parameter environments, run-boundary handling, and per-result coverage reporting
  (`unknownCallees`). The shadow sweep (`jvm/test/.../RowShadowSweepTest`) compiles a combined real
  program (Console blocks, `catch` pure+effectful, State at IO, Abort under a local Id carrier,
  discharge-to-pure) plus an `Inf` super-loop through the live v2 pipeline over the full
  `lang`/`stdlib`/`jvm` layers in a cold session, then row-checks **every** body-carrying
  `OperatorResolvedValue` the compile demanded — stdlib dischargers, jvm ability implementations and the
  synthetic entry included. **Result: zero disagreements with v2**, after triaging the two the sweep
  caught (Appendix A.7): the nominal-run return form (`def main: IO[Unit]`) and the accessor-merge row
  metadata loss — the latter a genuine latent defect fixed in the layer merge. Arrow row-defaulting landed
  as the checker's conservative-latent reading rule (A.5). Gate: lang + jvm (295) green, HelloWorld,
  eliot-test 11/11.
- **R4 — elaboration desugar, shadow: DISCHARGE SLICE DONE (2026-07-26).** The elaborator core exists —
  `lang/src/com/vanillasource/eliot/eliotc/row/RowElaborator.scala`, unwired, decision-free (every choice
  reads declared rows/slot modes via `RowChecker`, never a type) — covering strict-slot bind hoisting
  (leftmost-outermost), block/`val` sequencing over the applied-lambda desugar, the uniform pure-wrap rule
  (a pure node in a carrier position — innermost continuations and declared-row boundaries alike), mixed
  and multi-argument calls, suspended/pinned/run-boundary slot pass-through, and the pure-code identity
  (empty row ⇒ byte-identical output, no `Id`). Acceptance: `RowElaboratorTest` compiles each
  direct-style program *and its hand-written explicit-monadic twin* through the real pipeline and asserts
  the elaborated body is **structurally identical** (α-renamed binders) to the twin — 17 shapes green,
  including the suspended-slot rule (a pure argument at a declared-suspended slot lifts via `pure`, an
  effectful one passes unrun — v2's `tryPureWrap` arm as a declared-slot-mode read).
  The machinery nodes are spelled by the same FQNs the v2 checker splices (`WellKnownTypes.effect*FQN`),
  so the output is today's explicit monadic core by construction.
  The **discharge slice (A.4) landed 2026-07-26**: the elaborator's effectfulness notion became
  **carrier-valued-ness** — a call is a carrier computation exactly when its callee's declared return is
  headed by one of the callee's own carrier binders (`readLine : F[Str]`, `catchX : G[A]`; declared
  shape, no tag) — and a carrier-valued node meeting a region with *no* ambient carrier (a pure
  definition's boundary, `val` binding, or strict argument slot) is unwrapped with `runId` at that same
  boundary, the region's base carrier being `Id` by declaration; under an ambient carrier it binds like
  any effectful call. Handler lambdas at declared carrier-codomain slots (`onError: E => G[A]`) elaborate
  their bodies as carrier regions (effectful body = already a computation, pure body = `pure`-wrapped),
  and a suspended-parameter reference is itself carrier-valued (never re-wrapped when forwarded). One
  mechanical find: the surface `=>` reaches the operator phase as the **unexpanded operator-named alias**
  (Default namespace), so the elaborator sees through one alias level by expanding the alias's own
  declared body over its binders (`asArrowLike` — universe lookup + substitution, no evaluation).
  The **callback/latent-arrow slice landed 2026-07-26** (24 twin shapes total): an `{Effect}`-marker
  callback slot (`action: A => {Effect} Unit` — the codomain desugars to the callee's minted carrier) is
  a **carrier-codomain slot, not a suspended slot** — the slot classifications became purely
  shape-declared (suspended = the declared parameter type itself carrier-headed; carrier-codomain = a
  declared arrow ending in the callee's carrier binder), which fixed the `parameterEffects`-index
  conflation of the two. A lambda at a **plain arrow slot elaborates naturally** (effectful body → bind
  chain, pure body untouched), and a carrier-valued body **instantiates a bare-generic codomain at a
  carrier**, making a generic-eliminator call (`weird[A, B](f: A => B, a: A): B`) carrier-valued by
  declared binder plumbing plus the elaborated argument's shape — still no types. Calling a
  **function-typed parameter** (`action(s)` on the callee side) is carrier-valued when its declared
  arrow's final codomain is carrier-headed and the call saturates it, with strict hoisting of its
  arguments. Consequence: the region's *ambient* flag is now "declared return carrier-headed", not
  "declared user row non-empty" — a machinery-marker signature (`foreach`) has an empty user row but a
  real ambient carrier.
  The **pinned-region slice landed 2026-07-26** (30 twin shapes total): region carrier-ness became a
  *positional* flag threaded through the elaboration (like `needCarrier`), starting at the definition —
  true when the declared return is carrier-headed, **pinned** (`{X | G} A`), or headed by a **platform
  run carrier** (the nominal-run spelling, read off the run-boundary registry) — and flipping to true
  inside every **pinned or run-boundary argument**: a captured compound computation binds on the
  pinned/run stack even under a pure definition (`catchX(use(boom), h)` hoists `boom` *inside* the
  capture), a pure captured argument lifts via `pure`, and a pinned-return body is itself the captured
  computation (pure body ⇒ `pure`-wrapped, effectful body untouched). The nominal-run rule also fixed a
  latent wrong-`runId` on `def main: IO[Unit]`-shaped bodies.
  The **end-to-end shadow compile landed 2026-07-26 — R4 is COMPLETE.** The seam is fact injection:
  `CompilationSession.compileOnce(seedFacts = …)` registers facts before the run, and a registered fact
  preempts its processor, so everything downstream regenerates from it — no production pipeline change.
  The experiment (`jvm/test/.../RowElaborationShadowCompileTest`) compiles the R3 sweep's combined corpus
  normally, elaborates **every** body-carrying runtime `OperatorResolvedValue` the compile demanded
  (stdlib dischargers, jvm ability implementations — `Effect[StateCarrier]` et al. — and the synthetic
  entry included), then recompiles the same sources with the elaborated values seeded. Oracle:
  **behavioral identity** — run B compiles clean and its executable prints exactly run A's output
  (byte-identity of jars is deliberately not the gate; `$row$N` binders legitimately rename lambda
  classes). Two rules were corrected by the experiment, both declared-shape reads: (i) a **nominal-run
  typed callee** (`prog : IO[Pair[..]]`) is carrier-valued (the run-carrier head from the boundary
  registry, A.7), and applied-result shapes see through **over-application** (an accessor returning an
  arrow with carrier codomain: `runStateCarrier(fa)(s)`); (ii) `runId` is inserted **only at the two
  boundaries v2 Id-defaults at** — a definition's pure return and a `val` binding — never at argument
  slots, where the still-flex base must instead flow to the slot's expected type (the pervasive
  hand-monadic `runId(runAbort(x))` / `.runThrow.runId` shape would otherwise double-unwrap).
- **R5 — flip: IN PROGRESS under the A.8.6 resolution (2026-07-26).** The seam landed as designed —
  elaboration is an ordinary phase (`RowElaboratedValue`, produced by `RowElaborationProcessor` between
  the recursion gate and saturation, with a demand-driven universe that fetches exactly what elaboration
  consults rather than guessing it). Running the elaborator over the *whole* corpus rather than over twin
  shapes then forced five rules R4 did not have. Three are genuine information gaps (a bare `[F[_]]` is
  not a carrier; `runId` only at a declared discharge; a concrete carrier-typed parameter is data) and
  stand on their own. The other two — hoist-iff-the-row-is-non-empty, and *relaying* a slot mode through
  a callee's generic binder — are where the deciding fact is an **instantiation**, not a declaration, and
  the second was reverse-engineered from the dot-chained discharger. **The full record is Appendix A.8;
  the decision — bounded staging: the desugar defers instantiation-decided positions instead of
  guessing them — is A.8.6, which also records the landed state: the deferral-based flip is live and
  green, and the per-definition row verification is wired (bounded to ambient-declaring definitions,
  full coverage, no bare-generic-slot uncertainty). The resolver that replaces v2's machinery for the
  deferred residue is designed in **A.8.7** (post-drain resolution at quiescence: suspended
  obligations, classification against the solved store, splice via the desugar's own rules, loop
  restart; mid-spine resolution rejected).** Deletion slices proceed only against the implemented
  resolver, Phase A/B last.
- **R6 — closeout:** stdlib signature updates (§6), CLAUDE.md cornerstone rewrite, skills/memory
  sweep, v2 doc marked superseded.

## 9. Open questions

1. **ANSWERED (R1, Appendix A.4):** `Id`-at-discharge is fully syntax-directed, including nested
   stacks — rows never involve `Id` at all; the capture boundary is the pinned argument position, the
   elaborated stack's shape and order come verbatim from the declared pinned spelling, and the base
   carrier is the enclosing region's carrier, `Id` exactly when the enclosing residual row is empty.
2. **ANSWERED (R1, Appendix A.5):** one latent row per arrow. The spike joins a function-valued
   argument's latent row conservatively at the receiving call; production mints a fresh row variable
   per unannotated arrow with `latent ⊆ var`, and calling a function-typed parameter contributes its
   declared arrow row (the `{Effect}` machinery marker denoting the ambient variable).
3. Whether accounting can eventually retire (post-R5 experience) — not before.
4. **ANSWERED (R1, Appendix A.6):** production rows are multisets of (ability, type-args) — the
   spike's name-set `Row` collapses `{Throw[A], Throw[B]}`. Discharge at a pinned slot consumes by
   type-arg match; two same-ability entries with a non-pinning handler have no canonical order and
   stay a diagnostic asking for a pin (v2 §4's rule, carried over verbatim).
5. The exact fate of `Checker.scala`'s non-effect Phase-A/B remnants — whether spine inference
   simplifies further once slots are effect-free.

## Appendix A. The R1 spike: row rules, worked examples, findings (2026-07-25)

Spike code: promoted at R3 to `lang/src/com/vanillasource/eliot/eliotc/row/RowChecker.scala` (the
production checker, still unwired) with `lang/test/.../row/RowCheckerTest.scala` (16 cases, each
compiled through the real pipeline to `OperatorResolvedValue` and row-checked with **no types, no
carriers, no metavariables**). All green inside the ordinary `./mill lang.test` gate.

### A.1 The rules

A `Row` is a set of effect-ability entries (production: multiset of (ability, type-args) — A.6).
Judgments are per definition, over the operator-resolved body, reading only *declared* information:

- **value-of**: `row(literal) = row(λ) = row(under-applied ref) = ∅`; `row(saturated call f(a₁…aₙ))
  = declared(f) ∪ ⋃ᵢ contrib(aᵢ)`; `row(applied λ)` (the block/`val` desugar) `= row(bound arg) ∪
  row(body)`.
- **contrib** at slot *i*: `contrib(aᵢ) = (row(aᵢ) ∪ latent(aᵢ)) ∖ pinnedEntries(f, i)` — the
  subtraction applies only when slot *i* is pinned (`EffectRow.pinnedParameterIndices`); every
  non-pinned slot — strict *or suspended* — contributes identically.
- **latent**: `latent(λx.e) = row(e)`; `latent(under-applied ref f) = declared(f)`; else `∅`.
- **declared**: the open-row return entries ∪ the effects constrained on the signature's carrier
  binders (machinery excluded) — an effect-ability method's contribution is its own ability.
- **check**: `row(peeled body) ⊆ declared`, reported per definition at the definition ("performs the
  effect 'X' but does not declare it").

### A.2 The central finding: suspension is row-neutral

The doc's §2 sketched suspended slots as needing their own derivation treatment. The spike shows they
need none: whether a slot is strict (bind now) or declared-suspended (pass the computation) changes
only *when* the effect runs — elaboration's business — never whether the caller must declare it
(declaration-level conservatism, exactly v2 accounting's stance on a generic consumer running an arm
twice). The *only* slot mode that touches derivation is pinned capture, as subtraction. Verified by
the strict-vs-suspended twin test deriving identical results. Consequence: the row checker's
derivation is smaller than estimated, and slot modes matter only to the elaboration desugar.

### A.3 Worked examples (each is a green test)

| example | result |
|---|---|
| `use(items)` with `items: {Con} Str`, `use` effect-oblivious (the `foldLeft`-chain shape; dot chains hit the same spine) | derived `{Con}` = declared; `use` needs no declaration |
| `choose(readLine, readLine)` and `pick(readLine, pure)` — v2's one irreducibly instantiation-dependent pair | identical derivations by the one strict rule |
| `def leaky: Str = readLine` | leak `{Con}` located at `leaky` (subsumes `DeclaredPureChecker`, with a good location) |
| `catchX(failing, h)` under a **pure** return (the `sign` shape) | derived `∅` — discharge-to-pure with **zero `Id` anywhere** |
| `catchX(failing, hEff)` with an **effectful handler** | handler's latent `{Con}` joins; derived `{Con}` |
| `catchX(failLog, h)` with `failLog: {X, Con}` (partial discharge) | `{X}` subtracted, residual `{Con}` rides |
| `catchBoth(failTwo, h)` over `{X, Y \| G}` (nested two-effect stack) | both entries subtracted; nesting fully syntax-directed |
| `forever(printLine(…))` with/without `Nf` declared (the `Inf` shape) | ordinary entry: rides the union, leaks when omitted |
| `{ val x = readLine … }` | the applied-lambda desugar sequences the binder's row |

### A.4 `Id` at discharge (answers §9 Q1)

Rows never mention `Id` — the pure-boundary examples derive `∅` with no identity carrier anywhere in
the row story. `Id` appears only in the *elaboration* of a discharge region under a pure residual:
the capture boundary is the pinned argument position (syntactically fixed), the stack's layers and
their order come verbatim from the declared pinned spelling (the nested example's entries are read
off the declared stack, outer→inner), and the base carrier is the enclosing region's carrier — `Id`
exactly when the enclosing residual row is empty, with `runId` at that same boundary. Every input to
that decision is declared; nothing depends on solver state.

### A.5 Latent rows and lambda scoping (answers §9 Q2)

One latent row per arrow. The spike implements the conservative form — a function-valued argument's
latent row joins the receiving call ("the callee may run it") — which is sound and declaration-level.
Production replaces the conservative join with a fresh row variable per unannotated signature arrow
(`latent ⊆ var`, solved by union), and calling a function-typed *parameter* contributes that
parameter's declared arrow row, with the `{Effect}` machinery marker denoting the enclosing
signature's ambient row variable.

### A.6 Production deltas (what the spike deliberately simplified)

1. **Rows as multisets of (ability, type-args)** — the name-set `Row` collapses `{Throw[A],
   Throw[B]}`; discharge must consume by type-arg match, and the v2 §4 multiplicity rule (no inferred
   order; pin to choose) carries over (answers §9 Q4).
2. **Record pinned-slot entries in `EffectRow`** beside the position tag — **DONE at R2 (2026-07-25)**:
   `returnPinnedEffects`/`pinnedParameterEffects` carry the entries in declared order, and the spike now
   reads them (its name-based stack inversion is deleted). The nuance that motivated this: only the
   *outer* stack layer is payload-applied (`ThrowCarrier[E, StateCarrier[S, Id], A]` — the inner layer's
   base is its last argument) — shape knowledge that belongs at the desugar, not at a consumer.
3. **First-order abilities** (`Show`) are distinguished from effect abilities by their missing HKT
   binder when the method's signature is at hand; the spike assumes effectful when it is not.
4. **The suspension surface already exists**: open rows on by-value parameters parse today and
   populate `EffectRow.parameterEffects` (they are *rejected* later, at checking) — so R2's surface
   work is reinterpretation plus arrow row-defaulting, not new grammar.

### A.7 The R3 shadow-sweep triage (both findings fixed, sweep now clean)

The first cold sweep over the real corpus found exactly two disagreement classes; both were
information gaps, not rule gaps, and the derivation rule survived unchanged:

1. **The nominal-run return** (`def main: IO[Unit]`): a definition returning the platform's concrete
   run carrier is v2-exempt (the subset check fires only for open-row values) — it is the *nominal
   run* spelling of a boundary, where the concrete carrier captures the whole row. The row checker
   mirrors it by declared information only: the run-carrier head is read off the **registered run
   boundary's own first parameter** (`runMain(io: IO[A])` ⇒ `IO`, the `RunBoundaryFunction` registry —
   tag source (ii)), never guessed from a name; a return headed by it sets `RowResult.runCaptured`.
2. **The accessor merge dropped row metadata — a genuine latent defect, fixed in the layer merge.**
   The stdlib's abstract discharger signature (`def runAbort[G[_], A](obj: {Abort | G} A): G[Option[A]]`)
   carries the R2 pinned entries; its concrete twin is the jvm `data AbortCarrier(runAbort: …)`
   *accessor*, whose synthesized definition carries an empty `EffectRow` — and the merge's
   body-preference silently discarded the abstract twin's row. `UnifiedModuleValueProcessor` now merges
   the row metadata fieldwise (signature equality is already verified, so every layer's row describes
   the same signature; the layer that spells a position in effect vocabulary supplies the entries).
   Live v2 consequence, deliberate and gate-verified: `Checker.calleePinnedParams` now sees the pinned
   tag for accessor-merged dischargers (`runAbort`/`runThrow`/`runStateTo…`), extending the join-solver
   routing to those capture shapes — the residual whole-unify traffic v2's §10 item 13 left unrouted
   for want of a benefit; the row metadata is that benefit, and the full behavioral gate (295 jvm
   output-asserting tests, catch shape matrix included, HelloWorld, eliot-test 11/11) is green under it.

### A.8 The R5 flip: what wiring the elaborator into the pipeline revealed (2026-07-26)

R5's first slice — making the pipeline *consume* elaborated bodies — landed as designed. Running the
elaborator over the whole corpus rather than over twin shapes then exposed a class of gaps R4's
acceptance could not: **the desugar must decide slot modes and lift placement that flow through a
callee's generic binders, and those are not always readable from declarations.** This section records
the seam, every rule the corpus forced, the one that is a red flag, and the design question they add
up to. The work is **paused here pending that decision** — nothing below is committed.

#### A.8.1 The seam (landed, uncontroversial)

Elaboration is an ordinary phase in the value chain, one fact wide:

`OperatorResolvedValue` → `NamedValuesRewrittenValue` → `RecursionCheckedValue` → **`RowElaboratedValue`**
→ `SaturatedValue` → …

- `lang/src/.../row/fact/RowElaboratedValue.scala` carries the `OperatorResolvedValue` with its body
  rewritten and **every other field untouched** — which is why the *sideways* reads later phases perform
  for a callee's signature, fixity or effect row keep reading `OperatorResolvedValue` directly. Only
  `SaturatedValueProcessor` (the sole body consumer downstream) was repointed.
- `lang/src/.../row/processor/RowElaborationProcessor.scala` runs it, placed **after** the recursion
  gate (which walks the *user's* reference graph, before any machinery call is spliced in).
- **The universe is built by demand, not guessed.** Elaboration consults the declared signature, row and
  slot modes of every callee it meets, plus one alias level inside those signatures and the registered
  run boundaries — and *which* names those are depends on classifications made along the way, so the set
  cannot be read off the body. `RowChecker.Universe` therefore gained an `onMiss` callback: the
  processor elaborates against what it holds, fetches exactly what was missed, and repeats until a round
  misses nothing new. Guessing the set instead would silently fall back to the unknown-callee
  approximations, and a wrong slot mode changes *when* an effect runs — which no later phase catches.
- Position fidelity: `assemble` now returns the **original** nodes when nothing changed. Rebuilding an
  equal spine re-attributes it to per-argument positions, which silently moves every diagnostic anchored
  at a call (caught by an existing "too many type arguments" location assertion).

#### A.8.2 The rules the corpus forced

Each entry: the shape that broke, the rule, and whether it is stated in the design's own vocabulary or
reverse-engineered from an idiom.

1. **A bare `[F[_]]` is not a carrier** — *principled*. `def id[F[_]](x: F[A]): F[A]` and a
   constructor-class `ability Container[F[_]] { def wrap(s: String): F[String] }` were classified as
   carrier-returning, so `def f: Box[String] = wrap(s)` got a spurious `runId` and `id(someBox)` a
   spurious `pure`. `EffectCarriers.declaredCarrierBinders` now asks which binders a signature *declares*
   as carriers: ability-constrained (`[G[_] ~ Effect]`, every `{E}`-minted binder), or the base of a
   declared **pinned** row (`runAbort[G[_], A](obj: {Abort | G} A)` — deliberately unconstrained, so
   nothing else marks it), or — for an ability method — its ability's own binder. `Console` and
   `Container` are the same shape and stay the same shape; what separates them is the *use site*.
2. **`runId` only at a declared discharge** — *principled, and stronger than R4's rule*. A carrier-valued
   node at a pure boundary was unwrapped unconditionally, which is wrong for anything whose carrier the
   context supplies. `runId` now requires the node to be a call that **captures a row in a pinned
   parameter** (or a run boundary): a discharger has *consumed* the row, so what remains rides a carrier
   nothing constrains — `Id` by declaration (A.4). A merely carrier-*returning* call has discharged
   nothing: the desugar writes nothing and unification decides (`def f: Box[String] = wrap(s)` takes
   `Box`; an undeclared effect under a pure return is a leak the row check reports). This also restored
   the friendly declared-pure diagnostic, which the unconditional rule had replaced with a stray
   "Name not defined.".
3. **A concrete carrier-typed parameter is data** — *principled*. `implement Effect[IO]`'s own
   `fa: IO[A]` was read as a suspended computation and hoisted, rewriting the very machinery elaboration
   emits (`IO(IO(IO(Type)))`). A parameter holds a computation only when its declared type is headed by
   one of the definition's **own carrier binders**, or is a **pinned stack** (`computation: {Dep[X] | G} A`).
   Symmetrically, a `pure`-lift now requires the node to be **definitely pure**: not-carrier-valued also
   covers everything the desugar cannot classify (a lambda binder whose type only inference knows), and
   only an *atomic* declared type says "payload" — an applied one may be a carrier stack the desugar
   cannot name.
4. **Hoist iff the argument's row is non-empty** — *principled in vocabulary, but already incomplete*.
   `foldLeft(pure(unit), …)` and `foldOption(fallback, …)` fold **over** computations: the accumulator is
   data, and binding it strips the carrier the slot exists to receive. Carrier-valued-ness alone cannot
   tell those from `readLine`; the *row* can — `pure(x)` and a `fallback: G[A]` parameter have the empty
   row, `readLine` has `{Console}`. This is the design's own vocabulary, and it keeps the §6 semantic
   break exactly where it belongs. **But it is not sufficient as stated**: a *discharging* call also has
   an empty row and still must be sequenced, which is the one remaining lang failure
   (`printLine(catchX(failing, h))` under an ambient carrier no longer hoists). The fix is another clause
   — and "another clause" is the pattern this section is really about.
5. **Carrier instantiation through generics, and relayed slot modes** — *the red flag*. Two shapes:
   `list.foreach(action)` and `foreach`'s own `foldLeft` body need to know that a callee's **bare generic
   return** was instantiated at a carrier by one of its arguments; and `firstDep.provide(Database(…))`
   needs to know that a computation reaching a bare generic slot is **captured**, not run. The rules
   added were: propagate "this argument instantiates this generic at a carrier" from arguments to the
   return, and *relay* a slot mode — if a slot is typed by a bare generic that another slot's declared
   arrow takes as its **domain**, and that arrow's argument is an **under-applied call whose next
   parameter is pinned**, the slot inherits pinned.

#### A.8.3 Why rule 5 is a red flag

The relay rule names nothing: no FQN check, no `.`-specific branch, and it fires for any combinator of
that shape. But it exists because `a.f(b)` is `.(a, f(b))`, and it only handles depth 1 — an
under-applied call, not a lambda, not a longer chain. **The dot operator is an ordinary user-land
function and must stay one**; a rule that has to be invented so that one idiom elaborates is a rule
shaped by the idiom, whatever its stated generality. It is also plainly inference: it propagates a mode
across a generic binder through a higher-order argument, which is what the *checker* does with types.

Read together with rule 4's missing clause, the five rules are one signal: a declared-only desugar keeps
being asked to re-derive, one shape at a time, what v2's type-directed checker knew for free. Rules 1–3
are genuine information gaps (R4 read the wrong declared fact) and stand on their own. Rules 4 and 5 are
where the information is not in the declarations at all.

#### A.8.4 The design question this poses

**Can the elaboration desugar decide slot mode and lift placement for positions that flow through a
callee's generic binders, from declarations alone?** The corpus says: not for the two shapes above,
because the deciding fact is an *instantiation* (`B := F[Unit]`, `A := {Dep[X] | G} String`), not a
declaration. §3's premise — elaboration is a desugar, decision-free, before checking — holds for every
shape where the mode is spelled on the callee's own parameter; it does not obviously hold where the mode
arrives through a generic.

Three directions, none of them taken:

- **(a) Declare it.** Make the surface carry what the relay rule guesses: a combinator that forwards a
  slot to a pinned/suspended parameter says so. Costs surface; keeps the desugar decision-free; needs an
  answer for `.` itself, which is where it hurts (`.`'s `A` is *any* slot of *any* callee).
- **(b) Stage it.** Let elaboration run with some type information — after (or interleaved with) enough
  checking to know the instantiation. Gives up "elaboration is a desugar" as an absolute, and moves the
  boundary v3 drew with v2 rather than erasing it.
- **(c) Bound it.** Keep the desugar declaration-only and accept that these shapes do not elaborate —
  then R5 cannot delete v2's checker machinery for them, and the effect surface loses dot-chained
  discharge (a shipped, documented idiom).

#### A.8.5 State of the tree at the pause

Uncommitted, and deliberately so: rule 5 is in the working tree and neither of us believes in it.

- Wiring (A.8.1) and rules 1–4: implemented.
- Gate: **lang 1076 passed / 1 failed** (the rule-4 discharge clause, above); **jvm 275 passed / 21
  failed** (State/Dep/file-io/Inf integration programs — all downstream of rules 4 and 5).
- Test updates already made and independently sound: the `MonomorphicTypeCheckTest` lift group was
  rewritten to v3's shapes (strict-by-default; one bind combinator, `pure` under `flatMap` instead of
  selecting `map`) and renamed to "the effect elaboration"; `liftedBody` now **fails** when a value
  produces no monomorphic body instead of returning an empty name list — which surfaced one assertion in
  that group that had been passing vacuously since it was written (`foldOr(none, …)`: `none`'s `A` was
  determined by nothing, so nothing ever compiled).
- The stub `IO` in `ProcessorTest` gained the run boundary `runMain`, and `MonomorphicTypeCheckTest`
  registers it — the harness now declares `IO` a carrier head exactly as a real build does, instead of
  the elaborator having to infer it.

#### A.8.6 Resolution: bounded staging (decided 2026-07-26)

**The decision is (b), tightly bounded.** The A.8.4 question — can the desugar decide slot mode and
lift placement for positions flowing through a callee's generic binders from declarations alone — is
answered *no*, and the design stops pretending otherwise. §0's audit already identified the
generic-slot mode as the one irreducibly instantiation-dependent decision of v2's eight elaboration
sites; R5's corpus run is the proof that declaring suspension removes it for direct slots but not for
slots reached *through* a generic. The correction is not more rules — it is making the one
type-informed decision **explicit, singular, and owned**:

1. **The desugar decides every declaration-decided position and explicitly defers the rest.** Where
   the deciding fact is an instantiation — a computation meeting a bare-generic slot, a call whose
   declared return is a bare generic — the elaborator **writes nothing**: no hoist, no `pure`, no
   `runId`. Deferral is the only sanctioned reaction to missing declared information (the §3
   whitelist). Rule 5 (both halves: carrier-instantiation propagation and relayed slot modes) is
   deleted, never committed. Rule 4 keeps its row test and gains its one missing — declared — clause:
   a *discharging* argument (its callee captures a pinned slot or is a run boundary) sequences even
   though its row is empty. `definitelyPure` tightens symmetrically: only a declared-atomic/concrete
   result is definitely pure; a bare-generic result is unclassifiable, hence deferred.
2. **Deferred positions are finished by the checker.** During the transition that is v2's live
   machinery, unchanged — a deferred node is direct-style code, which is exactly what the v2 checker
   elaborates today, and elaborated fragments are indistinguishable from hand-written monadic code it
   already accepts. So the flip can go green *before* any deletion starts, with the desugar owning
   the declaration-decided majority and v2 owning the deferred residue.
3. **The end state replaces v2's Phase A/B with one small resolver** (est. 150–300 lines, beside the
   row machinery, not in the checker's unification path). It reads the *solved* instantiation of a
   deferred slot — post-drain or at mono, where instantiations are ground by construction — classifies
   it three ways (payload → the desugar's strict-hoist rule; carrier-headed → suspended pass-through;
   pinned stack → capture), and applies the desugar's own placement rules. The binding constraint:
   **mode resolution must never intercept unification mid-flight** — if a mode were needed before
   checking can proceed, the v2 ordering machinery (ladders, Phase A/B sequencing) would creep back.
   The `choose`/`pick` pair is the acceptance test for exactly that risk. Deletion of v2's machinery
   proceeds in slices only after the deferral-based flip is green; Phase A/B goes last, replaced by
   the resolver.

**What this concedes and what it keeps.** "Elaboration is a desugar" weakens to "a desugar plus
exactly one type-informed decision, made where types are ground." In exchange: the §4 deletion list
survives intact (the theft class, the ladders, the Id apparatus, `EffectLifter`, `UniformCarrierChecker`
all still die — Phase A/B is *replaced*, by something smaller in kind); the §6 semantic break narrows
to nothing user-visible (bare-generic slots keep v2's instantiation-determined behaviour, so
dot-chained discharge, `foreach`, `provide` and `choose`/`pick` are unchanged); and the dot operator
stays an ordinary function with no rule shaped after it. The rejected alternatives: (a) declaring
mode-forwarding in the surface fails precisely at `.` (its `A` can forward to any slot of any callee)
and still needs annotations for lambdas and deep chains; (c) bounding the desugar loses dot-chained
discharge, a shipped idiom. Note (a)'s special case "make `.` compiler magic" would not even close the
class — `foreach`'s own `foldLeft` body needs the same instantiation fact with no dot in sight.

**Landed (2026-07-26, same day): the deferral-based flip is wired and the full gate is green** — every
module suite (lang, jvm, eliotc, LSP), HelloWorld, the discharge examples, and the shadow compile.
Rule 5 was deleted, rules 1–3 kept, rule 4 completed with the declared discharge clause. Landing the
deferral over the whole corpus forced four corollaries, all of them *consequences* of the discipline
rather than new judgment calls:

1. **Payload-by-construction binders.** A block binder bound by an elaborator-*inserted* `flatMap`
   holds the computation's payload by construction, so a reference to it is definitely pure and may be
   `pure`-lifted (`swap`'s `old`). This is elaborator-owned information, not inference — without it the
   checker meets the un-wrapped tail against the machinery's carrier slot and first-contact-unifies the
   payload meta with the carrier (the State-under-Id miscompile).
2. **An inserted rewrite must be fully discharged or not made.** A bind chain whose eventual tail is
   deferred is rolled back whole (the binding stays direct-style for the checker), and hoisting requires
   a classifiable core — carrier-valued or definitely pure. Otherwise the machinery the elaborator
   writes puts a deferred node (`resultValue(r)`'s bare `A`, `andThen($row$1, abort)`) into a
   carrier-typed position the checker then commits wrongly: the readFile `ClassCastException` and the
   Abort-stack double-wrap were both this shape.
3. **Pinned captures never boundary-wrap.** Pinned means captured: a pure actual does not lift into a
   pinned slot — v2 parity, and what preserves the curated val-bound-discharge diagnostic
   (`Expected: {Abort | IO} String`) instead of a downstream ability demand.
4. **Untouched code keeps its original nodes, transitively.** A lambda whose body elaborates to itself
   must not be rebuilt: an equal-but-new node reads as a changed argument upstream and re-attributes the
   whole application spine, which surfaced as duplicate co-located LSP hover hints.

The shadow-compile changed-count tripwire was relaxed to `> 0` (deferral legitimately rewrites fewer
definitions; the tripwire only guards against identity degradation). Next: the R5 deletion slices —
the checker's effect machinery goes cold piece by piece against this same gate, Phase A/B last,
replaced by the ground-instantiation resolver.

**Landed, second slice (2026-07-26): the per-definition row verification is wired** —
`RowElaborationProcessor.verifyRow` enforces `derived ⊆ declared` before elaboration, located at the
definition, in accounting's own wording; a leak aborts the value, so the user gets one friendly error
instead of the downstream symptoms (this replaces the cryptic `AbilityResolver` demand for a
`State`/`Throw`/`Abort` leak from an ambient-declaring definition, and turns the val-bound-discharge
limitation into a located "performs the effect 'Abort' but does not declare it"). Wiring it over the
corpus forced the same honesty the elaborator needed, and drew the enforcement boundary in three
declared conditions:

- **only for a definition that declares an ambient** (non-empty declared row / pinned return). The
  constructor-class idiom (`def f: Box[String] = wrap(s)` with `Container[Box]`, a row-spelled
  combinator instantiated at a concrete carrier) proves a callee-row contribution's *ridership* is an
  instantiation fact — a no-ambient definition cannot be enforced from declarations. Consequence,
  corrected in §4: `DeclaredPureChecker` is **not** subsumed pre-mono; being mono-failure-triggered it
  is instantiation-informed, and it stays until the resolver era (as does the purpose-built
  `Suspend`-at-`Id` message for pinned-to-pure stores);
- **only under full coverage** (no unknown callees);
- **only with no uncertain contributions**: a rowed argument meeting a *bare-generic* slot moved to a
  new `uncertain` channel in the derivation (`Derivation.deferred`) — the dot-chained discharge
  (`counter.runStateToValue("init")` in a `{Console}` body) captures it there while a strict
  instantiation runs it, the same A.8.6 fact on the row side — and any uncertainty disables pre-mono
  enforcement for the definition. R1's "suspension is row-neutral" survives *where suspension is
  declared*; it is the *bare-generic* slot whose row destination is instantiation-decided.

The post-mono `EffectAccountingProcessor` remains the unconditional ground-truth verifier gating
codegen in every case the pre-mono check declines.

#### A.8.7 The resolver design: post-drain resolution at quiescence (decided 2026-07-26)

The A.8.6 end state needs one type-informed component: something that finishes the desugar's deferred
positions once the instantiation is known. Two architectures were weighed; **the decision is post-drain
resolution** — the resolver runs only when unification is at rest.

**The forced core, common to any design.** First-contact unification is itself a mode decision: in
`pick(readLine, "x")`, eagerly unifying `readLine : F[Str]` into `pick`'s `A` silently commits
pass-through (`A := F[Str]`), and `"x"` then mismatches — unfixable later without retracting solved
metas, which a mutable meta store must never do. So under *any* design, a computation meeting a
bare-generic slot must not unify eagerly; it becomes a **suspended obligation** — (position, slot meta,
argument judgment) — held open until resolved. That much of Phase A/B's *mechanism* survives no matter
what; the decision is only about **who resolves the obligation and when**.

**Rejected: mid-spine resolution (a slimmed Phase A/B).** Resolve each obligation as soon as a sibling
rigidifies its meta, inside the same spine walk, and continue checking with the result — v2's
architecture with the lifter's arm collection swapped for the desugar's 3-way rule. Its virtue is early
resolution; its cost is structural and is precisely v2's bug record: resolution runs while other metas
are half-solved, so *ordering becomes a correctness concern* (which sibling first, what the
resolution's own unifications may capture) — all four of v2's highest-impact bugs were this shape, and
shortening the arms does not remove the interleaving that makes ordering load-bearing. Early errors,
its one advantage, lost most of their value when the row verification started reporting effect leaks
pre-mono at the user's own location; a post-drain mode error still carries the node's original
`Sourced` position, so only the internal phase is later.

**Decided: the post-drain resolver.** Checking runs to drain with obligations held; by quiescence,
everything the rest of the body can determine about the slot metas is determined. Then, as a peer of
the existing `runPostDrainResolution` hooks:

1. classify each obligation against the **solved** meta store, three ways: payload → the desugar's
   strict-hoist rewrite; carrier-headed → suspended pass-through; pinned stack → capture;
2. a **still-unsolved** meta defaults to pass-through — the argument's carrier is adopted
   (`choose(readLine, readLine)`: nothing rigidifies `A`, the branches instantiate it at the carrier —
   v2's default, kept);
3. the chosen rewrite is **spliced as a tree rewrite and the loop re-checks** — the placement rules
   are `RowElaborator`'s own, applied with the mode now known (one rulebook: the desugar finishing its
   job late, never a second in-checker implementation of placement). Each restart strictly reduces the
   obligation count, so the loop terminates;
4. hook ordering in the post-drain fixpoint: mode resolution runs **before** ability resolution, so an
   adopted carrier can still have its instances resolved in the same round.

Why this is the cleaner architecture, beyond taste: (i) ordering ceases to be a correctness concern —
the classifier runs when nothing else is running, so the theft window is structurally absent, the same
way the row channel made theft absent for rows; (ii) one rulebook, since resolution is
rewrite-then-recheck rather than patching an in-flight judgment; (iii) the architectural slot already
exists and is debugged — `CalculatedReturnResolver` already does post-drain body rewrites with restart,
`AbilityResolver`/`CarrierKindChecker` are peers of the same kind; (iv) it is the use-site-verification
cornerstone applied to elaboration: decide where knowledge is maximal.

**Guardrails** (same discipline as the §3 whitelist):

- *The resolver's contract*: it may **read** solved metas and **splice** desugar rewrites that trigger
  a re-check. It may never run inside unification, never retract a solution, never grow an ordering
  arm.
- *The tripwire*: if a shape genuinely requires resolving an obligation *before* the drain can finish
  soundly, that is a stop-and-redecide signal — not a license for a mid-flight arm. The sanctioned
  degree of freedom is the ordering among post-drain hooks, nothing finer.

**Acceptance tests**: `choose`/`pick` (unsolved default vs. sibling-rigidified payload), the
dot-chained discharger (capture through `.`'s bare generic), `foldLeft`'s accumulator (carrier
pass-through), plus the full gate.

**Honest residue.** "The checker ends with zero effect code" becomes "the checker ends with zero
effect *decisions*": one obligation queue held during checking, one post-drain peer. That is the
residue bounded staging always implied — mid-spine resolution would leave strictly more. The deletion
slices then proceed against the resolver: the ladders, the `EffectLifter` arms, the pinning mechanisms,
the slot routers and Phase A/B's resolution logic all die; what Phase A/B leaves behind is exactly the
obligation queue, re-owned by the resolver.

**Landed (2026-07-26): the resolver is implemented and live; full gate green** (every module suite,
HelloWorld, the discharge examples, the shadow compile; the acceptance shapes — `choose`/`pick`, the
dot-chained discharger, `foldLeft`'s accumulator, `andThen(printLine(..), abort)` — all pass). The
shape as built:

- **Suspension** (`Checker.resolveDeferredSlot`, runtime track only): a Phase-A `Deferred` record no
  longer takes any mid-spine decision — it is recorded as a `CheckState.ModeObligation` (arg node, its
  instantiated carrier-headed type, the slot's domain meta, the app node's result type, and the whole
  spine's result type) with **no unification into the slot**, and the slot passes the argument
  provisionally (`SlotOutcome.Suspended`). The compiler track keeps the v2 mid-spine decision verbatim
  (the §8 boundary).
- **The resolver** (`monomorphize/check/ModeResolver`, a `CalculatedReturnResolver`-style CheckIO peer):
  driven from `TypeStackLoop`'s post-drain fixpoint — each round is drain → **mode resolution** →
  ability resolution, so an adopted carrier resolves its instances in the same round. Solved domains
  classify as designed: carrier-headed / applied-meta ⟹ pass (unify, one committed Expected/Actual on
  contradiction); rigid non-carrier ⟹ hoist when the computation's *payload* speculatively fits, else
  whole-unify (the capture — dot-chained discharge — exactly v2's `sequenceBeforeUnify` discipline).
  Still-flex at full quiescence ⟹ the v2 default kept: ride-up occurs-check → adopt, else hoist.
- **Splice + restart**: hoists are spliced by `RowElaborator.spliceResolvedModes` — the desugar's own
  `bindNodes`/`pureWrap` builders, targets matched by node identity, `$row$N` numbering continued — and
  `TypeStackLoop.process` restarts the mono on the rewritten body (fueled loop; each restart strictly
  reduces deferred positions). A signature twin can never request a restart (asserted).

Landing it over the corpus forced three corollaries, each an instance of the A.8.6 discipline rather
than a new judgment call:

1. **The splice applies the desugar's core rule with the mode known.** A hoisted chain's innermost
   continuation must be classifiable on the re-check: a bare-generic core tail
   (`identity($row$1) : ?A`) meeting the machinery's carrier codomain is stolen by first-contact
   unification. The resolver reads the *spine's* solved result type off the store — rigid non-carrier ⟹
   the core is a payload and the splice `pure`-wraps it (the eager `assemble` rule, finished late);
   carrier-headed or undetermined ⟹ left bare.
2. **A suspension-holding spine wraps no mid-spine binds** (corollary 2 of A.8.6, checker-side). With a
   suspended slot the core's carrier-ness is undecided, and `wrapBinds`' flex-core `map` default is a
   first-contact commitment that silently reorders effects (`andThen(printLine("trying"), abort)`
   printed nothing). Bound slots of such a spine become **born-hoist obligations** — mode known
   (payload), placement deferred — and the guaranteed splice-restart re-spells the whole chain
   leftmost-outermost with the suspension deferred inside.
3. **The deferred `let` is an obligation of its own.** A `val`/statement binding over a
   bare-metavariable bound type cannot decide bind-vs-plain at build time; it is recorded
   (`CheckState.LetObligation`) and, at full quiescence, a bound type that resolved carrier-headed gets
   the desugar's binding rewrite spliced (`(x -> rest)(bound)` ⟹ `flatMap(x -> rest, bound)`) — without
   this, a post-drain adoption under an already-built plain `let` would store the computation unrun (a
   silently dropped effect).

Spelling note: a hoisted generic slot now reads as the desugar's one-bind-combinator form —
`flatMap` over a `pure`-wrapped core — where v2's mid-spine bind spelled `map`; behaviourally
identical, and the `MonomorphicTypeCheckTest` lift-group expectations were updated to the new
spellings.

#### A.8.8 Deletion slice 1: the arms the resolver made unreachable (2026-07-27)

The deletion slices are **evidence-driven, not eyeballed**. A temporary arm tracer (a counter per
decision arm, dumped at JVM exit) was hung on every v2 elaboration arm in `Checker` /
`EffectLifter` / `UniformCarrierChecker`, platform-tagged, and the *whole* gate was run under it:
every module suite (871 targets, 1,567 tests) plus a compile of all 40 examples. An arm that fires
zero times across that corpus is what a slice may delete; anything that fires stays. The map is
worth keeping in mind for the remaining slices — the live arms are the uniform bridge's slot
routers (`uniformPayloadSlot` 5.9k, `uniformCarrierSlot` 1.6k, the capture 964), the uniform return
boundary (6.0k), the compile-track default ladder (62k — the §8 boundary, as designed), the
suspension recording (205) and the eager row pin (78).

**Deleted in this slice (544 net lines), each with a zero-fire record:**

- **`EffectLifter.tryBindLift`** — all four call sites (the ladder's `preBind` pre-arm, the failure
  ladder's bind arm, `sequenceBeforeUnify`, `deferredGenericDefault`) fired zero times on *both*
  tracks. The desugar now writes every bind the elaboration needs, so nothing reaches a
  checker-inserted one; the surviving bind producers are the uniform bridge's payload slot (51) and
  the immediately-applied-lambda `let` rule (44), both of which build their `Bind` directly.
- **`EffectLifter.tryIdDefault`** — all three call sites zero. The uniform return boundary's own
  discharge-to-pure arm (`checkReturnBoundary`, 59 fires) is what actually lands a fully-discharged
  body on a pure return now.
- **The `allowBindLift` flag and the arms it selected.** With the bind-lift and `Id`-default arms
  gone, position no longer changes the ladder: `resolveLadder` is pure-wrap pre-arm → unify →
  pure-wrap → mismatch, and `resolveFailureLadder` loses two of its four arms. A carrier the ladder
  cannot reconcile is now a committed mismatch — the fail-safe direction (a loud error, never a
  silently stripped effect).
- **Two of the four pinning mechanisms.** `recordRowArgumentPins` *recorded* 98 deferred pins across
  the gate and `applyPendingCarrierPins` applied **zero** of them — by post-drain finalize the slot
  is never still free, because the eager `eagerRowPinIntoDomain` (78 fires) already pinned it. The
  whole deferred pin-if-still-free path, its `CheckState.PendingPin`/`pendingPins` state and its
  `TypeStackLoop` hook are gone; the eager pin is the only pinning mechanism left.
- **The compiler-track Phase-B remnant.** `sequenceBeforeUnify` and `deferredGenericDefault`
  collapse: their bind-sequencing arms were the `tryBindLift` call sites above (zero), so Phase B is
  now two arms — a still-bare-flex domain adopts the carrier-headed argument, anything else runs the
  ladder.
- **The zero-caller uniform code**: `UniformCarrierChecker.resolveSlot` /
  `finalizeAndMaterialize` / `resolveGenericSlot` and `UniformLadder.resolveGenericSlot` /
  `materialize` / `MaterializedLift` / `LiftKind` / the `DeferredLift` hierarchy (which collapses to
  a `PassJoin(pureActual: Boolean)` flag) — reachable only from their own unit tests.

Gate after the slice: **identical** to before it — 871 targets / 1,567 tests green, the same 36 of
40 examples compiling (`IfDemo` and the three `Plugin*` fragments were already failing), and the
effect examples producing byte-identical output.

Still live, so **not** this slice's business: the uniform bridge itself (`UniformCarrierChecker`'s
return boundary and argument slot, `UniformLadder.resolveSlot`, `Carrier`/`CarrierJoin`), the
`uniformPayloadSlot`/`uniformCaptureSlot`/`uniformCarrierSlot` routers, `mustLiftBeforeUnify` (the
capture's doomed test, 10) and `mustPureWrapBeforeUnify`/`tryPureWrap`, the compile-track default
ladder, and `IdNormalizer`. Those need the *rows* elaboration to take over their remaining
decisions, which is the next slice's subject — not a dead-code sweep.

#### A.8.9 Deletion slice 2: the checker stops inserting binds (2026-07-27)

Slice 1 deleted what the resolver made *unreachable*. Slice 2 is the other half of the method: the
arms that still fired were traced at **outcome** granularity — not "did the router run" but "which
decision did it take, on which source shape" (each hit carrying up to 40 distinct sample sites) — and
the ones that turned out to be *elaboration* decisions were moved to the rows machinery until they
fired zero times, at which point they were deleted.

**What the finer trace showed.** The routers' 5.9k/1.6k/964 entry counts are almost entirely
*routing*, not rewriting. Across the whole gate the runtime track's node-inserting decisions were
only ≈236: the payload-slot **bind** (48), the carrier-slot **pure lift** (40), the return boundary's
`pureIntoCarrier` (78) and `dischargeToPure` (60), plus 10 doomed-capture binds. Everything else was
a pass (`purePayloadPass` 4.5k, `passJoin` 1.7k, `effectfulJoin` 3.2k) — the `runId`/`Id` machinery
that erases downstream.

**Where the surviving binds came from — the dot operator.** Every one of them was a position the
desugar had *correctly deferred*, not one it missed. `printLine(x.field)` reaches the checker as
`printLine(.(x, field))`: the callee `.` is an ordinary generic function, its return is its own bare
binder `B`, so A.8.6 defers — and the deferral propagates outward, because an argument whose mode is
unknown cannot be classified at the enclosing strict slot either. `printLine`'s slot is
declaration-*concrete*, so it never reached the suspension path; the bridge bound it mid-spine
instead. The same shape explains the `++`/`==` cases: a *declaration-generic* slot that a sibling
argument has already rigidified (`"Hello, " ++ readLine` solves `A := String` from the left operand)
forced to a concrete domain before the deferral test ever ran.

**The change: two more suspension sites, no new decision.** Both are the A.8.7 obligation queue
reused, and both classify through `ModeResolver` unchanged:

- **declaration-genericity is read *before* forcing** (`Checker.isDeclarationGeneric`) — a slot whose
  declared type is one of the callee's binders is a deferred position whether or not a sibling has
  solved that meta since. A carrier-headed actual there suspends (`genericArgSlot`); a pure one
  routes normally. This alone was gate-green and output-identical.
- **a payload slot whose actual is carrier-headed suspends** rather than binding: that *is* the hoist
  shape, and hoisting is the desugar's rewrite. Same for the doomed capture (`mustLiftBeforeUnify`),
  which must sequence and never capture.

**One corollary the corpus forced — the core rule needs the carrier *tag*, not the forced shape.**
Routing the `if(readLine == "yes", …)` shape through the splice diverged ("post-drain mode resolution
did not converge"): `pendingHoistTargets` classified the hoisted spine's result as a *payload*
whenever it forced to a non-ambient `VTopDef`, and a discharge stack (`AbortCarrier[IO, String]`) is
exactly that. So the splice `pure`-wrapped a computation, the next round hoisted it back out, wrapped
again, forever. The fix reads the **unforced** spine result: a carrier-role-flagged metavariable
application (`if`'s `{Abort} T` return before its meta was solved) is a computation. That is the
elaboration-threaded tag the cornerstone sanctions — recognising `AbortCarrier` by name would not be.

**Deleted (evidence: `Runtime/uniformSlot/bound` 48 → 0 over the full gate):** the bridge's
`UniformSlotOutcome` sum type entirely (`resolveArgumentSlot` now returns the slot expression; the
payload-slot-with-effectful-actual case is unreachable by construction and throws as a compiler bug),
`freshLiftName` and the now-unread `CheckState.liftCounter`, the checker's `SlotOutcome.Bound` with
`suspendBoundSlot` (A.8.7's born-hoist workaround — with no mid-spine bind there is nothing to convert),
the bind fold in `assembleSpine` and the `paramNeutral` codomain arm it fed, and
`EffectLifter.wrapBinds`. The one remaining bind producer in the whole checker is the
immediately-applied-lambda `let` rule, which calls `bindWrap` directly.

Gate after the slice: **identical** — 871 targets green, the same 36 of 40 examples compiling, every
effect example's output byte-identical to the pre-slice baseline.

Still live for the next slice: the routers themselves (now pure *unification* routing — the
carrier-slot pure lift, the pinned capture with its eager row pin, `payloadFitsDomain`), the uniform
return boundary's four arms, `Carrier`/`CarrierJoin`/`UniformLadder`, and `IdNormalizer`. What they do
is no longer effect *elaboration*; it is carrier-safe unification, which is what §4's "the checker
ends with zero effect code" needs the row channel to absorb next.

#### A.8.10 Deletion slice 3: the checker stops manufacturing `Id` (2026-07-27)

Slices 1 and 2 removed *elaboration* decisions. Slice 3 attacks what was left of the v2 bridge, and the
outcome trace showed at once that most of it was not a decision at all but an **encoding**.

**The invariant, and what it cost.** v2's uniform bridge made every runtime judgment carrier-headed *by
construction*: a pure type `T` was wrapped `Id[T]` (`intoCarrierHeaded`) and a pure term was wrapped
`pure@Effect[Id](term)` (`intoCarrierHeadedTerm`), so `Carrier.split` could peel a carrier off
unconditionally at every slot and boundary. Downstream the `Id`-normalizer erased the whole apparatus
again. Traced at outcome granularity over the full gate (871 targets / 1,567 tests plus a compile of all
40 examples), that round trip was overwhelmingly the identity:

| arm | before | after |
| --- | ---: | ---: |
| `intoCarrierHeadedTerm/idWrap` — a pure term wrapped `pure@Effect[Id]` | 6,641 | *arm deleted* |
| `intoCarrierHeadedTerm/pass` — already headed, nothing to do | 5,282 | *arm deleted* |
| `uniformSlot/purePayloadPass` — `runId(...)` undoing the wrap at a payload slot | 4,504 | **0** |
| `uniformSlot/idPayloadUnwrap` — a *genuine* `Id[T]` value projected | — | 2 |
| `return/pureIntoCarrier` — a pure body lifted into the declared return | 2,233 | 147 |
| `return/purePass` — a pure body meeting a plain return, **no node** | — | 2,081 |
| `return/dischargeToPure` — the genuine `runId` at a fully-discharged pure boundary | 57 | 53 |
| `uniformSlot/carrierPureLift` — a pure actual lifted into a carrier slot | 39 | 37 |

(Counts drift by a few between runs — the pipeline is demand-driven and the suites fork — so read the
orders of magnitude, not the last digit. The behavioural gate is the byte-identical program output below.)

The 4,504 payload-slot unwraps were exactly `runId(pure@Id(x))` — a wrap the checker had just written,
stripped one step later — and of 793 distinct `pureIntoCarrier` return-boundary *sites*, 756 lifted into
`Id` itself. Roughly 95% of the machinery existed to be erased.

**The change.** The bridge no longer wraps; it **classifies**. `UniformCarrierChecker.actualForm` reads
the same positional recognition the rest of the effect machinery uses (`effectCarrierSplit` — the value's
ambient carriers and the unifier's carrier-role flags) plus the compiler-owned `Id` head, and yields one
of three `UniformLadder.ActualForm`s: `Carried(C, T)`, `IdCarried(T)` — a **genuine** `Id[T]` value, the
identity carrier used as ordinary data inside `runId` / an `Effect[Id]` instance — or `Pure(T)`, a term
that simply is not on a carrier. Each ladder arm then reads the actual's form beside the expected slot's
tag, and a pure term needs no node at all unless the position is carrier-**headed**.

**The asymmetry the re-trace caught.** A first cut classified only a literal `Id[T]` as needing its payload
projected, and `carrierPureLift` collapsed from 39 firings to 1. The missing 38 were computations whose
*carrier metavariable had already been solved to `Id`*: `Carrier.split` had reported them as pure (the
`resolve`d carrier is bottom) while `effectCarrierSplit` reports them as carried (the head is still a
role-flagged meta). Passing such a term through unchanged would put an `Id[..]`-typed value in a `?G[..]`
slot, and the join — for which `Id` is *no contribution* — would not object. So the `CarrierSlot` arm
resolves the actual's carrier first and returns `PureLift(projectPayload = true)` when it is bottom, which
is v2's `PassJoin(pureActual)` verbatim. The same rule at the return boundary is `headed(bodyForm)`. This
is the fail-safe direction and the reason the slice is measured twice, before and after.

**Deleted:** `intoCarrierHeaded` / `intoCarrierHeadedTerm` and the `Id`-uniform judgment invariant they
enforced; `isCarrierHeaded`; `carrierSlotLift`'s `unwrapPureId` shortcut (the double-wrap it defended
against — a `pure@Effect[C](runId(pure@Id(x)))` that mis-erased into a `ClassCastException`, finding 3 —
can no longer arise, since nothing writes the inner `pure@Id`), the shortcut's host renamed to the honest
`pureLift`; and `UniformLadder.Outcome.Bound`, whose payload-slot bind arm became `PayloadPass` /
`PayloadUnwrap` / `PayloadBound` (the last unreachable by construction, a compiler-bug throw at the
bridge, still exercised by the mechanism suite for its carrier-safety property).

Gate after the slice: **identical** — 871 targets green, the same 36 of 40 examples compiling
(`IfDemo` and the three `Plugin*` fragments were already failing), and all 36 programs' output
byte-identical to a baseline worktree built at the pre-slice commit.

Still live for the next slice: the routers (`uniformPayloadSlot` / `uniformCaptureSlot` /
`uniformCarrierSlot`, `payloadFitsDomain`, the pinned capture with its eager row pin), the return
boundary's remaining arms, `Carrier`/`CarrierJoin`/`UniformLadder` and `IdNormalizer`. With the `Id`
encoding gone, what those do is visible for what it is — **carrier-safe unification**, the last thing
§4 needs the row channel to absorb before the checker holds no effect code at all.
