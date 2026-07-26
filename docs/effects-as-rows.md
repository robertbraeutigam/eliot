# Effects as Rows, v3: Declared Suspension + a Desugared Elaboration

Status: **R1–R4 COMPLETE (2026-07-26) — shadow-verified, not yet flipped.** Successor direction to
`docs/effects-as-channel.md` (v2, still the live implementation). The row checker (`lang/.../row/RowChecker`)
sweeps the real corpus with zero v2 disagreements (R3), and the elaboration desugar
(`lang/.../row/RowElaborator`) is twin-verified on 30 shapes and **shadow-compiled end to end**: the full
corpus — stdlib, jvm layer bodies and the synthetic entry included — recompiled from pre-elaborated facts
runs behaviorally identically (R4, `RowElaborationShadowCompileTest`). Both remain **unwired**; v2 stays
the live, green default and nothing changes behaviour until **R5 (the flip)** — the next step, starting
with the §6 semantic-break corpus audit. Progress detail per step: §8.

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
- **R5 — flip:** mono consumes elaborated facts; the checker's effect machinery goes cold; delete in
  slices (per-slice gate: lang + jvm tests, HelloWorld, eliot-test).
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
