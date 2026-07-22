# Effects as a Channel: Full Separation of Effects from Type Checking

Status: **DESIGN** — no implementation. The carrier-based elaboration in `monomorphize/check`
(`EffectLifter`, `EffectResidualChecker`, ambient-carrier tracking) is current until the flip in
Phase 4 below.

## 1. The problem

Open effect rows are desugared (`core/processor/EffectSugarDesugarer.scala`) into ordinary
higher-kinded-constrained generics *before* checking: `{Console} Unit` becomes `F[Unit]` with
`F[_] ~ Console`. From that point on, the information "this application is an effect carrier, not
a data container" is erased, and the checker spends `EffectLifter`'s 400+ lines reconstructing it
from *shape* — ambient-carrier state, flex-payload tests, arity comparisons, Id-defaulting
ladders. The lifter's own documentation concedes the endgame: *"the expected being a genuine
effect carrier is not syntactically distinguishable from a plain container here (`IO` and `List`
are both `VTopDef` constructors)."*

The consequence is a treadmill: every new program shape needs a new guard, and every guard needs
sub-guards to not steal a legitimate unification. The compound-state fix (2026-07-20) added the
equal-arity arm with three guards; the eliot.file work (2026-07-22) immediately found the case
those guards exclude (`?F[List[String]] ~ List[A]` with a *concrete* payload — structurally
identical to the legitimate `?F[String] ~ Box[String]` HKT dispatch, and therefore undecidable by
structure). The same erasure is behind the effectful-`catch`-handler failure (`tryIdDefault`
commits a still-flex carrier to `Id` before the handler's `Console → Suspend` demand is
collected) and the `if(c, None) else Some(x)` mis-defaulting (arms threaded through the `Abort`
carrier machinery commit a flex element type before the sibling arm constrains it). These are not
three bugs; they are one representation problem.

## 2. Why full separation is possible here

Modular languages must decide effect well-formedness from declared signatures alone, so effects
must ride the types through unification. Eliot deliberately is not modular: the **use-site
verification cornerstone** monomorphizes the whole program from `main`, and post-drain everything
is concrete — every call monomorphic, every callee's declared row known, nothing flex. The
questions the lifter answers heuristically mid-check (*is this term effectful? what carrier
realizes it? bind or pure?*) are trivially decidable after monomorphization. Even the `Checker`'s
Phase A/B flex-slot deferral exists only because the decision is currently made too early.

There is also an in-repo precedent for exactly this move: the **refinement channel**
(`docs/bounds-as-refinements.md`). `Int` bounds used to be type parameters, mixing with
unification; they moved out of the type language into compiler-tracked meta-information
(`monomorphize/channel/RefinementChannelProcessor` — a post-pass over `MonomorphicValue`, its
results stamped by `reconcile`). Effects join bounds as the second channel: value types describe
data shape; refinements and effects are checker-adjacent information verified and elaborated
downstream. The effects channel is even *more* separated than the bounds one — bounds are
consulted mid-check (`where`, `Coerce`); the effect channel is fully **inert through the
checker** and consumed only by two downstream processors.

This is also the endpoint of an existing trajectory: carrier-free `main`, pinned rows, ambient
effects, and the `eliot.carrier` split each evicted carriers from a piece of the *surface*. The
remaining ambiguity all lives in the one place carriers still exist — inside the checker.

## 3. The design at a glance

The two row forms, which today differ only in desugar strategy, become two genuinely different
things:

- **Open row `{Console} Unit` = ambient effects = the channel.** "What this expression does."
  No carrier generic is minted; the value type is the payload (`Unit`); the row is structured
  signature metadata the checker never reads. Verified and elaborated post-mono.
- **Pinned row `{Throw[E] | Id} A` = a concrete type = a value.** "A computation as data." The
  desugar to the canonical carrier stack (`ThrowCarrier[E, Id, A]`, `<Ability>Carrier`
  convention, leftmost-outermost) is unchanged — but the result is now an *ordinary* concrete
  data type, fully inert in checking: no ambient recognition, no lifter interaction. Pinning is
  the **reification boundary**: the one door between the channel world and the value world.
  (First-class effectful values are load-bearing — eliot-test stores
  `data TestCase(name: String, body: {Throw[AssertionError] | Id} Unit)` and enumerates
  `type Test` values via `namedValues` — so pinning cannot be retired; it changes role.)

Responsibilities move as follows:

| concern                        | today                                             | after                                    |
|--------------------------------|---------------------------------------------------|------------------------------------------|
| row in a signature             | desugars to carrier generic `F[_] ~ E`            | stripped to payload + channel metadata   |
| bind/`pure` decision           | `EffectLifter` mid-check, shape heuristics        | weaver, syntax-directed, post-mono       |
| carrier identity               | unification (incl. `main`'s boundary trick)       | weaver synthesis (canonical stacks)      |
| verification (residual ⊆ declared) | `EffectResidualChecker` off carrier residue   | row accounting, purely syntactic         |
| discharge                      | structural (inner transformer carrier)            | structural (reify boundary)              |
| pinned rows                    | concrete types + ambient recognition patches      | concrete types, nothing else             |
| compile-time effects           | checker evaluates on `Either` carrier             | unchanged (bounded mini-weave, §8)       |

## 4. The channel: representation and the reify boundary

**Desugar.** `EffectSugarDesugarer` stops minting the carrier generic for open rows. An open
`{E1, E2} A` anywhere in a signature rewrites to `A`, and the definition gains a structured
**declared row**: the set of entries (ability FQN + type arguments), plus **row positions** —
which parameter types carried a row (and where, e.g. an arrow codomain) and whether the return
did. Today's "one shared carrier per signature" rule carries over as semantics: a signature has
*one* ambient effect context, and every open row in it denotes that same context. The metadata is
forwarded on the existing fact chain (`FunctionDefinition` → `NamedValue` → `ResolvedValue` →
`OperatorResolvedValue` → mono facts) per the lean-fact-flow rule — a new field, not a parallel
projection fact. Pinned rows desugar exactly as today.

**Machinery entries.** `Effect` in a row (`action: A => {Effect} Unit`) is the
*ambient-transparent* marker: the actual argument's effects flow into the caller's ambient
context. `Suspend` remains the platform-I/O base requirement every fine I/O effect rides. Both
keep their current surface meaning; they simply become channel vocabulary instead of constraint
vocabulary.

**The reify boundary.** A direct-style effectful expression meeting a *pinned-row-typed position*
(a `data` field like `TestCase`'s body, a discharger's pinned parameter like
`runThrow(obj: {Throw[E] | G} A)`) is a **reify point**: the expression's effects named by the
pinned entries are captured into the concrete stack rather than joining the ambient. Crucially,
reify points are **declared-type-directed**: pinned types appear only in declared signatures and
fields, never inferred for direct-style terms — so reify insertion needs no unification, no flex
metas, no heuristics. The checker types `reify(e) : Stack[…]` from `e : A` by one trivial rule;
whether the capture is *legal* (the expression's row ⊆ pinned entries ∪ base) is the channel
verifier's job. The dual, **reflect/run** (a pinned value's entries unwinding into the ambient),
is what dischargers and accessors already do — a pinned value is plain data, and its accessors
(`runThrow` the field accessor, `runId`) remain plain functions.

**Sequencing semantics become spec, not artifact.** `val x = <effectful>` sequences now — the
effects join the ambient at the binding, and `x` is the payload. Argument position at a reify
point captures instead. This is today's documented behaviour ("a discharger must receive the
effectful call as an expression, never a `val` binder"), but it stops being a carrier-plumbing
accident and becomes a coherent, explainable rule: *`val` runs, reify captures.*

**Dischargers need no recognition mechanism.** Consumption is structural one level up from
today: effects named in a pinned parameter's entries are captured at the reify boundary and are
therefore simply absent from the caller's derived row. The stdlib discharger signatures
(`runThrow`, `catch`, `else`, `runStateToPair`, `provide`, the Writer dischargers) are unchanged,
and the pinned-row/accessor merge story (`docs/effect-row-tails.md`) is untouched.

**Parameters with open rows** (`def getOr(x: {Abort} String, d: String)`) become "a computation
over the ambient": reified at the call over the caller's ambient base, first-class inside the
body. The current limitation — such a handler must return a carrier-headed type, because a
caller-chosen carrier can never default to `Id` — dissolves: the weaver knows the stack.

## 5. Row accounting and verification (per mono key)

A new post-mono processor (template: `RefinementChannelProcessor`) computes each mono'd value's
**derived row** by a bottom-up walk of the checked body:

- an **ability method** reference contributes its owning ability (machinery abilities excluded);
- an **ordinary callee** contributes its declared row (with `Effect`-transparent entries expanded
  to the rows of the argument values flowing into its effect-marked positions — decidable,
  everything is monomorphic);
- a **reify point** subtracts: the captured expression contributes nothing beyond what its pinned
  base lets through;
- **`Inf`** is an ordinary entry and propagates through the same union — the totality story is
  unchanged.

Verification then checks, with source-anchored diagnostics:

1. **derived ⊆ declared** — the residual check, now purely syntactic ("performs the effect 'X'
   but does not declare it" — for *every* effect uniformly, including the `State`/`Throw`/`Abort`
   leaks that today fail cryptically inside `AbilityResolver`);
2. **reify legality** — captured expression's row fits the pinned entries;
3. **pure-position fail-safes** — an effectful expression in a position that cannot absorb or
   capture effects (a non-pinned `data` field, a type-level argument, an effectful lambda passed
   into a parameter declared without a row: "this handler performs 'Console' but a pure function
   is expected") is an error, never silent.

This replaces `EffectResidualChecker` (including its declared-pure fail-safe) with strictly
simpler logic: no ambient-head re-forcing, no carrier-argument inspection — the channel *is* the
ground truth. The fact it produces is also the LSP's hover source for rows.

## 6. The weaver (per weave key)

A second post-mono processor performs the direct-style → monadic elaboration the checker does
today, over concrete terms:

- **Carrier assignment.** The ambient stack is synthesized, not unified: the platform boundary
  supplies the base (the jvm target's `SyntheticMainSourceProcessor` currently binds `main`'s
  carrier to `IO` by unification — it instead *tells the weaver* the base; `Suspend`-riding
  entries ride the base; control-effect entries reified at discharge points get their canonical
  `<Ability>Carrier` layers in pinned order). The canonical-stack computation built for pinned
  rows becomes the weaver's core algorithm.
- **Weave key = mono key × assigned stack.** Today the carrier is a type argument, so a
  `{Throw[E]} Unit` helper used over `Id` and over `IO` yields two mono keys; after erasure it is
  one mono key woven at two stacks. Cardinality of generated code is unchanged — the carrier
  moves from a *type* dimension to a *weave* dimension. `used`/`uncurry`/codegen re-key on woven
  facts (jvm mangling gains the stack component).
- **Bind/`pure` insertion.** Blocks are already lowered to immediately-applied lambdas
  (`BlockDesugaringProcessor`: `val x = e; rest ⟹ (x -> rest)(e)`), so the weaver monadifies
  exactly where an applied argument or spine element has a non-empty row, emitting
  **already-resolved** references to the concrete carrier's `Effect` instance methods
  (`NamedValuesRewriteProcessor` is the precedent for a rewrite processor injecting lowered
  references). No flex slots exist, so there is nothing to defer.
- **Reify lowering.** `reify(e)` becomes construction of the concrete stack around the woven
  computation.
- **Evaluation order** becomes a one-place spec commitment. v1 preserves today's order (the
  resolved application's argument order, as the checker's bind insertion produces now); moving to
  source order (the weaver has `Sourced` positions) is a recorded follow-up decision, not an
  accident.

Pipeline placement (`LangProcessors`): after `MonomorphicTypeCheckProcessor` /
`RefinementChannelProcessor`, before `UsedNamesProcessor` and `MonomorphicUncurryingProcessor`.

## 7. What is deleted, what stays

**Deleted** (the flip, Phase 4): `EffectLifter` entirely (both must-*-before-unify arms,
`tryBindLift`/`tryPureWrap`/`tryIdDefault`, `wrapBinds`), the `Checker`'s Phase A/B flex-slot
deferral, `CheckState.ambientCarriers` + `recordAmbientCarriers` + `effectCarrierSplit`,
`EffectResidualChecker`, `CarrierKindChecker`'s carrier-specific duties, and the synthetic-main
carrier-unification trick. The `Suspend[Id]`-shaped failure class and the pinned-row
block-sequencing recognition patch (`recordAmbientCarriers` on concrete stacks) go with them.

**Stays**: pinned rows and everything below the line — the platform carrier `data` types and
their `Effect`/`Suspend` instances (`eliot.carrier`), `Id` (now purely the pure pinned base; its
compile-time overlay remains for §8), the discharger signatures and the accessor merge, the
`termination` story (`Inf` as a row entry), `namedValues`, eliot-test unchanged.

**Stdlib deltas are additive**: parameter rows make previously inexpressible signatures sayable —
notably an effectful-handler `catch` (`onError: E => {Effect} A`), turning the eliot.file
`catch`-handler failure from a checker artifact into an ordinary vocabulary choice with a clear
error when the pure variant is given an effectful handler.

## 8. The compile-time residue

The checker itself *consumes* effect discharge on the compiler platform: effectful signatures
(`{Throw[String]} Type` calculated returns, guards) evaluate on the `Either[String, _]` carrier
and are read back by `CalculatedReturnResolver`. This cannot move downstream — but it is closed
and small: only pure control effects (no `Suspend`), one fixed carrier, no polymorphism. The
compiler track keeps a **fixed mini-weave** (eagerly monadify signature bodies onto
`Either[String]`) inside `CompilerMonomorphicTypeCheckProcessor`'s path. Honest statement:
separation is complete for the runtime track and bounded-not-total for the compiler track.

## 9. Held invariants and interactions

- **Normalization must not observe erasure.** With rows out of value types, an effectful subterm
  is pure-typed; any future normalizer that inlines or prunes (reduce-and-reify) MUST treat
  row-non-empty terms as observation-ordered — it consults the channel before deleting,
  duplicating, or reordering. Today's mono output is not aggressively normalized, so this is an
  invariant on future work, recorded here so it is not discovered as a miscompile.
- **Pinned types are declared, never inferred** — the invariant that makes reify points
  syntax-directed. Inference never produces a carrier-stack type for a direct-style term.
- **Suspend-riding effects still cannot be pinned** (first-class `Console` values): the ambient
  half of that wart disappears (the weaver assigns the platform base — no user-visible
  `Throw`-vs-`Console` asymmetry in direct style), the first-class half remains, and the designed
  `Suspended` base-alias extension (`docs/effect-row-tails.md` §Limits) is still the answer.
- **Types-are-values, decided.** Open rows stop being values — a deliberate second channel beside
  the refinement channel, same rationale as bounds-as-refinements: strictly downstream of type
  formation, never flowing back into a type. Where a row must *be* a type, the pinned form is
  exactly that value. Recorded as a Cornerstone amendment when Phase 4 lands.
- **LSP**: hover composes the value type with the declared/derived row from the accounting fact;
  `GroundValueRenderer`'s stack→pinned-row rendering stays for pinned values. Signature help and
  diagnostics get rows from the channel, not from carrier types.

## 10. Migration phases

The flip is wide, so the plan follows the gated-flip playbook (signature-unification precedent):
build the new path dark, shadow-verify semantics, gate the flip on a flag, delete only after both
tracks are green.

- **Phase 1 — channel plumbing, dark.** `EffectSugarDesugarer` records the structured declared
  row + row positions as new signature metadata *while still* performing today's carrier desugar.
  Metadata forwards through the fact chain to mono facts. Zero behaviour change; lands
  independently.
- **Phase 2 — shadow accounting.** The row-accounting processor (§5) computes derived rows from
  the channel and *compares its verdicts* against `EffectResidualChecker` across the full lang +
  jvm + eliot-test + examples suites, logging divergences. Every divergence is a semantics we did
  not understand — resolved before anything flips. Deliverable: byte-identical accept/reject
  behaviour in shadow mode.
- **Phase 3 — the gated new path.** A compiler flag (`effect-channel`) switches the desugar to
  strip open rows, disables the lifter arms, and enables the weaver. Grown in slices, each with
  its tests green under the flag while the default path stays untouched:
  - **3a** ambient `Suspend`-riding effects only: bind insertion, base assignment at the
    synthetic main. HelloWorld/Console examples green.
  - **3b** control effects, reify points, dischargers, weave keys threaded through
    `used`/`uncurry`/codegen (mangling gains the stack component).
  - **3c** higher-order: parameter rows, `Effect`-transparent positions (`foreach`), lambdas,
    the additive effectful-handler signatures.
  - **3d** pinned rows first-class: eliot-test suite green under the flag.
  - **3e** the compiler-track mini-weave (`Either` carrier): guards + calculated returns green.
- **Phase 4 — flip and delete.** Flag becomes the default, then the deletions of §7, the
  Cornerstone amendment, the doc/skill sweep (`eliot-code` global skill, `eliot-layers`,
  CLAUDE.md effect section), LSP hover rewire. The old path is removed, not kept as a mode.
- **Phase 5 — follow-ups unlocked.** Row-bearing diagnostics everywhere (the friendly "performs
  X but does not declare it" for *all* effects), the evaluation-order decision (source order?),
  `Suspended` for first-class platform actions, and reduce-and-reify's channel consultation
  (§9) when that design activates.

## 11. Risks

- **Shadow divergence volume** (Phase 2) is the honest unknown: the current lifter's behaviour
  is the sum of its guards, and some accepted programs may rely on shapes the clean semantics
  rejects (or vice versa). Budget real time; every divergence is either a bug today or a rule the
  doc must state.
- **Weave-key plumbing** (3b) touches `used`/`uncurry`/codegen keys — mechanical but broad; the
  mono-key precedent bounds the design.
- **Two paths during Phase 3** cost maintenance; slices keep the window short, and Phase 2's
  shadow harness doubles as the equivalence test.
- **Error-message regression** at the flip: the channel's diagnostics must be written up-front
  (§5), not recovered later — they are the point of the exercise.

## 12. Open questions

1. Exact metadata shape on the fact chain (row entries + positions) — one field or two, and its
   `signatureEquality` treatment in the layer merge (rows must merge character-exact like the
   rest of the signature).
2. Whether `reify` needs surface syntax for users (the design says no — declared-type-directed
   insertion covers every current use; an explicit form could be added later for clarity).
3. Whether parameter-row reification defaults the base to the caller's ambient stack or its pure
   prefix (`Id`) when the pinned entries cover the whole row — observable only through
   first-class capture of `Suspend`-riding effects, which v1 forbids anyway.
4. Evaluation order: keep resolved-argument order or move to source order (v1 keeps; §6).
