# Effects as a Channel: Full Separation of Effects from Type Checking

Status: **DESIGN + Phases 1–2 landed + Phase 3 in progress behind `--effect-channel`** (dark plumbing
→ shadow accounting → the gated effect-blind path: desugar/checker, real accounting, weaver slice 1,
codegen redirect to the woven fact).
The carrier-based elaboration in `monomorphize/check` (`EffectLifter`, `EffectResidualChecker`,
ambient-carrier tracking) is still the live **default** path and drives compilation unchanged; the
channel path exists only under the `--effect-channel` flag (developer-only until a program runs
end-to-end under it — see §0), and the flip in Phase 4 makes it the default and deletes the old path.
Implementation status is tracked per-phase in §10; the concrete landed state is summarised in §0
immediately below.

## 0. Implementation state (handover)

Phases 1–2 (§10) and the first slices of Phase 3 are implemented and committed on `master`. The
**default** compiler behaviour is unchanged: Phases 1–2 are dark/shadow, and everything Phase 3 is
gated behind `--effect-channel` (off by default). Under the flag the checker is effect-blind and the
channel is the real verifier, but no program runs end-to-end yet (the weaver's remaining slices —
bind insertion, codegen redirect, entry-point — are pending), so the flag is developer-only.
Concretely:

- **Phase 1 (dark plumbing) — done.** A generic `EffectRow[C]` (`ast/fact/EffectRow.scala`) captures a
  signature's **open** rows, position-attributed (`returnEffects` = the value's own ambient row;
  `parameterEffects` = per-value-parameter callback rows). `EffectSugarDesugarer.desugar(function)`
  populates it from the open rows *before* the carrier rewrite erases the `{…}` nodes (open rows only —
  body rows and generic-bound rows excluded; pinned rows never populate it). It is threaded down the
  value fact chain exactly like `paramConstraints`, converting the entry type at each hop:
  `FunctionDefinition → NamedValue → ResolvedValue → BlockDesugaredValue → MatchDesugaredValue →
  OperatorResolvedValue` (the termination/saturate/namedvalues wrappers carry it for free). It is **not**
  on `MonomorphicValue` — accounting reads the declared row by `vfqn` from the `OperatorResolvedValue`,
  per the refinement-channel precedent. It is never part of any `signatureEquality` (the desugared
  signature still carries the character-exact merge check while both paths coexist).

- **Phase 2 (shadow accounting) — done, verified byte-identical.** The channel-sourced verdict is
  computed **inside `EffectResidualChecker`** at its existing verdict point, not (yet) as a standalone
  post-mono processor — a deliberate deviation from §5/§6's "new processor" so the shadow sees the *same
  value at the same moment with the same ambient/body and both verdicts*, isolating the one variable
  Phase 2 de-risks: sourcing declared rows from the channel vs. the carrier-binder constraints. The
  ability-method handling and the ambient-ride/discharge filtering are borrowed unchanged (their
  post-mono reconstruction is Phase 3's job). `channelEffectsOf`/`channelDeclaredFor` read rows from
  `effectRow`; `shadowCompareSubset`/`shadowCompareVerdict` `warn` (marker `EFFECT-CHANNEL-SHADOW`) on a
  verdict divergence and `debug` on a set-only difference — never acting on the channel verdict.
  Verified **zero verdict divergences** across `lang.test`, `jvm.test`, all example mains, and the
  eliot-test suite.

  The one **rule the shadow surfaced** (§11 anticipated exactly this): a **carrier-machinery ability
  implementation** — the `implement Effect/Suspend/Throw/State/Abort/Writer[…Carrier]` methods, e.g.
  `Abort` lifted through a `StateCarrier`, which *are what the `{…}` sugar desugars to* and so cannot use
  the sugar — declares its effect through its carrier constraint (`[G[_] ~ Throw[E2]]`), a source the
  open-row channel does not capture. The channel treats these as an explicit exception
  (`channelDeclaredFor`: for a `Qualifier.AbilityImplementation` value the row is read the current,
  carrier-constraint way). This keeps the shadow byte-identical for machinery while the channel is
  validated for all ordinary code, and it is the rule Phase 3+'s real accounting must also honour for
  these impls. (Note: such an impl's module is generally *not* the ability's module — the four cases
  found were cross-carrier lifts like `eliot.effect.State::abort^Abort#StateCarrier` — so a naive
  ability-FQN reconstruction from the value's own module is wrong; reading `EffectCarriers.declaredEffects`
  avoids it.)

- **Phase 3 — foundation landed; accounting + weaver pending.** The `--effect-channel` flag exists
  (`LangPlugin.effectChannelKey`, threaded to `CoreProcessor` and both mono processors → `Checker` →
  `AbilityResolver`). Under it the desugar is **effect-blind**: `EffectSugarDesugarer.desugarChannel` strips
  open rows to payload (no carrier minted) and *carrier-erases effect-ability methods* — an ability with a
  higher-kinded `F[_]` carrier has its methods' carrier dropped and every `F[X]` rewritten to `X`
  (`Console[F].printLine : F[Unit]` ⤳ `printLine : String -> Unit`), while the ability **marker keeps its
  carrier** as the queryable "this is an effect ability" signal (every HKT ability in the tree is an effect
  ability; the first-order ones — `Eq`/`Show`/`Numeric`/`Combine`/`Meta`/… — are untouched). `AbilityResolver`
  then **leaves effect-ability references unresolved** under the flag (recognised via the marker's HKT binder),
  so the quoter emits them abstract instead of aborting on the (correct) `NoImplementation`; first-order
  ability demands still resolve/error as before. Net: an effectful program **monomorphizes effect-blind**, its
  effect operations surviving as abstract ability-method references. Verified: default suites byte-identical
  (flag off); flag-on `EffectChannelDesugarTest` (Console + Abort mono effect-blind, the op stays an abstract
  `Ability` ref, a first-order `Show` with no instance still errors). The `EffectLifter`/`EffectResidualChecker`
  are inert under the flag (no carriers to recognise) but not yet explicitly disabled.

  The **§5 accounting is now built** (`monomorphize/channel/EffectAccountingProcessor` + the `EffectAccounting`
  fact, on the `RefinementChannelProcessor` template): a post-mono rider on `MonomorphicValue` that computes the
  value's **derived row** — each abstract effect-ability reference contributes its ability (machinery excluded),
  each ordinary callee contributes its declared row (`OperatorResolvedValue.effectRow`), unioned — and requires
  `derived ⊆ declared`, aborting with the friendly "performs the effect 'X' but does not declare it" on a leak.
  It is the real verifier under the flag (replaces the Phase-2 shadow), verified by `EffectAccountingTest`
  (derive/subset/propagate accept + undeclared-direct/undeclared-propagated reject). Transparent-parameter
  expansion, reify/discharge subtraction, and the carrier-machinery-impl exception (§0 Phase 2, §11) are later
  accounting slices.

  The **§6 weaver's first slice is built** (`monomorphize/channel/WovenValueProcessor` + the `WovenValue` fact,
  mirroring `MonomorphicValue`'s consumable shape): carrier assignment + effect-operation resolution for the
  Suspend-riding base carrier. Two enabling changes: the carrier **machinery** (`Effect`/`Suspend`) is now kept
  *non-erased* under the flag — only *user* effect abilities are carrier-erased — which is load-bearing (erasing
  `Suspend` would break the `F ~ Suspend` guard `Console`'s instance carries) and lets effect-instance carrier
  towers monomorphize normally, so the weaver only resolves *top-level* user operations rather than re-weaving
  the tower; and the ability↔impl signature-conformance check is relaxed for a user effect ability under the flag
  (ability carrier-erased `op: Unit`, instance not `op: F[Unit]` — conform by construction, the instance's own
  mono still type-checks its body). The weaver walks the mono body, resolves each abstract user effect-operation
  reference to its concrete instance method via `AbilityImplementation` at the base carrier (exactly as
  `PostDrainQuoter.resolveIfAbility` does), and carrier-wraps an effectful value's signature; verified by
  `WovenValueTest`.

  The **codegen redirect is now done** (the next §6 slice): the post-monomorphization codegen chain reads the woven
  value in place of the `MonomorphicValue`. `WovenValue` gained a `naturalArity` (mirroring `MonomorphicValue`'s), and
  the three codegen-driver reads were swapped `MonomorphicValue.Key`→`WovenValue.Key`: `UsedNamesProcessor` (the
  breadth driver, so the *woven* body — with its resolved effect-operation references, and later its inserted
  `flatMap`/`pure` — is what gets walked and whose callees get materialised), `MonomorphicUncurryingProcessor` (its
  transformer source), and the jvm `ExpressionCodeGenerator`'s natural-arity read. The refinement/accounting reads
  stay on `MonomorphicValue` (they are parallel channels off the effect-blind body, upstream of weaving). Because
  `WovenValue` is the **identity image** of its `MonomorphicValue` off the flag (no base carrier), this is
  behaviour-neutral: verified byte-identical across `lang.test`, `jvm.test`, and the example mains (HelloWorld and the
  effect/state/throw/discharge examples build and run unchanged).

  **Remaining for a *running* program under the flag (multi-part):** `flatMap`/`pure` insertion (nested effectful
  args, blocks); precise carrier-headed node types on the woven body (kept as effect-blind payload types here);
  control-effect carrier stacks (`weave key = mono key × stack`); the base-carrier `Configuration` key that
  `JvmPlugin` sets to `eliot.jvm.IO` (threaded into the existing `LangProcessors(baseCarrier = …)` call the way
  `effectChannel` already is); and the **entry-point rework** — the synthetic main's `block(main)` needs
  `main : IO[Unit]`, but the effect-blind checker sees `main : Unit`, so the platform entry must run the *woven* main.
  Until those land the flag stays developer-only (per the fail-safe rule it can never become default before the whole
  reachable program is verified and woven end-to-end).

- **Phase 3 remaining slices + Phases 4–5 — not started.** The weaver's remaining work (bind/`pure` insertion,
  precise woven node types, control-effect weave-key stacks, the base-carrier config, and
  the entry-point rework — the path to a running program, above), the later accounting slices
  (transparent-parameter expansion, reify/discharge subtraction, the carrier-machinery-impl exception), the
  flip/deletions, and follow-ups remain as designed below. Phase 1's `EffectRow` and Phase 2's shadow are the
  substrate they build on; the shadow (and the entire `EffectResidualChecker`) is deleted at Phase 4.

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

The user-facing model fits in three sentences, and everything below exists to make them true:

1. **Types ignore effects.** `{Console} List[T]` type-checks exactly as `List[T]`, everywhere —
   chain it, fold it, match on it. (`something.foldLeft(…)` on a `{Console} List[T]`-returning
   `something` just works; today it needs a `val` workaround.)
2. **Effects run where they are written.** An effectful expression performs its effects at its
   position, joining the enclosing definition's ambient row; the compiler checks performed ⊆
   declared — with effect-vocabulary diagnostics, never type errors.
3. **Pinned means captured.** The one crossing point between the two worlds: a position whose
   *declared* type is a pinned row (`{… | base}`) captures the computation as a value instead of
   running it. Open rows never capture. Capture positions are therefore always visible in a
   signature, never inferred.

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

**Open rows on by-value parameters are rejected.** `def getOr(x: {Abort} String, d: String)`
would be a third semantics: under strict evaluation the argument's effects run at the *call
site*, so the row could neither run "inside" the callee nor capture. To keep the model at one
rule (*open = runs here, pinned = captured*), the desugar rejects it — mirroring the stored-row
rule for `data` fields — with the fix in the message: pin the tail for capture
(`x: {Abort | G} String`, the spelling every stdlib discharger and eliot-test's `expect` already
use) or drop the row. Rows on *function-typed* parameter positions (`action: A => {Effect}
Unit`) are unaffected — they describe the function's own row and flow through the transparency
rules above. The current carrier-parameter limitation ("such a handler must return a
carrier-headed type") becomes visible arithmetic instead of a checker artifact: capture over
`Id` can return pure; capture over a generic tail returns what the tail still carries — spelled
in the signature either way.

## 5. Row accounting and verification (per mono key)

*Implementation note (§0): the standalone post-mono processor described here is **built** —
`monomorphize/channel/EffectAccountingProcessor` + the `EffectAccounting` fact (`RefinementChannelProcessor`
template), the real `derived ⊆ declared` verifier under `--effect-channel`. It currently implements the two
core contribution rules below (effect-ability method → its ability; ordinary callee → its declared row) plus
the subset check; the transparent-parameter expansion, reify subtraction, and the machinery-impl exception are
later slices. Phase 2 (landed) proved the row logic first by shadow-comparing inside `EffectResidualChecker`,
so the semantics were validated byte-identical against the current checker before the processor was built.*

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

**Exactness (held property).** The accounting is exact in the same sense the in-mono residual
check is exact: computed per concrete instantiation of the whole program, never from
declarations-before-instantiation — the deleted pre-mono phase's inexactness is *not*
reintroduced. Its inputs are syntactically complete: an effect enters a body only through an
ability-method reference or a callee's declared row (transparent markers expanded at the
concrete arguments of each monomorphic call), and leaves only through a declared capture
position — there is no semantic recognition step (ambient-head forcing, carrier-instance
matching) to mis-fire, which is where the current machinery has already silently dropped
effects once (the pinned-row block-sequencing bug). Granularity is unchanged and intended:
declaration-level per instantiation (a declared effect counts on every path) — the static
effect contract itself, not an approximation. The weaver is the second net (a row-non-empty
term it cannot weave cannot reach codegen), and Phase 2's shadow gate turns this whole claim
into a tested equivalence with the current exact checker rather than an assertion.

## 6. The weaver (per weave key)

*Implementation note (§0): the **first slice is built** — `monomorphize/channel/WovenValueProcessor` + the
`WovenValue` fact — doing carrier assignment and effect-operation resolution for the Suspend-riding base carrier
(the "Carrier assignment" and instance-resolution parts below). `flatMap`/`pure` insertion, precise woven node
types, control-effect weave-key stacks, the codegen re-key, and the entry-point rework are later slices; see §0.*

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
  accident. The same commitment fixes *argument strictness*: an effectful argument to a
  non-capture position runs at the call site, before the callee — delayed effects are always
  spelled, as a function or a pinned capture. Today's implicit suspension of an effectful
  argument into a bare carrier-typed slot has no counterpart; the only such slots in real code
  are the dischargers' pinned parameters, which remain capture positions (§4 rejects the
  open-row parameter form outright).

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

- **Phase 1 — channel plumbing, dark. ✅ Done** (see §0). `EffectSugarDesugarer` records the
  structured declared row + row positions (`EffectRow[C]`, `ast/fact/EffectRow.scala`) as new
  signature metadata *while still* performing today's carrier desugar. Metadata forwards through the
  fact chain (mirroring `paramConstraints`, stopping at `OperatorResolvedValue`, which the mono
  phase's input carries). Zero behaviour change; landed independently.
- **Phase 2 — shadow accounting. ✅ Done, byte-identical** (see §0). Realised *inside*
  `EffectResidualChecker` rather than as the standalone post-mono processor of §5 (a deliberate
  choice — the tightest possible shadow, comparing both verdicts on identical inputs; the standalone
  processor is built in Phase 3 where it becomes the real path). It computes the derived row from the
  channel and compares its accept/reject verdict against the current carrier-constraint verdict,
  logging divergences (marker `EFFECT-CHANNEL-SHADOW`). Verified byte-identical across the full lang +
  jvm + eliot-test + examples suites; the one divergence class it surfaced (carrier-machinery ability
  impls) is now an explicit, documented exception (§0, §11). Deliverable met: byte-identical
  accept/reject in shadow mode.
- **Phase 3 — the gated new path.** A compiler flag (`effect-channel`) switches the desugar to
  strip open rows, disables the lifter arms, and enables the weaver. Grown in slices, each with
  its tests green under the flag while the default path stays untouched:
  - **3-foundation (landed).** Flag plumbing + the effect-blind desugar (strip open rows to payload;
    carrier-erase *user* effect-ability methods, marker keeps its carrier as the effect signal; the
    machinery `Effect`/`Suspend` kept non-erased) + the resolver leaving user-effect refs abstract.
    Effectful programs now *monomorphize* effect-blind (effect ops survive as abstract ability refs); see §0.
  - **§5 accounting (landed).** The standalone `EffectAccountingProcessor` — the real `derived ⊆ declared`
    verifier under the flag (replaces the Phase-2 shadow); see §0/§5.
  - **§6 weaver slice 1 (landed).** `WovenValueProcessor` — carrier assignment + effect-operation
    resolution for the Suspend-riding base carrier (+ the conformance-check relaxation that unblocks it);
    see §0/§6.
  - **§6 codegen redirect (landed).** The post-mono codegen chain reads `WovenValue` in place of
    `MonomorphicValue`: `naturalArity` ported onto `WovenValue`, and `UsedNamesProcessor` /
    `MonomorphicUncurryingProcessor` / the jvm `ExpressionCodeGenerator` natural-arity read swapped to
    `WovenValue.Key`. Behaviour-neutral off the flag (`WovenValue` is the identity image), verified
    byte-identical across `lang.test` / `jvm.test` / the example mains; see §0/§6.
  - **3a (remaining).** ambient `Suspend`-riding effects to a *running* program: `flatMap`/`pure`
    insertion, precise woven node types, the base-carrier `Configuration` key (`JvmPlugin` →
    `eliot.jvm.IO`), and the entry-point rework (run the woven `IO[Unit]` main). HelloWorld/Console
    examples green under the flag.
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

1. Exact metadata shape on the fact chain (row entries + positions) — RESOLVED by Phase 1: **one**
   generic field `EffectRow[C]` (`returnEffects` + `parameterEffects`, entries in the phase's
   ability-constraint representation), converted at each hop like `paramConstraints`. It is
   **excluded from `signatureEquality`** while both paths coexist — the desugared carrier signature
   still carries the character-exact merge check, so the raw row would be redundant there. When the
   carrier desugar is removed at Phase 4 the row *becomes* the merge-checked signature surface, so
   `signatureEquality` must then be taught to compare it character-exact (open item for the flip).
2. Whether `reify` needs surface syntax for users (the design says no — declared-type-directed
   insertion covers every current use; an explicit form could be added later for clarity).
3. Parameter-row reification base — RESOLVED by the §4 rule: open rows never capture, so a
   captured computation's base is always spelled by its pinned tail (`Id` or a generic `G`);
   there is nothing to default.
4. Evaluation order: keep resolved-argument order or move to source order (v1 keeps; §6).
