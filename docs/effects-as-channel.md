# Effects as a Channel, v2: Uniform Carriers (Id-Uniform) + a Verification Channel

Status: **FOUNDATION RESOLVED (2026-07-23) — Variant A, carrier-everywhere / Id-uniform — with a
reconstructed migration plan (U1–U5, §10). U1 COMPLETE (2026-07-23): the Id-normalization stage is
on by default and leaves **no `Id` residue** — U1a body rewrites + jvm newtype representation, U1b
`Id[X] ⤳ X` type/key erasure, and first-class-combinator eta-expansion (the last normalizer step:
a bare `Id` combinator reference ⤳ the identity/apply lambda). §6, §10. U2 FOUNDATION SPIKE LANDED
(2026-07-23): a self-contained, not-wired-in prototype (`lang/test/.../monomorphize/spike/`) with the
four historical failure cases + the flagship effectful/mixed conditional green under one guard-free
rule set; it produced two sharpenings now in §3 (`Id` is the lattice bottom *everywhere*; the ladder
*classifies by the expected slot* rather than trying pass-through first — the surviving recognition is
a positional effect-carrier tag on the expected binder), pinned the compile-time boundary (§8), and
confirmed Id-free rendering + constant-factor perf. Next: U3 (the uniform checker behind
`--effect-channel`).** The §13 fork raised during the Phase-3
effectful-conditional slice is decided: the erase-then-reconstruct foundation (v1 of this design,
§1–§6 of the previous revision) is **superseded**, and the committed foundation is **uniform
carriers**: every runtime term's checked type is carrier-headed, `Id` is the pure carrier, and a
mandatory **Id-normalization stage** erases the pure overhead before codegen. The **channel half of
v1 survives unchanged**: effect rows remain the user surface and the input to post-mono
**accounting** (`derived ⊆ declared`, friendly effect-vocabulary diagnostics) — effects stay
"annotation-only" at both ends (the surface and the generated code); the carrier is the compiler's
internal, uniform representation in between.

The **default** compiler path (carrier desugar + `EffectLifter` + `EffectResidualChecker`) is still
the live path and drives compilation unchanged until the U4 flip. The `--effect-channel` flag's
landed effect-blind slices (v1 Phase 3) are **scheduled for deletion at U3 start** (§0, §7); the
flag is retained as the gate under which the uniform checker grows.

## 0. Where we are (handover)

**Decision (2026-07-23):** the §13 fork is resolved to **Variant A**. Full decision record with the
evidence and the sharpenings it produced: §13. One-line version: recognition of carriers is
undecidable (the `EffectLifter` treadmill) *and* erasure of carriers is unrecoverable (effect-blind
mono specializes generics at payload types — `fold[Unit]` — destroying exactly the information
weaving needs, so a sound weaver converges toward a second checker). The only foundation avoiding
both is to make carrierhood **structural**: universal, uniform, `Id` for pure.

**Landed inventory and disposition** (details of each landed slice are in the git history and the
project memory; here only what each piece *becomes*):

| landed piece (commit era) | disposition |
|---|---|
| **Phase 1** — `EffectRow[C]` channel plumbing on the fact chain (`ast/fact/EffectRow.scala`, threaded `FunctionDefinition → … → OperatorResolvedValue`) | **KEEP.** The verification channel's input and the LSP's row source; needed by every variant. |
| **Phase 2** — shadow accounting inside `EffectResidualChecker` (byte-identical verdicts; the carrier-machinery-impl exception) | **KEEP until U4** (deleted with `EffectResidualChecker`, as always planned). Its methodology — shadow, byte-identical gates — is the U3 acceptance harness; its ability-method handling is the template for re-pointing accounting (§5). |
| **§5 accounting** — `monomorphize/channel/EffectAccountingProcessor` + `EffectAccounting` fact (`derived ⊆ declared`, friendly diagnostics) | **KEEP.** Still the post-mono verifier. One U3 slice re-points its derivation: under uniform checking effect ops arrive *resolved* (impl references), not abstract, so derivation reads ability-of-impl the way the Phase-2 shadow already does. |
| **Phase 3 foundation** — effect-blind desugar (`desugarChannel`: strip open rows, carrier-erase user effect-ability methods), `AbilityResolver` abstain, ability↔impl conformance relaxation | **SUPERSEDED — delete at U3 start.** These implement erasure, the rejected foundation. |
| **§6 weaver monadification** — `WovenValueProcessor.weave` (bind/`pure` insertion, `sequenceSpine`, `weaveBlock`, `peelAndWeave`, `pureWrap`, the `isLazyConditionalHead` `fold`/`if` FQN hardcode) | **DELETED (U3-0a, 2026-07-23).** Under uniform checking mono output is already monadic and resolved; there is nothing to weave. The whole weaver branch + both stopgaps (FQN hardcode, lambda-peeling) are gone; `WovenValueProcessor` is now just the Id-normalization stage. Reachable only via `baseCarrier = Some`, so the deletion was inert for the default path. |
| **§6 codegen redirect** — `WovenValue` fact + `used`/`uncurry`/jvm reading `WovenValue.Key` instead of `MonomorphicValue.Key` | **KEEP.** The post-mono seam between checking and codegen — exactly where the **Id-normalization stage** (§6) plugs in. The redirect investment is preserved. |
| **§6 entry-point rework** — jvm `runMain[A](io: IO[A]): A` in `IO.els`; synthetic main as bare ref under the flag; weaver-built run boundary; `LangPlugin.baseCarrierKey` / `entryPointKey` config | **SPLIT — weaver-boundary + config DELETED (U3-0a, 2026-07-23).** `runMain` itself: **KEPT** (ordinary Eliot, useful on both paths). The weaver-built boundary (`weaveEntry`/`runBoundary`), both config keys + their `JvmPlugin.withBaseCarrier`/`baseCarrierFQN` setters, and `SyntheticMainSourceProcessor`'s bare-ref branch are gone; the synthetic main is the sole `carrierMainSource` (`apply(block(main), unit)`) until U3b spells `runMain(<user main>)`. |
| `--effect-channel` flag + `LangProcessors(effectChannel=…)` threading | **KEEP the gate, replace its meaning.** At U3 start the effect-blind behaviors behind it are deleted (flag briefly inert); the uniform checker then grows under the same flag. |
| Tests: `EffectChannelDesugarTest`, `WovenValueTest` monadification cases | Deleted with their subjects at U3 start. `EffectAccountingTest`: kept, re-pointed with the derivation. |

Until U3 lands, the default path remains byte-identical to today; nothing user-visible changes
before the U4 flip.

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

**The v1 experiment added the problem's second half.** v1's answer was to erase carriers from
checking entirely and reconstruct them post-mono (the weaver). The Phase-3 effectful-conditional
slice proved that unrecoverable: effect-blind mono specializes every generic instantiation at
*payload* types (`fold[Unit]`, not `fold[IO[Unit]]`), so the bind-vs-pass-through decision — does
this slot consume the payload or carry the action? — has no sound name-agnostic signal left. The
two attempted reconstructions (callee-FQN hardcode; signature-genericness parsing) were both
re-derivations of information mono had and dropped, and a weaver that forwards enough checker
knowledge to decide (per-node rows *plus* per-slot genericness off evaluated Pi types, then
re-instantiation of mono keys at carrier types) is a second checker — the single-evaluator
anti-pattern in new clothes.

So the full problem statement: **recognition is undecidable, and erasure is unrecoverable.** The
carrier must be neither guessed nor removed — it must be *structural*.

## 2. The resolution: carrierhood becomes structural, not recognitional

The committed foundation makes the carrier **universal and uniform**: every runtime term's checked
type is carrier-headed, with the carrier *outermost by construction* — `Id[String]` for a pure
string, `IO[String]` for `readLine`, `Id[List[String]]` for a pure list (carrier `Id`, data
`List[String]` inside). Carrierhood stops being a semantic property of an arbitrary type (the
undecidable question) and becomes a *positional* property of the judgment: the outermost head of a
term's type **is** its carrier, because elaboration put it there. The elaboration maintains the
invariant; nothing ever needs to recognize anything.

Every guard in `EffectLifter` exists to answer the recognition question, and dies with it:

- `?F[List[String]] ~ List[A]` (the compound-state / eliot.file class) cannot arise: an actual is
  always a known (carrier, payload) split, so payload unifies with payload and the carrier meta can
  never be stolen by a container.
- The degenerate `?F := const String` pure-wrap hazard cannot arise: there is no uncarriered
  `String` actual anymore.
- `tryIdDefault`'s ordering fragility (the `catch`-handler and `if(c, None) else Some(x)` classes)
  dies because carrier variables are solved by **join**, not first-contact unification (§3) — no
  premature commitment exists to mis-order.

**What survives of "separation."** v1's slogan was "types ignore effects." The half of it that was
right — and is **kept, already built, and unchanged by this decision** — is the *channel*: rows are
recorded as structured signature metadata (Phase 1), verified post-mono by syntactic accounting
with effect-vocabulary diagnostics (§5), and rendered by the LSP. Verification never reads carrier
residue. The half that was wrong is erasure *in checking*: "annotation-only" is a statement about
the **user surface** (rows are all you write) and the **generated code** (no effect machinery
survives for pure code — §6), not about the checker's internals. In between, the carrier is the
compiler's internal representation, applied *uniformly* — which is precisely what makes every
construct (calls, chaining, conditionals, blocks) behave identically: one rule, no boundary
between "carried" and "uncarried" code, because there is no uncarried code.

**Cornerstone fidelity.** This is *more* types-are-values-faithful than v1, not less: carriers are
ordinary type constructors, `Id` is ordinary `data`, and effect flow through generics is ordinary
instantiation (`fold`'s `A := IO[Unit]`) — no side channel doing type-like work behind the type
system's back. The rows remain checker-adjacent bookkeeping (like `paramConstraints`), consumed by
a verifier, never flowing back into types.

## 3. The user model (unchanged) and the elaboration that realizes it

The three-sentence user model of v1 is unchanged — it is the goal, and uniform carriers are how it
is actually achievable:

1. **Types ignore effects** — in everything the user sees and writes. `{Console} List[T]`
   chains/folds exactly as `List[T]` (`something.foldLeft(…)` on an effectful `something` just
   works — the uniform ladder binds it; today's `val` workaround dies). Diagnostics speak payload
   types and effect rows; `Id` and carriers are never rendered.
2. **Effects run where they are written** — an effectful expression performs its effects at its
   position, joining the definition's ambient row; the compiler checks performed ⊆ declared with
   effect-vocabulary diagnostics. (One precise carve-out, below: a *generic* slot receives the
   suspended action instead — which is exactly what makes conditionals work and stay
   user-extensible.)
3. **Pinned means captured.** A position whose *declared* type is a pinned row (`{… | base}`)
   captures the computation as a value. Open rows never capture. Under uniform carriers this
   needs **no special rule at all**: a pinned/carrier-spelled slot captures via ordinary
   whole-type unification (pass-through, arm 1 below); `runMain(io: IO[A])` and every stdlib
   discharger work by the same arm.

**The elaboration invariant.** Every runtime term's checked type is carrier-headed; `Id` is the
carrier of pure computation. Effectful signatures keep today's carrier desugar (a shared ambient
carrier binder per signature — unchanged); pure terms and signatures are brought into carrier-headed
form by the elaboration (exact representation: U2 spike, §10 — the recommendation is the carrier as
an ordinary outermost type application in `SemValue`, reusing the one unifier and the one
evaluator; a parallel (carrier, payload) judgment pair would need its own carrier-flow rules — row
polymorphism reinvented — and would make pinning/reify special again).

**The one ladder.** At every application slot (expected slot type `S` from the callee's evaluated
signature; actual argument elaborated to `C[T]`):

1. **Pass-through**: unify `C[T]` with `S` whole. Succeeds when `S` is a generic/flex position
   (`fold`'s arm `A := IO[Unit]` — the slot receives the *suspended action*), or spells a carrier
   form (a pinned stack, `IO[A]`, a callee's carrier binder `?F[…]`). Carrier positions
   participate by **join** (below), never first-contact binding.
2. **Bind**: otherwise unify the payload `T` with `S` and sequence the computation at this
   position (`flatMap`; for `C = Id` the sequencing erases to nothing at §6).
3. Otherwise: type mismatch, in payload vocabulary.

This is today's unify-first ladder with the guards deleted, and it is **spec, not artifact**:

- **Concrete-payload slot ⇒ the effect runs at the call site** (bound, payload passed) —
  `printLine(readLine)`.
- **Generic slot ⇒ the action is passed, suspended**, and runs wherever the consumer's result is
  sequenced into an ambient — `fold(c, printLine("a"), printLine("b"))` carries each arm as a
  value and the eliminator selects one. This replaces v1 §4's overclaim ("an effectful argument …
  runs at the call site" — true only for concrete slots). It is also the
  **extensible-conditionals mechanism**: any user- or platform-defined conditional/combinator gets
  lazy-in-effects arms through ordinary parametric instantiation, with zero compiler knowledge of
  its name — the property whose absence killed the v1 weaver's `fold`/`if` hardcode. A generic
  consumer that *places* an action twice runs it twice (same as today; accounting is
  declaration-level and conservative).
- `val x = <effectful>` sequences (the block-desugared applied lambda binds) — unchanged.

**U2-spike sharpening — the ladder classifies by the *expected slot*, it does not "try pass-through
first."** The spike (§10, `lang/test/.../monomorphize/spike/`) found that a literal "whole-unify
first, fall to bind on failure" ladder is **unsound**: `list.map(f)` on an effectful `list :
?G[List[Int]]` meets `map`'s Functor slot `xs: F[A]`, and whole-unifying `?G[List[Int]] ~ F[A]`
*succeeds spuriously* — it solves `F := ?G, A := List[Int]` (the effect stranded as the container,
the element type wrong), the same premature solve the theft cases show. The fix is that the ladder
reads the **expected slot's elaborated shape** and picks exactly one arm, with no speculative
first-attempt:

- **bare flex generic `?A`** (`fold`'s arm) ⇒ pass-through-whole (the suspended action is `?A`'s value);
- **effect-carrier form** — headed by an *effect-carrier-tagged* binder `?G[…]` (the ambient / a
  callee's `F ~ Effect` binder) **or** a known carrier constructor / pinned stack (`runMain`'s
  `IO[A]`, a discharger's `{Throw[E] | G} A`) ⇒ pass-**join** (carrier joins, payload unifies, a pure
  actual records a deferred `pure`);
- **data / Functor / concrete `H[…]`** (`printLine`'s `String`, `map`'s `F[A]`, `List[A]`) ⇒ bind.

The load-bearing distinction is effect-carrier form vs Functor/data form when both are structurally
`Head[arg]`: `if`'s `?G[A]` and `map`'s `F[A]` are indistinguishable by shape and are told apart
*only* by the **effect-carrier tag** on the head binder (ability-constrained to the effect machinery
/ the value's ambient — today's `CarrierRole.effectCarrier` / `ambientCarriers`, but read on the
**expected** side). This is the precise residue of "recognition": what **dies** is shape-based
carrier detection of the *actual* (`EffectLifter.effectCarrierSplit`'s guards, `mustLift`,
equal-arity, `underApplied`); what **survives** is a positional *tag on the expected binder*, set at
elaboration, never a shape guess. The v1/§3-draft phrasing "unify `C[T]` with `S` whole … otherwise
bind" is thus refined to "classify `S`, then apply the one arm."

**Carrier solving is join-based; `Id` is the lattice bottom.** This is the load-bearing correction
that dissolves the premature-commitment bug class rather than reproducing it. A carrier metavariable
(a callee's carrier binder, the value's ambient) is never solved by whichever term touches it first:

- `Id` **never solves a carrier meta** — a pure term meeting a flex carrier slot records "at least
  `Id`" (i.e. nothing) and stays liftable;
- a meta touched by exactly one non-`Id` carrier solves to it;
- two *different* non-`Id` carriers meeting one meta are an ordinary type mismatch (legitimately —
  one signature has one ambient; inner discharge stacks are separate carriers by construction, as
  today);
- a meta untouched by any non-`Id` carrier at the value's boundary solves to `Id`.

So `if(c, "a") else readLine` works order-independently: the pure arm no longer commits the slot to
`Id` before the effectful sibling contributes `IO` (the historical `if(c, None) else Some(x)` and
`catch`-handler bugs are exactly this mis-ordering). `tryIdDefault` is thereby **promoted from a
heuristic ladder arm into the solving rule itself**. **Lift materialization is deferred and
decision-free**: once a meta solves, each recorded `Id`-side term gets its mechanical lift
(`pure` at the solved carrier — or nothing, if the meta solved to `Id`) inserted at post-drain;
this replaces the `Checker`'s Phase A/B flex-slot *decision* deferral with the deferral of a
no-decision insertion.

**U2-spike sharpening — `Id` is the lattice bottom *everywhere*, never a concrete carrier value.**
The spike's own bring-up reproduced the premature-commitment bug *inside the join solver* the moment
a pure `Id[String]` actual split to a concrete carrier `CCon("Id")` instead of bottom: it was then
treated as a real contribution and *committed* the ambient meta to `Id`, so the effectful sibling
conflicted — exactly the historical failure, rebuilt. The invariant that dissolves it: the split of
any `Id`-headed judgment yields the bottom carrier, and the carrier of a pure term is bottom, so a
pure arm **contributes nothing** to the join. Consequences, now load-bearing: (1) a carrier meta
"solved to `Id`" is indistinguishable from unsolved/bottom (join has no way to *commit* `Id`, which
is the point); (2) the deferred pure-lift is recorded **only when the actual's carrier is bottom**
(a pure arm), keyed on the *result* carrier — an already-effectful arm records nothing; (3)
materialization is decision-free: `pure`/`flatMap` at the joined carrier, or *erased* if it defaulted
to `Id`. All four historical cases plus the flagship mixed conditional (`if(c, readLine) else
"default"` — the pure arm lifts to `pure@Effect[IO]` order-independently) are green in the spike
under this one rule set, with **none** of the `EffectLifter` guards.

**Scope of the invariant** (the sharpest constraint, held from v1 §13.5): the carrier attaches to
**runtime term judgments** — never to the type language itself, never to type-level/compile-time
evaluation. The NbE evaluator stays carrier-free; signature evaluation is untouched; the
compile-time track's `Either` discharge is unchanged (§8). `data` fields stay payload-typed (a
stored computation is spelled pinned, as today).

## 4. The channel: rows, positions, and the reify boundary

**Desugar — corrected from v1.** `EffectSugarDesugarer` **keeps minting the carrier desugar** for
open rows (v1's "strip to payload" is the superseded foundation). What Phase 1 added stays: the
structured **declared row** (`EffectRow[C]` — entries + row positions) is recorded from the open
rows and forwarded on the existing fact chain (a field, per the lean-fact-flow rule), the channel
metadata the checker never reads and the verifier/LSP consume. Pinned rows desugar to canonical
carrier stacks exactly as today.

**Machinery entries.** `Effect` in a row (`action: A => {Effect} Unit`) remains the
ambient-transparent marker; `Suspend` remains the platform-I/O base every fine I/O effect rides.
Unchanged surface meaning.

**The reify boundary.** Pinned types appear only in *declared* signatures and fields, never
inferred — the invariant that keeps capture syntax-directed. Under uniform carriers the mechanism
is ordinary: a direct-style effectful expression meeting a pinned-row-typed position captures via
ladder arm 1 (whole-type pass-through into the declared stack type); whether the capture is *legal*
(the expression's row ⊆ pinned entries ∪ base) is the channel verifier's job (§5). Dischargers
need no recognition mechanism: a captured effect lands on the inner stack and is simply absent
from the caller's derived row — consumption is structural absence, as today.

**Open rows on by-value parameters stay rejected.** `def getOr(x: {Abort} String, d: String)`
would claim the callee receives effects that in fact already ran at the call site (concrete-payload
slot ⇒ bind). The fix stays in the message: pin the tail (`x: {Abort | G} String`) for capture, or
drop the row. Rows on function-typed parameter positions (`action: A => {Effect} Unit`) are
unaffected.

## 5. Row accounting and verification (per mono key)

*Status: **built and kept** — `monomorphize/channel/EffectAccountingProcessor` + the
`EffectAccounting` fact, the real `derived ⊆ declared` verifier (v1 Phase-3 §5 slice), validated
byte-identical against the carrier verdicts by the Phase-2 shadow.*

The post-mono verifier computes each mono'd value's **derived row** bottom-up from the checked
body — an effect-operation reference contributes its owning ability (machinery excluded), an
ordinary callee contributes its declared row (`OperatorResolvedValue.effectRow`), transparent
`Effect`-marked positions expand at the concrete arguments, a reify point subtracts — and checks:

1. **derived ⊆ declared** — "performs the effect 'X' but does not declare it", uniformly for every
   effect (including the `State`/`Throw`/`Abort` leaks that today fail cryptically inside
   `AbilityResolver`);
2. **reify legality** — a captured expression's row fits the pinned entries;
3. **pure-position fail-safes** — an effectful expression where nothing can absorb or capture it is
   an error, never silent.

`Inf` is an ordinary entry propagating through the same union — the totality story is unchanged.
The accounting fact is also the LSP's hover source for rows. The exactness argument of v1 §5 holds
verbatim (computed per concrete instantiation of the whole program; syntactically complete inputs;
declaration-level granularity by intent).

**U3 adjustments** (scheduled, §10): (a) **re-point the derivation** — under uniform checking
effect operations arrive in the mono body *resolved* to concrete instance methods, not as abstract
ability refs, so the derivation must recover the ability from an impl reference; the Phase-2
shadow's ability-method handling (which did exactly this against the default path) is the
template. (b) **Diagnostics ordering** — for the friendly message to actually be what the user
sees, the accounting verdict must win over (or preempt) the cryptic carrier-instance resolution
failure for the same leak; the carrier-machinery-impl exception (Phase-2 finding: those impls
declare effects via carrier constraints, not rows) and the synthetic-entry exemption (`main::main`
references the effectful user main at the run boundary) land here too.

## 6. The Id-normalization stage (replaces the v1 weaver)

Under uniform checking there is nothing to weave: mono output is already monadic (binds inserted by
the elaboration) and resolved (ability resolution finds the inserted `Effect` references, as it does
on today's default path). The one remaining post-mono stage is **Id-normalization** — the pass that
makes pervasive `Id` acceptable by erasing it totally, so pure code recovers its efficient shape
and **no effect machinery ships for pure code** (the MCU requirement).

**The rewrites.** Over ground post-mono terms, all local, confluent, and terminating (each strictly
decreases the Id-node count):

- `runId(e)` ⤳ `e`; `Id(e)` (the constructor) ⤳ `e`;
- `pure@Effect[Id](e)` ⤳ `e`; `flatMap@Effect[Id](f, m)` ⤳ `f(m)`; `map@Effect[Id](f, m)` ⤳
  `f(m)`;
- a first-class reference to an `Effect[Id]` combinator (passed as a function value) ⤳ the
  identity function;
- **type positions**: `Id[X]` ⤳ `X` everywhere — in node types, signatures, and **mono keys /
  type arguments** (`fold[Id[String]]` ⤳ `fold[String]`), merging Id-instantiations with their
  payload instantiations (sound: after body rewrites `Id[X]` and `X` are representationally
  identical);
- pinned stacks over `Id` erase their base layer: post-mono the stack machinery's calls into
  `Effect[Id]` are concrete and reduce, so `runThrow`'s `Id[Either[E, A]]` becomes
  `Either[E, A]`; eliot-test's `TestCase` bodies keep their thunk minus the base.

**Recognizing by FQN is sanctioned here**, unlike the v1 weaver's `fold`/`if` hardcode: `Id`,
`runId`, and `Effect[Id]`'s methods are **compiler-owned machinery** the checker itself inserts by
fixed FQN (`WellKnownTypes.idFQN`/`runIdFQN`) — not user vocabulary — the ordinary well-known-types
practice. That is the precise line between the two: hardcoding user-extensible names is unsound;
hardcoding compiler-owned insertions is how every such pass works.

**Load-bearing, with a hard fail-safe.** Pervasive `Id` is only acceptable because it reliably and
*provably* erases, so this stage is a mandatory compilation stage, not an optimization
(per the gaps-must-be-fail-safe rule): a post-pass **assertion that no `Id` FQN survives** in any
emitted type, key, or reference — a warning during U1 bring-up, a hard build error from U4.
Belt-and-braces: give `Id` **newtype representation** in codegen (`Id[A] ≡ A`; constructor and
accessor emit nothing), so any hypothetically missed residue is a no-op rather than an allocation.

**Immediate value on the default path.** Today's default path *already* inserts `runId`/`Id`
(`tryIdDefault`, discharge-to-pure) and ships `Id` to bytecode as a real data type with real
allocations. The stage therefore lands **first** (U1), on by default, verified against the current
path — independent value now, proven machinery before the checker refactor leans on it.

**Home**: the `WovenValue` seam — the codegen redirect (v1 §6, landed: `used`/`uncurry`/jvm read
`WovenValue`) is exactly the slot between checking and codegen this stage occupies. The processor
sheds its monadification and becomes the normalizer (rename at U4).

**Landed (U1a, 2026-07-23).** The rewrites live in `monomorphize/channel/IdNormalizer.scala`, invoked
from `WovenValueProcessor`'s default branch, on by default; the newtype half is `GroundValue.carrierFQN`
(`Id[X]` erases to `X`'s carrier). **One subtlety the bring-up surfaced and §6's rewrite list did not
call out:** the `runId` *accessor's own body* is the Church-encoded `PatternMatch.handleCases` apparatus,
not a `getfield`, so it must itself be rewritten to `obj -> obj` (`IdNormalizer.normalizeValue`).
Otherwise `used` keeps the whole `Id` pattern-match apparatus alive and a first-class `runId` reference
(a dot-chain `x.runId`) runs it over an `Id` wrapper the newtype no longer allocates — a crash. With the
accessor identity, `used` sees no `handleCases`, and the `Id` data class / `handleCases` / selector
lambdas / `PatternMatch` singleton are never generated.

**U1b landed (2026-07-23).** `IdNormalizer.eraseIdTypes`/`eraseIdInBody` erase every `Id`-headed type
to its payload in signatures, node types, and reference type arguments; erasing the last of these
shifts the callee's demanded mono key, so an `Id`-instantiation merges with its payload instantiation
(`fold[Id[String]]` ≡ `fold[String]`). The WovenValue's *own* key is left as demanded (the
`TransformationProcessor` requires produced-key = demanded-key, and the demand is already erased). A
*bare* `Id` (the higher-kinded `G` of `AbortCarrier[Id, A]`) is left — it has no payload and survives
to deeper stack lowering. The residue fail-safe now also flags a residual `Id[X]` *type*.

**Eta-expansion landed (2026-07-23) — U1 complete.** The last normalizer step handles a **first-class**
`Id` combinator reference — the combinator passed as a function value, e.g. `runId` reached through a
dot-chain `x.runId` (which lowers to `_dot_(x, runId)`) — which the applied-form rewrites do not reach.
`IdNormalizer.etaExpand` rewrites it to the equivalent lambda: `runId`/`Id`/`pure@Effect[Id]` (arity 1)
⤳ `x -> x`; `flatMap`/`map@Effect[Id]` (arity 2) ⤳ `f -> m -> f(m)`, built from the reference's own
function type. A reference reached as a *child* node carries its own type; a bare reference standing as
the *whole body* (`def r = runId`) is covered by threading the value's signature as the top node's type
(`normalizeValue(vfqn, signature, body)`). With this, **no `Id` machinery survives normalization at all**
— the residue fail-safe is now silent across the suites, examples, and eliot-test (`eliot.lang.Id` ships
no `runId`/`pure`/`flatMap`/`map` method), which is what lets the U4 assertion become a hard error.

**The MCU story.** With `Id` erased, pure code compiles to plain calls. For *effectful* MCU code,
the carrier is compile-time bookkeeping the backend may lower away: post-mono, every carrier
value's construction and run site is statically known, so straight-line effect sequences erase
wherever the platform's carrier is representationally identity, and suspended conditional arms
defunctionalize into branches — a standard whole-program lowering (`fold(c, IO(a), IO(b))` run
directly is statically `if c then a() else b()`). Control-effect stacks keep real representation
or lower to CPS/state — a per-backend choice. Nothing monadic needs to survive to a
microcontroller; recorded here as the design intent for the MCU backend (U5 follow-up).

## 7. What is deleted, what stays

**Deleted as they become separable, across U3** (the superseded v1 Phase-3 erasure path, currently
flag-gated and dormant — *not* a single atomic "U3 start" delete, see §10 U3-0b):
**Already gone (U3-0a):** `WovenValueProcessor`'s entire monadification
(`weave`/`weaveMonadic`/`sequenceSpine`/`weaveBlock`/`peelAndWeave`/`pureWrap`/`finalApply`/
`Combinators`, the `isLazyConditionalHead` FQN hardcode, the lambda-peeling), the weaver-built run
boundary + `LangPlugin.baseCarrierKey`/`entryPointKey` and their `JvmPlugin.configure`
contributions, `SyntheticMainSourceProcessor`'s bare-ref flag branch (replaced by spelling
`runMain(<user main>)`), and `WovenValueTest`'s monadification cases.
**Deferred into U3a/U3c** (coupled to the kept `EffectAccounting` verifier, §10 U3-0b):
`EffectSugarDesugarer.desugarChannel`'s open-row stripping + user-effect-ability carrier-erasure,
`AbilityResolver`'s effect-ability abstain, the ability↔impl conformance relaxation in
`AbilityImplementationCheckProcessor`, and `EffectChannelDesugarTest` — these come out when the
uniform checker monomorphizes effect-polymorphic values at a bound carrier and the accounting
derivation + `EffectAccountingTest` are re-pointed.

**Deleted at the U4 flip** (the old default-path machinery the uniform checker replaces):
`EffectLifter`'s recognition arms — `mustLiftBeforeUnify`/`mustPureWrapBeforeUnify`, the
equal-arity arm with its three guards, `underApplied`/`isFlexMeta`, `effectCarrierSplit`'s
ambient/role recognition — and `tryIdDefault` *as an arm* (promoted into the join solver, §3);
`CheckState.ambientCarriers` + `recordAmbientCarriers`; the `Checker`'s Phase A/B flex-slot
deferral (replaced by decision-free deferred lift materialization); `EffectResidualChecker`
including the Phase-2 shadow; `CarrierKindChecker`'s carrier-specific duties; the synthetic main's
`apply(block(main), unit)` spelling (→ `runMain`). The bind/`pure` *mechanics*
(`wrapBinds`/`bindWrap`, the `$eff$N` splice convention) survive reshaped as the uniform ladder's
insertion step.

**Stays**: the surface (open + pinned rows, ambient effects, dischargers unchanged); the channel
(`EffectRow` plumbing, `EffectAccountingProcessor`, LSP row rendering); pinned rows as the declared
capture boundary; the platform carrier `data` types and their `Effect`/`Suspend` instances
(`eliot.carrier`); **`Id`, promoted** — the universal pure carrier during checking, erased by §6
(its compile-time overlay remains for §8); `runMain`; the `WovenValue` seam as the normalizer's
home; the `termination` story (`Inf` as a row entry); `namedValues`; eliot-test unchanged.

**Stdlib deltas stay additive** as in v1: parameter rows make the effectful-handler `catch`
(`onError: E => {Effect} A`) expressible, turning the eliot.file `catch`-handler failure into an
ordinary vocabulary choice.

## 8. The compile-time residue

Unchanged from v1, with the scoping constraint now explicit. The checker *consumes* effect
discharge on the compiler platform: effectful signatures (`{Throw[String]} Type` calculated
returns, guards) evaluate on the `Either[String, _]` carrier and are read back by
`CalculatedReturnResolver`. This stays as is — one fixed carrier, pure control effects only,
bounded. The uniform-carrier elaboration applies to **runtime term judgments in value bodies**
(both tracks use the one checker, so compile-track value bodies get the same uniform treatment);
what is *never* carrier-wrapped is the type language itself — signature evaluation, NbE forcing,
`VType`-level computation. The U2 spike pins this boundary precisely before the checker refactor
begins; entangling `Id` into type-level evaluation is the failure mode to guard against.

**U2-spike confirmation.** The spike encodes the boundary as three green assertions: a type-level
judgment (`Int[0,255] ~ Int[0,255]`) unifies by plain payload unification and **introduces no
carrier metavariable**; `split` **refuses** a type-of-types (`VType`) judgment — a type-level term is
not carrier-headed, so nothing can accidentally carry it; and the §8 `Either` discharge carrier is a
**data** constructor (`isCarrierCon` false), joined nowhere as a runtime carrier — the compile-track
`{Throw[String]}` discharge stays a type-level `Either` fold. The rule that keeps this sound is
mechanical: the carrier machinery (`split`, `joinCarrier`, the ladder) is invoked **only on runtime
term judgments**; the NbE/signature path never calls it. That is the invariant U3 must preserve as it
threads the elaboration through the (shared) checker.

## 9. Held invariants and interactions

- **The elaboration invariant**: every runtime term judgment is carrier-headed, carrier outermost,
  maintained by construction — never by recognition. No phase may reintroduce a "is this type a
  carrier?" query.
- **Carrier metas solve by join** (`Id` bottom, one non-`Id` winner, conflict = mismatch, unsolved
  = `Id`); first-contact unification of carrier positions is the premature-commitment bug class
  and must not be reintroduced.
- **No `Id` residue**: the §6 assertion is a permanent invariant from U4 on — `Id` exists between
  elaboration and normalization, nowhere downstream.
- **Pinned types are declared, never inferred** — capture stays syntax-directed.
- **Normalization/reordering reads carriers, not rows**: any future normalizer (reduce-and-reify)
  treats non-`Id`-carriered terms as observation-ordered. This information is now *in the types*;
  the v1 invariant "consult the channel" and the 2026-07-23 per-node-row forwarding decision are
  both **subsumed and reversed** — no per-node row annotation is forwarded through mono
  (`MonomorphicExpression` is untouched); the carrier is the per-node signal, the channel stays
  per-declaration.
- **Suspend-riding effects still cannot be pinned** (no canonical carrier); the designed
  `Suspended` platform-base extension (`docs/effect-row-tails.md` §Limits) remains the answer.
- **Types-are-values, restated at the flip**: effects are represented in types uniformly; `Id` is
  an ordinary value; rows are the user surface and the verifier's vocabulary, checker-adjacent
  metadata that never flows back into types. (Replaces v1's "open rows stop being values"
  amendment.)
- **LSP**: hover composes the payload type with declared/derived rows from the channel;
  `GroundValueRenderer` keeps stack→pinned-row rendering; `Id` and carriers are never rendered to
  users — error messages likewise (a U2 spike item, a U4 gate).

## 10. Migration phases (reconstructed)

The gated-flip playbook again (signature-unification precedent; v1's own Phases 1–3 ran it
successfully — the dead end was found behind a flag with the default path byte-identical
throughout). Phases are ordered to de-risk: the erasure stage first (independent value, proven
before anything leans on it), the foundation spike second, the checker refactor third.

- **U1 — Id-normalization stage, on by default.** Build §6 in the `WovenValue` seam and enable it
  for the *current* default path (which already ships `Id` to bytecode). Two sub-slices:
  **U1a** body rewrites + newtype representation (no key changes); **U1b** type/key erasure
  (`Id[X] ⤳ X` in `GroundValue`s and mono keys, merging Id-instantiations). Assertion lands as a
  warning. Gate: full suites + examples + eliot-test byte-behavioral; `javap` shows no `Id`
  allocation on the erased paths.
  - **U1a — LANDED (2026-07-23).** `IdNormalizer` (`monomorphize/channel/IdNormalizer.scala`), called
    from `WovenValueProcessor`'s default (non-flag) branch, applies the §6 body rewrites over every
    monomorphic body: `runId(e) ⤳ e`, `Id(e) ⤳ e`, `pure@Effect[Id](e) ⤳ e`,
    `flatMap@Effect[Id](f, m)`/`map@Effect[Id](f, m) ⤳ f(m)`. Recognition is by FQN
    (`WellKnownTypes.runIdFQN`/`idConstructorFQN`; the `Effect[Id]` methods by their `Id` module +
    `Effect` ability qualifier — sanctioned, compiler-owned machinery). **One extra rewrite proved
    load-bearing, not optional:** the `runId` *accessor's own body* is the data-accessor
    `PatternMatch.handleCases` apparatus, so `normalizeValue` rewrites it to `obj -> obj` — otherwise
    `used` keeps pulling in the whole `Id` pattern-match machinery (the data class, `handleCases`, the
    selector lambdas) and a **first-class** `runId` reference (from a dot-chain like
    `suite.runWriterToLog.runId`) runs that apparatus over an `Id` wrapper the newtype no longer
    allocates — a crash the eliot-test suite caught. **Newtype representation**: `GroundValue.carrierFQN`
    erases `Id[X]` to its payload's carrier, so any `Id`-typed node the rewrites leave in place is
    representationally its payload — no cast, no allocation (node *types* are otherwise left as `Id[X]`;
    key/type erasure is U1b). **Fail-safe**: `WovenValueProcessor.warnIdResidue` warns on any surviving
    `Id`-machinery *reference* (a warning in U1, a hard error from U4); first-class combinator references
    still warn (their normalized bodies + the newtype keep them safe no-ops) until U1/U4 eta-expands them.
    Verified: `lang.test`/`jvm.test` green, eliot-test 11/11 byte-identical, ~20 example mains run
    unchanged, `javap` shows no `Id$Id` class and no `new Id` anywhere. `IdNormalizerTest` covers the
    rewrites.
  - **U1b — LANDED (2026-07-23).** `IdNormalizer.eraseIdTypes`/`eraseIdInBody` erase every `Id`-headed
    type (`Id[X] ⤳ X`, recursively) in the value's **signature**, every body **node type** and
    function-literal parameter type, and every value **reference's type arguments**. Erasing a
    reference's type arguments is what *merges* an `Id`-instantiation with its payload instantiation:
    the erased args become the callee's demanded mono key, so `fold[Id[String]]` and `fold[String]`
    resolve to one demand and one generated method. The WovenValue's **own key** (`mv.typeArguments`)
    is deliberately *not* erased in the processor — the `TransformationProcessor` contract requires the
    produced fact's key to equal the demanded key, and that demand is already erased (it came from a
    referencing body whose reference args were erased); so key merging falls out of the demand shift
    rather than a key rewrite. A **bare** `Id` (unapplied, the higher-kinded `G` of
    `AbortCarrier[Id, A]`) is left untouched — it has no payload to collapse to, its carrier already
    erases to its own head, and it survives until deeper stack lowering (§6 "pinned stacks over `Id`
    erase their base layer", a later step). The residue fail-safe now also flags a residual `Id[X]`
    *type* (`hasResidualIdType`), not just a reference. Verified: `lang.test`/`jvm.test` green (no
    mangled-name breakage — the surviving `$Id$` names are bare-`Id` carrier markers, unchanged),
    eliot-test 11/11 byte-identical, IfDemo/examples report **zero** `Id`-type residue, only the
    first-class *value*-reference residue remains (eta-expansion to identity is the last normalizer
    step, deferred; backstopped by the newtype + identity accessor today).
- **U2 — foundation spike.** Decide and prototype, recording results here: (a) the
  representation — carrier as ordinary outermost `SemValue` application (recommended) vs a
  judgment pair; how the ladder reads the split, multi-layer stack splitting
  (`StateCarrier[S, G, A]` prefix+last as today), occurs-check/kind interaction with carrier
  metas; (b) the join solver + deferred lift materialization replacing Phase A/B; (c) the four
  historical failure cases (`?F[List[String]] ~ List[A]`, the `catch`-handler default, the
  `if(c, None) else Some(x)` default, the compound-state equal-arity) as executable regression
  cases demonstrating no guards needed; (d) the compile-time boundary (§8); (e) error rendering
  without `Id`/carriers; (f) the compile-perf constant. Exit: decisions written into §3/§8, spike
  harness green.
  - **LANDED (2026-07-23).** The spike is `lang/test/src/com/vanillasource/eliot/eliotc/monomorphize/`
    `spike/UniformCarrierSpike.scala` (a minimal faithful model — `TCon`/`TMeta`/`TType` ↔
    `VTopDef`/`VMeta`/`VType`; two-store solver making "a carrier meta can never be stolen by a
    container" *structural*) + `UniformCarrierSpikeTest.scala` (the regression harness, green; U3's
    acceptance cases). It is **not wired into the compiler** — the default path is byte-identical.
    Results, folded into §3/§8:
    - **(a) representation — CONFIRMED.** Carrier as an ordinary outermost application; `split` is
      positional and total on runtime judgments (multi-arg stack = prefix + last argument, as
      `effectCarrierSplit` already does). Occurs/kind: carrier metas and payload metas are kept apart
      (in the real unifier by `carrierRoles` membership; the spike by two maps) — payload
      occurs-check never touches a carrier meta, which is *why* the theft cases dissolve.
    - **(b) join solver — CONFIRMED, with two sharpenings now in §3.** (i) `Id` is the lattice bottom
      **everywhere** — the spike rebuilt the premature-commitment bug the instant a pure `Id[X]` split
      to a concrete carrier; the pure-lift is recorded only for a bottom-carriered actual and
      materialized decision-free. (ii) The ladder **classifies by the expected slot** (bare-generic /
      effect-carrier-form / data-Functor-form), it does *not* "try pass-through first" — a literal
      whole-unify-first ladder mis-solves `map`'s Functor slot. The one surviving recognition is the
      effect-carrier **tag on the expected binder**, not shape detection of the actual.
    - **(c) four cases — GREEN, no guards.** `?F[List[String]] ~ List[A]` (A := String, effect bound,
      no theft), the equal-arity compound-state (`?S := List[X]`, carrier preserved), the
      `catch`-handler default (join is order-independent), `if(c, None) else Some(x)` (element := Int,
      carrier defaults to Id) — plus the flagship effectful/mixed conditional with no `fold`/`if`
      hardcode, and the pre-uniform injectivity **theft** kept as an executable regression contrast.
    - **(d) compile-time boundary — PINNED (§8):** type-level unify introduces no carrier; `split`
      refuses a `VType` judgment; the `Either` discharge carrier is data, not a runtime carrier.
    - **(e) error rendering — CONFIRMED:** `render` strips the carrier and shows payload only
      (`Id[String]`, `IO[String]` → `String`); rows come from the channel; no `Id`/carrier/meta leaks.
    - **(f) perf — CONSTANT:** one payload-unify per slot (linear), join is O(1) per contribution with
      no fixpoint, materialization is one post-drain pass.
- **U3 — the uniform checker behind `--effect-channel`.** Delete the superseded v1 slices (§7 first
  list) **as they become separable** — *not* all at "U3 start": the weaver slice separated cleanly and
  is gone (U3-0a), but the `desugarChannel`/abstain/relaxation slice is **coupled to the kept
  `EffectAccounting` verifier and folds into U3a/U3c** (the finding below). Then grow in slices,
  default path byte-identical throughout: (a) uniform judgments + the ladder +
  join solver in `Checker`/`EffectLifter`-successor; (b) synthetic main spells
  `runMain(<user main>)` under the flag; (c) accounting derivation re-pointed + diagnostics
  ordering (§5); (d) the acceptance gate — full `lang.test`/`jvm.test`, all example mains,
  eliot-test, and the U2 regression cases green under the flag, **including effectful
  conditionals and user-defined conditionals** (the motivating test), byte-identical where
  behavior overlaps the default path; (e) the compiler track green (guards + calculated returns).
  Note U3 is a *regularization*, not new semantics: control effects, dischargers, pinned rows,
  and higher-order effects already work on the default path and must simply stay green under
  uniform judgments — v1's remaining 3b–3e construction slices have no successor here.
  - **U3-start deletion — split into two green sub-slices (the §7 first list is not a single
    atomic delete).** The `EffectAccountingProcessor` verifier is **kept** (§0), and its one
    test `EffectAccountingTest` drives the *whole* flag-on pipeline from source (`def main:
    {Console} Unit = printLine(…)`), relying on the effect-blind `desugarChannel` to leave
    `printLine` as an **abstract** ability ref for the derivation to read. Deleting `desugarChannel`
    therefore turns that ref **resolved** (an impl ref) and reddens the kept test — which is exactly
    what U3c's "re-point the derivation" fixes. So the deletion is ordered to keep every commit green:
    - **U3-0a — LANDED (2026-07-23).** Delete the pieces nothing accounting depends on — reachable
      only via `baseCarrier = Some`, which *only* `JvmPlugin.withBaseCarrier` set (under the flag),
      so it is provably inert for the default path and every kept test. Deleted: the entire
      `WovenValueProcessor` weaver branch (`weave`/`weaveEntry`/`sequenceSpine`/`weaveBlock`/
      `peelAndWeave`/`pureWrap`/`finalApply`/`flatMapApply`/`weaveMonadic`/… and `object Combinators`)
      — the processor is now **just the Id-normalization stage**; the entry-point rework (`weaveEntry`/
      `runBoundary`/`runMainFQN`); `LangPlugin.baseCarrierKey`/`entryPointKey` + their `LangProcessors`
      params + `JvmPlugin.withBaseCarrier`/`baseCarrierFQN`; `SyntheticMainSourceProcessor`'s
      `effectChannelMainSource` bare-ref branch (leaving `carrierMainSource`); and `WovenValueTest`
      (all cases exercised the weaver via `baseCarrier = Some`). The `--effect-channel` flag stays as
      the gate, threaded to the checker + `EffectAccountingProcessor` only. Gate met: `lang.test`/
      `jvm.test` green, HelloWorld builds+runs, eliot-test 11/11, and the kept flag-on suites
      (`EffectAccountingTest`, `EffectChannelDesugarTest`) still green.
    - **U3-0b — BLOCKED on U3a; the `desugarChannel` deletion moves out of "U3 start" (finding,
      2026-07-23).** A trial deletion (`desugarChannel` + its 4 helpers + the `rewrite` `stripOpen`
      arm; `AbilityResolver`'s abstain; `AbilityImplementationCheckProcessor`'s relaxation;
      `EffectChannelDesugarTest`) compiled cleanly but **reddened `EffectAccountingTest` with
      `"Cannot resolve type."` — not** the anticipated "derived row is empty". The root cause is
      deeper than a derivation re-point: `EffectAccountingTest` demands
      `MonomorphicValue.Key(main, Seq.empty)` for an **effect-polymorphic** `main : {Console} Unit`,
      and on the carrier path that value **cannot be monomorphized standalone** — its ambient carrier
      `F` is unbound (there is no synthetic-main use-site in the test, and the **lang test track has no
      runtime carrier at all**: `IO` is a jvm-platform type, and `Console` is `Suspend`-riding so it
      cannot even resolve at the compile-track `Id`). The effect-blind `desugarChannel` is precisely
      what let `main` monomorphize *carrier-free* (as pure `Unit` with abstract `Console` refs). All
      four deletion targets are coupled to `desugarChannel`, which is coupled to
      `EffectAccounting(Test)`, which needs a **carrier-bound monomorphization of an effect-polymorphic
      value** — i.e. U3a infrastructure. **So the §7 "delete `desugarChannel` at U3 start" ordering has
      a hidden dependency and is corrected:** this deletion folds into **U3a/U3c** (when the uniform
      checker monomorphizes effect-polymorphic values at a bound carrier, and the accounting derivation
      **and `EffectAccountingTest` itself** are re-pointed — the test likely relocating to the jvm
      module, the only track with a concrete runtime carrier). The trial was reverted; the tree is at
      U3-0a. U3-0a stands as the *only* cleanly-separable pre-U3a deletion (it was inert because gated
      on `baseCarrier = Some`). **NEXT is U3a itself** (the uniform checker under the flag), which
      subsumes this deletion.
- **U4 — flip and delete.** The flag becomes the default; the §7 flip-deletions land; the §6
  assertion becomes a hard error; the Cornerstone amendment (§9 restatement) and the doc/skill
  sweep (`eliot-code` global skill, `eliot-layers`, CLAUDE.md effect + monomorphize sections);
  LSP/diagnostic rendering verified `Id`-free. The old path is removed, not kept as a mode.
- **U5 — follow-ups unlocked.** Row-bearing diagnostics everywhere; the evaluation-order decision
  (resolved-argument order vs source order — v1 §6's recorded question, carried over);
  `Suspended` for first-class platform actions; the MCU lowering (§6: identity-carrier erasure +
  arm defunctionalization) when that backend activates; reduce-and-reify's carrier-based
  observation ordering (§9).

## 11. Risks

- **Checker-refactor invasiveness** (U3a) is the honest big one: the elaboration invariant
  threads through `Checker` (~1000 lines), the unifier (carrier joins beside payload
  unification), and the domain — mechanical in principle, wide in practice. The U2 spike and the
  four-case regression suite bound it; the flag keeps the default path safe.
- **Join-solver correctness**: the deferred lift materialization must be total (every recorded
  `Id`-side term gets its lift when the meta solves late). A missed insertion is a loud type/codegen
  error, not silence — but budget for the tail.
- **Erasure totality** (U1): the assertion catches residue; the risk is merely schedule.
- **Mono-key churn** (U1b): key erasure changes emitted jvm names where Id-instantiations merge —
  behavior-identical, but any test asserting mangled names needs the sweep.
- **Error-message regression**: uniform carriers must never leak `Id`/`F[…]` into user-facing
  text; rendering is a U2 item and a U4 gate, not an afterthought.
- **Two checkers during U3** cost maintenance; slices + the byte-identical gate keep the window
  short, as v1's Phases 1–3 demonstrated in practice.

## 12. Open questions

1. **Representation details** (U2a): the `SemValue` form is **resolved** (carrier as outermost
   application; §3, spike-confirmed) and occurs-check with carrier metas is **resolved** (carrier and
   payload metas are separate namespaces; the payload occurs-check never sees a carrier meta). One
   sub-item **carries into U3a**: whether pure *signatures* are brought into carrier-headed form by a
   desugar rewrite or by check-time wrapping — the spike models runtime *judgments* directly and does
   not commit the signature-side mechanism; U3a decides it against the real `EffectSugarDesugarer` /
   `Checker`.
2. **The join lattice at discharge sites** (U2b/U3): the spike confirms "two different non-`Id`
   carriers = mismatch" and the bottom/single-winner rules for the flat cases; **carries into U3**
   as designed — nested discharge (`StateCarrier[S, G]` under ambient `G`) needs no refinement *in
   principle* (the inner stack is a distinct carrier by construction, never joined with `G`), but the
   spike does not exercise a real multi-layer discharge stack, so U3's compiler-track gate is where it
   is proven on live code.
3. **Evaluation order** (carried over, unchanged): v1 keeps resolved-argument order; source order
   is a recorded U5 decision.
4. **`reify` surface syntax** (carried over, unchanged): declared-type-directed capture covers
   every current use; an explicit form remains a possible later addition.
5. ~~Metadata shape on the fact chain~~ — resolved (Phase 1, kept). ~~Parameter-row reification
   base~~ — resolved (§4 rule, kept). ~~Per-node effect rows through mono~~ — **reversed**, §9.

## 13. Decision record: the carrier-model fork, resolved (2026-07-23)

**Decision: Variant A — carrier-everywhere, Id-uniform — is the committed foundation.** Raised
during the Phase-3 effectful-conditional slice; evaluated and resolved 2026-07-23. This section is
the record; §§1–10 above are the reconstructed plan it produced.

**What forced it.** The weaver's one hard per-argument decision — bind (`printLine(readLine)` runs
`readLine`, hands the `String`) vs pass-through (`fold`'s arms stay `IO[Unit]`, one is selected) —
has no sound name-agnostic signal under erasure, because effect-blind mono had already specialized
the types away (`IO[Unit]` and `String` both collapse to payload; `fold` instantiates at `[Unit]`).
Both attempted reconstructions failed on principle: callee-FQN hardcoding (conditionals are
user/platform-extensible — no fixed name set is sound) and signature-genericness parsing (a
signature is a value-level computation; a static parse is fragile, and it re-derives what mono
dropped). A weaver that forwards enough to decide — per-node rows *plus* per-slot genericness off
the checker's evaluated Pi types, then re-instantiation of mono keys at carrier types — is a second
checker: the single-evaluator anti-pattern. The committed stopgaps (`isLazyConditionalHead`,
`peelAndWeave`) were that convergence made visible.

**Why A.** The evaluation sharpened the §13.2–13.4 draft in four ways, all now folded into the plan:

1. **The dissolution is representational**: today's bug class is *recognitional* ("is this type a
   carrier?" — semantically undecidable per the lifter's own doc); erasure's bug class is
   *reconstructive* (the information is destroyed). Uniform carriers make carrierhood *positional*
   — outermost by construction — so the question every `EffectLifter` guard answers stops existing
   (§2). All four historical failure cases were re-derived to confirm (§3, U2c makes them
   regression tests).
2. **Uniformity alone does not answer bind-vs-pass — the unify-first ladder does** (§3), and
   generic-slot pass-through is simultaneously the *extensible-conditionals mechanism*: laziness in
   effects = parametricity + reified actions, zero compiler knowledge of any conditional's name.
   This also corrected v1 §4's argument-strictness overclaim into the two-case spec rule.
3. **Naive whole-unify-first would have rebuilt the premature-`Id`-commitment bugs** at flex
   carrier slots (`if(c, "a") else readLine` — the pure arm must not commit the slot before the
   effectful sibling contributes). Hence the **join solver** with `Id` as lattice bottom and
   deferred, decision-free lift materialization (§3) — `tryIdDefault` promoted from heuristic arm
   to the solving rule, Phase A/B's decision-deferral retired. This is the key technical content
   the U2 spike must validate first.
4. **`Id`-erasure is provably total** (local, confluent, terminating rewrites over ground terms;
   compiler-owned FQNs, so rewrite-by-name is sanctioned — the precise line the `fold` hardcode was
   on the wrong side of), **but must be load-bearing**: a mandatory stage with a hard no-residue
   assertion and newtype belt-and-braces (§6). It pays for itself immediately on the default path,
   which already ships `Id` allocations — hence U1 first.

**Variant B** (effect-blind checking kept; carrier assigned post-mono from forwarded rows) is
rejected as the foundation: it keeps the cornerstone's original phrasing but cannot answer the
position question without forwarding checker knowledge and re-instantiating mono keys — the
second-checker convergence above. The **per-node effect-row forwarding decision** taken when the
fork opened is thereby **reversed**: no per-node row is added to `MonomorphicExpression`; under A
the carrier in the types *is* the per-node signal, and the channel stays per-declaration (§9).

**What A costs, eyes open**: carriers return to checking — but *universally and unconditionally*
(mechanical Kleisli lifting + join solving), not as the heuristic conditional lifting this redesign
set out to delete; "types ignore effects" is restated as a user-surface property realized by
uniformity rather than erasure (§2, §3). The compile-time track must stay out of the wrapping
(§8) — the sharpest constraint. The checker refactor is wide (§11). The v1 erasure slices
(effect-blind desugar, weaver monadification, both stopgaps) are deleted at U3 start; their
byte-identical/flag discipline and their negative result are what earned this decision, and the
durable v1 assets — the row channel, the accounting verifier, the `WovenValue` codegen seam,
`runMain`, the shadow methodology — carry over intact (§0).
