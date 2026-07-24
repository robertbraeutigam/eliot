# Effects as a Channel, v2: Uniform Carriers (Id-Uniform) + a Verification Channel

Status: **Variant A — carrier-everywhere / Id-uniform — is the committed foundation (§13, decided
2026-07-23).** The plan has two halves that can be handed over independently: a **verification
channel** (post-mono effect accounting) and a **uniform-carrier checker**.

**Verification channel — DONE (U4-b + the U4-c course, 2026-07-24).** `EffectAccountingProcessor` is
the **sole effect verifier**: for each monomorphized value it derives the effect row from the checked
body (a reference counts iff it rides the value's own ambient carrier — its carriers are the *callee's*
forwarded `MonomorphicValue.ambientCarriers`, compared by exact `GroundValue` equality) and requires
`derived ⊆ declared` for a value with an open effect row. It is **wired as a codegen precondition**
(`WovenValueProcessor` demands `EffectAccounting` via `getFactOrAbort`) and verifies
**unconditionally**. The pre-mono `EffectResidualChecker` is **deleted**; its one diagnostic accounting
cannot voice ("declared pure but performs an effect", a value whose mono fails) moved to the focused
`DeclaredPureChecker`. Design + interface in §4/§5; the derivation is fully live, no flag needed.

**Uniform-carrier checker — IN PROGRESS.** U1 (Id-normalization, on by default), U2 (spike), and the
U3a bridge are **landed**; the transitional `--uniform-carrier` gate covers every value return, the
argument→payload-slot case, the whole conditional surface, and the `CarrierSlot` arm — byte-identical
wherever the default path succeeds, plus three of the four historical non-overlap wins (coverage table
below). **Remaining work**: **U4-a** — complete uniform coverage (the `Generic` arm carrying the
ride-up-vs-bind check + reshaping the capture/mismatch fallbacks; the invasive ~1200-line `Checker`
flip), then **U4-d** — delete the default-path machinery (`EffectLifter` recognition arms, the `Checker`
Phase A/B deferral, `CheckState.ambientCarriers`, respell the synthetic main to `runMain`), then
**U4-e** — make the uniform path the default, remove `--uniform-carrier` + the vestigial
`--effect-channel`, land the effectful-`catch`-handler stdlib delta, and turn the §6 Id-residue
assertion into a hard error. See §10 and the pinned findings.

Per-slice history and commit trails live in the git log — this document keeps only the design, the
current state, and the path forward.

## 0. Current state

**Tree**: `master`; all gates green: `./mill lang.test` / `./mill jvm.test` (incl.
`UniformCarrierByteIdenticalTest` — the whole base + test programs compiled flag-off vs flag-on,
every generated class's bytes equal — and `UniformCarrierConditionalTest`, the non-overlap
compile-succeeds gate), HelloWorld builds+runs
(`./mill examples.run jvm exe-jar examples/src/ -m HelloWorld` then `java -jar
target/HelloWorld.jar`), eliot-test 11/11 (exact command in `eliot-test/.claude/CLAUDE.md`; args
are order-strict), and 32/32 compiling example mains byte-identical under the flag (the 3
remaining flag failures — `EffectsTwoDeps`/`EffectsTwoThrows`/`WherePrecondition` — fail
identically at baseline: pre-existing multi-layer-discharge / `where`-precondition gaps). The
default path is byte-identical to pre-U1. Because effect **accounting now verifies on every
compile** (U4-c-2, unconditional), whole-base accounting parity — no over-count reddens valid code —
is a standing gate covered by `lang.test`/`jvm.test` (every example integration test compiles with
accounting live); rejection is `EffectAccountingWiringTest` (undeclared `Console`, undeclared `Inf`)
and correct-derivation is `EffectAccountingDerivationTest`.

**Verifier**: `EffectAccountingProcessor` (the §5 post-mono verifier) is now the **sole effect
verifier** (U4-c-2). It is wired as a codegen precondition (`WovenValueProcessor` demands
`EffectAccounting.Key` via `getFactOrAbort`, U4-c-1) and verifies **unconditionally** (the
`--effect-channel` gate on verification is gone; the flag is vestigial until U4-e). The pre-mono
`EffectResidualChecker` is **deleted**; the one diagnostic it voiced that accounting cannot — "declared
pure but performs an effect", for a value whose mono *fails* — moved to the focused
`DeclaredPureChecker`, run per value mono from `TypeStackLoop.runPostDrainResolution`. The subset check
fires only for a value with an *open effect row* (a concrete-carrier `IO[Unit]` return is exempt); a
leak reddens through accounting with no flag.

**Flags**: `--uniform-carrier` — the transitional gate the uniform checker grows under (coverage
below); the uniform checker grows on default carrier-desugared input, compared byte-identical.
`--effect-channel` — **vestigial**: accounting now verifies unconditionally, so the flag gates
nothing (it once gated accounting's verification and, before U4-b, an erasure path — both gone).
Both flags are removed at U4-e.

**Component map**:

- `monomorphize/carrier/` — `Carrier` (the lattice `Bottom`=`Id` / `Con` / `Var` + the positional,
  total `split`), `CarrierJoin` (the join solver; **built, uncalled live** — first live use is the
  catch-handler join at the flip), `UniformLadder` (classify-by-expected-slot + decision-free
  `materialize`; plus `resolveGenericSlot` — the ride-aware Generic-arm resolver, **live** (U4-a(i),
  wired through the Phase-B deferred slot)). Acceptance: `CarrierMechanismTest` (the
  four historical failure cases, the injectivity-theft contrasts run on the real `Unifier`, and the
  Generic-arm ride-up-vs-bind decision).
- `monomorphize/check/` — `Checker` (the gated `uniform*` routing + the verbatim
  `checkAgainstDefault`/`defaultArgSlot` fallbacks; arg-slot routing gated to `Platform.Runtime`),
  `UniformCarrierChecker` (the bridge: `intoCarrierHeaded`/`intoCarrierHeadedTerm`,
  `classifyExpectedSlot`, `resolveArgumentSlot`, `resolveGenericSlot` (the ride-aware Generic arm),
  `checkReturnBoundary` with the discharge-to-pure arm, `finalizeAndMaterialize`), `Checker`'s
  `deferredGenericDefault` (the verbatim default-path Generic Phase-B decision the uniform arm
  mirrors), `EffectLifter` (default path; the shared node mechanics
  `pureWrapNode`/`runIdNode` extracted for both paths), `DeclaredPureChecker` (the "declared pure
  but performs an effect" diagnostic — the one effect check accounting cannot voice, since its value's
  mono fails; run per value mono from `TypeStackLoop.runPostDrainResolution`), `TypeStackLoop`
  (`recordAmbientCarriers` — the checker-side ambient *heads* for the live lifter; and
  `groundAmbientCarriers` — the U4-c-0a single writer of the *full ground* ambient carriers onto
  `MonomorphicValue.ambientCarriers`, from the two spellings: open-row binders and pinned/concrete
  returns).
- `monomorphize/channel/` — `WovenValueProcessor` (the Id-normalization stage at the `WovenValue`
  seam; **also demands `EffectAccounting` via `getFactOrAbort`, the codegen precondition** that makes a
  leak block codegen), `IdNormalizer` (U1, on by default), `EffectAccountingProcessor` +
  `EffectAccounting` (the §5 verifier — **the sole effect verifier, unconditional**;
  `verifySubset`/`derivedRow`/`ridesAmbient`/`openRow`), `RefinementChannelProcessor` (the
  architectural template: policy verified post-mono against the final program).

**Uniform-path coverage** (under `--uniform-carrier`, byte-identical to the default path, runtime
track):

| construct | routes uniform? | how |
|---|---|---|
| **value RETURN boundary** (`checkAgainst`) | **yes** — pure, effect-carrier, *and* discharge-to-pure | `uniformReturnBoundary` → `checkReturnBoundary`; pure re-carried via `Id` (erased), effect-carrier passed through, a fully-discharged flex `?G[T]` body under a pure return `Id`-defaulted + `runId`-unwrapped. Gate `uniformReturnRoutable`/`uniformValueReturn`. |
| **argument → PAYLOAD slot** (`checkArgumentSlot`, a concrete non-carrier domain) | **yes** — bind-vs-capture by **payload-fit**; capture pass-throughs uniform (U4-a(ii)) | `uniformPayloadSlot`: if the actual's **payload genuinely fits** the domain ⇒ **bind** (`printLine(readLine)`; the compound-state `items : ?F[List[X]]` into `foldLeft`'s `List[A]`, which the **default path rejects**), pure passes (`runId`). No fit ⇒ `uniformCaptureSlot`: a **capture** (a carrier-stack/pinned domain — a discharger's `{Abort\|G} A` ⤳ `AbortCarrier[G,A]`, `runMain`'s `IO[A]`) routes through the uniform **arm-1 whole-type pass-through** (`tryUnifyCommitting` succeeds ⤳ `Resolved`); a *doomed* under-applied bind (`mustLiftBeforeUnify`) and a *mismatch* stay on `defaultArgSlot`. A **bare-flex payload `?A`** is guarded out of "fits" (`payloadFitsDomain`) — it absorbs any domain and strips the carrier, so it captures. Gate `uniformPlainValueType(domain)` + `Platform.Runtime`. |
| **conditional bodies** (`if`/`else`/`fold`) | **yes** — byte-identical | The whole `IfDemo` surface: return boundary + discharger capture + `fold`'s bare-`A` `Generic` arms all route uniform. |
| **argument → CARRIER-SLOT arm** (`if`'s `value: {Abort} T` = `?G[T]`, a discharger's `fallback: G[A]`) | **yes** — pure pure-wraps first, effectful pass-joins (U4-a(ii)) | `uniformCarrierSlot`: a **pure** actual (`None : Option[?E]`) pure-wraps (`EffectLifter.tryPureWrap`) *before* the default ladder's stealing equal-arity unify — fixing `if(c, None) else Some(x)`, which the **default path rejects**; an **effectful** actual (`if(flag, printLine("on"))`) routes through the uniform CarrierSlot **pass-join** (`uniformArgumentSlot` — the actual's carrier meta joins the domain's, payloads unify, the action passes through), byte-identical to the default whole-unify (no longer a `defaultArgSlot` hand-off). |
| **argument → GENERIC arm** (`fold`'s bare-`A`, a discarded type-param slot) | **yes** — ride-up-vs-bind (U4-a(i)) | The still-bare-flex `Generic` domain's Phase-B deferred decision routes through `UniformCarrierChecker.resolveGenericSlot` → `UniformLadder.resolveGenericSlot`: `occursInValue(metaId, retType)` ⇒ **pass-through** the whole action (transparent callee — `fold`'s selected arm, `identity`), else **bind** the payload and sequence the effect (non-transparent callee — a discarded type-param slot). Byte-identical to the default `deferredGenericDefault` (pinned finding 6 discharged). |
| **the effectful-`catch`-handler** | **no** — gated on U4-e | The stdlib delta works but is not flag-gatable (pinned finding 7); lands atomically at U4-e. |
| function/polytype/`VType` returns, guard/calc-return/W3, **compile-time track** | **no → default** | `checkAgainstDefault` / §8 boundary — *by design*, permanent. |

**Background — conditionals are ordinary functions (no FQN ever hardcoded).** `fold[A](c,
whenTrue: A, whenFalse: A): A` (bare-`A` arms ⇒ `Generic` slots, both must already match — no
auto-lift) and `if[T](c, value: {Abort} T): {Abort} T = fold(c, value, abort)` (the arm declared
`{Abort} T` = `?G[T]` ⇒ a `CarrierSlot`, a pure arm auto-lifts). The ladder classifies by the
*expected slot's* shape, so a user- or platform-defined conditional routes the same way — the
property whose absence killed the v1 weaver's `fold`/`if` hardcode.

**Pinned findings (each validated the hard way — do not re-derive):**

1. **The flip cannot grow per-value under `--effect-channel`** — the (now deleted) `desugarChannel`
   made effectful programs *look pure*, so no per-value gate could separate genuinely-pure from
   effect-blinded values. Hence the distinct transitional `--uniform-carrier` gate; the flags
   unify only at U4-e.
2. **Carrier-heading fires on terminal value leaves only, never a `VPi` reference** — a
   `printLine` leaf (`String → …`) must not wrap to `Id[String → …]`; only a fully-*applied*
   result carrier-heads.
3. **The `if(f,"+")` `VerifyError` crux was the `carrierSlotLift` double-wrap**
   (`pure(runId(pure@Id(…)))` — the inner `pure@Id` confuses the outer `pure`'s impl-keyed erasure,
   `IdNormalizer.isEffectIdMethod`), **not** a `finalize`-defaults-to-`Id` problem; reuse the clean
   single `tryPureWrap` node. The single-slot `if` arm needs no `CarrierJoin`/`finalize`.
4. **The join must never default an ability-constrained carrier meta to `Id`** — ability
   resolution / the discharge must solve it (`?G ~ Abort` ⤳ `AbortCarrier[Id]`). Relevant when
   `CarrierJoin` goes live (the catch-handler's function-return join at `foldEither`'s arms).
5. **An effect-carrier-headed return must guard the self-join** — joining `?F` toward `?F` writes
   a self-referential cycle and loops `resolve`; the guard is landed in `CarrierJoin.join`.
6. **`fold`'s bare-`A` `Generic` arm carries the ride-up-vs-bind check — DISCHARGED (U4-a(i)).** A
   bare-meta domain is *every* generic argument's slot in the base (recognising `fold` by name is
   forbidden); the eager Id-wrap cascades into occurs-check failures, and the arm must **not** be the
   naive pass-whole — it must make the **ride-up-vs-bind** check the default Phase A/B makes
   (`occursInValue(metaId, retType)`: transparent `fold` rides the carrier up, non-transparent
   `putState` must bind-lift). Landed as `UniformLadder.resolveGenericSlot` wired through the Phase-B
   deferred slot (`UniformCarrierChecker.resolveGenericSlot`), byte-identical to the default
   `deferredGenericDefault` for both the ride-up (`fold`'s selected arm) and bind (a discarded
   type-param slot) sides — *not* the eager Id-wrap, which is why it is a Phase-B decision keyed on the
   already-computed `retType`, never a Phase-A eager wrap.
7. **The effectful-`catch`-handler stdlib delta works** (`onError: E => G[A]`, `flatMap`+`pure`
   body — enables `failUnit catch (err -> printLine(err))`, backward-compatible for a single
   discharger) **but regresses two-plus sequenced pure-handler catches on the default path**
   (ambient carrier-stacking — the premature-commitment class the uniform `CarrierJoin`
   eliminates). A stdlib signature change is not flag-gatable, so it lands atomically at U4-e.
8. **A naive accounting wiring (an unfiltered reference union) over-counts run and discharge** —
   the synthetic entry rejects itself (`Console` run on concrete `IO`, nothing declared) and
   discharged ops on inner transformer carriers count as leaks. Superseded by the U4-c
   explicit-interface course; never wire accounting without the ride test (§5).
9. **The ambient-vs-concrete carrier distinction survives monomorphization** — in the mono key ↔
   signature-binder alignment (`establishSignature` binds `typeArguments.lift(i)` per
   `binders.zipWithIndex`) and in each reference's own type arguments
   (`PostDrainQuoter.resolveAbilityRefs` preserves impl type args). It is erased only from a bare
   body walk. This is what makes the §5 derivation possible; the U4-c course makes the value-side
   half explicit as a fact field instead of relying on the alignment as a cross-module contract.

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
form by the elaboration (check-time wrapping in the `Checker` — the resolved §12-Q1 — with the
carrier as an ordinary outermost type application in `SemValue`, reusing the one unifier and the
one evaluator; a parallel (carrier, payload) judgment pair would need its own carrier-flow rules —
row polymorphism reinvented — and would make pinning/reify special again).

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

**The ladder classifies by the *expected slot*, it does not "try pass-through first"** (U2-spike
result). A literal "whole-unify first, fall to bind on failure" ladder is **unsound**:
`list.map(f)` on an effectful `list : ?G[List[Int]]` meets `map`'s Functor slot `xs: F[A]`, and
whole-unifying `?G[List[Int]] ~ F[A]` *succeeds spuriously* — it solves `F := ?G, A := List[Int]`
(the effect stranded as the container, the element type wrong), the same premature solve the theft
cases show. The fix is that the ladder reads the **expected slot's elaborated shape** and picks
exactly one arm, with no speculative first-attempt:

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
elaboration, never a shape guess.

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

**`Id` is the lattice bottom *everywhere*, never a concrete carrier value** (U2-spike result — the
spike rebuilt the premature-commitment bug the moment a pure `Id[String]` actual split to a
concrete carrier contribution). The invariant that dissolves it: the split of any `Id`-headed
judgment yields the bottom carrier, and the carrier of a pure term is bottom, so a pure arm
**contributes nothing** to the join. Consequences, load-bearing: (1) a carrier meta "solved to
`Id`" is indistinguishable from unsolved/bottom (join has no way to *commit* `Id`, which is the
point); (2) the deferred pure-lift is recorded **only when the actual's carrier is bottom** (a pure
arm), keyed on the *result* carrier — an already-effectful arm records nothing; (3) materialization
is decision-free: `pure`/`flatMap` at the joined carrier, or *erased* if it defaulted to `Id`.

**Scope of the invariant** (the sharpest constraint, held from v1 §13.5): the carrier attaches to
**runtime term judgments** — never to the type language itself, never to type-level/compile-time
evaluation. The NbE evaluator stays carrier-free; signature evaluation is untouched; the
compile-time track's `Either` discharge is unchanged (§8). `data` fields stay payload-typed (a
stored computation is spelled pinned, as today).

## 4. The channel: rows, positions, and the reify boundary

**Desugar.** `EffectSugarDesugarer` **keeps minting the carrier desugar** for open rows (v1's
"strip to payload" is the superseded foundation, deleted). What Phase 1 added stays: the
structured **declared row** (`EffectRow[C]` — entries + row positions) is recorded from the open
rows and forwarded on the existing fact chain (a field, per the lean-fact-flow rule) — from
U4-c-0b on this is **rendering metadata** (LSP hover, diagnostics vocabulary), never a
verification input (§5). Pinned rows desugar to canonical carrier stacks exactly as today.

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

*Status: **DONE** (U4-c, 2026-07-24) — `monomorphize/channel/EffectAccountingProcessor` + the
`EffectAccounting` fact is the **sole effect verifier**, wired as a codegen precondition and
verifying unconditionally (§0). The interface to type checking is **explicit and fact-carried**, on
the principle adopted 2026-07-24:*

> **Forward what is declared, derive what is done.** Declaration-level facts (the ambient
> carriers, the declared effects) are forwarded as explicit fact fields with one writer each.
> Per-op verdicts (does this reference's effect actually ride the value's ambient?) are always
> *derived* from the ground instantiations in the mono body — the same artifact codegen consumes —
> never taken from a checker self-report.

**Inputs (explicit, fact-carried):**

- **Declared** = the ability constraints on the value's carrier binders —
  `EffectCarriers.declaredEffects(carrierBinders(view) ∩ paramConstraints)`, machinery
  (`Effect`/`Suspend`) excluded — read off the value's `OperatorResolvedValue`. This is the
  **single source of truth**: surface rows desugar into these constraints, and hand-written
  carrier-generic code (the stdlib dischargers, the lifting instances
  `implement[S, G ~ Abort] Abort[StateCarrier[S, G]]`) declares *only* this way — so the former
  "carrier-machinery-impl exception" stops being an exception and becomes the rule. The surface
  `effectRow` is demoted to rendering metadata (§4).
- **Ambient** = `MonomorphicValue.ambientCarriers` (new field, U4-c-0a): the value's ambient
  effect carriers as **full ground values** (`IO`, `StateCarrier[S, IO]` — never just heads),
  stamped once at mono-fact production from the same two spellings
  `TypeStackLoop.recordAmbientCarriers` reads — an open row's carrier binders (their ρ-values at
  the mono key) and a pinned/concrete-carrier return (the return's carrier prefix, `Effect[C]`
  resolving as the authority). Empty for a pure value and for the synthetic entry.

**Derivation (per mono key, from ground instantiations, U4-c-0d):** walk the checked body
(`MonomorphicValue.runtime`); an effect-ability method reference (`Qualifier.AbilityImplementation`,
or a constraint-covered `Qualifier.Ability` left abstract) contributes its owning ability, an
ordinary callee its declared set — in both cases **iff the reference rides an ambient**:

> the reference's own carrier — read as the **callee's forwarded `MonomorphicValue.ambientCarriers`
> at the reference's mono key** (`getFactOrAbort(MonomorphicValue.Key(ref, typeArgs))`) —
> **equals an ambient carrier by exact `GroundValue` equality**.

The callee's ambient *is* the reference carrier for every class, computed once by the 0a writer:
a generic effect method / carrier-generic callee forwards its carrier-binder value, and a binder-less
**concrete-carrier impl** (`implement Inf[IO]`, whose impl method carries no type argument) forwards
the carrier from its own return head. So there is no positional reconstruction and no per-class arm —
reading the fact means both sides compare identical quotes (exact equality is reliable), and the
concrete-impl carrier that has no `typeArgument` to point at is simply the callee's own ambient. Two
qualifier classes are excluded up front, never ride-tested: the **machinery** (`Effect`/`Suspend`) and
the **match-family eliminators** (`PatternMatch`/`TypeMatch`) — the latter because a match's result
type follows its branches, so it is carrier-headed over an effectful `match` (a non-empty ambient that
would otherwise spuriously ride), yet it is structural dispatch, never a user effect. First-order
abilities (`Show`/`Eq`/…) need no exclusion: their result is a fixed non-carrier type, so their ambient
is empty and the ride test drops them (this is what let the effect-vs-first-order marker lookup — and
its spurious `Could not find` on non-colocated markers — be deleted).

Exactness matters: it is strictly tighter than the checker's head-level `CarrierHead` test and
correctly separates nested same-transformer stacks (`ThrowCarrier[E2, ThrowCarrier[E1, IO]]` ≠
ambient `ThrowCarrier[E1, IO]`). One rule then covers everything that used to look like it needed
subtraction or exemptions:

| situation | ride test outcome |
|---|---|
| effect op / callee riding the value's own row (`printLine@[IO]` in `main@[IO]`) | carrier = ambient ⇒ **counted** |
| discharged op (`raise` on `ThrowCarrier[…]`/`AbortCarrier[…]` under a `catch`) | inner transformer ≠ ambient ⇒ absent — **discharge is structural, no annotation** |
| captured computation (passed into a pinned slot — a discharger's `{E \| G} A`, `runMain`'s `IO[A]`) | concrete stack ≠ ambient ⇒ absent; its legality is check 2 |
| the synthetic entry (`def main: Unit`, runs the user main on concrete `IO`) | ambient set **empty** ⇒ everything absent — **no synthetic-entry exemption exists** |
| a `{Throw}` value monomorphized *at* its capture stack | its own `raise` = ambient ⇒ counted, and declared |

**Checks:**

1. **derived ⊆ declared** — "performs the effect 'X' but does not declare it", uniformly for every
   effect including `Inf` (an ordinary entry riding the same union — the totality story is
   unchanged);
2. **reify legality** — a captured reference (concrete stack ≠ ambient) has its row ⊆ the pinned
   entries (a later slice; the ride test is its foundation — capture is now a *detected outcome*,
   not a guess);
3. **fail-safe reads** — a counted-class reference (a non-machinery `AbilityImplementation` op)
   whose `OperatorResolvedValue`/ability-marker fact is absent **aborts** (`getFactOrAbort`),
   never contributes `Set.empty`: silent-empty is the under-count direction, the one that lets a
   leak through.

The accounting fact is the LSP's hover source for rows (declared from the rendering metadata,
derived from the fact). The exactness argument holds as always: computed per concrete
instantiation of the whole program; declaration-level granularity by intent.

**Rejected interface designs (recorded so they are not re-proposed):**

- **Per-op forwarded bits** (a `ridesAmbient`/`discharged` flag on `MonomorphicExpression`
  references): converts the audit into a checker **self-report** — a premature-commitment-class
  checker bug would forward its own wrong belief into the verifier, which exists to catch exactly
  that class; plus per-node schema churn across every phase, and the §9 per-node-forwarding
  reversal stands. The mono type-arguments already *are* the forwarded information, in the
  strongest form: they determine what codegen emits and which instances resolve.
- **Negative effects** (`-E` on signatures): re-runs a mechanism deleted 2026-07-20 for cause.
  Wrong granularity — a signature-level annotation cannot distinguish two occurrences of one
  effect in a body (one caught, one propagating); a new lying surface — a wrong or stale `-E`
  silently *hides* a leak (the fail-safe-violating direction); and a UX regression — structural
  discharge (dot-chained `catch`/`runStateTo…` just compiling) is one of the system's best
  properties.

**What verification can never absorb** (and how it landed): diagnostics for programs that never
produce mono facts — `State`/`Throw`/`Abort` leaks dying (cryptically but soundly) in
`AbilityResolver`, and the "declared pure but performs an effect" case, off `unifier.errors`. At
U4-c-2 the declared-pure case was extracted from the deleted residual checker into the focused
`DeclaredPureChecker` (a default-path check run per value mono from
`TypeStackLoop.runPostDrainResolution`, reading `unifier.errors`), *not* the uniform checker's
boundary — the uniform checker is not the default path, so relocating there would have regressed the
diagnostic. The `AbilityResolver` control-effect leaks stay cryptic (independent of the residual
checker; a friendlier message is a possible later polish, not a blocker).

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
  identity/apply lambda (eta-expansion, built from the reference's own function type);
- **type positions**: `Id[X]` ⤳ `X` everywhere — in node types, signatures, and **mono keys /
  type arguments** (`fold[Id[String]]` ⤳ `fold[String]`), merging Id-instantiations with their
  payload instantiations (sound: after body rewrites `Id[X]` and `X` are representationally
  identical);
- pinned stacks over `Id` erase their base layer: post-mono the stack machinery's calls into
  `Effect[Id]` are concrete and reduce, so `runThrow`'s `Id[Either[E, A]]` becomes
  `Either[E, A]`.

**Recognizing by FQN is sanctioned here**, unlike the v1 weaver's `fold`/`if` hardcode: `Id`,
`runId`, and `Effect[Id]`'s methods are **compiler-owned machinery** the checker itself inserts by
fixed FQN (`WellKnownTypes.idFQN`/`runIdFQN`) — not user vocabulary — the ordinary well-known-types
practice. That is the precise line between the two: hardcoding user-extensible names is unsound;
hardcoding compiler-owned insertions is how every such pass works.

**Load-bearing, with a hard fail-safe.** Pervasive `Id` is only acceptable because it reliably and
*provably* erases, so this stage is a mandatory compilation stage, not an optimization
(per the gaps-must-be-fail-safe rule): a post-pass **assertion that no `Id` FQN or `Id[X]` type
survives** in any emitted type, key, or reference — a warning today, a hard build error from U4-e.
Belt-and-braces: `Id` has **newtype representation** in codegen (`GroundValue.carrierFQN` erases
`Id[X]` to its payload's carrier; constructor and accessor emit nothing), so any hypothetically
missed residue is a no-op rather than an allocation.

**Implemented (U1 complete, on by default).** `monomorphize/channel/IdNormalizer.scala`, invoked
from `WovenValueProcessor` — the `WovenValue` codegen redirect seam (`used`/`uncurry`/jvm read
`WovenValue.Key`) is exactly the slot between checking and codegen this stage occupies.
Load-bearing implementation notes, kept because they were found the hard way:

- the `runId` *accessor's own body* is the Church-encoded `PatternMatch.handleCases` apparatus,
  not a `getfield`, and must itself be rewritten to `obj -> obj` (`normalizeValue`) — otherwise
  `used` keeps the whole `Id` pattern-match apparatus alive and a first-class dot-chain `x.runId`
  runs it over a wrapper the newtype no longer allocates (a crash);
- erasing a *reference's* type arguments is what merges an `Id`-instantiation with its payload
  instantiation — the erased args become the callee's demanded mono key; the WovenValue's **own**
  key is deliberately not rewritten (the `TransformationProcessor` contract requires produced key
  = demanded key, and the demand arrives already erased);
- a **bare** `Id` (the higher-kinded `G` of `AbortCarrier[Id, A]`) is left — it has no payload to
  collapse and survives until deeper stack lowering;
- a bare reference standing as a *whole body* (`def r = runId`) is eta-expanded by threading the
  value's signature as the top node's type.

With eta-expansion, **no `Id` machinery survives normalization at all** (`eliot.lang.Id` ships no
`runId`/`pure`/`flatMap`/`map` method), which is what lets the U4-e assertion become a hard error.

**The MCU story.** With `Id` erased, pure code compiles to plain calls. For *effectful* MCU code,
the carrier is compile-time bookkeeping the backend may lower away: post-mono, every carrier
value's construction and run site is statically known, so straight-line effect sequences erase
wherever the platform's carrier is representationally identity, and suspended conditional arms
defunctionalize into branches — a standard whole-program lowering (`fold(c, IO(a), IO(b))` run
directly is statically `if c then a() else b()`). Control-effect stacks keep real representation
or lower to CPS/state — a per-backend choice. Nothing monadic needs to survive to a
microcontroller; recorded here as the design intent for the MCU backend (U5 follow-up).

## 7. What remains to delete, what stays

The v1 erasure path is already fully deleted (the weaver + entry-point config at U3-0a; the
effect-blind `desugarChannel`, the `AbilityResolver` abstain, the conformance relaxation and their
tests at U4-b Bundle A; the residual checker's Phase-2 shadow separately) — details in the git
log.

**Deleted at the U4 flip** (the old default-path machinery the uniform checker replaces, live
until then):
`EffectLifter`'s recognition arms — `mustLiftBeforeUnify`/`mustPureWrapBeforeUnify`, the
equal-arity arm with its guards, `underApplied`/`isFlexMeta`, `effectCarrierSplit`'s ambient/role
recognition — and `tryIdDefault` *as an arm* (promoted into the join solver, §3);
`CheckState.ambientCarriers` + `recordAmbientCarriers` (their accounting role is replaced by the
forwarded `MonomorphicValue.ambientCarriers` field, whose *writer* then reads the uniform
checker's carrier bookkeeping — the fact contract is unchanged, §9); the `Checker`'s Phase A/B
flex-slot deferral (replaced by decision-free deferred lift materialization);
`CarrierKindChecker`'s carrier-specific duties; the synthetic main's `apply(block(main), unit)`
spelling (→ `runMain(<user main>)` — making the one legitimate run boundary **nominal**). The
bind/`pure` *mechanics* (`wrapBinds`/`bindWrap`/`tryPureWrap`/`pureWrapNode`, the `$eff$N` splice
convention) survive reshaped as the uniform ladder's insertion step.

(`EffectResidualChecker` was already deleted at U4-c-2 — earlier and independent of the flip, since
accounting replaced its subset check — with its one declared-pure diagnostic moved to
`DeclaredPureChecker`; it is not part of this flip-deletion list.)

**Stays**: the surface (open + pinned rows, ambient effects, dischargers unchanged); the channel
(`EffectRow` plumbing as rendering metadata, `EffectAccountingProcessor` as the verifier, LSP row
rendering); pinned rows as the declared capture boundary; the platform carrier `data` types and
their `Effect`/`Suspend` instances (`eliot.carrier`); **`Id`, promoted** — the universal pure
carrier during checking, erased by §6 (its compile-time overlay remains for §8); `runMain`; the
`WovenValue` seam as the normalizer's home; the `termination` story (`Inf` as a row entry);
`namedValues`; eliot-test unchanged.

**Stdlib deltas stay additive**: the effectful-handler `catch` (`onError: E => G[A]`) lands
atomically at U4-e (pinned finding 7).

## 8. The compile-time residue

Unchanged from v1, with the scoping constraint explicit. The checker *consumes* effect
discharge on the compiler platform: effectful signatures (`{Throw[String]} Type` calculated
returns, guards) evaluate on the `Either[String, _]` carrier and are read back by
`CalculatedReturnResolver`. This stays as is — one fixed carrier, pure control effects only,
bounded. The uniform-carrier elaboration applies to **runtime term judgments in value bodies**
(both tracks use the one checker, so compile-track value bodies get the same uniform treatment);
what is *never* carrier-wrapped is the type language itself — signature evaluation, NbE forcing,
`VType`-level computation. Entangling `Id` into type-level evaluation is the failure mode to guard
against.

The boundary is pinned by three green assertions (U2 spike, kept as regression cases): a
type-level judgment (`Int[0,255] ~ Int[0,255]`) unifies by plain payload unification and
**introduces no carrier metavariable**; `split` **refuses** a type-of-types (`VType`) judgment — a
type-level term is not carrier-headed, so nothing can accidentally carry it; and the §8 `Either`
discharge carrier is a **data** constructor, joined nowhere as a runtime carrier — the
compile-track `{Throw[String]}` discharge stays a type-level `Either` fold. The rule that keeps
this sound is mechanical: the carrier machinery (`split`, the join, the ladder) is invoked **only
on runtime term judgments**; the NbE/signature path never calls it.

## 9. Held invariants and interactions

- **The elaboration invariant**: every runtime term judgment is carrier-headed, carrier outermost,
  maintained by construction — never by recognition. No phase may reintroduce a "is this type a
  carrier?" query.
- **Carrier metas solve by join** (`Id` bottom, one non-`Id` winner, conflict = mismatch, unsolved
  = `Id` — except an ability-constrained carrier meta, which ability resolution/discharge must
  solve, pinned finding 4); first-contact unification of carrier positions is the
  premature-commitment bug class and must not be reintroduced.
- **The channel interface is fact-carried and stable.** Verification inputs are explicit fields on
  facts (`MonomorphicValue.ambientCarriers`; the constraint-based declared set on
  `OperatorResolvedValue`). A field's *writer* may change (U4-d swaps the ambient's source from
  `CheckState.ambientCarriers` to the uniform checker's bookkeeping); the schema and meaning may
  not. Verification never reads checker state.
- **Forward what is declared, derive what is done.** Declaration-level facts may be forwarded
  through mono; per-op verdicts are always derived from ground instantiations. A forwarded per-op
  bit is a checker self-report and is rejected (§5), as is any negative-effect surface.
- **`effectRow` is rendering metadata** (LSP hover, diagnostics vocabulary) — never a verification
  input (from U4-c-0b).
- **No `Id` residue**: the §6 assertion is a permanent invariant from U4-e on — `Id` exists between
  elaboration and normalization, nowhere downstream.
- **Pinned types are declared, never inferred** — capture stays syntax-directed. With `runMain`
  nominal (U4-d), every effect boundary is *declared* (capture), *structural* (discharge), or
  *nominal* (run) — never guessed.
- **Normalization/reordering reads carriers, not rows**: any future normalizer (reduce-and-reify)
  treats non-`Id`-carriered terms as observation-ordered. No per-node row annotation is forwarded
  through mono (`MonomorphicExpression`'s expression shapes are untouched); the carrier in the
  types is the per-node signal, the channel stays per-declaration.
- **Suspend-riding effects still cannot be pinned** (no canonical carrier); the designed
  `Suspended` platform-base extension (`docs/effect-row-tails.md` §Limits) remains the answer.
- **Types-are-values, restated at the flip**: effects are represented in types uniformly; `Id` is
  an ordinary value; rows are the user surface and the verifier's vocabulary, checker-adjacent
  metadata that never flows back into types.
- **LSP**: hover composes the payload type with declared/derived rows from the channel;
  `GroundValueRenderer` keeps stack→pinned-row rendering; `Id` and carriers are never rendered to
  users — error messages likewise (a U4-e gate).

## 10. Migration: landed phases and the path forward

The gated-flip playbook throughout (signature-unification precedent): every slice lands with the
default path byte-identical, gated by the §0 harness.

### Landed (summary — details in git log)

- **U1 — Id-normalization stage: COMPLETE, on by default.** §6. Body rewrites + newtype
  representation, `Id[X] ⤳ X` type/key erasure (Id-instantiations merge with payload
  instantiations via the demand shift), first-class-combinator eta-expansion. Zero `Id` residue
  across suites, examples, eliot-test.
- **U2 — foundation spike: COMPLETE.** Results folded into §3 (`Id` bottom everywhere;
  classify-by-expected-slot) and §8 (the compile-time boundary); Id-free rendering and
  constant-factor perf confirmed; its cases became `CarrierMechanismTest`.
- **U3a — the uniform mechanism + checker bridge: LANDED as far as it goes pre-flip.** The
  `monomorphize/carrier/` package and the `UniformCarrierChecker` bridge, wired into the `Checker`
  behind `--uniform-carrier` with tight per-shape gates and verbatim default fallbacks; coverage
  per the §0 table, incl. two non-overlap wins the default path rejects.
  `CarrierJoin`/`finalizeAndMaterialize` are built but uncalled (first live use: the catch-handler
  join). The v1 erasure path is fully deleted (weaver at U3-0a; `desugarChannel`/abstain/relaxation
  at U4-b Bundle A).

### U4 — the flip (in progress)

1. **U4-a — complete uniform *coverage* (the gating prerequisite for U4-d).** Nothing that
   legitimately runs on the runtime track may fall back to the default ladder. Gaps: (i) the
   **`Generic` arm** — **DONE (U4-a(i))**, carries the **ride-up-vs-bind** decision
   (`occursInValue(metaId, retType)` → pass-through if the meta rides the result, else bind; never the
   naive `PassWhole`, pinned finding 6); (ii) reshaping the **capture / mismatch** fallbacks
   (`defaultArgSlot`'s capture case, `resolveGuardedLadder`/`resolveLadder`) into the uniform ladder so
   the carrier-stack/pinned capture is a uniform outcome, not a default hand-off — **in progress**
   (U4-a(ii)-0: the CARRIER-SLOT arm's *effectful* actual pass-joins uniform; U4-a(ii)-1: the
   PAYLOAD-slot no-fit *capture* whole-type pass-throughs uniform; U4-a(ii)-2: the PAYLOAD-slot
   *mismatch* leaf commits uniform — all landed; **remaining**: the *doomed under-applied bind*
   (`uniformCaptureSlot`'s bind-lift branch) and the Generic arm's Phase-A deferral entry still hand off
   to `defaultArgSlot` — both routed uniform at U4-d, where the byte-identity constraint lifts and the
   default path is deleted). *By-design defaults, permanent* (§8): the compile-time track,
   `VType`/guard/calc-return/W3, and function/polytype (`VPi`/`VLam`) returns (pinned finding 2).

   - **U4-a(i)-0 — the ride-aware Generic resolver (pure mechanism): LANDED (2026-07-24).**
     `UniformLadder.resolveGenericSlot(unifier, actual, metaId, retType, context)` makes the
     ride-up-vs-bind decision the naive `resolveSlot`/`PassWhole` omits (pinned finding 6): a bare
     flex domain `?metaId` receiving a carrier-headed actual passes the *whole* action through iff
     `unifier.occursInValue(metaId, retType)` (a transparent callee whose result flows from the
     domain — `fold`'s selected arm, `identity`, `const`; byte-identical to the default Phase-B
     `doUnify(?metaId, actual)` → `Resolved`), else **binds** — `?metaId :=` the payload, the carrier
     sequences here (a non-transparent callee like `putState[S, F](s: S): F[Unit]` whose result carrier
     is independent of the domain; byte-identical to the default `tryBindLift` → `Bound`). Built and
     unit-tested in isolation (`CarrierMechanismTest`: ride-up pass-through, byte-identity with the
     plain `PassWhole` primitive it composes, and the bind case), **uncalled live** so the default path
     stays byte-identical — the U3a "build the mechanism, then wire" discipline (as `CarrierJoin` was).
   - **U4-a(i)-1 — wire the Generic arm: LANDED (2026-07-24).** The Phase-B deferred-slot decision
     (`Checker.resolveDeferredSlot`, the still-bare-flex `VMeta(id, SNil)` domain) routes through
     `UniformCarrierChecker.resolveGenericSlot` under `--uniform-carrier` + `Platform.Runtime`; the
     verbatim default ride/bind logic was extracted to `Checker.deferredGenericDefault` (the flag-off
     fallback, byte-identical by construction — same code, moved). The uniform bridge builds the bind
     node with the **proven** payload-bind mechanics (`Carrier.toSemValue(actualCarrier)`, slot typed at
     the split payload, `EffectLifter.Bind` for `wrapBinds`), so PassWhole ⤳ `Resolved` and Bound ⤳ the
     same `$eff$N`/`Bind` the default `tryBindLift` produces. **Byte-identical, both sides:** the ride-up
     side is exercised by `pick(flag) = fold(flag, printLine("a"), printLine("b"))` (fold's bare-`A`
     arms) in `UniformCarrierByteIdenticalTest`'s conditional corpus; the bind side by a new program
     `first[A, B](a, b): A = a` called `first("x", readLine)` (an effectful arg into a discarded
     type-param slot). Full §0 gate green (lang/jvm test, HelloWorld, eliot-test 11/11).
   - **U4-a(ii)-0 — effectful actual into a carrier slot pass-joins: LANDED (2026-07-24).**
     `uniformCarrierSlot`'s effectful branch (`if(flag, printLine("on"))`) now routes through the uniform
     CarrierSlot **pass-join** (`uniformArgumentSlot` → `resolveArgumentSlot`: the actual's carrier meta
     joins the domain's, the payloads unify, the whole action passes through as `Passed`) instead of
     handing off to `defaultArgSlot`. Byte-identical to the default whole-unify (`?F[Unit] ~ ?G[T]` ⇒
     `?G := ?F`, `T := Unit`, slot expr unchanged) — validated by `report`'s `if(flag, printLine("on"))`
     in `UniformCarrierByteIdenticalTest`'s conditional corpus. Full §0 gate green.
   - **U4-a(ii)-1 — payload-slot capture whole-type pass-throughs uniform: LANDED (2026-07-24).**
     `uniformPayloadSlot`'s no-fit branch now routes through `uniformCaptureSlot` instead of handing off
     to `defaultArgSlot`: the *doomed* under-applied bind (`EffectLifter.mustLiftBeforeUnify` — must
     bind-lift, not capture) is checked **first** and stays on `defaultArgSlot`; otherwise the uniform
     **arm-1 whole-type pass-through** (`tryUnifyCommitting`) is tried — **success is the capture**
     (`Resolved`, byte-identical to the default `resolveFailureLadder` arm-1 whole-unify — same
     `tryUnifyCommitting`, same solutions, same slot expr), **failure hands the mismatch to
     `defaultArgSlot`** (byte-identical because `tryUnifyCommitting` commits nothing on contradiction, so
     the re-run just commits the same mismatch). No carrier-constructor recognition was needed — the
     whole-type unify *is* the capture, succeeding iff the domain spells a carrier form (§3 arm 1).
     Validated by a new byte-identical program `parseOk : {Throw[String]} String` captured whole by
     `parseOk catch (err -> err)` (`?F[String]` ⤳ `ThrowCarrier[E, G]`). Full §0 gate green.
   - **U4-a(ii)-2 — payload-slot mismatch commits uniform: LANDED (2026-07-24).** `uniformCaptureSlot`'s
     not-doomed / whole-unify-fails leaf now commits the mismatch **directly** via `commitMismatch`
     instead of handing off to `defaultArgSlot` (which only bottomed out at exactly that `commitMismatch`
     — a non-fitting non-doomed actual's bind-lift / pure-wrap arms cannot fire, and the failed
     `tryUnifyCommitting` commits nothing, so the state and the error are identical). Validated by a new
     error-comparison test: `printLine(true)` (`Bool` into `String`) reports the identical non-empty
     error set off vs on. Full §0 gate green. **Remaining U4-a(ii):** the *doomed under-applied bind*
     (`uniformCaptureSlot`'s `mustLiftBeforeUnify` branch — the last argument-slot bind-lift on the
     default path) and the Generic arm's Phase-A deferral entry, both routed uniform at U4-d.

2. **U4-b — Bundle A: LANDED (2026-07-24).** The `--effect-channel` erasure path deleted;
   `effectChannel` threads to `EffectAccountingProcessor` only (removed at U4-e);
   `contributedEffects` re-pointed to the resolved-impl view. Two mechanisms it established, used
   by U4-c: an effect ability is discriminated from a first-order impl by the **ability marker's**
   HKT carrier binder (never the impl marker — a concrete `implement Inf[IO]` marker has no
   binder), and the ability's module is `ref.moduleName` (effect abilities and their instances are
   colocated in `eliot.effect`; the marker lookup succeeding there confirms it).

3. **U4-c — swap the verifier: the explicit-interface course (adopted 2026-07-24).** Principle
   and mechanism in §5; rejected alternatives recorded there. Steps, in order — the first four
   are small, independently testable, and **modify already-landed code**:

   - **U4-c-0a — forward the ambient (schema + writer): LANDED (2026-07-24).**
     `MonomorphicValue.ambientCarriers: Set[GroundValue]` — full ground carriers, quoted post-drain
     (every carrier meta solved) by the single writer `TypeStackLoop.groundAmbientCarriers` from the
     two spellings `recordAmbientCarriers` reads (open-row: each `carrierBinders ∩ paramConstraints`
     binder's ρ-value at the mono key; pinned/concrete return: the return's carrier prefix, `Effect[C]`
     as the authority). Empty for pure values and the synthetic entry; a carrier not ground-quotable
     (residual metas at a partial-arity mono) is skipped (fail-safe). The field has **no default** — a
     silent `Set.empty` is the under-count direction. Stamped at `MonomorphicTypeCheckProcessor`; the
     12 direct `MonomorphicValue` constructions in `UsedNamesProcessorTest` pass `Set.empty`. Pinned
     directly by `MonomorphicAmbientCarriersTest` (jvm full compile: `greet:{Console}Unit` ⤳ `{IO}`,
     pure `label` ⤳ `∅`), since nothing consumes the field until U4-c-0d. This supersedes
     reconstructing the ambient from the key↔binder positional alignment inside accounting — the
     alignment stays true, but stops being a load-bearing cross-module contract.
   - **U4-c-0b — single source of truth for "declared": LANDED (2026-07-24).**
     `EffectAccountingProcessor.declaredEffectsOf` reads
     `EffectCarriers.declaredEffects(carrierBinders ∩ paramConstraints)` off the value's
     `OperatorResolvedValue` — the residual checker's own definition — instead of
     `channelDeclaredEffects(effectRow)`. This makes the lifting instances and hand-written
     dischargers correct *by the rule*: the "carrier-machinery-impl exception" is deleted as a
     concept. `channelDeclaredEffects` (+ `EffectAccountingChannelDeclaredTest`) survive **only** as
     the rendering-side row extraction (LSP declared-row vocabulary, §4/§5), no longer a verification
     input. Byte-identical by construction — the processor is still unwired (`effectChannel` off ⇒
     inert), so this changes no live compile; validated at U4-c-1's parity + rejection gate.
   - **U4-c-0c — the pure ride-test core: LANDED (2026-07-24).**
     `EffectAccountingProcessor.ridesAmbient(referenceCarriers, ambient)` (companion object, the
     `channelDeclaredEffects` pattern — pure over `GroundValue` sets, no `CompilerIO`): ride iff
     **exact `GroundValue` equality** between one of the reference's carriers and an ambient carrier.
     `Eq[GroundValue] = fromUniversalEquals`, so exactness separates nested same-transformer stacks and
     makes discharge/capture structural. **The reference's carriers are the callee's own forwarded
     `MonomorphicValue.ambientCarriers` at the reference's mono key** (the 0a writer) — which *is* the
     reference carrier for every class with no positional reconstruction: a generic effect method /
     carrier-generic callee forwards its carrier-binder value, and a binder-less concrete-carrier impl
     (`implement Inf[IO]`, no type argument) forwards the carrier from its return head, both already in
     the one field (empirically confirmed: `forever@Inf#IO` carries **empty** type args, `IO` only via
     its own ambient). Matrix unit test `EffectAccountingRideTest` (8 cases): run / captured-or-discharged
     (inner transformer) / empty ambient (no synthetic-entry exemption) / no carrier (pure callee) /
     any-of-several-ambients / disjoint / pinned-stack ambient / nested same-transformer stack.
     Additive, no live caller yet — byte-identical by construction.
   - **U4-c-0d — rewire the derivation + fail-safe reads: LANDED (2026-07-24).** `collectReferences`
     keeps `MonomorphicValueReference.typeArguments` (the `(vfqn, _)` discard was the naive-wiring gap,
     pinned finding 8); `contributedEffects` gates **every** contribution through the ride test, reading
     the reference's carriers as the **callee's** forwarded `MonomorphicValue.ambientCarriers`
     (`getFactOrAbort(MonomorphicValue.Key(ref, typeArgs))`, so a counted-class reference with a missing
     callee mono **aborts**, never `Set.empty`). Two refinements the wiring surfaced: (i) the
     effect-vs-first-order **marker lookup was deleted** — it triggered a spurious `Could not find` for a
     non-colocated / synthetic ability marker, and is unnecessary since a first-order impl is pure
     (empty ambient) and the ride test filters it; (ii) the **match-family eliminators**
     (`PatternMatch`/`TypeMatch`) join the machinery exclusion — their result type follows the eliminated
     branches, so over an effectful `match` it is carrier-headed (a non-empty ambient that would
     spuriously ride), yet they are structural dispatch, never a user effect. Still unwired
     (`effectChannel` off ⇒ inert, byte-identical), but the derivation is validated one slice early by
     `EffectAccountingDerivationTest` (jvm full compile under `--effect-channel`, demanding accounting for
     every mono value): the discharge program derives `main`⤳`{Console}` with `parseOk`/`parseBad`⤳`∅`
     (capture excluded) and **no over-count error**, and the `Inf` program derives `{Inf, Console}`
     (`forever` counted via its forwarded ambient). **Touches landed code:** `EffectAccountingProcessor`
     only.
   - **U4-c-1 — wire + parity: LANDED (2026-07-24).** `WovenValueProcessor` demands
     `EffectAccounting.Key(mv.vfqn, mv.typeArguments)` via `getFactOrAbort` before producing its
     `WovenValue`, so accounting runs as a **codegen precondition per used value, alongside the live
     `EffectResidualChecker`**: a leak's accounting abort blocks the value's `WovenValue` and so its
     codegen. Off the `--effect-channel` flag the demand resolves to the empty row (no verification), so
     the woven output — and all codegen — is **byte-identical** (`UniformCarrierByteIdenticalTest` +
     HelloWorld + eliot-test 11/11 green). `UsedNamesProcessorTest`, the one minimal-set harness driving
     `WovenValue`, gains `EffectAccountingProcessor` in its set. **Parity (on flag):** HelloWorld and
     every effect example (`Effects*`, `DischargeDemo`, `HandleWith`) compile clean under
     `--effect-channel` — no over-count blocks valid codegen (the three baseline failures
     `EffectsTwoDeps`/`EffectsTwoThrows`/`WherePrecondition` fail earlier at mono, so accounting never
     runs for them). **Rejection (on flag, `EffectAccountingWiringTest`, jvm full compiles):** an
     undeclared `Console` reddens, and an undeclared `Inf` reaching a `{Console}` value reddens (the
     concrete-carrier `implement Inf[IO]` arm — `forever` counted via its forwarded ambient). During the
     two-verifier window the residual checker preempts (aborts the mono before accounting runs), so these
     lock the end-to-end behavior and become accounting-specific at U4-c-2; the wiring's own correctness
     is carried now by parity (no over-count) + `EffectAccountingDerivationTest` (correct rows).
   - **U4-c-2 — delete `EffectResidualChecker`: LANDED (2026-07-24).** Done in two slices. **Slice A:**
     accounting verifies **unconditionally** (the `--effect-channel` gate on verification dropped — the flag
     is now vestigial, removed at U4-e), so it is the sole subset verifier on the *default* path. This
     surfaced one real over-count fixed here: a **concrete-carrier return** (`def main: IO[Unit] =
     printLine(…)`) has no carrier binder, so `declaredEffectsOf` is empty, yet it performs Console — the
     subset check (`verifySubset`) now fires **only for a value with an open effect row** and exempts a
     concrete-carrier return (its chosen carrier permits its effects), mirroring what the residual checker's
     `checkDeclaredPure` did by exempting an *applied* return. **Slice B:** `EffectResidualChecker` deleted;
     its one diagnostic accounting cannot voice — "declared pure but performs an effect", for a value whose
     mono *fails* (a nullary non-carrier return can't host its effect) — extracted to the focused
     `DeclaredPureChecker` (no `force`/ambient/ride machinery, just the platform), still called per value
     mono from `TypeStackLoop.runPostDrainResolution`. The subset diagnostics were byte-identical between the
     two emitters, so the existing subset tests pass unchanged (now via accounting, post-mono); the
     declared-pure tests pass via `DeclaredPureChecker`. **Accounting is now the sole effect verifier** and a
     leak reddens through it with no flag. The `State`/`Throw`/`Abort` `AbilityResolver` leaks stay cryptic
     (independent of the residual checker — nothing to relocate). Gates: whole base + all example integration
     tests + eliot-test 11/11 + HelloWorld green.

4. **U4-d — delete the default-path machinery** (dead once U4-a coverage is complete): the §7
   flip-deletion list. Two items need care beyond deletion: (i) **diagnostics relocation** — the
   "declared pure but performs an effect" message and friendly voicing for the
   `AbilityResolver`-killed control-effect leaks move to the uniform checker's boundary (they
   concern programs that never produce mono facts and can never live in accounting) — this
   **precedes** U4-c-2 if U4-c finishes first; (ii) the synthetic main respells to
   `runMain(<user main>)`, making the run boundary nominal (§9). The
   `MonomorphicValue.ambientCarriers` writer switches its source from `CheckState.ambientCarriers`
   to the uniform checker's carrier bookkeeping — the fact contract is unchanged.

5. **U4-e — make it the default + close out.** Remove `--uniform-carrier` and `--effect-channel`
   and their threading; land the **effectful-`catch`-handler stdlib delta** atomically (pinned
   finding 7 — the join solver is now the default carrier handling, so the stacking cannot
   occur; acceptance: `failUnit catch (err -> printLine(err))` runs and `EffectsThrow` stays
   green); turn the §6 Id-residue assertion into a **hard error**; the §9 Cornerstone amendment +
   doc/skill sweep (`eliot-code` global skill, `eliot-layers`, CLAUDE.md effect + monomorphize
   sections); verify LSP/diagnostic rendering `Id`-free.

### U5 — follow-ups unlocked

Row-bearing diagnostics everywhere; the evaluation-order decision (resolved-argument order vs
source order — §12); `Suspended` for first-class platform actions; the MCU lowering (§6) when
that backend activates; reduce-and-reify's carrier-based observation ordering (§9); the reify
legality check (§5 check 2) on the ride-test foundation.

## 11. Risks

- **U4-a coverage invasiveness** is the honest big one: the remaining ladder reshaping threads
  through the ~1200-line `Checker`. The spike + `CarrierMechanismTest` + the byte-identical gate
  bound it; the flag keeps the default path safe.
- **Join-solver correctness at first live use** (the catch-handler): deferred lift
  materialization must be total, and an ability-constrained carrier meta must never default to
  `Id` (pinned finding 4). A missed insertion is a loud type/codegen error, not silence — but
  budget for the tail.
- **Accounting under-count hazards** (the fail-safe direction — a leak passing silently): the two
  named ones are the concrete-impl arm and missing-fact reads, each with an explicit
  countermeasure (the rejection tests; abort-on-missing). Over-count is self-announcing during
  parity (a red compile on valid code).
- **Error-message regression**: the U4-d diagnostics relocation must land before the residual
  checker's deletion, or the friendly messages silently revert to cryptic ones; `Id`/carriers
  must never leak into user-facing text (a U4-e gate).
- **Two verifiers during the parity window** cost maintenance; the window is kept short by the
  slice ordering, as the v1 phases demonstrated in practice.

## 12. Open questions

1. **Evaluation order** (carried over): resolved-argument order vs source order — a recorded U5
   decision.
2. **`reify` surface syntax** (carried over): declared-type-directed capture covers every current
   use; an explicit form remains a possible later addition.

Everything else previously tracked here is resolved and folded into the design sections: the
`SemValue` representation and check-time carrier-wrapping (§3), the join lattice and discharge
stacks (§3, §8), channel metadata shape (§4), per-node rows through mono (reversed, §9), the
verifier interface (§5, adopted 2026-07-24).

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
   (§2). All four historical failure cases were re-derived to confirm (§3; they are regression
   tests).
2. **Uniformity alone does not answer bind-vs-pass — the unify-first ladder does** (§3), and
   generic-slot pass-through is simultaneously the *extensible-conditionals mechanism*: laziness in
   effects = parametricity + reified actions, zero compiler knowledge of any conditional's name.
   This also corrected v1 §4's argument-strictness overclaim into the two-case spec rule.
3. **Naive whole-unify-first would have rebuilt the premature-`Id`-commitment bugs** at flex
   carrier slots (`if(c, "a") else readLine` — the pure arm must not commit the slot before the
   effectful sibling contributes). Hence the **join solver** with `Id` as lattice bottom and
   deferred, decision-free lift materialization (§3) — `tryIdDefault` promoted from heuristic arm
   to the solving rule, Phase A/B's decision-deferral retired.
4. **`Id`-erasure is provably total** (local, confluent, terminating rewrites over ground terms;
   compiler-owned FQNs, so rewrite-by-name is sanctioned — the precise line the `fold` hardcode was
   on the wrong side of), **but must be load-bearing**: a mandatory stage with a hard no-residue
   assertion and newtype belt-and-braces (§6). It paid for itself immediately on the default path,
   which already shipped `Id` allocations — hence U1 landed first.

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
(§8) — the sharpest constraint. The checker refactor is wide (§11). The v1 erasure slices were
deleted across U3/U4 as they separated; their byte-identical/flag discipline and their negative
result are what earned this decision, and the durable v1 assets — the row channel, the accounting
verifier, the `WovenValue` codegen seam, `runMain`, the shadow methodology — carried over intact.
