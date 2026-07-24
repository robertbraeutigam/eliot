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

**Uniform-carrier checker — the LIVE DEFAULT (U4-e core flip landed 2026-07-24).** U1
(Id-normalization), U2 (spike), the U3a bridge, **U4-a** (complete uniform coverage — every value
return, all PAYLOAD-slot outcomes, the carrier-slot arm, and the Generic arm route uniform), and the
**U4-e core flip** are all landed. `LangPlugin` now defaults `uniformCarrier` to `true`; the transitional
opt-*out* `--legacy-carrier` reaches the pre-uniform path only for the byte-identity / non-overlap
regression tests. The whole jvm integration suite runs uniform, green. The first flip attempt regressed
9 jvm integration tests (pinned finding 10); it was unblocked by two fixes — **refinement-channel
Id-transparency** (`RefinementChannelProcessor` normalizes `Id` on its `MonomorphicValue` input up front,
as the codegen seam does) and **nested-carrier solving** (`CarrierJoin` now unifies the carrier-stack
**prefix** pairwise, so the inner binder `G` of `AbortCarrier[AbortCarrier[IO]]` / a nested `DepCarrier`
solves) — both no-ops on legacy.

**Remaining — the U4-e close-out** (uniform is already the working default):
- **Slice 1 — DONE (2026-07-24, `3aa5b2d4`):** the vestigial `--effect-channel` flag + threading removed
  (`effectChannelKey`, the `effectChannel` ctor params on `LangProcessors`/`EffectAccountingProcessor`,
  dropped from the two accounting tests). Inert — accounting already verified unconditionally; lang 233/233,
  jvm 283/283.
- **Slice 2 — the flip is LANDED (2026-07-24); only flag/test cleanup + legacy deletion remain.** The six
  `uniformCarrier` constructor defaults are now `true`, so the raw-mono processor unit tests run uniform too.
  Getting there needed three fixes, all landed: (a) the **SemExpression-level Id-strip in the staging gate**
  (`PostDrainQuoter.stripIdMachinery`) so an erased-determined body still folds under uniform (finding 11) —
  plus a companion resolution-agnostic recognition of the abstract `Effect[Id]` form in `IdNormalizer`; (b)
  an **Id-normalize-before-assert helper** on the raw-mono unit tests that now see `pure@Id`/`runId` wrapper
  noise (`MonomorphicTypeCheckProcessorTest`, `MonomorphicTypeCheckTest`); (c) the **`checkReturnBoundary`
  injectivity fix** (finding 12) — a callee's HKT ability binder (`Container`'s `?F`, flagged as a carrier
  unfiltered) is resolved by whole-type injectivity `?F := Box`, not split as a carrier, with an arity guard
  keeping the effectful-body-under-pure-return fail-safe. `ReificationTest` and the higher-kinded-ability +
  carrier-bookkeeping suites are all green under the uniform default. **Remaining:** remove `--legacy-carrier`
  + `uniformCarrierKey` and convert the byte-identity transition tests to uniform-only (then the legacy
  machinery, §7, can be deleted).
- **Then — retire the legacy default path** (the `EffectLifter` recognition arms, the `Checker` Phase A/B
  deferral, `defaultArgSlot`/`resolveLadder`, `CheckState.ambientCarriers`); respell the synthetic main to
  `runMain`; land the **effectful-`catch`-handler stdlib delta** (pinned finding 7); turn the §6 Id-residue
  assertion into a **hard error**; the §9 Cornerstone amendment + doc/skill sweep; verify LSP/diagnostic
  rendering `Id`-free. (The old U4-d "delete default-path machinery" step folds into this close-out.) See §10
  and the pinned findings.

Per-slice history and commit trails live in the git log — this document keeps only the design, the
current state, and the path forward.

## 0. Current state

**Tree**: `master`; **uniform carriers are the live default** (the U4-e core flip) — and, since close-out
slice 2, the raw-mono processor unit tests run uniform too (the six ctor defaults are `true`; see Flags).
All gates green: `./mill lang.test` (233/233) / `./mill jvm.test` (283/283 — the **whole integration suite
runs uniform**),
HelloWorld builds+runs (`./mill examples.run jvm exe-jar examples/src/ -m HelloWorld` then
`java -jar target/HelloWorld.jar`, now compiled uniform), eliot-test 11/11 (exact command in
`eliot-test/.claude/CLAUDE.md`; args are order-strict). The **transition regression suites** compare the
two paths via `--legacy-carrier` (off) vs default-uniform (on): `UniformCarrierByteIdenticalTest` — the
whole base + nine targeted programs (pure/effect return, payload bind/capture/mismatch/doomed-bind,
carrier-slot, generic-arm, a State transformer-stack, a **nested `AbortCarrier` stack**, **two nested
`Dep` carriers**), every generated class's bytes equal — and `UniformCarrierConditionalTest`, the
non-overlap compile-succeeds gate plus the refinement-through-`Id` case. The `EffectsTwoDeps` /
`EffectsTwoThrows` / `WherePrecondition` **example files** still fail *identically on both paths*
(pre-existing multi-layer-discharge / `where`-precondition gaps unrelated to the carrier model; note the
*integration-test* two-Deps/two-Throws programs are simpler and now pass uniform). Because effect
**accounting verifies on every compile** (U4-c-2, unconditional), whole-base accounting parity — no
over-count reddens valid code — is a standing gate; rejection is `EffectAccountingWiringTest` (undeclared
`Console`/`Inf`), correct-derivation is `EffectAccountingDerivationTest`.

**Verifier**: `EffectAccountingProcessor` (the §5 post-mono verifier) is now the **sole effect
verifier** (U4-c-2). It is wired as a codegen precondition (`WovenValueProcessor` demands
`EffectAccounting.Key` via `getFactOrAbort`, U4-c-1) and verifies **unconditionally** (the
`--effect-channel` gate on verification is gone; the flag is vestigial until U4-e). The pre-mono
`EffectResidualChecker` is **deleted**; the one diagnostic it voiced that accounting cannot — "declared
pure but performs an effect", for a value whose mono *fails* — moved to the focused
`DeclaredPureChecker`, run per value mono from `TypeStackLoop.runPostDrainResolution`. The subset check
fires only for a value with an *open effect row* (a concrete-carrier `IO[Unit]` return is exempt); a
leak reddens through accounting with no flag.

**Flags**: **the uniform checker is the LIVE DEFAULT (U4-e core flip, 2026-07-24)** — and, since
close-out slice 2 (2026-07-24), **the six `uniformCarrier` constructor defaults are `true` too**
(`MonomorphicTypeCheckProcessor`, `CompilerMonomorphicTypeCheckProcessor`, `TypeStackLoop` ×2, `Checker`,
`LangProcessors`), so the raw-mono processor unit tests now run uniform as well — the transitional
"constructor defaults stay `false`" inconsistency is gone. `LangPlugin` defaults `uniformCarrier` to
`true`; `--legacy-carrier` is the transitional opt-*out* to the pre-uniform carrier-based path, kept only
to drive the byte-identity / non-overlap transition tests, and removed together with the legacy path in the
remaining close-out. A raw-mono processor unit test that must still exercise legacy passes
`uniformCarrier = false` explicitly. The vestigial `--effect-channel` flag (accounting verifies
unconditionally, so it gated nothing) was **removed** at close-out slice 1.

**Component map**:

- `monomorphize/carrier/` — `Carrier` (the lattice `Bottom`=`Id` / `Con` / `Var` + the positional,
  total `split`), `CarrierJoin` (the join solver — **live** as the default carrier handling; its equal-FQN
  `Con`-vs-`Con` arms **unify the carrier-stack prefix pairwise** (`unifyPrefixes`) so a nested stack's
  inner binder solves — the U4-e nested-carrier fix), `UniformLadder` (classify-by-expected-slot +
  decision-free `materialize`; plus `resolveGenericSlot` — the ride-aware Generic-arm resolver, live via
  the Phase-B deferred slot). Acceptance: `CarrierMechanismTest` (the four historical failure cases, the
  injectivity-theft contrasts run on the real `Unifier`, and the Generic-arm ride-up-vs-bind decision).
- `monomorphize/check/` — `Checker` (the gated `uniform*` routing + the verbatim
  `checkAgainstDefault`/`defaultArgSlot` fallbacks; arg-slot routing gated to `Platform.Runtime`),
  `UniformCarrierChecker` (the bridge: `intoCarrierHeaded`/`intoCarrierHeadedTerm`,
  `classifyExpectedSlot`, `resolveArgumentSlot`, `resolveGenericSlot` (the ride-aware Generic arm),
  `checkReturnBoundary` with the discharge-to-pure arm, `finalizeAndMaterialize`), `Checker`'s
  `deferredGenericDefault` (the verbatim default-path Generic Phase-B decision the uniform arm
  mirrors), `PostDrainQuoter` (the reification staging gate; its `stripIdMachinery` — the
  `SemExpression`-level twin of `IdNormalizer` — strips checker-inserted `Id` machinery before the gate's
  eval so an erased-determined body still folds under uniform, finding 11), `EffectLifter` (default path;
  the shared node mechanics `pureWrapNode`/`runIdNode` extracted for both paths), `DeclaredPureChecker` (the "declared pure
  but performs an effect" diagnostic — the one effect check accounting cannot voice, since its value's
  mono fails; run per value mono from `TypeStackLoop.runPostDrainResolution`), `TypeStackLoop`
  (`recordAmbientCarriers` — the checker-side ambient *heads* for the live lifter; and
  `groundAmbientCarriers` — the U4-c-0a single writer of the *full ground* ambient carriers onto
  `MonomorphicValue.ambientCarriers`, from the two spellings: open-row binders and pinned/concrete
  returns).
- `monomorphize/channel/` — `WovenValueProcessor` (the Id-normalization stage at the `WovenValue`
  seam; **also demands `EffectAccounting` via `getFactOrAbort`, the codegen precondition** that makes a
  leak block codegen), `IdNormalizer` (U1, on by default; recognises both the resolved `Effect[Id]` impl
  and — resolution-agnostically, guarded by the `Id` carrier type-arg — the *abstract* `Effect[Id]`
  combinator form, so its erasure matches `PostDrainQuoter.stripIdMachinery`), `EffectAccountingProcessor` +
  `EffectAccounting` (the §5 verifier — **the sole effect verifier, unconditional**;
  `verifySubset`/`derivedRow`/`ridesAmbient`/`openRow`), `RefinementChannelProcessor` (the
  architectural template: policy verified post-mono against the final program — and, since it reads the
  un-normalized `MonomorphicValue`, **normalizes `Id` on its input up front** the same way
  `WovenValueProcessor` does, so a `where`-precondition sees an argument's range through the uniform
  path's `Id[Int[range]]` wrapper; the template every future `MonomorphicValue` consumer follows).

**Uniform-path coverage** (now the default; byte-identical to the legacy `--legacy-carrier` path where it
succeeds, runtime track):

| construct | routes uniform? | how |
|---|---|---|
| **value RETURN boundary** (`checkAgainst`) | **yes** — pure, effect-carrier, *and* discharge-to-pure | `uniformReturnBoundary` → `checkReturnBoundary`; pure re-carried via `Id` (erased), effect-carrier passed through, a fully-discharged flex `?G[T]` body under a pure return `Id`-defaulted + `runId`-unwrapped. Gate `uniformReturnRoutable`/`uniformValueReturn`. |
| **argument → PAYLOAD slot** (`checkArgumentSlot`, a concrete non-carrier domain) | **yes** — bind-vs-capture by **payload-fit**; `uniformCaptureSlot` fully uniform (U4-a(ii)) | `uniformPayloadSlot`: if the actual's **payload genuinely fits** the domain ⇒ **bind** (`printLine(readLine)`; the compound-state `items : ?F[List[X]]` into `foldLeft`'s `List[A]`, which the **default path rejects**), pure passes (`runId`). No fit ⇒ `uniformCaptureSlot` — **all three sub-cases uniform, no `defaultArgSlot`**: a *doomed* under-applied bind (`mustLiftBeforeUnify`, always a bare-flex payload) **binds** via `uniformArgumentSlot`; else a **capture** (a carrier-stack/pinned domain — `{Abort\|G} A` ⤳ `AbortCarrier[G,A]`, `runMain`'s `IO[A]`) whole-type pass-throughs (`tryUnifyCommitting` succeeds ⤳ `Resolved`); else a **mismatch** commits directly (`commitMismatch`). A **bare-flex payload `?A`** is guarded out of "fits" (`payloadFitsDomain`). Gate `uniformPlainValueType(domain)` + `Platform.Runtime`. |
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
6. **`fold`'s bare-`A` `Generic` arm carries the ride-up-vs-bind check — DISCHARGED (U4-a(i)).** Never the
   naive pass-whole and never an eager Id-wrap (which cascades into occurs-check failures):
   `occursInValue(metaId, retType)` decides — transparent `fold` rides the carrier up, non-transparent
   `putState` bind-lifts. Landed as `UniformLadder.resolveGenericSlot` on the Phase-B deferred slot,
   byte-identical to the default `deferredGenericDefault` on both sides; a Phase-B decision keyed on the
   already-computed `retType`, never a Phase-A wrap.
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
10. **Codegen byte-identity is NOT flip-readiness — post-mono `MonomorphicValue` consumers see the
    un-erased `Id`** (the first flip attempt's lesson; both of its blockers — refinement-channel
    Id-transparency `9078e894`, `CarrierJoin` prefix-unify `9a30f815` — are fixed, §10 U4-e).
    Id-normalization runs only at the `WovenValue` codegen seam (§6), so every *other* post/mid-mono
    consumer reads `pure@Id`/`runId`-laden mono and mis-analyzes unless given the Id-normalize-first
    treatment (finding 11 is the same class at the reification gate). Gate any representation change on
    the **whole jvm.test integration suite**, never example-main codegen bytes — the byte-identity corpus
    alone missed all of this.
11. **The reification staging gate cannot fold a carrier-headed erased-determined body — FIX LANDED
    (2026-07-24): the SemExpression-level Id-strip.** Under uniform, an erased-determined runtime body
    (`name(A)` on an erased `A: Person`) is wrapped in inserted machinery (`pure@Effect[Id](runId(…))`), and
    `PostDrainQuoter.tryMaterialise` evaluated with a **bare** `semEvaluator.eval` — the machinery ref has
    no body binding, the eval stalls, read-back fails, and the fold degrades to an un-reduced runtime
    projection (`name(Person(...))` instead of `Str(Alice)`). Runtime-correct, fail-safe intact (§8 keeps
    `Type` returns un-wrapped, so `bad[A: Type]: Type = A` still errors) — a quality regression, not a
    soundness hole; an instance of the finding-10 class at the gate. **The landed fix**
    (`PostDrainQuoter.stripIdMachinery`): before the gate's eval, strip the checker-inserted `Id`
    machinery from the `SemExpression` — `runId(e) ⤳ e`, `Id(e) ⤳ e`, `pure@Effect[C](e) ⤳ e` and
    `flatMap`/`map@Effect[C](f, m) ⤳ f(m)` **iff `C` forces to `Id`** — recognised by the compiler-owned
    FQNs, the `SemExpression`-level twin of `IdNormalizer` (as `resolveAbilityRefs` is of `resolveIfAbility`).
    It touches only the gate's *eval input*, never the emitted tree; is a no-op on legacy / the compile-time
    track (no machinery inserted); and, critically, **matches the abstract ability-method FQNs**
    (`effectPureFQN` etc.), so it folds **before** ability resolution and needs no resolution or impl-body
    reachability — the finding's open question dissolves. `ReificationTest` runs uniform and passes
    **verbatim** (no re-baseline), an `Id`-normalize-before-assert helper cleaning the runtime-param wrapper
    noise. A **companion** change made `IdNormalizer` recognise the same *abstract* `Effect[Id]` combinator
    form (guarded by the `Id` carrier type-arg) so the mono-level erasure is resolution-agnostic too and the
    residue fail-safe tightens — additive, a no-op in production (always resolved) and on legacy.
    Two analysis results (validated by code reading 2026-07-24 — do not re-derive) settled *why the strip,
    not `resolveAbilityRefs`*:
    - Production DOES resolve the inserted refs (mono bodies carry resolved impl refs; the fix attempt's no-op
      in the `Effect[Id]`-less `ReificationTest` stub was an artifact) — but this is moot for the landed
      strip, which matches the abstract FQNs and runs pre-resolution.
    - `resolveAbilityRefs` in `tryMaterialise` was REJECTED: `Track.Runtime.implBindings = Map.empty` *by
      design*, so the impl body is unreachable to the gate's evaluator even in production; making it work
      would drag the escalating reducer into the runtime staging gate (compiler-pool eval re-deriving that
      `Effect[Id]` is the identity).
    **Empirical-vehicle finding (2026-07-24):** a *compilable* full-jvm surface program that reification-folds
    a non-leaf term is **not constructible** — the only foldable non-leaf shapes hit orthogonal walls
    (field projection `name(A)` → the explicit-parametric-def abstract check "Expected: Person, Actual: Type",
    identical on both paths; erased-arithmetic `N + N` → "Function not implemented"; `fold(B, …)` → the
    *runtime* `fold` is used, no compile-time fold). Reification-fold is exercised only via direct mono-key
    injection (`ReificationTest`). Since the strip is resolution-independent by construction, that + the
    jvm.test byte-identity suite are the empirical confirmation; step (2)'s literal "compile through the full
    jvm path" is subsumed, not separately achievable.
12. **A callee's HKT ability binder is flagged as an effect carrier *unfiltered*, and the uniform return
    boundary splits before whole-unifying — FIXED at the ctor-default flip (2026-07-24).**
    `CarrierKindChecker.recordIfHigherKinded` flags **every** `[F[_]]` instantiation meta as an effect carrier
    (deliberately, the callee-side notion), so `wrap(someString) : ?F[String]` from `ability Container[F[_]]`
    carries a flagged `?F`. Its own doc says a spurious flag is "harmless because the lift arms fire only after
    unification failed" — but that invariant is a **legacy-path** one: the default ladder whole-unifies first
    (`?F[String] ~ Box[String]` ⤳ `?F := Box` by injectivity), so the flag never bites. The **uniform** return
    boundary (`checkReturnBoundary`) instead *splits* the body `?F[String]` into carrier `?F` + payload `String`
    and hits the discharge-to-pure arm `(Bottom, Var(?F))`, defaulting `?F` to `Id` and unifying
    `String ~ Box[String]` → a spurious mismatch. Flipping the ctor defaults surfaced this on five
    `AbilityImplementationCheckProcessorTest` cases. **Fix:** the discharge-to-pure arm now speculatively checks
    **payload-fit first** (like the default path's `tryIdDefault`): payload fits ⇒ genuine discharge (default the
    carrier to `Id`, `runId`); payload does **not** fit ⇒ resolve by whole-type injectivity (`?F := Box`) and
    pass the body through — **but only when the declared return is a rigid application** of matching arity. The
    arity guard is the fail-safe: against a **nullary** pure return (`?F[Unit] ~ String`, an effectful lambda
    body under a rigid pure codomain — `twice(s -> printLine(s))`) whole-unify would *degenerately* solve
    `?F := const String` and silently strip the effect, so there the payload mismatch is reported instead. The
    deeper lesson (same class as findings 10/11): a legacy "harmless because unify-first" invariant does not
    transfer to the split-first uniform path — audit every such assumption when the flip surfaces it.

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

1. **U4-a — complete uniform *coverage*: DONE.** Every argument slot and value return that legitimately
   runs on the runtime track routes uniform. (i) the **`Generic` arm** — carries the **ride-up-vs-bind**
   decision (`occursInValue(metaId, retType)` → pass-through if the meta rides the result, else bind;
   never the naive `PassWhole`, pinned finding 6); (ii) the **capture / mismatch** fallbacks are reshaped
   into the uniform ladder (U4-a(ii)-0..3: the CARRIER-SLOT arm's effectful actual pass-joins, the
   PAYLOAD-slot no-fit *capture* whole-type pass-throughs, the *mismatch* leaf commits via `commitMismatch`,
   the *doomed under-applied bind* binds via `uniformArgumentSlot`) — so `uniformCaptureSlot` has **no
   `defaultArgSlot` hand-off**. The only remaining `defaultArgSlot` touches under uniform are
   `uniformCarrierSlot`'s pure-wrap-fails mismatch edge and the Generic arm's Phase-A deferral *marker* —
   both cosmetic (the deferral's decision is uniform in Phase B), retired when the legacy path is deleted
   at the close-out. *By-design defaults, permanent* (§8): the compile-time track,
   `VType`/guard/calc-return/W3, and function/polytype (`VPi`/`VLam`) returns (pinned finding 2). The whole
   nested-carrier surface (multi-layer transformer stacks) works via the `CarrierJoin` prefix-unify fix.
   (Per-slice trails — U4-a(i)-0/1 and U4-a(ii)-0..3, each landed byte-identical with a pinning program in
   `UniformCarrierByteIdenticalTest` — live in the git log.)

2. **U4-b — Bundle A: LANDED (2026-07-24).** The `--effect-channel` erasure path deleted;
   `effectChannel` threads to `EffectAccountingProcessor` only (removed at U4-e);
   `contributedEffects` re-pointed to the resolved-impl view. Two mechanisms it established, used
   by U4-c: an effect ability is discriminated from a first-order impl by the **ability marker's**
   HKT carrier binder (never the impl marker — a concrete `implement Inf[IO]` marker has no
   binder), and the ability's module is `ref.moduleName` (effect abilities and their instances are
   colocated in `eliot.effect`; the marker lookup succeeding there confirms it).

3. **U4-c — swap the verifier: DONE (2026-07-24).** The explicit-interface course — principle, mechanism,
   and rejected alternatives all in §5; per-step trails (0a–0d, 1, 2) in the git log. Landed shape: the
   forwarded `MonomorphicValue.ambientCarriers` (single writer `TypeStackLoop.groundAmbientCarriers`, no
   default — a silent `Set.empty` is the under-count direction), "declared" read off the value's
   `OperatorResolvedValue` constraints (the "carrier-machinery-impl exception" deleted as a concept;
   `channelDeclaredEffects` survives only as LSP rendering), the pure exact-equality ride test, the
   derivation gated through it with fail-safe `getFactOrAbort` reads (machinery + match-family eliminators
   excluded up front; the effect-vs-first-order marker lookup deleted — a first-order impl has an empty
   ambient, so the ride test filters it). Wired as a codegen precondition (`WovenValueProcessor` demands
   `EffectAccounting.Key`), verifying unconditionally; one real over-count fixed on the way (the subset
   check fires only for an open effect row — a concrete-carrier return's chosen carrier permits its
   effects). Then `EffectResidualChecker` was deleted, its one un-absorbable diagnostic ("declared pure
   but performs an effect", a value whose mono *fails*) extracted to the focused `DeclaredPureChecker`.
   Locked by `EffectAccountingRideTest` (the 8-case ride matrix), `EffectAccountingDerivationTest`
   (correct rows, no over-count), and `EffectAccountingWiringTest` (undeclared `Console`/`Inf` redden).

4. **U4-d — delete the default-path machinery: folded into the U4-e close-out** (the core flip landed
   with the legacy path still present behind `--legacy-carrier`). The §7 flip-deletion list applies.
   Beyond deletion: the synthetic main respells to `runMain(<user main>)`, making the run boundary
   nominal (§9); the `MonomorphicValue.ambientCarriers` writer switches its source from
   `CheckState.ambientCarriers` to the uniform checker's carrier bookkeeping (the fact contract is
   unchanged); friendly voicing for the `AbilityResolver`-killed control-effect leaks stays optional
   polish. (The "declared pure" diagnostic already moved to `DeclaredPureChecker` at U4-c.)

5. **U4-e — make it the default + close out. CORE FLIP LANDED (2026-07-24).** The live default is now
   uniform (`LangPlugin` `uniformCarrierKey` default `true`, opt-*out* `--legacy-carrier`; constructor
   defaults stay `false` so raw-mono processor units observe the pre-uniform representation during the
   transition). Full gate green under uniform-as-default: **lang.test 233/233, jvm.test 283/283
   (the whole integration suite now runs uniform), HelloWorld, eliot-test 11/11.** Got here by fixing the
   two blockers pinned finding 10 identified (both landed, both no-ops on legacy):
   - **refinement-channel Id-transparency** (`9078e894`): normalize `Id` on the channel's `MonomorphicValue`
     input up front (as `WovenValueProcessor` does), so a `where`-precondition sees the argument range
     through `Id[Int[range]]`.
   - **nested-carrier solving** (`9a30f815`): `CarrierJoin`'s equal-FQN `Con`-vs-`Con` arms dropped the
     carrier stack **prefix**, leaving the inner binder `G` unsolved at `AbortCarrier[AbortCarrier[IO]]` /
     `DepCarrier[X1, DepCarrier[X2, IO]]`; the fix unifies the prefixes pairwise (restoring what legacy's
     full structural unify did), carrier identity still FQN-only so no theft. This one fix resolved all six
     nested-stack failures (two-Throws, two-Deps, the compiler-track constant-fold — all nested `if`).
   Nested-carrier programs are pinned byte-identical in `UniformCarrierByteIdenticalTest`; the refinement
   case in `UniformCarrierConditionalTest`.

   **Close-out slice 1 — DONE (2026-07-24):** the vestigial `--effect-channel` flag and its threading
   removed (`effectChannelKey`, the `effectChannel` constructor params on
   `LangProcessors`/`EffectAccountingProcessor`, `--effect-channel` dropped from the two accounting tests).
   Accounting already verified unconditionally, so this is inert; lang 233/233 + jvm green.

   **Close-out remaining:** remove `--legacy-carrier` and its threading (and flip
   the constructor defaults to `true`, updating the ~13 raw-mono processor unit tests to the uniform
   representation, retiring the legacy path — the reification-folding fix below is now **landed**, so the
   flip is unblocked); land the **effectful-`catch`-handler stdlib delta**
   atomically (pinned finding 7 — the join solver is now the default, so the stacking cannot occur;
   acceptance: `failUnit catch (err -> printLine(err))` runs and `EffectsThrow` stays green); turn the §6
   Id-residue assertion into a **hard error**; the §9 Cornerstone amendment + doc/skill sweep (`eliot-code`
   global skill, `eliot-layers`, CLAUDE.md effect + monomorphize sections); verify LSP/diagnostic rendering
   `Id`-free.

   **Close-out slice 2 — the folding fix is LANDED (2026-07-24, pinned finding 11); the flip remains.**
   Flipping the six `uniformCarrier: Boolean = false` constructor defaults to `true`
   (`MonomorphicTypeCheckProcessor`, `CompilerMonomorphicTypeCheckProcessor`, `TypeStackLoop` ×2,
   `Checker`, `LangProcessors`) makes the raw-mono processor units run uniform, surfacing **15 unit-test
   failures** in four suites. What landed and what remains:

   - **The folding regression — FIXED.** `ReificationTest` (5) + the `name(A)` case of
     `MonomorphicTypeCheckProcessorTest`. The fix — `PostDrainQuoter.stripIdMachinery`, run before the
     gate's eval — strips the checker-inserted `Id` machinery from the `SemExpression`: `runId(e) ⤳ e`,
     `Id(e) ⤳ e`, `pure@Effect[C](e) ⤳ e` **iff `C` forces to `Id`**, `flatMap`/`map@Effect[C](f, m) ⤳
     f(m)` under the same guard — recognised by the compiler-owned FQNs (the `SemExpression`-level twin of
     `IdNormalizer`, as `resolveAbilityRefs` is of `resolveIfAbility`; §6 sanctions rewrite-by-name for
     checker-inserted machinery). It matches the **abstract** ability-method FQNs, so it folds *before*
     resolution — resolution- and reachability-independent (the finding-11 open question dissolves), and
     it implements §9's carrier-based fold criterion (a non-`Id` carrier fails the guard, stays
     observation-ordered, never folds). Touches only the gate's *eval input*, never the emitted tree;
     fail-safe unchanged (a missed shape stalls the eval → the gate declines → structural quote, never a
     bad emit). Full detail + the empirical-vehicle finding in pinned finding 11.
   - **Id-wrapping noise — helper landed for `ReificationTest`.** The runtime-param `ReificationTest`
     cases (`keepX`, `mixed`) take the structural-quote path and carry `pure@Id(runId(…))` wrapper noise;
     an **Id-normalize-before-assert helper** (`IdNormalizer.eraseIdInBody(normalizeValue(…))`, exactly as
     `WovenValueProcessor` does) cleans it, so `ReificationTest`'s baselines (`Str(Alice)`, …) hold
     **verbatim**. Because the stub ships no `Effect[Id]` instance the inserted `pure` never resolves to
     the impl, so a **companion** change made `IdNormalizer` recognise the *abstract* `Effect[Id]`
     combinator form too (guarded by the `Id` carrier type-arg) — its erasure is now resolution-agnostic,
     matching `stripIdMachinery`, and the residue fail-safe tightens. `ReificationTest` runs under
     `LangProcessors(uniformCarrier = true)`; the same helper still needs applying to the other
     `MonomorphicTypeCheckProcessorTest` (4) whole-structure cases (part of the remaining sequence).
   - **Not yet characterised: higher-kinded-ability resolution (5) + `CarrierBookkeepingTest`
     "carrier-typed storage slot" (1).** Characterise each before any re-baselining.

   **Sequenced steps for slice 2** (✓ = landed 2026-07-24): (1) ✓ implement the Id-strip; `ReificationTest`
   passes **verbatim** under uniform; (2) ✓ *equivalent-of* — the literal "compile a reification-style program
   through the full jvm path and confirm the folded constant" is **not achievable** (no compilable full-jvm
   surface program reification-folds a non-leaf term — pinned finding 11's empirical-vehicle finding),
   subsumed by the resolution-independent `ReificationTest` + the jvm.test byte-identity suite; (3) ✓ Id-normalize
   test helper on the raw-mono unit tests (`MonomorphicTypeCheckProcessorTest`'s 4 whole-structure cases via a
   `normalizedBody` helper, `MonomorphicTypeCheckTest`'s `liftedBody` — the discharge-helper `pure@Id` noise
   erased before the combinator assert); (4) ✓ flip the six constructor defaults to `true`; (5) ✓ characterise +
   fix the higher-kinded-ability (5) and carrier-bookkeeping (1) failures — the carrier-bookkeeping one was pure
   `pure@Id` noise (helper), the five higher-kinded ones a **real checker bug** (pinned finding 12); (6)
   **remaining** — remove `--legacy-carrier` + `uniformCarrierKey`, convert the byte-identity transition tests to
   uniform-only. Only after that can the legacy machinery be deleted (the §7 list). **Landed state (flip
   complete):** `PostDrainQuoter.stripIdMachinery` + `IdNormalizer` abstract-form recognition + the six ctor
   defaults `true` + the `checkReturnBoundary` injectivity fix (finding 12) + the test-helper adaptations. Gate
   green: lang 233/233, jvm 283/283, HelloWorld, eliot-test 11/11.

   (The first flip attempt's two failure classes and their fixes — refinement-channel Id-transparency,
   nested-carrier prefix-unify — are summarised in finding 10 and the core-flip paragraph above; forensic
   detail in the git log.)

### U5 — follow-ups unlocked

Row-bearing diagnostics everywhere; the evaluation-order decision (resolved-argument order vs
source order — §12); `Suspended` for first-class platform actions; the MCU lowering (§6) when
that backend activates; reduce-and-reify's carrier-based observation ordering (§9); the reify
legality check (§5 check 2) on the ride-test foundation.

## 11. Risks

- **Join-solver correctness at the catch-handler's first live use**: deferred lift materialization
  must be total, and an ability-constrained carrier meta must never default to `Id` (pinned
  finding 4). A missed insertion is a loud type/codegen error, not silence — but budget for the tail.
- **Accounting under-count hazards** (the fail-safe direction — a leak passing silently): the
  countermeasures are standing (the rejection tests; abort-on-missing reads). Over-count is
  self-announcing — a red compile on valid code.
- **`Id`/carriers leaking into user-facing text**: diagnostics and LSP rendering must stay
  payload/row vocabulary — a close-out gate (§9).
- **Per-consumer Id-transparency is a recurring tax** (findings 10/11 are the same class): until the
  §6 no-residue assertion is a hard error, any new consumer of `MonomorphicValue` or of mid-mono
  `SemExpression`s must get the Id-normalize-first treatment.

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
