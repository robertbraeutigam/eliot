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
confirmed Id-free rendering + constant-factor perf. U3-0a LANDED (2026-07-23): the v1 weaver +
entry-point rework + config keys deleted (`WovenValueProcessor` is now just the Id-normalizer);
default path byte-identical. U3-0b finding (2026-07-23): the `desugarChannel` deletion is **not
separable before U3a** — it couples through the kept `EffectAccounting` verifier to carrier-bound
monomorphization of effect-polymorphic values, so the §7 "delete at U3 start" ordering is corrected
to fold that deletion into U3a/U3c (§7, §10 U3-0b). U3a-1 LANDED (2026-07-23): the U2 spike mechanism
is **productionised onto the real `SemValue`/`Unifier` domain** as a new `monomorphize/carrier/`
package (`Carrier` lattice + `split`, `CarrierJoin` join solver, `UniformLadder` classify-by-expected-slot
ladder + deferred lift materialization), **not yet wired into the ~1000-line `Checker`** — so the default
path stays byte-identical. Its 27-case regression suite (`CarrierMechanismTest`) is U3a's acceptance
suite in real types, and the theft-contrast cases run the *real* `Unifier` to show the exact injectivity
theft that splitting the carrier head off first avoids. U3a-2a LANDED (2026-07-23): the **checker-side
bridge** `check/UniformCarrierChecker` — the §12-Q1 decision resolved to **check-time carrier-wrapping**
(`intoCarrierHeaded`: a pure `T` ⤳ `Id[T]`, `VType` never wrapped), the expected-slot classifier reading
the value's real carrier bookkeeping (`ambientCarriers`/`carrierRoles` via the reused `effectCarrierSplit`),
and the CheckIO-threaded ladder + boundary finalize/materialize — built and **unit-tested in isolation**
(`UniformCarrierCheckerTest`, like `EffectLifterTest`), **not yet constructed/called by the `Checker`** so the
default path stays byte-identical (and clear of the `desugarChannel`/accounting knot). U3a-2b(i) LANDED
(2026-07-23): the bridge's **node mechanics** — `EffectLifter.pureWrapNode`/`runIdNode` **extracted** (a
behavior-preserving refactor of `tryPureWrap`/`tryIdDefault`) and reused (reshape, not rebuild) by
`UniformCarrierChecker.resolveArgumentSlot` (the node-producing successor of the checker's `checkArgumentSlot`:
classify ⤳ ladder ⤳ build the slot node — Generic pass-through / CarrierSlot pass-join with a pure actual
re-carried by `carrierSlotLift` = `pure@Effect[?G](runId(actual))` / PayloadSlot bind via `EffectLifter.Bind`),
returning a `UniformSlotOutcome` that mirrors the checker's `SlotOutcome`. U3a-2b(i+) LANDED (2026-07-23): the
**bridge surface is now complete** — `intoCarrierHeadedTerm` (the eager term-level dual: a pure term's value
`expr:T` ⤳ `pure@Effect[Id](expr):Id[T]`) and `checkReturnBoundary` (the uniform `checkAgainst`: join the body's
carrier to the declared return's, pure body re-carried by `carrierSlotLift`, erased at `Id`) added, so the flip is
pure wiring. Still isolation-tested, still not called by the `Checker` — default path byte-identical. U3a-2b(ii)
**infrastructure LANDED (2026-07-23)**: the transitional **`--uniform-carrier` gate** (the recommended de-risking,
below) is threaded end-to-end — `LangPlugin` CLI flag + `uniformCarrierKey` → `LangProcessors` → both mono checkers
(runtime + compiler) → `TypeStackLoop` → `Checker`, which now **constructs `UniformCarrierChecker`** beside
`EffectLifter` (`new UniformCarrierChecker(force, lifter.effectCarrierSplit)`) — unconditionally but never called while
the flag is off, so the default path is byte-identical (verified). U3a-2b(ii) **wiring slice 1 LANDED (2026-07-23,
`8fadd27f`)**: the **return boundary** is the first spine point routed through the bridge. Under `--uniform-carrier`,
`checkAgainst` routes the *plain pure value* return case through `intoCarrierHeadedTerm` + `checkReturnBoundary`
(inserting `pure@Id`/`runId` the Id-normalization stage then erases → byte-identical bytecode); every other shape
falls back to the verbatim default ladder (`checkAgainstDefault`). The gate (`uniformReturnRoutable`) is deliberately
tight — it routes only when *both* the declared return and the body's inferred type are plain, **non-carrier-headed**
`VTopDef` value types (`uniformPlainValueType`) *and* the payload already fits by pure definitional equality (a
non-committing speculative unify — pure definitional equality is exactly the right test now that `Int == Int` and its
bounds live in the separate refinement channel, so `Int`-returning pure values *do* route). This falls back for:
effect-carrier-headed returns (**routing one would self-solve its carrier meta `?F := ?F` → infinite loop** — a concrete
finding), the guard/calc-return/W3 discharge and the §8 type-level boundary (all `VType`/carrier-headed), function/polytype
returns, and any genuine definitional-equality *miss* (an ordinary mismatch the default path reports — there is **no**
`Coerce`/widening machinery to reconcile a near-miss; it was deleted when `Int` became nullary, bounds-in-channel).
Durable gate: `UniformCarrierByteIdenticalTest` (jvm.test) compiles a pure-value program **+ the whole base
layer** with the flag off and on and asserts every generated class's bytes match — the *entire* base compiles
byte-identically under the flag (every pure value return in lang/stdlib/jvm routes through the uniform boundary and
Id-normalizes away). **SINCE THEN (see §0 "Uniform-path coverage NOW" for the current, authoritative state):** the
argument→payload-slot case (pure args pass, effectful args bind — `527af90a`/`16a432f6`), **every value return**
including effect-carrier-headed (`?F[Unit]`/`IO[Unit]`, runtime track — `b35bf80c`), and the **whole conditional
surface** (`if`/`else`/`fold` — `IfDemo` byte-identical, `23eb785a`: return-boundary discharge-to-pure + discharger
capture-vs-bind + `Platform.Runtime` arg-slot gate), the **`if` CarrierSlot arm** (`5864f95f`: a pure `H[X]` actual
pure-wraps before the default ladder's stealing equal-arity unify — `if(c, None) else Some(x)` compiles where the default
path rejects it), and the **payload slot decided by payload-fit** (`ba208c48`: an effectful value whose payload fits a
data slot **binds** — the compound-state `items : {Console} List[String]` into `foldLeft`'s `List[A]`, another default
rejection) now route uniform, byte-identical where the default path succeeds, with **two non-overlap wins** the default
rejects; `CarrierJoin` guards the self-join (`ead5d631`). **NEXT: fold the effectful-`catch`-handler into U4; the
`Generic` arm is DROPPED (findings, 2026-07-23).** Two candidates were tried this session and both re-scoped (details in
the "Next" subsection). `fold`'s bare-`A` `Generic` arm is **not** a standalone byte-identical slice — routing
bare-flex-meta domains through `PassWhole` fires for *every* generic argument in the base (`Some`/`Pair`/`identity`/`min`,
not just `fold`), Id-wrapping them into occurs-check failures; without the Id-wrap it is vacuous (`?A := payload` = the
default) and omits the ride-up-vs-bind check Phase A/B makes — so it is folded into the flip, not landed early. The
effectful-`catch`-handler was tried empirically (correcting the earlier claim): the stdlib delta (`onError: E => G[A]`,
`flatMap`+`pure` body) **does enable effectful handlers** (`failUnit catch (err -> printLine(err))` compiles+runs) **and
is backward-compatible for a single discharger**, but it **requires a user-facing stdlib signature change**, which — since
stdlib source is not flag-gated — hits **both** paths and **regresses one default-path shape**: two-or-more dischargers
with *pure* handlers sequenced in a block (`EffectsThrow`/`MinCatch2`), where the pure handler's forced codomain lift
mis-unifies the shared ambient into a stacked `ThrowCarrier` (bogus `Throw[String, IO]` at jvm `Throw.els:54`). That
stacking is the premature-carrier-commitment class the uniform `CarrierJoin` eliminates, so the delta can only land when
the uniform path is the **default** — **at/after U4**. It is therefore **not** a pre-flip `--uniform-carrier` win like the
other three cases (those were pure-checker fixes with no stdlib change). **The single-slot `if` arm needed no join and
no `finalize`** — the earlier "ability-constrained-carrier / `finalize`-defaults-to-`Id`" crux hypothesis was **wrong**;
the real cause of the `if(f,"+")` `VerifyError` trial was the `carrierSlotLift` **double-wrap** (`pure(runId(pure@Id(…)))`
mis-erased), fixed by reusing the clean single `tryPureWrap` node. **Wiring finding
(2026-07-23):** `intoCarrierHeadedTerm` (and every heading site)
must fire on *terminal value* leaves only, **never a function-typed (`VPi`) reference** — a `printLine` leaf
(`String → …`) is not `VType` and not carrier-headed, so the bridge would wrongly wrap it `Id[String → …]`; only a
fully-applied result carrier-heads. This is why the flip stays a coupled bundle even under `--uniform-carrier` (the
points must expect/produce carrier-headed judgments together) and is sliced further per-shape rather than landed at
once. On the
`--effect-channel` gate the flip remains a **non-partitionable bundle** (finding, §0/§10): `desugarChannel` makes
effectful programs *look pure*, so no per-value gate can grow the uniform path under `--effect-channel` without
disturbing the kept effect-blind tests — that flip must land `desugarChannel`-removal + uniform-checker +
`AbilityResolver`-abstain-removal + `EffectAccounting` re-point + `EffectChannelDesugarTest` delete +
`EffectAccountingTest`→jvm together (kept green by the uniform checker binding the effect-poly value's carrier).
Reframing: the default checker *already* compiles effectful carrier-desugared programs, so the uniform checker's
job is to **match** it (byte-identical gate). The **transitional `--uniform-carrier` gate** distinct from
`--effect-channel` lets the uniform checker grow on default carrier-desugared input, compared byte-identical,
decoupled from the `desugarChannel`/accounting knot until U4 unifies the flags. Commit trail (on `master`): U2 spike
`6fc17e99`, U3-0a `71c39704`, U3-sequencing correction `4ad8b333`, U3a-1 `5f08a12a`, U3a-2a `455575bb`, U3a-2b(i)
`e1445031`, U3a-2b(i+) `ec46b7fa`, U3a-2b(ii)-infra `dd61f027`, U3a-2b(ii)-return-boundary `8fadd27f`, U3a-2b(ii)-arg-slot `527af90a`, U3a-2b(ii)-effectful-arg `16a432f6`, U3a-2b(ii)-self-join-guard `ead5d631`, U3a-2b(ii)-effect-return `b35bf80c`, U3a-2b(ii)-CarrierSlot-arm `5864f95f`, U3a-2b(ii)-compound-state `ba208c48`; then **U4**: Generic-arm-dropped/catch-handler finding `3fa0dbec`, catch-handler correction `fb58c830`, U4 execution plan `c0654a3f`, **U4-c shadow deletion `26ce08b2` (code, green)**, Bundle A re-point recipe `f7767998`, U4-c wiring hook + blast-radius `20a0b88b`; the tree is green (`lang.test`/`jvm.test`, HelloWorld,
eliot-test 11/11).** The §13 fork
raised during the Phase-3
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
remaining landed effect-blind slices (v1 Phase 3: `desugarChannel`, the `AbilityResolver` abstain, the
`AbilityImplementationCheck` relaxation) are **deferred for deletion into U3a/U3c** (§0, §7, §10
U3-0b — not U3 start, as first planned); the flag is retained as the gate under which the uniform
checker grows.

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
| **Phase 3 foundation** — effect-blind desugar (`desugarChannel`: strip open rows, carrier-erase user effect-ability methods), `AbilityResolver` abstain, ability↔impl conformance relaxation | **SUPERSEDED — delete DEFERRED into U3a/U3c** (was "U3 start"; corrected by the U3-0b finding, §10 U3-0b). These implement erasure, the rejected foundation, but the deletion couples to the kept `EffectAccounting` verifier — an effect-polymorphic value cannot monomorphize carrier-free once `desugarChannel` is gone, so it needs the uniform checker's carrier-bound mono first. Still flag-gated + dormant until then. |
| **§6 weaver monadification** — `WovenValueProcessor.weave` (bind/`pure` insertion, `sequenceSpine`, `weaveBlock`, `peelAndWeave`, `pureWrap`, the `isLazyConditionalHead` `fold`/`if` FQN hardcode) | **DELETED (U3-0a, 2026-07-23).** Under uniform checking mono output is already monadic and resolved; there is nothing to weave. The whole weaver branch + both stopgaps (FQN hardcode, lambda-peeling) are gone; `WovenValueProcessor` is now just the Id-normalization stage. Reachable only via `baseCarrier = Some`, so the deletion was inert for the default path. |
| **§6 codegen redirect** — `WovenValue` fact + `used`/`uncurry`/jvm reading `WovenValue.Key` instead of `MonomorphicValue.Key` | **KEEP.** The post-mono seam between checking and codegen — exactly where the **Id-normalization stage** (§6) plugs in. The redirect investment is preserved. |
| **§6 entry-point rework** — jvm `runMain[A](io: IO[A]): A` in `IO.els`; synthetic main as bare ref under the flag; weaver-built run boundary; `LangPlugin.baseCarrierKey` / `entryPointKey` config | **SPLIT — weaver-boundary + config DELETED (U3-0a, 2026-07-23).** `runMain` itself: **KEPT** (ordinary Eliot, useful on both paths). The weaver-built boundary (`weaveEntry`/`runBoundary`), both config keys + their `JvmPlugin.withBaseCarrier`/`baseCarrierFQN` setters, and `SyntheticMainSourceProcessor`'s bare-ref branch are gone; the synthetic main is the sole `carrierMainSource` (`apply(block(main), unit)`) until U3b spells `runMain(<user main>)`. |
| `--effect-channel` flag + `LangProcessors(effectChannel=…)` threading | **KEEP the gate, replace its meaning.** As of U3-0a it is still threaded to `CoreProcessor`/`AbilityImplementationCheckProcessor`/the checker chain (`MonomorphicTypeCheckProcessor` → `TypeStackLoop` → `Checker` → `AbilityResolver`) + `EffectAccountingProcessor`, and still switches the surviving effect-blind behaviors (desugar/abstain/relaxation) on. Those come off in U3a/U3c; the uniform checker then grows under the same flag. |
| Tests: `EffectChannelDesugarTest`, `WovenValueTest` monadification cases | `WovenValueTest`: **DELETED (U3-0a)** with the weaver. `EffectChannelDesugarTest`: **deferred to U3a/U3c** — deleted with `desugarChannel`. `EffectAccountingTest`: kept, re-pointed with the derivation in U3c — and it must **relocate off the lang track** (which has no runtime carrier — see §10 U3-0b) to a track with a concrete carrier (jvm), since an effect-polymorphic `main` cannot monomorphize on the carrier path without one. |

Until U3 lands, the default path remains byte-identical to today; nothing user-visible changes
before the U4 flip.

### Handover snapshot (cold-start read this first)

**Where the tree is:** `master`, in **U4 (the flip)**, **Bundle A (U4-b) LANDED (2026-07-24)** — the superseded
`--effect-channel` erasure path is deleted and `EffectAccounting.contributedEffects` is re-pointed to the resolved-impl
(`AbilityImplementation`) view. **U4-c was BLOCKED, now UNBLOCKED IN DESIGN (investigation, 2026-07-24, below):** the
row-based `EffectAccounting` cannot replace `EffectResidualChecker` without a **run/discharge-subtraction** slice — but
that slice is buildable post-mono as a **positional key↔binder fact join** (the mono key retains the ambient-carrier
binding; the naive derivation was discarding it), now fully specified as **U4-c-0** (§10 U4-c). `EffectResidualChecker`
stays the live verifier until U4-c-0 lands and passes the parity gate. **U3a is complete as far as it goes pre-flip** (the two remaining pre-flip arm items
were resolved to findings — see below). Green everywhere: `./mill lang.test`,
`./mill jvm.test` (incl. `UniformCarrierByteIdenticalTest` conditional-surface case + `UniformCarrierConditionalTest` —
the non-overlap compile-succeeds gate: `if(c, None) else Some(x)` *and* an effectful list into `foldLeft`'s `List[A]`,
both rejected off, accepted on), HelloWorld builds+runs (`./mill examples.run jvm exe-jar examples/src/ -m HelloWorld`
then `java -jar target/HelloWorld.jar`), and eliot-test 11/11 (build `-m eliot.test.Runner` over
`/home/robert/personal/eliot-test/{src,test}` — exact command in `eliot-test/.claude/CLAUDE.md`, args are order-strict —
then run `Runner.jar`). The default path is byte-identical to pre-U1; `--effect-channel` is dormant.

The transitional `--uniform-carrier` gate is **live for every *value* return** (pure re-carried via `Id`, effect-carrier
passed through, discharge-to-pure `Id`-defaulted + `runId`-unwrapped — runtime track), the **whole
argument→payload-slot case** (bind-vs-capture by **payload-fit**, incl. the compound-state effectful-value-into-a-data-slot
the default path rejects; a carrier-stack domain or a bare-flex payload captures), the **whole conditional surface**
(`if`/`else`/`fold` — `IfDemo` byte-identical), and the `if` **CarrierSlot arm** (`value: {Abort} T` — a pure `H[X]`
actual pure-wraps before the default ladder's stealing equal-arity unify, fixing `if(c, None) else Some(x)`) — all
Id-normalized back to byte-identical where the default path already succeeds, plus **two non-overlap wins** the default
path rejects. The compile-time track falls back to the default path (arg-slot routing is `Platform.Runtime`-gated).
`CarrierJoin`/`finalizeAndMaterialize` are built but **not yet called** by the checker (the single-slot `if` arm needs
no join).

**What resolved the two pre-flip arm items (this session, 2026-07-24 — both re-scoped, no code):**
- **`fold`'s bare-`A` `Generic` arm — DROPPED as a standalone slice.** Routing bare-flex-meta domains through the uniform
  `PassWhole` ladder fires for *every* generic argument in the base (`Some`/`Pair`/`identity`/`min`, not just `fold` — the
  design forbids naming `fold`) and Id-wraps them into occurs-check failures; without the Id-wrap it is vacuous
  (`?A := payload` = the default) *and* omits the ride-up-vs-bind check Phase A/B makes (`occursInValue`). Folded into the
  U4 flip's Generic arm, not landed early. (Full write-up: §0 "Next", §10.)
- **The effectful-`catch`-handler — GATED ON U4** (corrected from an earlier malformed-probe claim). The stdlib delta
  (`onError: E => G[A]`, `flatMap`+`pure` body) **works** — it enables effectful handlers (`failUnit catch (err ->
  printLine(err))` runs) and is backward-compatible for a single discharger — but a *user-facing stdlib signature change*
  is not flag-gated, so it hits both paths and regresses two-or-more sequenced pure-handler catches (`EffectsThrow`) via
  ambient carrier-stacking on the *default* path. It can only land once the uniform `CarrierJoin` is the default carrier
  handling → **moved into the U4 milestone** (land the delta atomically with the flip).

**Bundle A LANDED (U4-b, 2026-07-24, this session).** The deletions + flag-threading simplification + the
`contributedEffects` re-point all landed, byte-identical on the default path (`EffectAccounting` stays behind its
`--effect-channel` gate, demand-driven / test-only, so a real compile never runs it — `EffectResidualChecker` is still
the sole live verifier). What landed:
- **Deleted the `--effect-channel` erasure path** (all compile-clean, all understood): `EffectSugarDesugarer.desugarChannel`
  + `eraseAbilityCarrier`/`abilityCarrierName`/`isHigherKindedBinder`/`eraseCarrierApplications`, and the `rewrite`
  `stripOpen` parameter + its arm (`desugar(function)` is now just the carrier desugar; `desugar(data)` dropped the unused
  flag); `AbilityResolver`'s effect-abstain (`isEffectAbilityRef` + the `filterA`) + its `effectChannel` param;
  `AbilityImplementationCheckProcessor`'s conformance relaxation (`isUserEffectAbility` + the skip) + its param;
  `EffectChannelDesugarTest`; the old lang-track `EffectAccountingTest` (its premise — effect-blind abstract refs — is
  gone).
- **Flag-threading simplification:** `effectChannel` came off `CoreProcessor`/`AbilityImplementationCheckProcessor`/both
  mono processors/`TypeStackLoop`/`Checker`/`AbilityResolver`; it now threads **`LangPlugin` → `LangProcessors` →
  `EffectAccountingProcessor` only** (removal is U4-e). `uniformCarrier` threading is untouched.
- **The re-point** (`EffectAccountingProcessor.contributedEffects`, fail-safe-critical): recognises an effect op as a
  `Qualifier.AbilityImplementation(name, _)` reference (the carrier path resolves every ability ref to its impl), an
  effect ability discriminated from a first-order impl (`Show`/`Eq`/`==`, the synthetic `PatternMatch`/`TypeMatch`/`Meta`
  impls) by the **ability marker**'s HKT carrier binder — read on the ability marker (a concrete `implement Inf[IO]` impl
  marker has *no* HKT binder, only the ability does), via `isEffectAbility(ref.moduleName, name)`. **Module resolved to
  `ref.moduleName`** (not a separate ability-module lookup): effect-ability instances are colocated with their ability —
  a carrier-generic `implement[F ~ E] Ability[F]` can only live in the ability's module, and a concrete `implement Inf[IO]`
  is placed there too (verified: every effect ability + its instances are in `eliot.effect`) — so the impl method's module
  *is* the ability's, confirmed by the ability-marker lookup succeeding there. This mirrors `EffectResidualChecker`'s own
  `AbilityFQN(vfqn.moduleName, name)`. The `Qualifier.Ability` arm is kept for a constraint-covered method the checker
  left abstract.
- **Re-point validated** by the whole-base probe (below): a Console program's **user `main` accounts as `{Console}`** at
  its carrier-bound mono key — correct.

**U4-c blocker — found 2026-07-24, RESOLVED IN DESIGN the same day (do not retry the *naive* wiring; the correct
mechanism is U4-c-0, §10).** The plan assumed the row-based `EffectAccounting` could become the sole verifier once
wired + re-pointed (§10 U4-c: "wire a demand … then delete `EffectResidualChecker`"). The naive derivation cannot,
because **it walks the *fully monomorphic* body as a bare reference union, where the ambient-vs-concrete carrier
distinction is not visible** — the exact information the residual checker uses for **run/discharge subtraction**. Two
over-count classes make wiring it whole-base reject valid code:
1. **Run-via-concrete-carrier.** The synthesized entry `def main: Unit = apply(block(User::main), unit)` runs `Console`
   on the concrete `IO` carrier but declares no row. `EffectResidualChecker` accepts it (its `checkDeclaredPure` fires
   only on a *committed unifier mismatch*, and `IO` absorbs the effect cleanly — no mismatch); `EffectAccounting` derives
   `{Console} ⊄ {} = ` a false leak. **Empirically observed:** wiring the demand made HelloWorld fail with "This value
   performs the effect 'Console' but does not declare it" *on the synthetic main* (the user `main` accounted fine).
2. **Discharge.** A discharged `raise`/`get` resolves to the effect op on an *inner transformer carrier*
   (`Either`/`AbortCarrier`/`StateCarrier`), not the value's ambient `IO`. `EffectResidualChecker` drops it via its
   "rides the ambient carrier" filter (a type arg forced to an ambient head); post-mono every carrier is concrete, so
   `EffectAccounting` reads the bare `AbilityImplementation` and over-counts the discharged effect.

   The residual checker's discharge/run awareness rests on **`CheckState.ambientCarriers` + `unifier.errors`** — checker
   state that does not survive to the post-mono body. The plan's own rationale for keeping the row-based accounting was
   that it *avoids* `ambientCarriers` (which U4-d deletes); the finding was that avoiding it is exactly what makes the
   *naive* derivation unable to subtract run/discharge.

   **Investigation finding (2026-07-24, later the same session) — the blocker's generalization was wrong; the
   subtraction is a positional fact join over information that DOES survive monomorphization.** The distinction is
   erased *from the body walk*, not from the mono facts:
   - `TypeStackLoop.establishSignature` binds `typeArguments.lift(i)` against `binders.zipWithIndex`, so
     **`MonomorphicValue.typeArguments` is positionally aligned with the value's signature binders**. The value's
     **ambient carrier set = the key's type arguments at its `carrierBinders(view) ∩ paramConstraints` positions**
     (the identical filter `EffectResidualChecker.check` uses, read off the value's own `OperatorResolvedValue` — a
     front-end fact alive post-mono). Exact ground values, no shape-parsing of the return type.
   - Every body reference is `MonomorphicValueReference(vfqn, typeArguments)`, and `PostDrainQuoter.resolveAbilityRefs`
     **preserves the impl's type args** through the ability→impl rewrite — so each effect op / callee carries its *own*
     carrier instantiation, positionally aligned with *its* binders.
   - The current `EffectAccountingProcessor.collectReferences` matches `MonomorphicValueReference(vfqn, _)` — it
     **discards exactly this signal**. That one discard is the entire gap.
   The ported ride test: **count a reference iff the ground value at its carrier-binder position(s) equals (one of) the
   value's own ambient instantiation(s), by exact `GroundValue` equality.** Total and positional (never recognitional —
   the §13 doctrine applied to the verifier), and *strictly more precise* than the in-checker head-level
   `CarrierHead` test: exact equality distinguishes nested same-transformer stacks
   (`ThrowCarrier[E2, ThrowCarrier[E1, IO]]` ≠ ambient `ThrowCarrier[E1, IO]`), which the head test cannot — masked
   today only by `AbilityFQN` (ability-name) granularity. Case walk, all resolved: the synthetic entry has *no* carrier
   binder ⇒ ambient ∅ ⇒ `User::main@[IO]` subtracted (**no synthetic-entry exemption needed at all**); user `main` at
   `[IO]` ⇒ `printLine@[IO]` == ambient ⇒ counted, `{Console} ⊆ {Console}`; a discharged `raise` at an inner transformer
   ≠ ambient ⇒ subtracted; a captured `{Throw}` value mono'd *at* the stack ⇒ its own `raise` == ambient ⇒ counted and
   declared. Structural verdict from the same investigation: an extrinsic row verifier is **architecturally forced** in
   Eliot (use-site checking always sees concretized carriers, and concrete instances are promiscuous —
   `implement[F[_] ~ Suspend] Console[F]` resolves `Console[IO]` regardless of what the caller declared, so instance
   resolution can never enforce rows), and the governing condition matches GHC Core Lint's: post-hoc verification works
   iff the lowered form retains the checked structure — which the mono key does. Definitive recipe + caveats
   (concrete-impl arm, constraint-based declared side, pinned returns, the diagnostics that can never move): **§10
   U4-c-0**.

   Until U4-c-0 lands and passes the parity gate, `EffectResidualChecker` **stays** the live verifier (kept as the
   fail-safe — [[feedback_gaps_must_be_failsafe]]), the `WovenValueProcessor` demand is **not** wired, and the
   re-pointed `EffectAccounting` remains gated + demand-driven.

Commit trail this session: Bundle A (deletions + flag-simplification + re-point) — one commit. The whole-base wiring was
built as a probe (`WovenValueProcessor` demand + unconditional accounting), used to surface the run/discharge finding,
then reverted; only Bundle A landed.

**Done:**
- **U1 (Id-normalization) — COMPLETE.** `monomorphize/channel/IdNormalizer.scala`, invoked from
  `WovenValueProcessor` on by default. Body rewrites + newtype rep (U1a), `Id[X] ⤳ X` type/key
  erasure (U1b), first-class-combinator eta-expansion. No `Id` residue anywhere. §6.
- **U2 (foundation spike) — COMPLETE (green, not wired in).**
  `lang/test/src/com/vanillasource/eliot/eliotc/monomorphize/spike/UniformCarrierSpike.scala`
  (mechanism) + `UniformCarrierSpikeTest.scala` (26 cases). Two sharpenings folded into §3: `Id` is the
  lattice bottom *everywhere*; the ladder *classifies by the expected slot* (the surviving recognition
  is a positional effect-carrier tag on the expected binder, not shape detection of the actual). §3/§8
  updated; §10 U2 has the full result list. These 26 cases become U3a's acceptance suite.
- **U3-0a — LANDED.** The v1 weaver (`WovenValueProcessor.weave…`, `object Combinators`), the
  entry-point rework (`weaveEntry`/`runBoundary`, `baseCarrierKey`/`entryPointKey`,
  `JvmPlugin.withBaseCarrier`/`baseCarrierFQN`, `SyntheticMainSourceProcessor.effectChannelMainSource`),
  and `WovenValueTest` are deleted. Safe because all of it was reachable only via `baseCarrier = Some`,
  which nothing sets anymore.
- **U3a-1 (carrier mechanism, productionised) — LANDED.** The U2 spike ported from its toy `SType`
  model onto the real domain, as a new **`lang/src/.../monomorphize/carrier/`** package:
  - `Carrier.scala` — the carrier lattice (`Bottom` = `Id`, `Con(fqn, prefix)`, `Var(id)`) with the
    **positional, total `split`** (`C[T] ⤳ (carrier, payload)`; needs *no* carrier-constructor set, unlike
    the spike — it peels whatever outermost head elaboration put there, `Id` ⤳ `Bottom`), `ofHead`, and
    `toSemValue`.
  - `CarrierJoin.scala` — the **join solver** over the real `Unifier`/`MetaStore` (`Id` = lattice bottom,
    single non-`Id` winner, conflict = `addError`, `finalize` defaults untouched metas to `Id`). Carrier
    metas live in the shared meta store, kept unstealable by *routing them exclusively through this join
    channel* (the ladder splits the carrier off before any payload `unify`).
  - `UniformLadder.scala` — `resolveSlot` (the classify-by-expected-slot ladder: `Generic` pass-through /
    `CarrierSlot` pass-join / `PayloadSlot` bind), `classifyExpected` (the surviving positional recognition —
    an `isEffectCarrierSlot` tag on the *expected* binder), and **deferred, decision-free `materialize`**.
  - `lang/test/.../monomorphize/carrier/CarrierMechanismTest.scala` — 27 cases (the four historical failure
    cases + the flagship effectful/mixed conditionals + the join lattice + the §8 boundary + the classifier
    + payload rendering), green. The theft-contrast cases run the **real** `Unifier` so the injectivity
    theft is demonstrated on production types, not a toy.
  - **Not wired into the `Checker`** — the default path is byte-identical (verified: `lang.test`/`jvm.test`
    green, HelloWorld builds+runs). This is the foundation U3a-2 (the checker wiring) consumes.
- **U3a-2a (checker-side bridge) — LANDED.** `check/UniformCarrierChecker.scala` lifts the U3a-1 domain
  mechanism into `CheckIO` (reading/writing `CheckState.unifier` like `EffectLifter`/`CarrierKindChecker`),
  unit-tested in isolation via `check/UniformCarrierCheckerTest.scala` (the `EffectLifterTest` harness:
  build a `CheckState`, run the `StateT`). It resolves **§12-Q1 to check-time carrier-wrapping**:
  - `intoCarrierHeaded(tpe)` — a pure `T` ⤳ `Id[T]` (via `applyValue(Id, tpe)`); an already-carrier-headed
    judgment (ambient/role carrier, or `Id`) or a `VType` (the §8 boundary — never wrapped) is left. The
    recognition it needs is the *positional* read of the value's own bookkeeping, reusing
    `EffectLifter.effectCarrierSplit`, not the undecidable "is an arbitrary type a carrier?".
  - `classifyExpectedSlot(expected)` — `UniformLadder.classifyExpected` with the effect-carrier tag read on
    the **expected** side from `ambientCarriers`/`carrierRoles`.
  - `resolveSlot` / `finalizeAndMaterialize` — the `CheckIO`-threaded ladder over `CheckState.unifier`, and
    the boundary rule (default every unsolved `carrierRoles` meta to `Id`, then materialise the deferred
    lifts decision-free).
  - **Not constructed/called by the `Checker` yet** — the default path is byte-identical (verified). This
    deliberately avoids the `desugarChannel`/`EffectAccounting` coupling (U3-0b), which the *flip* must
    untangle.
- **U3a-2b(i) (the bridge's node mechanics) — LANDED.** The "execute a ladder decision as a real
  `SemExpression`" toolkit, built by **reusing** `EffectLifter`'s insertions (reshape, not rebuild), still
  isolation-tested and uncalled by the `Checker` (default path byte-identical, verified incl. eliot-test 11/11):
  - `EffectLifter.pureWrapNode`/`runIdNode` **extracted** into the companion (a behavior-preserving refactor of
    `tryPureWrap`/`tryIdDefault`; `idCarrier` moved there too), so both the default path and the uniform path
    build the same nodes from one place.
  - `UniformCarrierChecker.resolveArgumentSlot(arg, argExpr, argType, expected)` — the node-producing successor
    of the checker's `checkArgumentSlot`: classify the expected → run the ladder (join + payload unify) → build
    the slot node. `Generic` passes the action through unchanged; `CarrierSlot` pass-joins and re-carries a
    **pure** actual via `carrierSlotLift` (`pure@Effect[?G](runId(actual))`, whose `?G` the join solves and the
    Id-normalizer erases at `Id`); `PayloadSlot` **binds** (fresh `$eff$N` reference + `EffectLifter.Bind` for
    the spine's `wrapBinds` — a bind over a bottom `Id` carrier erases, so a pure actual into a pure slot costs
    nothing). Returns a `UniformSlotOutcome` (`Passed`/`Bound`) mirroring the checker's `SlotOutcome`.
- **U3a-2b(i+) (bridge surface complete) — LANDED.** The last two primitives the spine flip will call, so
  U3a-2b(ii) becomes pure wiring (additive, default path byte-identical):
  - `intoCarrierHeadedTerm(expr, source)` — the eager **term-level** dual of `intoCarrierHeaded`: a pure term's
    *value* `expr : T` ⤳ `pure@Effect[Id, T](expr) : Id[T]` (via the extracted `pureWrapNode`), leaving an
    already-carrier-headed or `VType` term unchanged. `infer` applies it to its pure leaves so every judgment is
    carrier-headed.
  - `checkReturnBoundary(bodyExpr, bodyType, declaredReturn, source)` — the uniform successor of `checkAgainst`:
    `intoCarrierHeaded` the declared return, the body's carrier **joins** it + payloads unify, a **pure** body
    re-carried via `carrierSlotLift` (erased at `Id`). An effectful body against a pure (`Id`) return leaves its
    carrier to default to `Id`, where the effect op's `Id` instance fails to resolve — the loud fail-safe, as on
    the default path.
  The bridge is now feature-complete: `intoCarrierHeaded` (type) + `intoCarrierHeadedTerm` (term) +
  `classifyExpectedSlot` + `resolveArgumentSlot` (+ extracted node mechanics) + `checkReturnBoundary` +
  `finalizeAndMaterialize`.
- **U3a-2b(ii) infrastructure — LANDED (2026-07-23, `dd61f027`).** The transitional **`--uniform-carrier` gate**
  (the recommended de-risking, below) is threaded end-to-end and the bridge is **constructed in the `Checker`** —
  the plumbing the spine-loop wiring consumes, with nothing routed through it yet (default path byte-identical):
  - `LangPlugin` — `--uniform-carrier` CLI flag + `uniformCarrierKey`, forwarded into `LangProcessors` beside
    `effectChannel`.
  - `LangProcessors` → `MonomorphicTypeCheckProcessor` **and** `CompilerMonomorphicTypeCheckProcessor` (both tracks,
    per §8 "both tracks use the one checker") → `TypeStackLoop` (ctor + companion `process`) → `Checker`.
  - `Checker` constructs `private[check] val uniformChecker = new UniformCarrierChecker(force, lifter.effectCarrierSplit)`
    beside `EffectLifter` — unconditionally (cheap; two function refs) but never called while `uniformCarrier` is off,
    so `lang.test`/`jvm.test`/HelloWorld/eliot-test are all byte-identical.
- **U3a-2b(ii) wiring slice 1 (the return boundary) — LANDED (2026-07-23, `8fadd27f`).** The first spine point routed
  through the bridge. Under `--uniform-carrier`, `checkAgainst` routes the **plain pure value return** case through
  `uniformReturnBoundary` (`intoCarrierHeadedTerm` the body + `checkReturnBoundary`), inserting `pure@Id`/`runId` the
  Id-normalization stage erases → byte-identical bytecode; every other shape falls back to the verbatim
  `checkAgainstDefault`. The gate `uniformReturnRoutable` routes only when both the declared return and the body's
  inferred type are plain non-carrier-headed `VTopDef` value types (`uniformPlainValueType`) *and* the payload fits by
  pure definitional equality (a non-committing speculative unify — the right test now that `Int == Int` with bounds in
  the separate refinement channel, so `Int` returns route too). It falls back for effect-carrier-headed returns,
  guard/calc-return/W3 discharge, the §8 `VType` boundary, function/polytype returns, and any genuine
  definitional-equality miss (an ordinary mismatch — there is **no** `Coerce`/widening machinery, it was deleted when
  `Int` became nullable-bounds-in-channel). **Concrete finding:
  routing an effect-carrier-headed return (`?F[Unit]`) through `checkReturnBoundary` self-solves its carrier meta
  (`?F := ?F`) → infinite loop, hence the non-carrier-headed gate.** Durable gate: `UniformCarrierByteIdenticalTest`
  (jvm.test) compiles a pure-value program **+ the whole base layer** with the flag off and on and asserts every class's
  bytes match (two full base compiles by design — the entire base compiles byte-identically under the flag).
- **U3a-2b(ii) spine wiring slice 1 (the argument slot) — LANDED (2026-07-23, `527af90a`).** `checkArgumentSlot` now
  routes the **plain pure-value-into-payload-slot** case through the uniform ladder (`intoCarrierHeadedTerm` the arg +
  `resolveArgumentSlot`), gated by `uniformArgSlotRoutable` (the argument-position analogue of `uniformReturnRoutable`:
  both the arg's inferred type and the domain are plain non-carrier-headed `VTopDef` value types + payload fits by pure
  definitional equality). Everything else — effectful args, generic/flex domains + Phase-A deferral, effect-carrier /
  HKT-dispatch slots, function/polytype args — falls back to the verbatim default Phase-A logic. **Bridge fix the wiring
  surfaced:** a pure (`Id`, bottom) actual into a `PayloadSlot` must **pass its payload directly (`runId`), not bind** —
  binding it was unsound because `wrapBinds`/`bindWrap` unifies the bind's carrier with the enclosing core's, so an `Id`
  bind reaching an *effectful* core (`printLine(greeting)`, core `F[Unit]`) would wrongly solve `F := Id` and strip the
  effect (only an effectful actual binds). For the gated case the bridge returns `Passed(runId(pure@Id(arg)))`, erased
  by the Id-normalizer → byte-identical direct pass. The whole-base `UniformCarrierByteIdenticalTest` now exercises the
  argument path across every pure-arg-into-payload-slot in lang/stdlib/jvm and stays byte-identical.
- **U3a-2b(ii) spine wiring slice 2 (effectful argument) — LANDED (2026-07-23, `16a432f6`).** The argument→payload-slot
  case is now complete for both pure *and* effectful actuals. `checkArgumentSlot` routes a **plain payload** domain
  through `uniformPayloadSlot`, which instantiates the arg once (peeling `readLine`'s `[F ~ Console] F[String]` to its
  carrier-headed monotype `?F[String]`), computes its payload (`uniformPayloadOf`: effect-carrier payload for an
  effectful actual, the value for a pure `VTopDef`, `None` for a function/polytype/type/bare-meta), and routes when the
  payload fits by pure definitional equality — a pure actual **passes** (`runId`, erased), an effectful actual **binds**
  (`Bound`, folded by the spine's `wrapBinds` into `flatMap`, exactly as the default `tryBindLift`). The default logic is
  extracted verbatim into `defaultArgSlot` (the fallback). **Why it stays byte-identical:** an effectful arg's carrier
  *is* the ambient meta, the same as the enclosing core, so `wrapBinds`'s `doUnify(?F, coreCarrier)` connects them
  correctly — the join is only needed later for *conditionals*, where arms carry different carriers; straight-line
  effectful args match by construction. Gate: `UniformCarrierByteIdenticalTest` program is now
  `label(readLine)` (effectful arg bound) + `label`'s pure value return; whole base + program byte-identical, and a probe
  confirmed the effectful `Bound` path is live (`carrier=?F, payload=String` for `readLine`).
- **U3a-2b(ii) carrier-meta self-join guard — LANDED (2026-07-23, `ead5d631`).** `CarrierJoin.join` no-ops a carrier
  joined toward *itself* (a contribution resolving to the same representative meta), which would else write a
  self-referential cycle into the store and loop `resolve`. Isolation-tested; **defensive** — the checker links two
  distinct carrier metas by union, so this exact self-join is not yet triggered live. Its companion is the
  **miscompile finding** (see "Next"): the conditional `CarrierSlot` pure-arm routing was trialled and reverted because
  it defaults an ability-constrained carrier to `Id`.
- **U3a-2b(ii) effect-carrier-headed returns — LANDED (2026-07-23, `b35bf80c`).** `uniformReturnRoutable` broadened from
  plain-pure-value returns to **any value return** (`uniformValueReturn` = `uniformPlainValueType` OR
  `effectCarrierSplit` non-empty), **runtime track only** (§8 keeps the compile-track `Either` discharge carrier-free).
  So `main : {Console} Unit`'s `?F[Unit]` and the synthetic entry's `IO[Unit]` now route through `checkReturnBoundary`
  (an effectful body passes through unchanged — carriers join/union; a pure body re-carries via `pure@?F`, `?F` solved
  at the entry to `IO`, never defaulted to `Id`). Byte-identical (whole-base test; probe-confirmed the `IO[Unit]` return
  routes). This supersedes slice 1's "falls back for effect-carrier-headed returns".
- **U3a-2b(ii) conditionals byte-identical — LANDED (2026-07-23, `23eb785a`).** The whole `IfDemo` conditional surface
  (`if`/`else`/`fold`, pure/effectful/chains/blocks/discharge-to-pure) compiles byte-identically under the flag, with the
  conditional **bodies** on the uniform path and the **arms** still on the default ladder. Three fixes the
  conditional-bodied programs exposed, all runtime-track: (1) `checkReturnBoundary` grew a **discharge-to-pure** arm —
  a fully-discharged flex-carrier body (`?G[T]`) under a pure `Id` return defaults that carrier to `Id`
  (`CarrierJoin.finalize` over the one meta) and unwraps with `runId` (`EffectLifter.runIdNode`), the successor of
  `tryIdDefault`, so `sign(f) = if(f,"+") else "-"` drops into pure code; and it `force`s `bodyType`/`declaredReturn`
  before `Carrier.split` (a solved-but-unforced carrier meta would else throw "not a carrier-headed runtime judgment"),
  as does `resolveArgumentSlot`. (2) `uniformPayloadSlot` distinguishes **capture from bind**: an *effectful* actual
  whose *whole* carrier-headed type unifies the domain is a **capture** — the domain is a carrier form / pinned stack (a
  discharger's `computation: {Abort | G} A` ⤳ `AbortCarrier[G, A]`, `runMain`'s `IO[A]`) — so it defers to the default
  unify-first ladder, which stores the computation whole (`?G_if := AbortCarrier[G]`). Binding it let the flex payload
  meta *steal* the domain (`?T := AbortCarrier[..]`), inverting `if`'s carrier to the ambient `IO` and leaking the
  discharged `Abort` (the observed `if` type args `[IO, AbortCarrier[..]]` instead of `[AbortCarrier[..], Unit]`). (3)
  the arg-slot uniform routing is gated to `Platform.Runtime`, matching the return boundary (§8: the compile-track
  `eliot-compiler/` bodies stay carrier-free) — fixes a compiler-track `Id.els` mismatch eliot-test hit under the flag.
  Gate: eliot-test 11/11 byte-identical (was failing under the flag), 32/32 compiling example mains byte-identical, no
  regressions (the 3 flag failures — `EffectsTwoDeps`/`EffectsTwoThrows`/`WherePrecondition` — fail identically at
  baseline: pre-existing multi-layer-discharge / `where`-precondition gaps). New `UniformCarrierByteIdenticalTest`
  conditional-surface case; `lang.test`/`jvm.test`/HelloWorld green. The conditional **arms** still resolve on the
  default ladder — routing them (the non-overlap improvement) is the next step.
- **U3a-2b(ii) CarrierSlot conditional arm — LANDED (2026-07-23, `5864f95f`).** The first *non-overlap* improvement:
  `if(c, None) else Some(x)` (and the reversed `if(c, Some(x)) else None`), which the **default path rejects** (`Type
  mismatch`), now compiles+runs under `--uniform-carrier`. `checkArgumentSlot` routes an **effect-carrier** parameter
  domain (`?G[T]` — `if`'s `value: {Abort} T`, a discharger's `fallback: G[A]`) through `uniformCarrierSlot`: a **pure**
  actual (a plain `H[X]`, `None : Option[?E]`) **pure-wraps first** (`EffectLifter.tryPureWrap` — payload fills `?T`, `?G`
  kept a meta the `else` discharge solves to `AbortCarrier[Id]`) *before* the default ladder's `tryUnifyCommitting`,
  which at **equal arity** (`Option[?E]` vs `?G[?T]`) **steals** the carrier whole (`?G := Option`) because the pure-wrap
  pre-arm fires only on a strictly *under*-applied actual — that theft is the rejection. An **effectful** actual stays on
  `defaultArgSlot`. **Reuses `tryPureWrap` unchanged** (reshape, not rebuild) — the clean single `pure@Effect[?G](arg)`
  node the default path emits, **not** the eager-heading `carrierSlotLift` double-wrap `pure(runId(pure@Id(arg)))` (a
  trial: its inner `pure@Id` confused the outer `pure`'s `Effect`-instance resolution and mis-erased it to raw payload — a
  `VerifyError` that broke `sign` too; **so the crux was the double-wrap, not the `finalize`-defaults-to-`Id` hypothesis**,
  and this single-slot arm needs **no** `CarrierJoin`/`finalize`). Byte-identical everywhere it overlaps (`sign`/IfDemo/
  eliot-test 11/11 / 32-of-32 example mains, same 3 pre-existing flag-gaps, **no regressions**); new compile-succeeds gate
  `UniformCarrierConditionalTest` (rejected off, accepted on); `lang.test`/`jvm.test` green.
- **U3a-2b(ii) compound-state (payload-fits-first) — LANDED (2026-07-23, `ba208c48`).** The second *non-overlap*
  improvement: an effectful value into a data slot (`items : {Console} List[String]` into `foldLeft`'s `list : List[A]`),
  which the **default path rejects** (the equal-arity unify steals `?F := List`, then `Effect[List]` has no instance —
  the `val` workaround was required), now **binds** under the flag. `uniformPayloadSlot` decides bind-vs-capture by
  whether the actual's **payload genuinely fits** the domain (checked *first*), not whether the *whole* actual
  whole-unifies (the prior order): a carrier-stack/pinned domain's inner value never fits its outer carrier (⇒ capture —
  a discharger's `AbortCarrier[G,A]`, `runMain`'s `IO[A]`), a data container's element type does (⇒ bind). A
  **bare-flex payload `?A`** is guarded out of "fits" (`payloadFitsDomain`, `force`d to a `VMeta(_, SNil)` test) — it
  absorbs any domain and strips the carrier, so it captures; this is what keeps a discharger's `raise(err) : ?F[?A]` into
  `map`'s `fa : F[A]` a capture (and eliot-test byte-identical — the guard's absence reddened `Throw.els` with
  `Effect[String, Id]`). Byte-identical everywhere it overlaps (eliot-test 11/11, sign/IfDemo, 32/32 mains, **no
  regressions**); compound-state compile-succeeds cases added to `UniformCarrierConditionalTest` (rejected off, accepted
  on — runs `loading...` then `start`); `lang.test`/`jvm.test` green.
- **U4 started (2026-07-24).** The two remaining pre-flip arm items were resolved to findings (both re-scoped, no code —
  `3fa0dbec`, `fb58c830`): the `Generic` arm is **dropped** as a standalone slice (folded into the flip) and the
  effectful-`catch`-handler is **U4-gated** (the stdlib delta works but a non-flag-gated signature change regresses the
  default path). The **grounded 5-slice U4 execution plan** was written from a full code map (`c0654a3f`, §10).
  - **U4-c partial — LANDED (`26ce08b2`, code, green).** Deleted the `EffectResidualChecker` **Phase-2 shadow**
    (`channelEffectsOf`/`channelDeclaredFor`/`shadowCompareSubset`/`shadowCompareVerdict`/`shadowMarker` + the
    `effectsOfFn` parameterization) — purely observational, byte-identical; the pure row-extraction unit test retargeted
    to `EffectAccountingProcessor.channelDeclaredEffects` (new `EffectAccountingChannelDeclaredTest`, deleted
    `EffectResidualCheckerTest`). All gates green incl. eliot-test 11/11.
  - **U4-b/U4-c recipe pinned (`f7767998`, `20a0b88b`, docs).** The definitive Bundle A re-point mechanism (carrier-path
    effect ops carry `Qualifier.AbilityImplementation` — `contributedEffects` must recover the ability via a HKT-marker
    fact lookup + the *ability's* module, fail-safe-critical), the flag-threading simplification that falls out, the
    concrete wiring hook (`EffectAccounting` is *unwired* — demand it at the `WovenValueProcessor`/`UsedNamesProcessor`
    seam), and the whole-base self-verifying gate + blast-radius caveat. §10 U4-b/U4-c.

**Uniform-path coverage NOW (under `--uniform-carrier`, all byte-identical to the default path, runtime track):**

| construct | routes uniform? | how |
|---|---|---|
| **value RETURN boundary** (`checkAgainst`) | **yes** — pure, effect-carrier, *and* discharge-to-pure | `uniformReturnBoundary` → `checkReturnBoundary`; pure re-carried via `Id` (erased), effect-carrier passed through, a fully-discharged flex `?G[T]` body under a pure return `Id`-defaulted + `runId`-unwrapped (tryIdDefault's successor). Gate `uniformReturnRoutable`/`uniformValueReturn`. |
| **argument → PAYLOAD slot** (`checkArgumentSlot`, a concrete non-carrier domain) | **yes** — bind-vs-capture by **payload-fit** | `uniformPayloadSlot`: if the actual's **payload genuinely fits** the domain ⇒ **bind** (`printLine(readLine)`; the compound-state `items : ?F[List[X]]` into `foldLeft`'s `List[A]` — `List[X]` fits `List[A]`, which the **default path rejects** by stealing `?F := List`), pure passes (`runId`). If the payload does **not** fit ⇒ `defaultArgSlot`: a **capture** (a carrier-stack/pinned domain whose inner value never fits its outer carrier — a discharger's `{Abort\|G} A` ⤳ `AbortCarrier[G,A]`, `runMain`'s `IO[A]`) or a mismatch. A **bare-flex payload `?A`** is guarded out of "fits" (`payloadFitsDomain`) — it absorbs any domain and strips the carrier, so it captures. Gate `uniformPlainValueType(domain)` + `Platform.Runtime`. |
| **conditional bodies** (`if`/`else`/`fold`) | **yes** — byte-identical | The whole `IfDemo` surface compiles byte-identically: return boundary + discharger capture route uniform; `fold`'s bare-`A` `Generic` arm still on the default `defaultArgSlot` ladder. |
| **argument → CARRIER-SLOT arm** (`if`'s `value: {Abort} T` = `?G[T]`, a discharger's `fallback: G[A]`) | **yes** — pure pure-wraps first, effectful on default | `uniformCarrierSlot`: a **pure** actual (`None : Option[?E]`) pure-wraps (`EffectLifter.tryPureWrap`) *before* the default ladder's stealing equal-arity unify — fixing `if(c, None) else Some(x)`, which the default path **rejects** (the pure arm steals the carrier `?G := Option`); an **effectful** actual stays on `defaultArgSlot` (its carrier unifies with `?G` correctly). Byte-identical where the default already pure-wraps (`sign`). |
| **argument → GENERIC arm** (`fold`'s bare-`A`) | **no → default, and DROPPED as a standalone step** (finding) | `defaultArgSlot`. Routing bare-flex-meta domains through `PassWhole` fires for *every* generic argument in the base (not just `fold`) and Id-wraps them → occurs-check failures; without the Id-wrap it equals the default (`?A := payload`) and omits Phase A/B's ride-up-vs-bind check. Folded into the flip, not landed early; see "Next". |
| **the effectful-`catch`-handler** | **GATED ON U4** (corrected finding) | The stdlib delta (`onError: E => G[A]`, `flatMap`+`pure` body) **works** — enables effectful handlers (`failUnit catch (err -> printLine(err))` runs) and is backward-compatible for a *single* discharger. But it needs a user-facing stdlib signature change (not flag-gated ⇒ hits both paths), and on the default path it **regresses two-or-more sequenced pure-handler catches** (`EffectsThrow`) via ambient carrier-stacking (bogus `Throw[String, IO]` at jvm `Throw.els:54`). That stacking is the premature-carrier-commitment class the uniform `CarrierJoin` removes, so the delta can only land once the uniform path is the default — at/after U4. See "Next". |
| function/polytype/`VType` returns, guard/calc-return/W3, **compile-time track** | **no → default** | `checkAgainstDefault` / §8 boundary. |

Every routed case inserts `pure@Id`/`runId`/`flatMap@Id` that the Id-normalization stage erases, so the emitted bytecode
equals the default's. **The wiring lives in `Checker.scala`** (the gated `uniform*` helpers + the `defaultArgSlot` /
`checkAgainstDefault` verbatim fallbacks; the arg-slot routing gated to `Platform.Runtime`) + `UniformCarrierChecker.scala`
(the bridge: `checkReturnBoundary` with its three arms incl. discharge-to-pure, the pure-actual-passes fix, `force`
before every `Carrier.split`) + `CarrierJoin.scala` (self-join guard); the durable gate is
`jvm/test/.../UniformCarrierByteIdenticalTest.scala` (whole base + `label(readLine)` *and* the conditional surface
`if`/`else`/`fold`/discharge-to-pure, flag off vs on, class-bytes equal) plus `UniformCarrierConditionalTest` (the
non-overlap compile-succeeds gate). `finalizeAndMaterialize` and the `CarrierJoin` join solver are built but **uncalled**:
the landed slices (return boundary, payload-fit bind/capture, `CarrierSlot` pure-wrap) each needed only a single carrier
meta solved by the surrounding context, never a *join* of sibling arms. They are wired only for the effectful-`catch`-handler
(the one join-requiring case), where an ability-constrained carrier meta must be solved by ability resolution / discharge,
never defaulted to `Id`.

**Conditional surface — byte-identical LANDED (2026-07-23, `23eb785a`).** The whole `IfDemo` (every `if`/`else`/`fold`
form: pure, effectful, chains, blocks, discharge-to-pure) now compiles byte-identically under `--uniform-carrier`, with
the conditional **bodies** routed through the *existing* uniform return-boundary + payload/capture slots and the
**arms** still on the default (byte-identical) `defaultArgSlot` ladder. Three fixes made it work: (1) `checkReturnBoundary`
grew the **discharge-to-pure** arm (`?G[T]` flex body under a pure `Id` return ⇒ default the carrier to `Id`, unwrap
with `runId` — the successor of `tryIdDefault`) and now `force`s before `Carrier.split`; (2) `uniformPayloadSlot`
distinguishes **capture from bind** (an effectful actual whose whole carrier-headed type unifies the domain — a
discharger's `AbortCarrier[G,A]` slot — defers to the default unify-first ladder, which *captures* it; binding stole the
domain into the flex payload and inverted `if`'s carrier to the ambient `IO`, leaking the discharged `Abort`); (3) the
arg-slot uniform routing is gated to `Platform.Runtime` (matching the return boundary; keeps the compile-track
`eliot-compiler/` bodies carrier-free — §8). eliot-test 11/11 byte-identical (was failing under the flag), 32/32
compiling example mains byte-identical, no regressions (the 3 remaining flag failures — `EffectsTwoDeps`,
`EffectsTwoThrows`, `WherePrecondition`: pre-existing multi-layer-discharge / `where`-precondition gaps, fail identically
at baseline). Durable `UniformCarrierByteIdenticalTest` conditional case added.

**CarrierSlot conditional arm — LANDED (2026-07-23, `5864f95f`).** The first non-overlap improvement: `if(c, None) else
Some(x)` (and `if(c, Some(x)) else None`), which the **default path rejects**, now compiles+runs under the flag.
`checkArgumentSlot` routes an **effect-carrier** parameter domain (`?G[T]` — `if`'s `value: {Abort} T`, a discharger's
`fallback: G[A]`) through `uniformCarrierSlot`: a **pure** actual (a plain `H[X]`, e.g. `None : Option[?E]`) **pure-wraps
first** (`EffectLifter.tryPureWrap`) — the payload fills the carrier's payload slot `?T` and `?G` stays a meta the `else`
discharge solves to `AbortCarrier[Id]` — *before* the default ladder's `tryUnifyCommitting`, which at **equal arity**
(`Option[?E]` vs `?G[?T]`, both arity 1) **steals** the carrier whole (`?G := Option`) because the pure-wrap pre-arm only
fires on a strictly *under*-applied actual (the theft is exactly the `if(c, None) else Some(x)` rejection). An
**effectful** actual stays on `defaultArgSlot` (its carrier unifies with `?G` correctly — no theft hazard). It **reuses
`tryPureWrap` unchanged** (reshape, not rebuild): the clean single `pure@Effect[?G](arg)` node the default path emits —
**not** the eager-heading `carrierSlotLift` double-wrap `pure(runId(pure@Id(arg)))`, whose inner `pure@Id` confused the
outer `pure`'s `Effect`-instance resolution and mis-erased it to raw payload (a `VerifyError` that broke `sign` too — a
trial, reverted; the crux was **not** a `finalize`-defaults-to-`Id` issue as first theorised but the double-wrap). So
this arm needs **no** `CarrierJoin`/`finalize` — `if`'s `value` is a single slot with no pure/effectful *sibling* in one
call. Byte-identical everywhere it overlaps (`sign`/IfDemo/eliot-test/32-example-mains all still byte-identical, no
regressions); new compile-succeeds gate `UniformCarrierConditionalTest` (rejected off, accepted on).

**Compound-state — LANDED (2026-07-23, `ba208c48`).** The second non-overlap win: an effectful value into a data slot
(`items : {Console} List[String]` into `foldLeft`'s `list : List[A]`), which the **default path rejects** (the
equal-arity unify steals `?F := List`, then `Effect[List]` has no instance — the `val` workaround was required), now
**binds** under the flag. `uniformPayloadSlot` decides bind-vs-capture by whether the actual's **payload genuinely fits**
the domain (checked *first*), not whether the *whole* actual whole-unifies (checked first, before) — a carrier-stack
domain's inner value never fits its outer carrier (⇒ capture), a data container's element type does (⇒ bind). A
**bare-flex payload `?A`** is guarded out of "fits" (`payloadFitsDomain`) — it absorbs any domain and strips the carrier
(a discharger's `raise(err) : ?F[?A]` into `map`'s `fa : F[A]` has such a payload and must capture; this guard keeps
eliot-test byte-identical). Byte-identical everywhere it overlaps (eliot-test 11/11, sign/IfDemo, 32/32 mains, no
regressions); compound-state compile-succeeds cases in `UniformCarrierConditionalTest`.

**Next: the effectful-`catch`-handler (the Generic arm is dropped — see the finding).** Both candidates were tried
this session; both are re-scoped:

- **`fold`'s bare-`A` `Generic` arm — DROPPED from the pre-flip NEXT (finding, 2026-07-23).** Routing a bare-flex-meta
  domain (`VMeta(_, SNil)`) through the uniform `Generic` `PassWhole` ladder is **not** a standalone byte-identical
  slice, for three compounding reasons, all found empirically (the routing + a pure-`fold` byte-identical case were
  built, broke, and were reverted — tree green):
  1. **It fires for *every* generic application's bare-meta slot in the whole base** — `Some(x)`, `Pair(a, b)`,
     `identity`, `min`/`max`, every data constructor — not just `fold`. The uniform foundation *forbids* recognising
     `fold` by name, and there is no positional tag distinguishing `fold`'s arm from `Some`'s value slot (both are bare
     `?A` domains). So "route the `Generic` arm" unavoidably means "route every generic argument".
  2. **`PassWhole` with the eager `intoCarrierHeadedTerm` Id-wrap Id-wraps every generic argument.** For a pure actual it
     commits `?A := Id[payload]` (not `?A := payload`), so `Some(x)`'s element, `Pair`'s fields, etc. all become
     `Id[…]`-headed — which cascades into occurs-check failures (`Cannot construct infinite type`) compiling the base and
     regressed two previously-passing `UniformCarrierConditionalTest` cases.
  3. **Without the Id-wrap it is vacuous, and even so it omits a decision the default path makes.** Committing
     `?A := payload` for a pure arg is *exactly* what `defaultArgSlot → resolveGuardedLadder` already does. And for an
     *effectful* arg `PassWhole`'s unconditional `?A := <carrier-headed action>` omits the **ride-up-vs-bind**
     discrimination the default Phase A/B encodes (`resolveDeferredSlot`'s `occursInValue(id, retType)`): a *transparent*
     callee (`fold`, whose result *is* `A`) rides the carrier up, but a *non-transparent* one
     (`putState[S, F](s: S): F[Unit]`, `S` absent from the result) must **bind-lift** or the carrier is stranded in a
     type parameter. The U2 spike's `Generic` case (`CarrierMechanismTest` line ~189) only ever tests the transparent
     ride-up, so it never exercised this.

  **Consequence for the flip:** the `Generic` arm is not a warm-up and adds no capability. When the U4 flip replaces the
  Phase A/B deferral wholesale, the uniform `Generic` arm must itself carry the ride-up check (`occursInValue(metaId,
  retType)` → pass-through, else bind) — it is **not** "pass-through-whole, zero knowledge". Folded into the flip; removed
  from the standalone NEXT.
- **The effectful-`catch`-handler — the stdlib delta *works* for effectful handlers, but is GATED ON U4 (corrected
  finding, 2026-07-23; supersedes the earlier "breaks even the pure-handler backward-compat case", which came from a
  malformed probe that hit the pre-existing discharge-to-pure gap).** The delta is `catch`'s handler
  `onError: E => A` (pure) → `onError: E => G[A]` with body
  `flatMap(e -> foldEither(onError, a -> pure(a), e), runThrow(computation))`. Measured behaviour, each case built and
  run:
  - ✅ **Enables effectful handlers** (the target capability): `failUnit catch (err -> printLine(err))`
    (`failUnit : {Throw[String]} Unit`) compiles and runs on **both** the default and the `--uniform-carrier` path. The
    signature change is *necessary* — without it a pure `onError: E => A` cannot absorb an effectful handler
    (`err -> printLine(err) : E => ?F[Unit]` would make `catch` return `G[?F[Unit]]` = a double carrier), which is why
    the effectful case is "not even expressible" today.
  - ✅ **Backward-compatible for a single discharger** (even inside a block): `printLine(parseOk catch (err -> err))`
    compiles and runs.
  - ✅ **The `flatMap` + `pure(a)` body is sound in isolation**: keeping the *pure* signature `onError: E => A` but
    switching the body to `flatMap(e -> pure(foldEither(onError, a -> a, e)), runThrow(computation))` compiles the whole
    two-discharger `EffectsThrow` example — so the body rewrite is not the problem.
  - ❌ **Regresses exactly one shape on the default path: two-or-more dischargers with *pure* handlers sequenced in one
    block** (`EffectsThrow`'s `{ printLine(parseOk catch (err -> err)); printLine(parseBad catch (err -> err)) }`, and
    the minimal `MinCatch2`). The new signature forces the pure handler `err -> err` to **lift its codomain**
    (`pure@?G(err) : ?G[String]`); under block-sequenced dischargers over the pinned row `{Throw[E] | G}` that lift
    mis-unifies the shared ambient carrier into a **stacked `ThrowCarrier`**, spuriously demanding `Throw[String, IO]` at
    the inductive lift (jvm `Throw.els:54`, the `where E1 != E2` diagonal). `--uniform-carrier` does **not** currently fix
    it (the lambda-handler codomain lift is not uniform-routed).

  **Why this is gated on U4, not a pre-flip `--uniform-carrier` win like the other three cases.** The other historical
  fixes (`if(c, None) else Some(x)`, compound-state) were *pure checker* improvements — no stdlib change — so the default
  path was untouched and the flag could grow them incrementally. The catch-handler is different: it *requires a
  user-facing stdlib signature change* (`onError: E => G[A]`), and stdlib source is **not** flag-gated, so the change
  hits **both** paths at once. On the default path it regresses sequenced pure handlers (the carrier-stacking bug above),
  and that stacking is precisely the *premature-carrier-commitment* class the uniform `CarrierJoin` (Id-as-bottom, join
  not unify) is built to eliminate. So the delta can only land when the uniform carrier handling is the **default** — i.e.
  at/after the **U4 flip** — or behind a throwaway fix to the default checker's stacking (work U4 deletes anyway). The
  sibling join it needs lives at `foldEither`'s **function-typed** arms (`onLeft: E => B`/`onRight: A => B`, joined at
  `B = G[A]`) — a function-return join. **Recommendation:** move the effectful-`catch`-handler into the U4 milestone
  (land the stdlib delta atomically with the flip), and do not attempt it as a pre-flip slice. `CarrierJoin` /
  `finalizeAndMaterialize` stay built-but-uncalled until then.

**Background — conditionals are ordinary functions (no FQN ever hardcoded).** `fold[A](c, whenTrue: A, whenFalse: A): A`
(bare-`A` arms ⇒ `Generic` slots, both must already match — no auto-lift) and `if[T](c, value: {Abort} T): {Abort} T =
fold(c, value, abort)` (the arm declared `{Abort} T` = `?G[T]` ⇒ a `CarrierSlot`, a pure arm auto-lifts). The ladder
classifies by the *expected slot's* shape, so a user- or platform-defined conditional routes the same way — the property
whose absence killed the v1 weaver's `fold`/`if` hardcode.

**Disproven crux (kept as a warning, do not re-derive it).** The conditional-arm work was long expected to hinge on an
*ability-constrained-carrier* crux: a 2026-07-23 trial routed `if`'s pure `"+"` arm via `carrierSlotLift`
(`pure@?G(runId(pure@Id "+"))`), `sign` threw a `VerifyError`, and this was theorised as "`finalize` defaults the
ability-constrained `?G` to `Id`, erasing the `pure`". **That hypothesis was wrong** (`5864f95f`): the real cause was the
`carrierSlotLift` **double-wrap** — the inner `pure@Id` confuses the outer `pure`'s `Effect`-instance resolution (erasure
keys on the *resolved impl module*, `IdNormalizer.isEffectIdMethod`, not the type argument), mis-erasing it to raw
payload. Reusing the default `tryPureWrap`'s clean single `pure@Effect[?G](arg)` node fixed it, and the single-slot `if`
arm needs **no** `CarrierJoin`/`finalize` — `?G` is solved by the surrounding `else` discharge, never defaulted. The
*genuinely* useful residue of the finding, for the catch-handler's real join: when the join solver *is* wired, an
ability-constrained carrier meta must be solved by ability resolution / discharge, never defaulted to `Id`.

**Wiring finding (2026-07-23):**
`intoCarrierHeadedTerm` (and every heading site) must fire on **terminal value leaves only, never a function-typed
(`VPi`) reference** — a `printLine` leaf (`String → …`) is not `VType` and not carrier-headed, so the bridge as
written would wrongly wrap it `Id[String → …]`; only a fully-*applied* result carrier-heads. The `infer` call site
must itself restrict to non-`VPi` leaves (the bridge's isolation test exercises ground data / `Id`-headed / effectful
/ `VType`, never a `VPi`). This is why the three points stay a coupled bundle even under `--uniform-carrier` (they must
expect/produce carrier-headed judgments together); slice further per-shape, not all at once. The honest big step (§11).

**KEY FINDING (2026-07-23) — the flip is a non-partitionable bundle; it cannot be grown per-value under the
existing `--effect-channel` flag.** Two facts force this:
1. **The flip *keeps* the carrier desugar and removes only `desugarChannel`'s effect-*blinding*.** `{Console}
   Unit` still desugars to `F[Unit]` (F ambient) exactly as on the default path — so the default checker *already*
   compiles effectful programs on carrier-desugared input, and the uniform checker's job is to **match** it (the
   byte-identical/shadow gate compares uniform-under-flag vs default). This is a *matching* problem against a
   working reference, not from-scratch effectful compilation.
2. **`desugarChannel` makes effectful programs *look pure*** (it strips `{Console}` → `main : Unit` with an
   abstract `printLine`). So under `--effect-channel` no per-value gate (e.g. "engage the uniform path only for
   `ambientCarriers.isEmpty`") can separate a genuinely-pure value from an effect-blinded one — the effect-blinded
   `main` has no ambient carrier either. Any uniform-spine-under-`--effect-channel` therefore disturbs the
   effect-blind path the kept flag-on tests exercise. The two behaviors cannot coexist under one flag.
   Consequently the flip must land as one bundle: **stop `desugarChannel` effect-blinding + switch the checker to
   uniform + stop the `AbilityResolver` abstain + re-point `EffectAccounting` + delete `EffectChannelDesugarTest`
   (it pins the deleted effect-blindness) + relocate `EffectAccountingTest` to jvm** — all together, kept green by
   the uniform checker binding the effect-poly value's carrier (which is what makes it monomorphize once
   `desugarChannel` no longer blinds it). **Recommended de-risking: a transitional `--uniform-carrier` gate**
   distinct from `--effect-channel`, so the uniform checker grows on the *default* carrier-desugared input (no
   `desugarChannel`, normal ability resolution — `--effect-channel` off) and is compared byte-identical against the
   default path, leaving `--effect-channel` and its tests untouched until U4 unifies them. This turns the bundle
   back into an incrementally-green sequence.

Start points and constraints (the broad U3a shape — **much of "what to build" is now landed** under the transitional
`--uniform-carrier` gate; see the Done list + coverage table above for the current state. This subsection is the
overall map and the still-pending `--effect-channel` coupling, not the immediate task):
- **Where it lives:** the checker chain `monomorphize/processor/MonomorphicTypeCheckProcessor` →
  `monomorphize/check/TypeStackLoop` → `monomorphize/check/Checker` (~1200 lines) + the unifier
  `monomorphize/unify/Unifier` + the domain `monomorphize/domain/SemValue`, plus the bridge
  `monomorphize/check/UniformCarrierChecker` and the `monomorphize/carrier/` package
  (`Carrier`/`CarrierJoin`/`UniformLadder`). The immediate work uses the **`--uniform-carrier`** gate (`uniformCarrier`,
  distinct from `--effect-channel`); the `effectChannel` flag threading is the separate future bundle (below).
- **What to build** (from the spike, §3): uniform carrier-headed judgments; the classify-by-expected-slot
  ladder (Generic / effect-carrier-form / data-Functor-form); the **join solver** (`Id` = lattice
  bottom, one non-`Id` winner, conflict = mismatch, unsolved-at-boundary = `Id`) replacing
  `EffectLifter.tryIdDefault`-as-an-arm and the `Checker`'s Phase A/B decision-deferral with *deferred,
  decision-free* lift materialization. Keep the `EffectLifter` bind/`pure` insertion *mechanics*
  (`wrapBinds`/`bindWrap`, the `$eff$N` splice) — reshape, don't rebuild. **Status:** the carrier-headed judgments,
  the return boundary, the payload-slot bind/capture, and the `CarrierSlot` pure-wrap are landed; the `Generic`
  pass-through arm and the **join solver's first live use** (the effectful-`catch`-handler) are what remain.
- **The coupled deletion rides here:** deleting `desugarChannel` + the `AbilityResolver` abstain + the
  `AbilityImplementationCheck` relaxation + `EffectChannelDesugarTest` (the §7 leftovers) is only
  possible once the uniform checker monomorphizes an effect-polymorphic value at a **bound carrier**
  (the U3-0b finding, §10). Re-point `EffectAccountingProcessor.contributedEffects` to recover the
  ability from a **resolved impl ref** (module-layer qualifier `AbilityImplementation(name, pattern)` —
  recover the ability *FQN*, not just the name, or the module-precise `derived.diff(declared)` will
  spuriously leak), and **relocate `EffectAccountingTest` to the jvm module** (only track with a
  concrete runtime carrier `IO`; the lang track has none, and `Console` is `Suspend`-riding so can't
  even resolve at the compile-track `Id`).
- **The sharpest constraint (§8):** the carrier attaches to **runtime term judgments only** — never to
  type-level/signature/NbE evaluation. The compile-track `Either` discharge (`CalculatedReturnResolver`)
  stays carrier-free.
- **Gate:** full `lang.test`/`jvm.test`, all example mains, eliot-test, and the 26 spike cases green
  *under the flag* (incl. effectful + user-defined conditionals), byte-identical where behavior overlaps
  the default path; compiler track green (guards + calculated returns). Default path byte-identical
  throughout — the flag keeps it safe.

**To be deleted only at the U4 flip (do NOT touch during U3a):** the `EffectLifter` recognition arms,
`CheckState.ambientCarriers`, `EffectResidualChecker` + its Phase-2 shadow, `CarrierKindChecker`'s
carrier duties. They are the *default* path and must stay live until the flag becomes the default (§7,
second list).

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

**U3/U4 adjustments** (scheduled, §10): (a) **re-point the derivation** — under uniform checking
effect operations arrive in the mono body *resolved* to concrete instance methods, not as abstract
ability refs, so the derivation must recover the ability from an impl reference; **LANDED (Bundle
A, 2026-07-24)**. (b) **the run/discharge subtraction (U4-c-0, §10)** — the derivation counts a
reference only when its carrier instantiation *is* the value's ambient (the positional key↔binder
join, 2026-07-24 investigation); this makes discharge subtraction structural post-mono and
**obsoletes the previously planned synthetic-entry exemption** (the entry has no carrier binder, so
its ambient set is empty and `main::main@[IO]` drops out with no special case). The
carrier-machinery-impl exception (Phase-2 finding: those impls declare effects via carrier
constraints, not rows) is absorbed there too (the constraint-based declared side). (c)
**Diagnostics ordering** — for the friendly message to actually be what the user sees, the
accounting verdict must win over (or preempt) the cryptic carrier-instance resolution failure for
the same leak; note the leaks that die *before* mono facts exist (`AbilityResolver`,
`checkDeclaredPure`) can never be re-voiced by the accounting and need a home on the uniform
checker's boundary (§10 U4-c-0 last bullet).

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
  - **U3a-1 (carrier mechanism, productionised) — LANDED (2026-07-23).** Before touching the ~1000-line
    `Checker`, the U2 spike is ported from its throwaway toy `SType` model onto the real domain as a new,
    unit-tested **`monomorphize/carrier/`** package — the foundation the checker wiring (U3a-2) consumes,
    with **nothing wired in** so the default path is byte-identical (the doc's risk section: "the U2 spike
    and the four-case regression suite bound it"). Contents and the full 27-case acceptance suite are
    detailed in the §0 handover ("U3a-1 — LANDED"). The two non-obvious wins the port surfaced: (i) the
    production `split` needs **no** carrier-constructor set — under uniformity it is purely *positional*
    (peel the outermost head, `Id` ⤳ `Bottom`), because elaboration guarantees carrier-headedness, whereas
    the spike's toy `split` needed `carrierCons` defensively; (ii) the theft the pre-uniform equal-arity
    guard blocked is now an *executable* contrast against the **real** `Unifier` — `?F[List[String]] ~
    List[A]` whole-unified injectivity-decomposes to `?F := List, A := List[String]` (the theft), which
    the ladder avoids by `split`ting `?F` off so only `List[String] ~ List[A]` (⇒ `A := String`) reaches
    payload `unify`. **NEXT is U3a-2** — the checker wiring.
  - **U3a-2a (checker-side bridge) — LANDED (2026-07-23).** `check/UniformCarrierChecker` lifts the U3a-1
    domain mechanism into `CheckIO` and resolves **§12-Q1 to check-time carrier-wrapping** (`intoCarrierHeaded`:
    pure `T` ⤳ `Id[T]`, `VType` never wrapped) + the expected-slot tag classifier + the boundary
    finalize/materialize. Unit-tested in isolation (`UniformCarrierCheckerTest`, the `EffectLifterTest`
    harness); **not constructed/called by the `Checker` yet**, so the default path is byte-identical and the
    `desugarChannel`/accounting knot is untouched. Details in the §0 handover ("U3a-2a — LANDED").
  - **U3a-2b(i)/(i+) (node mechanics + bridge surface) — LANDED (2026-07-23).** `resolveArgumentSlot`,
    `intoCarrierHeadedTerm`, `checkReturnBoundary` — the bridge is feature-complete, still isolation-tested and
    uncalled by the `Checker`. Details in the §0 handover.
  - **U3a-2b(ii) infrastructure — LANDED (2026-07-23, `dd61f027`).** The transitional **`--uniform-carrier` gate**
    threaded end-to-end (`LangPlugin` CLI flag/key → `LangProcessors` → both mono checkers → `TypeStackLoop` →
    `Checker`), and `Checker` **constructs `UniformCarrierChecker`** beside `EffectLifter` — unconditionally but never
    called while the flag is off, so the default path is byte-identical. Nothing routed through the bridge yet.
  - **U3a-2b(ii) wiring slice 1 (the return boundary) — LANDED (2026-07-23, `8fadd27f`).** `checkAgainst` routes the
    **plain pure value return** case through the bridge (`intoCarrierHeadedTerm` + `checkReturnBoundary`), gated by
    `uniformReturnRoutable` (both sides plain non-carrier-headed `VTopDef` value types + payload fits by pure
    definitional equality); everything else falls back to `checkAgainstDefault`. Concrete finding: routing an
    effect-carrier-headed return self-solves its carrier meta (`?F := ?F`) → loop, hence the non-carrier-headed gate.
    Durable gate `UniformCarrierByteIdenticalTest` (jvm.test): the whole base + program compiles byte-identically with
    the flag off vs on (`Int` returns route too — `Int == Int` definitionally, no `Coerce` to reconcile; bounds stay in
    the refinement channel).
  - **U3a-2b(ii) spine wiring slice 1 (the argument slot) — LANDED (2026-07-23, `527af90a`).** `checkArgumentSlot`
    routes the **plain pure-value-into-payload-slot** case through the uniform ladder (`intoCarrierHeadedTerm` +
    `resolveArgumentSlot`), gated by `uniformArgSlotRoutable`; everything else falls back to the default Phase-A logic.
    **Bridge fix:** a pure (`Id`, bottom) actual into a `PayloadSlot` passes its payload directly (`runId`), *not* a bind
    — binding it lets `wrapBinds`/`bindWrap` unify the `Id` carrier with an effectful core's and strip the effect (only
    effectful actuals bind). Whole-base `UniformCarrierByteIdenticalTest` stays byte-identical with the arg path live.
  - **U3a-2b(ii) spine wiring slice 2 (effectful argument) — LANDED (2026-07-23, `16a432f6`).** The argument→payload-slot
    case is complete for both pure and effectful actuals: `uniformPayloadSlot` instantiates the arg (peeling a polytype
    like `readLine`), routes when its payload fits — a pure actual passes (`runId`), an effectful actual **binds**
    (`Bound` → `wrapBinds` → `flatMap`, as the default `tryBindLift`); the default logic is `defaultArgSlot`.
    Byte-identical because an effectful arg's carrier *is* the ambient meta (same as the core), so `wrapBinds`'s
    `doUnify` connects them correctly — the join is only needed for conditionals (different-carrier arms). Test program
    now `label(readLine)` + `label`'s pure return; whole base byte-identical, probe-confirmed effectful `Bound` live.
  - **U3a-2b(ii) carrier-meta self-join guard — LANDED (2026-07-23, `ead5d631`).** `CarrierJoin.join` no-ops a carrier
    joined toward *itself* (`?F` against `?F` — a value's own ambient meeting its declared return), which would else
    write a self-referential cycle and loop `resolve`. A prerequisite for effect-carrier-headed returns and the
    conditionals; isolation-tested, not yet triggered by the checker. **The conditional `CarrierSlot` pure-arm routing
    was TRIALLED and REVERTED — it MISCOMPILES** (`VerifyError` on `if(f,"+") else "-"`: `"+"` reaches `if` as a raw
    `String` where an `AbortCarrier` is expected). Root cause: `if`'s arm carrier `?G` is *ability-constrained*
    (`~ Abort`) and the default path solves it to the concrete `AbortCarrier[Id]`, but the uniform join left `?G` to
    default to `Id`, erasing the `pure` — so **`finalize` must never default an ability-constrained carrier meta to
    `Id`**; ability resolution / the discharge must solve it. Corrected understanding: `if`'s arms are `CarrierSlot`s
    because `if`'s signature declares `value: {Abort} T` (carrier-headed) — no eager generic instantiation needed.
    **NEXT (the first *non-overlap* step — uniform does better than default, needs compile-succeeds tests):**
    the conditionals — `CarrierSlot` pass-join + `Generic` pass-through with the join solver **respecting
    ability-constrained carrier metas** (never defaulting them to `Id`) + `finalizeAndMaterialize`.
  - **U3a-2b(ii) effect-carrier-headed returns — LANDED (2026-07-23, `b35bf80c`).** `uniformReturnRoutable` broadened
    from plain-pure-value returns to any *value* return (`uniformValueReturn` = pure `VTopDef` OR effect-carrier-headed),
    **runtime track only** (§8 keeps the compile-track `Either` discharge carrier-free). So `main : {Console} Unit`'s
    `?F[Unit]` and the synthetic entry's `IO[Unit]` now route through `checkReturnBoundary` (an effectful body passes
    through unchanged — carriers join, a no-op via the self-join guard for `?F` vs `?F` or a union for distinct metas;
    a pure body re-carries via `pure@?F`, `?F` solved at the entry to `IO`, never defaulted to `Id`). Byte-identical
    (whole-base test; probe-confirmed the `IO[Unit]` return routes). The remaining fallback is the conditional arms (a
    flex/carrier *domain*) and the compile-time track.
  - **U3a-2b(ii) conditionals byte-identical — LANDED (2026-07-23, `23eb785a`).** The whole `IfDemo` conditional surface
    (`if`/`else`/`fold`) compiles byte-identically under the flag: conditional **bodies** on the uniform path (return
    boundary + discharger capture), **arms** still on the default ladder. Three fixes (full detail in the §0 "conditionals
    byte-identical — LANDED" bullet): (1) `checkReturnBoundary`'s **discharge-to-pure** arm (`?G[T]` flex body under a
    pure `Id` return ⇒ `Id`-default + `runId`) + `force` before every `Carrier.split`; (2) `uniformPayloadSlot`
    **capture-vs-bind** (an effectful actual whole-unifying the domain — a discharger's `AbortCarrier[G,A]` slot — is a
    capture, deferred to the default unify-first ladder, never bound); (3) arg-slot routing gated to `Platform.Runtime`
    (compile-track bodies stay carrier-free, §8). eliot-test 11/11 byte-identical (was failing under the flag), 32/32
    example mains byte-identical, no regressions.
  - **U3a-2b(ii) CarrierSlot conditional arm — LANDED (2026-07-23, `5864f95f`).** The first *non-overlap* improvement:
    `if(c, None) else Some(x)` (and reversed), which the **default path rejects**, compiles+runs under the flag.
    `checkArgumentSlot` routes an effect-carrier parameter domain (`?G[T]`) through `uniformCarrierSlot`: a **pure**
    actual pure-wraps first (`tryPureWrap`) *before* the default ladder's stealing equal-arity unify (`?G := Option`);
    an **effectful** actual stays on `defaultArgSlot`. Reuses `tryPureWrap` unchanged (the clean single
    `pure@Effect[?G](arg)`) — **not** the `carrierSlotLift` double-wrap `pure(runId(pure@Id(arg)))`, whose inner `pure@Id`
    confused the outer `Effect`-instance resolution and mis-erased it (a `VerifyError` that broke `sign`; the crux was
    the double-wrap, not a `finalize`-to-`Id` issue — so the single-slot `if` arm needs no `CarrierJoin`/`finalize`).
    Byte-identical everywhere it overlaps; new compile-succeeds gate `UniformCarrierConditionalTest`.
  - **U3a-2b(ii) compound-state (payload-fits-first) — LANDED (2026-07-23, `ba208c48`).** The second *non-overlap*
    improvement: an effectful value into a data slot (`items : {Console} List[String]` into `foldLeft`'s `list : List[A]`),
    which the **default path rejects** (the equal-arity unify steals `?F := List`, then `Effect[List]` has no instance),
    now **binds** under the flag. `uniformPayloadSlot` decides bind-vs-capture by whether the actual's **payload genuinely
    fits** the domain (checked *first*, not the whole-unify): a carrier-stack domain's inner value never fits its outer
    carrier (⇒ capture), a data container's element does (⇒ bind). A **bare-flex payload `?A`** is guarded out of "fits"
    (`payloadFitsDomain`) — it absorbs any domain and strips the carrier (a discharger's `raise(err) : ?F[?A]` must
    capture; the guard's absence reddened `Throw.els` with `Effect[String, Id]`). Byte-identical everywhere it overlaps
    (eliot-test 11/11, no regressions); compound-state cases added to `UniformCarrierConditionalTest`.
  - **U3a-2b(ii) GENERIC arm + catch-handler — SCOPING FINDING (2026-07-23, no code landed; tree green).** Both stated
    next candidates were tried empirically and re-scoped (full write-up in §0 "Next"):
    - **The `Generic` (`fold`) arm is DROPPED as a standalone slice.** A `checkArgumentSlot` route for a bare-flex-meta
      domain (`VMeta(_, SNil)`) through `uniformArgumentSlot` (`PassWhole`) + a pure-`fold` case in
      `UniformCarrierByteIdenticalTest` were built, broke, and were reverted. It cannot be a byte-identical regularization
      because (1) a bare-meta domain is *every* generic argument's slot in the base (`Some`/`Pair`/`identity`/`min`, not
      just `fold` — and the uniform design forbids recognising `fold` by name), (2) `PassWhole`'s eager `Id`-wrap commits
      `?A := Id[payload]` for pure args, Id-wrapping the whole base into `Cannot construct infinite type` occurs-check
      failures + regressing two `UniformCarrierConditionalTest` cases, and (3) without the Id-wrap it is *vacuous*
      (`?A := payload` = the default's `resolveGuardedLadder`) while still omitting the **ride-up-vs-bind** check the
      default Phase A/B makes (`resolveDeferredSlot`'s `occursInValue(id, retType)`: transparent `fold` rides up,
      non-transparent `putState` must bind-lift). So the uniform `Generic` arm must carry the ride-up check and is folded
      into the U4 flip, not landed early. The U2 spike's `Generic` test only covered the transparent ride-up.
    - **The effectful-`catch`-handler works with the stdlib delta but is GATED ON U4 (corrected from an earlier,
      malformed-probe finding).** The delta (`onError: E => A` → `onError: E => G[A]`, body
      `flatMap(e -> foldEither(onError, a -> pure(a), e), runThrow(computation))`) was applied and measured case by case:
      it **enables effectful handlers** (`failUnit catch (err -> printLine(err))` compiles+runs on both paths — and the
      signature change is *necessary*, since a pure `onError: E => A` cannot absorb an effectful handler without a double
      carrier), is **backward-compatible for a single discharger** (`printLine(parseOk catch (err -> err))` runs), and its
      `flatMap`+`pure` body is sound in isolation (a pure-signature variant compiles the two-discharger `EffectsThrow`).
      It **regresses exactly one shape on the default path**: two-or-more dischargers with *pure* handlers sequenced in a
      block (`EffectsThrow`/`MinCatch2`), where the new signature forces the pure handler's codomain to lift
      (`pure@?G(err)`) and, over the block-sequenced pinned rows `{Throw[E] | G}`, that lift mis-unifies the shared ambient
      into a stacked `ThrowCarrier` (bogus `Throw[String, IO]` at jvm `Throw.els:54`); `--uniform-carrier` does not fix it.
      **The gating conclusion:** unlike the other three historical fixes (pure-checker improvements, no stdlib change, so
      flag-gatable), this one needs a *user-facing stdlib signature change* that is not flag-gated and so hits both paths —
      regressing the default path via exactly the premature-carrier-commitment stacking the uniform `CarrierJoin` removes.
      So it can only land once the uniform path is the **default** (at/after U4), or behind a throwaway default-checker
      stacking fix U4 deletes anyway. Its sibling join lives at `foldEither`'s **function-typed** arms
      (`onLeft: E => B`/`onRight: A => B`, joined at `B = G[A]`), a function-return join.
    **NEXT: move the effectful-`catch`-handler into the U4 milestone** — land the stdlib delta atomically with the flip
    (when the uniform `CarrierJoin` is the default carrier handling and the stacking cannot occur), with the effectful- and
    sequenced-pure-handler cases as its acceptance tests. Do not attempt it as a pre-flip slice. `CarrierJoin` /
    `finalizeAndMaterialize` stay built-but-uncalled until then. The coupled `desugarChannel`/accounting deletion stays on
    the `--effect-channel` gate and is untangled at U4.
- **U4 — flip and delete.** The flag becomes the default; the §7 flip-deletions land; the §6
  assertion becomes a hard error; the Cornerstone amendment (§9 restatement) and the doc/skill
  sweep (`eliot-code` global skill, `eliot-layers`, CLAUDE.md effect + monomorphize sections);
  LSP/diagnostic rendering verified `Id`-free. The old path is removed, not kept as a mode. **Also
  lands here (moved from a pre-flip slice — finding 2026-07-23): the effectful-`catch`-handler stdlib
  delta** (`onError: E => G[A]`, `flatMap`+`pure` body), which is ready and works but regresses
  sequenced pure-handler catches on the *default* path — so it can only ship once the uniform
  `CarrierJoin` is the default carrier handling. Acceptance: `failUnit catch (err -> printLine(err))`
  runs, and `EffectsThrow` (two sequenced pure-handler catches) stays green.

  **U4 execution plan (grounded in a full code map, 2026-07-23).** A code-state map (flag threading, the
  remaining `--uniform-carrier` fallbacks, and deletion sizes) established the real dependency structure, which
  is **not** "flip a switch": under `--uniform-carrier` the uniform bridge covers only narrow shapes and the
  **default ladder does the bulk of the work**, so the big deletions are *coverage-gated*. The two live
  `uniformCarrier` read sites are `Checker.checkAgainst:248` (return boundary) and `Checker.checkArgumentSlot:956`
  (argument slots); everything else falls back to `checkAgainstDefault`/`defaultArgSlot`. Ordered slices:

  1. **U4-a — complete uniform *coverage* (the gating prerequisite for every deletion).** Nothing that
     legitimately runs on the runtime track may fall back to the default ladder. Genuine remaining gaps (the
     rest are default *by design* and stay — see the note): (i) the **`Generic` arm** (`fold`'s bare-`A`),
     which needs the **ride-up-vs-bind** decision (`occursInValue(metaId, retType)` → pass-through if the meta
     rides the result, else bind — *not* the naive `PassWhole` that Id-wraps every generic argument, the dropped
     finding); (ii) reshaping the **capture / mismatch** fallbacks (`defaultArgSlot`'s capture case,
     `resolveGuardedLadder`/`resolveLadder`) into the uniform ladder so the carrier-stack/pinned "capture" is a
     uniform outcome, not a default hand-off. *By-design default, not gaps* (§8): the whole **compile-time
     track** (`platform != Runtime`, `Checker:309`), `VType`/guard/calc-return/W3, and **function/polytype
     (`VPi`/`VLam`) returns** — heading fires on terminal value leaves only, never a `VPi`.
  2. **U4-b — Bundle A: retire the superseded `--effect-channel` erasure path + re-point accounting — LANDED
     (2026-07-24).** All the deletions, the flag-threading simplification, and the re-point landed byte-identical (see
     §0 "Bundle A LANDED"). Two divergences from the recipe below, both findings: **(a) module recovery is
     `ref.moduleName`, not a separate ability-module lookup** — every effect ability and its instances are colocated in
     `eliot.effect` (a carrier-generic `implement[F ~ E] Ability[F]` must be, and a concrete `implement Inf[IO]` is placed
     there too), so the impl method's module *is* the ability's, confirmed by the ability-marker lookup succeeding there;
     the recipe's "impl lives in jvm while ability is in eliot.effect" concern does not manifest (the module in a
     `ValueFQN` is path-derived, and both layers' `.../eliot/effect/` files resolve to `eliot.effect`). **(b) the HKT
     check reads the ability marker, not the impl marker** — a concrete-carrier impl (`implement Inf[IO]`) has no HKT
     binder of its own, only the ability `Inf[F[_]]` does. **(c) the test was not relocated to a jvm fact-query test** —
     no jvm `ProcessorTest` fact-query harness exists (jvm tests use `Compiler.createSession`), and the derivation is not
     a live verifier yet (U4-c was blocked at the time; since unblocked in design — see U4-c-0 below), so the old test
     was deleted, the pure-row half stays on `EffectAccountingChannelDeclaredTest`, and the derivation test lands with
     U4-c. The original recipe (kept for the U4-c re-check):
     - `EffectSugarDesugarer.desugarChannel` + `eraseAbilityCarrier`/`abilityCarrierName`/`isHigherKindedBinder`
       /`eraseCarrierApplications` + the `rewrite` `stripOpen` parameter and its `EffectfulType(_,_,None) if stripOpen`
       arm; `desugar(function, effectChannel)` ⤳ `desugarCarrier(function)` and `desugar(data, effectChannel)` drop the
       (already-unused, `val _ =`) flag param.
     - `AbilityResolver`'s effect-ability abstain (`isEffectAbilityRef`, the `if (effectChannel) …filterA…` at `:81-90`
       ⤳ `resolvable = unresolved.toList`) + its `effectChannel` ctor param.
     - `AbilityImplementationCheckProcessor` conformance relaxation (`isUserEffectAbility` + the `:54` skip ⤳ always
       `checkSignatures`) + its `effectChannel` ctor param.
     - `EffectChannelDesugarTest` (it pins the deleted effect-blindness).
     - **Flag-threading simplification that falls out:** `effectChannel` was the checker's only use *via* `AbilityResolver`
       (the map's finding), so removing it there drops it from `Checker` → `TypeStackLoop` → both mono processors, and from
       `CoreProcessor`/`AbilityImplementationCheckProcessor`. After Bundle A `effectChannel` is threaded to
       **`EffectAccountingProcessor` only** (its removal is U4-e).
     - **The re-point — the delicate part (definitive mechanism, pinned 2026-07-24).** On the carrier path an effect-op
       reference is NOT left abstract: `PostDrainQuoter.resolveIfAbility` (`:494-508`, SemExpression twin `:232-241`)
       rewrites it to the resolved impl-method FQN carrying `Qualifier.AbilityImplementation(abilityName, pattern)` — so
       `contributedEffects`'s current `EffectMachinery.abilityNameOf` (only matches `Qualifier.Ability`) returns `None`
       and silently under-counts (derives the impl method's empty row). Re-point `contributedEffects` to recognise
       `Qualifier.AbilityImplementation(name, _)`, **but with two caveats that make it not a pure qualifier match**: (a)
       *first-order* abilities (`Show`/`Eq`) resolve to `AbilityImplementation` too, so it must discriminate an **effect**
       ability by the ability *marker*'s HKT carrier binder — the same test `AbilityResolver.isEffectAbilityRef` does
       (`EffectCarriers.isHktBinder` on the marker's `OperatorResolvedValue` signature, machinery excluded), i.e. a **fact
       lookup**, not a pure match; (b) the `AbilityFQN`'s **module** must be the *ability's* module, not `ref.moduleName`
       (the impl lives in the platform layer, e.g. jvm, while the ability is in `eliot.effect`), or `derived` will not
       match `declared` (which `channelDeclaredEffects` sources from the `effectRow`, in the ability's module). Recover the
       ability module from the marker/`OperatorResolvedValue` looked up in (a). **Get this exactly right — a wrong module
       or a missed HKT check silently mis-counts effects and lets a leaking value pass (a fail-safe violation).**
     - **Relocate `EffectAccountingTest`.** Its `main:{Console} Unit` at `EffectAccounting.Key(main, Seq.empty)` cannot
       monomorphize once carrier-desugared (the lang test track has no concrete carrier to bind `F` — `Cannot resolve
       type.`, the U3-0b blocker). It must move to jvm.test and drive a **full compile** where the synthetic main's
       `runMain` binds `F := IO`, then query the accounting fact at the carrier-bound key. Note the semantic shift the
       carrier path forces: `main:Unit = printLine(…)` (its "reject undeclared" case) now errors *during check* via
       `EffectResidualChecker.checkDeclaredPure`, not via `EffectAccounting` — so those assertions belong to the checker,
       not the post-mono accounting; re-express accordingly. (`EffectAccountingChannelDeclaredTest`, the pure
       row-extraction unit test, already lives on the channel package and is unaffected.)
  3. **U4-c — swap the verifier. UNBLOCKED IN DESIGN (investigation, 2026-07-24): build the U4-c-0 subtraction slice
     below first, then wire + prove parity + delete.** The naive plan ("wire a demand for `EffectAccounting.Key`, then
     delete `EffectResidualChecker`") was **built as a probe and reverted**: wiring it made HelloWorld reject its own
     synthesized entry (`def main: Unit = apply(block(User::main), unit)` runs `Console` on `IO`, declares nothing →
     false `{Console}` leak). Root cause of the probe failure: the naive derivation is a bare reference union, blind to
     the **run/discharge subtraction** `EffectResidualChecker` does — (1) an effect *run* on a concrete carrier
     (`Console` on `IO` at the entry boundary; the residual checker's `checkDeclaredPure` fires only on a *committed
     unifier mismatch*, and `IO` absorbs it cleanly), and (2) an effect *discharged* onto an inner transformer carrier
     (`raise` on `Either`/`AbortCarrier`, dropped by the residual checker's "rides the ambient carrier" filter), both
     read as bare `AbilityImplementation` ops and over-count. The blocker's *generalization* ("the ambient-vs-concrete
     distinction is erased post-mono, so accounting cannot subtract") was **refuted by the 2026-07-24 investigation**
     (§0): the distinction survives in the **mono key ↔ signature-binder alignment** and in each reference's own type
     arguments — the derivation was simply discarding them. The re-point of `contributedEffects` (Bundle A) is landed
     and validated (user `main` → `{Console}`); U4-c-0 adds the ride filter it lacks.

     **U4-c-0 — the run/discharge-subtraction slice (the definitive mechanism: port
     `EffectResidualChecker.residualEffects`/`ridesAmbient` as a positional fact join — never shape recognition):**
     - **Ambient reconstruction (positional).** For the value under accounting, read its `OperatorResolvedValue`
       (runtime track), compute `carrierNames = EffectCarriers.carrierBinders(view).filter(paramConstraints.contains)`
       — identical to `EffectResidualChecker.check` — and map each such binder's *index* to
       `MonomorphicValue.typeArguments(i)`: `TypeStackLoop.establishSignature` binds `typeArguments.lift(i)` against
       `binders.zipWithIndex`, so the alignment is guaranteed. The resulting ground-carrier set is the ambient set.
       Empty for the synthetic entry (no carrier binder, pure `Unit` return) — the entry passes with **no
       synthetic-entry exemption needed**. (A codegen-demanded value mono is full-arity; `GroundValue.Param` reaches
       only signature twins — assert, don't assume.)
     - **The ride test.** Stop discarding reference type args (`collectReferences` currently matches
       `MonomorphicValueReference(vfqn, _)` — that discard is the whole gap). For each contributing reference, recover
       *its* carrier instantiation positionally from *its own* `OperatorResolvedValue`'s carrier-binder positions
       (`PostDrainQuoter.resolveAbilityRefs` preserves the impl's type args through the ability→impl rewrite), and
       contribute iff that instantiation is in the ambient set by **exact `GroundValue` equality** — not head equality;
       exact is strictly tighter than the checker's `CarrierHead` head test and correctly separates nested
       same-transformer stacks (`ThrowCarrier[E2, ThrowCarrier[E1, IO]]` ≠ ambient `ThrowCarrier[E1, IO]`), which the
       head test cannot (masked today only by ability-name `AbilityFQN` granularity).
     - **Second arm — concrete-carrier impls.** `implement Inf[IO]`'s methods have *no* carrier binder, so the
       positional join finds nothing; recover the fixed carrier from the impl identity (the `AbilityImplementation`
       pattern / the method signature's return head) and run the same equality. A missed arm here **under-counts — a
       fail-safe violation** ([[feedback_gaps_must_be_failsafe]]); gate with a dedicated rejection test (an undeclared
       `Inf` reaching a `{Console}`-only value must redden).
     - **Declared side switches to constraint-based.** Use `EffectCarriers.declaredEffects(carrierNames,
       paramConstraints)` (as the residual checker does), not only `channelDeclaredEffects(orv.effectRow)`: the lifting
       instances (`implement[S, G ~ Abort] Abort[StateCarrier[S, G]]`) perform on `G` and declare via binder
       constraints with an *empty* surface row — the known carrier-machinery-impl exception (§5). For user values the
       two views coincide.
     - **Pinned-return values — decide, don't inherit.** Today they get no subset check at all (no carrier binder ⇒
       `checkDeclaredPure` ⇒ applied return ⇒ accepted), and a pinned `{… | IO}` body can perform `Console` via the
       promiscuous `Suspend` lift unrecorded. Treat the concrete return carrier as the ambient (mirroring
       `TypeStackLoop.recordConcreteReturnCarrier`) — which also lays the §5 reify-legality foundation: a reference
       whose carrier is a concrete stack *≠ ambient* is a **capture**, and its row-⊆-pinned-entries check happens right
       there.
     - **What accounting can NEVER absorb (goes on the U4-d checklist instead).** Diagnostics for programs that never
       produce mono facts: `State`/`Throw`/`Abort` leaks dying cryptically in `AbilityResolver`, and
       `checkDeclaredPure`'s friendly "declared pure but performs" (it rides `unifier.errors`). Those messages must
       find a home on the uniform checker's boundary *before* U4-d deletes the residual checker, or error quality
       silently regresses.

     Until U4-c-0 lands and the parity gate below passes, **`EffectResidualChecker` stays the live verifier**
     (fail-safe — [[feedback_gaps_must_be_failsafe]]), accounting is unwired + gated. Everything below is the original
     U4-c wiring recipe — still valid, executed *after* U4-c-0. **Wiring finding (2026-07-24): `EffectAccounting` is *not wired
     into the compile pipeline* today** — nothing demands `EffectAccounting.Key` except `EffectAccountingTest`
     (it is a demand-driven `TransformationProcessor`), so despite the "real verification path" framing it currently
     runs *only* in its own test, never on a real compile; `EffectResidualChecker` (unconditional, in-checker) is the
     sole actual verifier. So U4-c is **two coupled steps**, not just a deletion: (a) **wire** a demand for
     `EffectAccounting.Key` for every used value, so it runs alongside `EffectResidualChecker` on all real code; then
     (b) delete `EffectResidualChecker`. **Concrete hook (located 2026-07-24):** the codegen chain reaches each used
     value at `UsedNamesProcessor:87` (`getFactIfProduced(WovenValue.Key(vfqn, typeArgs))`), and the refinement channel
     is reached analogously by `ReconcileProcessor:43` demanding `RefinementTable.Key`. Wire `EffectAccounting.Key` at
     the post-mono seam — `WovenValueProcessor` demanding it (with `getFactOrAbort`, so a leak aborts weaving), or
     `UsedNamesProcessor` — so it becomes a **precondition of codegen** per used value. Step (a) is *also how Bundle A's
     re-point gets verified*: with both verifiers live, the whole example/eliot-test suite must stay green (any
     **over-count** → a red compile, caught immediately), and a dedicated **rejection** test confirms no **under-count**
     before `EffectResidualChecker` is removed — the fail-safe gate the re-point requires. **Blast-radius caveat (why
     this is a dedicated session, not a quick slice):** the re-point fires only on the *carrier* path (under
     `--effect-channel` the ops are still abstract `Qualifier.Ability`, so the `AbilityImplementation` branch is dormant
     there) — so verifying it requires running EffectAccounting on the *default* path, i.e. as a codegen precondition
     over the **whole base**. The re-point must then correctly account **every** base value (dischargers, carrier-machinery
     `Effect`/`Suspend` impl bodies, first-order `Show`/`Eq`/`==` methods that also resolve to `AbilityImplementation`) —
     an open-ended debug against the full base, best done focused. It cannot be validated in isolation (a bounded
     carrier-machinery compile with fact access at carrier-bound mono keys, which no current test harness provides). Its **Phase-2 shadow is already deleted** (`26ce08b2`,
     2026-07-24 — `channelEffectsOf`/`channelDeclaredFor`/`shadowCompareSubset`/`shadowCompareVerdict`/`shadowMarker`
     were purely observational; the pure row-extraction unit test was retargeted to
     `EffectAccountingProcessor.channelDeclaredEffects` as `EffectAccountingChannelDeclaredTest`). What remains is
     deleting the real verifier itself once `EffectAccounting` covers the carrier path (needs Bundle A's re-point) —
     and note `EffectResidualChecker` reads `CheckState.ambientCarriers`, which U4-d deletes, so it *cannot* be the
     kept verifier (this is *why* the row-based `EffectAccounting` is the keeper, not the carrier-constraint one).
  4. **U4-d — delete the default-path machinery** (now dead once coverage is complete): `EffectLifter`'s
     recognition arms (`mustLiftBeforeUnify`/`mustPureWrapBeforeUnify`/equal-arity+guards/`underApplied`
     /`isFlexMeta`, `~123-211`) and `tryIdDefault`-as-an-arm (`~295-316`); `CheckState.ambientCarriers`
     +`recordAmbientCarriers` (+ its `TypeStackLoop` writers); the `Checker` **Phase A/B** deferral
     (`SlotOutcome.Deferred`, `resolveDeferredSlot:772-815`, `assembleSpine`/`rebuildChain`) and the dead default
     ladders (`checkAgainstDefault`/`resolveGuardedLadder`/`resolveLadder`/`resolveFailureLadder`/`defaultArgSlot`);
     `CarrierKindChecker`'s carrier duties; the synthetic main's `apply(block, unit)` → `runMain`. The bind/`pure`
     *mechanics* (`wrapBinds`/`bindWrap`/`tryPureWrap`/`pureWrapNode`/`$eff$N`) **survive** — the uniform ladder
     reuses them.
  5. **U4-e — make it the default + close out.** Remove the `uniformCarrier`/`effectChannel` flags and their
     threading (LangPlugin/LangProcessors → both mono processors → `TypeStackLoop` → `Checker`/`AbilityResolver`);
     land the **effectful-`catch`-handler stdlib delta** atomically (now the stacking cannot occur); turn the §6
     Id-residue assertion into a hard error; the §9 Cornerstone amendment + doc/skill sweep; verify LSP rendering
     `Id`-free.

  Slice 2 (Bundle A) **is landed** (2026-07-24). Slice 3 (U4-c) turned out **not** mechanical, but is no longer blind:
  it is "build U4-c-0 (the key↔binder-join subtraction, fully specified above), wire both verifiers, prove parity,
  *then* delete." U4-c-0 is independent of U4-a coverage and can be built next — it runs on the *default* path's mono
  output and needs no uniform-checker progress. Slice 1 (U4-a coverage) remains the gating prerequisite for U4-d.
  Gate every slice with the existing harness (`lang.test`/`jvm.test`, `UniformCarrierByteIdenticalTest`, HelloWorld,
  eliot-test 11/11, the 32 example mains). The whole flip stays a coupled bundle *on `--effect-channel`* (§0 finding),
  which is why the transitional `--uniform-carrier` gate carries slices 1–4 and only slice 5 unifies the flags.
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
   payload metas are separate namespaces; the payload occurs-check never sees a carrier meta). The
   sub-item that carried into U3a — whether pure *signatures/terms* are brought into carrier-headed form by
   a desugar rewrite or by check-time wrapping — is **RESOLVED (U3a-2a): check-time wrapping**
   (`UniformCarrierChecker.intoCarrierHeaded`, a pure `T` ⤳ `Id[T]` in the `Checker`). Chosen over a
   `core`-phase desugar rewrite because it localises the uniform elaboration to the `Checker` (the flip's
   home), leaves `EffectSugarDesugarer` and the surface untouched, and keeps clear of the `desugarChannel`
   deletion; the "is it already carrier-headed?" read it needs is the positional read of the value's own
   `ambientCarriers`/`carrierRoles` (via the reused `effectCarrierSplit`), never the undecidable
   arbitrary-type carrier query.
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
(effect-blind desugar, weaver monadification, both stopgaps) are deleted across U3 as they separate —
the weaver at U3-0a, the effect-blind desugar folded into U3a/U3c (§10 U3-0b), not all at U3 start;
their byte-identical/flag discipline and their negative result are what earned this decision, and the
durable v1 assets — the row channel, the accounting verifier, the `WovenValue` codegen seam,
`runMain`, the shadow methodology — carry over intact (§0).
