# The Refinement Channel's Transfer Reduction: Link Monomorphized Callees (the Compiler Platform's Missing `used` Step)

Status: **PLAN (2026-07-16).** Converts the same-day investigation writeup ("why `^Meta` transfers must bottom
out at natives") into a plan that *repeals* that rule. The investigation's evidence is retained (§1–§2); the
diagnosis is sharpened by the 2026-07-16 architecture review (§3): the "natives only" restriction is not a
property of the evaluator or of the monomorphizer — it is a **linking bug**. The channel evaluates compile-time
code linked against *raw* (pre-monomorphization) callee bodies, so ability references that every mono correctly
exchanges for exact implementations get smuggled back in at link time. The fix is to give the compiler platform
the post-monomorphize step the runtime platform already has: **link only monomorphized callees, transitively,
demand-driven** — after which the bare NbE `force` is exactly as legitimate an executor as the JVM is for
codegen output.

The invariant this plan enshrines (the compiler-platform twin of "codegen links only monomorphized callees"):

> **Post-monomorphize evaluation may only ever see monomorphized bodies.** A raw (operator-resolved)
> body may appear only beneath a native leaf or inside a monomorphization; it must never be spliced into a
> post-mono reduction.

## 1. The two stages of a range transfer (retained evidence)

The refinement channel (`monomorphize/channel/RefinementChannelProcessor.scala`) is a post-pass over each
runtime `MonomorphicValue`. Computing one arithmetic node's result range happens in two stages:

- **Stage 1 — the companion's own monomorphization.** A return brace `def add(…): Int {expr}` desugars
  (`MetaTransferDesugarer`) into the `^Meta` companion `add^Meta(a: Int$Meta, b: Int$Meta): Int$Meta = …`, an
  ordinary compiler-track value monomorphized by `CompilerMonomorphicTypeCheckProcessor → TypeStackLoop` — the
  same unified checker as every runtime body and signature twin, with full ability resolution. Its own body's
  ability references are drained and rewritten to exact impls at read-back
  (`PostDrainQuoter.resolveAbilityRefs` + `Track.Compiler.implBindings`). **Stage 1 is correct and stays.**

- **Stage 2 — the channel's application.** At a companion-bearing call the channel fetches the reduced body
  (`ReducedBindingClosure.reduceInstance`, **one-hop, `deep = false`**), applies the concrete operand metas, and
  runs a **bare `Evaluator.force`** (`metaViaCompanion`; same pattern in `demandPrecondition` for `^Where`).
  `force`/`renormalize` unfold cached bodies and (re-)fire natives; they have — correctly, per the one-evaluator
  cornerstone — **no ability rule**. Stage 2 is where the plan intervenes.

Because the companion's parameters are abstract during stage 1, the per-call computation is structurally forced
into stage 2. Under today's stage 2, anything on that path must reduce without dispatch — hence the "must bottom
out at natives" rule, and hence the `intervalAdd`/`intervalSubtract`/`intervalMultiply`/`intervalJoin`
plain-function indirections in `stdlib/eliot-compiler/eliot/lang/Interval.els`.

## 2. The observed failures (retained evidence)

- `Numeric[BigInteger]::add` fires in stage 2 (native intrinsic bound at the ability-method FQN);
  `Numeric[Interval[T]]::add` does not (an Eliot `implement` instance needing dispatch) — stays stuck, transfer
  quotes to nothing, result ⊤.
- Wrapping the ability call in a plain function (`intervalAddViaAbility`) makes stage 1 succeed but stage 2
  still yields ⊤; adding the instance to the compiler overlay does not help. Availability was never the blocker.
- Written **inline** as `{a.range + b.range}` the companion fails earlier: its compiler-track `SaturatedValue`
  is never produced (stage-1 entry never fires; the plain-function form does). Still undiagnosed — see Step 5b.
- `ImplementBlock` (`ast/fact/ImplementBlock.scala:58-65`) rebuilds impl methods without `returnMeta`: a
  transfer brace on an `implement` method parses and is **silently dropped**. See Step 5a.
- Discriminating probe (the only test that observes narrowing — value-printing tests pass either way):

  ```
  def useByte(x: Int): Int where withinByte(range(x)) = x
  def main: IO[Unit] = printLine(intToString(useByte(add(40, 40))))
  ```

  Narrowing transfer ⟹ compiles; broken transfer ⟹ "value range is not known".

## 3. The corrected diagnosis: it is the linker, not the mono

"There are no abilities in expressions at the end of the pipeline" is true — but it is a **transitive-closure
property, and each monomorphization only de-abilifies its own body.** When a body calls `+[Interval,Interval]`,
the `Numeric::add` ability reference is not in that body; it is in `+`'s body, and it is exchanged for the exact
impl inside `+`'s **own** mono at that instantiation. On the runtime track the closure property is established
by **`used` + codegen linking**: every reachable callee's `MonomorphicValue` is demanded at its concrete
instantiation, and codegen links *those monomorphized forms*. Raw bodies never reach execution.

The channel violates exactly this at link time: `ReducedBindingClosure.collectBindings` with `deep = false`
closes the companion's (correct, ability-free) reduced body over each callee's **raw `NativeBinding` body** —
the pre-monomorphization, operator-resolved form in which constraint-covered ability references are still
abstract. The stuck abilities of §2 all come from these spliced raw bodies, never from the companion itself.
Meanwhile the checker's own escalation read-back already links the other way — `TypeStackLoop.scala` passes
`reduceInstance(_, _, deep = true)`, and `PostDrainQuoter.reduceWithEscalation` fetches a stuck reference
**reduced at its own instantiation**. Two linking disciplines; the channel got the wrong one.

Corollary: **no new dispatch capability is needed anywhere.** Dispatch stays where the pipeline put it — inside
each value's monomorphization. The executor only has to guarantee it never evaluates a raw body. §7 of the
original writeup framed the fix as "a dispatch-aware reduction"; that framing is hereby corrected — the channel
needs the *linker* the rest of the pipeline already uses, i.e. the compiler platform's demand-driven
`used`-equivalent.

## 4. Rejected alternative: per-value monomorphization keys

Considered and rejected: shaping companions with erased value-kinded *type* binders
(`add^Meta[A: Int$Meta, B: Int$Meta]`) so the channel demands `CompilerMonomorphicValue.Key(companion, metas)`
per call site (the ability-guard-verdict / `integerLiteral[V]` pattern). Rejected because metas are **normal
value parameters** and should stay so: the runtime track also monomorphizes at type arguments only and lets
values flow later — keying monos by values is a category error the runtime side never commits, and a fact-space
cost with no compensating need once §3's linking fix restores the closure property. (`integerLiteral[V]` is not
a precedent: `V` is genuinely a compile-time constant in the language.)

## 5. The plan

### Step 1 — extract the stuck-driven escalation loop into a standalone linker-executor — **DONE (2026-07-16)**

Landed as `monomorphize/processor/EscalatingReducer.scala`: `reduceApplied(vfqn, typeArgs, argMetas)` plus the shared
loop skeleton `escalatingLoop(metaStore, eval, fetch)` + `reducibleStuck(forced, metaStore)` + the escalation-fetch
helper `escalate(candidates, already, reduceInstance, wrap)`. `PostDrainQuoter.reduceWithEscalation`/`escalationBindings`
were refactored onto that skeleton (its private `reducibleStuck` deleted; the in-checker fetch pre-quotes its
`SemExpression` type args to ground candidates and passes `absorbTypeArgs` as `wrap`, exactly preserving prior
behaviour — the `MonomorphicExpression` path passes identity since `MonomorphicEvaluator` already drops type args). The
`groundArgs.isEmpty` skip now lives in the shared `escalate`, so Step 2's gate-open is a one-line change benefiting both
instantiations. `ReducedBindingClosure.collectBindings`/`valueReferences` widened to `private[processor]`. `reduceApplied`
is wired into the channel in Step 3; it is created-but-unused after Step 1. Full suite green (eliotc 133, lang 233, jvm
283, ide.lsp 383) + HelloWorld builds and runs; no behaviour change (pure extraction).

A single-owner component beside `ReducedBindingClosure` (e.g. `monomorphize/processor/EscalatingReducer.scala`)
exposing roughly `reduceApplied(vfqn, typeArgs, argMetas: Seq[SemValue]): CompilerIO[Option[GroundValue]]`:

1. fetch `CompilerMonomorphicValue(vfqn, typeArgs)`'s reduced body (a `MonomorphicExpression`);
2. bindings₀ = the one-hop raw closure (today's `collectBindings`) — the cheap common case pays nothing new;
3. evaluate via `MonomorphicEvaluator(lookup = bindings)`, apply `argMetas`, `Evaluator.renormalize` (re-fires
   natives; empty metastore);
4. if the result quotes, done; if it is *reducibly* stuck (same `reducibleStuck` exemptions as
   `PostDrainQuoter`: a runtime `VLam` and a value-parameter neutral are legitimately structural), fetch each of
   the body's value references **reduced at its own ground type arguments**
   (`reduceInstance(fqn, args, deep = true)`, `activeFactKeys`-guarded), splice, re-evaluate; loop until it
   quotes or no new binding is available.

This is a near-verbatim extraction of `PostDrainQuoter.reduceWithEscalation`'s loop shape, freed from the
checking session (standalone it needs no metastore, no drain `abilityResolutions` — every fetched reduced body
already has its drain rewrites baked in — and no `monoEnv`). Extract the loop skeleton parameterized by (eval
function, escalation fetch) and instantiate it twice, so `PostDrainQuoter`'s in-checker loop and this one are
**one** mechanism, not siblings. Stuck-driven (lazy) rather than eager `deep = true` everywhere is deliberate:
it avoids the recorded defaulted-`Type` spurious-resolution gotcha (`ReducedBindingClosure` docstring) by only
ever monomorphizing what evaluation actually demands.

### Step 2 — close the monomorphic-callee escalation gate — **DONE (2026-07-16)**

Removed the `groundArgs.isEmpty` skip from the shared `EscalatingReducer.escalate` (Step 1 had already moved the gate
there, so this one-line change benefits both instantiations — the in-checker read-back and the channel executor).
**Archaeology:** the gate entered in `b104bba6` (signature-unification Phase A), where the escalation loop was built for
the **guard tower** — whose candidates (`else`/`guardOr`/`catch`) are all carrier-generic, so an empty-arg (monomorphic)
reference never needed escalation. It was conservatism, not a correctness requirement. Opening it is sound: escalation
only ever fires on a `reducibleStuck` term (splicing more reduced bodies can make it quote, never change an
already-quoting result), a native leaf / `data` constructor still returns `None`, `wrap(0, sem) = sem` splices a
monomorphic reduced body directly, and the `CompilerMonomorphicValue.Key(fqn, [])` ancestor check still guards cycles.
Verified: full suite green (eliotc 133, lang 233, jvm 283, ide.lsp 383) + **all 34 examples build** end-to-end;
WherePrecondition runs `100`, Ranges runs `21` (known-good regression outputs).

Original text: `PostDrainQuoter.escalationBindings` only escalates references with **non-empty** ground type args
(`groundArgs.nonEmpty`, `PostDrainQuoter.scala:218`), so a *monomorphic* stuck callee is never fetched at its
`CMV(fqn, Seq.empty)`. Runtime-abstract ability-performing monomorphic values are covered by
`CompilerNativesProcessor`'s reduced `Leaf` contribution, but a runtime-*concrete* (borrowed-body) monomorphic
callee with ability calls has neither. Extend escalation to empty-arg references (in the shared skeleton, so
both instantiations benefit); first do the archaeology on why the gate existed (likely conservatism inherited
from the guard machinery's one-instantiation-per-FQN assumption) and keep the full suite green.

### Step 3 — route the channel through the executor — **DONE (2026-07-16)**

`metaViaCompanion` and `demandPrecondition` now call `EscalatingReducer.reduceApplied` instead of
`ReducedBindingClosure.reduceInstance + foldLeft(applyValue) + Evaluator.force`; the bare-force application (and its
`MetaStore`/`Quoter` imports) is deleted. `checkWhere` no longer fetches the `^Where` companion's reduced `SemValue`
separately — it keeps only the `MonomorphicValue` fetch for the arity gate, and `demandPrecondition` reduces the
companion through `reduceApplied` (so an existing-but-unreducible companion now fails loudly via the "Cannot evaluate"
arm rather than silently skipping — strictly more fail-safe; no test relied on the skip). The channel is still a post-pass
that never mutates checker state — it only demands more facts (memoized, `activeFactKeys`-cycle-guarded); its "zero risk
to the checker's invariants" separation survives.

Verified — **no regression** (this is a mechanism swap; the *narrowing payoff* is Step 4): full suite green (eliotc 133,
lang 233, jvm 283, ide.lsp 383) + all 34 examples build & run (Ranges 21, Arithmetic 14, WherePrecondition 100, …).
`WhereOnDefIntegrationTest`'s literal-through-`where` narrowing (`useByte(42)` passes, `useByte(1000)` rejected,
`useByte(y)` unknown) all flow through `reduceApplied` and pass. The §2 **discriminating** ability-form probe
(`useByte(add(40, 40))` ⟹ `Interval[80,80]`) is deferred to Step 4 as intended: it needs the `Int` vessels' transfers
rewritten to route through `implement Numeric[Interval[T]]`; today an arithmetic result still reaches `where` as ⊤
(`40 + 40` ⟹ "value range is not known"), which is the §6 operator-level non-goal (the generic `+`/`Numeric[Int]::add`
carries no `^Meta` companion), unchanged by this step.

Original text: `metaViaCompanion` and `demandPrecondition` call `reduceApplied` instead of
`reduceInstance + foldLeft(applyValue) + Evaluator.force`. Delete the bare-force application. The channel
remains a post-pass that never mutates checker state — it only demands more facts (memoized, cycle-guarded);
its "zero risk to the checker's invariants" separation survives with that re-justification.

Verify: the §2 probe with the transfer routed through `implement Numeric[Interval[T]]` narrows
(`add(40, 40)` ⟹ `Interval[80,80]`, `useByte` passes); the ability-form no longer reports "value range is not
known"; full suite + examples green.

### Step 4 — make the stdlib transfers idiomatic; delete the plain-function indirections

The payoff, and the proof the rule of §1 is repealed:

- give the compiler pool the `Numeric[Interval[T]]` instance **bodies** (the pure endpoint-wise delegation —
  either as the sanctioned overlay copy in `stdlib/eliot-compiler/eliot/lang/Interval.els`, or hoisted into the
  base split instance if the layer-merge rules allow a platform-independent body there);
- rewrite the `Int` vessels' braces to the idiomatic form (`{range(a) + range(b)}` / `add(range(a), range(b))`)
  and, once Step 5a lands, move them onto the impl methods where they belong;
- route the auto-derived `Meta[Int$Meta]` join through the `Meta[Interval[T]]` instance's own `join`;
- **delete** `intervalAdd`, `intervalSubtract`, `intervalMultiply`, `intervalJoin` and the three overlay
  comments that state the natives-only rule; update `RefinementChannelProcessor`'s class docs likewise.

### Step 5 — independent fail-safe front-end fixes

- **5a.** Forward `returnMeta` in `ImplementBlock`'s method reconstruction (`ImplementBlock.scala:58-65`,
  one-liner) so a transfer brace on an impl method generates its companion instead of silently vanishing; add a
  test asserting the companion exists.
- **5b.** Diagnose the inline-brace stage-1 failure (§2: inline `{a.range + b.range}` never produces the
  companion's compiler `SaturatedValue`). Start from a failing test; silent non-production of a fact is a
  fail-safe violation regardless of this plan.

### Step 6 — sweep and bump

Update the stale rule statements (this doc's referrers, the `eliot-layers`/channel-adjacent comments citing
"only natives re-fire transitively"), and bump `CACHE_VERSION` (channel results change for previously-⊤ nodes).

## 6. Non-goals / deferred

- **Operator-level narrowing** (original §6 finding): the channel is intra-procedural, so `x + y` through the
  generic `+` (no `^Meta` companion) still does not narrow — only direct companion-bearing calls do. This plan
  makes the *fix* expressible (after Steps 3+5a a brace on the generic `+`/its impl methods can contain ability
  calls and would reduce), but wiring companions onto the operators is its own follow-on with its own tests.
- **The channel does not run over compiler-track code**: a `where`-guarded def called *from* an
  `eliot-compiler` overlay body is never demanded (the channel walks runtime `MonomorphicValue`s only). A real
  fail-safe gap, orthogonal to transfers; track separately.
- **The marker-guard reader's one-hop fetch** (`deep = false` by recorded choice) stays as-is in this plan;
  revisit once the shared skeleton exists, since stuck-driven escalation would give it the same behavior without
  the eager-deep gotcha.

## 7. Risks

- **New fact edges from the channel** (`channel → CompilerMonomorphicValue` at escalated instantiations):
  already exist via `reduceInstance`; cycle safety is the existing `activeFactKeys` guard; cost is memoized.
- **Escalation at empty type args** (Step 2) touches the checker's own read-back path — the reason it is a
  separate, individually-verified step.
- **Behavioral widening, not narrowing**: every change makes previously-⊤ nodes narrow; ⊤ was sound, so a
  regression here can only be a *wrongly narrow* interval — the probe suite must therefore include a case
  asserting a correct non-narrowing (an unknown operand stays ⊤) alongside the narrowing cases.
