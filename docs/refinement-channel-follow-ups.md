# Refinement Channel & Escalation: Follow-Ups

Status: **OPEN (2026-07-16).** Supersedes and retires `docs/refinement-channel-transfer-reduction.md` — the
transfer-reduction plan, **complete** 2026-07-16 (all six steps landed, commits `8f642672..bc374388`; the full
step-by-step record lives in those commit messages and the deleted doc's git history). This doc carries (a) the
retired plan's durable results, which code comments cite (§1), and (b) the follow-up work: the 2026-07-16
post-landing review's findings plus the retired plan's still-open deferred items, each with its rationale.

## 1. Record: what the retired plan established (durable — cited by code)

- **The repeal.** "A `^Meta` transfer must bottom out at natives" is repealed. A transfer and a merge are
  recognised solely by the callee's `^Meta` companion, and the companion's body may route through ordinary
  ability instances (`Numeric[Interval[T]]`, `Meta[Interval[T]]` in `stdlib/eliot-compiler/…/Interval.els`);
  the plain-function indirections (`intervalAdd`/…/`intervalJoin`) are deleted.
- **The mechanism.** `monomorphize/processor/EscalatingReducer.scala` is the compiler platform's
  post-monomorphize **linker-executor** — the demand-driven `used`-equivalent the runtime platform has in
  used+codegen linking. `reduceApplied` seeds evaluation with the cheap one-hop raw closure and, when the result
  is *reducibly stuck*, escalates: it fetches each stuck callee **reduced at its own instantiation**
  (`ReducedBindingClosure.reduceInstance(deep = true)`, `activeFactKeys`-cycle-guarded, memoized) and
  re-evaluates. The loop skeleton (`escalatingLoop`/`reducibleStuck`/`escalate`) is the **one** mechanism shared
  by the in-checker read-back (`PostDrainQuoter`) and the channel executor. Monomorphic (empty-type-arg) callees
  escalate too; the former `groundArgs.nonEmpty` gate was guard-tower conservatism, not a correctness need.
- **The invariant, in its implemented (honest) form: "raw until visibly stuck".** Escalation links only
  monomorphized bodies; a raw (operator-resolved) body is evaluated only until it visibly stalls, at which point
  it is replaced by the monomorphized form. The retired plan's absolute statement — *post-monomorphize evaluation
  may only ever see monomorphized bodies* — is the **target**, not the current state: the one-hop raw seed still
  evaluates raw bodies whenever they reduce without dispatch, and that is currently **load-bearing**
  (`byteMin`/`byteMax` reduce only via the raw path — their own `CompilerMonomorphicValue` would fail the check,
  §2.4). Tightening to the strict form is gated on §2.4. Enforcement today catches everything that *stalls*
  (ability dispatch, effect machinery); it cannot catch a checked-vs-raw elaboration divergence that still
  reduces (an inserted `Coerce`) — theoretical today, since meta-domain code (BigInteger/Interval/Bool) needs no
  coercions, but it is the structural difference from codegen, which never sees raw bodies.
- **The base-binding corollary** (`CompilerNativesProcessor.runtimeConcrete`): a refinement artifact
  (`Qualifier.Meta` companion, or a `Meta[<T>$Meta]` impl) is never treated as runtime-concrete even though it is
  physically emitted (dead) in the runtime pool — it contributes its monomorphized `CMV.reduced` form, never the
  raw body whose inner ability stays abstract and stalls. Same invariant, applied to the base binding a
  post-monomorphize reduction closes over.
- **5b record.** A *dotless* inline transfer brace (`{range(a) + range(b)}`) narrows end-to-end; the *dotted*
  `{a.range + b.range}` fails **loudly** at operator-precedence resolution (`.` and `+` have no declared relative
  precedence), correctly aborting the value — fail-safe, not silent. Locked by
  `InlineTransferBraceIntegrationTest`.
- **Rejected alternative** (still rejected): per-value monomorphization keys (companions keyed by value-metas).
  Metas are normal value parameters; keying monos by values is a category error the runtime track never commits.

## 2. Fail-safe follow-ups (correctness — ordered by priority)

### 2.1 Higher-order `where` escape — **CONFIRMED silent-accept gap**

**Evidence (probe-verified 2026-07-16):**

```
def useByte(x: Int): Int where withinByte(range(x)) = x
def call(f: Int => Int, x: Int): Int = f(x)
def main: IO[Unit] = printLine(intToString(call(useByte, 1000)))
```

compiles **and runs** (prints `1000`), while the direct `useByte(1000)` is correctly rejected ("precondition …
is not satisfied").

**Rationale/diagnosis:** `RefinementChannelProcessor.checkWhere` fires only when a full application's head is a
`MonomorphicValueReference`. A bare reference to a `where`-bearing def in *argument position* lands in
`walkFlow`'s ⊤ arm (no check), and inside the receiving combinator the call head is a `ParameterReference`, so
the demand is never made anywhere. This violates the Use-Site Verification cornerstone's "every manifest use is
checked" and the gaps-must-be-fail-safe rule — it is a silent acceptance of a violated precondition, the exact
class the channel's ⊤-fails-closed design (`demandPrecondition` hard-errors on an unknown range) exists to
prevent.

**Fix:** hard-error on any reference to a `^Where`-bearing def that is not the head of a full application ("a
def with a `where` precondition cannot be passed as a value") — a cheap `UnifiedModuleNames` membership test for
the `whereCompanionName` in the channel walk's value-reference arm (and the partial-application arm of
`checkWhere`). Conservative and loud; lifting the restriction later (demanding the precondition at the eventual
call through the function value) would need value-level tracking the channel deliberately does not have.

### 2.2 The channel does not run over compiler-track code (carried from the retired plan §6)

A `where`-guarded def called *from* an `eliot-compiler` overlay body is never demanded — the channel walks
runtime `MonomorphicValue`s only. Same failure class as §2.1 (a manifest use whose precondition is never
demanded), different entry path. Fix direction: walk `CompilerMonomorphicValue` bodies with the same
`checkWhere` (records can be discarded; only the demand matters), or reject `where`-bearing callees from
compiler-track bodies until then.

### 2.3 FQN-keyed escalation bindings — latent wrong-splice hazard

`EscalatingReducer.escalate` (and the in-checker candidate builder) does `candidates.distinctBy(_._1)`, and both
the spliced-binding map and the evaluator lookup are keyed by **FQN only**. If one stuck term references the same
FQN at two *different* ground instantiations, one instantiation's reduced body — with its ability-impl choices
baked in — serves both; in the channel path the winner is arbitrary (`ReducedBindingClosure.valueReferences`
returns a `Set`, so `.toSeq` order is hash-driven). **Rationale for acting despite no observed failure:** a
cross-spliced body usually sticks on a shape mismatch and degrades to ⊤/stuck (sound), but a same-shape pair —
Bool-valued predicates, guard-selected impls over one type shape — could *quote a wrong result silently*: a
wrongly-narrow interval or a wrong guard verdict, exactly the "behavioral widening must never become wrong
narrowing" risk the retired plan's §7 named. Pre-existing (inherited from the guard tower), but the channel
raised its stakes. **Fix (fail-safe, cheap):** when the candidate list contains one FQN at ≥2 distinct
ground-arg vectors, decline to escalate that FQN — the term stays stuck (loud error on the checker path, sound ⊤
on the channel path) — never splice an arbitrary winner. Keying the binding map by (FQN, args) is *not* the cheap
fix: the evaluators' lookups are FQN-only by design.

### 2.4 Compiler-track literal ascription (carried Step-4 follow-on; **blocker for the strict §1 invariant**)

A compiler-track `def x: BigInteger = <literal>` fails its own check when its `CMV(x, [])` is demanded:
check-mode types `integerLiteral[n]` as `Int`, never ascribing it to the declared `BigInteger`. Today this is
latent (a *loud* spurious rejection, not triggered — `byteMin`/`byteMax` reduce via the raw seed before
escalation ever demands their CMV), but it has two costs: any future escalation that reaches such a def rejects
a correct program, and it is **the** reason §1's invariant must stay "raw until stuck" rather than the strict
"monomorphized always". Fix by compiler-track literal ascription (a literal in checked position adopts a
declared `BigInteger` type), or by giving `byteMin`/`byteMax` genuinely-`BigInteger` bodies; then re-evaluate
tightening the seed (e.g. preferring `CMV.reduced` over raw wherever it exists).

### 2.5 `reducibleStuck` abstract-ability hardening (carried Step-4 follow-on, sharpened)

A stuck *abstract-ability* application quotes to a `GroundValue.Structure`, so `reducibleStuck` treats it as
non-stuck — after the linker fix this path should be unreachable (abilities are resolved before the channel sees
them), but if it ever fires it yields a bogus structure rather than escalation/⊤. The retired plan deferred this
because `reducibleStuck` is shared with the in-checker read-back and changing it risks the checker for no live
gain. **Sharper fix that removes the deferral's cost:** make the stuck-predicate a parameter of
`escalatingLoop`, so the *channel* instantiation hardens (stuck abstract ability ⟹ escalate, else ⊤ — never a
structure) while the checker instantiation keeps today's predicate byte-for-byte.

## 3. Simplifications / hardening (mechanical, behavior-preserving)

### 3.1 Remove `PostDrainQuoter`'s default no-op `reduceInstance`

`PostDrainQuoter.scala:50` defaults the escalation fetch to `(_, _) => None`. There is exactly one production
construction site (`TypeStackLoop.scala:349`) and it always passes the real fetch. **Rationale:** the default is
a silent-degradation trap — a future construction site that omits the parameter gets an escalation loop that
can never escalate: quiet behavior loss (terms that should reduce stay stuck), no compile error, nothing to
grep for. Make the parameter required.

### 3.2 Migrate the marker-guard reader onto `escalatingLoop` (carried from the retired plan §6)

The ability-guards marker reader still fetches sub-values one-hop (`reduceInstance(deep = false)` by recorded
choice) — the last reader on the old linking discipline. **Rationale:** stuck-driven escalation gives it the
same behavior without the eager-deep gotcha the one-hop choice was guarding against (spurious per-instantiation
resolutions at defaulted-`Type` arguments — laziness only monomorphizes what evaluation demands), and retiring
the last `deep = false` caller lets `ReducedBindingClosure.reduceInstance`'s two-mode `deep` flag become
unconditional, deleting the duality and its gotcha-laden docstring.

### 3.3 `ImplementBlock`'s impl-method rebuild → `f.copy(...)`

Step 5a fixed the silently-dropped `returnMeta`/`whereClause` by adding them to the positional rebuild
(`ImplementBlock.scala:58`). **Rationale:** the fixed-field rebuild is the *bug class*, not just the two fixed
fields — it still silently drops `doc` (impl methods lose their apidoc), `dischargedEffects` (an impl method
declaring `{…, -E}` loses the discharge — conservative/loud downstream, but the same trap), `fixity`, and
`precedence`, and every future `FunctionDefinition` field re-opens it. Rebuild with `f.copy(name = …,
genericParameters = …, visibility = Visibility.Public)` so fields forward by default and only deliberate
overrides are spelled out.

### 3.4 Cross-reference the two "monomorphized bodies only" mechanisms

`CompilerNativesProcessor`'s eager `Leaf` contribution (for checking-time evaluation, which is pure and cannot
demand facts mid-eval) and `EscalatingReducer`'s lazy escalation (read-back and channel) both implement §1's
invariant at different seams. **Rationale:** they cannot be merged (the evaluator purity boundary), and without
a sentence in `EscalatingReducer`'s class doc saying so, a future simplification pass will try. One comment,
no code.

### 3.5 `PostDrainQuoter` entry-point consolidation (low priority)

Seven quote/reduce entry points with near-miss semantics (`quoteSem`/`quoteSemOption`/`reduceSemExprToGround`/
`reduceSignatureToGround`(+`Option`)/`reduceSourced`/`quoteSourced`). The aborting variants are the `Option`
variants plus an error at the call site. Sanctioned signature-unification survivors, so consolidate
opportunistically — an API-surface cleanup, not a correctness item.

## 4. Deferred / language decisions (carried from the retired plan, unchanged rationale)

- **Operator-level narrowing:** the channel is intra-procedural and the generic `+`/`Numeric[Int]::add` carries
  no `^Meta` companion, so `x + y` through the operator does not narrow — only direct companion-bearing calls
  do. The transfer-reduction landing made the fix *expressible* (a brace on the generic `+`/its impl methods can
  now contain ability calls and would reduce, via §1's mechanism + the Step-5a `ImplementBlock` forward); wiring
  companions onto the operators is its own feature with its own tests.
- **`.` vs `+` precedence:** making the dotted `{a.range + b.range}` parse means declaring `.` above the
  arithmetic operators (member access binds tighter than `+`) — a deliberate language decision, not a fail-safe
  fix; today's behavior is a loud rejection.
- **Narrow-return representation across a user-def call boundary:** a transfer brace on a *user* (non-native)
  def crashes at runtime when its narrowed return crosses a call boundary (the callee body compiles once with ⊤
  params ⟹ wide representation the brace-narrowed caller does not match). A backend
  representation-reconciliation matter; loud (a crash, never a silent wrong result) and not reachable from the
  shipped layers, whose braces sit only on native leaves.
