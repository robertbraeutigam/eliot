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

Status: **§2.1, §2.2, §2.3, §2.5 LANDED 2026-07-16** (this commit). §2.4 **deferred** (see its note — it is an
already-loud, untriggered spurious rejection, not a silent-accept, and its fix is coupled to the strict-§1 tightening it
unblocks). CACHE_VERSION 29→30 (channel/escalation semantics changed).

### 2.1 Higher-order `where` escape — **CONFIRMED silent-accept gap** — **DONE**

**Fixed** in `RefinementChannelProcessor`: a bare `MonomorphicValueReference` to a `^Where`-bearing def (walk's
value-reference arm) and a *partial* application of one (`checkWhere`'s sub-arity arm) now hard-error — "a def with a
`where` precondition cannot be passed as a value". Membership is the same cheap `UnifiedModuleNames` `^Where` lookup the
full-application check already does. Locked by two `WhereOnDefIntegrationTest` cases (bare higher-order arg + partial
application). The original probe (`call(useByte, 1000)`) is now rejected instead of printing `1000`.

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

### 2.2 The channel does not run over compiler-track code (carried from the retired plan §6) — **DONE (conservative reject)**

A `where`-guarded def called *from* an `eliot-compiler` overlay body is never demanded — the channel walks
runtime `MonomorphicValue`s only. Same failure class as §2.1 (a manifest use whose precondition is never
demanded), different entry path.

**Fixed** with the plan's conservative-reject option: `CompilerMonomorphicTypeCheckProcessor` now scans a
`Runtime`-role compiler-track body's *raw* (operator-resolved) value references and hard-errors + aborts if any callee
declares a `^Where` companion — "a `where`-guarded value cannot be called from compile-time code" — until the channel
is extended to walk compiler-track bodies. The scan is on the raw body (a reduced body would have already folded a pure
`where`-def call away, hiding the reference) and gated to the real body reduction (a `Signature`-role key reduces the
signature, not the body). Verified never to fire on the base build (no shipped def carries a `where` precondition) and
not to false-fire on the existing `where`/transfer suites. No positive integration test: a compile-time call to a
`where`-def is not expressible from surface syntax today (it would need genuinely compile-time-evaluated user code
reaching a runtime precondition-bearing def), so the guard is covered by the full-suite-green + base-build-doesn't-fire
verification. When the walk-compiler-bodies path is built, this reject becomes the fallback it already documents.

### 2.3 FQN-keyed escalation bindings — latent wrong-splice hazard — **DONE**

`EscalatingReducer.escalate` (and the in-checker candidate builder) did `candidates.distinctBy(_._1)`, and both
the spliced-binding map and the evaluator lookup are keyed by **FQN only**. If one stuck term references the same
FQN at two *different* ground instantiations, one instantiation's reduced body — with its ability-impl choices
baked in — served both; in the channel path the winner was arbitrary (`ReducedBindingClosure.valueReferences`
returns a `Set`, so `.toSeq` order is hash-driven). **Rationale for acting despite no observed failure:** a
cross-spliced body usually sticks on a shape mismatch and degrades to ⊤/stuck (sound), but a same-shape pair —
Bool-valued predicates, guard-selected impls over one type shape — could *quote a wrong result silently*: a
wrongly-narrow interval or a wrong guard verdict, exactly the "behavioral widening must never become wrong
narrowing" risk the retired plan's §7 named.

**Fixed:** `escalate` now groups candidates by FQN (`candidates.distinct.groupBy(_._1).collect { case (_, Seq(one)) =>
one }`) and escalates only an FQN referenced at a *single* ground-arg vector; an FQN at ≥2 distinct vectors is
**declined**, so the term stays stuck (a loud error on the checker read-back, a sound ⊤ on the channel path) instead of
splicing an arbitrary winner. The in-checker candidate builder (`PostDrainQuoter.escalationBindings`) dropped its
pre-collapse `distinctBy(_._1)` so the ambiguous case actually reaches `escalate` (identical `(fqn, args)` duplicates
are folded by `escalate`'s own `.distinct`). Keying the binding map by (FQN, args) is *not* the cheap fix: the
evaluators' lookups are FQN-only by design. Strictly more conservative than before (only ever declines an escalation
that could not have been served correctly); full suite green confirms no reachable path relied on the arbitrary winner.

### 2.4 Compiler-track literal ascription (carried Step-4 follow-on; **blocker for the strict §1 invariant**) — **DEFERRED (not a silent-accept)**

A compiler-track `def x: BigInteger = <literal>` fails its own check when its `CMV(x, [])` is demanded:
check-mode types `integerLiteral[n]` as `Int`, never ascribing it to the declared `BigInteger`. Today this is
latent (a *loud* spurious rejection, not triggered — `byteMin`/`byteMax` reduce via the raw seed before
escalation ever demands their CMV), but it has two costs: any future escalation that reaches such a def rejects
a correct program, and it is **the** reason §1's invariant must stay "raw until stuck" rather than the strict
"monomorphized always". Fix by compiler-track literal ascription (a literal in checked position adopts a
declared `BigInteger` type), or by giving `byteMin`/`byteMax` genuinely-`BigInteger` bodies; then re-evaluate
tightening the seed (e.g. preferring `CMV.reduced` over raw wherever it exists).

**Deferred (2026-07-16), unlike its four siblings.** Unlike §2.1/§2.2 (silent-accepts) and §2.3/§2.5 (a silent-wrong
splice/structure), §2.4 is **already fail-safe**: an untriggered, *loud* spurious rejection — it never accepts a wrong
program, it would at worst reject a correct one, and only along a path nothing currently reaches. So it does not violate
the gaps-must-be-fail-safe rule. Its only available fix is compiler-track literal ascription in the checker's
*check-mode* (the "genuinely-`BigInteger` body" alternative is not real — `byteMin`/`byteMax` already *are* `= -128`/`=
127` literals; the issue is how check-mode types the literal). That change exists to **unblock the strict §1
tightening** (prefer `CMV.reduced` over the raw seed) and is best done *with* that tightening and its tests, not
decoupled — decoupled it adds checker check-mode risk for zero current correctness payoff. Tracked here for whoever
takes on the strict-§1 tightening.

### 2.5 `reducibleStuck` abstract-ability hardening (carried Step-4 follow-on, sharpened) — **DONE**

A stuck *abstract-ability* application quotes to a `GroundValue.Structure`, so `reducibleStuck` treats it as
non-stuck — after the linker fix this path should be unreachable (abilities are resolved before the channel sees
them), but if it ever fires it yields a bogus structure rather than escalation/⊤.

**Fixed** with the sharper fix: `escalatingLoop` gained a `stuck` predicate parameter defaulting to `reducibleStuck`,
so the in-checker read-back keeps today's predicate byte-for-byte while `reduceApplied` (the channel executor) passes a
hardened `channelStuck` — `reducibleStuck` *plus* "quotes to a `Structure` headed by a `Qualifier.Ability` method ⟹
stuck" (one quote, mirroring `reducibleStuck`, so the hot path pays nothing). If such a term ever survives, the loop
escalates rather than accepting it; and if escalation cannot resolve it, `reduceApplied`'s `isAbstractAbilityStructure`
filter drops it to ⊤ (never stores the bogus structure). A legitimate channel meta is a `$Meta` structure
(`Qualifier.Type`) or a `Direct`, never ability-qualified, so the filter drops only bogus values. Defensive (the path
is unreachable today, so no positive test); the parameterization is clean and the checker predicate is unchanged, and
the full suite confirms no regression.

## 3. Simplifications / hardening (mechanical, behavior-preserving)

Status: **all of §3 LANDED 2026-07-16** (§3.1–§3.5). Full suite green (0 failures, 0 aborts) + HelloWorld runs.
CACHE_VERSION 30→31 (the §3.3 field-forwarding changes impl-method AST fact content).

### 3.1 Remove `PostDrainQuoter`'s default no-op `reduceInstance` — **DONE**

`PostDrainQuoter`'s escalation-fetch parameter no longer defaults to the no-op `(_, _) => None`; it is **required**.
The one production construction site (`TypeStackLoop`) already passes the real fetch. The default was a
silent-degradation trap — a future construction site omitting it would get an escalation loop that can never escalate
(terms that should reduce stay stuck), with no compile error and nothing to grep for.

### 3.2 Migrate the marker-guard reader onto `escalatingLoop` (carried from the retired plan §6) — **DONE (already migrated; cleanup landed)**

**Finding:** the migration itself was already completed by the signature-unification rewrite — the marker-guard reader
(`AbilityImplementationProcessor.readGuardVerdict`) now reads the marker's **signature-twin** `CompilerMonomorphicValue`
(Phase D), whose mono runs the stuck-driven `escalatingLoop`; it no longer calls `reduceInstance` one-hop. So §3.2's
premise ("still fetches sub-values one-hop") was stale, and the last `deep = false` caller of
`ReducedBindingClosure.reduceInstance` was already gone.

**Cleanup landed:** dropped `reduceInstance`'s now-dead two-mode `deep` flag (every caller passed `deep = true`) — it
now always reduces deep, threading `TypeStackLoop.reduceInstance`, both processors, `EscalatingReducer`, and the test
seam down to a 2-arg signature — and rewrote the gotcha-laden docstring (deep is safe because `escalatingLoop`'s
laziness only escalates a *stuck* term, never eagerly monomorphizing what evaluation does not demand). `buildBinding`/
`collectBindings` keep their `deep` flag: they are still genuinely two-valued (the one-hop raw seed for the escalating
loop, and `CompilerNativesProcessor`'s eager `Leaf`, both pass `deep = false`).

### 3.3 `ImplementBlock`'s impl-method rebuild → `f.copy(...)` — **DONE**

Rebuilt the impl method with `f.copy(name = …, genericParameters = …, visibility = Visibility.Public)` — only the three
deliberate overrides spelled out, everything else forwarded by default. The positional rebuild was the *bug class*: it
silently dropped every field it did not restate — beyond the Step-5a `returnMeta`/`whereClause`, also `doc` (impl-method
apidoc), `dischargedEffects` (an impl method declaring `{…, -E}`), `fixity`, and `precedence` — and every future
`FunctionDefinition` field re-opened it. (Because this now forwards previously-dropped fields, impl-method AST fact
content can change → CACHE_VERSION bump.)

### 3.4 Cross-reference the two "monomorphized bodies only" mechanisms — **DONE**

Added a paragraph to `EscalatingReducer`'s class doc noting that `CompilerNativesProcessor`'s eager `Leaf` contribution
(for checking-time evaluation, which is pure and cannot demand facts mid-eval) and `EscalatingReducer`'s lazy escalation
(read-back and channel) are the eager and lazy halves of §1's one invariant at different seams, and that they cannot be
merged (the evaluator purity boundary) — so a future simplification pass does not try. One comment, no code.

### 3.5 `PostDrainQuoter` entry-point consolidation (low priority) — **DONE (opportunistic dedup)**

The seven entry points stay (their callers need the different return shapes), but their duplicated cores are now
consolidated. Two private helpers were extracted: `quoteDeep(v): Either[String, GroundValue]` (the deep-renormalise-then-
quote the four deep-quote entry points shared verbatim) and `orAbort(result, at)` (the shared `Cannot resolve type.`
fail-safe abort). `quoteSem`/`quoteSemOption`/`reduceSignatureToGround`/`reduceSignatureToGroundOption` are now
one-liners over those helpers — the aborting/`Option` split reduced to `orAbort` vs `.toOption`. `reduceSemExprToGround`/
`reduceSourced`/`quoteSourced` genuinely differ (no deep renormalise, or a materialise/staging-gate step) and are left
as-is. Pure refactor, behavior-preserving.

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
