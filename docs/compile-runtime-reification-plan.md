# Seamless compile→runtime reification (cross-stage persistence)

Status: **Planned.** Unblocks `docs/jvm-int-representation-plan.md` Phase 4 (replaces the proposed `bigInt[N]`
primitive entirely) and generalises to "use any compile-time value at runtime."

## Goal

Let a value that the compiler already computed at type/erased time be used in value-position (runtime) code
**without any explicit reification call**. Concretely, make this work:

```eliot
-- there is no dot syntax; `A.name` is written as the accessor application `name(A)`
def staticName[A: Person]: String = name(A)
```

`A` is an erased (`[]`-bound) parameter; after monomorphisation it is a concrete `Person`. `name(A)` must
appear at runtime as the *constant* `"Alice"`, even though `A` itself has no runtime representation.

The same mechanism removes the need for the `bigInt[N]` primitive: a width-dispatch body can mention its
erased bound `MIN`/`MAX` directly in value position and the compiler materialises the `BigInteger` constant.

## Why this is small: the front half is already generic

The pipeline already carries compile-time values to the boundary. The only special-cased part is the back
half (materialisation), and today it is hardcoded to integer literals.

- **Erased params are already concrete in the checker env.** `TypeStackLoop.applyTypeArgs`
  (`monomorphize/check/TypeStackLoop.scala:280-281`) binds each `[]`-param `name → VConst(groundArg)`.
- **Value-position references to them already resolve.** `ValueResolver` returns `ParameterReference("A")`
  for a type-stack param used anywhere (it short-circuits before the dictionary lookup), and the checker's
  `infer`/`ParameterReference` (`Checker.scala:536`) looks it up in the env and gets the concrete `VConst`.
- **The NbE evaluator already reduces `name(A)` to a ground value** once `A` is concrete — that is just
  ordinary evaluation/`Quoter.quote`.

So "is this expression a compile-time constant?" is a *detectable property* (`Quoter.quote` succeeds to a
`GroundValue`), not something the programmer must annotate. The only missing piece is: at the
`SemExpression → MonomorphicExpression` boundary, emit that constant instead of a broken
`ParameterReference("A")` (A has no runtime slot, so the current output is wrong/uncompilable).

## Where it lives: `lang`, at the quote boundary — not the backend

Materialisation is a **`SemExpression → MonomorphicExpression` rewrite**, i.e. a smarter `PostDrainQuoter`. It
is platform-independent: it produces ordinary literal / constructor-application expression nodes. The backend
stays unchanged — it already emits literals and constructor calls. Only the primitive-literal *leaf* emission
(how to push a `java.math.BigInteger` / `String` constant) is backend code, and it already exists.

## The mechanism

Walk the runtime body `SemExpression` top-down, threading a `runtimeParams: Set[String]` (names bound by
enclosing `FunctionLiteral`s) and an evaluation `Env`. For each value-position node `N`:

1. **`N` references no erased param** → recurse structurally, exactly as today. *No evaluation happens* — so
   ordinary runtime code (including recursive defs and large constants) is never evaluated and cannot diverge
   or bloat. This preserves current behaviour for everything that already works.
2. **`N` references an erased param *and* (a runtime param or an inner lambda)** → cannot be a single
   constant; recurse into children (the erased dependence resolves deeper).
3. **`N` references an erased param and nothing runtime** → it is a compile-time constant. Evaluate it,
   `force`, and `Quoter.quote`:
   - `Right(ground)` → **materialise**: expand `ground` into a literal / constructor-call tree.
   - `Left(_)` (closure / stuck / `Type`) → **hard compiler error** ("value depends on a compile-time
     parameter but does not reduce to a constant"). Fail-safe; never a silent bad emit.

The "references an erased param, nothing runtime" gate is what makes evaluation safe: we only ever evaluate
sub-terms whose value is fully determined by erased data — exactly the ones that *cannot* be compiled as
runtime code and *must* become constants. Genuinely-runtime sub-terms are never evaluated. This is the
classic staging boundary: the residual is the neutral (runtime-param-headed) part; everything ground is a
constant. Closures (`VLam`, e.g. the block inside `data IO(block)`) fail `Quoter.quote`, so effects are
automatically excluded from materialisation.

### The two new helpers

**(a) `evalSem(env, semExpr): SemValue`** — a ~15-line evaluator over `SemExpression`, mirroring
`Evaluator.eval` (`monomorphize/eval/Evaluator.scala:21`) but reading the already-evaluated `SemValue` type
args out of `SemExpression.ValueReference` instead of re-evaluating ORE. `lookupTopDef` is the checker's
binding cache (`fqn => state.bindingCache.getOrElse(fqn, None)`), which already holds every reachable binding
(prefetched during checking — same source `renormalize` uses). Reuses `Evaluator.applyValue`.

```
IntegerLiteral(v)            -> VConst(Direct(v, bigIntGroundType))
StringLiteral(v)             -> VConst(Direct(v, stringGroundType))
ParameterReference(name)     -> env.lookupByName(name).getOrElse(neutral)
ValueReference(vfqn, targs)  -> targs.foldLeft(lookupTopDef(vfqn))(applyValue)   // targs are SemValues already
FunctionApplication(t, a)    -> applyValue(evalSem(env,t), evalSem(env,a))
FunctionLiteral(n, _, body)  -> VLam(n, x => evalSem(env.bind(n,x), body))
```

**(b) `materialise(ground: GroundValue): MonomorphicExpression.Expression`** — the recursive
`GroundValue → Expression` expander:

```
Direct(v: BigInt,  _)  -> IntegerLiteral(v)
Direct(v: String,  _)  -> StringLiteral(v)
Direct(v: Boolean, _)  -> MonomorphicValueReference(true/false ctor)        // Bool is data on the platform side
Structure(ctorFqn, args, _)  -> nested FunctionApplication of
                                MonomorphicValueReference(ctorFqn, <type args>) to materialise(valueArg)…
Type                   -> error ("cannot reify a type into runtime")
```

Each produced `MonomorphicExpression` carries `expressionType = ground.valueType`; `RepresentationLowering`
(run later, unchanged) lowers it to the machine representation.

### Staging — ship the leaf case first

The `Structure` arm needs to split a constructor's `args` into type-args vs value-args (so the value-args
become applications and the type-args go into `MonomorphicValueReference.typeArguments`). That requires
constructor-shape metadata — the one sanctioned read of `RoleHint`/arity per the cornerstone, already used by
`match` reconstruction. Defer it:

- **Stage 1 (unblocks Phase 4 and `name(A)`):** materialise `Direct` leaves only (`BigInteger`, `String`,
  `Bool`). A `Structure` result raises the fail-safe error ("cannot yet materialise a structured compile-time
  value"). This already covers `bigInt[N]` → erased `BigInteger`, and `name(A)` → `String`, because projecting
  a primitive field yields a `Direct`, not a `Structure`.
- **Stage 2 (full "any data type"):** implement the `Structure` arm by splitting type/value args via the
  constructor-shape machinery `match` already uses, emitting nested constructor calls. This is what makes
  `def whole[A: Person]: Person = A` (returning the whole structure) and user data types work. Requires the
  constructor to be concrete on the runtime side (defined "on both sides") — otherwise the fail-safe error
  fires.

## Concrete code changes

1. **`monomorphize/eval/` — add `SemExpressionEvaluator`** (helper (a) above). One top-level type per file
   (project convention); reuses `Evaluator.applyValue`/`force` and `Quoter`.

2. **`monomorphize/check/PostDrainQuoter.scala` — fold materialisation into `quoteExpression`.**
   - New constructor params: `monoEnv: Env`, `lookupTopDef: ValueFQN => Option[SemValue]`.
   - Thread `runtimeParams: Set[String]` through `quoteExpression`/`quoteSourced`, adding the binder in the
     `FunctionLiteral` arm and binding it to a fresh neutral in the eval env.
   - Before the existing structural match, apply the gate (steps 1–3). On materialise, return
     `materialise(ground)`; otherwise fall through to the current structural recursion (unchanged).
   - Keep `resolveIfAbility` as-is.

3. **`monomorphize/check/TypeStackLoop.scala` — supply the new inputs.**
   - Capture `monoEnv` right after `instantiateRemaining` (env = erased type-stack params only; value params
     are `FunctionLiteral` binders bound later during `check`, so they are correctly absent here).
   - At the `PostDrainQuoter` construction (`:83`), pass `monoEnv` and
     `fqn => state.bindingCache.getOrElse(fqn, None)`.

4. **`resolve` — verify only.** Confirm `name(A)` (accessor applied to a type-stack param) resolves to
   `FunctionApplication(name, ParameterReference("A"))` with no incidental "type param used in value position"
   guard. No dot syntax exists, so the surface form is `name(A)`, not `A.name`. (If a guard exists, remove it;
   the cornerstone says `[]`/`()` are sugar over one binder, so a value-position reference is legitimate.)

5. **`stdlib`/Phase 4 — drop `bigInt`.** Width-dispatch bodies reference `MIN`/`MAX` directly in value
   position instead of via `bigInt[...]`. `integerLiteral[n]` for *literals* is untouched (a literal does not
   reference an erased param, so it keeps using the existing backend intrinsic).

## Decision required: `BigInteger`'s runtime representation

`NativeType.scala` currently maps `eliot_lang_BigInteger → java.lang.Long` (a Phase-3 stopgap), while
`JvmBigInteger → java.math.BigInteger`. A materialised `Direct(BigInt, bigIntType)` becomes an
`IntegerLiteral` at `BigInteger`'s representation, and the backend's integer-literal path boxes from `Long`.

- If width-dispatch only ever materialises Long-range bounds → keep `Long`, no backend leaf change.
- If bounds can exceed `Long` (the general case; bignum is the fallback representation) → map `BigInteger` to
  `java.math.BigInteger` and add a one-line leaf so an `IntegerLiteral` at that representation emits
  `BigInteger.valueOf(...)` / `new BigInteger(String)` (commit `f8126ae1` already added a `BigInteger`
  emission path in `convertRepresentation`, so the capability mostly exists).

Settle this explicitly rather than inheriting the stopgap. The consumer (Phase 4) decides whether `Long`
suffices.

## Fail-safe behaviour (per the "gaps must be fail-safe" rule)

- Erased-dependent sub-term that does not reduce to ground → hard error, never a silent emit.
- Stage 1 hitting a `Structure` result → explicit "not yet supported" error, never a partial/garbage emit.
- Stage 2 materialising a type whose constructor is abstract-only (no runtime form) → hard error
  ("compile-time value of an abstract type cannot be reified"). This *is* the "must be defined on both sides"
  requirement, surfaced as an error.
- `Type` reaching `materialise` → error.

## Test plan (ScalaTest; single-line asserts per `.claude/rules/testing-conventions.md`)

- `def staticName[A: Person]: String = name(A)` with `staticName[Person("Alice", 30)]` → runtime constant
  `"Alice"` (assert the monomorphic body is a `StringLiteral("Alice")`, and end-to-end via the jvm module).
- Erased `BigInteger` bound referenced in value position → materialises the constant (the Phase-4 unblock).
- `def f(x: Int): Int = x` (no erased ref) → body unchanged (regression guard: ordinary runtime code is not
  evaluated/folded).
- `def f[A: Person](x: String): String = x` → `x` stays a `ParameterReference` (runtime param not materialised
  even though an erased param is in scope).
- Mixed: `f(name(A), x)` → arg1 materialised, arg2 kept.
- Fail-safe: erased-dependent value that does not reduce to ground → expected compiler error (assert the
  error, not a crash). Stage 1: a `Structure`-valued erased reference → expected "not yet supported" error.

## Open questions

- **Stage-2 arg splitting:** confirm whether a data value-constructor's quoted `Structure.args` includes
  leading type-args (e.g. `cons[Int](h, t)`), and reuse the exact constructor-shape source that `match`
  reconstruction uses to split them — do not introduce a second arity source.
- **Code size:** a large materialised `Structure` inlines at every use site. Acceptable for now; a later
  optimisation can hoist a materialised value into a cached `static final` field.

## Stage 3 (follow-up): reconcile downstream, and fold in `integerLiteral`

Once Stages 1–2 land, two pieces of existing machinery were built to do by hand what this mechanism now does
generically. Revisit both.

### Adjust the `jvm-int` plan — **done**

`docs/jvm-int-representation-plan.md` documented `bigInt[N]` (compile-time→runtime `BigInteger` reification) as
the agreed Phase-4 unblock. That primitive is now subsumed — an erased `BigInteger` bound referenced in value
position materialises directly. Its "Phase 4 — status, blocker, and the reification unblock" section has been
updated to: (a) drop `bigInt[N]` as the fix, (b) point at this document as the actual unblock, and (c) note that
width-dispatch bodies reference `MIN`/`MAX` in value position rather than wrapping them.

Caveat to the "do this only after Phase 4 compiles" guidance: this edit was made early, by request. The
reification mechanism is built and tested, but the committed Phase-4 dispatch does **not** compile yet — the
type-level `fitsIn[…]` call in the checked dispatch bodies still raises "Too many type arguments" (deferred).
The cross-reference therefore describes the agreed mechanism plus an honest "not yet compiling" status, not
fully shipped Phase-4 behaviour.

### Evaluate simplifying / removing `integerLiteral` — **done (intrinsic deleted)**

`integerLiteral[n]` is itself a hand-written reification: it lifts the type-level `n: BigInteger` into a
value-position `Int[n,n]`, with a dedicated backend intrinsic (`Intrinsics.integerLiteralFQN`,
`ExpressionCodeGenerator.generateIntrinsic`) that reads `typeArgs.head` and emits `ldc`+box. A materialised
`Direct(BigInt, …)` now lowers to a `MonomorphicExpression.IntegerLiteral`, which the backend *already* emits
via its plain integer-literal path — so the intrinsic and possibly the `5 ⟶ integerLiteral[5]` desugaring
(`CoreExpressionConverter`) may be collapsible into the one general path.

**Decision (implemented).** Decompose `integerLiteral` into the *one part that is genuinely redundant* and the
*two parts that are load-bearing*:

- **Redundant — the backend intrinsic — DELETED.** The intrinsic's emission was byte-identical to the
  backend's own plain `IntegerLiteral` arm (both call `pushIntegerConstant(value, repr)`). The reification is
  **not type-checking** — by readback time the value `n` is already on the node (the erased type-arg) and the
  type `Int[n,n]` is already attached — so it is a purely **syntactic node-swap**: `PostDrainQuoter` (the
  `SemExpression → MonomorphicExpression` readback) recognises the `integerLiteral` FQN
  (`WellKnownTypes.integerLiteralFQN`, by name, like `coerceFQN`/`handleCases`) and emits a plain
  `MonomorphicExpression.IntegerLiteral(n)` node, which `structuralQuote` stamps with the node's existing
  `Int[n,n]` type. Because `used` runs *after* monomorphize and walks the `MonomorphicExpression` tree (where a
  literal is a leaf that marks nothing), `integerLiteral` then never appears as a value reference anywhere
  downstream → it is never collected as used → never reaches codegen at all. So **no `JvmClassGenerator`
  skip-marker is needed** (it was removed from `Intrinsics.all`), and the gate's not-firing-on-closed-literals
  is irrelevant (we never route through the gate — this is a direct by-name readback rule, not `materialise`).
- **Load-bearing — the desugaring (`5 ⟶ integerLiteral[5]`) — KEPT.** It is what gives a value-position literal
  its singleton `Int[n,n]` type instead of a bare (value-less) `BigInteger`.
- **Load-bearing — the `integerLiteral`/`IntegerLiteralType` typing protocol — KEPT.** `IntegerLiteralType[V] =
  Int[V,V]` is supplied by the *platform* layer, so a literal gets a concrete `Int` type **without the compiler
  ever naming `Int`** (hardcoding `Int` in the checker is a cornerstone anti-pattern). This is the same
  "recognise the protocol by name, not the type" discipline as `Coerce`/`PatternMatch`.

**Coercion is not a substitute** (explicitly evaluated, per the request). `Coerce[Int[s..],Int[t..]]` *widens an
existing* `Int[n,n]` to a broader range; it needs a source value to coerce *from* — which is the `Int[n,n]`
literal itself — so it is strictly downstream of literal *creation* and composes with it. Routing literals
through a hypothetical `Coerce[BigInteger, Int[n,n]]` is a dead end because Phase 6 deliberately removed the
value-position `BigInteger` form. So Coercion handles widening (already does); it cannot create the literal.

**Outcome:** one constant-materialisation path in `lang` (`PostDrainQuoter`), every backend gets integer-literal
emission for free via its ordinary literal node, and the redundant per-backend `integerLiteral` intrinsic is
gone. `integerLiteral` is reduced to a pure syntactic device that evaporates at readback.

## Phasing checklist

- [ ] Stage 1a: `SemExpressionEvaluator` + `materialise` (Direct leaves) + `PostDrainQuoter`/`TypeStackLoop`
      wiring; gate logic; fail-safe errors.
- [ ] Stage 1b: resolve verify; drop `bigInt`; Phase-4 bodies reference `MIN`/`MAX` directly; settle
      `BigInteger` representation.
- [ ] Stage 1 tests (above) green; Phase 4 compiles.
- [ ] Stage 2: `Structure` materialisation via constructor-shape arg splitting; abstract-type fail-safe;
      whole-structure + user-data tests.
- [x] Stage 3a: update `docs/jvm-int-representation-plan.md` Phase-4 section to drop `bigInt[N]` and reference
      this plan. **Done early by request** — note the Phase-4 dispatch does not compile yet (type-level `fitsIn[…]`
      call → "Too many type arguments", deferred); the section reflects that honestly.
- [x] Stage 3b: **done.** Decided + deleted the redundant part: the backend `integerLiteral` intrinsic is
      removed; `PostDrainQuoter` rewrites `integerLiteral[n]` into a plain `MonomorphicExpression.IntegerLiteral`
      node (by-name, at the readback — a syntactic node-swap, not via the `materialise` gate). The desugaring and
      the `integerLiteral`/`IntegerLiteralType` typing protocol are KEPT (platform-independent typing; keeps `Int`
      out of the compiler). Coercion is downstream widening, not a substitute. See "Evaluate simplifying /
      removing `integerLiteral`" above.
