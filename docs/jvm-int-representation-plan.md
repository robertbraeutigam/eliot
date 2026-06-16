# Exact `Int` representations via `opaque` + Eliot dispatch

Status: **Phases 1–3 implemented.** **Phase 4 is in progress and does not compile yet.** A partial Phase-4
implementation is committed (`f8126ae1 "Implementing int min-max eliot dispatch"`) that swapped the working
inline `+`/`-`/`*` intrinsics for an Eliot width-dispatch which does not type-check — so the *arithmetic*
integration tests currently fail, while the literal / representation-selection tests still pass. The original
Phase-4 blocker (the dispatch needs a *value-level* `BigInteger` for its width comparisons) is resolved in
principle by the compile→runtime **reification** mechanism (`docs/compile-runtime-reification-plan.md`, Stages
1–2, **built and tested**), which subsumes — and replaces — the once-proposed `bigInt[N]` primitive. The
remaining Phase-4 work (make the type-level `fitsIn[…]` call type-check in the checked dispatch bodies, then
re-verify) is **deferred** — see "Phase 4 — status, blocker, and the reification unblock" below. Phase 5 still
planned. After Phase 3 an `Int[MIN, MAX]` is laid out at the narrowest JVM wrapper its range fits
(`java.lang.{Byte,Short,Integer,Long}` / `BigInteger`), verified end-to-end for literal-position values
(`Int[0, 1000]` in `Short`, `Int[0, 70000]` in `Integer`, `Int[0, 5000000000]` in `Long`); arithmetic awaits the
Phase-4 completion.

Goal: store each `Int[MIN, MAX]` in the *smallest* machine representation whose range contains `[MIN, MAX]`
(`Byte`/`Short`/`Int`/`Long`, with a bignum fall-back), and make range widening (`Coerce`) a *real* runtime
conversion — with **no `Int`-specific code in the backend**. All width *policy* (which representation a range
uses, which operation/conversion to run) lives in Eliot. The backend only ever:

1. maps a small fixed set of **representation types** to a machine layout, and
2. implements **leaf natives over concretely-sized parameters** (`nativeAddByte`, …).

This is backend-agnostic on purpose: a microcontroller backend reuses the identical Eliot, swapping only the
representation-type→layout map and the leaf-native bodies (an 8-bit `ADD` instead of `IADD`+`i2b`). There is
no JVM-only "uniform `Number` carrier" — that trick relied on the JVM's erased reference ABI and does not
survive onto a target where a byte and a word are genuinely different ABIs.

## Core principle: representation is *derived from* the type, never *is* the type

`Int[0,255]` and `Int[0,1000]` must stay **distinct types** for the checker — otherwise it would accept
assigning a `[0,1000]` value into a `[0,255]` slot. So the bounds are the type's identity and meaning, and
they survive type-checking intact. The machine representation is a *separate, later* fact computed from those
bounds.

`opaque` is exactly the barrier that makes this sound: the jvm layer gives `Int` a body that computes its
representation from the bounds, but marks it `opaque` so the **type checker never unfolds it** (it stays a
stuck, bounds-carrying `Int[MIN,MAX]`), while **later phases unfold it** to reach the representation. This
restores the otherwise-unsound `type Int = <repr>` idea by phase-gating the unfolding.

The load-bearing consequence — the one easy to miss: *opacity is what hides the representation from the
backend.* "Distinct for the checker" is implemented as "body invisible during checking," and that same
invisibility means a user's `Int[0,255]` field carries no machine size until something **unfolds the opaque
body after type-checking**. That post-checking unfold is therefore unavoidable (Phase 3) — it is the price of
opacity, not an optional optimisation.

## Foundation (already implemented)

The `opaque` modifier exists and works in the type checker:

- Parsed on `type`/`def` (`opaque type Int[MIN,MAX] = …`), threaded `FunctionDefinition` → `NamedValue` →
  `ResolvedValue` → … as a `opaque: Boolean` field.
- `UserValueNativesProcessor` builds an `opaque` definition's checking binding as a stuck `VTopDef(fqn, None,
  …)` (body **not** cached), so the NbE evaluator never unfolds it during checking. The body remains in
  `OperatorResolvedValue.runtime` for later phases.

Verified property (`MonomorphicTypeCheckTest`): for `opaque type W[N: BigInteger] = String`, `W[1]` and `W[2]`
stay distinct, whereas transparent `type W[N] = String` collapses them. That distinctness is the soundness
guarantee this plan relies on.

## The lowering chain (zero `Int` knowledge in Scala)

```
Int[0,255]            -- abstract+bounds-carrying; the checker's view (opaque, never unfolded; distinct per range)
  │  (jvm layer: opaque Int body, in Eliot — selectByWidth over MIN/MAX)
  ▼  Phase 3: unfold opaque, force with concrete bounds
JvmByte               -- a fixed representation type
  │  (NativeType map: one line)
  ▼
java.lang.Byte        -- the JVM class  (on MCU: a 1-byte layout)
```

The Phase-3 rule is general: *to lay out a value, unfold `opaque` definitions until the head is a type with a
native mapping.* It never names `Int`.

---

## Phase 1 — Representation types  *(implemented)*

Body-less platform types (like `String`/`Unit` today), mapped to JVM classes:

- `jvm/resources/eliot/eliot/lang/Jvm.els` (module `eliot.lang.Jvm`): `type JvmByte`, `type JvmShort`,
  `type JvmInt`, `type JvmLong`, `type JvmBigInteger`. (One module rather than one-per-type; the opaque `Int`
  body and the unfold pass reference them via the `Jvm` module.)
- `NativeType.types`: added `JvmByte → java.lang.Byte`, `JvmShort → java.lang.Short`, `JvmInt →
  java.lang.Integer`, `JvmLong → java.lang.Long`, `JvmBigInteger → java.math.BigInteger`, via the new
  `NativeType.jvmRepresentationType` helper (fixed module `Jvm`, `Qualifier.Default` to match
  `CommonPatterns.stripDataTypeSuffix`).

These five entries are the **entire** new Scala "type knowledge."

**Deviation from the original bullet — `Int → Long` kept for now.** Removing the `Int → Long` entry is
deferred to **Phase 3**, not done here: until the post-checking unfold pass exists to lower `Int` through its
opaque body to a `Jvm*` type, `Int` *must* stay native-mapped or codegen for every `Int`-using program breaks
(`javaSignatureName` would fall back to a non-existent `eliot/eliot/lang/Int$Int` class). Keeping it preserves
the "nothing observable yet" property of Phases 1–2 (all jvm tests, incl. integer arithmetic/widening, still
pass). The removal lands in Phase 3 alongside the unfold pass. The `Jvm.els` module is currently unimported,
so the new types are inert until Phase 2 references them.

## Phase 2 — The representation policy, in Eliot (`opaque type Int`)  *(implemented)*

The jvm layer redefines `Int` with an `opaque` body that selects the representation from the bounds, using the
existing compile-time `BigInteger` predicates (`lessThanOrEqual`, `&&`, `fold`). Implemented in
`jvm/resources/eliot/eliot/lang/Int.els` (module `eliot.lang.Int`, merged with the base `stdlib` `Int.els`):

```eliot
import eliot.lang.Bool
import eliot.lang.Jvm

opaque type Int[MIN: BigInteger, MAX: BigInteger] =
  fold(fitsIn[-128, 127, MIN, MAX], JvmByte[],
  fold(fitsIn[-32768, 32767, MIN, MAX], JvmShort[],
  fold(fitsIn[-2147483648, 2147483647, MIN, MAX], JvmInt[],
  fold(fitsIn[-9223372036854775808, 9223372036854775807, MIN, MAX], JvmLong[],
       JvmBigInteger[]))))

def fitsIn(lo: BigInteger, hi: BigInteger, min: BigInteger, max: BigInteger): Bool =
  lessThanOrEqual(lo, min) && lessThanOrEqual(max, hi)
```

Module unification (`UnifiedModuleValueProcessor`) prefers the implementation (`runtime.isDefined`): base `Int`
stays body-less, jvm `Int` carries the opaque body. **Verified**: the merge selects the whole `NamedValue` of
the implemented (jvm) definition, which carries `opaque = true`, and `signatureEquality` compares only
`typeStack.signature` (not the body/flag), so the added body + flag don't break the merge. The jvm widening
integration tests exercise the merged opaque `Int` end-to-end.

**Two deviations from the plan's literal code above**, both forced by mechanics that postdate the plan:

1. **The representation candidates are written `JvmByte[]` … (empty type-argument brackets), and the width table
   is inlined into the `Int` body rather than factored into a `selectByWidth` helper.** Type constructors
   (`JvmByte`, …) are registered only under `Qualifier.Type` (`DataDefinitionDesugarer.createTypeFunction`), but
   `fold`'s branches are value-position args, where a bare `JvmByte` converts to `Qualifier.Default` and fails to
   resolve. The empty `[]` suffix is a language feature added for exactly this (see "`[]` type-namespace marker"
   below): it forces a bare type constructor into the Type namespace even in value position. With it, the fold
   tree references the representations directly, so no helper indirection is needed. (`selectByWidth` was an
   earlier workaround that passed the representations as type *parameters*; `[]` made it unnecessary.) The
   genuinely reusable kernel for Phases 4/5 is `fitsIn` (the threshold table), not the fold tree.
2. **The width thresholds go through a `fitsIn` helper, *invoked* with `[...]`.** After the Phase-6 literal flip,
   a value-position integer literal desugars to `integerLiteral[n] : Int[n, n]`, not a bare `BigInteger` — so
   `lessThanOrEqual(-128, MIN)` (literals in `()` value position) would feed an `Int` where a `BigInteger` is
   expected. `fitsIn` is an ordinary **value-parameter** predicate (`(lo, hi, min, max: BigInteger): Bool`);
   nothing about it is type-level. It is the *call site* that uses `[...]` brackets (`fitsIn[-128, 127, MIN,
   MAX]`) so the literal arguments land in type context and stay `BigInteger`. `[...]` and `()` are the same
   Pi-application — `Evaluator` applies `ValueReference` type-args via the same `applyValue` as ordinary
   application — so applying `[...]` to a value-parameter function is well-defined (covered by a dedicated
   `MonomorphicTypeCheckTest` case). The brackets only affect the arguments' context, not `fitsIn`'s signature.

**Language feature added — the `[]` type-namespace marker.** Eliot disambiguates the type vs value namespace by
*context*, not casing: a bare upper-case name resolves to the value constructor (`Qualifier.Default`) in value
position and the type constructor (`Qualifier.Type`) in type position — necessary because a `data` declaration
makes *both* (e.g. `Box`), and bare data constructors (`True`, `Just`, …) are upper-case Default-namespace
values. (Confirmed by experiment: forcing all upper-case names to `Type` broke 13 constructor-match tests.) To
let a *type-only* name (like the body-less `JvmByte`, which has no value constructor) be named in value
position, `[]` is now the type-level analogue of value-level `()`: `genericArguments` on the AST
`FunctionApplication` became `Option[Seq[...]]` (`None` = no brackets, `Some(Seq())` = `[]`), the parser accepts
an empty `[]`, and `CoreExpressionConverter` resolves to `Qualifier.Type` whenever brackets are present (even
empty) — so `JvmByte[]` is the Type-namespace `JvmByte`. (`CoreProcessor.isTypeBody` was updated for the new
`Option` field — its old `Seq()` pattern silently stopped matching, a warning not an error.)

**Foundation gap fixed (required to keep Phase 2 non-observable).** Making `Int` opaque exposed that two
checker-phase sites read `OperatorResolvedValue.runtime` directly and ignored the `opaque` flag (the
`UserValueNativesProcessor`/`NativeBinding` guard only covers the main checker evaluator):
`AbilityMatcher.classifyValueRef` (which unfolded `Int[7,7]`→`JvmByte`, breaking `Coerce[Int, Int]` matching)
and `TypeStackLoop.processIO` (which would check/emit the opaque body). Added
`OperatorResolvedValue.checkingRuntime` (`= if (opaque) None else runtime`) and routed both sites through it.
The `Checker` coercion/combine sites read *impl* bodies (never opaque) and resolve `Int` via the guarded
evaluator, so they needed no change. With these guards the checker's view of `Int` is identical to the
pre-Phase-2 abstract `Int` (a stuck, bounds-carrying head), so all arithmetic/widening tests stay green and the
opaque `W[1]`/`W[2]` distinctness property still holds.

## Phase 3 — The generic "unfold to representation" pass  *(implemented)*

The single mechanism that re-reveals the representation hidden by opacity. It runs **after** type-checking, on
monomorphized values, and is **not** `Int`-specific.

**As built.** `TransparentBinding` (fact) + `TransparentBindingProcessor` mirror `UserValueNativesProcessor` minus the
`if (fact.opaque) None` guard, so opaque bodies are cached and unfoldable.
`monomorphize/lowering/RepresentationLowering.representationOf(gv, context)` lowers a `GroundValue` structurally: a head
with an `OperatorResolvedValue.runtime` body (the opaque `Int`) is unfolded by applying its ground args to the
transparent binding, `force`-ing with an empty `MetaStore`, quoting, and recursing; any other head (representation
types, data/`type` constructors, `Function`) is a leaf whose args are lowered in place. A non-reducing opaque head
aborts (fail-safe) rather than looping. The pass is **folded into `MonomorphicUncurryingProcessor`** (rather than a
separate fact): it lowers the `signature`, parameter types, return type, and every nested `expressionType` /
`FunctionLiteral` parameter type. The instance-identity `typeArguments` — the `UncurriedMonomorphicValue` key **and** the
`MonomorphicValueReference` type args inside bodies (the call-site lookup key, and `integerLiteral[V]`'s constant) — are
left **un-lowered** on purpose; only descriptor-bound types are rewritten. The `Int → Long` `NativeType` entry is
removed.

**Deviation — JVM intrinsics made representation-aware now, not in Phase 4.** Narrowing descriptors below `Long` makes
the JVM verifier reject the old uniformly-`Long`-boxed values (a `java.lang.Long` is not assignable to a `Short`-typed
slot). So Phase 3 also makes the *inline* intrinsics (`ExpressionCodeGenerator.generateIntrinsic`) box/unbox at the
lowered operand/result representations: `integerLiteral` boxes the constant at the result rep; `+`/`-`/`*` unbox each
operand via `<rep>.longValue()`, compute `LADD`/`LSUB`/`LMUL` in `long`, and rebox at the result rep (`l2i`+`i2b`/`i2s`
narrowing for `Byte`/`Short`); `intToString` unboxes to `long` then `Long.toString(long)`. This is exactly the JVM
specialisation the plan predicts for the Phase-4 leaf natives ("unbox → `LADD` → narrow → box"), done inline; Phase 4
will replace the inline emission with real `nativeOp{Wc}To{Wr}` leaves + Eliot dispatch. The retype-based `Coerce`
(unchanged) propagates the wider target type down onto the value-producing site, so a *materialised* value (literal /
arithmetic) is produced directly at the target rep — which is why cross-representation **widening of literals/arithmetic
already works** without Phase 5. A cross-representation `Coerce` of a *non-materialised* value (e.g. `def f(x: Byte):
Short = x`) still needs Phase 5's real `nativeWiden` splice; no current path hits it.

- The checking binding for `Int` is stuck (`cached = None`). Add a *transparent* binding (a
  `TransparentBinding` fact, or a flag on `NativeBinding`) produced like `UserValueNativesProcessor` but
  **without** the `if (fact.opaque) None` guard — i.e. opaque bodies are cached and unfoldable here only.
- `representationOf(gv: GroundValue): CompilerIO[GroundValue]`: if `gv`'s head FQN is native-mapped, return it;
  otherwise fetch the head's `OperatorResolvedValue`, evaluate its (opaque) body applied to `gv`'s arguments
  via the transparent evaluator, `force` to weak-head normal form, quote, and recurse (an opaque type may
  unfold to another). Bottoms out at a `Jvm*`/native FQN.
- A pre-codegen pass rewrites **every** `GroundValue` in each `UncurriedMonomorphicValue` via
  `representationOf`. After it, `Int[0,255]` fields become `JvmByte`, and the monomorphized signatures of the
  Phase-4 leaf natives become concrete (`(JvmByte, JvmByte): JvmByte`). `CommonPatterns.valueType` and the ~33
  descriptor sites stay pure — they now key off the (concrete `Jvm*`) FQN.

The width *logic* it runs is your Eliot opaque body; this pass only evaluates-and-substitutes. That is the
distinction that keeps it on-concept: Scala holds no width policy.

**De-dup:** distinct ranges sharing a representation (`Int[0,3]`, `Int[0,5]` → `JvmByte`) become identical
after lowering; canonicalise them so the per-range method-name mangling (`mangleSuffix`, currently `"Int"` for
every range — pre-existing, so no regression) collapses to one method. If dedup is deferred, fold the
representation into the mangle suffix.

## Phase 4 — status, blocker, and the reification unblock

**Decision (settled with the user):** the leaf matrix is **operand×result keyed** — `nativeAdd{Wc}To{Wr}`,
keyed by the common operand representation `Wc` and the result representation `Wr` (the "plan's literal matrix"
below), *not* the simpler diagonal. Chosen for microcontroller fidelity: a backend with no uniform `long`
carrier needs genuinely different per-width instruction sequences, so the *selection* must be in Eliot and the
backend must only realise fixed leaves. **No `Int` width policy may live in Scala.**

**What is committed (`f8126ae1`) — does NOT compile yet:**
- `stdlib/.../Int.els`: `+`/`-`/`*` given bodies = `nativeWiden(dispatch{Add,Subtract,Multiply}(nativeWiden(left),
  nativeWiden(right)))`; 27 body-less leaf natives (`nativeAdd/Subtract/Multiply` × `{ByteToByte, ByteToShort,
  ShortToShort, ShortToInt, IntToInt, IntToLong, LongToLong, LongToBigInteger, BigIntegerToBigInteger}`); three
  `dispatch*` helpers (nested `fold` over `fitsIn`, selecting a leaf applied to the two equalised operands).
- `lang/.../BigInteger.els`: `fitsIn` moved here (ambient `BigInteger` module → resolvable from both `Int.els`
  files; same-module names are NOT shared cross-file at resolve time, so it could not stay in `Int.els`).
- `jvm/.../Int.els`: `fitsIn` removed (now ambient).
- Backend: `Intrinsics` drops `+`/`-`/`*`, registers the 27 leaves + `nativeWiden`; `ExpressionCodeGenerator`
  realises each leaf inline (long `LADD`/`LSUB`/`LMUL` for ≤`Long`; `java.math.BigInteger` `add/subtract/multiply`
  when an operand/result is `BigInteger` — so `nativeMultiplyLongToBigInteger` never truncates) and `nativeWiden`
  as unbox→rebox; `JvmClassGenerator` **dedups generated methods by (mangled name + lowered descriptor)** so two
  ranges sharing a representation collapse to one method (different reps stay legal overloads — no mangling
  rewrite needed). These backend pieces are correct and reusable as-is.
- Parser: `infix`/`prefix`/`postfix` promoted to **hard keywords** (`TokenParser` + `FunctionDefinition`). This is
  an independent latent-bug fix: a function *body* (greedy full-expression parser) used to swallow the *next*
  definition's `infix …` annotation as an application chain (the `TypeAliasDefinition` comment documents the same
  latent bug). Keep this regardless of how Phase 4 resolves.

**The blocker (original) — width dispatch needs a *value-level* `BigInteger`.** The dispatch compares range
bounds to literal width thresholds (`-128`, `127`, …) as `BigInteger`. When `f8126ae1` was written this could not
be expressed in a *checked* `def` body:
- `BigInteger` is a **purely type-level type** — it has no value constructor and no value-position literal. A
  value-position literal `n` desugars to `Int[n,n]` (`integerLiteral`, Phase 6), so `def x: BigInteger = 5` is
  rejected (no `Coerce Int→BigInteger`). There was *no* expression of type `BigInteger` writable in a value body.
- A generic `BigInteger` parameter used as a value (`def f[N: BigInteger]: Bool = lessThanOrEqual(N, …)`) was
  rejected — "Type mismatch" on `N`.
- The `Coerce`/`Combine` instances get away with `lessThanOrEqual(Tmin, Smin)` on generic params only because
  ability-instance bodies are **evaluated** (resolved + spliced), not generically checked — and the opaque `Int`
  body gets away with `fitsIn[-128, 127, MIN, MAX]` for the same reason (it is evaluated, never checked).

**The unblock — compile→runtime reification (replaces the proposed `bigInt[N]`).** The earlier fix was to add a
`bigInt[N] : BigInteger` primitive (a mirror of `integerLiteral`) that reified a compile-time `BigInteger` into a
value. That primitive is **dropped**: the now-built reification mechanism
(`docs/compile-runtime-reification-plan.md`, Stages 1–2) subsumes it generically. An erased (`[]`-bound) parameter
**referenced in value position** materialises into a constant at the `SemExpression → MonomorphicExpression`
boundary — no explicit reification call. Concretely, `def staticBound[N: BigInteger]: BigInteger = N` now
type-checks and lowers to a full-precision `BigInteger` literal (`ReificationTest`, green). That removes the
second blocker bullet above — a `[N: BigInteger]` bound **is** now usable as a value — so a value-level
`BigInteger` is available without any new primitive.

Two things follow. First, no `bigInt[…]` wrapper is needed anywhere: where a body does reference an erased bound
in value position, reification materialises it. Second — and this is the form actually chosen, confirmed with the
user — the width dispatch keeps `fitsIn` as an ordinary **value-parameter** predicate
(`fitsIn(lo, hi, min, max: BigInteger): Bool`, in `lang/.../BigInteger.els`) and it is the *call*
`fitsIn[-128, 127, MIN, MAX]` that is on the type level: the bracket arguments land in type context, so the
literals stay `BigInteger` and the bounds `MIN`/`MAX` flow through as type-args. `[...]` and `()` are the same
Π-application, so a type-level call on a value-parameter function is well-defined (evaluated via `applyValue`, the
`fold` selecting a single leaf at compile time), and the closed case (`fitsIn[1, 2]`) already type-checks
(`MonomorphicTypeCheckTest`). So reification is the *general* unblock — it retires `bigInt[N]` and proves a
value-level `BigInteger` exists — while this specific dispatch form's only remaining gap is a checker arity rule
(below), not reification.

**Remaining Phase-4 work (deferred until properly implemented):**
- The checked dispatch bodies still do not type-check: `fitsIn[-128, 127, Cmin, Cmax]` (a four-argument type-level
  call on the value-parameter `fitsIn`, two of whose arguments are the dispatch's own type-stack params) raises
  **"Too many type arguments."** in a checked caller (`TypeStackLoop.applyTypeArgs`), even though the closed
  two-argument `fitsIn[1, 2]` is accepted. Reconciling the checker's arity rule for type-level calls on
  value-parameter functions is the open task; until then the committed `+`/`-`/`*` bodies do not compile and the
  arithmetic integration tests fail.
- Once that compiles: run the regression suite (`2 + 3 * 4 = 14`, `count + count + count = 21`,
  representation-selection tests, widening tests), add mixed-width carry/cancellation + dedup tests, then proceed
  to Phase 5 (`Coerce` payload-splice for non-materialised cross-rep widening).

The remainder of this section is the original keyed-matrix design (still valid; reification only changes how a
value-level `BigInteger` is *obtained*, not the matrix or the leaf set).

## Phase 4 — Operations as Eliot dispatch to concretely-sized leaf natives

Every operation on `Int` is an **Eliot** function that dispatches via `selectByWidth` to leaf natives. The
leaf natives are declared **typed at `Int` ranges** (Option A) so the dispatch bodies type-check under the
global `opaque` (no `JvmByte` appears in Eliot, so the checker never needs to bridge `Int ≡ JvmByte`); the
Phase-3 pass then concretizes their monomorphized signatures to representation types for the backend.

```eliot
// abstract leaf natives — body-less; typed at Int ranges so they check under the global `opaque`.
// Each is keyed by (operand width → result width); post-Phase-3 each FQN unfolds to exactly ONE concrete
// signature, which the backend matches by FQN. So `…ByteToByte` and `…ByteToShort` are distinct functions.
nativeAddByteToByte  [MIN1,MAX1,MIN2,MAX2](a: Int[MIN1,MAX1], b: Int[MIN2,MAX2]): Int[MIN1+MIN2,MAX1+MAX2]  // (JvmByte ,JvmByte ):JvmByte
nativeAddByteToShort [MIN1,MAX1,MIN2,MAX2](a: Int[MIN1,MAX1], b: Int[MIN2,MAX2]): Int[MIN1+MIN2,MAX1+MAX2]  // (JvmByte ,JvmByte ):JvmShort
nativeAddShortToShort[MIN1,MAX1,MIN2,MAX2](a: Int[MIN1,MAX1], b: Int[MIN2,MAX2]): Int[MIN1+MIN2,MAX1+MAX2]  // (JvmShort,JvmShort):JvmShort
// …nativeAddShortToInt, nativeMulByteToShort, nativeMulShortToInt, … ; same matrix for `-`/`*`.
nativeReprConvert[FROM_MIN,FROM_MAX,TO_MIN,TO_MAX](x: Int[FROM_MIN,FROM_MAX]): Int[TO_MIN,TO_MAX]           // value-preserving widen/narrow
nativeIntLiteral[V](): Int[V, V]                                                                            // materialise constant V
nativeIntToString[MIN,MAX](x: Int[MIN,MAX]): String
```

- `+` / `-` / `*` dispatch in **two steps**, both via `selectByWidth`: **(1) equalize operands** — convert the
  narrower operand up to the wider operand's representation `Wc` with `nativeReprConvert` (always a lossless
  widen); **(2) select by result width** `Wr` — pick `nativeOp{Wc}To{Wr}`. Result width relative to `Wc`:
  *equal* (typical), *wider* (carry: +1 step for `±`, up to ~2× for `*` — e.g. `Int[0,1000]+Int[0,1000]
  :Int[0,2000]` stays `Short`, but `byte*byte` → `Short`), or *narrower* (additive cancellation:
  `Int[1000,1000]+Int[-1000,-1000] :Int[0,0]` has `Short` operands, `Byte` result — handled as
  `nativeAdd{Wc}To{Wc}` then a narrowing `nativeReprConvert`). The result width **must** be a dispatch key
  because computing at a width too small for the result overflows — you cannot add at `Byte` then widen. The
  backend implements each `nativeOp{Wc}To{Wr}` leaf as one fixed, branch-free sequence (e.g.
  `nativeAddByteToShort` = unbox bytes, sign-extend, `IADD`, box `Short`).
- On the JVM the common working representation is uniformly `Long`, so this collapses to today's
  "unbox → `LADD`/`LSUB`/`LMUL` → narrow to result → box" — a handful of `NativeImplementation` methods. On a
  microcontroller the same Eliot picks 8-/16-bit leaves; the door is open by construction, not bolted on.
- `nativeIntLiteral[V]` keeps `integerLiteral`'s compile-time-constant nature; after Phase 3 its result type
  is the concrete wrapper, so the backend boxes accordingly.
- Backend side: each leaf is a real `NativeImplementation` (like `printlnInternal`/`unit`) — a fixed
  instruction group with a fixed box, no branching, recognised by FQN, generated against the concrete
  post-Phase-3 signature.

## Phase 5 — `Coerce` as leaf-native widening

`Coerce` becomes a real conversion (retype-only is unsound once representations differ — a boxed `Byte` would
be `checkcast` to `Short`):

- **Checker (principled payload-splice):** in `tryCoerce`, instead of `expr.copy(expressionType = expected)`,
  splice the resolved `Coerce` instance's `some` payload (for `Int`, `nativeReprConvert[S,T](value)`) as a real
  expression node wrapping the term — carrying source and target bounds. Done by-name through the existing
  `coercionHolds` machinery (evaluate `coerce` with `value` bound to a fresh neutral, read back the `some`
  payload, substitute the actual expression). No `Int`-specific code in the checker; any future `Coerce`
  instance works the same way.
- **Backend:** `nativeReprConvert` is the same leaf used by Phase 4 — after Phase 3 its signature is concrete
  (`(JvmByte): JvmShort`, …). It reads, extends/narrows, and boxes the target wrapper; when source and target
  share a representation it is the identity native (optimisable to a no-op), so all-same-representation
  widening stays correct and cheap.

No representation `switch` in Scala anywhere; every source→target choice is an Eliot fold.

---

## Decisions (settled)

- **`opaque type Int` body** carries the representation policy; `Int` stays abstract on the base layer. (Chosen
  over a uniform `java.lang.Number` carrier — that was a JVM-only artefact of erased references and does not
  reach an MCU; chosen over a new `Representation` ability — the opaque body already serves that role, so no
  type-system addition.)
- **Leaf natives typed at `Int` ranges (Option A)** so dispatch bodies type-check under today's global
  `opaque`; Phase 3 concretizes them. (The alternative — `JvmByte`-typed native signatures written directly in
  Eliot — would require `opaque` to be transparent *within its defining layer*, a scope-rule change to the
  modifier; deferred.)
- **Over-`Long` ranges** (e.g. `UnsignedLong = [0, 2^64-1]`) → `java.math.BigInteger` (`JvmBigInteger`), via
  its own leaves (no lossy `longValue` round-trip).
- **Multiplication grows unbounded into `BigInteger`** — no saturation or rejection. `*` follows the same
  `(Wc → Wr)` rule as `±`, except `Wr` can be up to ~2× the operand width, so the `nativeMul*` matrix includes
  widening leaves all the way up (`nativeMulIntToLong`, `nativeMulLongToBigInteger`, …); any product whose
  result range exceeds signed `Long` selects `JvmBigInteger` exactly like an over-`Long` literal range.
- **Representation kept boxed** (`java.lang.{Byte,Short,Integer,Long}` / `BigInteger`), not unboxed primitives —
  preserves the `Function.apply(Object)`/erased-field ABI. Unboxed primitives are a separate, larger effort and
  the natural divergence point for microcontroller backends.

## Risks / open questions

- **Termination of the opaque body / `representationOf`.** Unfolding runs user Eliot at compile time, which
  Girard's paradox permits to diverge; bounded by the recursion/effect model work, not here. The width `fold`
  tree is non-recursive, so in practice it terminates.
- **Operand/result width reconciliation** (Phase 4) is the subtle correctness point: operations are keyed by
  *both* operand and result representation (`nativeAddByteToByte` vs `nativeAddByteToShort` are distinct
  natives — each FQN must unfold to one concrete signature for the backend to implement it). Enumerate the
  needed `(Wc → Wr)` leaves per operator; `Wr` can be equal, wider (carry/product growth), or narrower
  (additive cancellation, composed with a narrowing convert). Test mixed-width operands, carry, and
  cancellation explicitly.
- **`opaque` flag through module unification** — confirm it is preserved when the concrete `Int` is chosen.
- **Lowering mechanism** — a pre-codegen pass over `UncurriedMonomorphicValue` (keeps `valueType` pure, enables
  dedup) vs. an effectful on-demand resolver at each descriptor site. The pass is recommended.

## Tests

- **Representation selection** (jvm processor test): `Int[0,127]`→`Byte`, `Int[0,255]`→`Short`,
  `Int[0,70000]`→`Int`, a wide range→`Long`, `UnsignedLong`→`BigInteger` (assert the chosen JVM class /
  descriptor).
- **Runtime widening across representations** (`ExamplesIntegrationTest`): `Byte`→`Int[0,1000]` (Short) prints
  correctly; an arithmetic result whose operands and result span different representations; a data type with
  mixed-width `Int` fields round-trips.
- **Mangling/dedup**: a generic `f[A](a: A): A` instantiated at two ranges sharing a representation produces
  one method (or distinct, collision-free methods).
- **Regression**: all existing widening/arithmetic tests still pass (the all-`Long`-today programs now pick
  narrower representations but must print identically); `nativeReprConvert` no-op when representations coincide.
- **Soundness** (already covered by the `opaque` `W[1]`/`W[2]` test): distinct ranges never merge.

## Sequencing

Phase 1 → 2 (land the representation vocabulary and the Eliot policy; nothing observable yet) → 3 (the unfold
pass; backend now sees `Jvm*`, descriptors/boxing narrow) → 4 (operations) → 5 (`Coerce`), verifying tests at
each step. The narrower-wrapper programs must stay print-identical to today.
