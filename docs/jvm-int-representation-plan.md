# Exact `Int` representations via `opaque` + Eliot dispatch

Status: **Phases 1–5 implemented.** The full jvm-int representation pipeline now compiles and the whole test suite
is green (`./mill __.test`), including all the `ExamplesIntegrationTest` arithmetic, representation-selection, and
cross-representation widening cases. After Phase 3 an `Int[MIN, MAX]` is laid out at the narrowest JVM wrapper its
range fits (`java.lang.{Byte,Short,Integer,Long}` / `BigInteger`), verified end-to-end for literal-position values
(`Int[0, 1000]` in `Short`, `Int[0, 70000]` in `Integer`, `Int[0, 5000000000]` in `Long`); Phase 4 realises every
operation as an Eliot width dispatch over concretely-sized leaf natives; Phase 5 makes cross-representation `Coerce`
a real runtime conversion.

**How Phase 4 was unblocked (differs from the earlier plan).** The committed `f8126ae1` dispatch did not compile
because `fitsIn[-128, 127, MIN, MAX]` (a value-parameter predicate applied with `[...]`) was reaching
`TypeStackLoop.applyTypeArgs` as a standalone monomorphic value and raising "Too many type arguments." The fix is
*not* a checker arity-rule change (the once-"deferred" task) and *not* making `fitsIn` generic (that breaks the
opaque `Int` body, whose free generic params would no longer evaluate). Instead the dispatch's `Bool` `fold` is
collapsed at compile time: `PostDrainQuoter` now performs **compile-time branch selection** — a `fold` whose
condition forces to a ground `true`/`false` is replaced by its selected branch (runtime content preserved). So the
condition `fitsIn[…]` (and `fold` itself) never survive into the monomorphic body, only the one chosen leaf native
does; `fitsIn` is therefore never monomorphized as a standalone value and the arity error never arises. `fitsIn`
stays a value-parameter predicate; `[...]` and `()` are the same Π-application, so the checked dispatch bodies type
it by stepping its `VPi` signature in `Checker.infer`. (The reification mechanism — Stages 1–2, built — remains the
general "erased param in value position" unblock and retired the proposed `bigInt[N]` primitive; it is *not* what
reduces the dispatch fold, which needed the new expression-level branch selection because the fold's branches carry
runtime values.)

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

## Phase 4 — status and how it was unblocked  *(implemented)*

**Decision (settled with the user):** the leaf matrix is **operand×result keyed** — `nativeAdd{Wc}To{Wr}`,
keyed by the common operand representation `Wc` and the result representation `Wr` (the "plan's literal matrix"
below), *not* the simpler diagonal. Chosen for microcontroller fidelity: a backend with no uniform `long`
carrier needs genuinely different per-width instruction sequences, so the *selection* must be in Eliot and the
backend must only realise fixed leaves. **No `Int` width policy may live in Scala.**

**What is committed (originally `f8126ae1`, now compiling):**
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

**The actual blocker that remained — the dispatch `fold` did not collapse.** With the committed bodies, the
checked dispatch (`dispatchAdd` etc.) type-checks fine: `fitsIn` stays a **value-parameter** predicate
(`fitsIn(lo, hi, min, max: BigInteger): Bool`), and a `fitsIn[-128, 127, Cmin, Cmax]` *call* type-checks because
`[...]` and `()` are the same Π-application — `Checker.infer` applies the bracket arguments by stepping `fitsIn`'s
`VPi` signature through `applyValue` (the literals land in type context and stay `BigInteger`; the bounds
`Cmin`/`Cmax` flow through as the dispatch's type-stack params). The real failure was downstream: the checker keeps
the body's call structure, so the monomorphic body of `dispatchAdd` retained the whole `fold(fitsIn[…], leafA,
leafB)` tree — with `fitsIn[…]` *surviving*. `UsedNamesProcessor` then requested
`MonomorphicValue.Key(fitsIn, [4 args])`, and monomorphizing `fitsIn` (a value-parameter, hence `VPi`-headed,
signature) hit `TypeStackLoop.applyTypeArgs`'s `VLam`-only peel → **"Too many type arguments."** Worse, even had
it compiled, the surviving `fold`/`fitsIn` are compile-time-only constructs the backend cannot emit.

**The fix — compile-time branch selection in `PostDrainQuoter`.** The dispatch `fold` has an erased-determined
*condition* but *runtime* branches (`left`/`right`), so the whole-node reification gate (Stages 1–2) cannot fire on
it. So `PostDrainQuoter` gained a dedicated step (`trySelectFold`): an application of the `Bool` eliminator `fold`
(`WellKnownTypes.boolFoldFQN`) whose condition evaluates (via `SemExpressionEvaluator` + `force`) to a ground
`true`/`false` is replaced by the **structurally-quoted selected branch**, dropping the other and keeping the
selected branch's runtime content verbatim. This is exactly `fold`'s documented native semantics ("reduces when the
condition is a concrete `true`/`false`"), lifted to the expression read-back. Consequences: the dispatch collapses
to its one chosen leaf native; `fold` and `fitsIn[…]` never reach the monomorphic body, so `fitsIn` is never
monomorphized as a standalone value (the arity error never arises) and no compile-time-only construct reaches
codegen. A condition that stays abstract (a genuine runtime `Bool`, e.g. user `fold(isEven, …)`) is left as an
ordinary `fold` application.

Note this is a *different* mechanism from reification (Stages 1–2, built): reification materialises a sub-term that
is *wholly* erased-determined into a constant; branch selection reduces a `fold` whose *condition* is
erased-determined while its *branches* stay runtime. The reification work remains the general "erased param in value
position" unblock (it retired the proposed `bigInt[N]` primitive and proves a value-level `BigInteger` exists, e.g.
`def staticBound[N: BigInteger]: BigInteger = N`), but it is not what reduces the dispatch fold.

**Rejected alternative — making `fitsIn` generic (`fitsIn[lo, hi, min, max: BigInteger]: Bool`).** This makes
`applyTypeArgs`'s `VLam` peel succeed, but it breaks the opaque `Int` body: with generic params, `fitsIn`'s body
references them as *free names* evaluated in `Env.empty` (`buildCurriedBody` only lambda-binds value params), so the
cached `VTopDef` body is a stuck neutral and type-arg application via spine-growth never binds them — representation
lowering of *every* `Int` (even a pure literal) then fails with "Cannot quote neutral value." Value-parameter
`fitsIn` evaluates correctly in the opaque body (its body *is* a `VLam` chain), which is why it is kept.

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

## Phase 5 — `Coerce` as leaf-native widening  *(implemented)*

`Coerce` is now a real conversion (retype-only was unsound once representations differ — a `3 + 4 : Int[7,7]`
(`Byte`) flowing into a declared `Int[0,1000]` (`Short`) slot threw `ClassCastException` at runtime, because the
arithmetic leaf genuinely produced a `Byte`):

- **Checker (principled payload-splice, as built):** `coercionHolds` was generalised to `coercionPayload`, which
  returns the resolved `Coerce` instance's `some` payload (the value the coercion yields, with the `coerce` argument
  left as a `$coerceArg` marker neutral). `tryCoerce` → `buildCoercedExpr` then inspects the payload: a payload of
  the shape `conv[…](marker)` — a single body-less conversion native applied to the marker — is spliced as a real
  `SemExpression.FunctionApplication(conv[srcArgs ++ tgtArgs], expr)` typed at `expected`, leaving `expr` at its
  narrower `actual` type; any other payload (an identity coercion whose payload is the bare marker) falls back to
  re-typing. No `Int`-specific code: the conversion FQN comes from the resolved instance, and its type arguments are
  the source type constructor's arguments followed by the target's (`[Smin, Smax, Tmin, Tmax]` for `Int`). For `Int`
  the conversion native is `nativeWiden`.
- **Backend:** `nativeWiden` is the same leaf used by Phase 4. The JVM realisation reads the source/target
  representations from the argument's and the call's (already Phase-3-lowered) types — *not* the type arguments — so
  it unboxes the source wrapper and reboxes at the target; when source and target share a representation it is an
  identity unbox→rebox (effectively a no-op), so all-same-representation widening stays correct and cheap.

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
  correctly *(covered)*; an arithmetic result whose operands and result span different representations *(covered)*.
  A `data` type with mixed-width `Int` fields round-tripping is **blocked by a separate, pre-existing backend gap,
  not a jvm-int issue**. *Constructing* a multi-field `data` value codegens fine; *eliminating* it (pattern match
  or field accessor — accessors desugar to a `match`) does not: a `match` on an N-field constructor produces an
  N-parameter handler, which the uncurry pass hands the backend as a multi-parameter lambda, and
  `LambdaGenerator.generateLambda` is `??? // Multi-parameter lambdas not currently supported` for
  `parameters.length > 1` (long-standing, since `fc4a3f72`). So matching/accessing *any* two-field `data` — even
  one with `String` fields — fails codegen with `NotImplementedError`; single-field data works throughout (every
  `data` in the repo has one value field). Implementing multi-parameter lambdas is its own task (general backend
  codegen), outside this plan.
- **Mangling/dedup** *(covered)*: a generic `id[Mn, Mx](x: Int[Mn, Mx])` instantiated at two ranges sharing a
  representation (`Int[0,3]`, `Int[0,5]` → `Byte`) reuses one method, and at two ranges with different
  representations (`Byte` and `Long`) dispatches collision-free distinct methods — both verified at runtime in
  `ExamplesIntegrationTest`.
- **Regression**: all existing widening/arithmetic tests still pass (the all-`Long`-today programs now pick
  narrower representations but must print identically); `nativeReprConvert` no-op when representations coincide.
- **Soundness** (already covered by the `opaque` `W[1]`/`W[2]` test): distinct ranges never merge.

## Sequencing

Phase 1 → 2 (land the representation vocabulary and the Eliot policy; nothing observable yet) → 3 (the unfold
pass; backend now sees `Jvm*`, descriptors/boxing narrow) → 4 (operations) → 5 (`Coerce`), verifying tests at
each step. The narrower-wrapper programs stay print-identical to today.

**All phases are implemented and the full suite is green.** `ExamplesIntegrationTest` covers arithmetic
(`3 + 4`, `2 + 3 * 4`, `3 - 10`), representation selection (`Int[0,70000]`→`Integer`, `1000 * 1000`→`Integer`,
`Int[0,5000000000]`→`Long`), bare-literal and arithmetic-result widening (`Int[0,1000] = 7` / `= 3 + 4`),
byte→short carry (`100 + 100 = 200`, via `nativeAddByteToShort`), short-operand→byte additive cancellation
(`1000 - 999 = 1`, via the narrowing outer `nativeWiden`), and generic-instantiation dedup across same- and
different-representation ranges.

**Remaining (small, optional) follow-ups, none blocking the plan:**
- `nativeWiden` no-op when source and target share a representation (currently an unbox→rebox round-trip; could be
  elided as an optimisation — correctness is fine).
- A `data` type with mixed-width `Int` fields is *not yet testable* end-to-end because multi-field `data`
  constructor codegen is unimplemented (the `LambdaGenerator` `???` above) — a separate general-`data` task.
- Termination of the opaque body / `representationOf` is bounded by the future recursion/effect model, not here.
