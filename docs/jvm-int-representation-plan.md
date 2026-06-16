# JVM exact `Int` representations via `opaque`

Status: **planned** (the `opaque` modifier it depends on is **implemented** — see "Foundation" below).

Goal: have the JVM backend store each `Int[MIN, MAX]` in the *smallest* machine representation whose
range contains `[MIN, MAX]` (`Byte`/`Short`/`Int`/`Long`, with a `BigInteger` fall-back for ranges that
exceed signed `Long`), and make range widening (`Coerce`) a *real* runtime conversion. Crucially, the
backend must do this **without any `Int`-specific Scala code**: it only ever maps a small, fixed set of
platform representation types to JVM classes and applies one general rule — *unfold `opaque` types until
you reach a representation you know*.

## Core principle: representation is *derived from* the type, never *is* the type

`Int[0,255]` and `Int[0,1000]` must stay **distinct types** for the checker — otherwise the checker would
accept assigning a `[0,1000]` value into a `[0,255]` slot (both "are" a 16-bit int). So the bounds are the
type's identity and meaning for assignability/`Coerce`/arithmetic, and they must survive type-checking
intact. The machine representation is a *separate, later* fact computed from those bounds.

The `opaque` modifier is exactly the barrier that makes this sound: the jvm layer gives `Int` a body that
computes its representation, but marks it `opaque` so the **type checker never unfolds it** (it stays a
stuck, bounds-carrying `Int[MIN,MAX]`), while **later phases may unfold it** to reach the representation.
This restores the otherwise-unsound `type Int = <repr>` idea by phase-gating the unfolding.

## Foundation (already implemented)

The `opaque` modifier exists and works in the type checker:

- Parsed on `type`/`def` (`opaque type Int[MIN,MAX] = …`), threaded `FunctionDefinition` → `NamedValue` →
  `ResolvedValue` → `MatchDesugaredValue` → `OperatorResolvedValue` as a `opaque: Boolean` field.
- `UserValueNativesProcessor` builds an `opaque` definition's checking binding as a stuck
  `VTopDef(fqn, None, …)` (body **not** cached), so the NbE evaluator never unfolds it during checking.
  The body remains in `OperatorResolvedValue.runtime` for later phases to unfold.

Verified property (test in `MonomorphicTypeCheckTest`): for `opaque type W[N: BigInteger] = String`, the
types `W[1]` and `W[2]` stay distinct (a `W[1]` is rejected where `W[2]` is expected), whereas the
transparent `type W[N] = String` collapses them. That distinctness is the soundness guarantee this plan
relies on.

## Current backend state (what changes)

- `NativeType.types` maps the `Int` FQN to `java.lang.Long`; **every** `Int` is a boxed `Long`.
- `CommonPatterns.valueType(gv)` keys only off the type's FQN — the bounds in
  `GroundValue.Structure(IntFQN, [min,max], Type)` are ignored.
- `Intrinsics` emits `integerLiteral`/`+`/`-`/`*`/`intToString` inline on `Long`
  (`ExpressionCodeGenerator.generateIntrinsic`).
- `Coerce` is **retype-only**: `Checker.tryCoerce` does `expr.copy(expressionType = expected)` and discards
  the `nativeWiden(value)` payload. This is sound *only* because all ranges share the `Long`
  representation; it breaks the moment representations differ.

## The lowering chain (zero `Int` knowledge in Scala)

```
Int[0,255]            -- abstract, bounds-carrying; the checker's view (opaque, never unfolded)
  │  (jvm layer: opaque Int body, in Eliot)
  ▼  unfold opaque, force with concrete bounds
JvmShort              -- a fixed platform representation type
  │  (NativeType map: one line)
  ▼
java.lang.Short       -- the JVM class
```

The backend rule is general: *to lay out a value, force its type through `opaque` definitions until the
head is a type with a native mapping.* It never names `Int`.

---

## Phase 1 — Representation types

Declare opaque-/body-less platform types and map them in the backend:

- `jvm/resources/.../Jvm.els` (or per-type): `type JvmByte`, `type JvmShort`, `type JvmInt`,
  `type JvmLong`, `type JvmBigInteger` — body-less platform types (like `String`/`Unit` today).
- `NativeType.types`: add `JvmByte → java.lang.Byte`, `JvmShort → java.lang.Short`,
  `JvmInt → java.lang.Integer`, `JvmLong → java.lang.Long`, `JvmBigInteger → java.math.BigInteger`.
- Remove the `Int → Long` entry — `Int` is no longer a native-mapped type; it lowers through `jvmRepr`.

These five entries are the **entire** new Scala "type knowledge".

## Phase 2 — The representation policy, in Eliot (`opaque type Int`)

The jvm layer redefines `Int` with an `opaque` body that selects the representation from the bounds, using
the existing compile-time `BigInteger` predicates (`lessThanOrEqual`, `&&`, `fold`):

```eliot
// jvm layer; signature identical to base so the module merge accepts the added body
opaque type Int[MIN: BigInteger, MAX: BigInteger] =
  fold(lessThanOrEqual(-128, MIN) && lessThanOrEqual(MAX, 127),          JvmByte,
  fold(lessThanOrEqual(-32768, MIN) && lessThanOrEqual(MAX, 32767),      JvmShort,
  fold(lessThanOrEqual(-2147483648, MIN) && lessThanOrEqual(MAX, 2147483647), JvmInt,
  fold(lessThanOrEqual(-9223372036854775808, MIN) && lessThanOrEqual(MAX, 9223372036854775807), JvmLong,
       JvmBigInteger))))
```

Module unification (`UnifiedModuleValueProcessor`) prefers the implementation (`runtime.isDefined`); the
base `Int` stays body-less, the jvm `Int` carries the body. **Verify** the merge preserves the `opaque`
flag of the chosen definition (it selects a whole `NamedValue`, which now carries `opaque`) — add a test if
not.

Because the body is `opaque`, the checker keeps `Int[min,max]` stuck and bounds-carrying; only the backend
unfolds it. The whole width table lives here, in Eliot — adding a width = editing this file.

## Phase 3 — Backend unfolds opaque to a representation

The checking binding for `Int` is stuck (`cached = None`), so the backend needs a *transparent* evaluator
that **does** cache opaque bodies. Add:

- A `TransparentBinding` fact (or a flag on `NativeBinding`) produced like `UserValueNativesProcessor` but
  **without** the `if (fact.opaque) None` guard — i.e. opaque bodies are cached.
- A `representationOf(gv: GroundValue): CompilerIO[GroundValue]` in the jvm module: if `gv`'s head FQN is
  native-mapped, return it; otherwise fetch the head's `OperatorResolvedValue`, evaluate its (opaque) body
  applied to `gv`'s arguments via the transparent evaluator, `force` to weak-head normal form, quote, and
  recurse (an opaque type may unfold to another). The recursion bottoms out at a `Jvm*`/native FQN.

This is the one general rule; it is not specific to `Int`.

## Phase 4 — Representation-aware descriptors, boxing, and de-duplication

`Int[min,max]` survives monomorphization unchanged (the checker keeps it stuck), so two ranges remain
distinct `UncurriedMonomorphicValue`s. Lower representations **once**, just before codegen:

- A jvm-module lowering step rewrites every `GroundValue` in an `UncurriedMonomorphicValue` via
  `representationOf`, so `Int[0,255]` becomes `JvmShort`. After this, `CommonPatterns.valueType` stays pure
  and the existing ~33 descriptor sites need no change — they already key off the (now `Jvm*`) FQN.
- **De-duplicate** values that become identical after lowering: `Int[0,3]` and `Int[0,5]` both lower to
  `JvmByte` with byte-identical bodies. Canonicalising/deduping them dissolves the method-name collision
  that `mangleSuffix` (currently `"Int"` for every range) would otherwise produce. Without dedup, fold the
  representation into the mangle suffix instead.
- **Reads vs. writes:** every wrapper is a `java.lang.Number`, so unboxing is uniformly
  `Number.longValue()`; only *boxing* (`Byte.valueOf`/`Short.valueOf`/…) and *descriptors* need the exact
  wrapper. `JvmBigInteger` is the exception — it needs its own read/box path (`BigInteger`, no `longValue`
  round-trip).

## Phase 5 — Representation-aware operations

`ExpressionCodeGenerator.generateIntrinsic` becomes representation-aware (it can read each operand's and the
result's representation via the lowered type):

- `integerLiteral[V]` → box `V` into `representationOf(Int[V,V])`'s wrapper.
- `+` / `-` / `*` → unbox operands with `Number.longValue()`, compute in `long` (`LADD`/`LSUB`/`LMUL`),
  narrow to the **result** representation (`l2i`/`i2s`/`i2b`), box. The dependent bounds guarantee the
  result fits, so narrowing is lossless. The over-`Long` (`JvmBigInteger`) case takes a parallel
  `BigInteger` arithmetic arm.
- `intToString` → `Number.longValue()` + `Long.toString` (or `BigInteger.toString`).

On the JVM the uniform-`long` path keeps these as a handful of intrinsics. On a true microcontroller backend
the same shape is where per-width Eliot dispatch to tiny leaf natives (`nativeAddByte` vs `nativeAddInt`)
pays off; the design leaves that door open.

## Phase 6 — `Coerce` as a real conversion

Two coordinated changes (representation differences make the current retype-only `Coerce` unsound — a boxed
`Byte` would be `checkcast` to `Short`):

- **Checker (principled payload-splice):** in `tryCoerce`, instead of `expr.copy(expressionType = expected)`,
  splice the resolved `Coerce` instance's `some` payload (for `Int`, `nativeWiden[S,T](value)`) as a real
  expression node wrapping the term — carrying source and target bounds. Done by-name through the existing
  `coercionHolds` machinery (evaluate `coerce` with `value` bound to a fresh neutral, read back the `some`
  payload, substitute the actual expression). No `Int`-specific code in the checker; any future `Coerce`
  instance works the same way.
- **Backend (`nativeWiden` intrinsic):** add `nativeWiden` to `Intrinsics`; emit `Number.longValue()` →
  narrow/extend to the target representation → box into the target wrapper (with a `BigInteger` arm). When
  source and target representations coincide it collapses to a no-op, so the all-`Long` widening tests stay
  green.

---

## Decisions (settled)

- **Over-`Long` ranges** (e.g. `UnsignedLong = [0, 2^64-1]`) → `java.math.BigInteger` (`JvmBigInteger`).
- **`Coerce` insertion** → principled payload-splice (above), not synthesising a hard-coded `nativeWiden`.
- **Representation kept boxed** (`java.lang.{Byte,Short,Integer,Long}` / `BigInteger`), not unboxed
  primitives — the `Function.apply(Object)`/erased-field ABI is preserved. Genuine unboxed primitives are a
  separate, larger effort and the natural divergence point for microcontroller backends.

## Risks / open questions

- **Termination of the opaque body / `representationOf`.** Unfolding runs user Eliot at compile time, which
  Girard's paradox permits to diverge; bounded by the recursion/effect model work, not here. The width
  `fold` tree is non-recursive, so in practice it terminates.
- **Lowering mechanism.** A pre-codegen lowering pass over `UncurriedMonomorphicValue` (keeps `valueType`
  pure) vs. an effectful on-demand resolver at each descriptor site. The pass is recommended; it also
  enables the post-lowering de-dup that dissolves the mangling collision.
- **`opaque` flag through module unification** — confirm it is preserved when the concrete `Int` is chosen.

## Tests

- **Representation selection** (jvm processor test): `Int[0,127]`→`Byte`, `Int[0,255]`→`Short`,
  `Int[0,70000]`→`Int`, a wide range→`Long`, `UnsignedLong`→`BigInteger` (assert the chosen JVM class /
  descriptor).
- **Runtime widening across representations** (`ExamplesIntegrationTest`): `Byte`→`Int[0,1000]` (Short)
  prints correctly; an arithmetic result whose operands and result span different representations; a data
  type with mixed-width `Int` fields round-trips.
- **Mangling/dedup**: a generic `f[A](a: A): A` instantiated at two ranges sharing a representation produces
  one method (or distinct, collision-free methods).
- **Regression**: all existing widening/arithmetic tests still pass (the all-`Long`-today programs now pick
  narrower representations but must print identically); `nativeWiden` no-op when representations coincide.
- **Soundness** (already covered by the `opaque` `W[1]`/`W[2]` test): distinct ranges never merge.

## Sequencing

Phase 1 → 2 (land the representation vocabulary and the Eliot policy; nothing observable yet) → 3 → 4
(backend sees `Jvm*`; descriptors/boxing narrow) → 5 (operations) → 6 (`Coerce`), verifying tests at each
step.
