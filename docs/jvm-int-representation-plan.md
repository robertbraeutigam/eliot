# JVM exact `Int` representations via leaf natives + Eliot dispatch

Status: **planned**.

Goal: the JVM backend stores each `Int[MIN, MAX]` value in the *smallest* boxed wrapper whose range
contains `[MIN, MAX]` (`Byte`/`Short`/`Integer`/`Long`, with `BigInteger` for ranges exceeding signed
`Long`), and range widening (`Coerce`) is a *real* runtime conversion. Crucially the backend does this with
**no `Int`-specific Scala code and no type unfolding**: all width *policy* — which representation a range
uses, which operation/conversion to run — lives in Eliot, and the backend only ever sees **easy-to-map
native things**: one uniform carrier type plus a fixed set of trivial leaf natives.

## Core principle: width is a value-level detail, not a static-type detail

`Int[0,255]` and `Int[0,1000]` must stay **distinct types** for the checker — otherwise it would accept
assigning a `[0,1000]` value into a `[0,255]` slot. So the bounds are the type's identity for
assignability/`Coerce`/arithmetic, and they survive type-checking intact. `Int` therefore stays **abstract**
(body-less) on every layer — base *and* jvm — which already keeps distinct ranges distinct (there is nothing
to unfold). No `opaque` modifier is needed: abstractness alone gives the soundness guarantee.

The exact machine width is then **not** something the static type exposes at all. It is an internal detail of
the *values*: a boxed `java.lang.Number`'s concrete subclass (`Byte` vs `Long`) is chosen by the leaf native
that produced it, which the Eliot layer selected from the bounds. The static carrier the backend lays out is
a single uniform type.

## The lowering picture (zero `Int` knowledge in Scala)

```
Int[0,255]                 -- abstract, bounds-carrying; the checker's view (distinct per range)
  │  (jvm Int layer, in Eliot: selectByWidth fold over MIN/MAX)
  ▼  picks the leaf native + wrapper for this range
intLiteralShort / addShort / widenToShort …   -- fixed leaf natives (one instruction group each)
  │  (each leaf boxes its own fixed wrapper internally)
  ▼
java.lang.Short  ⊑  java.lang.Number           -- the runtime object; static carrier is Number
```

The backend's *only* type knowledge is `Int → java.lang.Number`. It never unfolds a type, never asks "what
width is this range" — the Eliot dispatch already routed each value through the width-correct leaf.

## Current backend state (what changes)

- `NativeType.types` maps the `Int` FQN to `java.lang.Long`; **every** `Int` is a boxed `Long`. → becomes
  `java.lang.Number`.
- `Intrinsics` emits `integerLiteral`/`+`/`-`/`*`/`intToString` inline on `Long`
  (`ExpressionCodeGenerator.generateIntrinsic`). → these become Eliot functions that dispatch to leaf natives;
  the inline `generateIntrinsic` representation logic goes away.
- `Coerce` is **retype-only**: `Checker.tryCoerce` does `expr.copy(expressionType = expected)` and discards
  the `nativeWiden(value)` payload. This is sound *only* because all ranges share one representation; it
  breaks the moment a `Byte`-boxed value must become a `Short`. → becomes a real payload-splice + leaf-native
  widening.

---

## Phase 1 — One carrier type

- `NativeType.types`: replace the `Int → java.lang.Long` entry with `Int → java.lang.Number`.

That single line is the **entire** backend type knowledge for `Int`. Every boxed width (`Byte`/`Short`/
`Integer`/`Long`/`BigInteger`) is-a `Number`, so descriptors, fields, and signatures are uniform; only the
*boxing* (which wrapper to allocate) varies, and that lives inside the leaf natives, not at any call site. No
`Jvm*` representation vocabulary is introduced.

## Phase 2 — The width table, in Eliot (`selectByWidth`)

A single Eliot helper in the jvm `Int` layer selects per range, using the existing compile-time `BigInteger`
predicates (`lessThanOrEqual`, `&&`, `fold`):

```eliot
// jvm Int layer; a plain (non-opaque) helper, not Int's type body — Int stays abstract.
selectByWidth[MIN: BigInteger, MAX: BigInteger, R](byteCase: R, shortCase: R, intCase: R, longCase: R, bigCase: R): R =
  fold(lessThanOrEqual(-128, MIN) && lessThanOrEqual(MAX, 127),                                   byteCase,
  fold(lessThanOrEqual(-32768, MIN) && lessThanOrEqual(MAX, 32767),                               shortCase,
  fold(lessThanOrEqual(-2147483648, MIN) && lessThanOrEqual(MAX, 2147483647),                     intCase,
  fold(lessThanOrEqual(-9223372036854775808, MIN) && lessThanOrEqual(MAX, 9223372036854775807),  longCase,
       bigCase))))
```

Both operation dispatch (Phase 5) and conversion dispatch (Phase 6) call this one helper, so the whole width
table lives here, in Eliot — adding a width = editing this one function.

## Phase 3 — (removed: no `representationOf`, no unfolding)

There is **no backend representation resolution**. Because the carrier is uniform `Number` and each value is
already boxed in its exact wrapper by the leaf native that produced it, the backend never unfolds `Int`,
needs no transparent/opaque evaluator, and runs no per-value lowering pass. This phase intentionally does
nothing — its prior job is dissolved by Phases 1, 2, and 5.

## Phase 4 — Descriptors, boxing, reads

- **Descriptors / fields / signatures:** uniform `java.lang.Number` (from the Phase 1 map). The ~33 existing
  descriptor sites and `CommonPatterns.valueType` stay pure and unchanged — they key off the `Int` FQN, which
  now maps to `Number`.
- **Boxing:** done *inside* the leaf natives; each hardcodes its own wrapper (`Byte.valueOf`/`Short.valueOf`/
  …). No call site chooses a wrapper.
- **Reads / unboxing:** uniformly `Number.longValue()` (or `intValue()`) for the `Byte…Long` family. The
  `BigInteger` family reads the `Number` *as* a `BigInteger` (it already is one) inside its own leaves — never
  a lossy `longValue` round-trip. Dispatch (Phase 5/6) guarantees BigInteger-range values only ever reach
  BigInteger leaves, so that cast is safe.
- **Mangling/dedup:** `mangleSuffix` is `"Int"` for every range *today already*, so this introduces no new
  collision. Distinct ranges that share a carrier (`Int[0,3]`, `Int[0,5]`) now have identical signatures and
  bodies and can be de-duplicated; if dedup is deferred, the existing per-range method identity is unchanged
  from today.

## Phase 5 — Operations as per-representation leaf natives, dispatched in Eliot

The arithmetic heavy lifting moves **out** of `ExpressionCodeGenerator.generateIntrinsic` (no representation
`switch` in Scala) and splits into (a) trivial **leaf natives** — each a *fixed* instruction sequence with a
*fixed* box, no branching — and (b) an **Eliot-level dispatch** in the jvm `Int` layer that picks the leaf
via `selectByWidth`.

Leaf natives, via the existing `NativeImplementation` pattern (real named methods, exactly like
`printlnInternal`/`unit` — one instruction group each, no conditionals):

- `addByte`/`addShort`/`addInt`: unbox both (`Number.intValue()`), `IADD`, narrow (`i2b`/`i2s`/none), box
  (`Byte`/`Short`/`Integer.valueOf`). `addLong`: `longValue` → `LADD` → box `Long`. `addBigInteger`:
  `BigInteger.add`.
- `subByte…`/`mulByte…`: identical shape with `ISUB`/`IMUL`/`LSUB`/`LMUL`/`BigInteger.{subtract,multiply}`.
- `intLiteralByte/Short/Int/Long/BigInteger[V]`: materialise the compile-time constant `V` and box into the
  matching wrapper (this keeps `integerLiteral`'s compile-time-constant nature, now one trivial native per
  representation).
- `toStringLong`/`toStringBigInteger`: `Long.toString` / `BigInteger.toString`.

Each add/sub/mul leaf takes operands that are **already at the result width** and returns that width — so it
needs no inter-width branching. The Eliot `+`/`-`/`*` bodies first `widenToResult` both operands (reusing the
Phase 6 widen leaves) then call the single same-width op leaf chosen by `selectByWidth` on the *result*
bounds. So every operation reduces to *widen + fixed-width op* — both halves dispatched in Eliot.

Scala holds only this fixed leaf set; it carries **zero `Int` policy**. This is the shape a microcontroller
backend reuses unchanged: there `addByte` is a tiny 8-bit add native instead of `IADD`+`i2b`, but the Eliot
dispatch is identical — JVM no longer takes a special uniform-`long` shortcut that would have to be unwound
later.

## Phase 6 — `Coerce` as leaf-native widening, dispatched in Eliot

Same split — the conversion logic lives in Eliot, the backend owns only fixed leaf conversions:

- **Checker (principled payload-splice):** in `tryCoerce`, instead of `expr.copy(expressionType = expected)`,
  splice the resolved `Coerce` instance's `some` payload (for `Int`, `nativeWiden[S,T](value)`) as a real
  expression node wrapping the term — carrying source and target bounds. Done by-name through the existing
  `coercionHolds` machinery (evaluate `coerce` with `value` bound to a fresh neutral, read back the `some`
  payload, substitute the actual expression). No `Int`-specific code in the checker; any future `Coerce`
  instance works the same way.
- **Backend = leaf natives + Eliot dispatch:** `nativeWiden`'s Eliot body uses `selectByWidth` on the target
  bounds to call a fixed leaf — `widenToByte/Short/Int/Long/BigInteger` (read `Number.longValue()`, extend,
  box the target wrapper; the `BigInteger` arm via `BigInteger.valueOf` / pass-through). When source and
  target select the same width the leaf is the identity native (optimisable away), so all-same-representation
  widening stays correct and cheap.

No representation `switch` in Scala anywhere; the source→target choice is an Eliot fold.

---

## Decisions (settled)

- **Carrier:** one uniform `java.lang.Number` static carrier; the exact wrapper is a value-level detail chosen
  by leaf natives. (Chosen over per-range `Jvm*` descriptors + a backend unfolding pass — see the
  `representationOf` discussion: the unfolding/opaque machinery is unnecessary once operations are leaf
  natives that box their own wrapper.)
- **No `opaque` for `Int`:** `Int` stays abstract on all layers; distinctness comes from abstractness. The
  `opaque` modifier (recently added) is **not used** by this plan — decide separately whether to keep it for
  other uses or revert it.
- **Over-`Long` ranges** (e.g. `UnsignedLong = [0, 2^64-1]`) → `java.math.BigInteger`, reached via its own
  leaf natives.
- **`Coerce` insertion** → principled payload-splice, not a hard-coded synthesised `nativeWiden`.
- **Representation kept boxed** (`java.lang.{Byte,Short,Integer,Long}` / `BigInteger`), not unboxed
  primitives — the `Function.apply(Object)`/erased-field ABI is preserved. Genuine unboxed primitives are a
  separate, larger effort and the natural divergence point for microcontroller backends.

## Risks / open questions

- **Width table runs user Eliot at compile time** (`selectByWidth`), which Girard's paradox permits to
  diverge; bounded by the recursion/effect model work, not here. The `fold` tree is non-recursive, so in
  practice it terminates.
- **Leaf-native count.** ~5 wrappers × {literal, add, sub, mul, widen, toString} is a fixed, modest set; each
  is a few instructions. Acceptable, and shared in shape with future backends.
- **De-dup vs. mangling.** Optional; no worse than today if skipped (see Phase 4).

## Tests

- **Width dispatch** (jvm processor / integration test): `Int[0,127]` boxes a `Byte`, `Int[0,255]` a `Short`,
  `Int[0,70000]` an `Integer`, a wide range a `Long`, `UnsignedLong` a `BigInteger` (assert the runtime class
  of the boxed value, e.g. via a value that round-trips through a field and is printed / reflected).
- **Runtime widening across representations** (`ExamplesIntegrationTest`): `Byte`→`Int[0,1000]` (Short) prints
  correctly; an arithmetic result whose operands and result span different representations; a data type with
  mixed-width `Int` fields round-trips.
- **Soundness:** distinct ranges never merge (an `Int[0,1000]` rejected where `Int[0,255]` is expected) —
  holds from `Int` being abstract.
- **Regression:** all existing widening/arithmetic tests still pass (programs that were all-`Long` today now
  box narrower wrappers but must print identically); `nativeWiden` is a no-op when source/target coincide.

## Sequencing

Phase 1 (carrier) + Phase 2 (`selectByWidth`) land the vocabulary. → Phase 5 (operations: leaf natives +
Eliot dispatch) replaces the inline intrinsics. → Phase 6 (`Coerce`). Phase 4 is mostly "nothing changes"
(uniform descriptors) plus optional dedup; Phase 3 is intentionally empty. Verify tests at each step; the
narrower-wrapper programs must stay print-identical to today.
