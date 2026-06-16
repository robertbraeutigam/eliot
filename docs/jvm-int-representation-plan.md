# Exact `Int` representations via `opaque` + Eliot dispatch

Status: **planned** (the `opaque` modifier it depends on is **implemented** — see "Foundation").

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

## Phase 1 — Representation types

Body-less platform types (like `String`/`Unit` today), mapped to JVM classes:

- `jvm/resources/.../Jvm.els`: `type JvmByte`, `type JvmShort`, `type JvmInt`, `type JvmLong`,
  `type JvmBigInteger`.
- `NativeType.types`: add `JvmByte → java.lang.Byte`, `JvmShort → java.lang.Short`, `JvmInt →
  java.lang.Integer`, `JvmLong → java.lang.Long`, `JvmBigInteger → java.math.BigInteger`; **remove** the
  `Int → Long` entry — `Int` is no longer native-mapped; it lowers through its opaque body.

These five entries are the **entire** new Scala "type knowledge."

## Phase 2 — The representation policy, in Eliot (`opaque type Int`)

The jvm layer redefines `Int` with an `opaque` body that selects the representation from the bounds, using the
existing compile-time `BigInteger` predicates (`lessThanOrEqual`, `&&`, `fold`). Factor the width table into
one helper so operations (Phase 4) and `Coerce` (Phase 5) dispatch on the *same* table:

```eliot
// jvm layer; signature identical to base so the module merge accepts the added body.
opaque type Int[MIN: BigInteger, MAX: BigInteger] = selectByWidth[MIN, MAX](JvmByte, JvmShort, JvmInt, JvmLong, JvmBigInteger)

selectByWidth[MIN: BigInteger, MAX: BigInteger, R](byteCase: R, shortCase: R, intCase: R, longCase: R, bigCase: R): R =
  fold(lessThanOrEqual(-128, MIN) && lessThanOrEqual(MAX, 127),                                  byteCase,
  fold(lessThanOrEqual(-32768, MIN) && lessThanOrEqual(MAX, 32767),                              shortCase,
  fold(lessThanOrEqual(-2147483648, MIN) && lessThanOrEqual(MAX, 2147483647),                    intCase,
  fold(lessThanOrEqual(-9223372036854775808, MIN) && lessThanOrEqual(MAX, 9223372036854775807), longCase,
       bigCase))))
```

Module unification (`UnifiedModuleValueProcessor`) prefers the implementation (`runtime.isDefined`): base `Int`
stays body-less, jvm `Int` carries the opaque body. **Verify** the merge preserves the `opaque` flag of the
chosen definition (it selects a whole `NamedValue`, which now carries `opaque`); add a test if not. The whole
width table lives here, in Eliot — adding a width = editing this file.

## Phase 3 — The generic "unfold to representation" pass

The single mechanism that re-reveals the representation hidden by opacity. It runs **after** type-checking, on
monomorphized values, and is **not** `Int`-specific.

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
