# A length meta for `String` — the refinement channel's second domain

Status: **design proposal**. The load-bearing mechanism was **prototyped end-to-end and reverted** — every claim
marked *verified* below was run against the real tree; everything else is design.

Prior art, and the two documents this one sits between:

- `docs/total-meta-transfers.md` — meta as a shadow logic (R1–R4, the leaf/derive split). Its §2.3 defers
  *structural* meta types to "the second refinement domain (`List`/`Array` `size`)". This is that second domain,
  in the one shape that needs **no** structural meta.
- The shipped value-range domain: `type Int {range: Interval[BigInteger]}`,
  `monomorphize/channel/RefinementChannelProcessor`, `core/processor/Meta{Constructor,Transfer,Where}Desugarer`.

---

## 1. What is being added, and why this one first

```eliot
type String {size: Interval[BigInteger]}
```

A `String` gains one meta slot: the interval of its length, in the units `String::length` counts. That single line
turns on the whole channel for strings — merges, `where` preconditions, transfers, hover — because everything
downstream of a slot declaration is already domain-agnostic.

Why `String` and not `List` first, even though the TODO names `List`/`Array` `size` as the next domain: **`String`
is nullary.** `meta(List[A])` wants to be structural (`List$Meta(size, meta(A))`, meta as a functor on the type
structure — `total-meta-transfers.md` §2.3), which drags in a decision about recursive `data` and multiplies the
leaves forced to state a transfer. `String` has no element type, so its meta is a plain one-slot structure exactly
like `Int`'s, and it needs **no** change to `metaTypeOf`, no functor, no recursion story. It is the second domain
minus its hardest sub-problem — and it is the domain that proves the machinery is genuinely generic rather than
`Int`-shaped, which is the precondition for `List` size later.

What it buys today:

- `where` preconditions over strings — the fixed-width case that motivates the whole channel on a microcontroller
  (`def banner(s: String): Unit where fitsDisplay(size(s))`).
- **Cross-domain flow**: `length`'s stated transfer turns a string's size into an `Int`'s range, so a literal's
  length feeds `Int` arithmetic, `Int` `where`s, and (later) array bounds. *Verified* (§4.4).
- A representation story for a future MCU backend: a `String` whose size is pinned to `[0, 16]` can be laid out as
  a fixed buffer, exactly as an `Int` pinned to `[0, 255]` is laid out as a byte today.
- The first honest test of R2 (`total-meta-transfers.md`) against a leaf set that is *not* three arithmetic
  operations. That test does not go well, and §7 says so.

---

## 2. What the existing machinery already gives, unchanged

Everything in this list works with **zero compiler change** once the slot is declared. This is the payoff of the
channel's "the `^Meta` companion is the one recognition point" discipline.

| mechanism | what happens for `String` | status |
|---|---|---|
| meta structure | `MetaConstructorDesugarer` emits `data String$Meta(size: Interval[BigInteger])` + accessor | *verified* |
| lattice | the auto-derived `Meta[String$Meta]` joins the slot through `Meta[Interval[T]]` | *verified* (compiles) |
| `metaTypeOf` | `String` ⤳ `String$Meta` by the same membership test, no new case | — |
| branch merges | `fold^Meta` reduces at `A := String$Meta`; `if`/`match` over strings joins for free | — |
| `where` | `MetaWhereDesugarer` already retypes *every* parameter `T` ⤳ `T$Meta`; a `where` over a `String` parameter needs no change (its own doc comment names this as the second-domain work) | *verified* (§4.3) |
| transfers | a return brace on a `String`-returning def desugars as usual; a **cross-domain** brace (`String$Meta` in, `Int$Meta` out) is just the ordinary per-position name transform | *verified* (§4.2) |
| backend | `IntRepresentation.isIntegerType` gates every width decision, so a `String$Meta` stamped on a node is ignored by the JVM backend | *verified* (§4.1) — but see §8 |

---

## 3. The three compiler changes

### 3.1 The α seed for string literals (the only real one)

Today a string literal is ⊤: `RefinementChannelProcessor.walkFlow` falls through to `case _`. Nothing else can
originate a `String` meta, so without a seed the whole domain is inert — which is precisely what makes the
declaration safe to land alone (§4.1).

Mirror the integer seed. `eliot.lang.Runtime` already carries `def integerLiteral[V: BigInteger]: Int {Interval(V,
V)}`; add its string twin and one arm in the channel:

```eliot
// stdlib/eliot/eliot/lang/Runtime.els
def stringLiteral[N: BigInteger]: String {Interval(N, N)}
```

```scala
// RefinementChannelProcessor.walkFlow, beside the IntegerLiteral arm
case MonomorphicExpression.StringLiteral(value) =>
  metaViaCompanion(
    WellKnownTypes.stringLiteralFQN,
    Seq(GroundValue.Direct(Literal.IntegerValue(BigInt(value.value.length)), bigIntType)),
    Seq.empty
  ).map(meta => Flow(meta, recordAt(node, meta)))
```

Nineteen lines including the `WellKnownTypes` entry and the slot itself — *verified*, end to end (§4).

Note what the channel does and does not decide. It supplies a raw `BigInteger` — the literal's measured length —
exactly as the integer seed supplies the literal's value; the *interval*, the singleton, and the `String$Meta`
wrapper are all built in Eliot, in the brace. The channel still constructs no domain structure. It does now know
that a string's meta is measured by counting, which is one notch more domain knowledge than the integer seed
carries, and §5 is the price of that notch.

The orthodox alternative — pass the literal *string* as the type argument and let the brace call a compile-time
`sizeOf(v: String): BigInteger` native — keeps that last notch in Eliot and makes the unit **platform-contributable**
(§5). It costs one native and one more name. Recommended if §5 is decided the strict way; the measured-count form
above is fine if it is decided the pragmatic way.

### 3.2 The slot cannot be called `length`

*Verified, and it is a hard error, not a preference:*

```
stdlib/eliot/eliot/lang/String.els:4: Name was already defined in this module.
  type String {length: Interval[BigInteger]}
               ^^^^^^
```

A meta slot generates its accessor (`length(m: String$Meta): Interval[BigInteger]`) in the type's **own module and
the Default namespace**, where `def length(s: String): Int` already lives. `Int` never hit this because nothing in
`eliot.lang.Int` is called `range`.

Two ways out:

- **v1: name the slot `size`.** Reads well in the two places it appears (`size(s)` in a brace or a `where`), leaves
  `length` to the runtime function it belongs to, and costs nothing. *Verified.*
- **The structural fix: put meta accessors in `Qualifier.Meta`.** A slot accessor is compile-time-only machinery; it
  has no business sharing a namespace with the type's public API, and the collision will recur for every future
  domain whose natural slot name is also a natural function name (`List`'s `size` and `List::size`, say). It means
  teaching the brace/`where` desugarers to resolve accessors in that namespace, so it is its own small slice.

Take `size` now; log the namespace fix as the follow-on that lets a later domain use whatever name it wants.

### 3.3 The LSP hover would mislabel a string's size as a value range

`TypeHintIndex.boundsOf` decodes *any* `X$Meta(Interval(lo, hi))` shape and the hover renders it as the node's value
range. Its own comment says "`None` for any other meta (a future domain), which is simply not shown as a range" —
but the match is structural, and `String$Meta(Interval(5, 5))` has exactly that shape. Hovering `"hello"` would
report a value range of `5..5`.

Fix with the same gate the backend uses: decode only when the node's type is the tracked `Int` (or only when the
meta's own FQN is `Int$Meta`). Cosmetic, one line, but it lands the moment the seed does — and §7's guidance is that
a wrong *rendering* is the only place recognising a domain by name is sanctioned, so this is where to do it.

---

## 4. What the prototype proved

The prototype was: the `size` slot, the `Meta` import in `String.els`, `stringLiteral`'s declaration, the
`WellKnownTypes` FQN, and the channel arm — 19 lines. Everything below was run.

### 4.1 The declaration alone is inert and harmless

With `type String {size: Interval[BigInteger]}` and no seed, `HelloWorld`, `Strings`, `Concat`, `Unicode`,
`WherePrecondition`, `Ranges`, `MatchRanges`, `IntField`, `Match` and `Arithmetic` all compile clean. Nothing
originates a string meta, so every node stays ⊤. The slot can therefore land as its own commit.

### 4.2 Cross-domain transfers desugar and typecheck

Both of these compile with no compiler change:

```eliot
def length(s: String): Int {size(s)}                      // String$Meta in, Int$Meta out
implement Combine[String] {
   def combine(a: String, b: String): String {size(a) + size(b)}   // Interval addition, as Int's `add`
}
```

`length^Meta(s: String$Meta): Int$Meta = Int$Meta(size(s))` falls straight out of `MetaTransferDesugarer`'s
per-position name transform — the mixed-domain case needed no special arm, which is the strongest evidence that the
transfer language is not `Int`-shaped.

### 4.3 A `where` over a string parameter works, in both directions

```eliot
def fitsFive(i: Interval[BigInteger]): Bool = lessThanOrEqual(end(i), 5)
def label(s: String): String where fitsFive(size(s)) = s

def main: {Console} Unit = printLine(label("hello"))          // compiles, runs, prints `hello`
```

and with an eleven-character literal:

```
StringSize.els:5:38: The precondition of 'StringSize::label' is not satisfied by the argument's value range.
```

A use-site check, discharged from the literal seed through the auto-derived instances, with no channel knowledge of
strings beyond the seed.

### 4.4 The cross-domain path carries all the way into the `Int` domain

```eliot
def withinFive(i: Interval[BigInteger]): Bool = lessThanOrEqual(end(i), 5)
def small(x: Int): Int where withinFive(range(x)) = x

def main: {Console} Unit = printLine(show(small(length("hello"))))        // accepted
//                                         small(length("hello world"))  // rejected at the call
```

`"hello"` ⤳ `String$Meta(Interval(5,5))` ⤳ `length^Meta` ⤳ `Int$Meta(Interval(5,5))` ⤳ the `Int` `where`. The two
domains compose through the ordinary machinery.

### 4.5 …and that exact program then fails to *run* — see §8

---

## 5. Which units? The one platform-independence question

`String.els` documents `length` as counting "the platform's storage units, not user-perceived characters", and the
JVM leaf is `java.lang.String::length` — UTF-16 code units. A UTF-8 target would count differently for the same
literal. So "the length of `"日本"`" is not a platform-independent number, and the seed has to get it from
*somewhere*.

Three answers, in increasing strictness:

1. **Pragmatic (what the prototype did).** The channel counts with the host's `String.length`. That is exactly what
   `StringReductions` already does for the compile-time `length` native — so the seed is consistent with the
   compile-time leaf **by construction**, and the pre-existing tension (a UTF-8 target's runtime `length` would
   disagree with `StringReductions`) is inherited, not created. Cheapest, and honest about being a JVM-flavoured
   assumption living in a platform-independent module.
2. **Contributed (recommended if the strict reading wins).** The seed calls a compile-time `sizeOf` native that
   lives *beside `length`'s own compile-time reduction* — today `StringReductions`, tomorrow whichever layer owns
   the target's `length` leaf. Then the unit is defined once, per platform, in exactly the place that already has to
   agree with the runtime leaf value-for-value. This is the `§2.1` rule of `total-meta-transfers.md` ("a leaf's
   transfer is platform data") applied to the seed.
3. **Define it away.** Declare the language's string length to be the **code-point** count and make every platform's
   `length` conform (JVM: `codePointCount`). Platform-independent by fiat, at the cost of a behaviour change to
   `length` and of making JVM slicing indices no longer the same units. Out of scope here, but worth recording as
   the only answer that makes `size` a portable number.

Whichever is chosen, say it in `String.els`'s doc comment: **the meta counts the same units `length` returns.** The
meta must never be able to disagree with the function.

---

## 6. The transfers the `String` leaves would state

Not needed for v1 (an unstated leaf is ⊤ — sound, just wide), but this is the list R2 will demand (§7), and working
it out is what shows where the domain is genuinely hard. `n` abbreviates `size(s)`.

| leaf | honest transfer | note |
|---|---|---|
| `combine` (`++`) | `{size(a) + size(b)}` | exact; `Interval` addition, as `Int`'s `add` |
| `length` | `{size(s)}` | cross-domain, exact |
| `substring(start, end, s)` | `{Interval(0, min(end(range(end)) - start(range(start)), end(n)))}` | clamping makes the lower bound `0` |
| `take` / `drop` | derived from `substring` — **if** derivation existed (§9) | today: bodied ⇒ ⊤ |
| `trim` | `{Interval(0, end(n))}` | |
| `toUpperCase` / `toLowerCase` | `{Interval(start(n), end(n) * 3)}` | **not** length-preserving: `ß` ⤳ `SS`, `ﬃ` ⤳ `FFI`. The discipline catches a bug an eyeball would not |
| `repeat(count, s)` | `{Interval(0, end(range(count)) * end(n))}` | the other cross-direction: an `Int` meta drives a `String` meta |
| `replace(target, replacement, s)` | `{Interval(0, end(n) + (end(n) + 1) * end(size(replacement)))}` | an empty `target` inserts between every pair, so the occurrence count is `n+1` |
| `Show[Int]::show` | needs a digit count of `range(value)` | **no honest bound on the JVM**, where `Int` is `BigInteger`-backed |
| `parseIntInternal`, `readLine`, environment/file/process reads | platform max | **no honest bound on the JVM** |

The last two rows matter more than the rest: `total-meta-transfers.md` §5 argues that no leaf needs a ⊤ form,
because "on a real target every meta-carrying type has a representation, hence a bound". Strings on the JVM are the
counterexample that argument's own §P2 already half-concedes for `parseInt`. Either `Interval` grows open endpoints,
or these leaves state `2³¹-1`-flavoured bounds that are true but useless, or R2 keeps an escape.

---

## 7. Interaction with R2 accounting — this makes arming harder, not easier

`MetaTransferAccountingProcessor` (landed at `526c7bc`, registered but **undemanded**) reports a body-less value
whose declared return head is a concrete meta-carrying type and which states no transfer. Its §P2 arming list was
five leaves: `String::length`, `indexOfInternal`, `parseIntInternal`, `Process::exitCode`, jvm `outcomeExitCode`.

Declaring `String` meta-carrying makes **every body-less `String`-returning leaf** join that list — the `String.els`
set above, plus `Path::show`, `File::message`, `Process::standardOutput`/`standardError`, `Environment`'s and the
jvm layer's private internals. Roughly a dozen more, several of which have no honest bound (§6).

The sequencing conclusion is one line: **land the `String` domain while R2 is dormant, and treat "arm R2" as
strictly downstream of the ⊤ decision.** Arming first would force a dozen dishonest braces; arming after, with open
endpoints decided, forces a dozen honest ones. Nothing here blocks the domain — an unstated leaf is ⊤ today, which
is exactly what a `String` leaf is right now anyway.

---

## 8. The backend gap the prototype found

The §4.4 program compiles and then dies at class-verification time:

```
java.lang.VerifyError: Bad type on operand stack
  Type 'java/math/BigInteger' is not assignable to 'java/lang/Byte'
```

Diagnosis. Stating `{size(s)}` on `length` makes the channel pin `[5,5]` on the **call node**, so the backend picks
that node's representation as `Byte` (`repInternalNameOf` reads the stamped meta). But `String::length` is emitted
as a *generated native static method* whose return descriptor is the ⊤ `BigInteger`. The backend widens **arguments**
to bignum at every ordinary call boundary (`generateArgumentToBignum`) but has no symmetric re-encode on a call's
**result** edge — because until now no ordinary call node ever carried a narrow meta. `Numeric[Int]`'s `add` is
invisible to this: it is an *inline intrinsic*, so the backend already controls the width it leaves on the stack.

This is precisely the hazard `total-meta-transfers.md` §7 warns about — "get that wrong and it arrives as a
`ClassCastException`, not a type error" — reached from the other side: not a callee's interior, but a leaf whose
stated transfer outruns its emitted descriptor.

Three ways to close it, in order of preference:

1. **Re-encode on the result edge.** After invoking a generated native static method, convert from the method's
   declared return representation to the node's stamped one — the mirror of `generateArgumentToBignum`, using the
   conversion the backend already has. Small, local, and it makes *every* future stated transfer on a static-method
   leaf work.
2. **Stamp nothing at a static-method call node** (only intrinsics, literals and merges narrow). Sound and trivial,
   but it throws away the transfer's whole point.
3. **Emit the native at the narrowed width** — call-site-sensitive codegen for a shared method. Rejected: it splits
   a native per call-site meta, and refinements must stay invisible to monomorphization identity.

Take (1), and land it **with** the first stated `String` transfer, not after. Until a transfer is stated the gap
cannot fire (§4.1 verified a clean sweep), so it does not block the slot — but it does block `length`'s brace, which
is the first thing anyone will write.

---

## 9. Precision reality check — say this in the docs before someone tries it

The channel is intra-procedural (`RefinementChannelProcessor`: a parameter, a value reference, a lambda body are all
⊤). So what actually narrows is: string literals, and directly-called leaves with stated transfers, within one body.

```eliot
def greet(name: String): String = "Hello, " ++ name      // ⊤: `name` is a parameter
def banner: String = "-" ++ "-"                          // [2,2] — literal ++ literal, via combine^Meta
def label(s: String): String = s                         // bodied: derives nothing today, so ⊤ at the call
```

That is the same shape of limitation `Int`'s range already has, and the same fix retires both: the **meta
interpretation** of `total-meta-transfers.md` §6.2/P4, which interprets a callee's body under the caller's argument
metas. `String` size is not a reason to build it, and it is not blocked by its absence — but the first user report
will be "why is my string's size unknown", and the answer should already be written down.

`String::show` being the identity is a good miniature of this: it is *bodied*, so under R3 it may not state a
transfer, and until P4 exists it derives nothing. Correct and useless — exactly the pattern §8.3 of
`total-meta-transfers.md` predicted for `foldLeft`.

---

## 10. Staging

Each stage compiles and passes the example sweep on its own.

- **S1 — the slot.** `type String {size: Interval[BigInteger]}` + the `eliot.compiler.Meta` import in `String.els`,
  plus a doc-comment sentence defining the unit (§5). Inert: no seed, so every node stays ⊤. *Verified.*
- **S2 — the seed.** `Runtime::stringLiteral`, `WellKnownTypes.stringLiteralFQN`, the channel arm, and the LSP
  `boundsOf` gate (§3.3). Test: the §4.3 `where` example, positive and negative, as a new `examples/src` entry plus
  a channel test. *Verified except the LSP gate.*
- **S3 — the backend result-edge re-encode** (§8), landing together with the first stated transfer, `length`'s
  `{size(s)}`. Test: the §4.4 program compiles **and runs**, plus a `javap` check that the narrow conversion is
  emitted.
- **S4 — the remaining `String.els` transfers** (§6), each with its bound argued in the doc comment. Stop at the
  leaves that have an honest bound; leave the unbounded ones unstated and ⊤.
- **S5 — decide ⊤** (open `Interval` endpoints or a stated platform max), then arm R2 (§7). This is where the
  original TODO — *"a native that produces a meta-carrying type must state its meta-information"* — actually closes,
  for both domains at once.

`List`/`Array` size (structural meta) and the meta interpretation (P4) stay out; S1–S4 is a complete, useful feature
without either.

---

## 11. Decisions needed before code

1. **Slot name `size`** (§3.2) — or do the `Qualifier.Meta` accessor-namespace fix first and call it `length`.
2. **Unit** (§5): the host count, a contributed per-platform `sizeOf`, or code points by fiat. This one is a
   language decision, not an implementation detail.
3. **Backend option (1)** for the result edge (§8) — confirm, since (2) is the tempting cheap answer that guts the
   feature.
4. **Scope**: S1+S2 alone (a working `where` domain, no stated transfers) is a defensible landing point and needs
   neither §8 nor the ⊤ question. S3+S4 is where it becomes useful.
5. **Order vs. R2** (§7): confirm the domain lands while accounting stays dormant.

---

## 12. Non-goals

- Structural/functorial meta types — still the `List`/`Array` domain's project.
- The meta interpretation (P4): bodied values keep deriving nothing.
- Any narrowing of `String` *representation* on the JVM (there is none to pick); the layout payoff is an MCU
  backend's, not this change's.
- Changing what `length` counts, unless §5 is decided the third way.
