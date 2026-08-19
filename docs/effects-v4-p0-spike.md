# Effects v4, P0: does the `WovenValue` seam know the carrier?

**Status (2026-08-19): DONE, and this is the gate note `docs/effects-as-channel-v4.md` §11 P0 asks for.**
**Verdict: yes, for all three shapes** — S1 unconditionally, S2 with one amendment to what the seam is *keyed
by*, S3 conditional on one rule v4 must state before P1 fixes the canonical form. R1 ("the seam may not be late
enough") is **cleared**: the risk it names does not materialise — nothing at the seam is still waiting to be
solved. Per v4 standing rule 5
the two things the measurement turned up that the design does not yet say are surfaced here (R7, R8) rather than
routed around; they are amendments, not blockers, and P0's own gate ("if any shape says no, stop") is not tripped.

This note decides nothing about whether v4 is taken. It reports what was measured.

## 1. What P0 asked

> **R1 — the seam may not be late enough.** The lowering needs the ambient carrier of *every* definition,
> including one reached only through a stored computation. Ground types should settle it; this is the first thing
> to spike, because if it does not, v4's central claim fails.

Sharpened before measuring, because "is the carrier known at the seam" is the wrong question — today it is known
trivially, since it is *in the type*. The question v4 actually poses is:

> With the carrier removed from every type, is the carrier of each definition still a function of what the seam
> **is keyed by**, plus what it may **demand**?

Under v4 a monomorphic key holds only *payload* type arguments, because a carrier never appears in a type. So the
test is whether the **payload key** determines the carrier, and where the carrier comes from when it does not.

## 2. What was measured, and how

`jvm/test/…/EffectsV4SeamGroundnessTest.scala` compiles three programs over the real `lang`/`stdlib`/`jvm` layers
and reads the fact map at the seam. Per monomorphic instance it computes two things:

- its **stack** — `MonomorphicValue.ambientCarriers`, the value's own ambient carriers as full ground values;
- its **payload key** — the instance's mono type arguments *minus* the ones that are its own carrier: exactly the
  key a v4 mono would be left with.

Eleven assertions, all passing, pin every number quoted below; the suite is permanent, so a later change that
would have invalidated this note fails instead.

## 3. Shape by shape

### S1 — a `{Console}` block: **yes**

```eliot
def echo: {Console} Unit = {
   printLine("a")
   printLine("b")
}

def main: {Console} Unit = echo
```

| instance | mono type arguments | payload key | stack |
| --- | --- | --- | --- |
| `Test::echo` | `[IO]` | `[]` | `{IO}` |
| `Console::printLine^Console#F` | `[IO]` | `[]` | `{IO}` |

`echo` names no carrier, so its mono key today is *nothing but* the carrier, and its payload key is empty: one
definition, one instance, one stack. The stack is the ambient of the region that calls it, which the platform run
boundary fixes at `main` (`RunBoundaryFunctions` / `SyntheticMainSourceProcessor`) and which the lowering carries
down its own demand. Ground, available, unambiguous.

### S2 — a `catch` discharge: **yes, and the seam's key must carry the stack**

```eliot
def emit(s: String): {Console} Unit = printLine(s)

def parseBad: {Throw[String], Console} String = {
   emit("parsing")
   raise("malformed input")
}

def main: {Console} Unit = {
   emit("start")
   emit(parseBad catch (err -> err))
}
```

| instance | mono type arguments | payload key | stack |
| --- | --- | --- | --- |
| `Test::emit` | `[IO]` | `[]` | `{IO}` |
| `Test::emit` | `[{Throw[String] \| IO}]` | `[]` | `{{Throw[String] \| IO}}` |
| `Console::printLine^Console#F` | `[IO]` | `[]` | `{IO}` |
| `Console::printLine^Console#F` | `[{Throw[String] \| IO}]` | `[]` | `{{Throw[String] \| IO}}` |
| `Throw::catch` | `[String, IO, String]` | `[String, String]` | `{IO}` |

This is the load-bearing measurement of the whole spike. **One definition, one payload key, two stacks, two
genuinely different weaves** — `emit` called from `main`'s `IO` region and from `parseBad`'s discharge stack is
not one instance but two, and their bodies differ (`printLine` at `IO` vs at `ThrowCarrier[String, IO]`). A
payload-only seam key would collapse them.

So: the carrier *is* ground at the seam, but it is **not a function of the payload key**. The seam must be keyed
by `(vfqn, payload arguments, carrier stack)` and the lowering must be a demand from the run boundary downwards,
each call demanding its callee at the stack that call runs on. This is not a new invention and not a new cost —
`WovenValue`'s own scaladoc already reserves it (*"weave key = mono key × stack; the stack dimension is added when
control-effect carriers arrive"*), and today's mono key already carries the stack *inside* its type arguments, so
the amended key reproduces exactly today's instance set. It does mean §6 must say it, because as written §6 says
the seam is keyed as it is today.

The discharge derivation itself survives untouched: `catch`'s ground signature
(`{Throw[String] | IO} String -> (String -> IO[String]) -> IO[String]`) is precisely what v3's rule
`carrier(call) = stack(callee.declaredRow ∖ ambient.declaredRow) over ambient` computes — the same rule, run on
ground inputs instead of declarations.

### S3 — a stored computation: **yes, conditional on a canonical row ⤳ stack rule**

v3 cannot write v4's `List[TestCase]` of unpinned computations at all (a list element is a payload, and a payload
may never be a computation — `examples/src/TestSuite.els`), so the spike measured the nearest expressible form:
the row pinned into a `data` field, and the `data` in a `List`.

```eliot
data TestCase(name: String, body: {Throw[String] | Id} Unit)
def assertEquals(expected: String, actual: String): {Throw[String]} Unit = …
def cases: List[TestCase] = append(append(empty, TestCase("ok", …)), TestCase("bad", …))
def main: {Console} Unit = cases.foreach(runCase)
```

| instance | mono type arguments | stack |
| --- | --- | --- |
| `Test::assertEquals` | `[{Throw[String] \| Id}]` | `{{Throw[String] \| Id}}` |
| `Test::body` (the field accessor) | `[]` | `{{Throw[String] \| Id}}` |
| `Test::cases` | `[]` | `{}` |
| `Test::runCase` | `[IO]` | `{IO}` |

`assertEquals` is reached **only** through the stored field — R1's named worst case — and its stack is ground at
the seam. So the answer is yes. But the *source* of that stack is the one thing v4 deletes: the accessor `body`
has no type argument of its own, and takes the stack from the **field's type**, where the author wrote the pin.
A v4 field type is `Computation[{Throw[String]}, Unit]` — a row with no base — so the lowering cannot read a
stack there; it must **compute one from the row**.

That rule is available and forced, and it is a function of the type, which is what a stored computation needs
(producer and consumer share nothing else):

> **the canonical stack of a row** is its canonical (sorted, deduplicated) ability order lowered to carriers, over
> the pure base when the row rides no `Suspend`, and over the platform's run carrier when it does.

Both bases are ground at the seam: there is one platform per compilation and `RunBoundaryFunctions` names its
carrier. This extends §4's canonicalisation obligation from the row to the stack the row lowers to — same
obligation, same reason ("one spelling"), one step further. With it, S3 is a yes; without it, S3 is the shape that
has no answer, so it must be stated in P1 and not discovered in P2.

Two consequences follow, both recorded as new risks below: a stored computation whose consumer runs it on a
*different* ambient needs a **hoist** v3 never needs (R7), and canonicalising a row **orders** a stored
computation's transformer stack, fixing a semantics today's author picks by writing the pin's order (R8).
Neither arises in this shape — S3's consumer discharges at the canonical base and lifts only the pure result —
and neither changes S3's verdict; both are v4 obligations that must land in the design before P2.

## 4. What the measurement settles beyond the three shapes

- **Nothing at the seam is unsolved.** No instance in any of the three programs carries a `GroundValue.Param`
  anywhere in its carriers. R1's literal worry — a carrier still waiting on something at the seam — is not real.
- **Ability selection at the seam is a lookup, per stack.** `printLine^Console#F` exists at both `IO` and
  `{Throw[String] | IO}` (S2), each keyed by its ground stack. §6's claim that relocating effect-method selection
  out of the checker is "a lookup, not a search" holds — but it is a lookup *per stack*, which is the same
  amendment S2 forces on the key. R2 is unchanged as a sizing question: relocation is still real work.
- **The declared row is reachable at the seam.** It rides `ResolvedValue`/`OperatorResolvedValue`, keyed by
  `(vfqn, platform)` — a sideways read on the value, not on the mono key. `WovenValueProcessor` performs no such
  read today; the lowering must, and may.
- **The stack-keyed demand terminates, with a finite instance set.** By the *Total by Default* cornerstone the
  value-reference graph is acyclic, and each call's stack is the discharge stack of a finite row difference over
  its caller's, so the demand can neither cycle nor grow without bound. This is not a new argument: it is exactly
  why today's carrier-in-the-key monomorphization terminates, inherited verbatim.

## 5. What this spike does **not** establish

- It measured **availability of information**, not that a lowering *writes the same bodies*. That is P2's gate and
  is unchanged: identical woven output on every example, now compared per `(payload key × stack)`.
- Nothing here touches re-check cost (R3), the flag day (R6), or diagnostics (R5).
- Three shapes are three shapes. S3 in particular had to be measured in its v3-expressible form; the v4-only form
  is precisely what R7/R8 are about.
- One platform, one run boundary. A program with two run carriers — none exists today — would test "the base is
  fixed by the row" harder than this could.

## 6. Amendments this makes to `docs/effects-as-channel-v4.md`

Applied in the same change as this note:

1. **§6** — the seam is keyed `(vfqn, payload arguments, carrier stack)`, the lowering is a demand from the run
   boundary down, and it performs a sideways read of the callee's declared row.
2. **§6/§4** — the canonical row ⤳ stack rule for stored computations, and the extension of the canonicalisation
   obligation to the stack.
3. **§10** — R1 marked cleared by measurement, with its two amendments; **R7** (the hoist a stored computation
   needs and v3 avoids by re-monomorphizing) and **R8** (canonicalisation fixes stored stack order) added.
4. **§11** — P0 marked done with this note as its gate; P1 gains the stack canonical form; P2's comparison is per
   `(payload key × stack)`.

## 7. Reproduce

```sh
./mill jvm.test.testOnly com.vanillasource.eliot.eliotc.jvm.EffectsV4SeamGroundnessTest
```

The three programs are the `object EffectsV4SeamGroundnessTest`'s literals; the S3 one also compiles and runs as
an ordinary program (it prints `ok: PASS` / `bad: FAIL expected 'a' but was 'b'`), which is how it was first
checked:

```sh
./mill examples.run jvm exe-jar <dir-holding-a-Test.els-with-the-S3-program> -m Test && java -jar target/Test.jar
```
