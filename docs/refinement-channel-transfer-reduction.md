# The Refinement Channel's Transfer Reduction: Why `^Meta` Transfers Must Bottom Out at Natives

Status: **Design record / investigation writeup (2026-07-16).** Captures why a range-transfer `^Meta`
companion body must reduce to platform **natives** and cannot route through an ordinary Eliot `implement`
instance (e.g. `Numeric[Interval[T]]`) — even one placed on the compiler track. The trigger was the proposal to
make the `Int` arithmetic transfers idiomatic:

```
implement Numeric[Int] {
   def add(a: Int, b: Int): Int {a.range + b.range}   -- use the Interval's own Numeric ability
   ...
}
```

instead of today's plain-function form `def nativeAdd(a: Int, b: Int): Int {intervalAdd(range(a), range(b))}`
(`stdlib/eliot/eliot/lang/Int.els`), where `intervalAdd`/`intervalSubtract`/`intervalMultiply` are the compiler-overlay
plain functions in `stdlib/eliot-compiler/eliot/lang/Interval.els`. The idiomatic form **does not work**, and the
reason is structural, not incidental. No code changed as a result of this investigation; this note records *why* the
plain-function indirection is load-bearing, so it is not "cleaned up" into an ability call later.

(The original `docs/bounds-as-refinements.md` design doc was retired in `19448816`; the mechanism now lives entirely
in the code paths referenced below. Source comments still cite the old doc by name.)

## 1. The two stages of a range transfer

The refinement channel (`monomorphize/channel/RefinementChannelProcessor.scala`) is a **post-pass over each
`MonomorphicValue`** — an abstract interpretation that runs *downstream* of type formation, over the checker's already
monomorphized output, and records a per-node value interval into a `RefinementTable`. Its own docstring is explicit
that it runs after the checker "with zero risk to the checker's invariants." It is **not** the checker and it does not
re-monomorphize.

Computing one arithmetic node's result range happens in two stages:

- **Stage 1 — the companion's own monomorphization.** A return brace `def add(…): Int {expr}` desugars
  (`MetaTransferDesugarer`) into a `^Meta` transfer companion `add^Meta(a: Int$Meta, b: Int$Meta): Int$Meta =
  Int$Meta(expr)`. That companion is a compiler-track value; it is monomorphized by
  `CompilerMonomorphicTypeCheckProcessor → TypeStackLoop.process` — **the same unified checker/evaluator that every
  runtime body and every signature twin goes through**, with full ability resolution (`resolveAbilityImpl` + the drain).
  Its output is a *reduced body* stored on the `CompilerMonomorphicValue`.

- **Stage 2 — the channel's transfer force.** When the channel walks an `Int` `+`/`-`/`*` leaf, `metaViaCompanion`
  fetches that reduced body (`ReducedBindingClosure.reduceInstance`, **one-hop**, `deep = false`), applies the two
  concrete operand metas, and reduces:

  ```scala
  val applied = argMetas.foldLeft(reducedBody)(Evaluator.applyValue)
  val forced  = Evaluator.force(applied, MetaStore.empty)   // <-- bare NbE force
  Quoter.quote(0, forced, MetaStore.empty).collect { case s: GroundValue.Structure => s }
  ```

  This is the **same `Evaluator`** as everywhere else (there is only one — the cornerstone holds; this is not a second
  interpreter). But it is invoked as a **bare `force`**: no `resolveAbility`, no drain, no stuck-driven escalation. It
  can unfold a top-level definition and **re-fire a native** when it meets that native's FQN with concrete arguments,
  but it does **not perform ability-instance dispatch** — selecting an `implement` from a receiver type is
  monomorphization's job, and monomorphization is not running here.

## 2. Why the operand-level computation is *forced* into the dispatch-free stage

The companion's parameters `a`, `b` are **abstract** during stage 1 (they are the companion's own binders), so
`range(a)`/`range(b)` are neutral and the interval arithmetic cannot be computed then — with or without dispatch. The
**only** point at which the operands are concrete is stage 2, inside the bare `force`. Therefore:

> The actual per-call range computation is structurally forced to happen in the dispatch-free `force`. Anything on that
> path must therefore reduce **without dispatch** — i.e. it must bottom out at a **native**.

This is the crux. It is not that "the Interval instance is missing from the pool," and it is not fixable by adding the
instance: the one place the computation can run is the one place dispatch does not.

## 3. Why `Numeric[BigInteger]` works but `Numeric[Interval[T]]` cannot

The two abilities are realized differently, and that difference is exactly what stage 2 is sensitive to:

| element | realization | in the stage-2 `force` |
|---|---|---|
| `Numeric[BigInteger]::add` | a **native intrinsic bound at the ability-method FQN** (`StdlibNativesProcessor.numericAddNative = (a,b) => a+b`, keyed on `numericFn("add")`) | fires with concrete `BigInteger` operands — **no dispatch needed** ✓ |
| `Numeric[Interval[T]]::add` | an ordinary **Eliot `implement` instance** | requires *dispatch* to the instance; the bare `force` doesn't do that → stays a stuck neutral → transfer quotes to nothing → ⊤ ✗ |

So `intervalAdd(range(a), range(b))` reduces because it is a plain function whose body bottoms out at
`Numeric::add`/`min`/`max` **on `BigInteger`** — natives at their ability-method FQNs, which fire once the interval
*endpoints* are concrete. `a.range + b.range` puts a `Numeric::add` **on `Interval`** in the path, and there is no
native at that FQN for `Interval`; the endpoints being concrete does not help, because the outer interval `add` never
gets selected.

This is why the compiler overlay's `Meta[Interval[T]]` join is *also* routed through the plain function `intervalJoin`
rather than the instance's own `join`, for the identical reason (the branch-merge path, `docs/generic-refinement-merges.md`).
The overlay comment states the rule — "a transitively-reached Eliot-body ability instance does not dispatch under the
channel's NbE evaluation — only natives re-fire" — and this investigation confirms that comment is literally the
mechanism.

## 4. Two distinct failure stages (evidence)

Written **inline** as `{a.range + b.range}`, the companion fails even earlier than stage 2: its compiler-track
`SaturatedValue` is never produced, so `CompilerMonomorphicValue(add^Meta)` is never even entered (traced: the
compiler-mono entry never fires for the inline form; it does fire for the plain-function form). Wrapping the *same*
ability call in a plain function —

```
def intervalAddViaAbility[T ~ Numeric[T] & Compare[T]](a: Interval[T], b: Interval[T]): Interval[T] = a + b
```

— makes stage 1 succeed (`bodyReduced = true`), but the transfer **still yields ⊤**, because at stage 2 the wrapped
function's body still reaches `Numeric[Interval]::add` with no native to fire and no dispatch. So the plain-function
wrapper only moves the failure from stage 1 to stage 2; it does not rescue it. Adding a full `Numeric[Interval[T]]`
instance to the compiler overlay also did not help — confirming availability was never the blocker.

Discriminating probe used throughout (a `where`-precondition is the only test that actually observes narrowing — the
existing `100 + 100` integration tests assert only the printed *value*, which is `200` whether the node narrows or
falls back to bignum):

```
def useByte(x: Int): Int where withinByte(range(x)) = x
def main: IO[Unit] = printLine(intToString(useByte(add(40, 40))))
```

With the transfer on the impl `add` and the plain `intervalAdd` body, `add(40, 40)` narrows to `Int$Meta(Interval[80,80])`
and the precondition passes; with the ability body it reports "value range is not known."

## 5. Relationship to the signature-unification work

This is **not** a regression of, or a counterexample to, the signature↔runtime unification. That plan unified the
**signature twin** with the **runtime value**: both are computed by the one `TypeStackLoop`/`Checker`/`Evaluator`, and
the `^Meta` companion *bodies* themselves go through that same unified monomorphizer (stage 1 above). That unification
is intact.

The non-unified piece is a **third** subsystem — the bounds-as-refinements *channel* — which the plan never touched. It
is a deliberately lightweight downstream post-pass, and its stage-2 transfer is a bare `Evaluator.force` without an
ability-dispatch context. So: same evaluator, different (stripped-down) invocation — orthogonal to signature/runtime
unification.

## 6. Adjacent findings surfaced by the same investigation

- **Operator-level arithmetic narrowing does not fire at all today** (independent of the idiomatic-transfer question).
  The channel is **intra-procedural**: in a body, `x + y` is a reference to `Numeric::+` (which has no `^Meta`
  companion), while the companion-bearing leaf (`nativeAdd`, or an impl `add`) appears only inside `+`/`add`'s *own*
  body applied to **parameters**, never concrete operands. So `useByte(40 + 40)` is rejected as "range not known" even
  on a clean tree; only a literal or a **direct** `add(40, 40)`/leaf call narrows. The `nativeAdd^Meta` transfer is
  therefore effectively dead for real user arithmetic written with the operators.

- **`ImplementBlock` silently drops the return-meta brace on `implement` methods.** The parser reads a `{…}` return
  brace on an impl method into `FunctionDefinition.returnMeta`, but `ImplementBlock`'s reconstruction of the qualified
  method copies only name/generics/args/type/body — omitting `returnMeta` (it defaults to empty). So a transfer brace on
  an impl method is parsed then discarded and no `^Meta` companion is generated. A one-line fix (`returnMeta =
  f.returnMeta`) makes it flow; it is a latent gap, currently unexercised because no shipped impl method carries a brace.

## 7. The design option (if `a.range + b.range` is wanted)

"Implemented on the compiler track" is necessary but not sufficient; the missing capability is **dispatch at the point
of concrete operands**. To make the idiomatic form work — and delete the `intervalAdd`/`intervalJoin` plain-function
indirection — the channel's stage-2 transfer would need to reduce the `^Meta` companion through a **dispatch-aware**
reduction at the concrete operand metas, rather than a bare `force`. The machinery mostly exists:
`PostDrainQuoter.reduceWithEscalation` already performs dispatch-aware, stuck-driven reduction using `reduceInstance`;
`metaViaCompanion` simply does not route its final operand application through it (it uses `Evaluator.force` with
`deep = false` one-hop bindings). Routing the transfer through the escalation loop — or equivalently, monomorphizing the
companion *at the concrete operand metas* rather than at abstract binders — is the "one evaluator, one path" instinct
applied to the one subsystem the signature-unification plan did not reach.

That is a real, self-contained piece of work with an upside (idiomatic transfers, less compiler-overlay duplication) and
a cost (the channel gains a monomorphizing reduction where today it is a cheap post-pass, and its "zero risk to the
checker's invariants" separation would have to be re-justified). It is recorded here as an option, not a commitment.
