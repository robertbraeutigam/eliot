# Type levels as named values — dissolve the type-stack, simplify `monomorphize`

**Status:** IN PROGRESS. **Step A is landed on master** (commit `614da60a`). The remaining work is a
**simplification of the `monomorphize` phase**, not a feature. Its whole purpose is to make the phase *smaller* and
remove accidental complexity by revealing that higher type levels are ordinary named values.

## The point — read this first

There is exactly **one** goal: **simplify `monomorphize`** by making a value's higher type levels *ordinary,
on-demand, compiler-track monomorphized named values*, so the bespoke type-stack walker dissolves into ordinary
per-value monomorphization demands. **The phase must get smaller — net-delete code — not grow.** A level `n ≥ 1` is
**not special in any way**: it is checked, reduced, and read like every other named value.

**Hard non-goals (this is where the first attempt went wrong — do not repeat it):**

- **Do not extend the compiler / the checker / the kernel to make any particular type-level computation reduce.**
  Whether a value reduces at compile time is a property of *that value and its layer bodies*, never something the
  checker should be grown to force.
- **Do not chase making effectful guards reduce at compile time.** `if..else..raise`, `when`/`orError`, `{Throw}` /
  `{Abort}` signature computations — none of these are a target here. They are ordinary values; if one does not reduce,
  that is a pre-existing reducibility property of the values on its path, out of scope for this plan. (The old
  "effectful returns read a guard verdict" framing has been **deleted** from this plan — it lured an implementer into
  re-implementing reduction machinery in the checker. Do not resurrect it.)
- **Do not re-implement reduction in the kernel.** No sub-value-binding composition, no type-arg-absorbing binding
  wrappers, no deep-`renormalize` read-back, no carrier synthesis on level signatures. §0 explains why: re-implementing
  the body pipeline inside the kernel bottoms out at `Match` and is the anti-pattern the whole "named values" idea
  exists to avoid.

**Guardrail, restated as a stop rule:** every checker-touching change in this plan must **net-delete** lines. If a
change *teaches* `Checker` / `renormalize` / the type-stack walk any new level-specific behaviour, stop — the fix is to
route the expression through the platform as an ordinary value, or not to do it.

## 0. Principle

> The unit of execution on every platform is a **named value**, not an expression. The NbE `Evaluator` is the
> *kernel*, not the platform: an expression runs by becoming (part of) a value's **body**. "Types are values" must
> therefore hold *representationally*, not just semantically — the type of a value is a named value one **TypeLevel**
> up, compiled by the same pipeline as every other value.

Only a named value passes through saturation → the effect phase → check-mode elaboration → ability resolution →
per-instantiation match reduction. A signature slot that is walked *in place* gets none of that — it gets only the bare
β/δ+native kernel, and match reduction in particular **cannot** be added to the kernel (`MatchNativesProcessor.stuck`
mints `VNeutral(Reserved(Match), …)`, dropping the reducer; adding a dispatch rule to `renormalize` would duplicate the
match native — the second-evaluator anti-pattern). So the way to make a higher-level expression behave *exactly* like
runtime code is to make it a **named value** and let the ordinary pipeline run it — not to grow the kernel to reduce it
in place.

## 1. The model

Named-value identity gains a **TypeLevel** dimension, parallel to `Qualifier`:

| TypeLevel | Content            | Today's representation            | Track    |
|-----------|--------------------|-----------------------------------|----------|
| 0         | runtime body       | `NamedValue.runtime`              | runtime  |
| 1         | the signature      | `TypeStack.levels(0)`             | compiler |
| n ≥ 2     | kind levels        | `TypeStack.levels(n-1)`           | compiler |
| top       | literal `Type`     | implicit                          | —        |

A value at level `n` has **body** = the level-`n` expression and **signature** = the level-`n+1` expression. Level `n`
is checked as an ordinary value **against the evaluation of level `n+1`** — the same recursive "check a value's body
against its signature", applied up a finite tower that bottoms at `Type`.

**The payoff (this *is* the simplification).** `TypeStackLoop`'s bespoke `walkTypeStack` is revealed as *not
primitive*: it is the ordinary body-vs-signature check applied recursively. So it dissolves into ordinary per-value
monomorphization demands; the **fact engine** supplies the ordering, caching, and recursion guard that the hand-rolled
walker currently re-implements. The per-level `= Type` kind-unify and its carve-outs stop existing because a level is
just an ordinary value checked against the eval of the level above.

## 2. What's landed — Step A (`614da60a`, `CACHE_VERSION` 23)

The naming at the saturate/monomorphize boundary, with **zero consumer changes** and the full suite green:

| Symbol | File | Role |
|---|---|---|
| `SaturatedValue(value, typeLevel = 0)` + `.Key(vfqn, platform, typeLevel = 0)` | `saturate/fact/SaturatedValue.scala` | level dimension on the saturate-boundary fact |
| `CompilerMonomorphicValue(…, typeLevel = 0)` + `.Key(vfqn, typeArguments, typeLevel = 0)` | `monomorphize/fact/CompilerMonomorphicValue.scala` | level dimension on the compiler-mono fact |
| `TypeLevelSaturatedValueProcessor` | `saturate/processor/TypeLevelSaturatedValueProcessor.scala` | derives `SaturatedValue.Key(v, Compiler, n≥1)` from the host level-0 value |
| `SaturatedValueProcessor` (level-0 guard) | `saturate/processor/SaturatedValueProcessor.scala` | `generateFact` declines `typeLevel != 0` |
| `CompilerMonomorphicTypeCheckProcessor` (rides `key.typeLevel`) | `monomorphize/processor/CompilerMonomorphicTypeCheckProcessor.scala` | maps the level onto the `SaturatedValue.Key` fetch + the produced fact |
| `TypeLevelEquivalenceTest` | `lang/test/…/monomorphize/processor/` | pins level-1-reduced == level-0-signature |

Both processors match `SaturatedValue.Key` by type; the fact engine (`SequentialCompilerProcessors`) runs **both** per
key, and each `abort`s (declines) the levels it does not own — so the level partition needs no new dispatch mechanism.

**The synthetic level-value construction (`TypeLevelSaturatedValueProcessor`).** For a requested level `n`, let
`levelExpr = host.typeStack.levels(n-1)` and `view = SignatureView.of(levelExpr)`:

- **runtime body** = `view.copy(binders = Seq.empty).toExpression` — the type expression with its leading generic
  `FunctionLiteral` binders stripped, so those references are *free* and resolve to the ρ bindings.
- **synthetic signature** = `view.withParameters(Seq.empty).withReturnType(Type).toExpression` — the generic binders
  wrapping `Type`, so `applyTypeArgs` peels each binder and the body checks against kind `Type`.
- **upper levels** = `host.typeStack.levels.drop(n)`; `n` beyond the stack top ⟹ the literal `Type`.

This value is **an ordinary compiler-track value with a body and a signature** — that is the entire point. It carries
no guard-specific carrier, no synthesised effect channel: keep it that way.

**Known Step-A finding the dissolution must resolve (not a guard issue — a general one).** A level **body** is checked
through the *value* path (`Checker.infer` → Γ), whereas today's **signature** walk evaluates through the *type* path
(`walkTypeStack` → `evalExpr` → ρ). They resolve a generic-parameter reference differently: in a level body,
`applyTypeArgs` binds a *type* argument's Γ slot to the instantiated **value** (`Γ(X) = A`), not its **kind** (`Type`),
so `def genArrow[X]: Function[X, X]` — whose level-1 body is `Function[X, X]` — reports a spurious `Type mismatch` at
`X` (pinned by `TypeLevelEquivalenceTest.genArrow` as a *finding*, not a silent accept). The dissolution's job is
precisely to make level checking *one* path, so this divergence disappears: a level is checked as an ordinary value and
a type-parameter reference resolves consistently. This is the substance of the work — not any effect/guard concern.

**Test-harness recipe (compiler-pool level tests).** A leaf `ProcessorTest(LangProcessors(systemModules = Seq.empty)*)`
demanding a `CompilerMonomorphicValue` at a non-trivial signature must **declare `Type` and `Function` in the compiler
pool** (`type Type` at `eliot.compiler.Type`; `type Function[A, B]` + `def apply` at `eliot.lang.Function`) and the test
module must `import eliot.lang.Function`. To assert equivalence, read a pure type-denoting `MonomorphicExpression` back
to a `GroundValue` (`TypeLevelEquivalenceTest.denote`) and compare to the host's level-0 `.signature`.

## 3. The simplification (the actual next work)

Make a value's signature come from its **level-1 monomorphized value**, deleting the in-place stack walk:

- In `TypeStackLoop.processIO`, replace `walkTypeStack(rv.typeStack)` (the only call site) with an ordinary demand for
  the level-1 value's reduced result — `CompilerMonomorphicValue((v, 1), typeArgs)` — and check `v`'s body against it.
  Level-1's own monomorphization recursively demands level 2, and so on; the tower is finite and bottoms at `Type`, and
  `activeFactKeys` already guards the back-edge.
- With that in place, the following **delete**: `walkTypeStack`; `flattenReturnToType`; the per-level `= Type`
  kind-unify and every carve-out that exists only to keep that unify happy. `TypeStackLoop` shrinks to a thin driver
  over "check level 0's body against the level-1 read", or dissolves into `Checker` outright.
- **Measure the net line count.** If this step does not remove more than it adds from the check ladder, the approach is
  wrong — reconsider before committing.

Sequence it behind the Step-A machinery so the guard pressure the old plan carried is gone; it can pause indefinitely
without leaving anything half-migrated. Give it a regression pass over the implicit-generics (W3/W4) and any ability
suites before landing, since the check ladder is the most delicate code in the compiler.

**Accidental complexity to re-examine as the walk dissolves** — remove it only if the dissolution genuinely makes it
unnecessary, never by special-casing: the `= Type` kind-unify carve-outs live in the same code as the effectful-return
special-casing (`sawGuardReturn` / kind-position guard-carrier acceptance and the like). If the ordinary level check
subsumes them, they go with the walker; if it does not, **leave them alone** — do not build new machinery to preserve or
extend them. The measure of success is *fewer* lines and *fewer* special cases in `monomorphize`, full stop.

## 4. Follow-on (optional): remove the `TypeStack` structure itself

The `TypeStack` *data structure* is pervasive (~35 files carry it through core → operator → saturate). Removing it —
moving `TypeLevel` into core `NamedValue` identity and dropping the carriage so the Step-A derivation moves from the
saturate boundary into core desugaring — is **mechanical once nothing walks it**, because §3 removes the only *reader*
of the stack's structure. Do it only if the carriage is causing friction; the semantic simplification (§3) is what buys
the win, and this is just tidying the plumbing afterward.

## 5. Guardrails (the stop rules)

- **Net-delete.** Every checker-touching change nets out to fewer lines and fewer special cases in `monomorphize`.
- **Routing, not capability.** No new level-specific behaviour in `Checker` / `renormalize` / the walk. Route through the
  platform as an ordinary value, or don't.
- **No kernel reduction re-implementation.** No sub-value binding composition, no read-back renormalize tricks, no
  carrier synthesis on level signatures. The named-value pipeline reduces; the kernel stays the kernel.
- **Higher levels are not special.** A level `n ≥ 1` value is checked, reduced, and read exactly like a level-0 value.
  If a task starts treating it specially, that task has drifted from the goal.

## Appendix — reference branches

`wip/return-position-unification-stage2` (`77c2ed43`), `wip/if-else-guard-idiom`, `failed/if-else-guard` are
reference-only historical attempts at the (now-abandoned) guard-reduction direction; do not mine them for machinery.
