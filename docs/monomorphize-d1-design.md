# D1 — Name the post-check resolution pipeline

Status: **Design + implemented.** This is deliverable **D1** of
[`monomorphize-architecture-review.md`](monomorphize-architecture-review.md): make the implicit
post-check pass sequence in `TypeStackLoop.processIO` an explicit, named, ordered structure that
terminates in a checked postcondition. **No semantic change** — the suite stays green by
construction; the value is auditability and a seam for D2.

## What it replaces

The tail of `TypeStackLoop.processIO` was a flat `for`-comprehension whose ordering constraints
lived only in prose comments ("report it as a hard error here, before `defaultUnsolvedMetas` masks
it"):

```
drainAndResolveLoop → resolveUpperBounds → verifyCarrierKinds
→ failOnUndeterminedCalculatedReturn → defaultUnsolvedMetas → report → quote
```

The review calls this "an implicit pipeline of ~8 ordered passes over a shared blackboard." D1 names
the passes and the ordering.

## The structural finding: it is tiered, not flat

The honest shape is **not** a flat ordered list (an earlier sketch proposed `data PostDrainPass {
name; runsAfter; run }` with a topo-sort — that turned out too weak). Reading the code, the steps
fall into **three tiers plus a postcondition**, and `drain` is **not** one of the passes:

```
TIER 1  Saturation   fixed-point loop, drain-interleaved   { resolve-abilities, resolve-combines }
TIER 2  Finalization linear, once, after the fixed point    { upper-bounds, carrier-kinds, calc-return }
TIER 3  Finalizer    the step nothing runs after            { default-metas }   ← D2 swaps this body
        ───────────  postcondition                          every meta solved-or-abstract (asserted)
```

Three observations justify the tiers — these are the validation result of the "does the plug-in
taxonomy hold up?" probe:

1. **`drain` is the equality core settling, not a feature.** The unifier's `drain()` is the
   cornerstone's pure-definitional-equality engine reaching a fixed point. The passes
   (abilities, combines, bounds, kinds, calc-return) are the *non-equality features bolted around
   it*. The runner therefore interleaves `drain` **between** feature passes rather than modelling it
   as a pass — keeping "core vs. features" visible in the structure. Folding `drain` into the pass
   list would be a category error.

2. **The hard ordering is a *phase boundary*, not a pairwise edge.** `resolveUpperBounds` may
   *assume* every `Combine` meta already holds its final join. That is "saturation has reached its
   fixed point," which no per-pass `runsAfter: ["resolve-combines"]` can express. So a plug-in
   registry for these passes must be **two-level (phases first, ordering second)**, not a single
   topo-sorted list. This is the concrete finding for the broader hook-architecture question.

3. **`runsAfter` within a tier is currently near-vacuous, so it is omitted.** Inside Finalization
   the only real edge is "everything before the finalizer," and the three Finalization passes are
   mutually order-independent. Declaring per-pass `runsAfter` would be speculative generality (one
   trivial edge) that oversells the plug-in-ness before a second pass-contributor exists. D1 uses
   **positional order within a tier**. When a real second contributor appears (the D6 / keying-B1
   plug-in story), `runsAfter` + a topo-sort is the additive step — not before.

## The shapes

Realized as a sealed hierarchy (nested in `object TypeStackLoop`, companion-private — internal to
the checker):

```scala
/** One named step of the post-check resolution pipeline. The pipeline is tiered, not flat (see
  * above): `drain` is interleaved by the runner as the equality core settling; the finalizer and
  * the postcondition assertion are fixed structural steps, not entries in this list. */
private sealed trait PostDrainPass { def name: String }

private object PostDrainPass {
  /** Solves metas iteratively; `run` returns whether it progressed this round. Re-run in the
    * saturation fixed-point loop (drain-interleaved) until no Saturation pass progresses. */
  case class Saturation(name: String, run: PassContext => CheckIO[Boolean]) extends PostDrainPass

  /** Runs once, after saturation reaches its fixed point. Order within the tier is registration
    * order (no `runsAfter` — see the finding above). */
  case class Finalization(name: String, run: PassContext => CheckIO[Unit]) extends PostDrainPass
}

/** Everything a pass closes over from the current check — the slot's closed input interface. That
  * every pass needs *only* this (nothing from `processIO`'s locals) is the evidence the slot
  * boundary is real: a pass reaching past `PassContext` could not be lifted out. */
private case class PassContext(
    resolvedValue: OperatorResolvedValue,
    abilityRefs: Seq[AbilityRef],
    paramConstraints: Map[String, Seq[OperatorResolvedValue.ResolvedAbilityConstraint]],
    returnMeta: Option[SemValue.VMeta]
)
```

## The registration — the existing five passes, declared

```scala
private val pipeline: List[PostDrainPass] = List(
  PostDrainPass.Saturation("resolve-abilities", ctx => resolveAbilities(ctx.abilityRefs, ctx.paramConstraints)),
  PostDrainPass.Saturation("resolve-combines", _ => checker.resolveCombines),
  PostDrainPass.Finalization("upper-bounds", _ => checker.resolveUpperBounds),
  PostDrainPass.Finalization("carrier-kinds", _ => checker.verifyCarrierKinds),
  PostDrainPass.Finalization(
    "calc-return",
    ctx => ctx.returnMeta.traverse_(failOnUndeterminedCalculatedReturn(_, ctx.resolvedValue))
  )
)
```

The finalizer (`defaultUnsolvedMetas`) and the postcondition are **not** in the list — they are
fixed steps of the runner, which is exactly what makes "what D2 changes" crisp: D2 swaps the
finalizer body for a role-total match, and the runner and assertion are untouched.

## The runner

```scala
private def runPostDrainPipeline(ctx: PassContext): CheckIO[Unit] =
  val saturation   = pipeline.collect { case s: PostDrainPass.Saturation => s }
  val finalization = pipeline.collect { case f: PostDrainPass.Finalization => f }
  for {
    _ <- saturateToFixedPoint(saturation, ctx)   // TIER 1
    _ <- finalization.traverse_(_.run(ctx))      // TIER 2
    _ <- defaultUnsolvedMetas                     // TIER 3 (finalizer; D2 seam)
    _ <- assertEveryMetaResolvedOrAbstract(ctx)   // postcondition
  } yield ()
```

`saturateToFixedPoint` reproduces the old `drainAndResolveLoop` round exactly — `drain` before each
saturation pass, loop while any progressed:

```scala
private def saturateToFixedPoint(passes: List[PostDrainPass.Saturation], ctx: PassContext): CheckIO[Unit] =
  def round: CheckIO[Boolean] =
    passes.foldLeftM(false) { (acc, p) =>
      for {
        _          <- modify(s => s.withUnifier(s.unifier.drain()))   // ← equality core settles between features
        progressed <- p.run(ctx)
      } yield acc || progressed
    }
  def loop: CheckIO[Unit] = round.flatMap(if (_) loop else pure(()))
  loop
```

## The postcondition (D1's terminal assertion; D2's safety net)

```scala
private def assertEveryMetaResolvedOrAbstract(ctx: PassContext): CheckIO[Unit] =
  inspect { s =>
    val protectedIds = s.abstractTypeMetas.values.map(_.value).toSet
    s.unifier.metaStore.entries.collect { case (rawId, None) if !protectedIds.contains(rawId) => rawId }.toList
  }.flatMap {
    case Nil      => pure(())
    case unsolved => liftF(compilerAbort(ctx.resolvedValue.name.as(
                       s"Internal: post-drain pipeline left metavariables unresolved: $unsolved.")))
  }
```

Today `default-metas` makes this true **by construction** (it solves every non-protected meta to
`Type`), so the assertion never fires. Its purpose is forward-looking: it is a *compiler-bug*
backstop, not a user diagnostic, so it `compilerAbort`s rather than reporting a type error. It turns
D2's central promise — "an unhandled meta role cannot silently become `Type`" — from a hope into an
enforced invariant: when D2 replaces the finalizer with a per-role match, any role the match forgets
leaves an unsolved meta, and this assertion catches it loudly at the definition instead of shipping
a mistyped value. This is the structural cure for F2 (`defaultUnsolvedMetas` masking obligations as
`Type`) being *prepared* by D1 and *delivered* by D2.

## Why this is a no-op semantically

- `saturateToFixedPoint` issues `drain → resolve-abilities → drain → resolve-combines` per round and
  loops on `abilityProgressed || combineProgressed` — identical to the old `drainAndResolveLoop`.
- The three Finalization passes run in the same order as before (`upper-bounds`, `carrier-kinds`,
  `calc-return`).
- `defaultUnsolvedMetas` runs in the same position (after Finalization, before error reporting).
- The assertion is unreachable today (post-`default`, every meta is solved or protected), so it
  changes no accepted/rejected program.
- Error reporting + abort + quoting below the pipeline are unchanged.

## Validation

- Full suite green (no behavioural test — pure refactor; relies on the suite, per the review's D-list
  validation note).
- Adversarial check that the postcondition is wired live (not dead code): temporarily neutering
  `defaultUnsolvedMetas` must make the assertion fire on any program with an unconstrained phantom
  meta — confirms the backstop is reachable. (Run manually during review; not committed.)

## What this sets up

- **D2** plugs into exactly one place — the finalizer step — replacing `defaultUnsolvedMetas` with a
  `Map[MetaId, MetaRole]` total match. The runner, the tiers, the `PassContext`, and the assertion
  are reused unchanged.
- The broader **hook taxonomy** question (can features be exported as closed plug-ins?) gets its
  first concrete data point: yes for the *mechanism* (named passes, closed `PassContext` input), but
  the registry must be **tiered (phase + order)**, and the features stay coupled through the shared
  metastore — D1 makes that coupling explicit rather than dissolving it.
