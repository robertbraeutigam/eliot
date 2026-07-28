# Effects as a Channel, v2: Uniform Carriers — RETIRED

> **This design is retired.** It was superseded in *direction* on 2026-07-25 and superseded in *code* on
> 2026-07-28, when the last of its machinery (the uniform-carrier bridge, the carrier join, the obligation
> path and the carrier side table) was deleted. The live design is **`docs/effects-as-rows.md`**.
>
> The full 2,002-line v2 document is in git — `git show c9dcd53b:docs/effects-as-channel.md` reads the last
> version before this file was reduced. Nothing below restates its argument; this is a signpost, so that the
> citations left in source comments still land somewhere true.

## If you arrived from a `docs/effects-as-channel.md §N` citation

Many scaladoc comments cite this document by section. Those citations are **historical pointers**: they say
"this code came out of v2 §N", not "v2 §N is the current rule". Read the live rule at the successor below.

| v2 § | What it covered | Live successor |
| --- | --- | --- |
| §0–§2 | the carrier model, and why carrierhood was made structural rather than recognitional | `effects-as-rows.md` §0 (why v2 was revisited) and §1 rule 4 |
| §3 | the user model + the checker-side elaboration realizing it | §1 (the four user rules), §3 (elaboration is a **desugar**, in `row/RowElaborator`) |
| §4 | the channel: rows, positions, the reify boundary; `EffectRow` metadata | §2 (two channels beside each other); `ast/fact/EffectRow` survives and now feeds the desugar instead of the checker |
| §5 | row accounting and verification per mono key | §2 "two verifiers, one vocabulary" — `channel/EffectAccountingProcessor` **survives unchanged** as the post-mono fail-safe, joined by the pre-mono `RowElaborationProcessor.verifyRow` |
| §6 | the `Id`-normalization stage | §1 rule 4 (third bullet) and §4 — the *erasure* survives (`channel/IdNormalizer`, `assertNoIdResidue`); the *encoding* that made it necessary (manufacturing a carrier head on pure judgments) is gone, and `Id` is now **written** by the elaborator, honestly typed |
| §7 | what remains to delete, what stays | §4 and A.11.0 — the deletion is complete |
| §8 | the compile-time residue | §5 ("compile-time track") and §8: the compile track keeps its mid-spine default ladder and deferred slots **by design** |
| §9 | held invariants, incl. user-facing text staying in payload/row vocabulary | §7 and the *Effects Are a Channel* cornerstone in `.claude/CLAUDE.md`; the rendering invariant is unchanged (`effect/EffectRowRendering`, one inverter) |
| §10 | the U1–U4 migration record | history only; the v3 record is `effects-as-rows.md` A.8–A.11 |
| §11–§13 | risks, open questions, and the carrier-model fork decision | superseded by `effects-as-rows.md` §0 and A.10 (the stock-take that reversed the fork) |

## What v2 got right, and what was kept

- **Verification is a separate channel, derived from ground instantiations.** `EffectAccountingProcessor`
  (`derived ⊆ declared` per mono key, ride-tested against `MonomorphicValue.ambientCarriers`, wired as a
  codegen precondition) is v2's, and is still the fail-safe. *Forward what is declared, derive what is done.*
- **Rows never flow back into types**, and no negative-effect surface exists (there is no `-E`).
- **Carrier-ness is recognized by a tag threaded from the desugar** — never by name, shape, or "has an
  `Effect` instance". That was v2's finding 14, and it still binds.
- **`Id` is ordinary `data` with no `Suspend[Id]`**, so real I/O can never run on a pure computation.
- **User-facing text stays in payload/row vocabulary**, through one inverter.

## What v2 got wrong, and why it was reversed

v2 left the carrier a **type argument the checker solves**. Everything expensive followed from that: carrier
metas needed a join instead of unification (`CarrierJoin`); flex-flex `?F[X] ~ ?G[Y]` stranded the pattern
unifier; pure judgments had to be wrapped in `Id` so every slot arm could split unconditionally; and the
bind/`pure` decision became undecidable from declarations, which grew mode obligations, a post-drain resolver
and a splice-and-restart loop. The checker's effect machinery grew by ~1,300 lines before the premise itself
was revisited (`effects-as-rows.md` A.10).

The fix was the one step v2 never took: **the elaborator writes the carrier**. Every carrier position is then
rigid, no metavariable is ever created for it, and the apparatus above becomes unnecessary rather than
improved. `check/` finished at 3,873 lines — below the pre-v2 baseline of 3,996.
