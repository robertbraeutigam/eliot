# Optimization Thread: In-Place Mutation via Inferred Uniqueness

Status: **Design record / discussion writeup (2026-07-13).** Captures a design conversation triggered by the
question: when `List` arrives and its *size* is tracked as meta-information in the refinement channel, can the
compiler *also* infer when a list may be backed by a **mutable** representation — or when a builder chain like
`emptyList().add(x).add(y)` can be collapsed entirely? The purpose is to name the analyses precisely, record
*why Eliot is unusually well-positioned to infer this without Rust-style annotations*, and mark where the work
would sit relative to the refinement channel and Reduce-and-Reify. No code yet — this is the "what is this and
why does it fit" note that precedes a build.

This is a **transparent, semantics-preserving optimization**, not a language feature. It can never change what
a program computes, only how the intermediate structures are laid out and allocated. That framing is
load-bearing: it means the thing needs no user-visible soundness story, only a correct internal analysis — so
it is low-risk and high-alignment with the cornerstones (see §5).

## 0. Thesis

`emptyList().add(item1).add(item2)` allocates two throwaway intermediate lists on the way to a two-element
result. In a pure language those intermediates are *observably* immutable, but if the compiler can prove they
are never referred to again after being fed to the next `add`, then nothing observable distinguishes "copy into
a fresh list" from "mutate the previous list in place." The claim of this thread:

> Eliot can compute that proof **statically and exactly** — without reference counting and without linear type
> annotations — because purity + no-recursion + whole-program monomorphization together make the value-flow
> graph a finite, fully-visible DAG with no aliasing-through-mutation and no unknown call sites.

That is the same bargain the **Use-Site Verification** cornerstone already strikes for correctness (give up the
modular per-definition certificate, verify the actual monomorphized program) applied to *memory*. Where Koka,
Roc, and Swift do opportunistic in-place update via a *runtime* reference-count check ("is the refcount 1 right
now?"), Eliot's design lets it answer "is this cell dead here?" at compile time for most cases.

The optimization degrades gracefully through three tiers, cheapest/safest first:

| what is statically known | what the compiler does | mechanism |
|---|---|---|
| the intermediate is **dead** at the next op | pick a **mutable** backing; mutate through the chain | last-use / uniqueness analysis (§2) |
| the **size** of the result | preallocate the exact final structure; skip growth | size from the refinement channel + fusion (§3) |
| the **contents** (compile-time-known) | collapse the whole chain to a literal | CTFE = Reduce-and-Reify (§4) |

Each tier subsumes the ones below it as its preconditions get stronger, and each falls back to a plain
functional copy when its precondition fails.

---

## 1. What the analysis is called

The question "how is this analysis called" has four related-but-distinct answers, and it is worth keeping them
apart because they are different tools solving different halves of the problem.

**1a. Linearity / affinity / uniqueness types** — the *type-system* framing (make it visible to the user).
- **Linear** = used *exactly* once. **Affine** = used *at most* once. Rust ownership is affine + **borrowing**;
  its borrow checker is a *modular, separately-compilable* proof of affinity, proven without seeing call sites.
- **Uniqueness types** (Clean) are the near-dual: a guarantee that *right now* this is the only live reference.
- The reason a language reaches for the *type-level* version is to **guarantee** in-place update in the API and
  to enforce **resource protocols** (a handle that must be used once and closed). That guarantee is the point,
  not the speed — see §6, which argues this is a *separate, later* feature for Eliot.

**1b. Liveness / last-use / uniqueness *analysis*** — the *compiler-inference* framing (no annotations). "Is
`L1` dead the instant `add` consumes it?" If a value is used at most once and its last use is as the argument
to the operation in question, that operation may mutate its argument in place. This is what Eliot should do
first, because it needs no surface syntax at all (§2).

**1c. Reuse analysis / FBIP ("functional but in place")** — the *optimization itself*: when a constructor cell
dies and one of the same shape is allocated nearby, reuse the memory instead of freeing + allocating. This is
**Perceus** (Koka; Reinking, Xie, de Moura, Leijen — "Perceus: Garbage Free Reference Counting with Reuse"),
the opportunistic mutation in **Roc**, and **Swift**'s copy-on-write arrays (`isKnownUniquelyReferenced` → COW).
All three decide it *dynamically* via a reference count; Eliot aims to decide it *statically* (§5).

**1d. Deforestation / fusion / escape analysis / CTFE** — for the "collapse the whole thing" half. Eliminating
the intermediate lists entirely is **deforestation** (Wadler) / build–foldr **fusion** (Gill–Launchbury–Peyton
Jones, "A Short Cut to Deforestation") / **stream fusion** (Coutts–Leshchinsky–Stewart). "We never refer to the
interim lists" is precisely **escape / liveness analysis** (the JVM does a runtime cousin as escape analysis for
scalar replacement). And if the elements are compile-time-known, full collapse to a literal is just **CTFE /
partial evaluation** — which in Eliot is the *existing* Reduce-and-Reify NbE pass, no new machinery (§4).

---

## 2. Tier 1 — last-use ⇒ in-place (the core, annotation-free)

Walking `emptyList().add(item1).add(item2)` through the analysis:

- `emptyList()` produces list `L0`.
- `L0.add(item1)` produces `L1`; `L0` is never mentioned again → **dead** after this call.
- `L1.add(item2)` produces `L2`; `L1` is never mentioned again → **dead** after this call.

Because each intermediate is used **at most once** and its last use is as the receiver of the next `add`, the
compiler may select a **mutable** backing structure and have each `add` mutate-and-return rather than copy. The
result is *observably identical* to the immutable version — same value, same purity, same everything the user
can see — so no soundness obligation is exposed to the user. It is a pure representation choice.

The **only** thing that blocks it is **sharing**:

```eliot
let l = emptyList().add(x)      -- l is used TWICE below
pair(l.add(y), l.add(z))        -- both add's need l intact ⇒ at least one must copy
```

Here `l` has two consumers, so it is *not* linearly used; `l.add(y)` and `l.add(z)` cannot both mutate the same
cells. The analysis is exactly: **is this value's use count ≤ 1 along every path, and is the op in question its
last use?** In a normal compiler that question requires conservative aliasing approximation. In Eliot it is
*exact* — see §5.

Idiomatically this matters a lot because Eliot's **subject-last dot-chain** style (`.claude/skills/eliot-code`)
makes builder chains the *common* shape, not the exception: `xs.map(f).filter(p).add(z)` is a stack of
single-use intermediates begging for this treatment.

---

## 3. Tier 2 — size from the refinement channel ⇒ exact preallocation

Even when the elements are runtime values (so Tier 3 can't fire), the **refinement channel already knows the
result size statically** once List/Array size is added as a refinement domain — that is the "second domain
(List/Array size)" already listed as the last follow-on of the bounds-as-refinements migration (see
[bounds-as-refinements.md](./bounds-as-refinements.md)). So for `emptyList().add(x).add(y)` the channel carries
`size = 2` at the result node.

With the size known, the compiler need not build two intermediate cons cells and grow a backing array; it can
**allocate the final 2-slot structure once and fill it** — classic fusion/deforestation, driven by the
refinement fact rather than a syntactic rewrite rule. Tier 2 composes with Tier 1: the size says *how big* the
one allocation is, uniqueness says *the fills may be destructive*.

This is the concrete pay-off that ties List-size-tracking to a real optimization, and it is the reason to think
about the two together rather than shipping size-tracking as a pure type-checking nicety.

---

## 4. Tier 3 — static contents ⇒ full collapse (this is Reduce-and-Reify)

If `x` and `y` are compile-time-known, `emptyList().add(x).add(y)` is a **closed pure term**, and the existing
single NbE evaluator reduces it to a literal two-element list at compile time. This is not a new pass: it is the
**total-residual** endpoint of [reduce-and-reify.md](./reduce-and-reify.md) — "reduce what you can, reify the
rest," where here everything reduces to ground and the whole result is reified. The same caveats from that doc
apply verbatim (the value is computed in the `compiler` platform and must be **lifted to portable runtime
surface syntax**, not structurally quoted; a native leaf with no lift rule declines, fail-safe).

So the three tiers are not three unrelated features — they are one spectrum keyed on *how much* is statically
known: contents (Tier 3, Reduce-and-Reify) ⊃ size (Tier 2, refinement channel + fusion) ⊃ uniqueness (Tier 1,
last-use analysis). Build the uniqueness analysis and Tier 1 stands alone; the other two are already-scoped
threads that plug into the same slot.

---

## 5. Why Eliot can *infer* this where others must annotate or refcount

Rust *needs* the borrow checker in the type system because it has (a) genuine mutation, (b) aliasing, and (c)
separate compilation — its ownership proof must be **modular**, established without seeing the call sites. Koka,
Roc, and Swift avoid annotations but pay a **runtime reference count** to answer uniqueness dynamically. Eliot
is under neither pressure:

- **Purity is enforced** (no Landin's-knot mutable cells; guarded by `termination/PurityGuardTest`). There is no
  aliasing-through-mutation to reason about; every value is referentially transparent.
- **No recursion / total by default.** The value-flow graph is a *finite DAG*, fully visible — no fixpoint over
  an unbounded call graph.
- **Whole-program monomorphization from `main`.** Every instantiation that actually manifests is reified; there
  are no unknown polymorphic call sites forcing conservative approximation.

Together these let the compiler compute **exact** last-use / uniqueness statically. Where Koka/Roc/Swift ask "is
the refcount 1 *right now*?", Eliot can usually *prove* "this cell is dead at this point" at compile time. The
closest existing languages in spirit are **Roc** (pure surface syntax, compiler mutates in place when the value
is unique) and Koka's **FBIP**; the closest fully-static ambition is **ASAP** ("As Static As Possible" memory
management, Proust) — but none of them start from Eliot's no-recursion + monomorphization combination, which is
what makes the *static* version tractable rather than merely aspirational.

This is the **Use-Site Verification (Sound, Not Modular)** cornerstone applied to memory: don't prove a
definition uses its argument uniquely for *every* caller (a modular linear-type certificate); analyze the actual
monomorphized program and pick the representation that is provably safe *there*. The philosophy already in the
project extends to this optimization for free.

### 5.1 Consequence for where representation lives

Whether a `List` is backed by a mutable or an immutable structure is a **representation decision**, and the
cornerstones are emphatic that representation is *derived, never the identity* (see the Platform-Independence
cornerstone and `feedback_minimize_scala_decompose_in_eliot`). So this analysis belongs on the
**compiler/backend side**, downstream of type formation, exactly like the refinement channel's `Represent`
policy — the base `List` stays abstract, and the mutable-vs-immutable choice never contaminates the language
surface. The natural home is near `used` / `uncurry`, consuming the monomorphized DAG plus the refinement
`size` domain.

---

## 6. What we are *not* doing yet: user-visible linear types

Everything above is inferred and transparent. A **separate, later** question is whether Eliot should also expose
linearity *in the types* — Rust/Clean/Linear-Haskell style. The justification for that is **not** the list
optimization (which needs no annotations); it is **resource protocols**, which matter specifically on
microcontroller targets:

- a UART/SPI peripheral you own **uniquely**,
- a hardware register that must be touched **linearly**,
- a handle/buffer that must be used once and released.

These want the guarantee *in the type* because the guarantee is the deliverable, and they are a genuine
substructural-type-system addition with real annotation cost. Note the **Total by Default** cornerstone already
lists **"linearity for in-place mutation"** among the *deferred* items ("needs foundations that do not yet
exist"), alongside WCET/resource bounds and size-indexing — so this is on the roadmap, but it should be kept
**decoupled** from the transparent optimization. Ship the inference first; reach for visible linear types only
when a resource-protocol client actually needs the guarantee.

---

## 7. Recommendation

1. **Build Tier 1 as inferred, transparent, static last-use/uniqueness analysis over the monomorphized DAG.**
   Zero annotation burden, semantics-preserving, cannot produce a wrong program. Highest alignment, lowest risk.
2. **Fold in Tier 2** as the List/Array **size** refinement domain lands — the size fact turns "mutate in place"
   into "preallocate the exact final structure," which is the concrete win. This is the reason to design
   size-tracking and in-place mutation *together*.
3. **Get Tier 3 for free** from Reduce-and-Reify once its value-path readback exists — closed builder chains
   collapse to literals with no new pass.
4. **Keep user-visible linear types (§6) as a distinct, later feature**, justified by MCU hardware resources,
   not by list building.

### 7.1 One design question to pin down first

On the sharing case (§2), decide the *stance*: purely **opportunistic** (silently copy when a value is shared,
mutate when unique — Roc's behaviour), or opportunistic **plus a lint/diagnostic** that tells a library author
"this `add` chain can't fuse because `l` escapes here." The diagnostic angle fits the use-site-verification
philosophy well (surface the latent cost at the definition, as the IDE already aims to for partiality) and is
cheap to add once the analysis exists — but it is a policy choice worth making before building, because it
shapes whether the analysis result is merely *consumed* by the backend or also *reported*.

---

## 8. Related work (for orientation)

- **Uniqueness types** — Clean (Barendsen & Smetsers). The guarantee-it-in-the-type route; the dual of affinity.
- **Affine ownership + borrowing** — Rust. Modular, separate-compilation-driven; the borrow checker is the price
  of mutation + no whole-program view — precisely the price Eliot's design avoids.
- **Linear Haskell** — Bernardy et al., "Linear Haskell: practical linearity in a higher-order polymorphic
  language" (`a %1 -> b`). Linearity as an opt-in typing discipline for safe in-place arrays.
- **Perceus / FBIP** — Koka (Reinking, Xie, de Moura, Leijen). Precise *runtime* reference counting with reuse;
  the dynamic version of what Eliot wants to do statically.
- **Roc** — pure functional surface, compiler mutates in place when the value is unique (refcount-backed). The
  closest existing language *in spirit*; the difference is Eliot targets a static proof, not a runtime check.
- **Swift** — value semantics + copy-on-write via `isKnownUniquelyReferenced`; the mainstream runtime-uniqueness
  instance of the same idea.
- **ASAP** — Proust, "As Static As Possible" memory management; fully static, annotation-free — the closest
  ambition to Eliot's *static* target, though from a different starting point.
- **Deforestation / fusion** — Wadler (deforestation); Gill–Launchbury–Peyton Jones (build/foldr, "A Short Cut
  to Deforestation"); Coutts–Leshchinsky–Stewart (stream fusion) — the Tier-3-adjacent intermediate-structure
  elimination.
- **Escape analysis** — the JVM's runtime cousin (scalar replacement / stack allocation of non-escaping objects)
  — the "we never refer to the interim lists" observation, done dynamically.
- **Partial evaluation / CTFE** — Jones–Gomard–Sestoft; realized in Eliot as NbE (Reduce-and-Reify), the Tier-3
  collapse.

---

## 9. One-line summaries (for notes / memory)

- **The idea:** when List size is tracked in the refinement channel, also infer when a list may be *mutable* —
  a builder chain `emptyList().add(x).add(y)` whose intermediates are never reused can mutate in place, or (size
  known) preallocate the exact final structure, or (contents known) collapse to a literal.
- **The analysis names:** linear/affine/**uniqueness types** (type-level guarantee) vs. **last-use/liveness
  analysis** (inference); **reuse analysis / FBIP / Perceus** (the in-place optimization); **deforestation /
  fusion** + **CTFE** (the collapse).
- **Why Eliot can infer it, not annotate it:** purity (no aliasing-via-mutation) + no recursion (finite visible
  DAG) + whole-program monomorphization (exact instantiation) = **exact** static uniqueness, no refcount, no
  borrow checker. This is Use-Site Verification applied to memory.
- **Three tiers, one spectrum on "how much is static":** uniqueness ⇒ in-place (Tier 1, standalone) ⊂ size ⇒
  preallocate (Tier 2, refinement channel + fusion) ⊂ contents ⇒ collapse (Tier 3, = Reduce-and-Reify).
- **Representation, not surface:** mutable-vs-immutable is a backend/representation choice (near `used`/
  `uncurry`), the base `List` stays abstract — representation is derived, never the identity.
- **Linear *types* are separate & later:** justified by MCU hardware/resource protocols (unique peripheral, once-
  and-close handle), already listed as a deferred Total-by-Default item; don't couple it to the list optimization.
