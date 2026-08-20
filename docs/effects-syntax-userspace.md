# Effects Syntax in User Space: `~` and `&` as Ordinary Values

**Status (2026-08-20): stages 1 and 2 are LANDED; stages 3 and 4 are a PROPOSAL that decides nothing, and
stage 3 has since been assessed and found blocked — see §7.** The
question this answers is Robert's: *what would it take to take the effects syntax into user space — the `~`
operator and the `&` combinator — to define them as stdlib things and accept them as normal values, per the
types-are-values cornerstone?*

---

## 1. Where `~` and `&` stood

They were not values, and they were not even syntax. They were a **side channel**.

- `~` is a hardcoded `symbol("~")` in one parser (`ast/fact/GenericParameter.scala`), and `&` was nothing but the
  separator in `.atLeastOnceSeparatedBy(symbol("&"))` on the next line. Both sat in `reservedSymbols`
  (`ast/fact/Primitives.scala`) precisely so the ordinary operator machinery could never see them.
- The result is not an expression. It is `Map[String, Seq[AbilityConstraint]]`, re-encoded in **five
  phase-specific case classes** (ast `GenericParameter.AbilityConstraint` → core `NamedValue.CoreAbilityConstraint`
  → resolve/block `ResolvedValue.ResolvedAbilityConstraint` → `MatchDesugaredValue.ResolvedAbilityConstraint` →
  `OperatorResolvedValue.ResolvedAbilityConstraint`), carried as `paramConstraints` on five value facts, plus a
  parallel `EffectRow[C]` generic over the same type. Thirty non-test source files touch it.

So a constraint was the one part of a signature that is **not in the expression language**. The binder's *kind*
already is (`typeRestriction: Sourced[Expression]`). Closing that asymmetry is the whole of this document.

## 2. Three precedents that say this is reachable

1. **`infix right type =>[A, B] = Function[A, B]`** (`stdlib/eliot/eliot/lang/Function.els`) — a *type-level infix
   operator already defined in user space*, with fixity, resolved by the ordinary operator phase.
   `TypeAliasDefinition` already accepts an operator-named `type` and puts it in the `Default` namespace precisely
   because operators are referenced bare. `&` is exactly this shape.
2. **`where`** — a user-space predicate living in a signature, desugared to a `^Where` companion and reduced by the
   one NbE evaluator on the compiler track. Proof that "a signature-position expression, evaluated at compile time,
   consulted by a channel" is already a working pattern.
3. **An ability is already a value.** `AbilityBlock` emits a synthetic marker `FunctionDefinition` (`Foo^Foo`)
   carrying the ability's common generics — an ordinary `NamedValue`, in the ordinary module dictionary.

## 3. What actually blocks the full move

**3.1 Namespace.** The marker lives under `Qualifier.Ability`, and `ValueResolverScope.getAbility` did not look it
up — it *scanned* `dictionary.values` for any name whose qualifier was `Ability(n)`. So an ability name resolved by
a mechanism no other name uses. Stage 2 fixes this.

**3.2 Phase order — bigger than it looks.** Constraints are *written* at core (phase 4, `EffectSugarDesugarer`
mints `F ~ Ei` from `{E} A`) and *name-resolved* at resolve (6). Infix operators resolve at **8**. Most downstream
readers — `EffectCarriers.declaredEffects`, `RowChecker`, `AbilityResolver`, `BinderRoles`, constraint-aware
declination — run at ≥ 8. Phase 4 only *builds* syntax, which is fine unresolved. Only **two** pre-8 *syntactic*
readers exist, both in `EffectSugarDesugarer`:

- `isEffectCarrierBinder` — scans the constraint list for the name `Effect`;
- `isHigherKinded` — matches `FunctionApplication("Function", …)` at the head of the *restriction*. **This one
  breaks if `~` folds into the restriction**, since `~` would then head it.

Both are mechanical fixes, not redesigns.

**The superability closure is the exception, and it is not mechanical.** `ValueResolver.superConstraints`
(effects-v5 §7) runs at **resolve, phase 6** — an earlier draft of this section listed it among the ≥ 8 readers,
which is simply wrong — and it is a *structural* reader: it takes a constraint's `abilityFQN`, looks up that
ability's marker, zips the marker's generic parameters against the constraint's `typeArgs` to find which parameter
this use bound to this binder, and substitutes. A constraint that is still an unstructured `FlatExpression` (an
un-precedence-resolved `&` run) cannot be decomposed that way, and the closure cannot simply move later: it is
where a `~` is *resolved in the ability's own scope*, and scope exists only at resolve. See §7.2.

**3.3 `~` is not evaluable.** `where` reduces to a `Bool`. `A ~ Show` cannot: deciding it needs ability *search*
(implementation lookup, `where` guards, constraint-aware declination), which is fact-driven in
`AbilityImplementationProcessor`, not evaluation-driven. So `~` has to be a **native leaf on the compiler
platform**, alongside `typeEquals`/`add` in `SystemNativesProcessor`. That is the sanctioned pattern, not a
violation. `&` needs no native.

**3.4 `signatureEquality` changes meaning.** Constraints are currently *excluded* from it
(`NamedValue.signatureEquality` compares only `signature`). Fold them into `typeRestriction` and they become part
of the signature, so a layer adding a body must then spell identical constraints. Per the layers cornerstone that
duplication is sanctioned — arguably an improvement, since drift gets caught — but it is a real behavioural change
across the abstract↔concrete merge, and it must be a deliberate decision rather than a side effect.

## 4. The stages

### Stage 1 — `&` is a stdlib declaration the compiler resolves (LANDED)

The surface is unchanged: `[F[_] ~ Console & Log]` parses exactly as before. What changes is that `&` stops being
a symbol the parser recognises and becomes a **name the compiler looks up**.

- `&` leaves `reservedSymbols`, so it is an ordinary user-operator symbol — definable, referenceable, manglable.
- `stdlib/eliot/eliot/lang/Ability.els` declares it: `infix left type &[A, B]`, an abstract type-level operator
  carrying its own fixity. The module joins the `eliot.lang` prelude, so it is ambient and needs no import.
- The parser records the operator that joined each constraint (`AbilityConstraint.combinedBy`, `None` for the
  first of a list), carried through `NamedValue.CoreAbilityConstraint`.
- `ValueResolver` resolves that name through the **ordinary dictionary** and rejects anything that is not
  `WellKnownTypes.abilityCombinatorFQN`. A program that shadows `&` locally, or builds without the prelude, now
  gets a real diagnostic instead of silent acceptance.

The combinator is dropped after that check — it does not reach `ResolvedAbilityConstraint`, so no later phase
knows it exists. Stage 3/4 is what turns the resolved FQN into an actually-applied type constructor.

**Status of `&`'s honesty.** It is recognised by well-known FQN, the same status `Bool::true`, `Function` and
`Type` already have. That is user-space *declaration* and ordinary *resolution*, not yet user-space *semantics*.

### Stage 2 — an ability name resolves like every other name (LANDED)

`getAbility` becomes a keyed lookup of `QualifiedName(name, Qualifier.Ability(name))` — the marker's own key — in
the ordinary `dictionary`, with the same `privateNames` fallback `resolveValueName` uses. Consequences:

- **Import scope and shadowing are honoured.** The old scan matched *any member* of an ability (`printLine` carries
  `Ability("Console")` too), so an ability resolved even when the weak-prelude tier had dropped its marker.
- **The nondeterminism is gone.** The scan was a `collectFirst` over `Map.values`; with two same-named abilities
  from different modules in scope, which one won was iteration order. It is now the dictionary's answer, i.e. the
  same answer the same name would get in any other position.
- It is O(1) rather than O(dictionary) per constraint.

This is the load-bearing step for everything after it: an ability is now addressed the way a value is addressed.

**It is a hygiene and determinism change, not a behaviour change, and that is deliberately not testable.** Where the
two implementations disagree at all, the old answer depended on `Map` hash-iteration order, so a test asserting
either answer would be asserting that order. Everything constructible from the stub prelude agrees between them —
verified by temporarily restoring the scan and finding the new tests still green. The evidence that nothing moved is
the sweep in §6, not a regression test.

### Stage 3 — constraints become expressions (PROPOSAL — assessed and blocked, see §7)

Collapse the five `*AbilityConstraint` case classes to a single `Sourced[Expression]` per constraint. resolve /
matchdesugar / operator stop re-encoding and simply resolve an ordinary expression; readers normalise via the one
evaluator and pattern-match the `Constraints` normal form. This is where the thirty-file footprint shrinks hard,
and where `&` becomes an applied type constructor rather than a checked separator.

Prerequisite: Stage 2, plus deciding whether an ability marker is nameable in an *expression* position (today it is
addressable but the `Ability` qualifier is not what a bare reference resolves into). §7.3 sharpens that
prerequisite: the question is not whether the marker is *nameable* — `searchAbilities` already finds it — but what
it **denotes**, since `&` applied to it has to have operands of some kind.

### Stage 4 — delete the channel (PROPOSAL, and the one to be careful about)

`~` becomes `infix def ~[K](kind: K, c: Constraints): K` — a compiler native per §3.3 — the binder is just
`[F : (Type => Type) ~ Console & Log]`, and `paramConstraints` disappears from all five facts, demands being
recovered by normalising the restriction. Requires fixing `isHigherKinded` (§3.2) and accepting §3.4.

**The reservation, stated plainly.** `~` returning its own first argument makes it an identity function carrying an
out-of-band payload — that is the side channel again, spelled infix, and it would be *less* honest than today's
explicit field. To make Stage 4 genuinely types-are-values the restriction wants to be a real refined kind
(`Restricted[K, Cs]`), which means every reader of a binder's kind must strip the wrapper. That is the point at
which the change is paying for elegance rather than being paid by it, and it should not be started without
deciding it is worth that.

## 5. The open design question Stage 4 forces

After Stage 4, `~` and `where` are two spellings of "a compile-time predicate on a signature" — one evaluable, one
search-driven. Whether they unify into one construct or stay deliberately separate is a language decision, not a
mechanical one, and it should be settled before Stage 3 commits to a representation.

## 6. Verification of stages 1 and 2

- `./mill __.test` green (871 tasks; the new `AbilityConstraintCombinatorTest` and `AbilityNameResolutionTest` add
  nine cases).
- Every example carrying a `main` compiles — 45 of 45 — and `EffectAbilitySet` (the `ability Web[F[_] ~ Console &
  Log]` example) runs with the expected output.
- **Byte-identity sweep**: all 45 example jars are byte-for-byte identical to those built from the same tree with
  these changes stashed. Adding a module to the prelude touches every module's dictionary, so this is the check that
  matters, and it is what says stage 2's rewrite of ability lookup and stage 1's new prelude entry changed no
  generated code.

---

## 7. Stage 3 assessment: what is mechanical, what is controversial

Asked directly — *can stage 3 be implemented, or is something controversial in it?* — the answer is: **the
plumbing is mechanical, the meaning is not, and the meaning is what stage 3 would have to commit to.** Three
findings, in increasing order of how load-bearing they are.

### 7.1 The doc already says stage 3 is gated, and it is right

§5 states that whether `~` and `where` unify "should be settled before Stage 3 commits to a representation".
That is not a formality. Stage 3's whole content *is* the representation: once a constraint is an expression, the
shape of that expression is the language's answer to "what is a constraint", and every reader is written against
it. Starting stage 3 before §5 is answered means answering §5 by accident, in a `case` clause.

### 7.2 The phase-order blocker is real (correcting §3.2)

`ValueResolver.superConstraints` — the superability closure, the rule that makes `ability Web[F[_] ~ Console &
Log]` mean three effects — runs at **resolve (6)**, four phases before infix operators are structured (8). It is
not a name-resolution pass that happens to touch constraints; it is a *structural* one:

```
use.abilityFQN            → look up that ability's marker
marker's generic params   → zip against use.typeArgs
the param bound to *this* binder → inherit that param's constraints
resolve them in the ability's own scope, substitute the use's arguments, recurse
```

None of that can run on an un-precedence-resolved `&` run, which at phase 6 is a flat `FlatExpression` list. There
are three ways out and each costs something:

1. **Special-case `&` at resolve** — a private, single-operator precedence pass just for constraints. This is
   stage 1 undone: `&` goes back to being a symbol one parser understands.
2. **Read fixity four phases ahead.** `InfixPrecedenceResolver` gets fixity from the *referenced value's*
   `MatchDesugaredValue` fact (`OperatorResolverProcessor.annotatePart`). A resolve-phase constraint resolver
   would have to demand phase-7 facts for `&` and every ability named in a constraint, from inside the phase that
   produces phase-6 facts — a fact-cycle hazard exactly where abilities and their users share a file.
3. **Move the closure to ≥ 8.** Defensible, and arguably cleaner (it could read the marker's *already closed*
   `OperatorResolvedValue.paramConstraints` and substitute, instead of re-resolving in the ability's scope). But
   it relocates the single source of truth that `RowChecker.declaredRow` and `EffectAccountingProcessor.openRow`
   both read, and CLAUDE.md pins that closure to `resolveParamConstraints` deliberately. It is a separate,
   reviewable change — and it should land *before* stage 3, not inside it.

### 7.3 The controversial part: `&` applied to *what*?

This is the finding that matters. Stage 3's stated payoff is that "`&` becomes an applied type constructor rather
than a checked separator". Applying it forces a question stage 1 never had to answer: **what does an ability
denote, as a value?**

Today `&` is `infix left type &[A, B]` — both binders default to the restriction `Type`, so `&` is
`Type → Type → Type`. And an ability's marker (`AbilityBlock`) is `Foo(arg0: A, arg1: B): A` — one value argument
per common generic. So `Console[F]` is a partially applied *function value* of type `F → F`. `&[Console[F],
Log[F]]` is therefore not merely unchecked; it is ill-kinded under the only declaration either side has. Nothing
catches that today only because no reader ever type-checks a constraint — they pattern-match it.

There are exactly three honest answers, and two of them are cornerstone-level decisions:

- **(a) Give constraints their own sort.** `Console[F] : Constraint`, `& : Constraint → Constraint → Constraint`.
  This is what most languages do — and it is precisely what the *Effects Are a Channel* cornerstone forbids: "no
  kind or sort is added to the type language". It would also be the first stratification in a deliberately
  non-stratified PTS.
- **(b) Make an ability a type inhabited by its implementations.** `Console[F] : Type`, `&` an intersection/pair
  former, a constraint an ordinary parameter. This *is* coherent with types-are-values — it is the dictionary
  reading — and it contradicts the architecture in CLAUDE.md head-on: "Ability references are fully resolved
  during monomorphization, never passed around in structures." That is a language redesign wearing a refactor's
  clothes.
- **(c) Leave it an uninterpreted stuck neutral.** `&` reduces to nothing; the ability marker means nothing as a
  value; the meaning stays entirely in the compiler's pattern matches on the `&` spine. This is implementable
  tomorrow — and it is **the same reservation §4 states about stage 4, arriving one stage early**: a stdlib name
  carrying an out-of-band payload no evaluator can read is the side channel again, now spelled as an application.
  §4 says stage 4 "should not be started without deciding it is worth that". The identical sentence applies here.

So the controversy is not that stage 3 is hard. It is that stage 3 in form (c) buys syntax and pays in honesty,
while forms (a) and (b) are decisions about the language, not about `paramConstraints`.

### 7.4 What the footprint claim is actually worth

§4 stage 3 promises "the thirty-file footprint shrinks hard". Measured:

- The five case classes total well under a hundred lines, and three of the four inter-phase conversions are
  already one-liners over `EffectRow`'s `map`/`traverse`.
- **46 structural reads** across **16 source files** consume `.abilityFQN` / `.typeArgs` directly
  (`EffectCarriers`, `RowChecker`, `RowElaborator`, `AbilityResolver`, `CarrierKindChecker`, `CheckState`,
  `Track`, `BinderRoles`, `EffectAccountingProcessor`, `AbilityImplementationProcessor`, apidoc's renderer, …).
  Every one of them would decode the expression back into `(abilityFQN, typeArgs)` at the point of use.

A representation whose every reader immediately re-derives the old representation has not been eliminated; it has
been moved behind an extractor. And the *volume* runs the wrong way for the honesty argument: the overwhelming
majority of constraints in a build are not user-written `~ A & B` at all (about fifteen `&` uses in the whole
tree, mostly `implement[…]` headers) — they are minted by `EffectSugarDesugarer` from `{E} A` rows. Stage 3 makes
the compiler's own metadata travel as surface syntax it must re-parse at every read.

### 7.5 Recommendation

**Do not implement stage 3 as written.** Instead, in order:

1. **Take the footprint win without the design commitment.** Collapse the five phase-specific case classes into
   *one* representation parametric in the phase's expression type — `AbilityConstraint[N, E](name: N, typeArgs:
   Seq[E])`, `N` being `Sourced[String]` pre-resolve and `AbilityFQN` after — exactly as `EffectRow[C]` is already
   parametric. The three hand-written converters become `map`/`traverse`. This deletes four case classes and the
   converters, changes no semantics, no phase order and no `signatureEquality`, and is byte-identity verifiable.
   It is the whole of stage 3's *mechanical* content and none of its controversy.
2. **Answer §5** — do `~` and `where` unify? — and with it 7.3's (a)/(b)/(c).
3. **Only then** decide whether a constraint should be an expression, and if so, land the closure relocation
   (7.2, option 3) as its own change first.

Stage 3 is not blocked by difficulty. It is blocked by the fact that it is the language decision, and it has been
filed as a refactor.
