# Effects Syntax in User Space: `~` and `&` as Ordinary Values

**Status (2026-08-20): stages 1 and 2 are LANDED; stages 3 and 4 are a PROPOSAL that decides nothing.** The
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

**3.2 Phase order — smaller than it looks.** Constraints are *written* at core (phase 4, `EffectSugarDesugarer`
mints `F ~ Ei` from `{E} A`) and *name-resolved* at resolve (6). Infix operators resolve at **8**. Every downstream
reader — superability closure, `EffectCarriers.declaredEffects`, `RowChecker`, `AbilityResolver`, `BinderRoles`,
constraint-aware declination — runs at ≥ 8. Phase 4 only *builds* syntax, which is fine unresolved. Only **two**
pre-8 syntactic readers exist, both in `EffectSugarDesugarer`:

- `isEffectCarrierBinder` — scans the constraint list for the name `Effect`;
- `isHigherKinded` — matches `FunctionApplication("Function", …)` at the head of the *restriction*. **This one
  breaks if `~` folds into the restriction**, since `~` would then head it.

Both are mechanical fixes, not redesigns.

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

### Stage 3 — constraints become expressions (PROPOSAL)

Collapse the five `*AbilityConstraint` case classes to a single `Sourced[Expression]` per constraint. resolve /
matchdesugar / operator stop re-encoding and simply resolve an ordinary expression; readers normalise via the one
evaluator and pattern-match the `Constraints` normal form. This is where the thirty-file footprint shrinks hard,
and where `&` becomes an applied type constructor rather than a checked separator.

Prerequisite: Stage 2, plus deciding whether an ability marker is nameable in an *expression* position (today it is
addressable but the `Ability` qualifier is not what a bare reference resolves into).

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
