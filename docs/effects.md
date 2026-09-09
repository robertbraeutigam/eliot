# Effects in Eliot — the design, and what is left to do

**Status (2026-09-07): the v5 effect system is shipped, this is its single document, and Part II is the decided
plan to replace its carrier with statically bound implementation names — effects as abilities, an implementation
as a *name* bound by `with` (v6).** Part I describes the tree as it is until §10's flag day lands. What is here is
(I) the design as it actually behaves, (II) the plan to replace it, and (III) enough provenance to read a source
comment that cites a retired document.

**One-sentence summary.** The user writes **effect rows**; the compiler works in **carriers**; suspension is
*declared* in signatures instead of inferred from genericity, and the carrier is *written* by a desugar
before checking instead of solved by the checker — so effect elaboration is a syntax-directed phase, effects
verify as a **channel** beside the type (the same architectural move as the `Int` refinement channel), and
the NbE checker holds one local rule and no effect decisions.

**How to read this document.** Part I states the shipped design; it is the authority for the tree, and the
CLAUDE.md *Effects Are a Channel* cornerstone is its summary. Part II is the plan: the v6 decision (§9), its
implementation steps (§10), the decisions still open (§11, numbered **D**) and the list of things that are
**closed by measurement or decision and must not be re-proposed** (§12). Part III is provenance.
Where Part I and any code, stdlib signature, example or test disagree, Part I wins and the artefact is the
defect.

---

# Part I — The design

## 1. The user model — four rules

1. **Effects run where they are written.** An effectful expression in any plain position performs its
   effects there, and they join the enclosing definition's row. Strict call-by-value in **every** plain
   position, a bare generic slot included: `choose(readLine, readLine)` runs both reads. The only exceptions
   are the two *declared* ones below.

2. **Suspension is declared, and a row is not a carrier.** A parameter that must *not* run its argument
   declares a row: `whenTrue: {} A` receives the computation unrun; `if[T](c: Bool, value: {Abort} T)`
   spells this and means it.

   A **row** position means *"a value or a computation"* — the empty row is a legal row, so a pure argument
   fits and is lifted. A **carrier-typed** position (`x: G[A]`, `IO[A]`, a pinned stack) means *"a
   computation on this carrier"*, and a plain `A` is a type error there, never a lift. Both are `F[A]` after
   desugaring, so the difference is read from the **row tag** (`EffectRow.parameterEffects`), never from the
   shape.

   Until this rule landed, every `~ Effect`-constrained carrier slot lifted a pure actual, which made the
   *calling convention depend on a constraint the callee declares for its own body's sake*: adding
   `~ Effect` to `def hold[G[_]](x: G[String])` silently changed what callers could pass, and without it the
   same call died in the quoter with "contains unresolved variable" instead of reporting a mismatch. That is
   exactly the implicitness this design exists to remove.

3. **Pinned means captured.** `{Throw[E] | Id} A` is a reified computation *and* an ordinary type, usable in
   `data` fields, discharger parameters, `List[TestCase]`. Open rows never appear in types; pinned rows are
   the only place a type contains a computation.

4. **An effect passes through a position if and only if that position declares it.** *(Found last, stated
   last, and it outranks the other three.)*
   - A **plain generic is a payload, always.** `A`, `B`, `T` in `def .[A, B](a: A, f: A => {} B): {} B`,
     `def ++[T ~ Combine[T]](left: T, right: T): T`, `def foldLeft[A, B](initial: {} B, …): {} B` can never
     be instantiated at a computation. A function that transports effects says so.
   - A **rowless slot may not receive a computation** — not a carrier-headed value, not a pinned capture,
     not a value whose declared row is non-empty. A hard error naming the slot, never a silent re-route.
   - `{}` is a **row variable**, not a fixed carrier. `ρ := {}` is an ordinary instantiation, so
     `dependency.url` (ρ = `{}`) and `items.foreach(x -> printLine(x))` (ρ = `{Console}`) go through the
     *same* declaration of `.`.
   - **`Id` is the value of the empty row, and that is allowed.** A row-polymorphic definition instantiated
     at `ρ := {}` is *written* at `Id` and erased by monomorphization. Rule 4 constrains declarations, not
     the representation of the empty row.
   - A **carrier-headed slot captures**, however that carrier is named — a pinned row's stack, one of the
     callee's own carrier binders, the concrete `Id`, or a platform run carrier (`data Box(action:
     IO[Unit])`). There is no third kind of slot and no name-keyed exemption: the four namings are one
     predicate, not four arms.

   Everything the elaborator decides is then decided by declarations, per call, order-free.

**Consequences the user sees.** `something.foldLeft(f, z)` with `something : {Console} List[T]` works with
zero declaration on `foldLeft`: the effects run, the payload flows, `Console` joins the caller's row, and
the collections library stays effect-oblivious. Evaluation order is readable from signatures. Rows remain
the only effect surface, and diagnostics stay in payload/row vocabulary.

**Rule 4 was agreed and then worked around four times, and every stall in this design's history traces to
that erosion.** Recorded so it cannot read as new:

| # | how rule 4 was worked around | what it cost |
| --- | --- | --- |
| 1 | bare-generic slots exempted from rule 1 — "mode belongs to the instantiation" | six days; a mode resolver, obligations, splice-restart (~350 lines), all reversed |
| 2 | declined a row on `foldLeft`/`foldOption` for ergonomics | the elaborator could not hoist at a generic-return callee *at all*; kept the whole payload router alive |
| 3 | the derived discharge stack routed a computation through `.`'s **rowless** slot "as data" | 5 `State`-family miscompiles; made "a generic is a payload" false, so nothing downstream could assume it |
| 4 | `foldOption` left with a strict `ifNone` because both declared spellings failed | a silent lazy-branch failure mode |

A fifth move was proposed and rejected in that form: *let the elaborator's payload test accept a
generic-headed return*. It **approximates** rule 4 instead of **declaring** it in the signature.

## 2. The surface

### 2.1 Rows, and the empty row `{}`

`{}` denotes the signature's own ambient carrier — *"on my own carrier, nothing added"*. It is the spelling
of every suspended-but-effect-transparent slot (`fold`'s arms, `else`'s fallback, `catch`'s handler,
`foldLeft`'s `initial`, `.`'s `f`), and it is the only spelling in the tree: zero `{Effect}` occurrences
remain in `.els`.

Mechanically `{}` parses as a tail-less row whose desugar treats the empty open row as the machinery entry
`Effect`, so `{}` and the older explicit `{Effect}` produce the *same* AST — one carrier, one `F ~ Effect`
constraint, one row tag. When a definition already binds exactly one `Effect`-constrained carrier
(`G[_] ~ Effect`, as every discharger does), its rows reuse *that* binder instead of minting a second, and
the row's entries join its constraints; two or more such binders are ambiguous and mint as before. That
reuse is why `else`'s `fallback: {} A` **is** `G[A]` and still accepts `host else "localhost"`.

The synthesized constraint resolves at its fixed FQN (`eliot.carrier.Effect`), so writing `{}` needs no
import — which is what keeps `map`/`flatMap`/`pure` out of user scope. A row with a base but no entries
(`{| G} A`) is rejected at the parser: that is just `G[A]`.

### 2.2 A parameter row is *supplied* — what makes a discharger

A **non-empty** row in a parameter position says "on my ambient carrier **extended by** these", and the
extension is the same subtraction the elaborator applies to a call, read one level up at the declaration:

- an entry the definition's own declared (return) row already has needs no extension, so `if`'s
  `value: {Abort} T` rides the ambient unchanged (`if` declares `Abort` itself);
- an entry it lacks is **supplied**, so the slot is that entry's carrier stacked over the ambient.

```eliot
def if[T](condition: Bool, value: {} T): {Abort} T                     -- performs Abort
def else[A](computation: {Abort} A, fallback: {} A): A                 -- supplies Abort: discharged here
def catch[E, A](computation: {Throw[E]} A, onError: E => {} A): A      -- supplies Throw[E]
def runStateToPair[S, A](initial: S, p: {State[S]} A): Pair[A, S]      -- supplies State[S]
```

Read as English they are already right: *"I will run this computation, and the `Throw` it needs comes from
me."* That is what a discharger is, and it needs no tail, no base and no second concept.

The first bullet has a consequence worth stating outright, because it reads as a bug the first time it is
met: **a definition cannot supply an entry its own declared return row already names.** A combinator that
consumes `Throw[E]` and re-raises `Throw[E]` itself — the "rewrite this failure's message" shape — cannot
spell its parameter `{Throw[E]} A`, because that denotes the very same carrier rather than one layer above
it, and there is then nothing to discharge. The two available answers are both honest: take the inner error
at a *different* declared type (`[E1, E2]`, the shape `Throw`'s own cross-lift instance already uses), or
pin the body (`{Throw[E] | Id} A`) and give up composing with effectful bodies. This is not a limitation of
the supply rule so much as the rule being per *entry* and syntactic, which is what keeps it decidable from
declarations alone.

`EffectSugarDesugarer.supplyPinnedParameters` rewrites the supplied entries into the **pinned** spelling
over the ambient carrier before anything else runs, so the signature and the capture tag
(`EffectRow.pinnedParameterEffects`) are identical to what `{Abort | G} A` produced by hand — which is why
this landed one signature at a time with no downstream phase touched. Machinery never supplies (so `{}` is
never a stack), and only a **top-level** parameter row supplies: a row in an arrow codomain
(`onError: E => {} A`) is the callback's own row on the ambient carrier.

A discharger's base carrier is found by the ordinary rule (§2.1's one `Effect`-constrained binder), so the
dischargers that used to leave it unconstrained *because* the pinned tail named it now say `G[_] ~ Effect`.
No new carrier-discovery rule exists, and none should be added.

### 2.3 Pinned rows — the one spelling of a stored computation

A **stored** effectful value must commit to one concrete representation. Before pinned rows that commitment
had to be spelled in machinery vocabulary (`TestCase[ThrowCarrier[AssertionError, Id]]`), leaking carrier
names onto exactly the users the row sugar hides them from.

```eliot
{Throw[E]} A          -- open: caller picks the carrier
{Throw[E] | Id} A     -- pinned: the canonical Throw layer over the pure base Id
```

A pinned row is not a constraint but a *concrete type*: the canonical carrier stack realizing exactly those
effects over the base, built in `core` by the `<Ability>Carrier` naming convention (the carrier is colocated
with its ability, so it resolves wherever the ability does):

```
{Throw[E], State[S] | Id} A   ⤳   ThrowCarrier[E, StateCarrier[S, Id], A]
```

- **Entries are ordered**: leftmost = outermost = discharged first. Nesting order is semantically observable
  (state preserved on failure or not), so pinning makes it a visible, user-written choice. Open rows stay
  unordered constraint sets.
- **No generic parameter is introduced**; everything downstream sees what the hand-written carrier spelling
  produced.
- A pinned row is one *solution* of the corresponding open row's constraints, so construction by
  unification just works.
- **A stored row MUST be pinned.** A `data` field with an open positive row is rejected: *"A stored effect
  row must be pinned to a base carrier."* So is an open row in a type-alias body (`type Susp = {Suspend}
  Unit`) — an open row lowers its carrier onto the alias's own generics, which a definition naming the alias
  cannot reach.
- A **generic tail** (`{E | G}`) is no longer written anywhere: §2.2's supply rule produces the identical
  type. A **concrete** base (`| Id`, `| IO`) is the live spelling.
- **Zero entries is a legal pin** (`{| Recorded} A`, shipped 2026-09-04 as W3): no layers, so the stack *is*
  the base and the type is the plain `Recorded[A]`. It is spelled as a row not for the type — that needs no
  row — but for the **tag**, which says the slot *hosts a computation on that carrier*. That is the one
  thing about a user's own carrier no declaration could state before, and it is what lets a fake run be
  written inline at a call site that has an ambient carrier of its own (§7.7). It is not a new meaning of
  the row and not a new kind of slot: §1 rule 4's first naming, "a pinned row's stack", already covers it,
  and the tag-not-shape distinction is the one that already separates `{} A` from `G[A]`. It was rejected
  until W3 on the reasoning that such a row "is that carrier itself and needs no row spelling" — true of the
  type, false of the tag.
- A pinned row spelled **through a type alias** is read through one level of alias expansion by all three
  consumers that need it: `topRegionCarrier` (the definition side), `RowElaborator.declaredResultKind` (a
  saturated call to such a callee is `Kind.Carrier`, not a payload to `pure`-wrap) and
  `RowChecker.pinnedReturnEntries` (such a return is a declared capture, so its abilities count as
  declared).

**Usage model.** Application code writes bare open rows and learns nothing of this. Storing an effectful
value costs one concept — rows have a base; `| Id` when only pure control effects remain, which also proves
the value can do no I/O (`Id` has no `Suspend`) and is total (`Inf` has no canonical carrier either).
Discharger authors learn nothing new: a parameter row is what you supply.

There are **no discharge markers**. A discharger's consumed effect vanishes structurally — it lands on an
inner transformer carrier, absent from the caller's ambient — so there is no negative member to spell.

### 2.4 Naming a set of effects — an ability that requires abilities

An ability may require other abilities **of its carrier**, and that is how a set of effects gets a name:

```eliot
ability Web[F[_] ~ Console & Log]

def handle(request: Request): {Web} Response       -- declares all three
def audited[A](action: {} A): {Web} A              -- effect-transparent over the set

ability Api[E, F[_] ~ Web & Throw[E, F]]           -- parameterised, and transitive
```

No syntax was added — `ability`'s common generic parameters already took `~`, and a method-less,
never-implemented ability already rode a row to `main`. The whole feature is one rule in
`ValueResolver.superConstraints`: **a `~` constraint is closed under what the named ability itself
requires.** Five details it settles:

- **Which requirements are inherited is decided by the use, not by a shape**: what is inherited is what the
  ability requires of *the parameter this use bound to this binder*. A requirement on an unrelated parameter
  (`ability Fallible[E ~ Show, F[_] ~ Throw[E, F]]`) stays on `E` and never lands on the carrier, where
  `declaredEffects` would read it as a declared effect.
- **It lands in `resolveParamConstraints` only.** A carrier binder's constraints are the single source of
  truth for "declared" that *both* verifiers read (`RowChecker.declaredRow`,
  `EffectAccountingProcessor.openRow`), so one place covers the whole channel. `EffectRow` is rendering
  vocabulary and keeps the name the user wrote — `{Web}` still reads as `{Web}`.
- The ability's requirements **resolve in the ability's own scope**, then its parameters are substituted by
  the use's arguments.
- **Closure is transitive and idempotent**, so mutually-requiring abilities close instead of looping.
- **It is uniform**: a hand-written `G[_] ~ Web` inherits exactly as a row entry does, and an ability *with*
  methods requires others the same way.

Two consequences to state plainly. The name is **real**, so it propagates to callers as itself — correct,
since the caller's carrier must have it too. And it is a property of the **carrier**, so discharging one
effect behind the name does not remove the name: name a set for what rides together, not for what one
function discharges.

The same rule can state a relation the tree could not express before: `ability Console[F[_] ~ Suspend]`
would put "Console rides Suspend" in the ability instead of repeating it on every instance. Under v6 (§9) an
`effect` has no carrier binder, so the question disappears — and so does this spelling of a *set* of effects (§12, "not now").

### 2.5 `~` and `&`

`&` is a standard-library name: `infix left type &[A, B]` in `eliot.lang.Ability` (prelude, so ambient). The
parser accepts *any* operator between two `~` constraints and `ValueResolver.resolveCombinator` looks the
name up in the ordinary dictionary, requiring `WellKnownTypes.abilityCombinatorFQN` — so a module declaring
its own `&` takes the name back and gets a diagnostic instead of the built-in meaning. The combinator rides
ast→core on the unresolved constraint's `combinedBy` purely to reach that check and is dropped there; no
phase past resolve knows it existed, and that is a property of the model rather than a convention, because
`combinedBy` exists only on the *unresolved* constraint type.

An ability name resolves like every other name: `ValueResolverScope.getAbility` is a keyed lookup of the
ability's **marker** (`QualifiedName(n, Qualifier.Ability(n))`) in the ordinary dictionary, with the same
`privateNames` fallback a value name gets. So an ability honours import scope and shadowing, and is no
longer decided by `Map` hash order. **Do not reintroduce the scan**, and do not add a second lookup path for
ability names.

`~` stays reserved — it is a binder marker like `:`, not a name. Taking it into user space is **D3**.

Constraints travel in **two** generic types, one per resolution state, each parametric in the phase's
expression type: `ast/fact/UnresolvedAbilityConstraint[E]` (ast and core) and
`resolve/fact/AbilityConstraint[E]` (resolve through operator resolution). The split is load-bearing rather
than cosmetic.

### 2.6 What deliberately has no spelling

- **A `type` spelling of a row.** `type Web = {Console, Log}` was proposed and rejected: `type X = …` names
  a *type*, and a set of abilities is not one — which is why that spelling needed a body no type expression
  could hold — and paying for it with a new `ast.fact.Expression` case is not how this language grows.
  §2.4's requiring ability is the answer.
- **Aliasing a computation with its row.** `type Test = {Writer[W]} Unit` is a carrier-applied type and is
  written as ordinary generics: `type Test[F[_] ~ Writer[W]] = F[Unit]`. The mechanical reason no sugar
  quietly does it: the row desugar runs in `core`, before names resolve, so a definition that merely *names*
  an alias carries no `{…}` of its own and mints no carrier.
- **A negative effect.** See §2.3 — discharge is structural.
- **Row inference of any kind.** See §5.

## 3. The mechanism

### 3.1 The elaborator writes the carrier

`row/RowElaborator`, run by `RowElaborationProcessor` between the recursion gate and saturation, rewrites
each definition into **fully explicit monadic core Eliot** — so monomorphization, ability resolution,
`used`/`uncurry`, the jvm backend, `runMain` and the synthetic main are unchanged consumers.

```
printLine("hi")        ⟶   printLine[F]("hi")
flatMap(k, readLine)   ⟶   flatMap[F](k, readLine[F])
```

The ambient carrier at any point is a **syntactic** function of the definition's own minted binder
(`EffectSugarDesugarer` mints it as generic **0**), so every carrier position is **rigid**, and the base
binds once at the platform entry point — both tracks (`IO` on jvm, `Either[String, _]` on the compile track)
work without the elaborator knowing which. Because the carrier is written, **no carrier metavariable is ever
created**.

Three rules complete it:

- **A region's carrier has three states** (`RegionCarrier`): `Absent`; `Spelled` — writable from the
  definition's own declaration (its minted binder, the pinned stack its return declares minus the payload, a
  platform run carrier); and `Unspelled` — a carrier exists but only a *callee* can name it (chiefly the
  interior of a pinned capture). All three place identically; **only `Spelled` writes.**
- **Every type argument a declaration determines is written**, as a leading positional prefix of the
  callee's binders. Two sources determine one: the **region** supplies the carrier, and a **pinned
  parameter** supplies its row's ability arguments, instantiated from the captured argument's own declared
  row (`catch`'s pinned slot against `bad`'s declared `{Throw[String]}` gives `E := String`). Writing stops
  at the first binder nothing determines, since `typeArgs` applies positionally.
- **The discharge stack is derived**: `carrier(call) = stack(callee.declaredRow ∖ ambient.declaredRow) over
  ambient`. A callee needing more than the ambient provides cannot be running on it, so it runs on the
  canonical stack of that difference — which is what lets a `val` bind a dischargeable computation as data.
  Delivering that result to a **rowless** slot is a hard error (rule 4); the fix at the call site is the
  direct call.
  - **The filter that makes it work**: an effect the ambient does not declare is *not* automatically
    dischargeable. `Suspend`-riding effects (`Console`, `Log`, `Inf`) have no `<Ability>Carrier` and are
    provided by the base. Dischargeability is read off **the universe's own pinned rows** — an effect is
    dischargeable in this body iff a discharger for it is among the names the body reaches — which needs no
    lookup, since probing for a carrier type that does not exist is itself a hard compile error.

**Pure code is untouched.** A definition with an empty row and no discharge elaborates to itself. The one
exception is a pure definition that *discharges*: its region is written at `Id`, its arms come out
`pure[Id]`, and `runId` is written beside it — **honestly well-typed**, not well-typed-modulo-normalization.

Two mechanical invariants of the phase, both of which cost real bugs to learn:

- **Position fidelity.** `assemble` returns the **original** nodes when nothing changed. Rebuilding an equal
  spine re-attributes it to per-argument positions, silently moving every diagnostic anchored at a call and
  duplicating LSP hover hints.
- **The universe is built by demand, not guessed.** `RowChecker.Universe.onMiss` reports every name
  consulted but absent; the processor fetches exactly those and repeats until a round misses nothing new.
  Guessing would fall back to unknown-callee approximations, and a wrong slot mode changes *when* an effect
  runs — which no later phase catches.

### 3.2 The whitelist — the anti-accretion guardrail, binding on every future change

The elaborator may consult exactly: a callee's declared parameter and return types (slot carrier-headedness,
carrier-codomain arrows, atomic-vs-applied shape), its declared row and carrier binders, its pinned metadata,
the run-boundary registry, one level of type-alias expansion inside those signatures, and **the derived row
of an argument at a position whose classification that argument settles**.

A decision that cannot be made from that list is a **design gap to close in the declarations** — never
approximated by a new syntactic rule. In particular, **a rule that inspects a *sibling argument's*
expression shape is inference, not desugaring, and is prohibited.** (An elaborator-local join over the
callee's *declared* parameter shapes is inside the whitelist; a sibling-expression rule is not.)

The argument-row clause is not a sibling rule: the derivation reads only declarations, and the position is
one the callee's own signature nominated by mentioning a carrier binder there. It is what settles
`ρ := {}` — by what the determining positions *do*, not by what kind of value they hold. Both readings are
needed and neither subsumes the other, and their conjunction can only **withhold** `ρ := {}`, never grant
it: a withheld empty row costs a `pure` wrap, a wrongly granted one puts an effect on a carrier that cannot
perform it.

The fail-safe direction is built in: a missing rewrite leaves direct-style code the checker rejects loudly;
a wrong rewrite silently changes when an effect runs.

### 3.3 Two verifiers, one vocabulary

Checking a runtime term yields a **payload type** (the existing NbE judgment, which never sees an effect)
and a **row** (a second output, exactly as an `Int`'s range lives in the refinement channel beside the type,
not inside it). Row constraints are set-shaped: union for sequencing, inclusion for boundaries
(`derived ⊆ declared`) — commutative and order-independent, so no argument-order sensitivity can exist.

- **Pre-mono**, per definition: `RowElaborationProcessor.verifyRow`, reported at the definition before
  anything downstream runs.
- **Post-mono**, at ground instantiations: `channel/EffectAccountingProcessor`, wired as a **codegen
  precondition** via `getFactOrAbort`, so an undeclared effect blocks code generation rather than merely
  warning. It gates each reference's contribution by the **ride test**: a contribution counts only if it is
  performed on the value's *own* ambient carrier, compared by exact `GroundValue` equality against
  `MonomorphicValue.ambientCarriers`.

Both emit the same message: *"This value performs the effect 'X' but does not declare it…"*.
*Forward what is declared, derive what is done* — a forwarded per-operation verdict would be a checker
self-report and is rejected, as is any negative-effect surface.

The pre-mono check is bounded exactly three times, by what declarations genuinely cannot settle:

1. **coverage** — an unknown callee may leave the derivation incomplete;
2. **decidability** — a definition whose declared return could *itself* be the carrier (an applied
   `Box[String]`, `IO[Unit]`, a generic head) is the constructor-class shape, settled only by the
   instantiation;
3. per row entry, a contribution handed to a slot that **fixes a foreign concrete carrier**
   (`RowChecker.fixesCarrier`) is performed in *that* carrier, not on this definition's ambient — which is
   the whole of the fake-carrier testing strategy (§6).

All three defer to accounting's ride test, which decides them exactly. Everything else is enforced,
including the one diagnostic accounting cannot voice — *declared pure but performs effects*, for a
definition whose return cannot host a carrier, since such a value's mono fails and produces no
`MonomorphicValue`.

`fixesCarrier` has three conditions, each fail-safe *towards deciding* (the direction that keeps a
diagnostic at the definition): the argument is a **saturated call to a callee with a non-empty declared
row**; the slot's declared type is a **concrete constructor applied to at least one argument**, not the
`Function` arrow and not one of the callee's own binders; and the slot is **not what the argument's own
declared payload already is** (a payload delivery, `orEmpty(readLine)`, must stay charged — reading it as a
carrier fixing silently dropped the everyday "I forgot the effect set" diagnostic). It compares the
*payload*, not the declared return, because a rowed callee's return is `F[X]` by the time the row check
reads it.

The first condition is asked of what the argument **finally delivers**, not of its outermost node. A
`{ … }` block is an applied lambda, so before that peel a block never matched and the harness was charged
for the very effect it fakes — while the same harness written as a single call deferred correctly, which is
a difference the user cannot see and cannot act on. The peel changes only *which* expression the
discriminator is applied to: a block delivering the slot's own payload (`takeOpt({ …; readOpt })`) is still
charged where it ran.

### 3.4 `Id`

`Id` is the value of the empty row, and it is **written**, not manufactured. It stays ordinary `data`
(`data Id[A](runId: A)` + `implement Effect[Id]`, deliberately **no** `Suspend[Id]`, so real I/O can never
run on it — only the pure control effects `Abort`/`Throw`/`State`/`Dep`). `channel/IdNormalizer` erases it
at the `WovenValue` seam and `WovenValueProcessor.assertNoIdResidue` is a **hard build error** on any
survivor — the proof that erasure is complete, and *more* load-bearing now that `Id` is written
deliberately.

What v2 was faulted for, and what is gone, is the checker **manufacturing** a carrier head on pure judgments
(`T` → `Id[T]`, term → `pure@Effect[Id](term)`) so slot arms could split unconditionally, then normalizing
it away — ~95% of which was the identity `runId(pure@Id(x))`. Written `Id` with an honest type is not that.

**Recurring tax: any new consumer of `MonomorphicValue` or of mid-mono `SemExpression`s must Id-normalize
first.** Rendering hides the machinery *names* but not the inserted machinery *nodes*.

### 3.5 Discharge

Discharge falls out structurally, with no annotation: a discharger's consumed effect lands on an *inner
transformer carrier* (`StateCarrier[S, G]`, not the caller's ambient `G`), so it simply drops out of the
derived row. That is why wrapper-reached discharge inside a `{Console}` body just compiles, and why there is
nothing to spell as a negative effect.

A discharger must be **called directly** (`runStateToPair(s0, p)`): by rule 4 the dot's subject is a plain
type parameter, which may not carry a computation, so `p.runStateToPair(s0)` is a hard error naming the fix.
The infix dischargers `catch`/`else` resolve to a direct call and are unaffected. A discharger's **handler
may itself perform effects** (`onError: E => {} A`, a row over the same carrier), and a **`val`-bound**
computation is dischargeable, since a call needing more than the ambient declares carries its own discharge
stack and the `val` binds the reified computation as data.

### 3.6 An ability is not an effect by nature

**A method performs an effect because it declares one.** An ability method spells its effects with a row on
its return, exactly as any other definition does:

```eliot
ability Console[F[_]] { def printLine(s: String): {Console} Unit }
```

That row desugars onto the *ability's own* binder (`EffectSugarDesugarer.abilityMethodCarrier`), so the
ordinary declared-carrier and declared-row rules answer for it and **no phase reads effect-ness off the
shape of an ability's signature**. A method declaring **no** row performs nothing — which is exactly what a
**constructor class** is (`ability Container[F[_]] { def wrap[A](a: A): F[A] }`), and what the old "any
higher-kinded binder of an ability method is a carrier" rule made impossible to express: it read `unwrap(b)`
as performing the effect `Container` and rejected `def unboxed(b: Box[String]): String = unwrap(b)` with no
spelling that could fix it.

The one exception is the **machinery** abilities `Effect`/`Suspend`, whose methods keep spelling `F[A]`:
machinery is filtered out of every row by design, and they are recognized by name
(`EffectMachinery.isMachineryAbility`).

`EffectCarriers.declaredCarrierBinders` therefore asks which binders a signature *declares* as carriers:
ability-constrained (`[G[_] ~ Effect]`, every row-minted binder), the base of a declared **pinned** row
(deliberately unconstrained, so nothing else marks it), or a *machinery* ability's method.

**Carrier-ness is recognized by a tag threaded from elaboration — never by name or shape.** A pinned row
desugars to a carrier stack with no residual marker, so the marker is added at the desugar and carried on
the fact (`EffectRow.returnPinnedEffects` / `pinnedParameterIndices`), plus the platform-contributed run
boundaries (`row/RunBoundaryFunctions`) for concrete carriers no row can spell, like the synthetic main's
`IO[A]`. Classifying by the `<Ability>Carrier` naming convention, an LSP reverse table, or "has an `Effect`
instance" **miscompiles in both directions** and is prohibited.

### 3.7 Rendering

User-facing text stays in payload/row vocabulary: carrier machinery names (`ThrowCarrier`, `AbortCarrier`, …)
and the `Id[X]` payload wrapper are never rendered. One inverter does it — `effect/EffectRowRendering` driven
by `EffectCarrierNaming.abilityNameOfCarrier` — used by `monomorphize/fact/GroundValueRenderer` (LSP hover,
ability-demand diagnostics) and `unify/SemValuePrinter` (`Expected:`/`Actual:` lines), so a carrier stack
always reads as the pinned row that spells it (`{Abort | IO} String`).

**Recognizing carrier-ness by name is sanctioned here and nowhere else** — a misrendering is cosmetic, the
same guess in the checker miscompiles. Two deliberate rules: `Id[X]` is erased to `X`, but an `Id` **row
base is kept** (`{Throw[E] | Id} A` is legal surface and is *not* the open row `{Throw[E]} A`). The one
demand with a story rather than a name — `Suspend` at `Id` — gets a purpose-built message, keyed on the
*base* of the row (`GroundValueRenderer.baseCarrier`), so any effect demanded on a stack that bottoms out at
`Id` keeps the pure-base explanation.

### 3.8 What the checker still holds

Exactly **one** effect rule, deliberately: a pure term meeting a **rigid** carrier-headed expected type is
`pure`-lifted (`check/EffectLifter.tryPureWrap` — no metas, no ordering, no lattice). The checker holds no
effect diagnostic of its own; the rule above is enforced where the declaration is known, in the desugar, and
this arm is the fallback for what the elaborator does not classify.

Beside it live two classes that are **not** effect machinery and must not be deleted as such (measured — see
§12): `EffectLifter`'s carrier recognition (`effectCarrierSplit`, `mustPureWrapBeforeUnify`) and
`check/CarrierKindChecker` (`recordCarrierMetas`, `verifyCarrierKinds`). `verifyCarrierKinds` is the only
thing rejecting a `[F[_]]` binder instantiated at a fully-applied proper type; with it off, a wrongly-kinded
program silently compiles. They are a **kind system living next door**, not effects.

The **compile track** keeps its mid-spine default ladder and deferred slots *by design* (`Track.Compiler`,
`Checker.resolveDeferredSlot`): an inline guard's carrier is inferred and pinned to `Either[E]` post hoc —
the sole live reader of the `Unifier`'s higher-kinded-meta record.

## 4. The derivation, as a spec

A `Row` is a set of effect-ability entries (production: multiset of *(ability, type-args)*). Judgments are
per definition, over the operator-resolved body, reading only *declared* information. This is the live spec
of `row/RowChecker`:

- **value-of**: `row(literal) = row(λ) = row(under-applied ref) = ∅`;
  `row(saturated call f(a₁…aₙ)) = riding(f) ∪ ⋃ᵢ contrib(aᵢ)`;
  `row(applied λ)` (the block/`val` desugar) `= row(bound arg) ∪ row(body)`.
- **riding**: `riding(f) = declared(f) ∖ capturedByStack(f)`, where
  `capturedByStack(f) = (declared(f) ∖ ambient) ∩ dischargeable`. Effects the ambient does not provide and a
  discharger in scope can consume land in that call's own carrier stack, for a consumer to discharge — not
  on this definition's row. **This must stay the mirror of `RowElaborator.carrierAt`**: a verifier that
  counts what the elaborator has just routed elsewhere reports a leak for correct code. A capture nothing
  discharges is still rejected — by the checker, against the declared return.
- **contrib** at slot *i*: `contrib(aᵢ) = (row(aᵢ) ∪ latent(aᵢ)) ∖ pinnedEntries(f, i)` — the subtraction
  applies only when slot *i* is pinned; every non-pinned slot, strict *or suspended*, contributes
  identically. A non-pinned slot that `fixesCarrier` (§3.3) still contributes, but its entries are recorded
  as *undecided* and drop out of the leak.
- **latent**: `latent(λx.e) = row(e)`; `latent(under-applied ref f) = declared(f)`; else `∅`. One latent row
  per arrow; a function-valued argument's latent row joins the receiving call conservatively ("the callee
  may run it").
- **declared**: open-row return entries ∪ the effects constrained on the signature's carrier binders
  (machinery excluded); an effect-ability method's contribution is its own declared row.
- **check**: `row(peeled body) ∖ undecided ⊆ declared`, bounded by the three cases in §3.3.

`RowElaborator.performs` reads the same derivation against the **region's** row, not the definition's:
inside a pinned capture the region carrier provides that slot's pinned entries, and inside a run-boundary
argument it provides everything. Without that widening an effectful argument inside a `catch` capture stops
binding.

**Suspension is row-neutral.** Whether a slot is strict or declared-suspended changes only *when* the effect
runs — elaboration's business — never whether the caller must declare it. The *only* slot mode that touches
derivation is pinned capture, as subtraction.

**Two same-ability entries** at different arguments are two entries; at identical arguments they
deduplicate. Two such entries with a non-pinning handler have no canonical order and stay a diagnostic
asking for a pin. First-order abilities (`Show`) are distinguished from effect abilities by the missing HKT
binder. Only the *outer* layer of a carrier stack is payload-applied — shape knowledge that belongs at the
desugar, not at a consumer.

**A definition returning the platform's concrete run carrier** (`def main: IO[Unit]`) is the *nominal run*
spelling of a boundary, where the concrete carrier captures the whole row. The run-carrier head is read off
the **registered run boundary's own first parameter** (`runMain(io: IO[A])` ⇒ `IO`), never guessed from a
name.

## 5. Standing rules

These bind every future change to the effect system.

1. **Where decisions live.** Part I states the decision. An appendix, a commit message or a measurement note
   that changes a decision must change Part I, never amend a rule in place elsewhere. (This exists because a
   *reversal* once read as a refinement for six days.)
2. **Stop on conflict; do not route around it.** If Part I's rules appear to conflict with each other, with
   the tree, or with a measurement — or if a step finds itself *narrowing*, *bounding*, *deferring*,
   *approximating* or *exempting* one of them — **stop and surface it.** Do not land the workaround and
   record it as a corollary. The tells, in this project's own vocabulary: "bounded staging", "the corpus
   forced", "narrowed to ~nothing", "a small local concession", "keeps the shipped idiom", "it usually
   holds". A conflict is a decision for Robert, not a judgement call in flight.
3. **No carrier inference.** No carrier metavariable, no join, no lattice, no ordering-sensitive slot
   decision. That is the historical bug class — carrier theft and premature commitment — and it is
   prohibited. The same prohibition covers rows if a row ever enters a type (§9.9 restates it for v6): rows are declared and
   written, never solved.
4. **Carrier-ness by tag, never by name or shape** — except in the one inverter (§3.7).
5. **The whitelist** (§3.2) is closed. No sibling-expression rule.
6. **A component may read solved metas and splice rewrites; it may never run inside unification, never
   retract a solution, never grow an ordering arm.** A shape that genuinely needs mid-drain resolution is a
   stop-and-redecide signal, not a licence for a mid-flight arm.
7. **A rule invented so that one idiom elaborates is a rule shaped by that idiom, whatever its stated
   generality.** Delete the rule; fix the declaration.
8. **Fail-safe direction.** Every bound, every deferral and every declination must be able only to *withhold*
   a permission, never to grant one silently.

## 6. Testing — the carrier is the injection point

**Status: adopted and working**, for application-owned abilities (`examples/src/EffectsFakeCarrier.els`),
for stdlib effects (`examples/src/EffectsFakeConsole.els`) and for a small framework built on it
(`examples/src/EffectsTestFramework.els`).

Production code that declares an effect row is already polymorphic in its carrier; it names no carrier, so
it commits to no interpretation. Which interpretation it gets is decided by what its carrier binder is
instantiated to, and that is decided by whoever runs it — in production the synthesized entry point
instantiating `main` at `IO`, in a test the test:

```eliot
data Session[A](runSession: Function[Pair[String, String], Pair[A, Pair[String, String]]])
implement Effect[Session] { … }
implement Terminal[Session] { … }

def greetTranscript: String = second(second(runSession(greet)(Pair("Bob", ""))))
```

Three properties fall out of the design rather than being added for testing. **The fake cannot cheat**: a
pure test carrier has no `Suspend` instance, and `Suspend` is the only route to a native side effect. **The
fake is confined to the test program**: compilation is whole-program monomorphization from `main`, so a test
binary is a different program and coherence is a per-program question. **The orphan rule is satisfied for
free**: the test module declares the test carrier, so the instance is legally colocated.

Two mechanisms had to be extended to reach it, and both are now load-bearing parts of the design:

- **Constraint-aware declination** (`AbilityImplementationProcessor.constraintsSatisfied`). Candidate
  selection is structural, so the jvm layer's `implement[F[_] ~ Suspend] Console[F]` matched *every* carrier
  and a fake `Console` was a second surviving candidate — ambiguity, decided before the constraint could
  fail. A candidate whose `~` constraints have no implementation at the matched bindings now **declines**.
  Every step is fail-safe *towards keeping*, so it can only remove a candidate that could not have worked; a
  constraint probe that leads back to a resolution already in progress is answered "satisfied" off
  `activeFactKeys`, never demanded. Effect accounting reads an implementation's ability off its own resolved
  declaration (`Qualifier.AbilityImplementation`), not off the implementation reference's module — the
  orphan rule admits two placements and a fake takes the second.
- **`RowChecker.fixesCarrier`** (§3.3), so the pre-mono verifier does not charge the harness for the effect
  it fakes. Before it, the identical harness was accepted or rejected purely on the shape of its own return
  type.

**What a fake gets, and what it does not: lifting.** A real effect instance is **carrier-polymorphic**
(`implement[F[_] ~ Suspend] Console[F]`), so it applies at any *stack* whose base can suspend — and
`Suspend` lifts through all five stdlib carriers. That is why real effects appear to compose freely: the
`Suspend` constraint is doing the work an mtl `lift` would. A fake instance is **monomorphic at one concrete
carrier** (`implement Console[Recorded]`) — which is exactly what makes it uncheatable — and therefore gets
no lifting at all. So a fake carrier can host any number of abilities, but the moment a stdlib control
carrier is stacked *over* it, resolution fails at the stack:

```
No ability implementation found for ability 'Transcript' with type arguments [{Throw[AssertionError] | Recorded}]
```

The strategy that follows, and the one the framework in `eliot-test` uses, is **do not stack over a fake**:
give the fake carrier its own instance of every ability the test body needs — assertions included
(`implement Throw[AssertionError, Recorded]`) — so everything rides one carrier and nothing has to lift.
That is what makes §7.7's interleaved case work. A missing cell *can* be hand-written
(`implement[E, G[_] ~ Transcript & Effect] Transcript[ThrowCarrier[E, G]]` resolves and runs), but it is one
instance per (ability × carrier layer) — the n² matrix the stdlib's own cross-lift comments name. Whether
that matrix should ever be derived rather than written is **not** an open decision here: nothing in the
testing strategy needs it, and the no-stacking answer costs nothing.

**Alternatives, deliberately kept documented.** `Dep[X]` + `provide` is the supported route when you are
willing to state the seam in the signature — it changes production signatures, so it is the fallback, not
the strategy. **Swapping the platform layer** (drop `jvm/eliot` from `--path`, put a test layer there)
substitutes everything at once and suits whole-program integration tests; a test layer added *beside* `jvm`
collides at the merge. **`where` is the wrong tool**: a guard can only make a candidate decline, would have
to be written on the library's instance, and needs a predicate no guard can express (guards see type
arguments, not the instance environment).

**This strategy is what ships; it is not the final form.** Part II (§9) replaces the carrier as the injection
point with an implementation *name* bound by `with` — every limitation below (the lifting wall, the region rule,
one interpretation per type argument) is a symptom of choosing an interpretation by instantiating a type, and
disappears with it.

## 7. Live limitations

Each is stated, fail-safe, and either has a plan entry or is a deliberate trade.

1. **A handler whose effects enter via a declared carrier-typed parameter must return a carrier-headed
   type.** That carrier is caller-chosen, so no declaration determines it.
2. **`Suspend`-riding effects cannot be pinned** (`{Console | X}` fails loudly at resolve — no
   `ConsoleCarrier`) **nor supplied** in a parameter row; the two are the same limitation and the same
   diagnostic. → **D4**.
3. **A pure actual at a pinned slot is rejected.** Pinned captures never boundary-wrap, which is what
   preserves the curated val-bound-discharge diagnostic. Rule 2's "pure arguments fit" is about *suspended*
   (open-row) slots, not pinned ones. Storing a pure value in a computation field must be written
   `Box(pure(x))`, which needs `import eliot.carrier.Effect`; declaring the field at its payload type is
   usually the better answer.
4. **An inline effectful call at a pinned slot is rejected unless it goes through a declared row.**
   `Task(raise("boom"))` reports the undeclared `Throw`, while `def bad: {Throw[String]} String =
   raise("boom")` + `Task(bad)` compiles — the elaborator instantiates a pinned row's ability arguments from
   the argument's **declared** row.
5. **A lambda body at a rowless arrow slot does not get its own pure region.** → **D5**.
6. **Rule-4 violations are diagnosed twice, unequally.** → §10.3 A3 (formerly W2).
7. **A fake run needs a region with no ambient carrier of its own.** Inside a pinned region the ambient
   carrier *is* the pinned stack, and the elaborator writes every carrier-generic callee at the region's
   carrier — so a fake run written *inside* a pinned body is written at the pinned stack, not at the fake
   carrier. A carrier-generic value can only be instantiated at a foreign carrier in a region with no
   ambient carrier of its own. W3 (below) removed the constraint; v6's `with` (§9) subsumes it.

   This was previously stated as "a test is run-then-assert, never interleaved", which is **wrong** and is
   corrected here: interleaving assertions with faked effects works today, and `eliot-test`'s
   `test/eliot/test/example/` is the worked example. What made it work was not stacking — a pinned
   `{Throw[AssertionError] | Session}` does fail, for the two reasons the retired L3 note recorded — but
   giving the *fake carrier itself* a `Throw[AssertionError]` instance, so assertions ride the same carrier
   as the faked effects and nothing has to lift (§6).

   **Since W3 shipped (2026-09-04) the region rule is opt-out-able rather than binding.** A slot declared
   `{| Recorded} A` (§2.3) is a capture, so the elaborator writes nothing into it and the checker
   instantiates the body at the declared carrier — even at a call site inside a region of its own. A faked
   case therefore costs **no** helper definitions: the run and a multi-statement body may both be written
   inline at the `in` site. The rule above still governs every *untagged* slot, which is why it stays
   stated. Two halves were needed and both are in: the tag, and the block peel (§3.3) so the harness is not
   charged for the effect it fakes.
8. **Rows are sets of abilities**, so a definition mixing a faked run with a real leak of *the same* ability
   defers that entry and the user gets the post-mono `Type mismatch` at the harness body instead of the
   located effect-vocabulary message. The program is still rejected; only the diagnostic degrades, and only
   in that one mixed shape.
9. **Rule 3 has no check of its own for a `data` field typed by the data's *own* open carrier binder**
   (`data Box[F[_]](action: F[Unit])`). The open-*row* field is rejected; this shape is not. Has no subject under v6 (§10.3 A3, formerly W4).

---

# Part II — The plan: effects are abilities, and an implementation is a name (v6)

**Status (2026-09-08): decided, including the implementation (§9.4).** An `implement` block stays what it is
today, statically resolved method bodies. No ability becomes a record type, no implementation is ever a
runtime value, and no row becomes a
runtime parameter. A **named** `implement` mints an addressable *name*; `with` binds that name for the calls
lexically inside its subject; and the binding joins the **monomorphization key**, so `greeting` under
`recordingConsole` is its own instantiation and every operation call erases, from day one. Part I stays the
authoritative description of the tree *until the flag day in §10 lands*; nothing in Part I is amended in
place before then (standing rule 1). This part is the design that replaces it, the reasoning behind it in
condensed form, the implementation steps, the decisions still open (§11) and the list of what is closed
(§12). Every entry marked **decision** is Robert's; what §11 lists is what is still his to decide.

**The model in one sentence (decision, 2026-09-08): a row entry is a compile-time parameter, and `with`
applies its argument.** A row entry behaves exactly as a type parameter does — the caller fills it, it joins
the monomorphization key, it is erased — with the implementation itself as the parameter instead of a type
it is derived from. What is *not* a type parameter about it is how the implicit case is filled: not by
inference but by lexical forwarding, the enclosing declaration's own binding, and never a solver.

## 8. How this plan is run

**Decision protocol.** Standing rules 1 and 2 (§5) govern. Nothing here is a judgement call to be made in
flight, and a step that finds itself narrowing one of the rules here stops instead of landing.

**The gate**, for every pre-flag-day step: `./mill __.test` green, all example programs carrying a `main`
compile, and every example jar `md5sum`-identical to the pre-change build. Byte-identity is a **safety
oracle, not a hard gate**. The sweep is `scripts/example-sweep.sh` (§10.1 step 9); its `jar.md5` lines are
this gate.

**The gate for the flag day itself** is different, because the output legitimately changes wholesale:
byte-identity is replaced by **behavioural identity**. Before the change, run every example jar and record
its standard output and exit code — that recording is `.v6/baseline.txt`, made by the same script (§10.1
step 9); after it, the same sweep must produce the same transcript. Plus: `./mill __.test` green; the
fake-carrier examples (`EffectsFakeCarrier`, `EffectsFakeConsole`, `EffectsTestFramework`) and the two
integration test classes express the same tests **without minting a carrier type**; `eliot-test`'s
single-word case (`"…" should "…" in mocked { … }`) still reads as one word; and the seam test finds every
binding at the `WovenValue` seam resolved to a known implementation. The sweep also records each jar's size
and its bytecode instruction count — over every class, and over the module's own class — and the flag-day
commit states the difference. Since specialisation is the mechanism (§9.7) and not a follow-up, the
expectation is no regression; a regression is a finding to explain, not a cost to accept.

**The flag day did not land as one change** (§10.2): the tree stops building the moment the desugar stops minting
carriers, and there is no green checkpoint until the primitives exist, so it is landing as five commits — each
stating that the tree does not build. The gate itself is unchanged and is read at the end, not per commit.

**The method, when the question is "is this still load-bearing?"** — reuse it rather than re-inventing it:

- **Arm-liveness tracing, not inspection.** A temporary env-gated tracer with a `fire(arm, sample)` call on
  every arm under consideration; run the whole gate *and* a compile of all examples; only delete zero-fire
  arms. Trace at **outcome** granularity — a router's entry count is mostly routing.
- **Switch it off, part by part.** An env-gated bypass answers in behaviour rather than in meta solutions.
  Gate each part **separately**, never only all-at-once: an all-off run once said "43 failures, delete
  nothing" while the per-part runs said one part costs 1 test and another 36, which was the whole finding.
- **An examples-only audit under-reports**, because elaboration is demand-driven: disabling *every* effect
  arm still compiled all 45 examples to byte-identical jars. The jvm end-to-end suites are what separate
  arms.
- **A firing arm is a question, not a verdict.** Having localised a live part, find the *one* arm that
  decides and ask what the elaborator would have had to know.
- **Measure twice, before and after.** One deletion's first cut looked like an improvement while dropping a
  fail-safe.
- **Tracer gotchas**, each of which cost time at least once: mill **prefixes every forwarded line with a
  worker id**, so never anchor a grep; sample keys must carry the range **end** as well as the start
  (elaborator-generated nodes reuse an argument's `Sourced`) and, for an argument, its spine **head**; and
  the cache (`target/.eliot-*`) must be deleted before every run or the pipeline replays facts and the trace
  comes back empty.

## 9. The model — an implementation is a name

### 9.1 In five sentences

An **ability** is what it is today: a marker plus one body-less method def per operation, and an
**implementation** is a block of concrete method bodies resolved at compile time. An **effect** is an
ability whose implementation is never searched for at a call site: a use must be lexically covered by a
declaration — the enclosing def's row, a slot's row, or a `with` — and the binding travels from `main`
inward through those declarations until the synthesized entry point binds the platform's instance.
A row entry is a compile-time parameter of the def, and `with` applies an argument to it — on an expression
in a body, on a slot's type in a signature — so a test's fake is one named `implement` in the test module and
needs no type, no colocation and no coherence question. Rows stay the
user surface and the verifiers' vocabulary — `derived ⊆ declared` per definition — and never enter a type:
a row-typed parameter or field is a thunk whose calls were bound where it was written. There is no carrier:
sequencing is strict evaluation order, a non-local exit and a threaded value are two platform-private
primitives no Eliot body can express, and specialisation is the monomorphizer keying on the row
arguments written at each reference, exactly as it keys on type arguments today.

### 9.2 What was decided, and why

- **The problem was never "instances may live in only two places".** §6 lets a test submit an interpreter,
  but only by conjuring a *type* and hanging instances on it — dependency injection routed through the type
  system. Every §6/§7 limitation is a symptom: a fake is monomorphic at one carrier and gets no lifting (the
  n² wall); a fake run needs a carrier-free region or the W3 tag; one type argument decides the
  interpretation of *every* effect at once. Named precisely, the carrier does three jobs — sequencing
  representation, discharge-stack representation, selector of interpretation — and the third is the misfit.
- **Decision: the interpretation is chosen, never searched, and it is chosen at compile time.** The nearest
  neighbour is algebraic effects and handlers (Koka, Effekt, Unison's abilities), where tail-resumptive and
  abortive operations — every effect Eliot has — compile to plain calls; Eliot additionally fixes the handler
  statically, so the call *erases*.
- **Decision: an implementation is a name, not a value.** It is not storable in a `data` field or a `List`,
  not chosen by a runtime `if`, not returned from a function, and cannot close over runtime data — a handler
  needing runtime data gets it through an effect. Two things forced this beyond simplicity. First,
  **identity**: `Qualifier.AbilityImplementation(name, pattern)` keys an implementation by its ability *and
  its type-argument pattern*, and the carrier is where today's discriminator lives (`Console[F]` versus a
  test's `Console[FakeCarrier]`). With `Console` nullary, every implementation of it has the same empty
  pattern and they all overlap; the carrier's third job was also serving as the implementations' primary
  key, and a name is exactly its replacement. Second, the record encoding is **unrepresentable** in the
  checker (§12, the step-6 measurement).
- **Decision: abilities and effects are one mechanism, differing in one bit.** A `~ Show[T]` constraint and
  a `{Console}` row entry are both declarations that bind an implementation for the calls beneath them.
  They differ in whether a use with **no** covering declaration may fall back to the two-site search at
  its own ground arguments. For an ability it may — a ground instance is a *fact* (there is one `Eq[Int]`),
  so search says nothing wrong. For an `effect` it may not — an implementation is a *choice*, so the
  declaration says something. That bit is why effects are **declared** as such: without it
  `def greeting(name: String): Unit = printLine(…)` would find the jvm `Console` at its first ground use and
  compile with no row. Removing the bit in the other direction ("every ability declared, none searched")
  is closed (§12): it buys nothing over a `~` constraint and costs `{Eq[Int], Combine[String], PatternMatch[Shape]}` on
  most monomorphic code, and the search cannot be deleted in any case, since the compile track dispatches
  `Meta`/`Numeric`/`PatternMatch`/`TypeMatch` from machinery with no call chain.
- **Decision: forwarding is lexical, by declaration.** A binding reaches a call only through a declaration
  the code in between wrote. The alternative — summing the abilities used in each monomorphized sub-graph
  and letting a `with` override transitively — is dynamic scoping resolved at compile time: the carrier's
  third job in a new costume.
- **Decision: the platform's defaults are found by the ordinary two-site search where the declaration chain
  ends.** For an effect that is the synthesized `main` and a slot whose row *supplies* the entry (§9.4);
  never an undeclared use inside a body.
- **Decision: three platform-private primitives, no lowering pass** (§9.6). A non-local exit and a threaded
  value are exactly what a strict pure core cannot express and what every target has: a jump, a register, a
  loop.
- **Decision: resumption is tail-resumptive or abortive, nothing else.** An operation either returns (a call)
  or exits to the frame that installed it. Multi-shot and non-tail resumption — generators, async,
  coroutines — are out.
- **Decision: a stored computation's binding is decided where it is constructed, not where it is run.**
  Deciding the handler before storing is unambiguous and easier to understand, and losing first-classness
  is acceptable because it is simpler.
- **Decision: `with` is written almost nowhere.** Most code fixes nothing: a def declaring `{Console}` receives
  its binding from its caller, up to `main`. That chain is what makes a fake possible — `greeting` never said
  which console, so a test may say. `with` is therefore written in a test to bind a fake, and at a call that
  declares an ability to override its default (`sort(xs) with reverseOrd`); storing a computation needs none,
  since the thunk freezes whatever the constructing def received. A `with` in production code is the same
  mistake as a hard-coded dependency.
- **Decision: one `with`, two positions.** `subject with name` where the subject is an expression in a body or
  a type in a signature — the same split as `f(x)` and `List[Int]`, both application under the types-are-values
  cornerstone, parsed by the expression parser and the restricted type parser. The ability is read from the
  name's declaration, never repeated beside it. A slot's `with` is the one way a callee decides the binding
  of calls it cannot see, since an actual's calls are monomorphized in the caller; it fixes the actual's
  compile-time parameter from the signature exactly as `body: List[Int]` fixes a type argument.
- **Decision: a row entry is a real binder, phantom.** One generic binder per row entry and per `~`
  constraint, occurring in no parameter or return type, written by the desugar at every reference and carried
  in `typeArguments` (§9.4). This reopens, in form only, the §12 entry that closed "a handler as a type": what
  defeated that draft was a lowering pass and in-type binders, neither of which a phantom binder has.
- **Decision: purity is decided by the evaluator, not declared** (§9.7).

### 9.3 The surface

**Unchanged:** every definition that declares a row and calls operations. Direct style, blocks, `val`, the
`.` pipe, `if`/`match`, the four user rules minus their carrier vocabulary.

```eliot
def greeting(name: String): {Console} Unit = printLine("Hello, " ++ name ++ "!")

def swap(next: String): {State[String]} String = {
   val old = state
   putState(next)
   old
}
```

**An effect is an ability declared with the `effect` keyword and no carrier binder** (decision). The
`[F[_]]` existed only because something had to be the monad. An `ability` is unchanged. **A member's row
lists what it performs beyond the ability it belongs to** — for `effect` and `ability` alike. Membership in
the block already says a member needs that binding, exactly as `show` inside `ability Show[T]` does not
repeat `~ Show[T]`; so `{Console}` on a member of `effect Console` is not written, and writing it is
rejected as a second spelling of one fact. A member's row is real where it names *other* effects —
`effect FileSystem { def readAll(p: Path): {Throw[IoError]} String }` performs `Throw`. A function that
needs no such binding is not a member: it lives outside the block as an ordinary def with its own row, as
`updateState`, `orRaise` and `orAbort` do today beside the primitives `state`, `putState` and `raise` inside.
§3.6's decision — effect-ness is declared, never read off a method's shape — is kept; the declaration moves
from the method to the block, where an ability's already is, and a constructor class is simply an `ability`.

```eliot
effect Console {
   def printLine(s: String): Unit
   def readLine: Option[String]
}

effect Throw[E] {
   def raise[A](err: E): A
}

ability Show[T] {
   def show(t: T): String
}
```

**An anonymous `implement` is a default.** An `implement` block in one of the **two sites** — the ability's
module or the type's module — is the **default** for its pattern, subject to the existing coherence and
`where` rules, and is what a declaration with no named implementation binds. The platform's
`implement[F[_] ~ Suspend] Console[F]` becomes the default instance of the nullary `Console`, in the same
module path the jvm layer already uses:

```eliot
implement Console {
   def printLine(s: String): Unit = printLineInternal(s)
   def readLine: Option[String] = lineOrNone(readLineInternal)
}
```

**A named `implement` is never a default** (decision). It may live anywhere, is never searched, is not
checked for overlap, and its clauses may declare rows — a clause row is what the implementation performs
beyond its ability, charged wherever the name is bound. It takes no parameters and closes over nothing: what
it needs at runtime it asks an effect for. A test's fake is therefore one declaration in the test module:

```eliot
implement recordingConsole: Console {
   def printLine(s: String): {Writer[String]} Unit = tell(s ++ ";")
   def readLine: Option[String] = None
}
```

**`with` binds a name for its subject** (decision): infix, subject-first, at the loosest precedence,
left-associative, so `xs.sort.render with reverseOrd` applies to the whole chain and `c with a with b` is
`(c with a) with b`, an inner `with` for the same ability shadowing the outer within its subject. It works on
an ability exactly as on an effect, and on a slot's type exactly as on an expression:

```eliot
def transcript: String = written(greeting("Bob") with recordingConsole)
def demo: Pair[String, String] = runState("first", swap("second"))
def safe: Configuration = parse(config) catch (_ -> emptyConfiguration)
def value: Option[String] = runAbort(allowed)
def sorted: List[Int] = sort(xs) with reverseOrd

def mocked(body: {Console, Throw[AssertionError]} Unit with mockConsole): {Throw[AssertionError]} Unit = …
data Suite(cases: {Console} Unit with recordingConsole)
```

The slot form reads "this slot's computation, run with mockConsole": the actual delivered there has its
parameter applied by the callee's signature, and the caller writes nothing. `with` **inside** a row
(`{Console with mockConsole}`) is rejected: it would put an ability on the left instead of a subject, a
second grammar repeating a pairing the implementation already declares. A slot doubling many effects writes
the chain; a name for a set of implementations or of effects is deliberately not added (§12, "not now"). A
`with` is accepted on a parameter's or a field's type only, not on a def's own return row, which would be a
second spelling of `with` around the body.

`catch`, `else`, `runThrow`, `runAbort`, `runState*` and `written` **keep their names and signatures** in
the base — a `G[_] ~ Effect` binder and a `G[…]` return become the plain payload — and lose their bodies
there (§9.6). None of them binds a name: a control effect has one implementation per platform (§9.6), and a
discharger only installs the frame it exits to.

**A stored computation is a row-typed field with no tail**: `data TestCase(body: {Throw[E]} Unit)`; the
`| Id` pin disappears. The field is a thunk, its calls are bound where the constructor is applied, and the
field's row is the declaration that covers them there (§9.4). Running it later runs it as bound; a `with`
applied to it later is an error, not a rebinding (§9.5).

**What disappears from the user's world:** the whole `eliot.carrier` package (`Effect`, `Suspend`,
`flatMap`, `pure`, `map`, `suspend`); every `<Ability>Carrier` type and `run*Carrier` accessor; the
cross-lift instance matrix; `~ Effect` and `~ Suspend` on definitions; `Id` and `runId`; pinned tails; the
`{| Recorded}` capture tag; the fake-carrier recipe; the "a discharger must be called directly" rule (§3.5) —
a thunk is a plain value and passes through the dot's plain `T` as the value it is. Rule 2's "carrier-typed
position" and rule 4's four carrier namings collapse to one predicate: a slot either has a row and covers
computations, or it does not and is a payload.

### 9.4 The core desugar — a row entry is a phantom binder, written at every reference

The tree already has almost all of this. Today `EffectSugarDesugarer` mints one carrier binder per signature
(generic 0, `auto`) and turns each row entry into a `~` constraint on it; `RowElaborator` writes that binder as
an explicit leading type argument at every reference, never leaving it to inference; the mono key is
`(vfqn, typeArguments)`, so the carrier is already what specialises a `{Console}` def per instantiation; and
`TypeStackLoop.bindTwinBinders` tolerates a binder the signature never mentions. **Decision:** keep that
discipline and change what the binder is.

1. **resolve** — `with h` resolves `h` in the ordinary dictionary to an implementation `ValueFQN`; only that
   FQN flows onward. **Not a string carried downstream to be searched by** (§12): that would give a `with`
   that ignores import scope, cannot be shadowed and is decided by hash order. The keyed lookup mirrors
   `ValueResolverScope.getAbility` exactly. `Qualifier.AbilityImplementation` grows an optional impl-name
   component: identity becomes (ability, pattern, name); anonymous defaults keep today's identity.
2. **desugar** — `effect` and `ability` produce the marker and the body-less method defs as today, with no
   carrier binder; an `implement` produces the qualified method bodies as today. A def's row entries and `~`
   constraints each become **one phantom generic binder**: in the generic list, in no parameter or return
   type, and so never in any type (rows never flow into types, §3.3 unchanged). `EffectRow` stays the
   declaration metadata the renderers and verifiers read. A row on a parameter or a `data` field **thunks**
   (`{Abort} T` ⤳ `Unit => T`, the existing suspension rule).
3. **the write** — at every reference the desugar writes each phantom binder's argument, by the resolution
   order below, as today's leading prefix. Its value is a ground value the checker already carries:
   - an implementation FQN — `greeting[recordingConsole]("Bob")`;
   - that FQN applied to its own clause-row bindings — `greeting[recordingConsole[cellWriter]]`, since a named
     implement with a clause row is itself parameterised by what its clauses perform; transitivity is an
     ordinary ground-value tree, not a fixpoint;
   - or the **`Default`** marker, "search at ground arguments" — what every `~` constraint gets unless a
     `with` says otherwise, and what an effect gets only where its chain ends (§9.5).
   A binder is never left to a meta: an unwritten one is a desugar defect, and the checker rejects an
   unsolved phantom rather than defaulting it to `Type` as it does for a leftover today.
4. **`AbilityResolver`** — reads the argument. An implementation value is used **directly** — no structural
   match, no `where` filter, no coherence question; `Default` goes to today's two-site search, unchanged.
   `with` **replaces** the search rather than parameterising it, which is what makes a named implementation
   free to overlap a default without being checked against it. `MonomorphicValue.Key` is untouched: the
   binder is in `typeArguments`.

**Where a binding comes from — the resolution order.** For every operation call, and for every call to a
def that declares the ability, walk outward lexically:

1. the nearest enclosing `with` for that ability;
2. the enclosing def's own phantom binder for it — a **received** binding, filled by the caller;
3. for an actual at a row-typed slot (a parameter, a `data` field), the slot's row: an entry the slot
   **supplies** binds the slot's `with` or, unnamed, `Default`; an entry the callee's own row already has is
   not supplied and the walk continues into the caller's scope (Part I's supplied-versus-rides rule, §2.2,
   unchanged);
4. for an ability, `Default`; for an effect, the "performs but does not declare" error at the call — except
   at the synthesized `main`, where the chain of every effect ends and `Default` is written (§9.5).

**The walk crosses a lambda boundary if and only if the lambda's slot has a row** (decision, D5); `{}` counts
as a row. That is the whole of rule 4 for lambdas: a lambda at a rowless arrow (`map`'s `f: A => B`) may
bind and discharge locally — `xs.map(s -> s.orAbort else "")` is accepted, since `else`'s slot supplies the
`Abort` and the walk stops inside — and may not reach the enclosing def's binders, so anything it performs
and does not discharge is the error at the lambda. A `{}`-slotted or rowed lambda body sees the enclosing
declarations, as today. There is no separate "is a lambda a region" rule.

**Both verifiers keep their vocabulary.** The pre-mono `RowChecker.verifyRow` becomes a **scope check**: an
operation or a rowed callee needs a covering declaration, and the only places one can come from are the
enclosing def's row or constraints, an enclosing `with`, or a slot's row. That check is complete before
monomorphization, since nothing about it is instantiation-dependent, and it owns the diagnostic for a
`with` whose subject contains no lexically covered use and no call to a declaring def. The post-mono
`EffectAccountingProcessor` stays the codegen precondition through the flag day: under names "performs X"
is "an operation reference resolved through a **received** binder for X", which is read off the value's own
type arguments, so the check is that set ⊆ the declared row (D7).

### 9.5 Semantics

- **An operation call** is a call to the bound implementation's method; the clause computes and returns,
  and evaluation continues after the call. **Effects run where they are written** is strict application,
  nothing more.
- **Forwarding is lexical (decision).** A def's declared rows and constraints are the ambient bindings for
  its body, received from its caller. `expr with h` extends or overrides the ambient for `expr`; a nearer
  `with` rebinds. Every call inside a region that declares an ability receives the ambient binding for it.
  Every one of these is answered from the enclosing def's signature and the enclosing `with`s: **there is no
  graph to sum.** A ground `printAll(xs: List[Int])` that declares nothing resolves `sort`'s `Ord[Int]` to
  the default at that call, and `printAll(xs) with reverseOrd` is an error because `printAll` has no
  `Ord[Int]` declaration to receive it in — the same discipline the row imposes today, and what makes a
  def's behaviour readable from its own signature.
- **Where `with` may be placed.** Around any expression that lexically contains the use, or that calls a def
  declaring the ability through a `~` constraint (`greeting[T ~ Show[T]]`). In one sentence,
  **`with` reaches every use inside its subject's own text, and crosses a def boundary only through a
  declaration.** A `with` whose subject does neither — a bare rowed parameter, a `data` field, a value
  received through a plain generic — is a hard error naming the fix (write it on the slot's type, §9.3),
  never a silent no-op.
- **A clause row is charged at the binding site.** `greeting("Bob") with recordingConsole` performs
  `Writer[String]` there, because the name's clauses declare it; the scope check reads that from the
  implementation's declaration, and the binding for it comes from the same resolution order.
- **Two families, and only one is rebindable.** The **control effects** — `Throw`, `Abort`, `State`,
  `Writer`, `Dep`, `Inf` — have exactly **one** implementation per platform, over its private primitives
  (§9.6); a discharger installs the frame that implementation exits to or threads through, and **nesting
  order at the run site decides interaction**: `runState(s, runThrow(c))` versus `runThrow(runState(s, c))`
  is the difference between state surviving a `raise` and not, written by the user where the frames are
  installed, with no canonical form to fix. The **interpretation effects** — `Console`, `Log`,
  `FileSystem`, `Process`, `Environment` — and every ability are what `with` and a naming slot are for. The
  families are a description, not a bit in the language: nothing keys on it.
- **Storage.** A row-typed `data` field is a thunk bound at construction; the field's row is the
  declaration that covers its calls there. Storing it, passing it through a plain generic, and running it
  later are ordinary. Nothing captures an escape, so nothing can dangle: a frame is installed by a
  discharger's call and left when that call returns or is exited.
- **Instantiation.** A definition is monomorphized per type arguments, of which the phantom binders are
  some (§9.4). Every binding is decided from `main` inward, so at the `WovenValue` seam every operation
  reference is resolved to one implementation — the seam test. A def declaring an effect it never uses is
  still instantiated per binding, as a `{Console}` def is per carrier today; deduplicating identical bodies
  is an optimisation, not a rule.
- **The run boundary (decision).** The synthesized entry point is ordinary code: it reads `main`'s row and
  binds, for each entry, the instance the **two-site search** finds, and installs the frame for a control
  effect. A miss is an error at the boundary naming the effect and the fix (discharge it).
  `SyntheticMainSourceProcessor` shrinks; `RunBoundaryFunctions` is deleted. Only the platform layer's
  defaults serve there: an effect with none reaching `main` is that error, and a user-declared boundary
  default is not a feature (§12, "not now").
- **`Inf`** is the platform's default instance of the `Inf` effect (`forever` over the loop primitive), and
  stays the one effect that reaches `main` and is handled by the platform alone.
- **"The fake cannot cheat" survives without `Suspend`.** A user module cannot declare a native, and the
  platform's natives and primitives are private to its layer, so a test's implementation reaches I/O only
  through effects its own clauses declare in their rows — which are charged, and bound, at the binding site.

### 9.6 The three primitives — where the two control-flow transformations live

A resuming clause is a call and needs nothing. A finishing clause is a **non-local exit**; a stateful
implementation needs a value **threaded through calls that never mention it**. Neither is expressible as a
def in a strict pure core — today both are expressed by the transformer instances (`ThrowCarrier`'s
`flatMap` is the exit, `StateCarrier`'s the threading), which is the whole reason the carrier existed.
**Decision:** they are three **platform-private leaves**, one per target, and nothing else:

| primitive | shape | jvm | microcontroller | compile track |
| --- | --- | --- | --- | --- |
| escape | `escape[E, A](body: {Exit[E]} A): Either[E, A]`, with `effect Exit[E] { def exit[A](e: E): A }` — abortive, never re-entered; the frame is the machine stack | an exception, one class per instantiation | a status flag and a jump (A2) | an evaluator intrinsic |
| cell | `withCell[S, A](initial: S, body: {Cell[S]} A): Pair[A, S]`, with `effect Cell[S] { def read: S; def write(s: S): Unit }` — scoped to one call, saved and restored around it | a static field per instantiation | a register | an evaluator intrinsic |
| loop | `foreverInternal` as today | `while(true)` | the super-loop | never runs (`Inf` is stuck) |

The exact shapes are the platform's to fix at F5; what is decided is that there are three, that they are
private, and that the control effects' single implementations are written over them: `Throw[E]`'s `raise`
is `exit`, `State[S]`'s `state`/`putState` are `read`/`write`, `Writer[W]` appends to a cell, `Dep[T]`
reads one. The frame an operation reaches is the **nearest enclosing** one of its instantiation — the
machine stack's discipline, inside the leaf and nowhere else (§12, "a runtime handler stack").

**They are private, and the dischargers are therefore abstract in the base.** A public cell is Landin's
knot — a cell holding a closure that reads the cell is a loop, and `termination/PurityGuardTest` exists to
keep it out — so `withCell` may not be a base name, and `escape` follows for uniformity. Consequently
`runThrow`, `catch`, `else`, `runAbort`, `runState*` and `written` are **body-less signatures in the base**
(Part I's rule that the base carries no representation-dependent body) and are **bodied per platform** over
that platform's private primitives — small Eliot defs over a trivial leaf, the "minimize Scala, decompose in
Eliot" shape — and in `stdlib/eliot-compiler/` over the evaluator's intrinsics for the compile track, where
they replace today's `AbortCarrier` overlay. What the compile track did by hand with `Either[String, _]`
the evaluator now does directly.

```eliot
/** jvm/eliot/eliot/effect/Throw.els */
implement[E] Throw[E] {
   def raise[A](err: E): {Exit[E]} A = exit(err)
}

def catch[E, A](computation: {Throw[E]} A, onError: E => {} A): A =
   escape(computation).foldEither(onError, identity)
```

`catch` binds no name: its slot supplies `Throw[E]` at the platform's default, `escape`'s slot supplies
`Exit[E]` at the leaf's, and `onError` is an ordinary parameter applied after the escape returns, outside
any handler.

The honest statement: the transformer monads do not vanish, they shrink to two leaves per target, outside
the library's bodies, the type system and the user's scope — and there is no pass.

### 9.7 Specialisation, purity, and the one rule on natives

**Specialisation** is the mechanism, not a follow-up: a definition is instantiated per `(vfqn, type
arguments)` and the phantom binders are type arguments (§9.4), so an operation call is a direct call to a
known method from the first build, exactly as an ability call erases today. There is no indirect call to
remove and no runtime representation of an implementation to fold away.

**Purity** is decided by the evaluator, not declared (§9.2): a term is pure iff the one NbE evaluator reduces
it, and a compile-time twin is a proof of purity, axiomatic as a meta transfer is. Reducing effectful code
under a pure implementation at compile time is possible under this model and is **not planned**: nothing on
the flag-day path needs it.

**No rule on natives — decided 2026-09-08.** A twin proves that a native *reduces*, not that it is pure, and
the two are different: a pure native over a platform representation the compile track does not have (the
jvm layer's `Path` algebra — `pathInternal`, `slashInternal`, `isAbsoluteInternal` — called from plain
defs in `eliot.file.Path`) can never have a twin and is not thereby effectful. Purity is not detectable
from the outside, and it is not what needs guarding. **Side effects are declared, never detected**: a
native that performs one is the body of an operation of some `effect` — `def now: Int =
currentTimeMillisInternal` is a bug of the *layer*, and its fix is an effect (`Time`), exactly as
cats-effect's `Clock` is. A native leaf is the axiomatic boundary, as a meta transfer is: the layer author
states what it does, nothing rechecks it. A twin remains what it always was — a compile-time
implementation for a native that has one, wanted by the refinement channel and `where` — and a native
without one is merely stuck at compile time, which is the loud fail-safe the compiler platform already has.
(The earlier reachability check, §10.1 step 2, is withdrawn — §12.)

### 9.8 What it deletes, keeps and adds

**Deleted:** `RowElaborator` and `RowElaborationProcessor`'s elaboration half; the carrier-minting,
pinning and supplying halves of `EffectSugarDesugarer`; `EffectLifter`; `IdNormalizer` and
`assertNoIdResidue`; `EffectCarrierNaming` and `EffectRowRendering`; `RunBoundaryFunctions`; the
constraint-aware declination and `activeFactKeys` probe in `AbilityImplementationProcessor`;
`RowChecker.fixesCarrier`, the derivation rules and the block peel; every `*Carrier` type, `Suspend` instance and cross-lift instance in
stdlib and jvm; the `eliot.carrier` package; the compile-track `Id.els` and `AbortCarrier`; the "a
discharger must be called directly" diagnostic.

**Kept:** `RowChecker.verifyRow` as the scope check and `EffectAccountingProcessor` (through the flag day,
D7); `AbilityResolver` and `AbilityImplementationProcessor` (structural match + `where`) for the two-site
default; `EffectRow` as declaration metadata; `CarrierKindChecker` as the kind system it is;
`WovenRecheck`; the seam-groundness test, re-pointed at bindings.

**Added:** the `effect` keyword, the named `implement`, and `with` in both positions, with their desugar;
the impl-name component of `Qualifier.AbilityImplementation`; the phantom binder per row entry and constraint,
the `Default` marker and the implementation-valued ground argument; the read-the-argument arm of
`AbilityResolver`; the three primitives per platform and the two evaluator intrinsics; the boundary rule in
`SyntheticMainSourceProcessor`. **Not added:** a bindings field on
`ValueReference`, a scoping node, a new `MonomorphicValue.Key` component, a consulted-set fixpoint.

### 9.9 Standing rules, re-read for v6

Rules 1, 2, 6, 7 and 8 of §5 carry over verbatim. Rule 3 is restated: **there is nothing to infer** — a
phantom binder is written by a `with`, by the enclosing declaration, by a supplying slot, or as `Default`,
in that order, and never left to a meta, a join, a lattice, an ordering-sensitive slot decision, or a sum over
a sub-graph; that bug class stays prohibited in both its forms. Rule 4 ("carrier-ness by tag")
has no subject left and is retired. Rule 5 (the whitelist) is retired with the elaborator; the desugar
consults declarations only, and there is no component that could accrete a sibling rule.

## 10. Implementation steps

Three groups: what lands **before** the flag day under the byte-identity gate, the **flag day** as one change
under the behavioural gate, and what follows. The flag day is one change because the ability's self
parameter changes kind (`F[_]` to none) and no ability can be both at once; everything else is staged around
that boundary.

### 10.1 Before the flag day (each independently landable, byte-identical)

1. **Compile-time twins for the pure natives — DONE 2026-09-08.** `String`'s already existed
   (`StringReductions`, stdlib); `List`'s were the last without one and landed in `ListReductions` (lang, beside
   `SystemNativesProcessor`, since `eliot.collection.List` is lang-owned): `empty`, `prepend`, `append`,
   `foldLeftInternal`, and the two string-splitting leaves that answer a list, `split` and `words`. The
   compile-time list has **no representation of its own**: a concrete list is its normal form, a `prepend` chain
   over `empty` (both body-less constructor applications, quoted and unified exactly like a `data` constructor),
   and the other four reduce over the chain. Nothing was added to the ground domain and nothing materialises — a
   chain at read-back declines materialisation as it always did, so codegen is byte-identical (verified over the
   45 example jars). One read-back defect had to be fixed for it: the quoter types every body-less application
   `Type`, so a borrowed runtime constant whose normal form is a chain was read back as a *type* and its literal
   elements collapsed to `Any` (`PostDrainQuoter.reduceSourced` now also requires the body to be *typed* `Type`).
   Two standing guard limitations were met and left as they are, neither about lists: `Eq[Int]` does not reduce in
   an ability guard (`where two == two` over borrowed constants), and `!` over a rowed call in a guard is a type
   mismatch against the compile-track `Either` carrier (spell the negative as `all(w -> !(w == S), xs)`).
2. **The twin-less-native check — WITHDRAWN 2026-09-08**, before any code. Its premise was false in the
   tree (`eliot.file.Path` calls six twin-less pure natives from plain defs) and wrong in principle: a
   twin is not a purity proof, and purity is not what the language guards — declared effects are (§9.7).
   Recorded in §12; nothing replaces it.
3. **Delete the dormant v4 formers — DONE 2026-09-08.** `GroundValue.Row`/`Computation`, `VRow`/`VComputation`,
   `CanonicalRow`, `CanonicalStack`, `WellKnownTypes.rowFQN` and their arms in the evaluator, the quoter (both), the
   unifier, `IdNormalizer`, both printers and the codecs — 231 lines, plus the P1 unit test that pinned them.
   `WovenRecheck` and the seam test stay. Verified byte-identical over the 45 example jars against a pristine
   baseline; every test green.
4. **Parser and AST for `effect`, the named `implement`, and `with` — DONE 2026-09-08, landed dark.** Two hard
   keywords, `effect` and `with`. `effect Name[G] { … }` parses to `ast.fact.EffectDefinition` and
   `implement name: Ability[pattern] { … }` to `ast.fact.NamedImplementation`, both carried on the `AST` beside the
   ordinary definitions (members kept as written; the flag-day desugar decides their qualifier and marker). `with`
   is one node, `Expression.WithBinding(subject, implementation)`, read as a trailing chain after everything else
   — so it is loosest and left-associative by construction — in `fullParser`, a block line, and a parameter's or
   field's type (`ArgumentDefinition`); a `with` on a def's own return type or inside a row fails to parse.
   `core/processor/UnsupportedSyntaxChecker` rejects every occurrence as "… not supported yet." at its own
   position, and `CoreExpressionConverter` lowers a `with` to its subject only after that error is recorded.
   Byte-identical over the 45 example jars; the TextMate grammar and the apidoc highlighter know the keywords.
   One consequence to keep: `effect` is now a keyword, and the package is named `eliot.effect`, so a dotted path
   segment (an `import`, a `module::` prefix) admits the keyword `effect` — `Primitives.isPackageSegment` — and
   nothing else does. Renaming the package was the alternative and was not taken.
5. **The phantom-binder spike — RUN 2026-09-08, and it holds.** Scratch programs against the current
   compiler, nothing landed. On the runtime track a phantom `[P]` on a rowless def is keyed: `tag[ImplA]` and
   `tag[ImplB]` emit `tag$ImplA` and `tag$ImplB` and a third call reuses the first; an applied ground tree is
   keyed structurally (`tag$Impl$CellWriter`); nullary abstract types unify only by identity
   (`Expected: Box[ImplA] / Actual: Box[ImplB]`); and an ability dispatches on a phantom through a `~`
   constraint (`pick[ImplA]` / `pick[ImplB]` select different instances). On the compile track an explicit
   phantom argument is carried into a `where` guard's checking and unifies by identity (the same mismatch,
   reported from the guard). Three findings beside the confirmation:
   - **An unwritten phantom is silently defaulted to `Type` today** (`tag("C")` with `[P]` compiles), so the
     F3 rejection is required, not optional: a row binder is a marked class of binder the checker refuses to
     default.
   - **Minted binders are prepended** (`carrierParam.toSeq ++ genericParameters`, the carrier at generic 0),
     so a call-site `greeting[ImplA](…)` today fills the *carrier* (`Actual: IO[ImplB[Unit]]`). v6's phantom
     row binders must not collide with a user's explicit argument list: mint them after the user's binders,
     or write them by name.
   - **Pre-existing and separate:** the `where` reducer (`EscalatingReducer.reduceApplied` over the `^Where`
     companion) cannot dispatch through a `~` constraint on a generic def, explicit or inferred ("Cannot
     evaluate the `where` precondition"), while a concrete dispatch inside a guard works. The compile-track
     mono checker is unaffected. Also observed: a failing `where` at `main` still writes the jar, against the
     build-artifact hygiene rule — a defect to fix on its own.
6. **The impl-name component, the `Default` marker and the read-the-argument arm — DONE 2026-09-08**, with no
   producer: the arm is dead until F1 and is tested by injection. `Qualifier.AbilityImplementation` (both the module
   and the resolved twin) carries `implementationName: Option[String]` as the third identity component, threaded
   through every match site, `ModuleAbilities.Impl`/`markerOf`, the marker utilities and the jvm name mangling (a
   named implementation appends its name; an anonymous one mangles exactly as before). `Default` is
   `WellKnownTypes.defaultImplementationFQN`, a compiler-owned sentinel like `Any` — declared in no layer, the
   nullary `Structure` a phantom binder carries when no `with` names an implementation. The reader is
   `monomorphize/check/ImplementationBinding`: a binding is the sentinel or a structure headed by an
   implementation's **marker** (an associated type shares the namespace but never the ability's own local name)
   applied to that implementation's own type arguments in declaration order, and
   **the contract with F1 is positional** — the binding is the **first ability-level type argument** (the marker
   declares the phantom, then its pattern parameters), so `AbilityResolver`'s existing arity slice begins with it.
   *That was fixed at F1, where the original "last" proved unwritable:* a type-argument list applies positionally, so
   writing an argument at index `k` means writing every argument before it, and the pattern arguments of an ordinary
   ability call (`show(x)`, `a ++ b`, `sort(xs)`) are exactly what no declaration determines — the write would have to
   infer them, which rule 3 prohibits. First, the write is a one-element prefix and everything after it is inferred
   exactly as before. The reversal is an encoding detail; nothing in §9 depends on which end it sits at.
   The arm: an implementation is recorded directly as its method of the reference's name at the binding's
   arguments, with no search, no `where` and no coherence question; `Default` and no binding at all run the
   two-site search at the pattern arguments; the resolution key keeps the binding, so one span under two bindings
   is two entries. Two things the step surfaced, both hash-order dependencies the new field exposed and both fixed:
   the missing-method diagnostic reported at whichever implementation method hashed first (now at the
   implementation's marker), and `JvmClassGenerator` emitted a module class's members in `Map` order, so every
   example jar's method order and constant pool changed with no semantic change — landed as its own commit ahead of
   this step, and verified by normalising all 175 differing classes to member order. Against that baseline the step
   is byte-identical over the 45 example jars; every test green.
7. **The two evaluator intrinsics — DONE 2026-09-08**, as `monomorphize/processor/EffectIntrinsics` (lang, folded
   into `SystemNativesProcessor`), keyed on `eliot.compiler.Escape::escape`/`exit` and
   `eliot.compiler.Cell::withCell`/`read`/`write` — compiler-owned names like `Type` and `Meta`, declared by no layer
   until F5's overlay bodies the dischargers over them. They replace the `Either`-based `AbortCarrier` overlay at the
   flag day. Tested directly (`EffectIntrinsicsTest`, the natives fired as the evaluator fires them) and end to end
   (`EffectIntrinsicsIntegrationTest`: an ability guard decided by a pure def that escapes, exits through a frame of
   another instantiation, and threads a cell — reduced on the compiler track through the `Id`-elaborated body, the
   borrowed `foldEither` and the `fold` twin). Byte-identical over the 45 example jars; every test green. Four
   findings, each a rule the overlay must follow:
   - **The instantiation is passed as a type *value*, the leading argument** — `escape(E, body)`, `exit(E, err)`,
     `withCell(S, s0, body)`, `read(S)`, `write(S, s)` — and a frame matches a key by definitional equality of
     concrete normal forms (`eval/ConcreteNormalForm`, factored out of the `Eq[Type]` leaf). The frame an operation
     reaches is the nearest enclosing one *of its instantiation* (`raise(msg)` at `String` must pass through an
     `else` at `Unit` — the everyday guard), and the evaluator cannot read that off type arguments: the post-mono
     `MonomorphicEvaluator` erases them and every binding is looked up by FQN alone, so one native serves every
     instantiation. Types are values, so the type itself is the key. A key that is not concrete, or an operation with
     no frame to reach, leaves the native **stuck** on its own FQN — loud at read-back, never a nearest-frame guess.
     The key parameter's declared type is the type `Type`, not `VType`, so a written type argument (a phantom binder,
     an explicit `[E]`) is never mistaken for the key.
   - **A native fires the moment it is applied; a bodied definition is applied lazily.** So a bare `exit` in a strict
     argument position fires before its consumer runs (`fold(c, pure(true), pure(exit(..)))` exits unconditionally),
     which is exactly why a post-flag-day row arm is a **thunk** applied after selection — the test writes
     `pick(c, _ -> true, _ -> exit(..))`. Conversely a thunk's *result* may still hold a pending application that
     would exit or read when forced, so every value crossing a frame is **settled** inside it (`renormalize`'s
     traversal: the head, constructor and stuck-native spines, never under a lambda): the body's result, an exit's
     payload before the exit is taken, a cell's initial and written content. Pending applications settle in that
     traversal's order, the one evaluation order the compile track has.
   - **A nullary top-level definition is evaluated once per binding** (`SemValue.Lazy`), so a nullary def that
     `read`s a cell would answer its first read forever — and `State[S]`'s `state` *is* nullary. The overlay must give
     such a def an argument (a thunk has one) or the binding must not be memoised; an escape is unaffected, since a
     failed initialisation is re-run. To settle at F5, not here.
   - **What an intrinsic answers is the overlay's data.** `escape` answers the overlay `Either`'s `Left`/`Right`
     and `withCell` the overlay `Pair`'s constructor (`stdlib/eliot-compiler/eliot/lang/Pair.els`, new — the jvm
     representation, kept in the overlay so the compile track stays self-sufficient), each applied to exactly its
     fields, since a `match` applies a handler to *every* spine entry. A private `pair` normal form with a
     `foldPair` twin was tried first and is refuted: the deep escalation links a bodied callee *reduced at its
     instantiation* ahead of a raw native, so the borrowed `foldPair` body always won the guard and could not read
     the private form. Two pre-existing limitations met on the way, neither about the primitives: a body-less leaf
     whose bare generic result is instantiated at a meta-carrying type (`read[S]` at `String`, and `exit[E, A]`'s
     `A`) trips R2 — the overlay must say what such a leaf states, an F5 item; and a function-typed payload through
     the compile-track `Id` (`val f = fold(c, thunk1, thunk2)`) is a mismatch at `runId` (`Unit -> Bool` against
     `Function[Unit, Bool]`), so the test wraps its arms in a `data`.
8. **The v6 stdlib, jvm layer, compile-track overlays, examples and `eliot-test` — DONE 2026-09-08**, staged in
   **`.v6/`** with its own manifest (`.v6/README.md`), which is where the flag day picks them up. A directory
   rather than a branch: it lands in ordinary commits on `master`, is reviewable next to the tree it replaces,
   applies at F7 as a copy plus the manifest's deletions, and — unlike a branch — carries a gate,
   `ast/processor/V6TreeParseTest`, which tokenizes and parses every staged file on every test run so a tree
   nobody compiles cannot rot while the compiler moves under it. It is *hidden* because the LSP's
   `SourceRootDiscovery` would otherwise take `.v6/stdlib/eliot` for a layer root beside the real one. Only files
   that *change* are staged; the manifest lists the deletions (`eliot.carrier`, all three `Id.els`, `eliot.jvm.IO`,
   `EffectAbilitySet`, `EffectsFakeCarrier`).

   Four consequences the design did not spell out, decided here and recorded in the manifest: a combinator's
   **return** row disappears (`foldLeft`, `map`, `foldOption`, `fold`, `.` — the callback's effects are bound at the
   caller, so the combinator declares nothing), `foldLeft`'s seed becomes a payload, only the five dischargers that
   actually touch a primitive are body-less in the base (`catch`/`else`/`runStateToValue`/… stay bodied there), and
   `runWriterToPair` grows the `~ Combine[W]` its platform body needs. The jvm leaves are declared `private`
   **per module** — five copies of two shapes, as `isNull` already is — because Eliot has no layer-private
   visibility and a public cell is Landin's knot; the compile-track intrinsics cannot do the same, since
   `EffectIntrinsics` matches their exact FQNs.

   One compiler change landed with it, byte-identical: `ImplementBlock`'s pattern is now **optional**, so an
   anonymous `implement Console { … }` parses. Step 4 landed the v6 surface dark but missed that shape, which every
   v6 `effect` implementation takes; the parse gate found it on its first run.

   **What it surfaced, and one thing that needs a decision.** For F5: R2 on the compile-track primitives (a brace
   over a generic result has no spelling); whether a generic binder can be written as a type value
   (`escape(E[], obj)`); and why the compile-track overlay stages `Abort` and `Throw` **only** — the track has one
   cell intrinsic keyed by the type it is handed, so `State[String]`/`Writer[String]`/`Dep[String]` would share a
   frame where the jvm layer separates them by declaring its leaves per module, and keeping them apart needs an
   *applied* marker key whose value-position spelling is unsettled; the nullary-read memoisation trap (step 7's
   third finding) hits the same three. `Abort` carries a private nullary marker (`Aborted`) rather than keying on
   `Unit`, so it cannot share frames with a `Throw[Unit]`. An unbodied discharger reached at compile time is
   *stuck*, which is loud, so nothing is lost meanwhile. **Needing a decision before F7:** `eliot.test`'s `pure { … }` has no v6 spelling. It
   forbade *all* effects in a test body by pinning it to `Id`; under §9.4 a slot's row does not close — an entry the
   slot does not supply continues the walk into the caller — so a slot cannot say "and nothing else". The staged
   framework drops the word and those cases become plain `in { … }`, bounded by the suite's own row. Making it
   expressible again would be a language addition (a closed row), so it is surfaced rather than invented.
9. **Record the behavioural baseline — DONE 2026-09-08**, as `.v6/baseline.txt` (deleted with the staging
   directory at F7) produced by `scripts/example-sweep.sh`. Per module: the jar's md5, size, class count and
   total bytecode instruction count, the instruction counts of the Main-Class stub and of the module's own
   class, and the exit code and standard output of one run with stdin at `/dev/null`. All 45 examples carrying
   a `main` compile and exit 0; totals are 950,823 bytes, 1,741 classes, 26,839 instructions. Three consecutive
   sweeps of the unchanged tree produced byte-identical reports, md5s included, so a later difference is a real
   one. Three things the recording settled:
   - **The script is durable, not scratch.** The same report answers both gates — a pre-flag-day step reads its
     `jar.md5` lines, F8 reads its `exit`/`stdout` lines — so the sweep recipe that had been rebuilt by hand for
     every broad change (`reference_verification_harness_recipes`) is now one committed script that encodes its
     four traps: the compiler's main class is `…eliotc.compiler.Main`, the three layer `--path`s are appended by
     `build.mill` and not by `Main`, `target/.eliot-cache` must go between compiles, and stdin must be
     `/dev/null` because four examples read it and a tty changes what they print.
   - **The jar's `Main-Class` is not the program.** It is the synthesized entry stub, eight instructions in
     every example, so on its own it measures nothing. The report keeps it and adds the two counts that do move:
     the class named after the module (the program's own code) and the instruction total over every class in the
     jar — which is what specialisation changes.
   - **A report is compared with its header stripped.** Everything non-reproducible (the commit, the module
     count) is a `#` line, and the compare is `diff <(grep -v '^#' before) <(grep -v '^#' after)`.

### 10.2 The flag day

It was planned as one change. It is landing as five, because the tree stops building the moment the desugar stops
minting carriers and there is no green checkpoint until the primitives exist — so the honest thing was to commit the
work in reviewable pieces, each stating that the tree does not build, rather than hold weeks of it uncommitted. The
gate is unchanged: §8's behavioural identity, read at the end.

**Where it stands (2026-09-09).** F1, F4, F5 and F7 are landed, and **the tree has no known blocker left**: a full
sweep is behaviourally identical to `.v6/baseline.txt` on every example the two trees share — same exit code, same
stdout, all 44 jars — and the only module-level differences are the three F7 made on purpose (`EffectAbilitySet`
deleted with the effect-set feature, `EffectsFakeCarrier` replaced by `EffectsNamedEffect`). Effects, dischargers,
per-instantiation frames, cells and `with` all run. F2, F3, F6 and F9 are untouched; the Scala tests still failing
are the v5 carrier, elaborator and mono suites, which F2 and F3 delete or rewrite.

- **F1 — the desugar. DONE** (`590e79dd`, `c73a9144`, `0eb0e06e`, `1b6482aa`, `c5c0485d`), in four parts.
  - *The `with` resolution path.* A named `implement` mints, beside its methods and its marker, a **name marker**
    `QualifiedName(name, Qualifier.Implementation(name))`, because `with h` has to be a keyed dictionary lookup —
    import scope and shadowing decide it, not `Map` order — and the real marker's qualified name cannot be built from
    the surface name alone (it also needs the ability name and the pattern key). So resolution is two keyed steps
    (`ImplementationNameResolver`), and only the real marker flows onward.
  - *The phantom-binder desugar.* §9.4 step 2 read literally: the return row vanishes onto a `Type`-kinded binder
    carrying the ability constraint; a `~` constraint keeps its subject binder and gains the binding as its **first**
    type argument; a top-level parameter or `data`-field row thunks to `Unit => A`. `ability` and `effect` lower
    identically (`AbilityMembers`) and differ in one thing: an `effect`'s members are given `{X}` as their declared
    row, which is the **only** place effect-ness is written down — §9.5's "the families are a description, not a bit
    in the language" holds literally, and a constructor class stays expressible.
  - *The write.* `row/BindingWriter` replaces `RowElaborator`: three jobs in one walk — write each callee's phantom
    binders as a leading positional prefix, thunk actuals at row slots and apply references to row-typed parameters,
    erase `with` from bodies and from slot types. The scope check falls out of the same walk rather than being a
    second pass. Two rules worth keeping: phantom-binder discovery needs **no new metadata** (a binder is one iff it
    occurs in no parameter and no return type *and* is the first type argument of one of the definition's own
    constraints), and thunk/apply is **unconditional** — the two are inverse, so a pass-through is an η-expansion
    and no argument's shape or type is ever inspected.
  - *Clause-row bindings.* §9.4 step 3's second value form — "that FQN applied to its own clause-row bindings",
    `greeting[recordingConsole[cellWriter]]`. An implementation whose clauses declare a row is parameterised by what
    they perform, so the marker has to *declare* that parameter and the write has to fill it. Three rules settled it:
    - **the binder is the union of the clauses' rows, declared by every value of the block** (`ImplementationRows`,
      applied by both `implement` forms). It cannot be per-clause: `AbilityResolver` hands the marker's type
      arguments to *every* method positionally, so there is one list and therefore one prefix. A clause that performs
      nothing declares what its siblings perform — true of the implementation, bound at the same `with`, and free at
      runtime since a phantom binder is erased. Nothing new mints: the union is written into the ordinary return-row
      position and `EffectSugarDesugarer` mints it exactly as it mints any other row, which is what keeps marker and
      methods in step by construction. The **marker** carries it on its guard slot — a row is erased from every type,
      so the guard reaches the discharge untouched. An **associated type is excluded**: a `type` in an `implement`
      block occupies a pattern slot, and a row there would change its arity at every use.
    - **a body `with` resolves the marker's own bindings in the scope it stands in**, which is all transitivity
      needs — the term is an ordinary written reference, so whatever the scope holds was itself written the same way.
    - **a slot `with` writes them `Default`.** The effects an implementation's clauses perform are supplied and
      discharged inside the *callee* (`transcriptOf`'s `runWriterToLog` covers the double's `{Writer[String]}`), and
      the caller writing the actual can neither see that nor name it. `Default` is what the callee's own body would
      have written. A caller-side binding would be the wrong frame, not a better one.

    It also closed a defect the slot form depended on: a slot's `with` **wraps** its row in the signature
    (`program: {Console} Unit with recordingConsole`), and `EffectSugarDesugarer`'s row readers matched on the
    outside, so the slot was never recorded as a row position at all — the actual was then written in the caller's
    scope and the very effects the `with` binds were reported undeclared there. Both readers now look through the
    `with`, exactly as the thunking rule already did.
- **F2 — deletions. NOT DONE.** Everything in §9.8's deleted list. `RowElaborationProcessor` keeps only the write
  and the scope check, and is renamed to say so; `RowChecker` keeps `Universe`, `checkable` and `peelBinders`, and
  its derivation half goes with `RowElaborator`.
- **F3 — the checker. NOT DONE.** No rigid carrier to lift into, so `tryPureWrap` goes; an unsolved phantom is an
  error rather than a junk `Type`. Rendering: an effect prints as the name the user wrote — no inverter, and a
  phantom binder is never rendered.
- **F4 — the run boundary. DONE** (`1b6482aa`). There is no carrier to instantiate: the synthesized entry is
  `def main: Unit = <user main>`, and `RunBoundaryFunctions` is repurposed from "values whose parameter 0 hosts a
  computation" to **the values where every effect's chain ends** — the jvm plugin registers the synthesized
  `main::main`, and the write binds `Default` there rather than reporting the row undeclared. `eliot.jvm.IO` and
  `runMain` are gone. *Known gap:* an undischarged control effect reaching `main` now resolves to the platform's
  single implementation and crashes at runtime on a frameless exit, where §9.5 asks for an error at the boundary.
  Telling a control effect from an interpretation one needs a bit the language deliberately does not have, so the
  honest fix is elsewhere — an A-item, not a silent hole (the failure is loud).
- **F5 — the primitives. DONE** (`1c175d66`), and the escape/cell are emitted **once per instantiation**, which is
  what makes a `raise` of a `NetError` pass through a `runThrow` installed for a `ParseError`. It needed no new
  mechanism: `generateAbilityImplNative` already iterated the monomorphic type parameters, so `ControlNatives` is a
  second registry whose makers take the ground type arguments too. The **first** type argument is the frame key;
  frames separate per effect (the leaves are `private` per module) and per instantiation (the key). Three decisions:
  the exit is a pre-allocated shared exception plus a tag rather than a generated class per instantiation (same
  unwinding semantics, no class generation to interleave with per-instantiation naming, no stack-trace cost for a
  value carrying no diagnostic); the leaves are **continuation-passing**, so no native constructs an Eliot `data` —
  the `Either`/`Pair` is built by the discharger's own body and the bytecode only applies a `Function`, which is the
  manifest's "only the five discharger bodies change"; and a primitive-represented key or payload (`{State[Int]}`)
  is a build error at the definition rather than mis-emitted bytecode.

  The **compile-track `Throw` is deleted**. Its `exit(E[], err)` needs a generic binder as a type value — the `.v6`
  manifest's open `E[]` question — and that does not work. The compile track keeps `Abort` alone: it keys on its own
  nullary `Aborted` marker and is what makes an `if..else` guard reduce, and a compile-time reduction that reaches
  the unbodied `runThrow` is *stuck*, which is loud rather than wrong.

  **REVERSAL, accepted by Robert on 2026-09-09: a guarded return type can no longer carry its author's message.**
  This deletion is not diagnostic-neutral, which the entry above did not say. `raise(msg)` is the vocabulary in which
  a guard states *why* it rejects — `def head[COND: Bool]: if(COND, T) else raise("empty")`, and the bare
  `def unavailable: raise("not available")` — and with nothing implementing `raise` on the compile track neither
  reduces. A rejecting guard still rejects, at the use site, but says only **"A type guard rejected this use."**
  The accepting half is unaffected: `if(COND, T) else T` types as `T` and runs as the bare type.

  So the effectful-signatures surface is narrowed, not merely re-implemented: the `where`-message vocabulary
  documented for guarded returns is gone until a compile-track `Throw` exists. The route that would restore it is
  keying `Throw`'s frame on a fixed marker the way `Abort` keys on `Aborted`, rather than on `E` — at the cost of two
  `Throw` instantiations sharing one compile-time frame. Not attempted; recorded so the loss is a decision rather
  than a surprise.

  Four debts the examples surfaced, each a real gap rather than a slip: **`fold`'s arms are thunks now**, so the
  backend intrinsic *and* the compile-time reduction have to run the selected arm; **`ClassWriter.getCommonSuperClass`
  cannot load a class being generated**, and merges only became reachable once a branch yields a lambda instance
  instead of a carrier value; **a derived instance's own binding binder is unsolvable by the pattern match** (it
  occurs in no pattern argument) so it is filled with `Default`, which is what it means, and `constraintArguments`
  must drop the leading binding rather than ground it; and **meta companions must be written too** — a `^Meta`
  brace calls ordinary abilities, its parameters are *not* thunked, and its row record must therefore be empty to
  match.
- **F6 — accounting. NOT DONE.** `EffectAccountingProcessor` reads received bindings instead of carriers and stays
  the codegen precondition; the "declared pure but performs effects" diagnostic stays in the pre-mono scope check.
- **F7 — the tree. DONE** (`1b6482aa`, with F5's five discharger bodies). The `.v6/` overlay applied over stdlib,
  jvm, lang and examples, with the manifest's deletions. Two staging gaps found: the compile-track `Either` still
  carried `implement Effect[Either[String]]` and `implement Throw[String, Either[String]]`, which cannot survive a
  tree with no `eliot.carrier`; and the `Default` sentinel **does** need a declaration after all
  (`stdlib/eliot/eliot/lang/Implementation.els`), because a written binding is an ordinary type argument and
  saturation demands the value it names. `eliot-test` is not yet moved.
- **F8 — the gate** (§8): behavioural identity on every example, tests green, the fake examples and
  integration classes with no minted carrier, the single-word `eliot-test` case, seam resolution, the size
  and instruction-count difference stated in the commit. **Behavioural identity is met** (see "where it stands"):
  every example the two trees share exits the same and prints the same. What is left of the gate is the tests, the
  `eliot-test` move, and stating the size difference. **The one size measurement so far:** the synthesized entry
  point is 7 instructions rather than 8, having lost its `runMain` call.
- **F9 — the documents. NOT DONE.** Part I rewritten to the v6 design (this Part's §9 is its draft); the CLAUDE.md
  *Effects Are a Channel* cornerstone rewritten; the `eliot-code`, `eliot-layers` and `eliot-jvm-backend`
  skills' effect sections; the `TODO.md` pointer.

If the gate cannot be met, the assessment in §9.2 is wrong somewhere — find where before landing anything,
and do not land a narrowed version (standing rule 2).

### 10.3 After the flag day

- **A2 — backend exit primitive**, if a microcontroller target replaces the jvm exception with a status flag
  and a jump; the primitive's shape (§9.6) does not change.
- **A3 — the reconsidered Part I limitations** (§7). Items 2 (cannot pin a `Suspend`-riding effect), 7 (a
  fake run needs a carrier-free region) and 9 (a `data` field typed by its own open carrier binder) have no
  subject. Item 6 (rule-4 violations diagnosed twice) is re-measured: with the elaborator gone the scope
  check's error is the only one left, and it must name the slot.
- **A4 — D4 dissolves** (any effect is storable and suppliable); Part I's limitation 5 dissolves with D5
  (§9.4).
- **A5 — retire the post-mono accounting verifier** under the §8 method (D7).

## 11. Open decisions

Numbers are kept where a Part I cross-reference uses them. Only rule decisions are listed; every
convenience is in §12 under "not now".

### D3 — `~` and `&` fully in user space (stages 3 and 4)

Still blocked on meaning, not difficulty. What is left is whether `~` and `where` unify, and the
phase-order blocker (the superability closure runs at resolve, before operators are structured) still lands
first if that is ever answered yes. Its half that asked "what does an ability denote as a value?" is
answered under names by *nothing*: an implementation is not a value, so an ability denotes no type of
values (§9.2).

### D4 — `Suspend`-riding effects: pinning and supplying

**Dissolves at the flag day.** There is no canonical carrier for an effect to lack; a stored
`{Console} Unit` is a thunk bound where it was written. Kept as a number only so §7.2 still resolves.

### D5 — a lambda body at a rowless arrow slot

**Closed 2026-09-08.** The walk crosses a lambda boundary iff the lambda's slot has a row (§9.4). A rowless
lambda is a barrier to received bindings only; it may bind and discharge locally, and must discharge everything
it performs. Kept as a number so §7.5 still resolves.

### D7 — can the post-mono accounting verifier retire?

Under names the post-mono check is "received bindings consulted ⊆ declared row", a subset of the mono key
that the pre-mono scope check already establishes lexically. The expectation is **yes, after the flag day**
(A5), by the §8 method: keep it through the flag day as the codegen precondition, trace it, retire it when
it fires on nothing. Not before, and not on the argument alone.

### D13 — can every `eliot-test` case be built with its handlers already applied?

**Closed 2026-09-08, yes.** `TestCase` carries no body; `in` runs the body in place and discharges only
`Throw[AssertionError]`; the runner reaches each suite by reflection into a declared slot and discharges only
`Writer`. Both are control effects at their single implementation. The one chosen interpretation, `Mock`, is
`with` on `mocked`'s slot type. Kept as a number so the memory notes resolve.

## 12. Closed by measurement or decision — do not re-propose

- **Carrier inference** — a carrier metavariable, a join solver, an `Id`-headed uniform judgment, a mode
  obligation, a post-drain mode resolver. Of 15 fix commits in the v2 window, the four highest-impact were
  one failure: a carrier metavariable captured by first-contact unification. Under v6 the class is still
  prohibited in its restated form (§9.9): a binding is filled by `with`, declaration, slot or default, never
  joined.
- **The carrier as the injection point** (§6's strategy as the *final* form). It chose an interpretation by
  instantiating a type, and every §6/§7 limitation was a symptom. Superseded by §9.
- **An implementation as a runtime value (a record whose fields are the operations).** Measured 2026-09-07:
  the checker cannot represent a Π-typed field. A field position demands a proper type
  (`Expected: Type / Actual: Type -> Type`), there is no surface spelling for a binder in a type position,
  and instantiating a polytype anywhere but a `ValueReference` throws — `Checker.appendTypeArgs`, whose
  comment states the invariant: *"polymorphism lives on named signatures."* The root cause is the mono
  key's indexing: a polytype is `VLam`, and instantiating it writes metas into a `ValueReference`'s
  `typeArguments`, which *is* the key; a field is reached by an accessor **application**, which has neither.
  Six operations carry their own type parameter, and the four that return it (`FileSystem.foldLines[B]`,
  `foldCodePoints[B]`, `PatternMatch.handleCases[R]`, `TypeMatch.typeMatch[R]`) admit no workaround. Two
  further defects of any record encoding: an ability may declare an **associated type** (`type Cases[R]`),
  which a record has no place for; and a **nullary operation as a plain field is strict**, so `readLine`
  would be computed once and every call would return the same line. Also lost with it: a parameterised
  `implement name(params)`, a record capturing an escape, and `with` as an ordinary def taking an
  implementation as a value.
- **A handler as a marker *type* in the type language** — a marker type per handler, a binder that occurs in
  types and is unified, a "two handlers meeting is a mismatch" rule, one environment per stored computation,
  a handler result function, and a post-mono lowering pass with `handler`/`returning`/`finish`/`resume`/
  `return`. What is closed is each of those: the lowering pass (§9.6's primitives make it unnecessary), the
  in-type binder and everything unification of it forced, and the four keywords. What is **not** closed —
  reopened and decided 2026-09-08 — is a *phantom* binder per row entry as the mono-key component (§9.4): it
  occurs in no type, so nothing unifies it, and it is the elaborator's existing prefix write with a name in
  place of a carrier.
- **Transitive `with` / summing abilities over a monomorphized sub-graph.** Dynamic scoping resolved at
  compile time; the carrier's third job in a new costume. Forwarding is by declaration only (§9.5).
- **A bindings field on `ValueReference`, a scoping node, or a consulted-set component on the mono key** as
  the carrier of a binding. Decided 2026-09-08 against, for the phantom binder (§9.4): the key already
  carries one argument per binder, and transitivity is a ground-value tree.
- **A runtime handler stack / dynamic scoping at runtime** as the language's mechanism. Kills erasure and
  makes storage semantics unwritable in a type. The one dynamic discipline that exists is the machine stack
  inside the escape and cell leaves (§9.6), private to the platform.
- **Full unification — every ability passed, none searched inside a function.** Declared rows for every
  ground ability use: no new capability over a `~` constraint, and the declaration burden puts
  `{Eq[Int], Combine[String], PatternMatch[Shape]}` on most monomorphic code, transitively. Rows inferred for
  abilities: that is inference, and it makes `printAll(xs) with reverseOrd` reach an undeclared resolution.
  Under either reading the two-site search stays, because the compile track dispatches
  `Meta`/`Numeric`/`PatternMatch`/`TypeMatch` from machinery with no call chain.
- **A public cell or escape in the base.** A public cell is Landin's knot; both primitives are
  platform-private, and the dischargers are abstract in the base for exactly that reason (§9.6).
- **A runtime free monad as the effect representation.** A heap tree of closures walked by an interpreter,
  a coproduct-with-injection to compose effects (the row back in the type), and no plain spelling of the
  scoped operations.
- **A `World` token / threading a fake dependency to sequence and protect I/O.** Not needed in a strict
  core (§9.7); purity is decided by evaluator stuckness.
- **A rule on twin-less natives** — the reachability check ("a native without a twin may be called only
  from an `implement`") and, with it, **a purity annotation for twin-less pure natives**. Withdrawn
  2026-09-08 (§9.7): a twin proves reduction, not purity, and the jvm layer's `Path` algebra is pure,
  twin-less and legitimately called from plain defs. Purity is not detected and not annotated; a side
  effect is declared as an operation of an `effect`, and a native leaf's declaration is axiomatic. The
  hole the rule aimed at (`def now: Int = currentTimeMillisInternal`) is a layer bug with a named fix — an
  effect such as `Time` — not a compiler check.
- **An anonymous implementation-literal expression** (`Console(printLine = …)`). A named `implement`
  covers every case in the tree, and a literal is a value.
- **Deleting `CarrierKindChecker`.** Measured per arm under v5: `verifyCarrierKinds` is the only thing
  rejecting a `[F[_]]` binder instantiated at a proper type. It is a kind system and stays; `EffectLifter`
  has no subject under v6 and goes at the flag day.
- **Putting the row on `VPi`** (Koka-style). Teaches every unification site, the printer and the `Function`
  native about rows. v6 keeps the row as declaration metadata beside the signature.
- **A `type X = {A, B}` row alias with its own AST node.** An `ast.fact.Expression` case is the most
  expensive thing this language can add; §2.4 replaced it with one resolve rule. (§2.4's own v6 spelling is
  in the "not now" group below.)
- **Discharge markers (`{-E}`).** There is no negative-effect surface; discharge is a frame a discharger
  installs.
- **Scanning the dictionary for an ability or implementation name.** Replaced by the keyed marker lookup,
  and the same closure covers `with`: an implementation name resolves at `resolve` to a `ValueFQN` by keyed
  lookup, and only that FQN flows onward — never a string carried downstream for `AbilityResolver` to search
  by, which would be a second lookup path with the same three failures the scan had: it ignores import
  scope, cannot be shadowed, and is decided by hash order.
- **The `<Ability>Carrier` convention as an explicit declaration.** Its premise — that an effect *has* a
  representation — is gone; the property it named ("has a canonical monad transformer") is exactly what
  §9.6's two primitives replace.

**Not now — conveniences deliberately left out of the flag day (2026-09-08).** Each is additive later, none
touches the mechanism, and the plan is kept as simple as possible until the flag day has landed:

- **A name for several implementations** (`implement mocks = mockConsole & mockFileSystem`) and **a name for
  several effects** (Part I §2.4's `ability Web[F[_] ~ Console & Log]`, whose carrier-binder spelling goes
  with the binder). A slot doubling many effects writes its chain and its row in full; a project wrapping
  `mocked` for an effect of its own restates them. `EffectAbilitySet.els` is deleted at the flag day; the
  superability closure stays for ordinary binders.
- **`with` on a def's own return row.** Rejected as a second spelling of `with` around the body.
- **A row spelling for a ground ability default** (`def printAll(xs: List[Int]): {Ord[Int]} Unit`). `with`
  on an ability reaches a callee only through a `~` constraint on a generic.
- **A user-declared boundary default** (`implement Throw[ConfigError]` handling an undischarged
  `Throw[ConfigError]` at `main`). It would overlap the platform's single `implement[E] Throw[E]`, which
  coherence rejects; the boundary binds the platform layer's defaults only.
- **Compile-time reduction of effectful code under a pure implementation** (a quiet-stall evaluator entry
  with fuel). Possible under the model, wanted by nothing.
- **Derived mocks** (`with mock[Database]`, EasyMock-style): compile-time reflection over an ability's methods,
  a derived implementation, a derived per-ability answer store and a recorder derivation. A feature in its
  own right, after the flag day, and a consumer for D3 if it is ever wanted.

---

# Part III — Provenance

## 13. Retired documents, and how to read a citation

Ten documents were merged into this one and deleted. Their full text is in git history (`git log --diff-filter=D
-- docs/`), and the table below says what each was and where its subject now lives. **Scaladoc comments across
the compiler cite them by their own section anchors** — those are *historical pointers* ("this code came out of
X §N"), not live references, and they do not index this document.

| retired document | what it was | anchor scheme in comments | where its live content is |
| --- | --- | --- | --- |
| `effects-as-channel.md` (v2, "uniform carriers") | the first shipped design: the carrier as a type argument the *checker solves* | `§0`–`§13`, `finding N`, `U1`/`U4-x` | superseded in direction and in code. What it got right and kept: the channel (§3.3), rows never flowing into types, carrier-ness by tag (§3.6), `Id` without `Suspend[Id]` (§3.4), payload/row vocabulary (§3.7). What it got wrong is §12's first entry |
| `effects-as-rows.md` (v3) | the landed design + its A.1–A.11 record: the elaborator writes the carrier | `§1`–`§9`, `A.x`, `R1`–`R6` | Part I in its entirety; §4 is its Appendix A.1; §5 its standing rules; §8 its A.9.4 method |
| `effect-row-tails.md` | pinned rows as the one spelling of a carrier stack | prose only | §2.3, §7.2, D4 |
| `testing-effects.md` | substituting effect implementations | `L1`–`L3`, `§2.x` | §6, §7.7 |
| `effects-v5-one-carrier.md` | rows as constraints on one carrier — the subtraction from v3 | `§4 step N`, `§5 Q1`–`Q4`, `§7` | §2.1 (step 1), §2.2 (step 2), §2.4 (§7), §3.8 + §12 (step 4 and Q1) |
| `effects-as-channel-v4.md` | the row leaves the type, the carrier leaves the language | `R1`–`R11`, `P0`–`P5`, `Q1`–`Q4`, `§0`–`§11` | superseded by **§9** (v6); its seam finding is §9.7 |
| `effects-v4-p0-spike.md` | does the `WovenValue` seam know the carrier? | `S1`–`S3` | §9.5's seam test; the test is permanent |
| `effects-v4-p2-sizing.md` | sizing the flag day | `§1`–`§5` | §10.2 |
| `effects-v4-flag-day-readiness.md` | is the flag day ready? (no) | `B1`–`B3` | §12 |
| `effects-syntax-userspace.md` | `~` and `&` as ordinary values | `stage 1`–`stage 4`, `§7.x` | §2.5 (stages 1–2, landed), **D3** (stages 3–4) |

**Citations to Part II's former numbering** (in commits and comments dated before 2026-09-07): *D1*/*B1*–*B4*
were the v4 decision and its blockers (now §9 and §12); *D2* the `<Ability>Carrier` declaration (§12's last
entry); *W1*–*W4* the v5 work items (§10.3 A3); *D8*/*D9*/*D10* the surface spelling, the cell and the purity
annotation (decided: §9.3, §9.6, §12); *D14*/*D15* (2026-09-07 only) were the slot `with` and the binding's
carrier (decided: §9.3, §9.4); *D6*, *D11*, *D12* and *D16* (grades, the ground-ability row spelling, the
user boundary default, bundles and effect sets) are §12's "not now" group. Two older citations in the tree — `docs/effect-lift-in-checker.md` and
`docs/effectful-signatures.md` — point at documents retired before these and are likewise historical.
