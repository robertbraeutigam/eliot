# Effects in Eliot — the design, and what is left to do

**Status (2026-09-06): the v5 effect system is shipped, this is its single document, and Part II is the decided
plan to replace its carrier with effects passed as invisible arguments (v6).** Part I describes the tree as it is until §10's flag day
lands. This document replaces ten separate notes — the v2, v3, v4 and v5 designs, the three v4 measurement notes, the row-tails note, the
`~`/`&` user-space note and the testing note — whose landed steps, migration roadmaps, deletion slices and
per-step measurements are removed. What survives here is (I) the design as it actually behaves, (II) the
plan to replace it, and (III) enough provenance to read a source comment that cites a retired
document.

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
`effect` has no carrier binder and the question disappears.

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

**This strategy is what ships; it is not the final form.** Part II (§9, decided 2026-09-06) replaces the carrier
as the injection point with an implementation *value* passed as an invisible argument and installed by `with` —
every limitation below (the lifting wall, the region rule, one interpretation per type argument) is a symptom of
choosing an interpretation by instantiating a type, and disappears with it.

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
10. **Totality leaks through a stored computation that names its own type in its row.** Measured 2026-09-06:
    `data Knot(run: {State[Knot] | Id} Unit)` with `def step: {State[Knot]} Unit = loop(state)` and
    `def loop(k: Knot): Unit = runId(runStateToPair(k, k.run)).first` compiles and overflows the stack at
    runtime, and so does the same shape over `Dep[Knot]`/`provide`. No value is recursive in the reference graph;
    the cycle runs through the computation obtaining itself from its own effect and running it.
    `StrictPositivityChecker` treats `Knot`'s occurrence as an effect argument as positive because it does not
    look through `StateCarrier`'s `S =>`. → §10.1 step 1, the positivity rule, which lands before the flag day.

---

# Part II — The plan: effects are abilities passed invisibly (v6)

**Status (2026-09-06): decided, revised the same day.** Part I stays the authoritative description of the tree
*until the flag day in §10 lands*; nothing in Part I is amended in place before then (standing rule 1). This
part is the design that replaces it, the reasoning that produced it in condensed form, the measurements it
rests on, the implementation steps, and the decisions still open. It supersedes the former D1 (v4, "does the
row leave the carrier behind?"), its blocker B1, and D2 (an ability declaring its carrier), whose premise it
removes; Part III says how to read a citation to them.

The first draft of this Part (the morning of 2026-09-06) modelled a handler as a *type* — a marker installed by
`with`, one nullary binder per row entry. It was reviewed against the tree the same afternoon by hand-writing
its desugar in today's syntax and compiling it (§8, the method; §9.2, the findings). The review found the
model right and the vocabulary wrong: the binder is the *type of an invisible argument*, and reading it as the
argument itself — a value — removed every gap the review found. That is the design below.

## 8. How this plan is run

**Decision protocol.** Standing rules 1 and 2 (§5) govern. Every entry below marked **decision** is Robert's;
nothing in it is a judgement call to be made in flight, and a step that finds itself narrowing one of the
rules here stops instead of landing.

**The gate**, for every pre-flag-day step: `./mill __.test` green, all example programs carrying a `main`
compile, and every example jar `md5sum`-identical to the pre-change build. Byte-identity is a **safety
oracle, not a hard gate**.

**The gate for the flag day itself** is different, because the output legitimately changes wholesale:
byte-identity is replaced by **behavioural identity**. Before the change, run every example jar and record
its standard output and exit code (the sweep recipe in the `reference_verification_harness_recipes` memory);
after it, the same sweep must produce the same transcript. Plus: `./mill __.test` green; the fake-carrier
examples (`EffectsFakeCarrier`, `EffectsFakeConsole`, `EffectsTestFramework`) and the two integration test
classes express the same tests **without minting a carrier type**; `eliot-test`'s single-word case
(`"…" should "…" in onConsole(input, { … })`) reads as a `with`; and the seam test still finds every
dictionary's type ground.

**The method, when the question is "is this still load-bearing?"** — reuse it rather than re-inventing it:

- **Hand-write the desugar in today's syntax and compile it.** The cheapest experiment this compiler
  offers, and the one that decided this Part: the v6 shape of an ability (§9.4) is expressible with today's
  `ability`/`implement`/explicit type arguments, so a proposed rule is tested against the real checker in
  seconds, before any compiler change. Every row of the table in §9.2 was produced this way, in a scratch
  directory, with `./mill examples.run jvm exe-jar <dir> -m <Name>`.
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

## 9. The decision — an effect is an ability passed as an invisible argument

### 9.1 The model, in five sentences

An **effect** is an ability, and an **effect implementation** is a named value of it, exactly as an
`implement` is today. A definition's **row** is a list of **invisible parameters**, one per entry, each
typed by that entry's ability; an operation call is a call on the in-scope parameter of its ability's type,
resolved the way `show` resolves against a `T ~ Show[T]` binder today. `with` supplies the value at a run
site, and every intermediate definition forwards it without naming it. Because the program is monomorphized
from `main`, each such value has a compile-time-known type, so an operation call is a direct call to the
implementation's method and the argument erases unless it carries runtime fields. The two operations a
strict, pure, recursion-free core cannot write as calls — a **non-local exit** and a **cell** — are
**platform natives below the line**, exactly as the loop already is; nothing else needs one.

### 9.2 What was decided, and why

Condensed from the 2026-09-05 assessment (free monads) and the 2026-09-06 review (handlers as types).

- **The problem was never "instances may live in only two places".** §6 already lets a test submit an
  interpreter, but only by conjuring a *type* and hanging instances on it — dependency injection routed
  through the type system. Every §6/§7 limitation is a symptom of that: a fake is monomorphic at one carrier
  and gets no lifting (the n² wall); a fake run needs a carrier-free region or the W3 tag; one type argument
  decides the interpretation of *every* effect at once. Named precisely, the carrier does three jobs —
  sequencing representation, discharge-stack representation, selector of interpretation — and the third is
  the misfit.
- **Runtime free monads: rejected** (§12). What they get right — the interpretation is a *term* — is kept.
- **Handlers as types, the first v6 draft: measured, and revised.** The draft's desugar — an ability whose
  self parameter is a nullary marker appearing in no method type, an `implement` over the marker, one
  binder per row entry — was written by hand in today's syntax and compiled:

  | program | shape | result |
  | --- | --- | --- |
  | `NullaryOne` | `ability Named[H] { def name: String }`, `data Alpha`, `implement Named[Alpha]`, `name[Alpha]` | compiles, prints `alpha` |
  | `NullaryRegion` | `def greet[H ~ Named[H]]: String = "hi " ++ name`, called `greet[Alpha]` | compiles, prints `hi alpha` |
  | `NullaryTwo` | `def both[H1 ~ Named[H1], H2 ~ Named[H2]] = name[H1] ++ name[H2]` | compiles, prints `alpha beta` |
  | `NullaryTwoBare` | as above with both `name` bare | compiles, prints **`alpha alpha`** |

  The first three say the checker already accepts the shape, so the flag day's "kind change" is not blocked
  by it. The fourth is a **silent miscompile**: a phantom binder cannot be solved by unification, since it
  occurs in no parameter or result type, and what resolves it today is the by-name constraint fallback in
  `AbilityResolver.tryResolveOne`, whose own comment says it returns the *first* constraint of that ability.
  Two `Throw[E]` entries or two `Dep[X]` entries are exactly two binders of one ability. Three further gaps
  were found by reading: a handler's own type parameters at `c with either` had no solver (constraints are
  never unified and resolution needs ground arguments, so nothing linked `either[?E, ?A]` to `c`'s row without
  an expected type); a stored `{Throw[E]} Unit` field minted a binder on the `data` that a pure `def tests:
  List[TestCase]` had no row to mint from; and a parameterised handler conflated its identity with its
  runtime parameter, so `recovering(f)` was not ground at the seam and an `implement` clause had no way to
  reach the parameter. **Every one of these is a symptom of passing the dictionary as a type.** Passed as a
  *value*, the binder is solved from the argument by ordinary first-order unification (the fourth row
  becomes `name(h1) ++ name(h2)`), a handler's type parameters are inferred as any value's are, a stored
  computation is a function of its dictionary, and a handler with fields is a value with fields whose
  *type* keys the instantiation.
- **Decision: the interpretation is a term, and the term is a value.** A definition's row is its invisible
  parameters; `with` supplies one; the platform's instances become its default values, installed at the
  run boundary. (Robert, 2026-09-06.)
- **Decision: an effect ability is a type inhabited by its implementations.** The former D3(b), decided
  **yes for effect abilities** and left as it is for ordinary ones: `Show[T]` keeps its global, coherent
  search; `Console` is supplied by scope. The two are one mechanism at different defaults — today's
  `AbilityResolver` already consults the in-scope constraint before it searches globally.
- **Decision: dictionaries are passed, never captured.** A lambda at a rowed slot receives its dictionaries at
  call time from its caller; a rowless lambda may reference none; a stored `{Throw[E]} Unit` is a function
  of a `Throw[E]` value supplied where it is run. This is rule 4 and D5 in one sentence, and it is what keeps
  an exit's frame and a cell from being outlived: the escape problem Effekt answers with second-class
  blocks is answered here without touching first-class functions.
- **Decision: the exit and the cell are platform natives; the language has no lowering.** `raise`/`runThrow`
  and the state cell are body-less in the base and bodied per platform, like `forever`; the compile track
  gets twins in the evaluator. A compiler-internal lowering (result-code form, parameter-passing form) is
  **not** required and is kept only as a possible backend optimisation (§10.3 A2). Chosen to keep the
  complexity out of the language: user code contains no loop, no exit and no cell, and each reaches it only
  through an effect the row tracks. (Robert, 2026-09-06.)
- **Decision: positivity through effect arguments.** Measured (§9.5): `data Knot(run: {State[Knot] | Id}
  Unit)` and its `Dep[Knot]` twin both compile today and stack-overflow at runtime — totality already leaks,
  because `StrictPositivityChecker` treats a type's occurrence as an effect's argument as positive. A type's
  own occurrence as a type argument of an effect in a stored row is a **negative occurrence**. Not specific
  to cells: the reader ties it too, so the rule is about a stored computation reachable from itself through
  its own dictionary. (Robert, 2026-09-06.)
- **Decision: ambiguity is an error.** Two in-scope dictionaries of one type are reported, never resolved to
  the first (the `alpha alpha` bug, and standing rule 8).
- **Decision: one handler environment per stored computation, fixed at compile time.** Running the *same
  runtime value* under two different dictionaries is a type error naming both — never a silent lift, never a
  runtime dispatch. A *definition* producing such computations is generic in the dictionary and is
  instantiated per use like any polymorphic definition.
- **Decision: resumption is a call or an exit, nothing else.** An operation either returns to its caller or
  finishes to its run site. Multi-shot and non-tail resumption — generators, async, coroutines — are out. It
  is what keeps a microcontroller in reach, and it is exactly why the exit and the cell need a primitive:
  a full algebraic-effect handler implements both purely because a clause receives the continuation, and
  removing continuation objects is the whole saving.
- **Decision: purity is decided by the evaluator, not declared** (§9.7). No `World` token, no purity
  annotation. A native without a compile-time twin may be called only from an effect implementation's body.

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

**An effect is declared without a carrier binder.** Effects get their own declaration pair mirroring
`ability`/`implement`, so effect-ness is a keyword and never a shape (§3.6's concern, answered by syntax).
Inside an `effect` block every operation performs that effect by definition; the `{E}` on each method is gone.

```eliot
effect Console {
   def printLine(s: String): Unit
   def readLine: Option[String]
}

effect Throw[E] {
   def raise[A](err: E): A
}
```

**An implementation is a named value.** Its clauses are the operations, each with its own row, so an
implementation may itself perform effects — those effects are the *value's* invisible parameters, supplied
from the scope of the site that installs it. The platform's `implement[F[_] ~ Suspend] Console[F]` becomes a
value calling its natives directly; a test's fake is the same declaration in the test module — no carrier
type, no colocation, no coherence question, because a value is chosen at a site and never searched for:

```eliot
handler jvmConsole: Console {
   def printLine(s: String): Unit = printLineInternal(s)
   def readLine: Option[String] = lineOrNone(readLineInternal)
}

handler recordingConsole: Console {
   def printLine(s: String): {Writer[String]} Unit = tell(s ++ ";")
   def readLine: Option[String] = None
}
```

**A value may carry fields.** `Dep`'s `provide` becomes the one field-carrying implementation the base
ships; the field is an ordinary runtime value and the implementation's *type* is what the instantiation is
keyed on (§9.5):

```eliot
handler constant[X](x: X): Dep[X] {
   def dependency: X = x
}
```

**The exit and the cell are not written as clauses at all.** `Throw`, `Abort`, `State` and `Writer` keep
their operations, and their dischargers are body-less defs in the base that the platform bodies over its
two primitives (§9.6). The base then builds every other discharge word on them in ordinary Eliot:

```eliot
def runThrow[E, A](body: {Throw[E]} A): Either[E, A]                    // platform-bodied
def runState[S, A](initial: S, body: {State[S]} A): Pair[A, S]           // platform-bodied

infix left
def catch[E, A](body: {Throw[E]} A, onError: E => {} A): A = runThrow(body).foldEither(onError, a -> a)
```

**One run form for a value: `with`.** `e with h` evaluates `e` with `h` as the in-scope implementation of
`h`'s effect, subject-first, so it reads as `catch` does today. It is a *binding form* — the one new
expression syntax — not a function: the subject is rewritten to a function of `h`'s effect and applied to
`h` (§9.4).

```eliot
def transcript: String = written(greeting("Bob") with recordingConsole)
def safe: Configuration = parse(config) catch (_ -> emptyConfiguration)
def demo: Pair[String, String] = runState("first", swap("second"))
def value: String = dependency with constant("x")
```

| today | v6 |
| --- | --- |
| `runThrow(c)` | `runThrow(c)` — base-abstract, platform-bodied |
| `c catch f` | `c catch f` — base Eliot over `runThrow` |
| `c else x` | `c else x` — base Eliot over `runAbort` |
| `runAbort(c)` | `runAbort(c)` — platform-bodied |
| `runStateToPair(s0, c)` | `runState(s0, c)` — platform-bodied over the cell |
| `provide(x, c)` | `c with constant(x)` — the tree's one existing handler-as-term, generalised |
| a fake via `implement Console[Recorded]` | `c with recordingConsole` |

**A stored computation is a row-typed field with no tail.** `data TestCase(body: {Throw[E]} Unit)`; the
`| Id` pin disappears. Its row lists the effects still unhandled, so *where it is run is where they are
handled*: the field is a function of a `Throw[E]` value, and the `data` is implicitly parameterised by that
value's type (§9.4). A computation created inside a `with` scope and run outside carries the effect in its
type and is handled at the run site; one created with its effect already handled is `{} Unit`.

**`{}` is one row variable per signature**, exactly as Part I rule 4 says: `foldLeft(initial: {} B, …): {} B`
is instantiated at its caller's row, receives the caller's dictionaries as its own invisible parameters, and
forwards them to the thunk at call time. A lambda at a `{} B` slot therefore reaches the enclosing
dictionaries **as parameters, not by capture**. A lambda at a rowless slot (`f: A => B`) has none and an
operation call inside it is an error naming the slot — D5, decided.

**What disappears from the user's world:** the whole `eliot.carrier` package (`Effect`, `Suspend`,
`flatMap`, `pure`, `map`, `suspend`); every `<Ability>Carrier` type and `run*Carrier` accessor; the
cross-lift instance matrix; `~ Effect` and `~ Suspend` on definitions; `Id` and `runId`; pinned tails; the
`{| Recorded}` capture tag; the fake-carrier recipe. Rule 2's "carrier-typed position" and rule 4's four
carrier namings collapse to one predicate: a slot either has a row and takes dictionaries, or it does not
and takes none.

### 9.4 The core desugar — everything is still a def, and the dictionary is the receiver

The front end fits the current core with **one new expression form** (`with`, a binder); a handler rides
exactly the path an `implement` block rides today. The ability's self parameter is the dictionary's type
and the method's first parameter is the dictionary — the shape `show(value: A)` has always had.

| construct | desugars to |
| --- | --- |
| `effect E[…] { def op(…): R }` | the ability marker `Qualifier.Ability("E")` plus a method def `op[H ~ E[…, H]](self: H, …): R` — as `ability` today, with the self parameter a **nullary** type `H` and the receiver **written** |
| `handler h[…](fields): E[…] { clauses }` | a `data h[…](fields)` (a nullary `data h` when it has none) plus `implement E[…, h[…]] { clause defs }`, each clause taking `self: h[…]` — `ImplementBlock`'s `Qualifier.AbilityImplementation` unchanged. A clause's own row is desugared like any def's: its dictionaries are fields of the value, supplied at the `with` site |
| a row `{E1, E2}` on a def | **one hidden value parameter per entry**, `h1: H1, h2: H2` with `H1 ~ E1[H1], H2 ~ E2[H2]` inferable binders — today's `EffectSugarDesugarer` mints one carrier for the whole row; this single change is what deletes stacking, lifting and canonical order |
| a row on a `data` field | the field is `Function[H, A]` with `H` an inferable binder minted **on the data**; a use of the data without that argument gets an inferable meta, and a signature carrying such metas is generic in them — the `inferable` mechanism `GenericParameter` already has for the carrier |
| a row on a parameter | the parameter is `Function[H, A]` over the callee's binder for that entry, and the argument expression is wrapped in a lambda over it — today's "supplied parameter row", now literally a parameter |
| `{}` on a parameter or return | the signature's one row variable: the callee's own hidden parameters, forwarded |
| an operation call `op(args)` | `op(h, args)` where `h` is the in-scope hidden parameter of `E`'s type — a **hole** filled by dictionary resolution (below) |
| a call to a rowed callee | a hole per entry of the callee's declared row, filled the same way |
| `e with h` | `(h' -> e')(h)` where `e'` is `e` with every hole for `h`'s effect filled by `h'`; `h'`'s type is inferred from `h`, and the ability resolves at it |

**Dictionary resolution is the one checker addition**, and it is deterministic. A hole is filled from the
enclosing definition's hidden parameters by matching the callee's declared constraint — ability *and*
arguments — against theirs: the desugar fills every hole a declaration determines (`printLine`'s `Console`
against the region's one `Console`), and the checker fills the rest **post-drain**, once the metas in the
callee's constraint are ground (`orRaise[E]` in a two-`Throw` region needs `E` first). Zero candidates is
the effect diagnostic ("performs `X` but does not declare it"); two is ambiguity, an error naming both; the
by-name fallback in `AbilityResolver` is deleted, never widened. It lives in `check/` beside
`AbilityResolver` as a post-drain collaborator, may consult only the enclosing definition's hidden
parameters and the callee's declared constraints, and never runs inside unification (standing rule 6).

**Operation resolution is the existing `AbilityResolver`.** With the receiver written, `H` is solved from
the argument by ordinary unification and is ground at the seam; `printLine(recordingConsole, s)` resolves to
the fake's clause by exact structural match, with no declination, no `~ Suspend` filter and no `where` guard.

**Both verifiers keep their vocabulary and their source of truth.** `RowChecker.declaredRow` and
`EffectAccountingProcessor.openRow` read a binder's `~` constraints; that is unchanged, now with one binder
per effect. The accounting ride test becomes **parameter identity**: an operation counts toward a
definition's row iff the dictionary it is called on *is* one of the definition's own hidden parameters; a
call on a `with`-bound or field-held dictionary is handled. The pre-mono verifier's job is largely absorbed
by dictionary resolution (a missing dictionary is reported at the operation), and it keeps the one
diagnostic resolution cannot voice, "declared pure but performs effects".

### 9.5 Semantics

- **An operation call** is a call on the dictionary's method; a clause computes and returns, and evaluation
  continues after the call. Sequencing is strict evaluation order and nothing else: blocks lower to
  immediately-applied lambdas that the backend compiles as a closure plus one application (strict, once), and
  a nullary definition is a static call at every reference — both verified in the current backend. An
  effect that "runs where it is written" is therefore the ordinary calling convention, not a rewrite.
- **The exit** (`raise`, `abort`) transfers control to the innermost dynamically enclosing `runThrow` /
  `runAbort` for that dictionary's type; the run site's result is the `Left` / `None`. **The cell**
  (`state`, `putState`, `tell`) is per activation of its `runState` / `runWriter`: two nested runs over one
  state type never share it.
- **Nesting order at the run site decides interaction.** `runThrow(runState(s, c))` versus
  `runState(s, runThrow(c))` is the difference between state surviving a `raise` and not — written by the
  user where the dischargers are called, as the pin's order was written, with no canonical form to fix.
  `EffectsOrdering.els` prints both and is the behavioural gate's witness.
- **Instantiation.** A definition is instantiated once per dictionary environment it runs under, keyed
  `(vfqn, payload arguments, dictionary types)` — the v4 seam finding with "dictionary types" in place of
  "carrier stack". Every `with` and every discharger call is a term reachable from `main`, so every type is
  ground at the seam; a dictionary's *fields* are runtime values and pass as an ordinary argument, so
  `constant(x)` with a runtime `x` is one instantiation, not one per value. A dictionary-typed *parameter* is
  allowed exactly as a `T ~ Show[T]` binder is: every instantiation binds it to a ground type. A dictionary
  chosen by a runtime `if` or stored in a data structure at an unknown type is a compile error — "ability
  references are never passed around in structures", applied to dictionaries.
- **The run boundary.** The synthesized entry point is ordinary code: `main with jvmConsole with jvmLog with
  jvmInf …`, one `with` per `Suspend`-riding ability the jvm layer implements today (`Console`, `Log`,
  `Inf`, `FileSystem`, `Environment`, `Process`). `SyntheticMainSourceProcessor` shrinks;
  `RunBoundaryFunctions` is deleted.
- **`Inf`** stays an effect handled only by the platform (`forever` as a native loop), and it stays the one
  effect that may reach `main` unhandled by anything but the platform.
- **"The fake cannot cheat" survives without `Suspend`.** Measured: a body-less def in a user module is
  rejected at codegen ("Function not implemented."), and one producing a meta-carrying type is rejected
  earlier by R2. So a test's implementation reaches I/O only through effects it declares in its own
  clauses' rows — which the verifiers charge as usual.
- **Totality.** Measured on the current tree, both programs compile and overflow the stack at runtime:

  ```eliot
  data Knot(run: {State[Knot] | Id} Unit)
  def loop(k: Knot): Unit = runId(runStateToPair(k, k.run)).first
  def step: {State[Knot]} Unit = loop(state)
  def main: {Console} Unit = { loop(Knot(step)); printLine("done") }
  ```

  and the same with `Dep[Knot]` / `provide` / `dependency`. No value in the reference graph is recursive;
  the cycle runs through a stored computation obtaining *itself* from its own dictionary and running it.
  The positivity rule (§9.2) closes it: `Knot` occurring as the argument of an effect in a field's row is a
  negative occurrence, and the `data` is rejected — exactly as it would be if `StateCarrier`'s `S =>` were
  visible to the check. This is the one change here that lands **before** the flag day and fixes the
  shipped tree (§10.1 step 1).

### 9.6 The two primitives — the platform's, below the line

A strict, pure, recursion-free core can write an operation as a def when it returns to its caller and its
result depends only on its arguments and the dictionary's fields. Two operation shapes cannot be written
that way, and they are precisely the two the carriers encode as data today:

- an operation that **does not return** — `raise[A](err: E): A` has no body of type `A` for every `A`;
  `ThrowCarrier.flatMap` *is* that exit, written as data;
- an operation whose result **depends on an earlier operation's argument** — `state` after `putState`, a
  value threaded through calls that never mention it; `StateCarrier.flatMap` *is* that threading.

Both live in the platform layer as body-less base defs given native bodies, the way `forever` does. The base
never says how; a backend chooses, and the choice is invisible above the line:

| | exit: `raise` / `runThrow` | cell: `state` / `putState` / `runState` |
| --- | --- | --- |
| **JVM** | `raise` throws; the exception class is the monomorphized instantiation itself (one per handler and error type, preallocated, `fillInStackTrace` overridden), the error rides in a field; `runThrow` is `try { Right(body(h)) } catch (ThatClass e) { Left(e.error) }`. No runtime tag check: two `Throw` types are two classes, and nesting resolves by the JVM's innermost catch | the dictionary is an object with one field; `runState` allocates one per activation and passes it as the actual argument; `state`/`putState` are a field read and write |
| **ATtiny** | *result-code form*: every call that may reach a `raise` for this handler returns its value in the return registers and a status in one reserved register or a global byte; the error is written to a static slot owned by the run site; intermediates return on status. Zero runtime, no libc. Or *setjmp/longjmp* from avr-libc, one jump buffer (~23 bytes) per nesting level, as the measured optimisation | the cell is a local in the run site's frame, its address passed in one register; when a state type has a single run site in the program the backend may give it a static RAM address, legal because there is no recursion, making `putState` one `sts` and `state` one `lds` |

Since no dictionary outlives its run site and there is no recursion, handler nesting depth is static, and the
RAM for every error slot, status byte and cell is computable at compile time — the resource-bound story the
language wants for microcontrollers. "Which calls may raise" needs no analysis: after monomorphization a
call takes the `Throw[E]` argument iff it may raise into that handler, so the row *is* the may-raise set.

The **compile track** needs no bytecode: the one NbE evaluator implements `raise` as a short-circuit and
`runThrow` as the catch, which is what the `AbortCarrier` overlay does by hand today, so the evaluator stays
the only interpreter. And `Writer`'s `tell` is a cell with a `Combine` — no third primitive.

### 9.7 Optimisation — purity is what the evaluator can reduce

The original plan (distinguish constants from World-dependent values by threading a `World`, as GHC's token
does) is not needed and would not fit. GHC needs the token because its core is lazy and purity alone lets
the optimiser reorder I/O; Eliot's core is **strict**, and rule 1 fixes the order and count of every
evaluation syntactically. So the rule is:

- **A term is pure iff the one NbE evaluator reduces it; a term is the World iff it is stuck on a
  runtime-only native.** The evaluator is also the optimiser — applied to runtime bodies as a partial
  evaluator — so the single-evaluator cornerstone holds. Transitivity is free: a body that calls an untwinned
  native is stuck at that call, and stuckness propagates.
- **A compile-time twin is a proof of purity**, axiomatic in the same sense a meta transfer is. A pure native
  without a twin is treated as impure — an optimisation withheld, never a miscompile.
- **The one constraint on the optimiser**: a stuck call keeps its position and its multiplicity. Everything
  reducible may be folded, deduplicated, inlined, deleted when unused, or evaluated at another time, because
  in a total pure language the strategy is unobservable. `Inf` is native, so the evaluator never loops; fuel
  bounds the cost of a legal large pure fold, and out-of-fuel means "leave the code as written".
- **Dictionaries make it better.** An effectful definition is pure code parameterised by its dictionaries,
  so purity is decided per instantiation: `greeting("Bob") with recordingConsole` reduces to the transcript
  at compile time; under `jvmConsole` the same body is stuck at `printLineInternal`, in place, once.
- **A nullary definition that reduces is a constant** (compile-time or once at startup); one that is stuck
  is re-evaluated at every reference — which is what "effects run where they are written" says, and what the
  backend does today.
- **Rule: a native without a compile-time twin may be called only from an effect implementation's body**,
  directly or through private helpers reachable only from one. Today nothing forces a native-calling
  definition to declare a row; the `suspend` wrapper was the informal guard and goes with the carrier. The
  check is a reachability check on runtime bodies like the recursion gate; the error names the two fixes.
  **Measured** (2026-09-06, every native and its call sites in `jvm/eliot` and `stdlib/eliot`): every I/O
  native — the `*Internal` leaves in `file`, `effect` and `system` — is inside an `implement` block or a
  private helper (`raiseError`, `lineOrNone`, `valueOrNone`) reachable only from one, so the jvm layer
  passes. What does **not** pass without a twin: `Path`'s six natives (`pathInternal`, `slashInternal`,
  `isAbsoluteInternal`, `fileNameInternal`, `parentInternal`, `extensionInternal`) and `isNull`, pure and
  called from plain defs, with no compile-time `Path` model to twin against; and the `String`/`List` leaves in
  stdlib bodies. Step 2 in §10.1 owns all of them, and D10 is the fallback for `Path` if a twin proves
  unreasonable.

### 9.8 What it deletes, keeps and adds

**Deleted:** `RowElaborator` and `RowElaborationProcessor`'s elaboration half; the carrier-minting,
pinning and supplying halves of `EffectSugarDesugarer`; `EffectLifter`; `IdNormalizer` and
`assertNoIdResidue`; `EffectCarrierNaming` and `EffectRowRendering`; `RunBoundaryFunctions`; the
constraint-aware declination and `activeFactKeys` probe in `AbilityImplementationProcessor`; the by-name
constraint fallback in `AbilityResolver`; `RowChecker.fixesCarrier` and the block peel; the dormant
`Computation`/`Row` formers, `CanonicalRow` and `CanonicalStack`; every `*Carrier` type, `Suspend` instance
and cross-lift instance in stdlib and jvm; the `eliot.carrier` package; the compile-track `Id.els` and
`AbortCarrier`.

**Kept:** `RowChecker` (verification) and `EffectAccountingProcessor` as the two verifiers;
`AbilityResolver`, `AbilityImplementationProcessor` (structural match + `where`), `CarrierKindChecker` as the
kind system it is; `WovenRecheck`; the seam-groundness test, re-pointed at dictionary types; the `inferable`
binder mechanism, now minting per row entry and on `data`.

**Added:** the `effect`/`handler`/`with` syntax and their desugar to marker + `data` + `implement`; the
`with` binder in core; dictionary resolution (§9.4); the positivity rule (§9.2); the two platform primitives
with their jvm natives and compile-track twins (§9.6); the twin-less-native check (§9.7); the evaluator's
quiet-stall mode and fuel.

### 9.9 Standing rules, re-read for v6

Rules 1, 2, 6, 7 and 8 of §5 carry over verbatim. Rule 3 is restated: **dictionary binders are ordinary
generics**, solved by first-order unification from the receiver argument, one per row entry, and two
different dictionaries meeting is a mismatch. No join, no lattice, no ordering-sensitive slot decision;
that bug class stays prohibited, and its v6 spelling is the deleted by-name fallback. Rule 4 ("carrier-ness
by tag") has no subject left and is retired. Rule 5 (the whitelist) is **kept**, re-pointed: the desugar and
dictionary resolution may consult only the callee's declared parameter/return types, its declared row and
constraints, and the enclosing definition's hidden parameters — never a sibling argument's expression shape.

## 10. Implementation steps

Three groups: what lands **before** the flag day under the byte-identity gate, the **flag day** as one change
under the behavioural gate, and what follows. The flag day is one change because the ability's self
parameter changes kind (`F[_]` to nullary `H`) and no ability can be both at once; everything else is staged
around that boundary, and the review's finding is that the boundary is now small: the checker already
accepts the target shape, and the two primitives can land dark beside today's carriers.

### 10.1 Before the flag day (each independently landable, byte-identical)

1. **The positivity rule.** `StrictPositivityChecker` treats a type's occurrence as an effect's type
   argument in a stored row (and in a pinned tail, which is the same field today) as negative. Rejects the
   two `Knot` programs of §9.5; every example is unaffected. Fixes the shipped tree, so it goes first.
2. **Compile-time twins for the pure natives.** `String`, `List`, `Path` and `isNull` (`StdlibNativesProcessor`
   covers arithmetic, comparison and `Bool` today). Owed to the refinement channel and `where` regardless of
   v6. `Path` decides D10.
3. **Ambiguity is an error.** `AbilityResolver`'s constraint path reports two in-scope constraints of one
   ability instead of taking the first. Byte-identical on the tree, since nothing today reaches it with two.
4. **The twin-less-native check, in today's spelling:** a native without a twin is reachable only from an
   ability implementation's method body. A reachability processor beside `RecursionCheckProcessor`,
   producing its own fact and a diagnostic naming the two fixes. The measurement in §9.7 is its baseline.
5. **The two primitives, dark.** `raise`/`runThrow`/`runAbort` and the cell behind `runState`/`runWriter` as
   jvm natives with compile-track twins, under names the tree does not yet reference, with a jvm end-to-end
   test that runs `EffectsOrdering`'s two nestings on them and compares the transcript. This is the one
   piece of the flag day with no measurement behind it yet, so it is measured first.
6. **Evaluator: quiet-stall mode and fuel.** A partial-evaluation entry point on the one evaluator in which
   a stuck runtime-only native is a residual neutral rather than the loud stall of the compile track, with a
   fuel budget. No caller yet; tested on runtime bodies directly. The optimiser's engine (A1).
7. **Delete the dormant v4 formers** — `Computation`, `Row`, `CanonicalRow`, `CanonicalStack`, their
   pass-through arms in the evaluator, the quoter, `unify` and both printers (~180 lines). Keep
   `WovenRecheck` and the seam test.
8. **Parser and AST for `effect`, `handler` and `with`**, landed dark: parsed into `ast.fact` nodes, rejected
   at `core` with "not supported yet". `effect` must stay legal as a module-path segment (`eliot.effect`).
   Lets the TextMate grammar, the IntelliJ plugin, the apidoc renderer and the `eliot-code` skill be prepared.
9. **Author the v6 stdlib, jvm layer, compile-track overlays, examples and `eliot-test` on a branch**, ahead
   of time, so the flag day is a compiler change plus a prepared tree.
10. **Record the behavioural baseline** — the stdout/exit-code transcript of every example jar.

### 10.2 The flag day (one change, behavioural gate)

- **F1 — the desugar.** `EffectSugarDesugarer`: a row mints one hidden value parameter per entry with an
  inferable nullary binder; `effect` produces the ability marker and method defs with the receiver written;
  `handler` produces the `data` and `implement` block; a row on a `data` field mints on the data; a row on a
  parameter wraps the argument; `with` becomes the binder. Delete pinning, supplying and the carrier-reuse
  rule.
- **F2 — deletions.** `RowElaborator`, `EffectLifter`, `IdNormalizer` + `assertNoIdResidue`,
  `EffectCarrierNaming` + `EffectRowRendering`, `RunBoundaryFunctions`, the declination and `activeFactKeys`
  probe, the by-name fallback, `fixesCarrier` and the block peel. `RowElaborationProcessor` keeps only
  `verifyRow` and is renamed to say so.
- **F3 — the checker.** No rigid carrier to lift into, so `tryPureWrap` goes; dictionary resolution fills
  holes post-drain; the capture rule (a rowless lambda referencing a dictionary) is its zero-candidate
  case. Rendering: a dictionary-instantiated type prints as itself (the handler is a name the user wrote) —
  no inverter.
- **F4 — the run boundary.** `SyntheticMainSourceProcessor` emits `main with <platform handlers>`; the jvm
  plugin contributes its default handler list instead of a boundary FQN.
- **F5 — the primitives, referenced.** The step-5 natives take their final names; `Throw`/`Abort`/`State`/
  `Writer` dischargers are body-less in the base and bodied in jvm over them.
- **F6 — accounting.** `EffectAccountingProcessor`'s ride test becomes parameter identity; the "declared
  pure but performs effects" diagnostic stays in the pre-mono verifier.
- **F7 — the tree.** Land the branch from step 9: `Throw`/`Abort`/`State`/`Writer`/`Dep`/`Console`/`Log`/
  `Inf`/`FileSystem`/`Environment`/`Process` as `effect` + handlers; `catch`, `else`, `orRaise`, `constant`
  in the base over the platform-bodied dischargers; delete `eliot.carrier`, `Id.els`, `AbortCarrier`, every
  `*Carrier`; examples and `eliot-test` in handler form. `EffectAbilitySet.els`'s `ability Web[F[_] ~ Console
  & Log]` becomes `effect Web ~ Console & Log`, and the superability closure (`ValueResolver.superConstraints`)
  **expands the row** — one hidden parameter per closed entry — instead of stacking constraints on one binder.
- **F8 — the gate** (§8): behavioural identity on every example, tests green, the fake examples and
  integration classes with no minted carrier, the single-word `eliot-test` case as a `with`, seam
  groundness on dictionary types.
- **F9 — the documents.** Part I rewritten to the v6 design (this Part's §9 is its draft); the CLAUDE.md
  *Effects Are a Channel* cornerstone rewritten; the `eliot-code`, `eliot-layers` and `eliot-jvm-backend`
  skills' effect sections; the `TODO.md` pointer.

If the gate cannot be met, the assessment in §9.2 is wrong somewhere — find where before landing anything,
and do not land a narrowed version (standing rule 2).

### 10.3 After the flag day

- **A1 — the optimisation pass.** Partial-evaluate every monomorphic runtime body with the step-6 evaluator
  under fuel; residualise stuck subterms; measure with `--statistics` and the example sweep. Enabled once the
  twins exist, off by default until measured.
- **A2 — a backend's own exit form.** A microcontroller target picks result-code form or `setjmp` for its
  `raise` native (§9.6); a compiler-internal lowering is **not** planned — if one is ever wanted it is an
  optimisation on ground code, never a language requirement.
- **A3 — the reconsidered work items.** W1 (the "cannot pin a `Suspend`-riding effect" diagnostic) has no
  subject: any effect can be stored. W3's tag is subsumed by `with`. W4 (a `data` field typed by its own open
  carrier binder) has no subject. W2 (the two rule-4 diagnostics) is re-measured: with the elaborator gone
  dictionary resolution's zero-candidate case is the only one left, and it must name the slot.
- **A4 — D4 dissolves** (any effect is storable and suppliable).

## 11. Open decisions

Kept numbers where a Part I cross-reference uses them.

### D3 — `~` and `&` fully in user space (stages 3 and 4)

**D3(b) is decided for effect abilities** (§9.2): an effect ability is a type inhabited by its
implementations, and a handler is a value of it. For ordinary abilities the question stays open and still
blocked on meaning (what `Show` denotes as a value, whether `~` and `where` unify); nothing in v6 forces it.
The phase-order blocker (the superability closure runs at resolve, before operators are structured) still
lands first if the remaining questions are ever answered yes.

### D4 — `Suspend`-riding effects: pinning and supplying

**Dissolves at the flag day.** There is no canonical carrier for an effect to lack; a stored `{Console} Unit`
is handled where it is run. Kept as a number only so §7.2 still resolves.

### D5 — a lambda body at a rowless arrow slot

**Decided: no capture** (§9.2). A lambda at a `{} B` slot receives the enclosing dictionaries as parameters;
a lambda at a rowless slot has none, and an operation call inside it is dictionary resolution's
zero-candidate error naming the slot. A lambda body is therefore not a region of its own: an effect handled
*inside* the lambda (`s -> s.orAbort else ""`) is fine, because `else` supplies its own dictionary; an effect
*performed* inside it needs the slot to say so.

### D6 — flow grades (cross-reference)

Lands better under v6 than under either v3 or v4: a row entry is a hidden parameter with a constraint, so a
grade is a new *kind of entry* in the channel with no representation question at all. B4's "canonical order
decides semantics" no longer exists to be answered.

### D7 — can the post-mono accounting verifier retire?

Default answer still **no**: it is the codegen precondition, the unconditional fail-safe, and the only
verifier that sees ground dictionary types. Dictionary resolution reports most of what it reports, earlier;
that is a reason to keep it cheap, not to delete it. Revisit only with evidence.

### D8 — the handler surface, exact spelling

Recommended and used throughout §9: `effect`/`handler` as a declaration pair mirroring `ability`/`implement`;
`handler h[…](fields): E[…] { clauses }`; `with` infix, subject-first, a binder. Gone from the first draft:
`returning`, `finish`, `resume` and the `return` clause — an implementation has no clause that does not
return, because the exit and the cell are not clauses (§9.6). Alternatives priced: a named `implement`
instead of `handler` keeps one declaration kind but makes effect-ness a shape again (§3.6). **Decision:** the
keywords; the exact spelling of a field-carrying handler is Robert's to confirm at step 8.

### D9 — a mutable cell for stateful handlers

**Decided in the platform's favour, below the line** (§9.2, §9.6): the cell exists, as a native behind
`runState`/`runWriter`, never as a language construct, never reachable except through the effect. Landin's
knot is closed by the positivity rule, not by the cell's absence — the knot ties through a pure reader too.

### D10 — a purity annotation for twin-less pure natives

An axiom without the proof a twin gives. **Deferred, with one named candidate**: `Path`'s natives need a
compile-time `Path` model to twin against (§9.7); if that model is unreasonable, `Path` is the native "pure
and genuinely without a compile-time expression" this decision was waiting for.

## 12. Closed by measurement or decision — do not re-propose

Each of these was tried, measured, or decided, and the record is the reason not to spend the time again.

- **Carrier inference** — a carrier metavariable, a join solver, an `Id`-headed uniform judgment, a mode
  obligation, a post-drain mode resolver. Of 15 fix commits in the v2 window, the four highest-impact were
  one failure: a carrier metavariable captured by first-contact unification. Under v6 the class is still
  prohibited in its restated form (§9.9): dictionary binders are solved by first-order unification from the
  receiver, never joined, never taken by name.
- **The carrier as the injection point** (§6's strategy as the *final* form). Decided 2026-09-06: it chose an
  interpretation by instantiating a type, and every §6/§7 limitation was a symptom. Superseded by §9.
- **A runtime free monad as the effect representation.** Assessed 2026-09-05: a heap tree of closures walked
  by an interpreter, a coproduct-with-injection to compose effects (the row back in the type), and no plain
  spelling of the scoped operations. The one thing it gets right — the interpretation is a *term* — is §9.
- **A handler as a type, with a phantom binder per row entry** (the first v6 draft, 2026-09-06 morning).
  Measured: the bare two-binder case silently resolves both operations to the first binder (`alpha alpha`,
  §9.2), and three further gaps followed from the same choice. Superseded the same day by the dictionary as
  a value, with the receiver written.
- **A compiler-internal lowering as the mechanism for exit and state** (result-code form, parameter-passing
  form, the first draft's §9.6). Decided 2026-09-06: the two are platform natives below the line, like the
  loop. A backend may *use* either form inside its native; the language never contains the pass.
- **v4 as written** (the row leaves the type, the carrier is lowered post-mono but *stays* the mechanism).
  Its measurements are inherited (§9.5's instantiation key; the seam is late enough), its blockers B2–B4
  dissolve under dictionaries rather than being answered, and its B1 options are superseded by "the
  interpretation is a term".
- **A `World` token / threading a fake dependency to sequence and protect I/O.** Not needed in a strict
  core (§9.7); purity is decided by evaluator stuckness.
- **Deleting `EffectLifter` and `CarrierKindChecker` *under v5*.** Measured per arm: five of six live, two for
  soundness. Under v6 `EffectLifter` has no subject and goes at the flag day; `CarrierKindChecker` stays as
  the kind system it is — `verifyCarrierKinds` is still the only thing rejecting a `[F[_]]` binder
  instantiated at a proper type.
- **Replacing concrete pins with ordinary generics *under v5*.** Refuted twice by running the tree, because
  the elaborator *writes* carriers rather than solving binders. Under v6 the stated reason is gone, and the
  replacement is exactly the desugar (§9.4: a stored row mints an inferable binder on the `data`) — not a
  re-proposal, a different premise.
- **Deleting `Id` or its erasure *under v5*.** Never a decision then; under v6 `Id` has no subject.
- **Bounded staging / deferring an instantiation-decided position.** A deferred position is one the
  elaborator writes nothing at, so it cannot write the carrier there either; kept v2 alive for six days.
- **Approximating rule 4 in the elaborator** instead of declaring it in the signature.
- **A relayed slot-mode rule.** Named nothing and handled depth 1 only.
- **Putting the row on `VPi`** (Koka-style). Teaches every unification site, the printer and the `Function`
  native about rows. v6 puts the row on *parameters*, which the Π-former already has.
- **A `type X = {A, B}` row alias with its own AST node.** An `ast.fact.Expression` case is the most
  expensive thing this language can add; §2.4 replaced it with one resolve rule.
- **Discharge markers (`{-E}`).** There is no negative-effect surface; discharge is a call or a `with`.
- **Scanning the dictionary for an ability name.** Replaced by the keyed marker lookup.
- **Resolving a binder by ability name from the in-scope constraints.** The by-name fallback in
  `AbilityResolver`: measured to pick the first of two silently. Replaced by dictionary resolution matching
  ability *and* arguments, with ambiguity an error.
- **Running v4's P2 before P4.** Not separable — and moot: the flag day is §10.2.
- **The `<Ability>Carrier` convention as an explicit declaration** (the former D2). Its premise — that an
  effect *has* a representation — is gone; the property it named ("has a canonical monad transformer") is
  exactly what §9.6 puts below the line.

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
| `testing-effects.md` | substituting effect implementations | `L1`–`L3`, `§2.x` | §6, §7.7, W3 |
| `effects-v5-one-carrier.md` | rows as constraints on one carrier — the subtraction from v3 | `§4 step N`, `§5 Q1`–`Q4`, `§7` | §2.1 (step 1), §2.2 (step 2), §2.4 (§7), §3.8 + §12 (step 4 and Q1), §12's last entry (Q2). Step 3 is §12's "pins with generics" entry |
| `effects-as-channel-v4.md` | the row leaves the type, the carrier leaves the language | `R1`–`R11`, `P0`–`P5`, `Q1`–`Q4`, `§0`–`§11` | superseded by **§9** (v6); its measurements in §9.5, its blockers in §12 |
| `effects-v4-p0-spike.md` | does the `WovenValue` seam know the carrier? | `S1`–`S3` | §9.5's instantiation key; the test is permanent |
| `effects-v4-p2-sizing.md` | sizing the flag day | `§1`–`§5` | §10.2 |
| `effects-v4-flag-day-readiness.md` | is the flag day ready? (no) | `B1`–`B3` | §12 (v4 as written) |
| `effects-syntax-userspace.md` | `~` and `&` as ordinary values | `stage 1`–`stage 4`, `§7.x` | §2.5 (stages 1–2, landed), **D3** (stages 3–4) |

**Citations to Part II's former numbering** (in commits and comments dated before 2026-09-06): *D1* was the v4
decision and *B1*–*B4* its blockers — now §9 and §12; *D2* was the `<Ability>Carrier` declaration — §12's last
entry; *W1*–*W4* were the v5 work items — §10.3 A3; the "2026-09-05 B1 assessment" is condensed into §9.2.
The first v6 draft (commit `ad4cc04`, the morning of 2026-09-06) spelled handlers as *types* with `returning`,
`finish`, `resume` and a compiler lowering; its §9.6 is now §12's "compiler-internal lowering" entry and its
binder-per-entry desugar §12's "handler as a type" entry.

Two older citations in the tree — `docs/effect-lift-in-checker.md` and `docs/effectful-signatures.md` — point
at documents retired before these and are likewise historical.
