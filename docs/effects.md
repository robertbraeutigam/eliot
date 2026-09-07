# Effects in Eliot — the design, and what is left to do

**Status (2026-09-06): the v5 effect system is shipped, this is its single document, and Part II is the decided
plan to replace its carrier with implementation records — effects as abilities, an implementation as a value
applied by `with` (v6).** Part I describes the tree as it is until §10's flag day lands. This document replaces ten separate notes — the v2, v3, v4 and v5 designs, the three v4 measurement notes, the row-tails note, the
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
as the injection point with a handler *value* installed by `with` — every limitation below (the lifting wall, the
region rule, one interpretation per type argument) is a symptom of choosing an interpretation by instantiating a
type, and disappears with it.

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

# Part II — The plan: effects are abilities, and an implementation is a value (v6)

**Status (2026-09-06): decided.** Part I stays the authoritative description of the tree *until the flag day in
§10 lands*; nothing in Part I is amended in place before then (standing rule 1). This part is the design that
replaces it, the reasoning that produced it in condensed form, the implementation steps, and the decisions
still open. It supersedes the former D1 (v4, "does the row leave the carrier behind?"), its blocker B1, D2 (an
ability declaring its carrier), and — recorded in §9.0 as a reversal, not a refinement — the **first draft of
this part**, written earlier the same day, which put the handler at the type level and lowered control flow in
a post-monomorphization pass. Part III says how to read a citation to any of them.

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
implementation record at the `WovenValue` seam a known constructor. One thing is **measured, not gated**: the
flag day trades today's erased ability calls for indirect calls through a record until A1 (§10.3) restores
specialisation, so the sweep also records each jar's size and the bytecode instruction count of its `main`
class, and the flag-day commit states the regression it accepts.

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

## 9. The decision — an implementation is a value

### 9.0 What changed since the first draft — the reversal record

The first draft of this part (commit `ad4cc043`, 2026-09-06 morning) already decided that *the
interpretation is a term*: a handler chosen at a `with` site, no carrier, whole-program constancy. It encoded
the handler as a **type**: a `handler` declaration desugared to a nullary marker type plus an `implement`
block, a row minted one hidden type binder per entry, `with` was an explicit type argument, and — because a
type cannot close over a runtime value — a finishing or stateful handler needed a **post-monomorphization
lowering pass** into result-code and parameter-passing form, with `returning`, `finish`, `resume` and a
`return` clause as its surface. The same day's discussion asked why the binder exists at all, and the answer
was: for one reason only, so that the handler is part of the monomorphization key and the operation call
erases. Everything else about the binder — the unification, the "two handlers meeting is a mismatch" rule,
"one environment per stored computation", the result function, the ride test — was consequence, not cause.

**Decision (Robert, 2026-09-06): the implementation is a value, not a type.** Specialisation moves from the
checker to the monomorphization key (§9.7, A1), and in exchange the binder, the lowering pass, the handler
result function and the four handler keywords are deleted before they are built. The five points agreed with
it: abilities and effects become **one** mechanism (§9.1); `implement` is a named record and the two-site
search is its registration as a default (§9.3); a row desugars to positional record parameters, written at
every call by the name of the effect, with no minted binder (§9.4); the platform owns **three** primitives —
an escape, a cell, a loop — and `Throw`/`Abort`/`State`/`Writer`/`Inf` are records over them (§9.6); and a
record that captures an escape may not outlive its `with` (§9.5). Two further points were settled in the same
discussion: forwarding is **lexical, by declaration only** — never a sum over a monomorphized sub-graph
(§9.5) — and the platform's default instances are found by the ordinary two-site search consulted **at the
synthesized `main` and nowhere else** (§9.5). Each is marked **decision** where it is stated below.

### 9.1 The model, in five sentences

An **ability** is a record type: its operations are the fields, and an **implementation** is a value of that
type. An **effect** is an ability whose implementation is never searched for at a call site: it is received
as a parameter, declared by the row, and forwarded to callees that declare it, until the synthesized entry
point supplies the platform's instance. `with` applies an implementation to a computation, so a test's fake
is an ordinary named record and needs no type, no colocation and no coherence question. Rows stay the user
surface and the verifiers' vocabulary — `derived ⊆ declared` per definition — and a row on a stored
computation makes it a closure over the records it still needs. There is no carrier: sequencing is strict
evaluation order, a non-local exit and a threaded value are two platform-private primitives no Eliot body can
express, and specialisation is the monomorphizer keying on a record argument it has reduced to a constructor.

### 9.2 What was decided, and why (condensed from the 2026-09-05 and 2026-09-06 assessments)

- **The problem was never "instances may live in only two places".** §6 already lets a test submit an
  interpreter, but only by conjuring a *type* and hanging instances on it — dependency injection routed
  through the type system. Every §6/§7 limitation is a symptom of that: a fake is monomorphic at one carrier
  and gets no lifting (the n² wall); a fake run needs a carrier-free region or the W3 tag; one type argument
  decides the interpretation of *every* effect at once. Named precisely, the carrier does three jobs —
  sequencing representation, discharge-stack representation, selector of interpretation — and the third is
  the misfit.
- **Runtime free monads: rejected** (§12). What they get right — the interpretation is a *term* — is this
  decision. The nearer neighbour is algebraic effects and handlers (Koka, Effekt, Unison's abilities), where
  tail-resumptive and abortive operations — every effect Eliot has — compile to plain calls.
- **Decision: the interpretation is a term.** An implementation record at a `with` site; the platform's
  instances are the records the synthesized entry applies.
- **Decision: abilities and effects are one mechanism, and the model is stated abilities-first.** An
  `ability` is what it is today: a marker plus one body-less method def per operation. An `implement` block
  is what it is today plus one thing: beside its concrete methods it defines a **def whose value is a
  constant record** of those methods, and an operation is a call through that record. An implementation in
  one of the two sites — the ability's module or the binder type's module — is **searched for by default**,
  at a use whose type arguments are ground, exactly as today. A `~ Show[T]` constraint and a `{Console}` row
  entry are both parameters receiving such a record; they differ in **one bit**: whether an unsupplied use may
  be defaulted silently at its own site. For an ability it may; for an `effect` it may not, anywhere but the
  synthesized `main`. That bit is the whole of effect-ness — one predicate at the one place a missing record
  would be defaulted, not a second resolution path — and it is why effects are **declared** as such: without
  it `def greeting(name: String): Unit = printLine(…)` would find the jvm `Console` at its first ground use
  and compile with no row. It is also what makes "an implementation cannot be passed around" — true of
  abilities today — false for both at once.
- **Decision: the bit stays; "everything is passed" is closed** (§12). Removing the default so that no ability
  is ever searched inside a function gives no capability the plan lacks — a function that wants an instance
  overridable declares it (D11) and it is a passed parameter — and costs the **declaration burden**: every
  `==` is an `Eq`, every `++` a `Combine`, every `match` a `PatternMatch[T]`, so `def isEmpty(s: String): Bool`
  becomes `{Eq[Int]} Bool` and every caller inherits it transitively, drowning the `Console` beside it. The
  criterion behind the bit is **canonicity**: a ground instance is a *fact* (there is one `Eq[Int]`), so
  search says nothing wrong; an effect's implementation is a *choice*, so the row says something. And the
  search cannot be deleted in any case: the compile track dispatches `Meta[Interval[T]]`,
  `Numeric[Bound[T]]`, `PatternMatch` and `TypeMatch` from compiler machinery with no call chain and no `main`
  to bubble to. Haskell is the precedent — dictionary passing, resolved at the ground site, forwarded only
  where the type is abstract, `IO` bolted on as the missing bit; Scala 3's `using` plus a global-less
  `CanThrow` is the same pair from the other side.
- **Decision: the handler is a value, so there is no binder** (§9.0). What the type-level encoding bought —
  erasure on day one — is recovered by A1; what it cost — a binder, unification, an environment rule, a result
  function, a lowering pass, four keywords — is not built.
- **Decision: forwarding is lexical, by declaration.** An implementation reaches a call only through a
  declared parameter. The alternative — computing the closure of abilities used in each monomorphized
  sub-graph and letting a `with` override transitively — is dynamic scoping resolved at compile time: the
  carrier's third job in a new costume, one binding site selecting the interpretation of everything beneath it
  whether or not the code in between said so. Effects had this argument (§12, "approximating rule 4");
  abilities inherit the settlement.
- **Decision: the platform's defaults are found by the ordinary two-site search, at the synthesized `main`
  only.** Localised magic: the user never writes `with` for a platform effect, the platform never knows what
  `main` uses, and a default consulted anywhere else would silently discharge `Console` inside `greeting`.
- **Decision: three platform-private primitives, no lowering pass** (§9.6). A non-local exit and a threaded
  value are exactly what a strict pure core cannot express and what every target has: a jump, a register, a
  loop.
- **Decision: resumption is tail-resumptive or abortive, nothing else.** An operation either returns (a call)
  or exits to the `with` site. Multi-shot and non-tail resumption — generators, async, coroutines — are out.
- **Decision: purity is decided by the evaluator, not declared** (§9.7). Unchanged from the first draft.

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

**An effect is an ability declared with the `effect` keyword and no carrier binder.** The `[F[_]]` existed
only because something had to be the monad. Inside an `effect` block every operation performs that effect by
definition; the `{E}` on each method is gone. An `ability` is unchanged.

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

**An implementation is a record, and `implement` writes one.** An `implement` block in one of the **two
sites** — the ability's module or the type's module — is the **default** for its pattern, subject to the
existing coherence and `where` rules, and is what a constraint at ground type arguments resolves to. The
platform's `implement[F[_] ~ Suspend] Console[F]` becomes the default instance of the nullary `Console`, in
the same module path the jvm layer already uses:

```eliot
implement Console {
   def printLine(s: String): Unit = printLineInternal(s)
   def readLine: Option[String] = lineOrNone(readLineInternal)
}
```

**A named `implement` is a value and never a default.** It may live anywhere, may take parameters like a
`def` (Leijen's parameterised handler as an ordinary function returning a record), and its clauses may
declare rows — a clause row makes the record a **function of those records**, applied at the `with` site
from its ambient. A test's fake is therefore one declaration in the test module, with no type minted:

```eliot
implement recordingConsole: Console {
   def printLine(s: String): {Writer[String]} Unit = tell(s ++ ";")
   def readLine: Option[String] = None
}

implement recovering[E, A](k: Exit[A], f: E => A): Throw[E] {
   def raise[B](err: E): B = exit(k, f(err))
}
```

**One run form.** `with` applies an implementation to a computation, subject-first, so it reads as `catch`
does today. Every discharge word is an ordinary def that builds a record and applies it:

```eliot
def transcript: String = written(greeting("Bob") with recordingConsole)
def demo: Pair[String, String] = runState("first", swap("second"))
def safe: Configuration = parse(config) catch (_ -> emptyConfiguration)
def value: Option[String] = runAbort(allowed)
def sorted: List[Int] = sort(xs) with reverseOrd
```

`catch`, `else`, `runThrow`, `runAbort`, `runState*` and `written` **keep their names and signatures** in
the base — a `G[_] ~ Effect` binder and a `G[…]` return become the plain payload — and lose their bodies there
(§9.6). The last line is the unification made visible: `with` works on an ability exactly as on an effect,
overriding the default for the calls that declare it.

**A stored computation is a row-typed field with no tail.** `data TestCase(body: {Throw[E]} Unit)`; the
`| Id` pin disappears. Its row lists the records it still needs, and *where it is run is where they are
applied*; a computation stored with its effect already applied is `{} Unit`. The lexical-versus-dynamic
handler question therefore has a definite answer written in the type.

**What disappears from the user's world:** the whole `eliot.carrier` package (`Effect`, `Suspend`,
`flatMap`, `pure`, `map`, `suspend`); every `<Ability>Carrier` type and `run*Carrier` accessor; the
cross-lift instance matrix; `~ Effect` and `~ Suspend` on definitions; `Id` and `runId`; pinned tails; the
`{| Recorded}` capture tag; the fake-carrier recipe; the "a discharger must be called directly" rule (§3.5) —
a computation is a closure, so `p.runStateToPair(s0)` passes it through the dot's plain `T` as the value it
is. Rule 2's "carrier-typed position" and rule 4's four carrier namings collapse to one predicate: a slot
either has a row and takes records, or it does not and is a payload.

### 9.4 The core desugar — everything is still a def, and a row is a parameter list

The front end fits the current core with **no new expression node**. **Decision:** a row desugars to
positional **value** parameters of record type, one per entry, in a leading prefix; a call writes them by the
**name of the ability**, from the ambient; there is no minted binder.

| construct | desugars to |
| --- | --- |
| `effect E[…] { def op(…): R }` / `ability A[…] { … }` | a record type whose fields are the operations, plus the ability marker `Qualifier.Ability` for the two-site registry; an operation `op` is a field selection on a record of that type. `raise[A]` is a **polymorphic field** — a Π-typed field, which `VPi` as the one primitive former admits (§10.1 step 6 confirms it on both tracks) |
| `implement E[…] { clauses }` in a two-site module | a record value, registered as the default for its pattern — `ImplementBlock`'s `Qualifier.AbilityImplementation` unchanged |
| `implement name[…](params): E[…] { clauses }` | a `def name[…](params): E[…]` whose body is the record; a clause row `{W}` adds a `W` record parameter to the def, so `with name` applies the ambient `W` first |
| a row `{E1, E2}` on a def's return | two leading parameters `e1: E1, e2: E2`; the body's **ambient** for those abilities is those parameters |
| `[T ~ A[T]]` on a def | the same parameter, with the two-site default written when nothing is supplied and the arguments are ground — what `AbilityResolver` does today, unchanged |
| a row on a **top-level parameter** `p: {E1} A` | `p: E1 => A`; the callee **always passes** — its own record for an entry its row has (`if`'s `value: {Abort} T`), a record it builds for one it lacks (`else`'s `computation: {Abort} A`); no "supplied vs rides" rule |
| the empty row `{} A` on a parameter | `Unit => A` — suspension; the actual's interior captures the caller's ambient |
| a row on an arrow codomain `f: X => {E} B` | `X => E => B`; `X => {} B` is `X => B` with capture of the ambient **permitted** in the lambda body |
| a rowless arrow `X => B` | `X => B` with capture **not** permitted (rule 4; D5 decides the lambda body's region) |
| a row on a `data` field | the same function type as the field's type; construction uses the slot rule below |
| an actual at a row-typed slot | if it already has the slot's type, passed as is; otherwise **abstracted over the slot's row**, the abstracted records being the ambient of its interior — one rule for `catch`'s first parameter, a `data` field and `with`'s subject alike (today's "captures at carrier-headed slots") |
| an operation call `op(args)` | a call through the record found by the **resolution order**: the nearest enclosing row binding — the def's own row or constraint, or a slot abstraction — else, for an ability, the two-site default at ground arguments; else the "performs but does not declare" error at the call (for an effect, always, except at the synthesized `main`) |
| `c with h` | `handle(c, h)`, `def handle[E, A](c: {E} A, h: E): A = c(h)` — an application whose subject slot is governed by the actual-at-a-row-typed-slot rule like any other; **compiler-known for one reason only**, that its slot's row is read from `h`'s type instead of from a signature |

**The context belongs to the slot, not to `with`.** Take the rule apart with something that is a plain def
under this table:

```eliot
def withShowInt[A](computation: {Show[Int]} A, impl: Show[Int]): A = computation(impl)

def greeting(i: Int): Unit = printLine(i.show).withShowInt(myIntShow)
```

`withShowInt`'s first parameter has a row, so the actual `printLine(i.show)` is abstracted over a
`Show[Int]` record, and inside that abstraction `show` finds the abstracted record before it falls back to
the search. Nothing here is `with`; it is the slot rule that makes `catch`, `else`, `if` and `runState` work
— the operation inside the argument finds the record the callee supplies — together with the resolution
order above, which is scope-aware by construction. `with` is that def with the ability name filled in from
its second argument: `{E} A` with `E` ranging over ability types is not a row the syntax can spell (D3), so
the desugar types `h` first and abstracts `c` over `h`'s type. That single step is the whole of `with`'s
special status; it is sugar over a construct the compiler must understand anyway. If D3 ever lets a row entry
be a type parameter, `with` becomes the stdlib def above with no change in meaning, and the compiler forgets
it exists. Per-ability forms like `withShowInt` are legal user code from day one and are the test of the rule.

Types are values, so an ability's name being the type of its implementations is the cornerstone working for
the design, and the former D3(b) question — "is an ability a type inhabited by its implementations?" — is
**answered yes by construction** rather than avoided. The orphan rule keeps its subject (a *default* must be
in one of the two sites); a named record is not an instance and is not checked for overlap.

**Both verifiers keep their vocabulary.** The pre-mono `RowChecker.verifyRow` becomes a **scope check** on
records: an operation or a rowed callee needs a record, and the only places one can come from are the
enclosing def's row or constraints, an enclosing `with`, or a capture the slot permits. That check is
complete before monomorphization, since nothing about it is instantiation-dependent. The post-mono
`EffectAccountingProcessor` is kept through the flag day as the codegen precondition it is, and retires only
under the §8 method (D7).

### 9.5 Semantics

- **An operation call** inside a region is a field call on the record its ambient holds; the clause computes
  and returns, and evaluation continues after the call. **Effects run where they are written** is strict
  application, nothing more.
- **Forwarding is lexical (decision).** A def's declared rows and constraints are the ambient records for
  its body. `expr with h` extends or overrides the ambient for `expr`; a nearer `with` rebinds. Every call
  inside a region that declares an ability receives the ambient record for it. A call that declares an
  ability the ambient lacks takes the two-site default at its ground type arguments; if there is none, it is
  the "performs but does not declare" error. Every one of these is answered from the enclosing def's signature
  and the enclosing `with`s: **there is no graph to sum.** A ground `printAll(xs: List[Int])` that declares
  nothing resolves `sort`'s `Ord[Int]` to the default at that call, and `printAll(xs) with reverseOrd` is an
  error because `printAll` has no `Ord[Int]` parameter to receive it in — the same discipline the row imposes
  today, and what makes a def's behaviour readable from its own signature.
- **Where `with` may be placed** follows: anywhere a record parameter exists to fill, which is exactly three
  places. At the **operation itself** (`i.show with myIntShow` — `show` is the ability's method def and its
  record is its hidden first parameter); around **any subexpression that lexically contains the use**
  (`printLine(i.show) with myIntShow`, the subject being abstracted and the abstraction being its interior's
  ambient); and at a **call to a function that declares the ability**, generically (`greeting[T ~ Show[T]]`)
  or at a ground type (D11). Not at `greeting(5) with myIntShow` against a `greeting(i: Int): Unit` that
  declared nothing: the error names the fix. In one sentence, **`with` reaches every use inside its subject's
  own text, and crosses a def boundary only through a declaration.** `with` binds looser than application and
  the `.` pipe, so `xs.sort.render with reverseOrd` applies to the whole chain, and `c with a with b` is
  `(c with a) with b`, an inner `with` for the same ability shadowing the outer within its subject.
- **Exit and threading.** A record built over the escape primitive makes its `finish` the value of the
  enclosing `escape`; a record built over a cell threads a value through calls that never mention it.
  **Nesting order at the run site decides interaction**: `runState(s, runThrow(c))` versus
  `runThrow(runState(s, c))` is the difference between state surviving a `raise` and not — written by the
  user where the records are applied, with no canonical form to fix.
- **Storage.** A computation is a closure over the records it still needs; storing it, passing it through a
  plain generic, and running it later under any implementation are all ordinary. The one hazard rule
  (**decision**): a record that **captures an escape** — `recovering(k, f)` and every record a discharger
  builds — may be passed and closed over, never stored in a `data` field or returned past its `with`. A
  dangling exit is the failure; the check is syntactic at the desugar, since the record types are
  compiler-known, and the fail-safe on the JVM is a loud uncaught exception, never silence. Plain ability
  records (`Show`, `Ord`) and effect records that exit nowhere (`Console`, `Log`, `Dep`) have no such hazard.
- **Instantiation.** A definition is monomorphized per payload type arguments as today; a record argument is
  a value and does not key the instantiation **until A1** (§9.7). Every `with` site is a term reachable from
  `main`, so at the `WovenValue` seam every record is a known constructor — the seam test — which is what A1
  keys on.
- **The run boundary (decision).** The synthesized entry point is ordinary code: it reads `main`'s row and
  applies, for each entry, the instance the **two-site search** finds — the same rule as everywhere, consulted
  for a row entry **only here**. A miss is an error at the boundary naming the effect and the fix (discharge
  it). `SyntheticMainSourceProcessor` shrinks; `RunBoundaryFunctions` is deleted. A consequence to state
  plainly rather than let arrive silently: for a *parameterised* effect the type site exists, so
  `implement Throw[ConfigError]` in `ConfigError`'s module becomes the boundary handler for an undischarged
  `Throw[ConfigError]` — a capability the tree does not have today (D12).
- **`Inf`** is the platform's default instance of the `Inf` effect (`forever` over the loop primitive), and
  stays the one effect that reaches `main` and is handled by the platform alone.
- **"The fake cannot cheat" survives without `Suspend`.** A user module cannot declare a native, and the
  platform's natives and primitives are private to its layer, so a test's record reaches I/O only through
  effects its own clauses declare in their rows — which are applied, and charged, at the `with` site.

### 9.6 The three primitives — where the two control-flow transformations live

A resuming clause is a call and needs nothing. A finishing clause is a **non-local exit**; a stateful record
needs a value **threaded through calls that never mention it**. Neither is expressible as a def in a strict
pure core — today both are expressed by the transformer instances (`ThrowCarrier`'s `flatMap` is the exit,
`StateCarrier`'s the threading), which is the whole reason the carrier existed. **Decision:** they are three
**platform-private leaves**, one per target, and nothing else:

| primitive | shape | jvm | microcontroller | compile track |
| --- | --- | --- | --- | --- |
| escape | `escape[R](body: Exit[R] => R): R`, `exit[R, A](k: Exit[R], r: R): A` — abortive, never re-entered | an exception | a status flag and a jump (A2) | an evaluator intrinsic |
| cell | `withCell[S, A](initial: S, body: Cell[S] => A): Pair[A, S]`, `read`, `write` — scoped to one call | a local | a register | an evaluator intrinsic |
| loop | `foreverInternal` as today | `while(true)` | the super-loop | never runs (`Inf` is stuck) |

**They are private, and the dischargers are therefore abstract in the base.** A public cell is Landin's
knot — a cell holding a closure that reads the cell is a loop, and `termination/PurityGuardTest` exists to
keep it out — so `withCell` may not be a base name, and `escape` follows for uniformity. Consequently
`runThrow`, `catch`, `else`, `runAbort`, `runState*` and `written` are **body-less signatures in the base**
(`docs/effects.md` Part I's rule that the base carries no representation-dependent body) and are **bodied
per platform** over that platform's private primitives — small Eliot defs over a trivial leaf, the
"minimize Scala, decompose in Eliot" shape — and in `stdlib/eliot-compiler/` over the evaluator's
intrinsics for the compile track, where they replace today's `AbortCarrier` overlay. What the compile track
did by hand with `Either[String, _]` the evaluator now does directly.

```eliot
-- jvm/eliot/eliot/effect/Throw.els
def catch[E, A](computation: {Throw[E]} A, onError: E => {} A): A =
   escape(k -> computation with recovering(k, onError))
```

The honest statement: the transformer monads do not vanish, they shrink to two leaves per target, outside the
library's bodies, the type system and the user's scope — and there is no pass.

### 9.7 Optimisation — specialisation is the mono key; purity is what the evaluator can reduce

**Specialisation.** Today an ability call erases because the instance is resolved at monomorphization; under
v6 an operation is a field call through a record parameter, an indirect call until specialised. **Decision:**
the flag day accepts that, measured (§8), and **A1** restores erasure by widening the monomorphization key:
a definition is instantiated per **record argument the evaluator has reduced to a constructor**, keyed
`(vfqn, payload arguments, record constructors)` — the v4 seam finding with "record constructors" in place of
"carrier stack". Under whole-program compilation every record flowing into a call is such a constant per code
path, since every `with` site is a term reachable from `main`; the only runtime part of a record is a field
of a known constructor (`recovering(k, f)`'s `f`), which specialises the call and leaves the field. A stored
`{} Unit` is a closure over a constant and reduces by the same act, later. This is the existing mechanism
with one more key component, not a new pass, and it is more faithful to the cornerstone than the binder was:
"type-level" is only "when it is forced", and a record that is a constant at the boundary is forced at
compile time either way.

**Purity.** The first draft's rule stands. A term is pure iff the one NbE evaluator reduces it; a term is
the World iff it is stuck on a runtime-only native; a compile-time twin is a proof of purity, axiomatic as a
meta transfer is; a stuck call keeps its position and its multiplicity, and everything reducible may be
folded, deduplicated, inlined, deleted when unused, or evaluated at another time. `Inf` is native, so the
evaluator never loops; fuel bounds a legal large pure fold, and out-of-fuel means "leave the code as
written". Records make it better: an effectful definition is pure code parameterised by its records, so
`greeting("Bob") with recordingConsole` reduces to the transcript at compile time and under the jvm instance
is stuck at `printLineInternal`, in place, once. A nullary definition that reduces is a constant; one that is
stuck — every rowed one, being a function of its records — is re-evaluated at every reference, which is rule
1 by construction.

**Rule: a native without a compile-time twin may be called only from an `implement` clause.** Today nothing
forces a native-calling definition to declare a row (`def now: Int = currentTimeMillisInternal` hides an
effect from both verifiers; the `suspend` wrapper was the informal guard and goes with the carrier). The
check is a reachability check on runtime bodies like the recursion gate; the error names the two fixes. Every
I/O native in the jvm layer is already inside an `implement` block; the pure natives without twins —
`String`'s and `List`'s — need one, which the refinement channel and `where` already want.

### 9.8 What it deletes, keeps and adds

**Deleted:** `RowElaborator` and `RowElaborationProcessor`'s elaboration half; the carrier-minting,
pinning and supplying halves of `EffectSugarDesugarer`; `EffectLifter`; `IdNormalizer` and
`assertNoIdResidue`; `EffectCarrierNaming` and `EffectRowRendering`; `RunBoundaryFunctions`; the
constraint-aware declination and `activeFactKeys` probe in `AbilityImplementationProcessor`;
`RowChecker.fixesCarrier`, the derivation rules and the block peel; the dormant `Computation`/`Row` formers,
`CanonicalRow` and `CanonicalStack`; every `*Carrier` type, `Suspend` instance and cross-lift instance in
stdlib and jvm; the `eliot.carrier` package; the compile-track `Id.els` and `AbortCarrier`; the "a
discharger must be called directly" diagnostic. **Not built** from the first draft: the handler marker type,
the per-entry type binder, the handler result function, the lowering pass, `handler`/`returning`/`finish`/
`resume`/`return`.

**Kept:** `RowChecker.verifyRow` as the scope check and `EffectAccountingProcessor` (through the flag day,
D7); `AbilityResolver` and `AbilityImplementationProcessor` (structural match + `where`) for the two-site
default; `CarrierKindChecker` as the kind system it is; `WovenRecheck`; the seam-groundness test, re-pointed
at records.

**Added:** the `effect` keyword, the named and parameterised `implement`, and `with`, with their desugar to
record types, record values and `handle`; the row-to-parameters desugar and the ambient-by-name call
writing; the actual-at-a-row-typed-slot rule; the escape-capture storage check; the three primitives per
platform and the two evaluator intrinsics; the boundary rule in `SyntheticMainSourceProcessor`; the
twin-less-native check (§9.7); the evaluator's quiet-stall mode and fuel; A1's key widening.

### 9.9 Standing rules, re-read for v6

Rules 1, 2, 6, 7 and 8 of §5 carry over verbatim. Rule 3 is restated: **there is nothing to infer** — a
record parameter is filled by the ambient, by `with`, or by the two-site default at ground arguments, in that
order, and never by a join, a lattice, an ordering-sensitive slot decision, or a sum over a sub-graph; that
bug class stays prohibited in both its forms. Rule 4 ("carrier-ness by tag") has no subject left and is
retired. Rule 5 (the whitelist) is retired with the elaborator; the desugar consults declarations only, and
there is no component that could accrete a sibling rule.

## 10. Implementation steps

Three groups: what lands **before** the flag day under the byte-identity gate, the **flag day** as one change
under the behavioural gate, and what follows. The flag day is one change because the ability's self
parameter changes kind (`F[_]` to none) and no ability can be both at once; everything else is staged around
that boundary.

### 10.1 Before the flag day (each independently landable, byte-identical)

1. **Evaluator: quiet-stall mode and fuel.** A partial-evaluation entry point on the one evaluator in which
   a stuck runtime-only native is a residual neutral rather than the loud stall of the compile track, with a
   fuel budget. No caller yet; tested on runtime bodies directly. This is A1's engine; it is no longer on the
   flag-day path.
2. **Compile-time twins for the pure natives.** `String` and `List` leaves (`StdlibNativesProcessor` covers
   arithmetic, comparison and `Bool` today). Owed to the refinement channel and `where` regardless of v6.
3. **The twin-less-native check, in today's spelling:** a native without a twin is reachable only from an
   ability implementation's method body. A reachability processor beside `RecursionCheckProcessor`, producing
   its own fact and a diagnostic naming the two fixes. Measured first by listing every native and its call
   sites; the jvm layer should already satisfy it.
4. **Delete the dormant v4 formers** — `Computation`, `Row`, `CanonicalRow`, `CanonicalStack`, their
   pass-through arms in the evaluator, the quoter, `unify` and both printers (~180 lines). Keep
   `WovenRecheck` and the seam test.
5. **Parser and AST for `effect`, the named/parameterised `implement`, and `with`**, landed dark: parsed
   into `ast.fact` nodes, rejected at `core` with "not supported yet". Lets the TextMate grammar, the IntelliJ
   plugin, the apidoc renderer and the `eliot-code` skill be prepared, and makes the flag-day diff smaller.
6. **A polymorphic record field on both tracks.** `raise[A](err: E): A` is the one operation with its own
   type parameter, so a `Throw[E]` record has a Π-typed field. Confirm with a small test that a `data` field
   may carry one, that the checker instantiates it per use, and that the jvm backend erases it as it erases a
   generic native. Expected to hold — `VPi` is the one primitive former — but it is the single place the
   record encoding touches the type system, and it is checked before anything depends on it.
7. **The two evaluator intrinsics** — escape and cell — on the compile track, tested directly. They replace
   the `Either`-based `AbortCarrier` overlay at the flag day.
8. **Author the v6 stdlib, jvm layer, compile-track overlays, examples and `eliot-test` on a branch**, ahead
   of time, so the flag day is a compiler change plus a prepared tree rather than one long day of both.
9. **Record the behavioural baseline** — the stdout/exit-code transcript of every example jar, plus each
   jar's size and `main`-class instruction count — so the flag-day gate has something to compare against.

### 10.2 The flag day (one change, behavioural gate)

- **F1 — the desugar.** `EffectSugarDesugarer` becomes the row-to-parameters desugar of §9.4: `effect` and
  `ability` produce a record type and the marker; a two-site `implement` produces a default record; a named
  `implement` produces a def returning one; a row on a def, a parameter, an arrow codomain or a `data` field
  produces the function type in the table; an operation call selects on the ambient; `with` is `handle`; the
  actual-at-a-row-typed-slot rule abstracts; the escape-capture storage check. Delete carrier minting,
  pinning, supplying and the carrier-reuse rule.
- **F2 — deletions.** Everything in §9.8's deleted list. `RowElaborationProcessor` keeps only `verifyRow`,
  rewritten as the scope check, and is renamed to say so.
- **F3 — the checker.** No rigid carrier to lift into, so `tryPureWrap` goes; an operation call is a field
  call; `handle` is a def. Rendering: a record type prints as the ability's name the user wrote — no inverter.
- **F4 — the run boundary.** `SyntheticMainSourceProcessor` reads `main`'s row and applies the two-site
  default per entry, erroring on a miss; the jvm plugin contributes nothing but its `implement` blocks.
- **F5 — the primitives.** The three jvm leaves, private; the platform bodies of the dischargers over them;
  the compile-track overlay bodies over the step-7 intrinsics.
- **F6 — accounting.** `EffectAccountingProcessor` reads records instead of carriers and stays the codegen
  precondition; the "declared pure but performs effects" diagnostic stays in the pre-mono scope check.
- **F7 — the tree.** Land the branch from step 8: `Throw`/`Abort`/`State`/`Writer`/`Dep`/`Console`/`Log`/
  `Inf` as `effect`s; the dischargers abstract in the base and bodied in jvm and the compile-track overlay;
  the jvm default `implement`s; delete `eliot.carrier`, `Id.els`, `AbortCarrier`, every `*Carrier`;
  examples and `eliot-test` in record form.
- **F8 — the gate** (§8): behavioural identity on every example, tests green, the fake examples and
  integration classes with no minted carrier, the single-word `eliot-test` case as a `with`, seam groundness
  on records, the size and instruction-count regression recorded in the commit.
- **F9 — the documents.** Part I rewritten to the v6 design (this Part's §9 is its draft); the CLAUDE.md
  *Effects Are a Channel* cornerstone rewritten; the `eliot-code`, `eliot-layers` and `eliot-jvm-backend`
  skills' effect sections; the `TODO.md` pointer.

If the gate cannot be met, the assessment in §9.2 is wrong somewhere — find where before landing anything,
and do not land a narrowed version (standing rule 2).

### 10.3 After the flag day

- **A1 — specialisation.** Widen the monomorphization key to record arguments reduced to a constructor
  (§9.7), driven by the step-1 evaluator under fuel; residualise stuck subterms. Measured against the step-9
  size and instruction-count baseline; the target is today's erasure or better, and the example sweep is the
  behavioural gate again.
- **A2 — backend exit primitive**, if a microcontroller target replaces the jvm exception with a status flag
  and a jump; the primitive's shape (§9.6) does not change.
- **A3 — the reconsidered work items.** W1 (the "cannot pin a `Suspend`-riding effect" diagnostic) has no
  subject: any effect can be stored. W3's tag is subsumed by `with`. W4 (a `data` field typed by its own open
  carrier binder) has no subject. W2 (the two rule-4 diagnostics) is re-measured: with the elaborator gone the
  scope check's error is the only one left, and it must name the slot.
- **A4 — D4 dissolves** (any effect is storable and suppliable); **D5** is re-decided in v6 terms (§11).
- **A5 — retire the post-mono accounting verifier** under the §8 method (D7), once the scope check is shown
  to be what makes it fire on nothing.

## 11. Open decisions

Kept numbers where a Part I cross-reference uses them.

### D3 — `~` and `&` fully in user space (stages 3 and 4)

Still blocked on meaning, not difficulty, but **half of it is now answered**: D3(b), "what an ability denotes
as a value", is settled by construction (§9.4) — an ability is a record type and its implementations are its
values. What is left is whether `~` and `where` unify, and the phase-order blocker (the superability closure
runs at resolve, before operators are structured) still lands first if that is ever answered yes.

### D4 — `Suspend`-riding effects: pinning and supplying

**Dissolves at the flag day.** There is no canonical carrier for an effect to lack; a stored `{Console} Unit`
is a closure applied where it is run. Kept as a number only so §7.2 still resolves.

### D5 — a lambda body at a rowless arrow slot

Still a rule decision, and the value-level model **flips its default**: a lambda at `f: X => B` could
silently close over the enclosing def's records, so rule 4 is no longer a theorem of the encoding but a
scope rule the desugar enforces (§9.4: capture permitted at `{}`, not at a rowless arrow). The question left
is whether a lambda body gets its **own** region — so that an effect applied *inside* the lambda
(`s -> s.orAbort else ""`) is accepted, as it is when the same code sits in a named pure helper. Recommended:
yes, a lambda body is a region whose ambient is what the slot's row declares, and a `with` inside it extends
that; the elaborator's old "become a bind chain on the enclosing carrier" arm has no v6 counterpart. Note
that a `with` inside a `{}` thunk or a lambda body is already a region of its own by the slot rule (§9.4),
since its subject is abstracted; what D5 decides is only what the lambda's *ambient* is before any `with`.

### D6 — flow grades (cross-reference)

Lands better under v6 than under either v3 or v4: a row entry is a parameter, so a grade is a new *kind of
entry* in the channel with no representation question at all. B4's "canonical order decides semantics" no
longer exists to be answered.

### D7 — can the post-mono accounting verifier retire?

Under records the pre-mono scope check is complete — nothing about "which record" is instantiation-dependent
— so the expectation is **yes, after the flag day** (A5), by the §8 method: keep it through the flag day as
the codegen precondition, trace it, retire it when it fires on nothing. Not before, and not on the argument
alone.

### D8 — the surface, exact spelling

**Decision:** `effect` as a declaration keyword mirroring `ability`; a **named** `implement name: E { … }`
and a **parameterised** `implement name[…](params): E[…] { … }` as the record forms; `with` infix,
subject-first. The first draft's `handler`, `returning`, `finish`, `resume` and `return` are **not** built: a
finishing record is a named `implement` over `escape`, its "return clause" is the def that wraps the
`escape`, and a stateful one is the same over the cell. Alternatives priced: an anonymous record-literal
expression (`Console(printLine = …)`) — deferred, a named `implement` covers every case in the tree. `with`
is infix at the loosest precedence, left-associative (§9.5), and is the one construct in the surface that is
compiler-known rather than a stdlib def, for the single reason §9.4 states; a user may always write the
per-ability def instead.

### D9 — a mutable cell for stateful records

Koka and Effekt implement `State` with a handler-local mutable variable. **Decision:** a cell **exists** as a
**platform-private leaf** (§9.6), scoped to one call and never a base name, never first-class. A public cell
is Landin's knot; a private one is a register the platform already has. The first draft's "no cell,
parameter-passing form" is superseded with the lowering pass.

### D10 — a purity annotation for twin-less pure natives

An axiom without the proof a twin gives. **Deferred**: the twins are owed anyway, and an annotation is a
second spelling of the same fact. Revisit if a native is found that is pure and genuinely has no
compile-time expression.

### D11 — the spelling of a ground ability parameter with a default

`[T ~ Ord[T]]` attaches to a generic; a ground `printAll(xs: List[Int])` that wants to be overridable by
`with reverseOrd` must declare an `Ord[Int]` parameter, and the row is the natural place —
`def printAll(xs: List[Int]): {Ord[Int]} Unit` would mean "takes an `Ord[Int]`, defaults to the searched
instance". Under §9.2 that is what a row already means, with effects being the entries with no default.
Whether the two spellings stay separate or merge is a syntax decision for the flag day's F9, not a mechanism
question; the mechanism is one either way.

### D12 — a user-declared boundary default for a parameterised effect

A consequence of §9.5's boundary rule: `implement Throw[ConfigError]` in `ConfigError`'s module is found by
the two-site search and would handle an undischarged `Throw[ConfigError]` at `main`. Consistent and probably
useful; the tree does not have it today. **Decide** before the flag day whether the boundary accepts it or
restricts itself to the platform layer's instances.

## 12. Closed by measurement or decision — do not re-propose

Each of these was tried, measured, or decided, and the record is the reason not to spend the time again.

- **Carrier inference** — a carrier metavariable, a join solver, an `Id`-headed uniform judgment, a mode
  obligation, a post-drain mode resolver. Of 15 fix commits in the v2 window, the four highest-impact were
  one failure: a carrier metavariable captured by first-contact unification. Under v6 the class is still
  prohibited in its restated form (§9.9): a record parameter is filled by ambient, `with` or the two-site
  default, never joined.
- **The carrier as the injection point** (§6's strategy as the *final* form). Decided 2026-09-06: it chose an
  interpretation by instantiating a type, and every §6/§7 limitation was a symptom. Superseded by §9.
- **The handler as a type — the first draft of v6.** A marker type per handler, a hidden type binder per row
  entry, `with` as a type argument, and a post-mono lowering pass into result-code and parameter-passing form
  for what a type cannot close over. Reversed the same day (§9.0): the binder existed only to key
  monomorphization, which A1 does with a value; everything else it forced — unification, the environment
  rule, the result function, the lowering, four keywords — is not built.
- **Transitive `with` / summing abilities over a monomorphized sub-graph.** Dynamic scoping resolved at
  compile time; the carrier's third job in a new costume. Forwarding is by declaration only (§9.5).
- **A runtime handler stack / dynamic scoping at runtime.** Kills erasure and makes storage semantics
  unwritable in a type. The `with` is lexical and a stored computation's row says what it still needs.
- **Full unification — every ability passed, none searched inside a function** (assessed 2026-09-07, §9.2).
  Two readings, both closed. Declared rows for every ground ability use: no new capability over D11, and the
  declaration burden puts `{Eq[Int], Combine[String], PatternMatch[Shape]}` on most monomorphic code,
  transitively, drowning the row's signal. Rows inferred for abilities: that is inference, and it makes
  `printAll(xs) with reverseOrd` reach an undeclared resolution — the transitive `with` above. Under either
  reading the two-site search stays, because the compile track dispatches `Meta`/`Numeric`/`PatternMatch`/
  `TypeMatch` instances from machinery with no call chain; unification would delete one predicate and add
  rows everywhere.
- **`with` as a construct with a resolution mode of its own.** The context is the row-typed slot's (§9.4),
  shared with every discharger; `with` is compiler-known only because its slot's row is generic over
  abilities, and a per-ability `withShowInt` is a plain def today.
- **A public cell or escape in the base.** A public cell is Landin's knot (D9); both primitives are
  platform-private, and the dischargers are abstract in the base for exactly that reason (§9.6).
- **A runtime free monad as the effect representation.** Assessed 2026-09-05: a heap tree of closures walked
  by an interpreter, a coproduct-with-injection to compose effects (the row back in the type), and no plain
  spelling of the scoped operations. The one thing it gets right — the interpretation is a *term* — is §9.
- **v4 as written** (the row leaves the type, the carrier is lowered post-mono but *stays* the mechanism).
  Its measurements are inherited (§9.7's instantiation key; the seam is late enough), its blockers B2–B4
  dissolve under records rather than being answered, and its B1 options are superseded by "the interpretation
  is a term".
- **A `World` token / threading a fake dependency to sequence and protect I/O.** Not needed in a strict
  core (§9.7); purity is decided by evaluator stuckness.
- **Deleting `EffectLifter` and `CarrierKindChecker` *under v5*.** Measured per arm: five of six live, two for
  soundness. Under v6 `EffectLifter` has no subject and goes at the flag day; `CarrierKindChecker` stays as
  the kind system it is — `verifyCarrierKinds` is still the only thing rejecting a `[F[_]]` binder
  instantiated at a proper type.
- **Replacing concrete pins with ordinary generics *under v5*.** Refuted twice by running the tree, because
  the elaborator *writes* carriers rather than solving binders. Under v6 the stated reason is gone, and the
  replacement is exactly the desugar (§9.4: a stored row is a function of its records) — not a
  re-proposal, a different premise.
- **Deleting `Id` or its erasure *under v5*.** Never a decision then; under v6 `Id` has no subject.
- **Bounded staging / deferring an instantiation-decided position.** A deferred position is one the
  elaborator writes nothing at, so it cannot write the carrier there either; kept v2 alive for six days.
- **Approximating rule 4 in the elaborator** instead of declaring it in the signature.
- **A relayed slot-mode rule.** Named nothing and handled depth 1 only.
- **Putting the row on `VPi`** (Koka-style). Teaches every unification site, the printer and the `Function`
  native about rows. v6 puts the row on a *parameter list*, which the Π-former already has.
- **A `type X = {A, B}` row alias with its own AST node.** An `ast.fact.Expression` case is the most
  expensive thing this language can add; §2.4 replaced it with one resolve rule.
- **Discharge markers (`{-E}`).** There is no negative-effect surface; discharge is a `with`.
- **Scanning the dictionary for an ability name.** Replaced by the keyed marker lookup.
- **Running v4's P2 before P4.** Not separable — and moot: the flag day is §10.2.
- **The `<Ability>Carrier` convention as an explicit declaration** (the former D2). Its premise — that an
  effect *has* a representation — is gone; the property it named ("has a canonical monad transformer") is
  exactly what §9.6's two primitives replace.

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
| `effects-as-channel-v4.md` | the row leaves the type, the carrier leaves the language | `R1`–`R11`, `P0`–`P5`, `Q1`–`Q4`, `§0`–`§11` | superseded by **§9** (v6); its measurements in §9.7, its blockers in §12 |
| `effects-v4-p0-spike.md` | does the `WovenValue` seam know the carrier? | `S1`–`S3` | §9.7's instantiation key; the test is permanent |
| `effects-v4-p2-sizing.md` | sizing the flag day | `§1`–`§5` | §10.2 |
| `effects-v4-flag-day-readiness.md` | is the flag day ready? (no) | `B1`–`B3` | §12 (v4 as written) |
| `effects-syntax-userspace.md` | `~` and `&` as ordinary values | `stage 1`–`stage 4`, `§7.x` | §2.5 (stages 1–2, landed), **D3** (stages 3–4) |

**Citations to Part II's former numbering** (in commits and comments dated before 2026-09-06): *D1* was the v4
decision and *B1*–*B4* its blockers — now §9 and §12; *D2* was the `<Ability>Carrier` declaration — §12's last
entry; *W1*–*W4* were the v5 work items — §10.3 A3; the "2026-09-05 B1 assessment" is condensed into §9.2.

Two older citations in the tree — `docs/effect-lift-in-checker.md` and `docs/effectful-signatures.md` — point
at documents retired before these and are likewise historical.
