# Effects in Eliot — the design, and what is left to do

**Status (2026-09-03): the effect system is shipped and this is its single document.** It replaces ten
separate notes — the v2, v3, v4 and v5 designs, the three v4 measurement notes, the row-tails note, the
`~`/`&` user-space note and the testing note — whose landed steps, migration roadmaps, deletion slices and
per-step measurements are removed. What survives here is (I) the design as it actually behaves, (II) the
work that is still open, and (III) enough provenance to read a source comment that cites a retired
document.

**One-sentence summary.** The user writes **effect rows**; the compiler works in **carriers**; suspension is
*declared* in signatures instead of inferred from genericity, and the carrier is *written* by a desugar
before checking instead of solved by the checker — so effect elaboration is a syntax-directed phase, effects
verify as a **channel** beside the type (the same architectural move as the `Int` refinement channel), and
the NbE checker holds one local rule and no effect decisions.

**How to read this document.** Part I states the design; it is the authority, and the CLAUDE.md *Effects Are
a Channel* cornerstone is its summary. Part II is the plan: numbered decisions (**D**), work items (**W**)
and a list of things that are **closed by measurement and must not be re-proposed**. Part III is provenance.
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
would put "Console rides Suspend" in the ability instead of repeating it on every instance — see **D2**.

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
§11): `EffectLifter`'s carrier recognition (`effectCarrierSplit`, `mustPureWrapBeforeUnify`) and
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
   prohibited. The same prohibition covers rows if a row ever enters a type (**D1**): rows are declared and
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
6. **Rule-4 violations are diagnosed twice, unequally.** → **W2**.
7. **A fake run needs a region with no ambient carrier of its own.** Inside a pinned region the ambient
   carrier *is* the pinned stack, and the elaborator writes every carrier-generic callee at the region's
   carrier — so a fake run written *inside* a pinned body is written at the pinned stack, not at the fake
   carrier. A carrier-generic value can only be instantiated at a foreign carrier in a region with no
   ambient carrier of its own. → **W3** is the convenience that would remove it.

   This was previously stated as "a test is run-then-assert, never interleaved", which is **wrong** and is
   corrected here: interleaving assertions with faked effects works today, and `eliot-test`'s
   `test/eliot/test/example/` is the worked example. What made it work was not stacking — a pinned
   `{Throw[AssertionError] | Session}` does fail, for the two reasons the retired L3 note recorded — but
   giving the *fake carrier itself* a `Throw[AssertionError]` instance, so assertions ride the same carrier
   as the faked effects and nothing has to lift (§6). What genuinely remains is the region rule above: the
   run must sit in its own definition, so a direct-style faked case costs a body `def` and a discharge
   `def`. Since the block peel (§3.3) the *body* of either may be a multi-statement `{ … }` block; before
   it, only a single call was deferred and a block was charged to the harness.
8. **Rows are sets of abilities**, so a definition mixing a faked run with a real leak of *the same* ability
   defers that entry and the user gets the post-mono `Type mismatch` at the harness body instead of the
   located effect-vocabulary message. The program is still rejected; only the diagnostic degrades, and only
   in that one mixed shape.
9. **Rule 3 has no check of its own for a `data` field typed by the data's *own* open carrier binder**
   (`data Box[F[_]](action: F[Unit])`). The open-*row* field is rejected; this shape is not. → **W4**.

---

# Part II — The plan

## 8. How this plan is run

**Decision protocol.** Standing rules 1 and 2 (§5) govern. Every entry below marked **decision** is
Robert's; nothing in it is a judgement call to be made in flight, and a step that finds itself narrowing one
of Part I's rules stops instead of landing.

**The gate**, for every step: `./mill __.test` green, all 45 example programs carrying a `main` compile, and
every example jar `md5sum`-identical to the pre-change build except where the step's own note says an output
legitimately changes. Byte-identity is a **safety oracle, not a hard gate**.

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

The harness recipes themselves (fast example sweep, byte-identity compare, cache clearing, mill/test-session
traps) are in the `reference_verification_harness_recipes` memory.

## 9. Open decisions

### D1 — v4: does the row leave the carrier behind? (the one large decision)

**The proposal.** Put the row in the type only where a computation is genuinely a *value* (arrows, stored
computations), keep everything else in the channel that already verifies it, and move the carrier out of the
language entirely into one post-monomorphization lowering where every type is ground.

- **Tier 1 — the channel (most code).** `def readLine: {Console} String` gives the checker `readLine :
  String` plus declared-row metadata. The checker never sees an effect.
- **Tier 2 — the type (only where a computation is a value).** One primitive former `Computation[r, A]`
  beside `VPi`; an arrow with a latent row is `VPi(A, _ => Computation(r, B))`, so **`VPi` gains no field**.
  A row is an ordinary canonical value; a row variable is an ordinary generic parameter of type `Row`.
- **Tier 3 — the carrier (compiler-internal, post-mono).** `IO`, `flatMap`, `pure`, the transformer stacks:
  introduced by one lowering pass at the `WovenValue` seam, where every type argument is ground.

**What it buys.** Rule 4 stops being a rule the elaborator enforces and becomes ordinary definitional
equality — the rule whose erosion caused every stall in this design's history becomes a theorem. `Id`,
`runId`, `~ Effect`, `~ Suspend`, the `<Ability>Carrier` convention, the open/pinned split, the `{}` binder
reuse, the inferable carrier binder and the type-alias limitation all become unreachable states rather than
handled ones. A backend also becomes free *not* to build a carrier (`Throw` as a branch, `State` as an
extra in/out parameter) — which matters on an ATtiny and is foreclosed today.

**What it costs.** One type former and a canonicalisation obligation; effect lowering becomes a compiler
pass instead of a desugar over library code; the lowered core is machine-generated and must be re-checked;
and it is a **flag day** — the checker, the elaborator and effect-method ability selection move in one
change, with no partial state.

**What already landed, and is dormant in the tree:**

| piece | where | state |
| --- | --- | --- |
| seam-groundness measurement | `jvm/test/…/EffectsV4SeamGroundnessTest.scala` (211) | permanent; pins that every instance's carrier is ground at the seam |
| `Row`/`Computation` formers, canonicaliser, `unify` cases, printers | `GroundValue`, `SemValue`, `fact/CanonicalRow` (67), `row/CanonicalStack` (109), `Unifier`, `SemValuePrinter` | **nothing produces either former**; behaviour-neutral |
| the woven re-check | `monomorphize/channel/WovenRecheck` (114) | **live and mandatory** at the seam beside `assertNoIdResidue`; a no-op on today's output and below the measurement noise floor |

**What the measurements settled**, and what any future attempt inherits:

- The seam **is** late enough — no instance carries an unsolved `Param` in its carriers.
- But the payload key does **not** determine the carrier: one `{Console}` helper called both inside and
  outside a `catch`-discharged region is two instances with two different bodies and one payload key. The
  weave must be keyed `(vfqn, payload arguments, carrier stack)` and the lowering must be a demand from the
  run boundary downwards. It terminates for the same reason today's monomorphization does.
- A stored computation's stack must be **computed from its row** (canonical ability order lowered to
  carriers, over the pure base when the row rides no `Suspend`, over the platform run carrier when it does),
  because a v4 computation type has no base to read one off.
- Effect-method ability selection is an **addition at the seam, not a move**: `AbilityResolver` serves every
  ability, so the checker keeps it and the seam gains a second, ground-carrier entry point.
- **P2 is not separable from P4.** The lowering's input must be a *direct-style* monomorphized body, and
  producing one *is* the checker change. There is no scaffold in between, so the flag day is one change.
- The machinery **abilities are the representation, not sugar over it.** Of the 40 `~ Effect`/`~ Suspend`
  constraints in `.els` today, the 23 on `implement` heads stay (they are what lets the instances' own
  bodies typecheck, and the lowering emits calls to them); only the 17 on `def` heads go. `eliot.carrier`
  can only leave *user scope*, which being import-required it already has. Same family: a native with a
  carrier-typed parameter (`foldLeftInternal`) means the lowering must lower leaf **signatures**, not only
  bodies.

**What blocks it, and none of these is cheap to discover later:**

- **B1 (the real one) — v4 deletes the only mechanism a program has to substitute an effect
  implementation, and nothing replaces it.** §6's strategy has three legs and v4 removes all three: a user
  cannot *name* a carrier (both routes — the pinned tail and a carrier-typed slot — go), cannot *choose* one
  (the stack is a function of the row, and every `Suspend`-riding row lands on the platform run carrier),
  and so never has their instance queried. This is wider than testing: an application-owned ability whose
  only implementation is on the program's own pure carrier has no v4 spelling either. "Discharge it instead"
  is not available — the `Suspend`-riding abilities have no carrier for a discharger to be written over, and
  user-visible handlers are what v4 explicitly does not build. Three example programs and two integration
  test classes depend on the mechanism, so the flag day cannot meet its own gate while this is open.
  Three options:
  - **(a) grow user-visible handlers** — most principled, and the largest scope increase in the project;
  - **(b) keep the choice of interpretation as a *term*, not a type** (recommended): one run-site form
    `runAt[Recorded](program)` — precisely `row/RunBoundaryFunctions` generalised from a
    platform-registered FQN with a fixed base to one form whose base is a type argument. Nothing is a
    carrier in a stored or passed type; the seam weaves the argument at *that* base and selects
    `Console[Recorded]` there. Every deletion then stands, tail included.

    **Two corrections to this option, both load-bearing** (2026-09-04):

    1. `runAt[B]` must mean **"the residual row resolves at `B`"**, *not* "the canonical stack of the row is
       built over `B`". The stack-from-row rule is right for a **stored** computation, which has no base to
       read one off — it is wrong here. Applied to `runAt[Recorded]` with a row of
       `{Console, Transcript, Throw[AssertionError]}` it builds `ThrowCarrier[AssertionError, Recorded]`,
       leaving `Console[Recorded]` and `Transcript[Recorded]` one layer down with nothing to lift them
       (§6) — so the option would fail to preserve the exact capability B1 exists to preserve. The correct
       rule is what the *real* path already does: whatever carrier is landed on, the row's abilities resolve
       **there**, and **dischargers build their own layers internally**. Today `Console` at
       `ThrowCarrier[E, IO]` resolves at the stack via the `Suspend` lift, and that stack came from a
       discharger, not from the row. A fake with no discharger in play therefore lands on `Recorded` itself,
       where its instances are. (A fake *with* an inner discharger still stacks and still hits §6 — the same
       limitation as today, not a new one.)
    2. The form **cannot be written in the language as it stands**, and not for a syntactic reason: the
       elaborator would have to take the capture's carrier from the **call's own explicit type argument**,
       which is not a source `§3.2`'s whitelist admits. Measured, so it is not re-tried:
       `def runAt[B[_], A](body: B[A]): B[A] = body` called as `runAt[Recorded](script)` *inside a region*
       is elaborated at the region's carrier and the script is charged to the enclosing definition — the
       `[Recorded]` is simply not read. Admitting explicit type arguments to the whitelist is a decision, not
       an implementation detail; it is defensible (an explicitly written type argument is not inference, and
       is arguably the most declared thing at a call) but it is an amendment and needs sign-off. **W3's
       parameter tag needs no such amendment** — see W3;
  - **(c) drop substituted interpretation** — delete the examples, the test classes and most of §6, and
    rewrite the flag-day gate. Note a test project is *additive*, so "test with a swapped layer" is a
    different thing (whole-layer replacement), not a version of the strategy.
- **B2 — R7, the stored-computation hoist.** A stored computation's representation is fixed at construction
  by its canonical stack, so a consumer whose ambient differs needs a base hoist, which v3 never meets
  because it forbids the programs that would. **Proposed rule** (fail-safe, costs nothing, cannot
  miscompile): *a stored computation is discharged at its canonical base, and a mismatch is a hard error
  naming both, never a silent lift.* No hoist exists in the tree and no program needs one; write one when a
  program asks, which is also when its semantics can be judged. **Adopt or reject before the lowering is
  written — it is one of the lowering's error paths.**
- **B3 — R11, weakening.** `{ρ₁} A` must be accepted at a `{ρ₂} A` slot when `ρ₁ ⊆ ρ₂` (today the slot is
  `{ρ₁ | G} A` elaborated at `G` per use, so no rows are ever compared). It must live **at declared slots in
  the elaboration**, exactly like the zero-row lift — never as an assignability arm in `unify`, which the
  Types-Are-Values guardrail forbids outright. Representationally the widening is a re-weave of the callee
  at the slot's stack: free for a call or a value reference, **impossible for a value already built at a
  narrower stack**, which is where an ordinary user meets B2.
- **B4 — R8/Q4, canonicalisation decides semantics.** Canonical order fixes a stored `{Throw[E], State[S]}`
  computation's transformer nesting — the difference between state surviving a raise and not — where today
  the author writes the pin's order. The canonical form is already fixed in the tree, so **adding an entry
  kind afterwards is the two-spellings trap**; the flow-grade generalisation (§9 D6) must therefore answer
  "grade in the type of a first-class computation, or only in the channel?" *before* anything produces a
  row.

**Recommendation.** Do not start the flag day. Decide B1 first — it is a language decision (*on what terms
may a program interpret an effect of its own?*), not a lowering decision, and option (b) is the one that
keeps the shipped capability while allowing every deletion. If v4 is dropped rather than deferred, revert
the dormant `Row`/`Computation` formers and their canonicaliser (~180 lines, plus their pass-through arms in
the evaluator, the quoter, `unify` and both printers — unreachable type language is exactly the sort of thing
that misleads a later reader) and **keep** `WovenRecheck` and the seam test, which earn their place
independently.

**The narrower alternative is already taken.** Row aliases — aliasing the row rather than the row-plus-payload
and splicing it into the constraint list at resolve — was the "right change if v3 stands"; §2.4's requiring
ability delivers what it wanted (the expanded row reaching the channel) and shipped.

### D2 — what does an ability declare about its carrier?

The `<Ability>Carrier` convention should become an explicit declaration on the ability, so "which
representation supplies this effect" is written once, in one place, and no phase recognises a carrier by
name. Half of this is already reachable and unused: `ability Console[F[_] ~ Suspend]` (§2.4) states "Console
rides Suspend" on the ability instead of repeating it on every instance. The other half — naming
`ThrowCarrier` as the representation of `Throw` — is what the pinned-row desugar and `EffectCarrierNaming`
read by convention today. **Decision needed**: the spelling, and whether it subsumes the resolve-time "no
such carrier" error that §3.1's dischargeability filter currently relies on.

### D3 — `~` and `&` fully in user space (stages 3 and 4)

Stages 1 and 2 landed (§2.5). Stage 3 would collapse the constraint types to a single `Sourced[Expression]`
and make `&` an applied type constructor; stage 4 would make `~` a compiler native
(`infix def ~[K](kind: K, c: Constraints): K`) and delete `paramConstraints` from every fact. **Assessed and
blocked, on meaning rather than on difficulty:**

- **What does an ability denote, as a value?** `&` is `Type → Type → Type` and an ability's marker is a
  partially applied *function value*, so `&[Console[F], Log[F]]` is ill-kinded under the only declarations
  either side has — caught by nothing today only because no reader ever type-checks a constraint. Three
  honest answers: **(a)** give constraints their own sort — forbidden by the *Effects Are a Channel*
  cornerstone ("no kind or sort is added to the type language") and the first stratification in a
  deliberately non-stratified PTS; **(b)** make an ability a type inhabited by its implementations — coherent
  with types-are-values, and head-on against "ability references are fully resolved during monomorphization,
  never passed around in structures"; **(c)** leave it an uninterpreted stuck neutral — implementable
  tomorrow, and the side channel again, now spelled as an application.
- **Do `~` and `where` unify?** After stage 4 they are two spellings of "a compile-time predicate on a
  signature", one evaluable and one search-driven. Settle this before any representation is committed.
- **A phase-order blocker must be cleared first, as its own change.** The superability closure (§2.4) runs
  at resolve (6) and is *structural*; infix operators are not structured until 8, and a constraint that is
  still an un-precedence-resolved `&` run cannot be decomposed. The only clean route is moving the closure
  to ≥ 8 — which relocates the single source of truth both verifiers read, so it lands **before** stage 3,
  not inside it.
- **The footprint win is illusory.** 46 structural reads across 16 files consume `.abilityFQN`/`.typeArgs`
  directly and would each decode the expression back into them; and the overwhelming majority of constraints
  in a build are not user-written `~ A & B` at all (about fifteen `&` uses in the whole tree) but minted by
  `EffectSugarDesugarer` from rows — so stage 3 makes the compiler's own metadata travel as surface syntax
  it must re-parse at every read.
- **`signatureEquality` changes meaning**: constraints are excluded from it today; folding them into the
  restriction makes a layer adding a body spell identical constraints. Sanctioned duplication, arguably an
  improvement — but a deliberate decision, not a side effect.

**Recommendation.** Do not implement stage 3 as written. Answer the two meaning questions first; land the
closure relocation on its own if and when they are answered yes.

### D4 — `Suspend`-riding effects: pinning and supplying

`Console`/`Log` have no canonical carrier, so they can be neither pinned nor supplied (§7.2). The designed
extension is two parts: **(a)** an abstract base-layer `type Suspended[A]` aliased per target
(jvm: `= IO[A]`) — the nameable platform base, the same commitment the synthetic entry already makes
grounding `main`; and **(b)** treating carrier-less effects left of `|` as *constraints on the tail* rather
than as layers. Neither is needed until a real use case stores platform actions in a row-stating field —
payload-indifferent generic containers already store effectful functions fine. **Decision**: whether to
build it, or to keep the loud resolve-time error as the answer (see **W1** for the diagnostic either way).

Note this decision interacts with D1: under v4 the limitation disappears, and under B1 option (b) the run
site names the base instead.

### D5 — a lambda body at a rowless arrow slot

```eliot
def call(f: Option[String] => String): {Console} Unit = printLine(readLine.f)
def main: IO[Unit] = call(s -> s.orAbort else "")        -- Expected: String, Actual: IO(String)
```

`f`'s codomain is declared rowless, so by rule 4 the lambda's body is a value position and its `else` should
discharge to `Id` — which is exactly what happens when the same discharge sits in a named pure helper. The
fix is one line in shape (`elaborateLambdaNatural` takes the slot's declared codomain rather than inheriting
the enclosing region), but it changes what "a lambda at a plain arrow slot" means for *every* call: the
existing arm deliberately lets an effectful body become a bind chain on the enclosing carrier. Per standing
rule 2 this is a **rule decision, not a patch**. Pinned by `ExamplesIntegrationTest1`'s "bind an effectful
subject dotted into a function-typed parameter", whose handler is spelled around the gap.

### D6 — flow grades (cross-reference)

The planned generalisation — the row becomes *"abilities + named grades"* (`{Timer, cycles: <=800}`), with
today's row as the powerset special case (`seq` = `branch` = union, `within` = `subset`) — is specified in
`TODO.md` and gated on `List`/`Array` `size` meta. Two connections to this document: it lands **better**
under D1 (a row is already an ordinary value with an algebra, so a grade is a new entry *kind*, not a new
mechanism, and "grade-only rows do not force a carrier" stops being a carve-out); and if D1 is taken, B4
must answer it before the canonical form is relied on.

### D7 — can the post-mono accounting verifier retire?

The pre-mono per-definition check is unbounded except for the three cases in §3.3, so in principle the
post-mono one is redundant for everything it decides. It stays until experience says otherwise: it is the
codegen precondition, the unconditional fail-safe, and the only verifier that sees ground instantiations.
**Default answer: no.** Revisit only with evidence.

## 10. Open work (no decision needed)

- **W1 — the "cannot supply / cannot pin a `Suspend`-riding effect" diagnostic must name the route to
  take.** Today it is a loud resolve-time failure about a missing `<Ability>Carrier`; it should say which of
  the two available answers applies (substitute the carrier, or declare the effect and let it ride).
- **W2 — make both rule-4 diagnostics name the slot.** A user pipe declaring no row
  (`|>[A, B](a: A, f: A => B): B`) given a computation gets the elaborator's own *"This argument is a
  computation, but argument N of '|>' declares no effect row"*. The stdlib `.` — whose `f` declares `{}` —
  instead hoists the subject and leaves the checker to report an unattributable
  `Type mismatch. Expected: IO(IO(Option(String)))`. Same violation, one message.
- **W3 — a user-declarable capture tag** (`def check(name: String, program: {| Session} Unit): TestResult`).
  **Decided 2026-09-04: build this (Form A), not the `runAt` term (Form B).** Form A needs no §3.2
  amendment, Form B does (B1(b) correction 2); both generalise the same mechanism, so A is a down payment on
  B and is not throwaway if D1 later lands. Scope of the build: allow the entry-less row tail in the parser,
  thread the existing pinned tag with **zero** entries, and **derive no discharge stack inside a capture
  whose carrier the declaration names concretely** — without that last part an assertion inside the block
  still stacks a `ThrowCarrier` over the fake and lands back at §6's wall, which is the whole point of the
  feature. Accepted cost: two spellings of one type (`{| Recorded} A` and `Recorded[A]`), separated by the
  rule that already separates `{} A` from `G[A]` — *row spelling when the slot runs the thing, bare type
  when it is data passed through*.
  This is the tag the platform already contributes for `runMain`'s `io: IO[A]`, made declarable instead of
  plugin-only; the cheapest surface reuses pinned-row syntax with no ability entries, so the existing tag
  pipeline applies unchanged. It would buy a runner taking a program on a bespoke carrier directly, removing
  §7.7's region constraint — the remaining cost of a faked case, now that interleaving itself works.
  **A later convenience, not a prerequisite** — a framework ships today without it. If D1/B1 lands option
  (b), `runAt[…]` supersedes it.

  One thing measured while correcting §7.7, so it is not re-derived: declaring the slot as a **pinned row
  over the fake** (`body: {Throw[AssertionError] | Recorded} Unit`) *does* widen the capture's ambient row —
  the higher-kinded mismatch disappears and the block elaborates — but it lands the body on
  `ThrowCarrier[…, Recorded]` and so walks straight into the fake-lifting wall (§6). Any W3 surface must put
  the body on the fake carrier itself, not on a stack over it. The entry-less spelling `{| Recorded} A` is
  exactly that, and it is why the tag carries **no** entries: what the body performs, ability resolution
  finds out on its own and reports precisely; the one thing no declaration states today is *"this slot hosts
  a computation on this carrier"*, which is one bit, and the tag is it. (`{| B} A` does not parse today —
  `ast.fact.Expression`'s row-tail parser reads `| base` only after at least one entry, on the reasoning that
  such a row "is that carrier itself and needs no row spelling". True of the *type*; the point of the tag is
  that it is not true of the *tag*. This is the same tag-not-shape distinction that already separates `{} A`
  from `G[A]` under §1 rule 2, so it adds no concept.)

  **`{| B} A` is not a third meaning of the row** — it is the pinned row at **n = 0**. The rule is uniform:
  `{E₁, …, Eₙ | B} A` is the canonical stack of the entries' carriers, leftmost-outermost, bottoming out at
  `B`; with no entries there are no layers and the type is `B[A]`. `{Console | Recorded}` fails under that
  *same* rule rather than a different one — there is no `ConsoleCarrier` to layer. What the tag adds is a
  **fifth naming** on §1 rule 4's list of four (a pinned stack, a callee's own carrier binder, `Id`, a
  platform run carrier); rule 4 already holds that the namings are "one predicate, not four arms", so this
  extends an enumeration rather than adding a kind of slot.

  **Why the tag cannot be replaced by reading `Effect` instances** — the first thing anyone reading this
  will propose, since asking for an `Effect` instance is the sanctioned "is this a carrier?" test elsewhere.
  It would decide a *caller's* calling convention from a declaration the carrier's author wrote for their
  own body's sake, which is the bug already fixed in the other direction (the elaborator's own note: adding
  `~ Effect` to `def hold[G[_]](x: G[String])` silently changed what callers could pass). The in-tree
  counterexample is live: `implement Effect[Either[String]]` exists in `stdlib/eliot-compiler`, so every
  `Either[String, A]` parameter in compile-track code would silently become a capture slot. Carrier-ness at
  a *slot* must be written by whoever writes the signature.

  **W3 and D1's B1(b) are the same mechanism at two granularities**, which is the argument for doing W3
  first regardless of how D1 goes. Both generalise `row/RunBoundaryFunctions` from "a platform-registered
  FQN with a fixed base" to "a base the user supplies": W3 declares the boundary **on a parameter**, so the
  base is in the callee's declared parameter type — already on §3.2's whitelist, no amendment needed;
  `runAt[B]` is a **built-in** boundary whose base is a call-site type argument, which is not (B1(b)
  correction 2). So W3 is a **down payment on B1**, in the one surface today's elaborator can already
  consume, and it is not throwaway work under any outcome of D1.
- **W4 — optional hardening: reject a `data` field typed by the data's own open carrier binder** (§7.9).
  The open-*row* field is already rejected; this shape reaches the same place by another spelling.

## 11. Closed by measurement — do not re-propose

Each of these was tried, measured, or decided, and the record is the reason not to spend the time again.

- **Carrier inference** — a carrier metavariable, a join solver, an `Id`-headed uniform judgment, a mode
  obligation, a post-drain mode resolver. Of 15 fix commits in the v2 window, the four highest-impact were
  one failure: a carrier metavariable captured by first-contact unification. The guard family was documented
  as uncompletable. Gone, and prohibited (standing rule 3).
- **Deleting `EffectLifter` and `CarrierKindChecker`.** Measured per arm, each behind its own switch, with
  the whole gate and all 45 examples run per switch: **five of six arms are live, two for soundness**. The
  one dead arm (`mustLiftBeforeUnify`) is deleted. `verifyCarrierKinds` is the only thing rejecting
  `def bad[F[_]](x: F): F` instantiated at `?F := Box[String]`; with it off that program silently compiles.
  Residual risk, stated rather than buried: `mustLiftBeforeUnify`'s removal is safe on the runtime track by
  construction (the elaborator writes the carrier, so a runtime carrier is never a meta) and rests on the
  gate on the compile track, where carrier metas still exist. If a compile-track program is ever found in
  which a carrier meta application meets an equal-arity data constructor at a flex payload, that is the
  commit to revisit — and **the fix is the elaborator writing that carrier too, not the guard coming back**.
- **Replacing concrete pins with ordinary generics** (`data TestCase(body: {Throw[E] | Id} Unit)` ⤳
  `data TestCase[F[_] ~ Throw[E]](body: F[Unit])`). Refuted twice by running the tree: **there is no site**
  (the tree contains zero pinned rows in `.els`; the one case was removed by `foldNamedValues`, which hands
  each gathered value to a *slot* instead of storing it), and **the replacement is not free** — leaving `F`
  to be inferred at each use fails, because the elaborator *writes* carriers rather than solving for them,
  so a carrier-generic stored value is written at the caller's ambient and the stored effect is charged to
  the caller. The working spelling names the machinery (`TestCase[ThrowCarrier[String, Id]]`), which is
  exactly the leak the pinned row exists to remove. A regression in the user surface, not a subtraction.
- **Deleting the pinned tail, the pinned/open desugar machinery, or the `{}` binder-reuse rule.** All three
  are load-bearing: the tail is the only spelling of a stored computation that does not name a carrier
  stack; the pinned machinery is what a supplied parameter row *lowers into*; and the binder-reuse rule is
  how a discharger's ambient carrier is found.
- **Deleting `Id` or its erasure.** Never a decision — an inference from v2's critique of the `Id`-headed
  *encoding*, which is a different thing and was removed separately.
- **Bounded staging / deferring an instantiation-decided position.** A deferred position is one the
  elaborator writes nothing at, so it cannot write the carrier there either: the "small local concession"
  is what kept the carrier inferred and the whole v2 machinery alive for six days.
- **Approximating rule 4 in the elaborator** (letting the payload test accept a generic head) instead of
  declaring it in the signature.
- **A relayed slot-mode rule.** It named nothing and handled depth 1 only; it existed because `a.f(b)` is
  `.(a, f(b))`. Standing rule 7.
- **Putting the row on `VPi`** (Koka-style `VPi(domain, codomain, row)`). Feasible and adds no sort, but it
  teaches every unification site, the printer and the `Function` native about rows — putting effects *into*
  the mechanism the design exists to keep them out of. D1's arrow-with-computation-codomain gets the same
  expressiveness with no change to the Π-former.
- **A `type X = {A, B}` row alias with its own AST node.** Rejected: `type` names a type, a set of abilities
  is not one, and an `ast.fact.Expression` case is the most expensive thing this language can add — one node
  forced arms in the codec, the core converter, the strict-positivity checker, two places in the desugarer, a
  new `EffectRow` field and a special case in `TypeAliasDefinition`. §2.4 replaced it with one resolve rule
  and no syntax.
- **Discharge markers (`{-E}`).** There is no negative-effect surface; discharge is structural.
- **Scanning the dictionary for an ability name.** Replaced by the keyed marker lookup; the scan was
  hash-order-dependent and ignored import scope.
- **Running v4's P2 before P4.** Not separable — see D1.

---

# Part III — Provenance

## 12. Retired documents, and how to read a citation

Ten documents were merged into this one and deleted. Their full text is in git history (`git log --diff-filter=D
-- docs/`), and the table below says what each was and where its subject now lives. **Scaladoc comments across
the compiler cite them by their own section anchors** — those are *historical pointers* ("this code came out of
X §N"), not live references, and they do not index this document.

| retired document | what it was | anchor scheme in comments | where its live content is |
| --- | --- | --- | --- |
| `effects-as-channel.md` (v2, "uniform carriers") | the first shipped design: the carrier as a type argument the *checker solves* | `§0`–`§13`, `finding N`, `U1`/`U4-x` | superseded in direction and in code. What it got right and kept: the channel (§3.3), rows never flowing into types, carrier-ness by tag (§3.6), `Id` without `Suspend[Id]` (§3.4), payload/row vocabulary (§3.7). What it got wrong is §11's first entry |
| `effects-as-rows.md` (v3) | the landed design + its A.1–A.11 record: the elaborator writes the carrier | `§1`–`§9`, `A.x`, `R1`–`R6` | Part I in its entirety; §4 is its Appendix A.1; §5 its standing rules; §8 its A.9.4 method |
| `effect-row-tails.md` | pinned rows as the one spelling of a carrier stack | prose only | §2.3, §7.2, D4 |
| `testing-effects.md` | substituting effect implementations | `L1`–`L3`, `§2.x` | §6, §7.7, W3 |
| `effects-v5-one-carrier.md` | rows as constraints on one carrier — the subtraction from v3 | `§4 step N`, `§5 Q1`–`Q4`, `§7` | §2.1 (step 1), §2.2 (step 2), §2.4 (§7), §3.8 + §11 (step 4 and Q1), D2 (Q2). Step 3 is §11's third entry |
| `effects-as-channel-v4.md` | the row leaves the type, the carrier leaves the language | `R1`–`R11`, `P0`–`P5`, `Q1`–`Q4`, `§0`–`§11` | **D1** |
| `effects-v4-p0-spike.md` | does the `WovenValue` seam know the carrier? | `S1`–`S3` | D1's "what the measurements settled"; the test is permanent |
| `effects-v4-p2-sizing.md` | sizing the flag day | `§1`–`§5` | D1, same |
| `effects-v4-flag-day-readiness.md` | is the flag day ready? (no) | `B1`–`B3` | D1's blockers |
| `effects-syntax-userspace.md` | `~` and `&` as ordinary values | `stage 1`–`stage 4`, `§7.x` | §2.5 (stages 1–2, landed), **D3** (stages 3–4) |

Two older citations in the tree — `docs/effect-lift-in-checker.md` and `docs/effectful-signatures.md` — point
at documents retired before these and are likewise historical.
