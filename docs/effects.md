# Effects in Eliot — the design, and what is left to do

**Status (2026-09-11): effects v6 is shipped, and this is its single document.** An effect is an ability
declared with the `effect` keyword; an **implementation is a name**, bound by `with` and forwarded lexically
from `main` inward through declarations. There is no carrier, no monad, no `Id`, and nothing to infer. Part I
describes the tree as it is; Part II is what is left — where the tree still diverges from Part I, the one
change decided and not yet built (a binding binder marked by its type, §9), and the decisions still open;
Part III is the list of things closed by measurement or decision, and enough provenance to read a source
comment that cites a retired document or a retired section.

**One-sentence summary.** The user writes **effect rows**; each row entry desugars to one **phantom generic
binder** whose value is an implementation, written at every reference by a syntax-directed pass that reads
declarations only — so an operation call is an ordinary call to a known method, effects verify as a
**channel** beside the type (the same architectural move as the `Int` refinement channel), and the NbE
checker holds no effect rule at all.

**How to read this document.** Part I states the shipped design; it is the authority for the tree, and the
CLAUDE.md *Effects Are a Channel* cornerstone is its summary. Part II lists the divergences (§8), the planned
change (§9), the method every change is run under (§10) and the open decisions (§11, numbered **D**). Part III
holds the list of things **closed by measurement or decision and must not be re-proposed** (§12) and the
provenance (§13). Where Part I and any code, stdlib signature, example or test disagree, Part I wins and the
artefact is the defect. The record of how v6 was decided and landed (the former §8–§10: the reasoning, the
flag-day log F1–F9 and the follow-ups A2–A11) is in git history, and §13 says how to read a citation to it.

---

# Part I — The design

## 1. The user model — four rules

1. **Effects run where they are written.** An effectful expression in any plain position performs its
   effects there, and they join the enclosing definition's row. Strict call-by-value in **every** plain
   position, a bare generic slot included: `choose(readLine, readLine)` runs both reads, `Box(shout)` runs
   `shout` and stores its value. The only exception is the *declared* one below.

2. **Suspension is declared.** A parameter that must *not* run its argument declares a row: `whenTrue: {} A`
   receives the computation unrun; `if[T](c: Bool, value: {Abort} T)` spells this and means it. After
   desugaring such a slot is a **thunk** (`{Abort} T` ⤳ `Unit => T`), since there is no carrier left to hold
   an unrun computation — but the thunk is an artefact of the lowering, not the surface: what a reader and
   every phase go by is the **row tag** on the declaration (`EffectRow.parameterEffects`), never the shape.

3. **A stored computation is bound where it is written.** A row-typed `data` field (`data TestCase(body:
   {Throw[E]} Unit)`) is a thunk whose operation calls were bound at construction, by the declarations in
   force *there*. Storing it, passing it through a plain generic and running it later are all ordinary;
   running it performs what the field's row declares, charged at the read. A `with` applied to it afterwards
   is an error, not a rebinding — there is nothing left to bind. This replaces v5's pinned rows: no base, no
   `| Id`, no canonical stack, and no ordering to spell.

4. **A binding passes into a position if and only if that position declares a row.** *(Found last, stated
   last, and it outranks the other three.)*
   - A **plain generic is a payload, always.** `A`, `B`, `T` in `def .[A, B](a: A, f: A => {} B): B`,
     `def ++[T ~ Combine[T]](left: T, right: T): T`, `def foldLeft[A, B](initial: B, …): B` carry values.
     A function that transports effects says so with a row on the slot.
   - A **rowless slot receives the value**, computed where the argument stands (rule 1). It is not a place a
     computation can be *handed on* unrun, and there is nothing to diagnose: the effects were the caller's.
   - **A row on the slot is what lets the callee's body reach the caller's declarations.** The lexical walk
     that decides an operation's implementation crosses into an argument iff the slot it sits at declares a
     row — `{}` counts. So a lambda at a rowless arrow (`map`'s `f: A => B`) may bind and discharge locally
     but may not reach the enclosing def's bindings, and anything it performs and does not discharge is the
     error at the lambda.
   - The four "carrier namings" of v5 collapse to **one predicate**: a slot either declares a row or it is a
     payload. There is no third kind of slot and no name-keyed exemption.

   Everything the write decides is then decided by declarations, per reference, order-free.

**Consequences the user sees.** `something.foldLeft(f, z)` with `something : {Console} List[T]` works with
zero declaration on `foldLeft`: the effects run, the payload flows, `Console` joins the caller's row, and
the collections library stays effect-oblivious. Evaluation order is readable from signatures. Rows remain
the only effect surface, and diagnostics stay in row vocabulary.

**Rule 4 was agreed and then worked around four times, and every stall in this design's history traces to
that erosion.** Recorded so it cannot read as new — the vocabulary is v5's, and the lesson is not:

| # | how rule 4 was worked around | what it cost |
| --- | --- | --- |
| 1 | bare-generic slots exempted from rule 1 — "mode belongs to the instantiation" | six days; a mode resolver, obligations, splice-restart (~350 lines), all reversed |
| 2 | declined a row on `foldLeft`/`foldOption` for ergonomics | the elaborator could not hoist at a generic-return callee *at all*; kept the whole payload router alive |
| 3 | the derived discharge stack routed a computation through `.`'s **rowless** slot "as data" | 5 `State`-family miscompiles; made "a generic is a payload" false, so nothing downstream could assume it |
| 4 | `foldOption` left with a strict `ifNone` because both declared spellings failed | a silent lazy-branch failure mode |

A fifth move was proposed and rejected in that form: *let the elaborator's payload test accept a
generic-headed return*. It **approximates** rule 4 instead of **declaring** it in the signature.

## 2. The surface

Three constructs carry the whole model.

**An `effect` is an ability with no carrier binder**, declared with its own keyword. **A member's row lists
what it performs *beyond* the ability it belongs to** — membership in the block already says the member needs
that binding, exactly as `show` inside `ability Show[T]` does not repeat `~ Show[T]`. So `{Console}` on a
member of `effect Console` is not written; a member's row is real where it names *other* effects. A function
that needs no such binding is not a member: it lives outside the block as an ordinary def with its own row,
as `updateState`, `orRaise` and `orAbort` do beside the primitives `state`, `putState` and `raise` inside.

```eliot
effect Console {
   def printLine(s: String): Unit
   def readLine: Option[String]
}

effect FileSystem {
   def readAll(path: Path): {Throw[IoError]} String
}

ability Show[T] {
   def show(t: T): String
}
```

**An anonymous `implement` is a default; a named one never is.** An anonymous block in one of the **two
sites** — the ability's module or the type's module — is the default for its pattern, subject to the existing
coherence and `where` rules, and is what a declaration with no named implementation binds. A **named**
`implement` may live anywhere, is never searched, is not checked for overlap, and its clauses may declare
rows — a clause row is what the implementation performs beyond its ability, charged wherever the name is
bound. It takes no parameters and closes over nothing: what it needs at runtime it asks an effect for.

```eliot
implement Console {                                       -- the platform's default
   def printLine(s: String): Unit = printLineInternal(s)
   def readLine: Option[String] = lineOrNone(readLineInternal)
}

implement session: Terminal {                             -- a test's, in the test module
   def write(line: String): {Writer[String]} Unit = tell(line ++ ";")
   def read: String = "Bob"
}
```

**`with` binds a name for its subject**: infix, subject-first, at the loosest precedence, left-associative,
so `xs.sort.render with reverseOrd` applies to the whole chain and `c with a with b` is `(c with a) with b`,
an inner `with` for the same ability shadowing the outer within its subject. It works on an ability exactly
as on an effect, and there are **two positions, one construct**: an expression in a body, and a slot's type
in a signature — the same split as `f(x)` and `List[Int]`, both application under the types-are-values
cornerstone.

```eliot
def greetTranscript: String = runWriterToLog(greet with session)
def sorted: List[Int] = sort(xs) with reverseOrd

def transcriptOf(program: {Console} Unit with recordingConsole): String = runWriterToLog(program)
```

The slot form reads *"this slot's computation, run with `mockConsole`"*: the actual delivered there has its
binding applied by the callee's signature, and the caller writes nothing. It is the one way a callee decides
the binding of calls it cannot see, since an actual's calls are bound in the caller. `with` **inside** a row
(`{Console with mockConsole}`) is rejected: it would put an ability on the left instead of a subject, a
second grammar repeating a pairing the implementation already declares. A `with` is accepted on a parameter's
or a field's type only, never on a def's own return row, which would be a second spelling of `with` around
the body.

**`with` is written almost nowhere.** Most code fixes nothing: a def declaring `{Console}` receives its
binding from its caller, up to `main`. That chain is what makes a fake possible — `greeting` never said
which console, so a test may say. A `with` in production code is the same mistake as a hard-coded dependency.

### 2.1 Rows, and the empty row `{}`

A row on a **return** is what the definition performs and does not discharge. A row on a **slot** is what the
slot receives unrun, and it is the declaration that lets the walk cross into the argument (§1 rule 4).

`{}` is the empty row: *"a computation, and I add nothing to it"*. It is the spelling of every
suspended-but-effect-transparent slot — `fold`'s arms, `else`'s fallback, `catch`'s handler, `.`'s `f` — and
it is the only spelling in the tree. It supplies no entry, so an operation inside such an argument is bound
by whatever the *caller* declares, and the argument is not run until the callee runs it. It needs no import,
and it puts nothing in the user's scope: under v5 `{}` named the machinery ability `Effect` and the whole
`eliot.carrier` package existed to hold it; both are gone.

A row with a base (`{Throw[E] | Id} A`, `{| Recorded} A`) has no v6 meaning — there is no carrier stack to
name — and is rejected by the desugar rather than silently read as an open row.

### 2.2 A parameter row is *supplied* — what makes a discharger

A **non-empty** row in a parameter position says *"I will run this computation, and these bindings come from
me"*. Which entries those are is read one level up, at the declaration:

- an entry the definition's own declared (return) row already has is **not** supplied — `if`'s
  `value: {Abort} T` rides the caller's `Abort`, because `if` declares `Abort` itself and the walk continues
  outward;
- an entry it lacks is **supplied**, bound by the slot's own `with` or, with none, by `Default`.

```eliot
def if[T](condition: Bool, value: {} T): {Abort} T                     -- performs Abort
def else[A](computation: {Abort} A, fallback: {} A): A                 -- supplies Abort: discharged here
def catch[E, A](computation: {Throw[E]} A, onError: E => {} A): A      -- supplies Throw[E]
def runStateToPair[S, A](initial: S, p: {State[S]} A): Pair[A, S]      -- supplies State[S]
```

Read as English they are already right, and that is the whole of what a discharger is: no tail, no base, no
second concept. Only a **top-level** parameter row supplies; a row in an arrow codomain (`onError: E => {} A`)
is the callback's own row, bound where the callback's body is written.

A supplied entry's own **type arguments** are written at the call from the actual's declaration, matched
entry-by-entry against the slot's row (`bad : {Throw[String]} String` against `Throw[E]` gives
`E := String`). Where no declaration answers, the call spells it — `runThrow[AssertionError, Unit](body)` —
and an argument nothing determines is **rejected** rather than defaulted, which is what stops a `catch` from
compiling against a frame it will not meet at runtime. Three shapes reach that rejection honestly, and all
three are ordinary: the actual is a **parameter reference** (a parameter has no callee whose declaration
could state the row); the actual **raises nothing**, so its row has no entry of that ability at all; or the
slot's row names the **same ability twice** (`{Throw[IoError], Throw[AssertionError]}`), where only the call
can say which one is being discharged.

Since v5's supply rule was per *entry* and syntactic, a definition could not supply an entry its own declared
return row already named. That limitation is **gone**: discharge is a frame installed at the discharger's
call, and the nearest enclosing frame is its own, so a definition may now discharge the very effect it
declares — which is what lets `describedAs(body: {Throw[AssertionError]} Unit): {Throw[AssertionError]} Unit`
catch its body's failure and re-raise a better one.

### 2.3 Storing a computation

A row-typed `data` field is the one place a type holds a computation, and it needs no extra spelling:

```eliot
data Task[E](step: {Throw[E]} String, label: String)
```

The field lowers to a thunk. Its calls are bound where the constructor is applied, and the field's row is the
declaration that covers them there — so `Task(failing, "load")` binds `failing`'s `Throw` at the
construction site rather than charging it to whoever built the value. Reading the field runs it: the accessor hands back the
thunk and the read applies it, so the entries the field's row declares are **charged at the read**, and an
undeclared read is the ordinary "performs but does not declare" error there. Nothing captures a frame, so
nothing can dangle — a frame is installed by a discharger's call and left when that call returns or is exited.

The accessor's return is the field *as stored*, deliberately not a return row: a return row would mint a
phantom binder and claim the accessor performs those effects.

Because the read binds nothing, a `with` covering one of the field's entries **at the read** is an error, not a
rebinding — in both of `with`'s positions, a body's and a slot's type, since it is one construct. It would
otherwise absorb the charge without changing what runs: the effect stops propagating outward while the thunk goes
on running the implementation it was built with. What stays legal over a read is what merely *describes* it — this
definition's own declared row, which propagates the effect to its caller, and the `Default` a discharging slot
supplies, whose frame the thunk enters at runtime (`runThrow(step(t))`).

### 2.4 Naming a row

A **row alias** is a type alias whose body is a row, and it names the row together with its payload:

```eliot
type Git[A] = {Process, FileSystem, Throw[IoError], Throw[GitError]} A

def publishedTags(root: Path, id: PackageId): Git[List[TagRef]] = …
```

Landed 2026-09-11 (`afd6d34c`) as a **splice**: `core/processor/RowAliasExpander` substitutes the alias's
arguments into its row and puts the row where the alias stood, *before* the binders are minted, so every later
phase sees the definition the user could have written by hand. Two limits, both interim and both lifted by
Part II §9: it works in **return position only**, every other position being an error rather than a silent
widening (before the splice a row in an alias body was erased with no diagnostic, so `type Printing = {Console}
Unit` silently meant `Unit`); and it is **file-local**, because it runs at `core`, one phase before the module
dictionary exists.

What has no spelling is v5's `ability Web[F[_] ~ Console & Log]` — a set of abilities required *of a binder*.
With no carrier binder there is nothing to hang such a requirement on, and a name for a set of
*implementations* is likewise deliberately absent (§12, "not now").

What survives is the ordinary **superability closure** on a `~` constraint: `~ A` is closed under what `A`
itself requires of the parameter this use bound to this binder (`ValueResolver.superConstraints`, transitive
and idempotent). That is a relation between abilities and their parameters, and it never lands an ability on
something it was not written about.

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

- **A row alias with its own AST node.** `type Git[A] = {…} A` names a row through the `{E} A` node the
  parser already has (§2.4). A bare set of abilities with no payload (`type Web = {Console, Log}`) is not a
  type and gets no node: paying for it with a new `ast.fact.Expression` case is not how this language grows.
- **A name for a set of implementations**, and a `with` that binds several at once. §12, "not now".
- **A closed row.** A slot's row cannot say *"and nothing else"*: an entry the slot does not supply continues
  the walk into the caller's scope, so there is no way to forbid what the caller allows. This is what deleted
  `eliot.test`'s `pure { … }`, whose whole meaning was that pin; making it expressible is a language
  addition, not a library one (§7).
- **A negative effect.** Discharge is a frame, so a discharged entry simply never joins the row.
- **Row or binding inference of any kind.** See §5.

## 3. The mechanism

### 3.1 The desugar writes the implementation

Two passes, both syntax-directed, both reading declarations only.

**`core/processor/EffectSugarDesugarer`** turns each row entry and each `~` constraint into **one phantom
generic binder**: a binder of kind `Type` that occurs in the generic list and in **no parameter or return
type**, so rows still never flow into types (§3.3). Three rewrites and nothing else:

```
def greeting(name: String): {Console} Unit    ⟶   def greeting[Impl](name: String): Unit      -- Impl ~ Console[Impl]
def sort[T ~ Ord[T]](xs: List[T])             ⟶   def sort[Impl, T ~ Ord[Impl, T]](xs: List[T])
computation: {Throw[E]} A                     ⟶   computation: Unit => A
```

Minted binders are a **leading prefix**, because `ValueReference.typeArgs` applies positionally and the write
is a prefix write; for an ability member the prefix moves, since its leading binders are the ability's own and
its binding must stay the last ability-level type argument. The pass is idempotent, because `CoreProcessor`
applies it uniformly to definitions the ability lowering has already produced.

**How the write recognises a phantom binder today — and this is what Part II §9 replaces.** Nothing marks one
past the AST. `GenericParameter.inferable` is set by the desugar and dies at `core`, where `CoreProcessor`
collapses it to a *count* (`NamedValue.inferableArity`, forwarded by every later fact and read by none);
`BindingWriter.mintedPhantoms` then re-derives the set from the signature alone — a binder is a binding iff it
occurs in no parameter and no return type *and* is the first type argument of one of the definition's own
constraints — and an ability member's own slot is a fourth rule, keyed on `Qualifier.Ability`. A count can only
describe a prefix, which is why the write is a prefix write and a binding behind an undetermined binder is
reported instead of written (`nonPrefixPhantom`: a member of a *parameterised* ability declaring effects of its
own). It is also why the row alias (§2.4) is spliced textually before minting: applied to binders, the alias
would *mention* them, and the write would stop seeing them as bindings.

**`row/BindingWriter`**, run by `RowElaborationProcessor` between the recursion gate and saturation, then
rewrites one definition so that every reference carries the implementation each of the callee's phantom
binders stands for. Three jobs, one walk:

1. **write the bindings** — each phantom binder is given a value by the resolution order below, as a leading
   positional prefix. A binder is never left to a metavariable.
2. **thunk and apply** — an actual delivered to a row-typed slot is wrapped in a lambda, and a reference to
   one of *this* definition's row-typed parameters is applied to `unit`. Doing both unconditionally is what
   makes a pass-through (`runAbort(computation)`, `val restFailures = rest`) come out right with no inspection
   of the argument's shape or type: wrap and apply are inverse, so a pass-through is an η-expansion.
3. **erase `with`** — the node exists to put a binding in scope for its subject; once the subject's references
   carry it, it is dropped, from a body and from a slot's type.

**Where a binding comes from — the resolution order**, walking outward lexically:

1. the nearest enclosing `with` for that ability;
2. this definition's own phantom binder for it — a **received** binding, filled by its caller. There is no
   graph to sum: forwarding is the enclosing signature, read once;
3. for an actual at a row-typed slot, the entries that slot **supplies** (§2.2) — bound by the slot's own
   `with`, or by `Default` with none;
4. `Default`, "search at the ground arguments" — today's two-site resolution — for an ordinary **ability**.
   For an **effect** there is no default: an uncovered one is the "performs but does not declare" error,
   reported here at the reference.

**Effect-ness is read from one place only**: the callee's declared row. An ability appearing in
`effectRow.returnEffects` is an effect at this reference — which for an `effect`'s member is what membership
recorded, and for an ordinary definition is what its `{ … }` says. A `~` constraint's ability is in no row, so
it defaults. Nothing keys on a name or a shape.

**Both halves of a definition are written, because both hold references.** A guarded return
(`def head[COND: Bool]: if(COND, String[]) else raise("empty")`) is compile-time code in type position, and
its `if`/`else`/`raise` are ordinary calls with row-typed slots and phantom binders. What differs is only the
scope check: a signature's effects are the guard channel's vocabulary, discharged by the guarded-return read
rather than performed, so an uncovered one defaults instead of being reported. The same exemption covers the
platform **run boundary** (`row/RunBoundaryFunctions`), which is where every effect's chain ends.

**Pure code is untouched**, and so is code that only forwards: a definition with no rows, no constraints and
no rowed callee is returned unchanged.

Two mechanical invariants of the pass, both of which cost real bugs to learn:

- **Position fidelity.** The walk returns the **original** nodes when nothing changed. Rebuilding an equal
  spine re-attributes it to per-argument positions, silently moving every diagnostic anchored at a call and
  duplicating LSP hover hints.
- **The universe is built by demand, not guessed.** `RowChecker.Universe.onMiss` reports every name consulted
  but absent; the processor fetches exactly those and repeats until a round misses nothing new. Guessing would
  fall back to unknown-callee approximations, and an unwritten binder silently runs on the platform's default.

### 3.2 What the write may consult

Declarations, and nothing else: the callee's declared parameter and return types, its declared row, its
membership in an `effect`/`ability` block, the implementation names a `with` resolved to, the run-boundary
registry, and one level of type-alias expansion inside those signatures.

v5 needed a written *whitelist* here (§5's retired rule 5), because its elaborator had to classify each
slot — carrier-headed or payload, pinned or open — and every classification is a place an approximation can
accrete. **There is no classification left to approximate**: a slot either declares a row or it does not, and
that is read off the declaration. A rule that inspects a *sibling argument's expression shape* is inference,
not desugaring, and is still prohibited; a decision that cannot be made from a declaration is a gap to close
**in the declarations**.

The fail-safe direction is built in: a missing write is an aborted definition with a violation at its own
position, never a binding silently taken from the platform's default.

### 3.3 One verifier, and one precondition

Checking a runtime term yields a **payload type** (the existing NbE judgment, which never sees an effect)
and a **row** (a second output, exactly as an `Int`'s range lives in the refinement channel beside the type,
not inside it). Row constraints are set-shaped: union for sequencing, inclusion for boundaries
(`derived ⊆ declared`) — commutative and order-independent, so no argument-order sensitivity can exist.

**The verifier is the scope check**, per definition, pre-monomorphization: not a separate pass but the write's
own walk (§3.1). An operation or a rowed callee needs a covering declaration, and the only places one can come
from are the enclosing def's row or constraints, an enclosing `with`, or a slot's row. It is complete before
monomorphization, since nothing about it is instantiation-dependent; it emits *"This value performs the effect
'X' but does not declare it…"* at the reference, and it owns the diagnostic for a `with` whose subject contains
no covered use. *Forward what is declared, derive what is done* — a forwarded per-operation verdict would be a
checker self-report and is rejected, as is any negative-effect surface.

There **was** a second one, post-monomorphization, re-deriving each ground instantiation's row and checking it
against the declaration. D7 (§11, closed) retired it, on the measurement it asked for rather than on the argument: under
names its "performs X" could only mean "a reference forwards a **received** binding to a callee declaring X as a
row entry", which sees propagation through a *declaring callee* but never a direct operation call — by then
`AbilityResolver` has rewritten `printLine` into the implementation method, which declares no row. Its coverage
was therefore a strict subset of the scope check's with no case of its own, and a second place to maintain one
diagnostic. **Do not reintroduce a post-mono effect verifier**: what a monomorphic body can still say about
effects is strictly less than what the declaration walk already said.

What survives at that seam is **not** its shadow: `monomorphize/channel/SuppliedRowArgumentsProcessor`, wired as
a **codegen precondition** via `getFactOrAbort`, rejecting a supplied row entry whose type argument nothing
determines (§2.2). That check needs ground arguments and so cannot move earlier.

### 3.4 The three primitives

A resuming clause is a call and needs nothing. A finishing clause is a **non-local exit**; a stateful
implementation needs a value **threaded through calls that never mention it**. Neither is expressible as a def
in a strict pure core — under v5 both were expressed by the transformer instances, which is the whole reason
the carrier existed. They are now three **platform-private leaves**, one per target, and nothing else:

| primitive | shape | jvm | microcontroller | compile track |
| --- | --- | --- | --- | --- |
| escape | `escapeInternal[K, A, R](body: {} A, onExit: K => {} R, onValue: A => {} R): R` — abortive, never re-entered; the frame is the machine stack | an exception, one class per instantiation | a status flag and a jump (hypothetical, §13's A2) | an evaluator intrinsic |
| cell | `withCellInternal[S, A, R](initial: S, body: {} A, combine: A => S => {} R): R` — scoped to one call, saved and restored around it | a static field per instantiation | a register | an evaluator intrinsic |
| loop | `foreverInternal` | `while(true)` | the super-loop | never runs (`Inf` is stuck) |

The control effects' single implementations are written over them: `Throw[E]`'s `raise` is `exit`, `State[S]`'s
`state`/`putState` are `read`/`write`, `Writer[W]` appends to a cell, `Dep[T]` reads one. The frame an operation
reaches is the **nearest enclosing** one of its instantiation — the machine stack's discipline, inside the leaf
and nowhere else.

They are **private, and the dischargers are therefore abstract in the base.** A public cell is Landin's knot —
a cell holding a closure that reads the cell is a loop, and `termination/PurityGuardTest` exists to keep it out
— so `withCell` may not be a base name, and `escape` follows for uniformity. `runThrow`, `runAbort`,
`runStateToPair`, `runWriterToPair` and `provide` are body-less signatures in the base and bodied per platform;
`catch`, `else`, `runStateToValue`, `runStateToFinalState`, `runWriterToValue` and `runWriterToLog` are ordinary
platform-independent bodies over those and stay in the base, where the base-layer rule says they belong.

Eliot has no *layer*-private visibility — `private` is module-scoped — so each jvm module needing a primitive
declares its own copy. That is five copies of two shapes, and it is the right trade: one public `eliot.jvm.Cell`
would put a mutable cell in reach of any jvm program. Repetition also separates the frames for free, since the
backend keys a frame class on the name that installed it.

### 3.5 Discharge

**Discharge is a frame, not a layer.** A discharger installs the frame its effect's operations exit to or
thread through, and the entry simply never joins the row — so there is nothing to spell as a negative effect,
and a wrapper-reached discharge inside a `{Console}` body just compiles.

**Nesting order at the run site decides interaction**, and it is written where the frames are installed:
`runStateToPair(s, runThrow(c))` versus `runThrow(runStateToPair(s, c))` is the difference between state
surviving a `raise` and not. There is no canonical form to fix and no ordering for the compiler to choose.

A discharger may be called any way a function can be — v5's "a discharger must be called directly" rule has no
subject, since a thunk is a plain value and passes through the dot's plain `T` as the value it is. A
discharger's **handler may itself perform effects** (`onError: E => {} A`, bound where it is written), and a
`val`-bound computation is dischargeable.

**Two families, and only one is rebindable.** The **control effects** — `Throw`, `Abort`, `State`, `Writer`,
`Dep`, `Inf` — have exactly one implementation per platform, over the private primitives; `with` has nothing
to choose there. The **interpretation effects** — `Console`, `Log`, `FileSystem`, `Process`, `Environment` —
and every ability are what `with` and a naming slot are for. The families are a description, not a bit in the
language: nothing keys on it.

### 3.6 An effect is declared, never read off a shape

v5 read effect-ness off an ability method's declared row; v6 moves the declaration to the block, where an
ability's already is. Membership in an `effect` block says a member needs that binding; a **constructor
class** is simply an `ability` (`ability Container[F[_]] { def wrap[A](a: A): F[A] }`), and needs no exception.

**No phase reads effect-ness from a name, a shape, or a higher-kinded binder.** The one reading is the callee's
declared row (§3.1). The `<Ability>Carrier` naming convention, an LSP reverse table and "has an `Effect`
instance" all miscompiled in both directions under v5 and are prohibited; under v6 there is no carrier to
recognise at all, so the prohibition has nothing left to guard and is kept only so it is not reinvented.

Nothing of carrier identification remains: the dead `Effect`/`Suspend` pocket (`EffectMachinery`,
`EffectCarriers.declaredEffects`, `ModuleName.carrierPackage`) was deleted on 2026-09-10 (`90b74273`). The one
structural predicate that survived is **not about effects**: `CarrierKindChecker.isHktBinder`, "is this binder
higher-kinded?", which `check/CarrierKindChecker` asks in order to reject a `[F[_]]` binder instantiated at a
fully-applied proper type. That is a kind system living next door, and it is soundness (§12).

### 3.7 Rendering

There is nothing to invert. An effect is an ordinary nullary ability, an implementation is a name, and a
computation is a thunk, so a type contains no machinery to hide: `GroundValueRenderer` prints what the user
wrote. v5's inverter — a canonical `ThrowCarrier[E, StateCarrier[S, Id], A]` stack rendered back as
`{Throw[E], State[S] | Id} A`, plus the `Id[X]` ⤳ `X` erasure — went with the carrier, and with it the two
entry points it needed, which existed only because a carrier's last argument meant one thing applied to a
payload and another unapplied.

### 3.8 What the checker holds

**No effect rule at all.** v5's one survivor, `EffectLifter.tryPureWrap`, had a carrier-headed expected type
for its subject and is deleted with it. There is no lift, no `pure`, no `Id`, and no effect diagnostic in the
checker.

Two things beside it are **not** effect machinery and must not be deleted as such (measured — see §12):
`check/CarrierKindChecker` (§3.6) and the compile track's mid-spine default ladder with its deferred slots
(`Track.Compiler`, `Unifier.resolveDeferredSlot`) — the sole live reader of the `Unifier`'s higher-kinded-meta
record.

## 4. The scope check, as a spec

A `Row` is a set of effect-ability entries (production: multiset of *(ability, type-args)*). The check is per
definition, over the operator-resolved body **and signature**, reading only *declared* information. It is the
write's own walk (§3.1), so this is a spec of `row/BindingWriter` rather than of a separate pass:

- **the environment** at a point is `bindings` (innermost-first, so a nearer `with` shadows an outer one for
  the same ability) and `thunks` (the row-typed parameters in scope, whose references apply to `unit`).
- **a reference** to a callee with phantom binders resolves each binder by §3.1's order. An **ability** with
  nothing in scope binds `Default`; an **effect** with nothing in scope is the error, at that reference.
- **entering an argument** at slot *i*: the walk descends with the slot's supplied entries bound (to the
  slot's `with`, else `Default`) iff slot *i* declares a row. A rowless slot is descended into with no slot
  scope, so a lambda there reaches nothing enclosing.
- **a `with`** binds its ability for its subject's text, and crosses a def boundary only through a
  declaration. A `with` whose subject contains no covered use and no call to a declaring def is a hard error
  naming the fix, never a silent no-op.
- **a stored read is charged, never bound**: a saturated call to a `data` field accessor whose field declares a
  row runs the thunk, so the field's entries are charged there (§2.3). They need a covering *declaration* — this
  definition's row, or a slot's supply — and a `with` in scope for one of them is the error at the read, in either
  of `with`'s positions.
- **a clause row is charged at the binding site**: `greeting("Bob") with recordingConsole` performs
  `Writer[String]` there, because the name's clauses declare it, and the binding for it comes from the same
  order.
- **two exemptions, both because something else answers**: a **signature**, whose `raise`/`abort` is the guard
  channel's vocabulary and is discharged by the guarded-return read; and a platform **run boundary**, where
  every effect's chain ends and each of `main`'s entries is bound to the two-site `Default`.

**Suspension is row-neutral.** Whether a slot is strict or declared-suspended changes only *when* the effect
runs and *whose* declarations bind it — never whether the caller must declare what it performs.

**Two same-ability entries** at different arguments are two entries; at identical arguments they deduplicate.
In one slot's row they are the shape §2.2 requires the call to spell.

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
3. **There is nothing to infer.** A phantom binder is written by a `with`, by the enclosing declaration, by a
   supplying slot, or as `Default`, in that order — never left to a metavariable, a join, a lattice, an
   ordering-sensitive slot decision, or a sum over a monomorphized sub-graph. Carrier inference was the
   historical bug class (carrier theft, premature commitment); binding inference would be the same bug in the
   new vocabulary, and **dynamic scoping resolved at compile time** is its exact shape. Both are prohibited.
4. *(Retired: "carrier-ness by tag, never by name or shape" — no subject. §3.6 keeps what it protected.)*
5. *(Retired: the elaborator whitelist — no subject. §3.2 says what replaced it.)*
6. **A component may read solved metas and splice rewrites; it may never run inside unification, never
   retract a solution, never grow an ordering arm.** A shape that genuinely needs mid-drain resolution is a
   stop-and-redecide signal, not a licence for a mid-flight arm.
7. **A rule invented so that one idiom elaborates is a rule shaped by that idiom, whatever its stated
   generality.** Delete the rule; fix the declaration.
8. **Fail-safe direction.** Every bound, every deferral and every declination must be able only to *withhold*
   a permission, never to grant one silently.

## 6. Testing — a named implementation is the injection point

Production code that declares an effect row commits to no interpretation: it names no implementation, so
whoever runs it decides. In production that is the synthesized entry point binding each of `main`'s entries to
the platform's default; in a test it is one `with`.

```eliot
effect Terminal {                                  -- the application's own effect
   def write(line: String): Unit
   def read: String
}

def greet: {Terminal} Unit = {                     -- production code, untouched by the test
   val name = read
   write("Hello, " ++ name ++ "!")
}

implement session: Terminal {                      -- the test's double, in the test module
   def write(line: String): {Writer[String]} Unit = tell(line ++ ";")
   def read: String = "Bob"
}

def greetTranscript: String = runWriterToLog(greet with session)
```

Four properties fall out of the design rather than being added for testing.

- **A double is one declaration.** It needs no type to hang on, no colocation with the ability or a carrier,
  and no coherence question: a named implementation is never searched, so it may freely overlap a default.
- **A double cannot cheat.** A user module cannot declare a native, and the platform's natives and primitives
  are private to its layer, so an implementation reaches the world only through effects **its own clauses
  declare** — which are charged, and bound, at the binding site.
- **A double keeps its own state through an effect**, not through a carrier: `session` above writes to
  `Writer[String]`, and that entry is charged where `session` is bound and discharged there by
  `runWriterToLog`. It never appears in `greet`'s row.
- **Interpretation is per effect, not per program.** `body with mockConsole with mockFileSystem` binds two
  doubles and leaves everything else at its default; under v5 one type argument decided every effect at once.

`eliot-test` is the worked framework: `mocked` binds five doubles on its slot's type, so a unit test writes no
fixture at all and reads `"…" should "…" in mocked { … }`. `examples/src/EffectsNamedEffect.els` is the
minimal version of the same thing, and `EffectsFakeConsole.els` does it for a stdlib effect.

**What this deletes from v5's testing story**, all of it symptom rather than design: the fake carrier and its
`Effect` instance; the rule that a fake gets no lifting because it has no `Suspend` (the n² cross-lift wall);
the region rule and the `{| Recorded}` capture tag that opted out of it; "do not stack over a fake"; and
run-then-assert as a necessary shape. `Dep[X]` + `provide` remains available for a seam you want stated in the
signature, and swapping the platform layer remains the whole-program integration answer.

## 7. Live limitations

Each is stated, fail-safe, and either has a plan entry or is a deliberate trade.

1. **A row cannot be closed.** A slot's row says what it supplies, not what it forbids: an entry it does not
   supply continues the walk into the caller's scope. So there is no way to write "this body may perform
   nothing at all", which is what `eliot.test`'s `pure { … }` meant and why it is deleted. Making it
   expressible is a language addition and is not planned.
2. **A discharger's type arguments are sometimes written by hand.** Where no declaration determines a supplied
   entry's arguments (§2.2's three shapes), the call spells them. The rejection is loud and names the fix; the
   alternative — defaulting — compiles a `catch` against a frame it will not meet.
3. **An under-applied backend *intrinsic* has nowhere to link.** An intrinsic is emitted inline at each call
   site, so only a saturated call can be emitted at all; `digits.map(show)` for such a name is a hard error at
   the definition naming the fix (`digits.map(n -> show(n))`). This is a gap in the backend, not a rule of the
   language — the same shape for an ordinary native, ability-implementation or not, is supported.
4. **A guarded return cannot carry its author's message.** The compile-track `Throw` went with the carrier
   (the flag day's F5, §13), so a `raise("…")` in a signature reduces without its text. Accepted rather than fixed; the route
   back is keying `Throw`'s compile-time frame on a fixed marker the way `Abort` keys on `Aborted`, at the cost
   of two instantiations sharing one frame.
5. **A row alias works in return position only, and only within its file** (§2.4). A slot or a field
   doubling many effects still writes the full chain. Interim: Part II §9 lifts both limits.
6. **A stored computation's binding is fixed where it is constructed.** Deciding the handler before storing is
   unambiguous and easier to understand; losing first-classness is the accepted price. A `with` applied to a
   stored computation later is an error, never a rebinding — rejected at the read, in both of `with`'s positions.
7. **An undischarged control effect reaching `main` fails at runtime, not at the boundary.** The run boundary
   binds every entry of `main`'s row to the two-site default, and a control effect's single implementation then
   exits into no frame. §3.1 asks for an error at the boundary naming the fix; telling a control effect from an
   interpretation one needs a bit the language deliberately does not have (§3.5). Loud, so fail-safe; open as
   D19 (§11).

---

# Part II — What is left

**Status (2026-09-11).** v6 landed on 2026-09-09 and its record — the reasoning, the flag-day log, the
follow-ups — is no longer here: Part I states what it built, and git history holds how (Part III §13 says how
to read a citation to it). This part holds only what is *not* done: where the tree still diverges from Part I
(§8), the one change decided and not yet built (§9), the method any such change is run under (§10), and the
decisions still open (§11). Every entry marked **decision** is Robert's.

## 8. Where the tree diverges from Part I

Found by probing the compiler while the user docs site was rewritten for v6 (2026-09-10) and by the flag day's
own record. Part I wins by standing rule 1, so each is a defect to close or a decision to make, never a doc
fix. The silent ones come first, because a silent acceptance is the one failure mode this design forbids
(standing rule 8).

1. **An unused `with` is a silent no-op.** §4 says a `with` whose subject contains no covered use and no call
   to a declaring def is a hard error naming the fix. No such check exists anywhere in `row/`. It needs
   something the write does not track — whether a binding was ever *consumed* — and is the same
   silent-acceptance family that A11 closed for a stored read (§2.3).
2. **A dot-read of a row-typed `data` field hands back the thunk.** Only the call form `step(task)` is a read
   (§2.3); `task.step` is a type error where a value is expected and a **silent no-op** as a block statement
   (`job.run` printed nothing). The `.` operator lowers to the same saturated accessor call, so the read rule
   should fire there too, and does not.
3. **`val x = comp` then `x else …` fails** with "performs Abort but does not declare". §3.5 says a
   `val`-bound computation is dischargeable; rule 1 says a plain position runs its expression, and a `val`'s
   right-hand side is a plain position. The two statements conflict *within* Part I, so this is **D17** (§11):
   the fix is either to the tree or to §3.5, and the second is a reversal, not a fix.
4. **`runThrow("no failure")` is accepted.** §2.2 lists an actual that raises nothing among the shapes
   rejected for an argument nothing determines; the tree accepts it when the call spells the argument. Every
   case is loud (the argument is written by hand), so this is last.
5. **An undischarged control effect reaching `main` fails at runtime**, not at the boundary (§7 item 7).
   **D19** (§11).
6. **A row alias works in return position only, and only within its file** (§2.4). Interim by design;
   §9 lifts both limits.

## 9. The next change: a binding binder is marked by its type

**Decision (2026-09-11).** A binder the desugar mints — for a row entry, for a `~` constraint, or for an
ability block's binding slot — is declared with the type `Implementation[A]`, `A` being the ability it binds,
and every phase that needs to know which binders are bindings reads that declared type and nothing else.
Abilities and effects are one mechanism here, so one marker serves both.

**Where the tree is (2026-09-11).** Steps 1–2 of §9.3 are built: every binding binder now carries its mark, and
nothing reads it yet. §9.2 records the two corrections that step measured. Steps 3–4 — the write reading marks,
and the four dead encodings going — are next, under the same byte-identity gate.

### 9.1 What the tree does today, and what it costs

Part I §3.1 states it: nothing marks a phantom binder past the AST, and the same fact is encoded four times —
`GenericParameter.inferable` on the AST binder, the count `inferableArity` that `CoreProcessor` collapses it to
and every later fact forwards unread, `BindingWriter.mintedPhantoms`' re-derivation (unmentioned in every
parameter and return type *and* the first argument of one of the definition's own constraints), and the
`Qualifier.Ability` special case for a member's own slot. Each costs something concrete:

- **The prefix rule.** A count describes only a prefix, so the write is a prefix write, and a binding behind a
  binder no declaration determines is an error (`nonPrefixPhantom`): a member of a *parameterised* ability
  declaring effects of its own (`ability Show[T] { def show(t: T): {Log} String }`) cannot be written at all.
- **The splice.** A row alias applied to binders *mentions* them, so the write would stop seeing them as
  bindings; `RowAliasExpander` therefore β-reduces the alias textually before minting, which is what confines
  it to return position (a parameter would have to be thunked, a rewrite of the slot rather than of the type
  naming it) and to one file (`core` has no dictionary).
- **Fragility.** A user's unmentioned `[P]` is told from a binding only by the constraint rule, and four
  encodings must be kept in step by hand.

### 9.2 The form chosen, and the two not chosen

The marker has three possible homes, and the reasoning is recorded because the first two are the natural
readings:

- **A flag on the signature's `FunctionLiteral`.** From `core` on, a signature is one expression — a chain of
  `FunctionLiteral`s, one per generic binder, around the curried `Function[…]` arrow chain — so a binder *is*
  that node, and `SignatureView.Binder(name, parameterType)` its projection. A new field there lands on every
  phase's expression case class and every match over it, and on runtime lambdas in bodies where it is always
  empty. Rejected on cost.
- **A typed marker the checker verifies.** Give the implementation marker's signature the return type
  `Implementation[Console]` in the ability lowering and declare `Default` over `Implementation[A]`, so the
  checker rejects a wrong name in the slot as an ordinary type error. Cornerstone-faithful, and **deferred**:
  only the compiler ever writes that slot, so the check would catch a compiler bug and nothing else, at the
  price of teaching the checker a typing for implementation values it has no decision to make with. The
  upgrade is contained — drop the alias body, retype the marker, retype `Default` — and nothing in the write
  or in resolution moves if it is ever taken.
- **The existing `parameterType` slot, with an alias that reduces to `Type` (chosen).** A binder already
  carries an optional declared type (the kind annotation in `type Int[MIN: BigInteger]`). The prelude declares

  ```eliot
  type Implementation[A] = Type
  ```

  in `eliot.lang.Implementation`, beside `Default`, with its FQN in `WellKnownTypes`; the desugar mints
  `Impl: Implementation[Console]`, module-qualified so the mark never depends on the file's imports. **The
  checker changes nothing**: the marker names and `Default` keep their `Type` typing and the kind check of a
  binder against what is passed is unchanged. The write reads the operator-resolved signature, where an alias
  is not yet expanded (the evaluator expands it at monomorphization), so the syntactic head `Implementation`
  is what it pattern-matches — recognition by a well-known FQN, exactly as `&` is recognised (§2.5). No case
  class changes.

**Two corrections, measured when steps 1–2 were built (2026-09-11). Do not re-propose the original readings.**

- **The alias does not reduce away for the checker; the mark is erased at `row` instead.** The plan said
  definitional equality normalises `Implementation[Console]` to `Type` and the checker therefore never sees a
  mark. It does see one, and it **kind-checks the argument** before any reduction: an ability's marker has one
  binder per ability parameter *plus* the binding slot, so its kind is `Type -> Type` and upwards, never
  `Type` — `HelloWorld` failed at its own `{Console}` with "Type mismatch. Expected: Type, Actual: Type ->
  Type". No declared kind for the alias's parameter accepts every ability, so the fix is not a better kind:
  the mark is **dropped at the end of the `row` phase**, beside the `with` nodes that phase already erases
  (`BindingWriter.Writer.unmarked` rewrites a marked binder's declared type back to `Type`). The mark
  therefore lives between `core` and `row` and nowhere else, and "the checker changes nothing" holds *by
  construction* — it is handed the signature it was handed before the mark existed — rather than by a
  reduction. Nothing else about the chosen form moves.
- **The mark's argument resolves as an ability, not as a type.** An ability name is not in the type namespace,
  so the ordinary value path reached it only through the ability fallback, missed the fixed-FQN abilities
  (`PatternMatch`/`TypeMatch`) entirely, and reported a mistyped effect as "Name not defined." instead of
  "Ability not found.". `ValueResolver.markedAbility` recognises a mark by its head's FQN and sends the
  argument through `resolveAbilityName` — the one lookup an ability name uses (§2.5) — yielding the ability's
  own marker value. One consequence is worth stating, because it reaches every pool: the mark is written into
  **every** row, `~` constraint and `ability` block, so `eliot.lang.Implementation` is now required wherever
  any of those is declared. Every layer has it; a hand-built test pool must carry it
  (`ProcessorTest.implementationStubContent`) or the declaration itself fails to resolve.

The read side already is what a marked binder needs: the slot holds a *name* — a ground `Structure` headed by
the implementation's marker FQN or by `Default` — and `ImplementationBinding` reads it back for `AbilityResolver`
to use directly or to search. Nothing there moves.

### 9.3 The work list

1. **The alias and its FQN — DONE (2026-09-11).** `type Implementation[A] = Type` in
   `stdlib/eliot/eliot/lang/Implementation.els`; `WellKnownTypes.implementationTypeFQN`. Every stub prelude
   grew it, not only the ones that had the module: the mark is written into every declaration that takes a
   binding, so a pool without `eliot.lang.Implementation` no longer resolves (§9.2).
2. **Mint with the declared type — DONE (2026-09-11).** `EffectSugarDesugarer` for a row entry and a `~`
   constraint, `AbilityMembers` for the block's binding slot, both through the shared
   `GenericParameter.implementationMark`. `abilityLevel` keeps its other job — delimiting the ability-level
   prefix that `AbilityResolver` slices and that a member's own binders follow. Two things this step needed
   that the plan did not foresee, both in §9.2: `BindingWriter.Writer.unmarked` erases the mark at the end of
   `row`, and `ValueResolver.markedAbility` resolves its argument as an ability. Nothing reads the mark yet —
   the write still derives a phantom binder the old way — so the gate is that the mark is **inert**: 45/45
   example jars byte-identical, 1689 tests green.
3. **The write reads marks.** `BindingWriter.phantoms` becomes "the binders whose declared type is headed by
   the marker, with the ability read off its argument", in index order. Deleted with it: `mintedPhantoms`'
   non-occurrence test, `constraintStartingWith`, `prefixOf`, `nonPrefixPhantom` and the `Qualifier.Ability`
   arm. The positional write becomes one merge: written names fill the marked indices, and a caller's explicit
   `typeArgs` fill the unmarked ones in order (`suppliedArguments` reads "the unmarked binders" where it reads
   `drop(phantomCount)` today). Type-argument application stays positional; only the prefix goes.
4. **Delete the dead encodings.** `GenericParameter.inferable`, `ArgumentDefinition.inferable`, and
   `inferableArity` on `NamedValue`, `ResolvedValue`, `BlockDesugaredValue`, `MatchDesugaredValue` and
   `OperatorResolvedValue`, with every forward. The desugar's idempotence test ("is this binder one I minted?")
   reads the mark instead.
5. **The alias is ordinary.** `RowAliasExpander` and the splice in `CoreProcessor` are deleted. A row alias is
   a def with a return row, so the desugar mints its binders exactly as on any def
   (`type Git[I0: Implementation[Process], …, A] = A`). A use `def f(…): Git[X]` mints fresh marked binders on
   `f`, one per marked binder of `Git` with the same ability, and rewrites its return type to `Git[J0, …, X]`.
   The `J`s are **dead arguments** — `Git`'s body is `A` — so the application reduces to `X` before any type
   is compared, and rows still flow into no type (§3.3). The write sees `f`'s marks whether or not the return
   type mentions them. Return position first, file-local as today; the other positions are D18 (§9.5).
6. **Tests.** The phantom-discovery cases in `BindingWriter`'s suite move to the mark; the `nonPrefixPhantom`
   case inverts into a positive one (a parameterised ability's member with its own row is written); the row
   alias cases (six defs of `eliot-build`'s Cache.els shape) compile byte-identical applied instead of spliced.
7. **Documents.** Part I §3.1 is rewritten to the new mechanism and its "how the write recognises a phantom
   binder today" paragraph deleted; §2.4's two limits and §7 item 5 go; the CLAUDE.md cornerstone's "minted
   binders are a leading prefix because `typeArgs` applies positionally; for an ability member the prefix
   moves" is replaced by the mark; the `eliot-monomorphize` and `eliot-layers` skills are re-read for the
   prefix.

### 9.4 What does not change

The resolution order (§3.1), `with` in both positions, `Default` and the two-site search, `ImplementationBinding`'s
read, the monomorphization key, thunk-and-apply, the scope check and its diagnostic, and the per-position meaning
of a row (received at a return, supplied and thunked at a parameter, the callback's own in an arrow codomain,
bound at construction in a field). The checker gains no effect rule and no new typing. Type-argument
application stays positional. Nothing infers a binding: the mark says *which* binders are bindings, and the
resolution order still says what each is written to.

### 9.5 The alias in every position — the phase question (D18)

With marked binders the alias is ordinary at a return. At a **parameter or field** the desugar must first know
the slot is a row slot — to thunk it and record it as supplying — and under the mark that reads as "the slot's
type is headed by a type-level function with marked binders", which needs the alias's *declaration*. Two ways
to reach it:

- **File-local, at `core`**, reading the alias among the file's own definitions exactly as the splice does
  today. Cheap; keeps the limit that an alias must be declared in the file that uses it, which is the same
  discipline the layer model imposes on every other name a file needs (§2.4).
- **Move the minting and thunking after `resolve`**, where the dictionary exists. The honest one, and not a
  relocation: `EffectSugarDesugarer` sits beside the `~` lowering and the `data` split, and the split's order
  is load-bearing (A7: the `data` is split first so the constructor reaches the desugar with an ordinary
  parameter row). Which of its rewrites can move and which cannot has to be mapped before choosing.

**Decision needed before §9.3 step 5 goes beyond return position.** Recommended order: steps 1–4 under the
byte-identity gate; step 5 for return position, file-local; then D18.

### 9.6 Reversals this records

Standing rule 1: a reversal is written down as one, never amended in place.

- F1's rule (2026-09-09) *"phantom-binder discovery needs **no new metadata**"* is reversed. The no-metadata
  rule did not avoid metadata: it produced a count nobody reads, a re-derivation, a prefix constraint and a
  splice.
- Part I §3.1's *"minted binders are a leading prefix because `typeArgs` applies positionally"* loses its
  reason. Positional application stays; the prefix does not.
- §12's entry closing "a handler as a marker type" says the phantom binder *"occurs in no type"*. Refined, not
  reopened: it occurs in no parameter or return type of any value; it may carry a declared type that reduces
  to `Type`, and it may stand as a dead argument of a row alias that reduces away. Neither is the in-type
  binder that entry closed — nothing unifies it.

### 9.7 The gate

§10's byte-identity gate over the 45 example jars, every test green, and the six-def `Cache.els` shape
byte-identical applied instead of spliced. No example writes a parameterised ability member with its own row,
so the inverted `nonPrefixPhantom` case is the only witness for that gain — a shape with no example has no gate
(§10).

## 10. How a change is run

Kept from the v6 record, because every change to this channel is run the same way.

**The gate.** `./mill __.test` green, every example program carrying a `main` compiles, and every example jar
`md5sum`-identical to the pre-change build — `scripts/example-sweep.sh`, whose `jar.md5` lines are the gate.
Byte-identity is a **safety oracle, not a hard gate**: when the output legitimately changes wholesale it is
replaced by **behavioural identity**, the same script's `exit`/`stdout` lines compared before and after, with
the size and instruction-count difference stated in the commit. A regression there is a finding to explain,
not a cost to accept. A report is compared with its `#` header lines stripped; the cache under `target/` must
go between compiles; stdin must be `/dev/null`; and never run `./mill` while the sweep runs, or it rebuilds
`out/` underneath and the report reads as a large regression that is not there.

**What the gate cannot see.** It is a behavioural identity over the corpus the examples cover, and the corpus
does not cover the type-level surface: no example writes a guarded return type, a `foreach`, a `catch` with an
ignoring handler, or a stored computation, and four real defects hid in exactly that gap at the flag day. A
shape with no example has no gate; the jvm end-to-end suites are what separate it.

**When the question is "is this still load-bearing?"** — reuse the method rather than re-inventing it:

- **Arm-liveness tracing, not inspection.** A temporary env-gated tracer with a `fire(arm, sample)` call on
  every arm under consideration; run the whole gate *and* a compile of all examples; delete only zero-fire
  arms, at outcome granularity (a router's entry count is mostly routing).
- **Switch it off, part by part.** An env-gated bypass answers in behaviour. Gate each part **separately**: an
  all-off run once said "43 failures, delete nothing" while the per-part runs said one part costs 1 test and
  another 36, which was the whole finding.
- **An examples-only audit under-reports**, because elaboration is demand-driven: disabling *every* effect
  arm once still compiled all 45 examples to byte-identical jars.
- **A firing arm is a question, not a verdict**; **measure twice, before and after** — one deletion's first
  cut looked like an improvement while dropping a fail-safe.
- **Tracer gotchas**: mill prefixes every forwarded line with a worker id, so never anchor a grep; a sample key
  must carry the range **end** as well as the start and, for an argument, its spine **head**; delete the cache
  before every run or the pipeline replays facts and the trace comes back empty.

**Two traps in the test harness.** A `ProcessorTest` needs an `Implementation` stub declaring `type Default`
(and, after §9, the `Implementation[A]` alias) or every snippet calling an ability method loses its
monomorphization **silently, with no error at all** — the write puts `Default` at the reference as an ordinary
type argument and saturation demands the value it names. And a test stub that diverges from the real
declaration (`fold`'s suspended arms) fails silently too.

## 11. Open decisions

Numbers are kept where a cross-reference or a source comment uses them. Only rule decisions are listed;
every convenience is in §12 under "not now".

### D3 — `~` and `&` fully in user space

Still blocked on meaning, not difficulty. What is left is whether `~` and `where` unify, and the phase-order
blocker (the superability closure runs at resolve, before operators are structured) still lands first if that
is ever answered yes. Its half that asked "what does an ability denote as a value?" is answered under names by
*nothing*: an implementation is not a value, so an ability denotes no type of values.

### D17 — a `val`-bound computation: §3.5 or rule 1

§3.5 says a `val`-bound computation is dischargeable; rule 1 and F9's own measurement say a `val`'s right-hand
side runs where it stands and charges the enclosing definition (§8 item 3). One of the two is wrong, and only
Robert can say which: closing the tree to §3.5 means a `val` becomes a suspended position, which is a new kind
of slot; closing §3.5 to rule 1 is a reversal of a Part I sentence.

### D18 — a row alias at a parameter or a field: which phase reads the declaration

§9.5. Whether the desugar stays at `core` and file-local, or the minting and thunking move after `resolve`.
Needed before §9.3 step 5 leaves return position.

### D19 — an undischarged control effect at `main`

§7 item 7. The boundary binds every entry of `main`'s row to the two-site default, so a control effect reaching
it resolves to its single implementation and exits into no frame at runtime. §3.1 asks for an error at the
boundary naming the effect and the fix. Telling a control effect from an interpretation one needs a bit the
language deliberately does not have (§3.5, "the families are a description"); the candidates are a property of
the platform's *implementation* (one that only makes sense under a frame declares so) or accepting the runtime
failure as the answer. Loud either way.

**Closed, numbers kept so citations resolve.** **D4** (`Suspend`-riding effects: pinning and supplying)
dissolved at the flag day — a stored computation is a thunk bound where it is written. **D5** (a lambda body at
a rowless arrow slot) closed 2026-09-08 as rule 4's third bullet. **D7** (the post-mono accounting verifier)
closed 2026-09-10, retired by measurement — §3.3 and §12. **D13** (every `eliot-test` case built with its
handlers already applied) closed 2026-09-08, yes — `Mock` is `with` on `mocked`'s slot type.

---

# Part III — What is closed, and provenance

## 12. Closed by measurement or decision — do not re-propose

- **Carrier inference** — a carrier metavariable, a join solver, an `Id`-headed uniform judgment, a mode
  obligation, a post-drain mode resolver. Of 15 fix commits in the v2 window, the four highest-impact were
  one failure: a carrier metavariable captured by first-contact unification. Under v6 the class is still
  prohibited in its restated form (§5 rule 3): a binding is filled by `with`, declaration, slot or default, never
  joined.
- **A second, post-monomorphization effect verifier.** Retired 2026-09-10 by measurement (D7): what a
  monomorphic body can still say about effects is strictly less than what the declaration walk already said, because
  an operation call is by then a call to an implementation method, which declares no row. Verification belongs where
  the declarations are — the write's own walk, before monomorphization. A post-mono re-derivation buys no case and
  splits one diagnostic across two files. (This is not about the *precondition* that stayed at that seam: a supplied
  row entry's arguments genuinely need ground types.)
- **The carrier as the injection point** (§6's strategy as the *final* form). It chose an interpretation by
  instantiating a type, and every one of v5's testing limitations was a symptom. Superseded by §6.
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
  `return`. What is closed is each of those: the lowering pass (§3.4's primitives make it unnecessary), the
  in-type binder and everything unification of it forced, and the four keywords. What is **not** closed —
  reopened and decided 2026-09-08 — is a *phantom* binder per row entry as the mono-key component (§3.1): it
  occurs in no parameter or return type of any value, so nothing unifies it, and it is the elaborator's existing
  prefix write with a name in place of a carrier. Part II §9 gives that binder a declared type and lets it stand
  as a *dead argument* of a row alias, which reduces away before any type is compared; neither is the in-type
  binder this entry closes.
- **Transitive `with` / summing abilities over a monomorphized sub-graph.** Dynamic scoping resolved at
  compile time; the carrier's third job in a new costume. Forwarding is by declaration only (§3.1).
- **A bindings field on `ValueReference`, a scoping node, or a consulted-set component on the mono key** as
  the carrier of a binding. Decided 2026-09-08 against, for the phantom binder (§3.1): the key already
  carries one argument per binder, and transitivity is a ground-value tree.
- **A runtime handler stack / dynamic scoping at runtime** as the language's mechanism. Kills erasure and
  makes storage semantics unwritable in a type. The one dynamic discipline that exists is the machine stack
  inside the escape and cell leaves (§3.4), private to the platform.
- **Full unification — every ability passed, none searched inside a function.** Declared rows for every
  ground ability use: no new capability over a `~` constraint, and the declaration burden puts
  `{Eq[Int], Combine[String], PatternMatch[Shape]}` on most monomorphic code, transitively. Rows inferred for
  abilities: that is inference, and it makes `printAll(xs) with reverseOrd` reach an undeclared resolution.
  Under either reading the two-site search stays, because the compile track dispatches
  `Meta`/`Numeric`/`PatternMatch`/`TypeMatch` from machinery with no call chain.
- **A public cell or escape in the base.** A public cell is Landin's knot; both primitives are
  platform-private, and the dischargers are abstract in the base for exactly that reason (§3.4).
- **A runtime free monad as the effect representation.** A heap tree of closures walked by an interpreter,
  a coproduct-with-injection to compose effects (the row back in the type), and no plain spelling of the
  scoped operations.
- **A `World` token / threading a fake dependency to sequence and protect I/O.** Not needed in a strict
  core (§3.5); purity is decided by evaluator stuckness.
- **A rule on twin-less natives** — the reachability check ("a native without a twin may be called only
  from an `implement`") and, with it, **a purity annotation for twin-less pure natives**. Withdrawn
  2026-09-08: a twin proves reduction, not purity, and the jvm layer's `Path` algebra is pure,
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
- **A row alias with its own AST node.** An `ast.fact.Expression` case is the most expensive thing this
  language can add. The row alias that exists (§2.4) rides the `{E} A` node the parser already has; a payload-less
  `type Web = {Console, Log}` is not a type and stays unspelled.
- **Discharge markers (`{-E}`).** There is no negative-effect surface; discharge is a frame a discharger
  installs.
- **Scanning the dictionary for an ability or implementation name.** Replaced by the keyed marker lookup,
  and the same closure covers `with`: an implementation name resolves at `resolve` to a `ValueFQN` by keyed
  lookup, and only that FQN flows onward — never a string carried downstream for `AbilityResolver` to search
  by, which would be a second lookup path with the same three failures the scan had: it ignores import
  scope, cannot be shadowed, and is decided by hash order.
- **The `<Ability>Carrier` convention as an explicit declaration.** Its premise — that an effect *has* a
  representation — is gone; the property it named ("has a canonical monad transformer") is exactly what
  §3.4's two primitives replace.

**Not now — conveniences deliberately left out of the flag day (2026-09-08).** Each is additive later, none
touches the mechanism, and the plan is kept as simple as possible until the flag day has landed:

- **A name for several implementations** (`implement mocks = mockConsole & mockFileSystem`), and **a set of
  abilities required of a binder** (v5's `ability Web[F[_] ~ Console & Log]`, whose carrier-binder spelling went
  with the binder; `EffectAbilitySet.els` was deleted at the flag day). A row *with its payload* does have a name
  since 2026-09-11 — the row alias, §2.4 — and the superability closure stays for ordinary binders.
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

## 13. Retired documents, and how to read a citation

Ten documents were merged into this one and deleted. Their full text is in git history (`git log --diff-filter=D
-- docs/`), and the table below says what each was and where its subject now lives. **Scaladoc comments across
the compiler cite them by their own section anchors** — those are *historical pointers* ("this code came out of
X §N"), not live references, and they do not index this document.

| retired document | what it was | anchor scheme in comments | where its live content is |
| --- | --- | --- | --- |
| `effects-as-channel.md` (v2, "uniform carriers") | the first shipped design: the carrier as a type argument the *checker solves* | `§0`–`§13`, `finding N`, `U1`/`U4-x` | superseded in direction and in code. What it got right and still holds: the **channel** (§3.3) and rows never flowing into types. Its carrier-specific results — carrier-ness by tag, `Id` without `Suspend[Id]`, the payload/row rendering inverter — went with the carrier. What it got wrong is §12's first entry |
| `effects-as-rows.md` (v3) | the previous landed design + its A.1–A.11 record: the elaborator writes the carrier | `§1`–`§9`, `A.x`, `R1`–`R6` | its **user model** is Part I §1–§2 and its **standing rules** are §5, both largely intact; its mechanism (the elaborator, the derivation spec, the whitelist) is superseded by §3. §10's gate method is its A.9.4 |
| `effect-row-tails.md` | pinned rows as the one spelling of a carrier stack | prose only | no live content: a stored computation is a thunk (§2.3) and D4 dissolved |
| `testing-effects.md` | substituting effect implementations | `L1`–`L3`, `§2.x` | §6 — the question is the same and the answer changed: a name, not a carrier |
| `effects-v5-one-carrier.md` | rows as constraints on one carrier — the subtraction from v3 | `§4 step N`, `§5 Q1`–`Q4`, `§7` | §2.1 (step 1) and §2.2 (step 2) both survive v6 unchanged in meaning; its §7 (an ability requiring abilities) has no v6 spelling (§2.4); step 4 and Q1 are §12 |
| `effects-as-channel-v4.md` | the row leaves the type, the carrier leaves the language | `R1`–`R11`, `P0`–`P5`, `Q1`–`Q4`, `§0`–`§11` | superseded by Part I (v6); its seam finding — specialisation is the mechanism, not a follow-up — is §3.1 |
| `effects-v4-p0-spike.md` | does the `WovenValue` seam know the carrier? | `S1`–`S3` | the seam test (`EffectsSeamGroundnessTest`); the test is permanent |
| `effects-v4-p2-sizing.md` | sizing the flag day | `§1`–`§5` | the flag-day record, history (below) |
| `effects-v4-flag-day-readiness.md` | is the flag day ready? (no) | `B1`–`B3` | §12 |
| `effects-syntax-userspace.md` | `~` and `&` as ordinary values | `stage 1`–`stage 4`, `§7.x` | §2.5 (stages 1–2, landed), **D3** (stages 3–4) |

**Citations to Part II's former numbering** (in commits and comments dated before 2026-09-07): *D1*/*B1*–*B4*
were the v4 decision and its blockers (now Part I and §12); *D2* the `<Ability>Carrier` declaration (§12's last
entry); *W1*–*W4* the v5 work items (A3, history); *D8*/*D9*/*D10* the surface spelling, the cell and the purity
annotation (decided: §2, §3.4, §12); *D14*/*D15* (2026-09-07 only) were the slot `with` and the binding's
carrier (decided: §2, §3.1); *D6*, *D11*, *D12* and *D16* (grades, the ground-ability row spelling, the
user boundary default, bundles and effect sets) are §12's "not now" group. Two older citations in the tree — `docs/effect-lift-in-checker.md` and
`docs/effectful-signatures.md` — point at documents retired before these and are likewise historical.

**Citations to Part II's retired sections.** Until 2026-09-11 Part II was the v6 plan and its record, and
Scala comments dated 2026-09-07 to 2026-09-10 cite its anchors: **§8** (how the plan was run), **§9** (the
model — §9.1 in five sentences, §9.2 what was decided and why, §9.3 the surface, §9.4 the core desugar, §9.5
semantics, §9.6 the three primitives, §9.7 specialisation and purity, §9.8 what it deleted/kept/added, §9.9 the
standing rules re-read) and **§10** (§10.1 the nine pre-flag-day steps, §10.2 the flag day **F1–F9**, §10.3 the
follow-ups **A2–A11**). Those are historical pointers into the document as it stood at `cb2a9c6d`
(`git show cb2a9c6d:docs/effects.md`), not live references. Where the live content is: §9.2's decisions are
Part I §2, §3.1 and §3.5; §9.3 is §2; §9.4 is §3.1 and §4; §9.5 is §3.5 and §4; §9.6 is §3.4; §9.7 is §3.5 and
§12; §9.8 is history; §10.1's steps and §10.2's F-items name their commits and live there; and of §10.3's
A-items, A5 is §3.3, A6 is §2.2, A7 and A11 are §2.3, A8 and A9 are §7, A3 and A4 are absorbed by §7 and §11,
and A2 (a microcontroller target's exit primitive) is still hypothetical and is §3.4's table. Part II's
*current* §8–§10 are new sections and share nothing but their numbers with those.
