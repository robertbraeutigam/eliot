# Effects in Eliot — the design, and what is left to do

**Status (2026-09-09): effects v6 is shipped and this is its single document.** An effect is an ability
declared with the `effect` keyword; an **implementation is a name**, bound by `with` and forwarded lexically
from `main` inward through declarations. There is no carrier, no monad, no `Id`, and nothing to infer. Part I
describes the tree as it is; Part II is how it was decided and landed, kept for its reasoning, its
measurements and its open decisions; Part III is enough provenance to read a source comment that cites a
retired document.

**One-sentence summary.** The user writes **effect rows**; each row entry desugars to one **phantom generic
binder** whose value is an implementation, written at every reference by a syntax-directed pass that reads
declarations only — so an operation call is an ordinary call to a known method, effects verify as a
**channel** beside the type (the same architectural move as the `Int` refinement channel), and the NbE
checker holds no effect rule at all.

**How to read this document.** Part I states the shipped design; it is the authority for the tree, and the
CLAUDE.md *Effects Are a Channel* cornerstone is its summary. Part II records the v6 decision (§9), how it
landed (§10), the decisions still open (§11, numbered **D**) and the list of things that are **closed by
measurement or decision and must not be re-proposed** (§12). Part III is provenance.
Where Part I and any code, stdlib signature, example or test disagree, Part I wins and the artefact is the
defect.

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

### 2.4 Naming a set of effects

**There is no spelling for one, deliberately** (§12, "not now"). v5's `ability Web[F[_] ~ Console & Log]`
worked by requiring abilities *of the carrier*, and with no carrier binder there is nothing to hang the
requirement on. A slot doubling many effects writes the full chain; a project wrapping a word of its own
restates the row and the chain.

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

- **A `type` spelling of a row.** `type Web = {Console, Log}` was proposed and rejected: `type X = …` names
  a *type*, and a set of abilities is not one — which is why that spelling needed a body no type expression
  could hold — and paying for it with a new `ast.fact.Expression` case is not how this language grows.
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

v5 needed a written *whitelist* here (§9.9's retired rule 5), because its elaborator had to classify each
slot — carrier-headed or payload, pinned or open — and every classification is a place an approximation can
accrete. **There is no classification left to approximate**: a slot either declares a row or it does not, and
that is read off the declaration. A rule that inspects a *sibling argument's expression shape* is inference,
not desugaring, and is still prohibited; a decision that cannot be made from a declaration is a gap to close
**in the declarations**.

The fail-safe direction is built in: a missing write is an aborted definition with a violation at its own
position, never a binding silently taken from the platform's default.

### 3.3 Two verifiers, one vocabulary

Checking a runtime term yields a **payload type** (the existing NbE judgment, which never sees an effect)
and a **row** (a second output, exactly as an `Int`'s range lives in the refinement channel beside the type,
not inside it). Row constraints are set-shaped: union for sequencing, inclusion for boundaries
(`derived ⊆ declared`) — commutative and order-independent, so no argument-order sensitivity can exist.

- **Pre-mono**, per definition: the **scope check**, which is not a separate pass but the write's own walk
  (§3.1). An operation or a rowed callee needs a covering declaration, and the only places one can come from
  are the enclosing def's row or constraints, an enclosing `with`, or a slot's row. It is complete before
  monomorphization, since nothing about it is instantiation-dependent, and it owns the diagnostic for a `with`
  whose subject contains no covered use.
- **Post-mono**, at ground instantiations: `monomorphize/channel/EffectAccountingProcessor`, wired as a
  **codegen precondition** via `getFactOrAbort`. Under names "performs X" is "a reference that forwards a
  **received** binding to a callee declaring X as a row entry", read off the value's own type arguments, and
  the check is that set ⊆ the declared row.

Both emit the same message: *"This value performs the effect 'X' but does not declare it…"*.
*Forward what is declared, derive what is done* — a forwarded per-operation verdict would be a checker
self-report and is rejected, as is any negative-effect surface.

**The post-mono check's coverage is a strict subset of the scope check's, and that is measured, not assumed**
(D7, `jvm/…/EffectAccountingDerivationTest` pins it). It sees propagation through a *declaring callee* —
`main` calling `{Inf, Console} loopForever` forwards both — and does **not** see a direct operation call, since
`AbilityResolver` has by then rewritten `printLine` into the implementation method, which declares no row.
That is not a hole: the scope check reports an uncovered effect at the reference for an operation and a
declaring callee alike. The processor nonetheless has a **second job that is nobody's shadow** — rejecting a
supplied row entry whose argument nothing determines (§2.2) — so retiring the subset check would not retire
the processor.

### 3.4 The three primitives

A resuming clause is a call and needs nothing. A finishing clause is a **non-local exit**; a stateful
implementation needs a value **threaded through calls that never mention it**. Neither is expressible as a def
in a strict pure core — under v5 both were expressed by the transformer instances, which is the whole reason
the carrier existed. They are now three **platform-private leaves**, one per target, and nothing else:

| primitive | shape | jvm | microcontroller | compile track |
| --- | --- | --- | --- | --- |
| escape | `escapeInternal[K, A, R](body: {} A, onExit: K => {} R, onValue: A => {} R): R` — abortive, never re-entered; the frame is the machine stack | an exception, one class per instantiation | a status flag and a jump (§10.3 A2) | an evaluator intrinsic |
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

What remains of carrier identification is **one structural predicate that is not about effects**:
`EffectCarriers.isHktBinder`, "is this binder higher-kinded?", which `check/CarrierKindChecker` asks in order to
reject a `[F[_]]` binder instantiated at a fully-applied proper type. That is a kind system living next door,
and it is soundness (§11, "do not re-propose").

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
(`Track.Compiler`, `Checker.resolveDeferredSlot`), where an inline guard's carrier is still inferred and pinned
post hoc — the sole live reader of the `Unifier`'s higher-kinded-meta record.

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
   (§10.2 F5), so a `raise("…")` in a signature reduces without its text. Accepted rather than fixed; the route
   back is keying `Throw`'s compile-time frame on a fixed marker the way `Abort` keys on `Aborted`, at the cost
   of two instantiations sharing one frame.
5. **A set of effects has no name** (§2.4), so a slot doubling many of them writes the full chain, and a
   project wrapping such a word restates the row. §12, "not now".
6. **A stored computation's binding is fixed where it is constructed.** Deciding the handler before storing is
   unambiguous and easier to understand; losing first-classness is the accepted price. A `with` applied to a
   stored computation later is an error, never a rebinding.

---

# Part II — How v6 was decided and landed

**Status (2026-09-09): landed. Part I now describes what this plan built**, so this part is kept for three
things a description does not hold: the **reasoning** behind each decision (§9), the **record of how it
landed** with what each step cost and found (§10), and the two lists that bind future work — the decisions
still open (§11) and what is **closed by measurement or decision and must not be re-proposed** (§12). Where
this part and Part I disagree about the tree, Part I wins (standing rule 1); where a decision is quoted here
and not restated there, this is still where the reasoning lives.

An `implement` block stayed what it always was, statically resolved method bodies. No ability became a record
type, no implementation is ever a runtime value, and no row became a runtime parameter. A **named**
`implement` mints an addressable *name*; `with` binds that name for the calls lexically inside its subject;
and the binding joins the **monomorphization key**, so `greeting` under `recordingConsole` is its own
instantiation and every operation call erases. Every entry marked **decision** is Robert's; what §11 lists is
what is still his to decide.

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
its standard output and exit code — that recording was `.v6/baseline.txt`, made by the same script (§10.1
step 9), and it is deleted with the staging now that it has done its job; its totals are in F8. After the change,
the same sweep must produce the same transcript. Plus: `./mill __.test` green; the
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

**Deleted** (all of it, 2026-09-09 — with two corrections to this list, marked below): `RowElaborator` and
`RowElaborationProcessor`'s elaboration half; the carrier-minting,
pinning and supplying halves of `EffectSugarDesugarer`; `EffectLifter`; `IdNormalizer` and
`assertNoIdResidue`; `EffectCarrierNaming` and `EffectRowRendering`; `RunBoundaryFunctions`; the
constraint-aware declination and `activeFactKeys` probe in `AbilityImplementationProcessor`;
`RowChecker.fixesCarrier`, the derivation rules and the block peel; every `*Carrier` type, `Suspend` instance and cross-lift instance in
stdlib and jvm; the `eliot.carrier` package; the compile-track `Id.els` and `AbortCarrier`; the "a
discharger must be called directly" diagnostic.

Two entries were wrong. **`RunBoundaryFunctions` is kept** — F4 repurposed it to "the values where every effect's
chain ends", which is where the write binds `Default` instead of reporting the row undeclared. And
**`RowElaborationProcessor` is not renamed**: it kept its name and gained a job, since the write covers signatures
too.

**Kept:** the scope check — which is the write's own walk, not a `RowChecker` method any more — and
`EffectAccountingProcessor` (through the flag day, D7); `AbilityResolver` and `AbilityImplementationProcessor`
(structural match + `where`) for the two-site default; `EffectRow` as declaration metadata; `CarrierKindChecker` as
the kind system it is; `WovenRecheck`; the seam-groundness test, re-pointed at bindings (`EffectsSeamGroundnessTest`:
no mono-key argument at the seam is still a parameter, and one definition at two bindings is two instances).

**Added:** the `effect` keyword, the named `implement`, and `with` in both positions, with their desugar;
the impl-name component of `Qualifier.AbilityImplementation`; the phantom binder per row entry and constraint,
the `Default` marker and the implementation-valued ground argument; the read-the-argument arm of
`AbilityResolver`; the three primitives per platform and the two evaluator intrinsics; the boundary rule in
`SyntheticMainSourceProcessor`. **Not added:** a bindings field on
`ValueReference`, a scoping node, a new `MonomorphicValue.Key` component, a consulted-set fixpoint.

Two things were added that this list did not anticipate, both because a thunked slot carries less than a carrier-typed
one did: the **signature half of the write** (F2, without which no guarded return type compiles) and the **rejection
of a supplied row entry whose argument nothing determines** (A6, without which a `catch` can compile and crash).

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

It was planned as one change. It landed as many, because the tree stops building the moment the desugar stops
minting carriers and there is no green checkpoint until the primitives exist — so the honest thing was to commit the
work in reviewable pieces, each stating what it left broken, rather than hold weeks of it uncommitted. The gate never
moved: §8's behavioural identity, re-read on every commit.

**Where it stands (2026-09-09).** **F1–F7 are landed.** A full `scripts/example-sweep.sh` is behaviourally identical
to `.v6/baseline.txt` on every example the two trees share — same exit code, same stdout, all 44 jars — with only the
three module-level differences F7 made on purpose (`EffectAbilitySet` deleted with the effect-set feature,
`EffectsFakeCarrier` replaced by `EffectsNamedEffect`). That identity held on every intermediate commit, not only at
the end, and the Scala suites are green — 1,680 tests, from 524 failing at the start of the flag day. The **size
difference is now stated** (F8 below): a third smaller, whole-tree. The `eliot-test` move and F9's documents are what
is left of the plan.

**What the gate could not see, and what that cost.** §8 reads the examples, and no example writes a guarded return
type, a `foreach`, a `catch` with an ignoring handler, or a stored computation. Four real defects hid in exactly that
gap, three of them found only by clearing the test suites:

1. **The write never walked signatures** — every guarded return type failed with a type mismatch. Fixed: the write
   covers both halves of a definition (see F2/F3 below).
2. **The compile-track `escape`/`withCell` declared `body: Function[Unit, A]` rather than `{} A`**, so a caller
   passing a suspended parameter through had it *applied* — running the computation outside the frame and handing the
   intrinsic its result to apply again. Fixed by spelling both slots the way the jvm leaves already do.
3. **`stdlib`'s `foreach` did not compile**: its body was `foldLeft(pure(unit), …)`, and `pure` went with the carrier.
4. **A `catch` whose handler ignores its error compiled and crashed** — guarded now, and open as A6.

The lesson is not that the gate is wrong; it is that the gate is a *behavioural* identity over the corpus the examples
cover, and the corpus does not cover the type-level surface. A shape with no example has no gate, and every one of the
four lived there.

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
- **F2 — deletions. DONE** (`72ba7558`, `4bb25c00`, `cfcb92b3`, 2026-09-09). Everything in §9.8's deleted list: `RowElaborator` and `RowChecker`'s
  derivation half (which keeps `Universe`, `checkable` and `peelBinders`), `IdNormalizer` with `assertNoIdResidue` —
  so the `WovenValue` seam now *rewrites nothing* and is only where the three codegen preconditions are checked —
  `EffectRowRendering`/`EffectCarrierNaming` with the renderer and printer arms that inverted a carrier stack back
  into a pinned row, `AbilityResolver.sideEffectOnPureCarrier`, and the `Id`/`Effect`-combinator well-known types.
  `RowElaborationProcessor` is not renamed: it kept its name and gained a job.

  **The write covers signatures, not only bodies** (`1dfd24db`). A guarded return is compile-time code in *type*
  position, and its `if`/`else`/`fold` are ordinary calls with row-typed slots and phantom binders, so the same
  thunking and the same binding write are needed there or the guard reaches the checker as a bare `Type` at a
  `Unit -> Type` slot. Two rules make that safe: the signature is walked with **no thunk parameters in scope**, so a
  dependent reference to a value parameter is never mistaken for a thunk; and an uncovered effect in a signature
  **defaults instead of being reported**, because a signature's `raise`/`abort` is the guard channel's vocabulary,
  discharged by the guarded-return read rather than performed. The flag moved from the `Writer` to the `Scope`, where
  it names both regions that have it — a platform run boundary and a signature. The body gate moved with it:
  `RowElaborationProcessor` no longer decides whether to write at all (a `@Signature` twin and a body-less declaration
  both need their signature written) and `BindingWriter` decides per half.
- **F3 — the checker. DONE** (`72ba7558`, `4bb25c00`, 2026-09-09). `EffectLifter` and the ladder's two pure-wrap arms are gone (no rigid
  carrier to lift into), and with them the **deferred slot** — `SlotOutcome`/`SlotRecord`/`rebuildChain` and the whole
  two-phase spine — `Track.Compiler.pinCarriers`, and `TypeStackLoop`'s C2 carrier fence. `typeImmediateLambda` is an
  ordinary `let`. Rendering: an effect prints as the name the user wrote — `GroundValueRenderer`'s two entry points
  collapse to one, since they existed only because a carrier's last argument means one thing applied to a payload and
  another not. *Not* done as stated: an unsolved phantom is still not an error of its own; what catches the one shape
  that mattered is A6's rejection at the call.
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
- **F6 — accounting. DONE** (`cfcb92b3`, 2026-09-09). `EffectAccountingProcessor` reads **received bindings**: an instantiation
  receives one implementation per phantom binder of its own signature (its mono key's arguments at those indices), and
  a reference *forwards* one when the binding written at its own phantom slot is that same implementation and the
  callee declares that ability as a row entry. `MonomorphicValue.ambientCarriers` went with the ride test — it was a
  copy-only projection of the mono key — and so did `CheckState.ambientCarriers`. Every non-empty derivation is
  **logged**, which is D7's trace. The "declared pure but performs effects" diagnostic stays in the pre-mono scope
  check, and the processor gained A6's rejection.
- **F7 — the tree. DONE** (`1b6482aa`, with F5's five discharger bodies). The `.v6/` overlay applied over stdlib,
  jvm, lang and examples, with the manifest's deletions. Two staging gaps found: the compile-track `Either` still
  carried `implement Effect[Either[String]]` and `implement Throw[String, Either[String]]`, which cannot survive a
  tree with no `eliot.carrier`; and the `Default` sentinel **does** need a declaration after all
  (`stdlib/eliot/eliot/lang/Implementation.els`), because a written binding is an ordinary type argument and
  saturation demands the value it names. **`eliot-test` moved 2026-09-09**, which is what finished F7 and let
  `.v6/` and its `V6TreeParseTest` gate be deleted.
- **F8 — the gate** (§8): behavioural identity on every example, tests green, the fake examples and
  integration classes with no minted carrier, the single-word `eliot-test` case, seam resolution, the size
  and instruction-count difference stated in the commit. **Behavioural identity is met and the tests are green**
  (see "where it stands"). The **fake carrier is gone from the suites too** — `ExamplesIntegrationTest1`'s testing
  group is a named implementation (`implement session: Terminal { … }` + `greet with session`, with the harness taking
  the program at a `with`-bound slot type), and `AbilityConstraintDeclinationTest`, whose whole subject was carrier
  substitution, is `FakeImplementationIntegrationTest`: the same claims, made the v6 way. **The gate is met**: the
  `eliot-test` move landed on 2026-09-09 and its 96 cases pass, the single-word case included.

  **The size difference, stated (2026-09-09).** Swept at `0a2f7c01` with `scripts/example-sweep.sh` against
  `.v6/baseline.txt`, like-for-like over the **44** modules the two trees share — `EffectAbilitySet` is deleted on
  purpose, and `EffectsFakeCarrier` is matched to its rename `EffectsNamedEffect`:

  | | baseline | v6 | |
  |---|---:|---:|---:|
  | jar bytes | 918,686 | 600,432 | **−34.6%** |
  | classes | 1,683 | 1,114 | **−33.8%** |
  | instructions | 25,924 | 17,174 | **−33.8%** |
  | the programs' own classes | 2,804 | 2,560 | −8.7% |
  | synthesized entry stubs | 352 (8 each) | 308 (7 each) | −12.5% |

  A third of the emitted program is gone, and it is the carrier: no `IO`, no `Id`, no `*Carrier` type, no `Effect`
  instance and no `map`/`flatMap`/`pure` chain to thread one. The floor is flat — **21 of the 44 modules lose exactly
  109 or 110 instructions** whatever else they do, which is what every program used to carry whether or not it
  performed an effect, and **no module is unchanged**. `HelloWorld` is 132 → 23.

  **Two modules grew, and both are explained.** `IfDemo` +222 (1,711 → 1,933, +19 classes) is §9.7's specialisation
  working as designed, charged to the one program written to exercise every form of `if..else`: it discharges `Abort`
  at three payload types (`Unit`, `String`, `Option[String]`), and `else` and `runAbort` are emitted **per
  instantiation**, so 18 of its 19 extra classes are exactly those two dischargers × three types × three lambdas. Its
  own class grows +167 for the thunk lambdas its suspended slots bind, where v5 built an `AbortCarrier` value instead.
  That is the trade the −34.6% is made of, and a program discharging at one type pays none of it — `EffectsAbort` is
  792 → 416. `Intervals` +13 (+0.9%, +1 class) has no effect machinery at all and its own class *shrank* by one
  instruction; the difference is the F7 tree change where a combinator's return row disappears
  (`foldBound[T, B](…): B`, not `{} B`), not a cost of the mechanism.

  So there is no regression to accept: the aggregate is a third smaller, and the one real grower is the priced
  mechanism rather than a surprise.

  **What the `eliot-test` move found (2026-09-09).** The staged framework was written before A6, so it met the write's
  determination rule in three places the staging had not anticipated, and each is the rule reading correctly rather
  than a gap:

  - **A parameter reference declares nothing to the write.** `describedAs`'s `runThrow(body)` and `raising`'s
    `runThrow(body)` take a *parameter* whose row is `{Throw[AssertionError]}` / `{Throw[E]}`. A6 reads a supplied
    entry's arguments off the **actual callee's** declared row, and a parameter has no callee, so the prefix stops and
    the accounting rejection fires. Both are written out (`runThrow[AssertionError, Unit]`, `runThrow[E, Unit]`) — the
    escape hatch the diagnostic names. Reading a parameter's *declared* row here would be in the whitelist and would
    settle these two, but it is not always enough and would need deciding on its own: `mocked`'s body declares **two**
    `Throw` entries (`Throw[IoError]` and `Throw[AssertionError]`), so nothing but the call can say which one its
    `catch` discharges, and it is written `catch[IoError, Unit](body, …)`.
  - **A body that raises nothing has no error type to read.** `raising("…", raising("expected", printLine("no raise
    here")))` checks that a raise did *not* happen, so the inner body's declared row has no `Throw` entry at all and
    `E` is genuinely unnameable from any declaration. Spelled `raising[AssertionError]`.
  - **An operation's row is now what it *adds*.** `effect Process`'s `run` declares `{Throw[IoError]}`, not
    `{Process}` — performing `Process` is what being its operation means — so a helper calling it declares
    `{Process, Throw[IoError]}`. Three helpers in the process suite had to say so.

  It also turned up a **backend defect of its own**, unrelated to effects and fixed with it (`32406522`): an
  ability-implementation native handed on as a function (`listDirectory(…).map(show)`) linked to a partial-arity
  method that was never emitted. `NativePartialApplication` had been wired into the module-level branch only; the
  impl branch now runs the same generator. All 44 example jars stay byte-identical, since no example under-applies
  one.

  **Two traps worth carrying forward.** `ProcessorTest` needs an `Implementation` stub (`type Default`) or every
  snippet calling an ability method loses its monomorphization **silently, with no error at all** — the write puts
  `Default` at the reference as an ordinary type argument and saturation demands the value it names. And never run
  `./mill` while `scripts/example-sweep.sh` is running: it rebuilds `out/` underneath the sweep, and the report then
  reads as a large regression that is not there.
- **F9 — the documents. IN PROGRESS.** **Part I is rewritten** (2026-09-09) and is again the authority for the
  tree; Part II is reframed as the record of how v6 was decided and landed rather than a plan, and A3's
  reconsidered limitations landed with it. What is left: the CLAUDE.md *Effects Are a Channel* cornerstone, the
  `eliot-code`, `eliot-layers` and `eliot-jvm-backend` skills' effect sections, the `TODO.md` pointer, and
  `eliot-test`'s own `.claude/CLAUDE.md` and `docs/mocking.md`, which still describe the mock carrier, fake
  carriers and the deleted `pure`.

If the gate cannot be met, the assessment in §9.2 is wrong somewhere — find where before landing anything,
and do not land a narrowed version (standing rule 2).

### 10.3 After the flag day

- **A2 — backend exit primitive**, if a microcontroller target replaces the jvm exception with a status flag
  and a jump; the primitive's shape (§9.6) does not change.
- **A3 — the reconsidered Part I limitations. DONE 2026-09-09**, as part of F9: Part I §7 was rewritten and
  now lists the live set. Of v5's nine, seven had no subject (all four carrier-shaped ones, the two about
  pinning and fake regions, and the `data`-field binder), and the eighth — rule-4 violations diagnosed twice
  — turned out to have no *violation*: an effectful call at a rowless slot simply runs where it is written
  (rule 1), so there is nothing left to diagnose twice or once. What replaced them is smaller and different in
  kind: a row cannot be closed, a discharger's arguments are sometimes written by hand, a set of effects has no
  name, a stored computation's binding is fixed at construction — plus the two backend/compile-track items A9
  and A8.
- **A4 — D4 dissolves. DONE** (any effect is storable and suppliable); the v5 limitation D5 answered dissolved
  with it (§9.4).
- **A5 — retire the post-mono accounting verifier** under the §8 method (D7).
- **A6 — the write fills a supplied row entry's own arguments. DONE 2026-09-09.** A parameter row lowers to a thunk,
  which erases the entry's arguments from the type, so `catch[E, A](computation: {Throw[E]} A, onError: E => {} A)`
  leaves `E` to the handler — and a handler that ignores its error determines nothing. It was written at the call
  (`catch[String, String](…)`) and a defaulted one was **rejected** by `EffectAccountingProcessor` rather than allowed
  to miscompile. The fix is the rule §3.1 already states for a supplying slot, and it needed no new mechanism:
  `BindingWriter.suppliedDetermination` matches the slot's declared row entry-by-entry against the **actual's own
  declared row** (`bad : {Throw[String]} String` against `Throw[E]` gives `E := String`) and writes the result as a
  leading positional prefix after the phantom binders, stopping at the first binder nothing determines. It reads only
  declarations, so it is inside §3.2's whitelist; it is skipped entirely when the call already spells its own
  arguments, so the explicit form stays the escape hatch; and the v5 free-binder guard came with it — an entry whose
  argument is still one of the *actual callee's* binders (`state`'s own `S`) is a rename, not a determination.

  The accounting rejection **stays**, now as the net for what no declaration answers: an *over-discharge* — a second
  `catch` over a computation the first already discharged — leaves an actual declaring no row at all, and is rejected
  at the call rather than crashing on a frame-key mismatch. `CatchShapeMatrixTest`'s Group B is back to the spelling a
  user writes.

- **A7 — a `data` field stores a computation. DONE 2026-09-09.** §9.5 says a row-typed field is a thunk bound at
  construction, and it was — in the *type* only. Both halves landed together, as the earlier measurement said they had
  to:
  - **The split comes first.** `EffectSugarDesugarer.desugar(DataDefinition)` thunked the field before
    `DataDefinitionDesugarer` split the `data`, so the value constructor's slot was never recorded as a row: the actual
    was neither thunked nor supplied and its effect was charged to whoever *built* the value (`Box(failing)` ⤳
    "performs the effect 'Throw' but does not declare it"). The `data` is now split first and the constructor reaches
    `desugar(FunctionDefinition)` with an ordinary parameter row, which thunks and records it like any other. That
    method is gone; what is left of it is `storedFieldType`, applied at the two places where a field's type is used
    **as a type** — the accessor's return, and the Church-encoded handler `handleCases` takes.
  - **A read runs it.** The accessor's return is the field *as stored*, which is deliberately not a return row: a
    return row would mint a phantom binder and claim the accessor performs those effects. It is recorded as
    `EffectRow.returnThunkEffects` instead, and the write applies a **saturated** call to such a callee — the exact
    mirror of a row-typed parameter reference, which is what makes wrap and apply cancel for a field read back at a
    rowed slot: `runThrow(step(t))` comes out as the η-expansion, not the double wrap the type rejects. Running it
    performs what the field's row declares, so the entries are **charged** at the read (there is no binding to write —
    it was written at construction), and an undeclared read is the ordinary "performs but does not declare" error at
    the read.

  One thing a signature cannot say came up on the way: its arrow chain runs straight through a returned function, so
  an accessor handing back a thunk reads as taking two parameters. Saturation is read off the body's own leading
  lambdas past the generic binders instead.

  A6 and A7 were **one family**: a row-typed slot lowers to a thunk, and the thunk carried neither the entry's own
  arguments nor the fact that it *is* a slot. Both were the write's to restore from declarations it already reads, and
  neither needed a mechanism the whitelist does not already allow. Covered by
  `jvm/…/StoredComputationIntegrationTest`; all 45 example jars stay byte-identical, since a `data` that stores no
  computation lowers exactly as before.

- **A10 — a compile-time escape frame must not be consumed before its arguments arrive. FIXED 2026-09-09.** The
  compile track reduces a definition's body once, at its own instantiation, and only later substitutes a concrete
  argument into the neutral that comes out. `EffectIntrinsics.escape` installed its frame, settled the body to a
  neutral (the condition inside was waiting on the definition's own parameter) and answered `Right(neutral)` — which
  *consumes* the frame: it is gone from the reduced body, and when that neutral is finally re-reduced at a concrete
  argument, an `exit` inside it either sticks or lands in whatever frame happens to be installed then. An escape whose
  body settles to a neutral now stays **stuck** instead, so the whole thing re-fires once the argument is there, with
  its frame installed around the part that needs it. `settle` gained the enclosing evaluation's native lookup with it
  (`Evaluator.currentNativeLookup`), because re-firing a stuck native is exactly what that re-reduction does — the
  original "a stuck native is legitimately stuck, re-firing is the checker's business" is not true *inside* a frame,
  where there is no later pass: the frame is gone by then.

  It was invisible because it fails in the safe direction: the guard reduced to a stuck value that read back as a
  *false* verdict, so the program compiled and selected the other implementation. What catches it is
  `EffectIntrinsicsIntegrationTest`'s two-frame case, an exit passing through a frame of another instantiation to its
  own.

- **A9 — an under-applied backend intrinsic has nowhere to link.** An intrinsic is emitted **inline** at each call
  site, so only a *saturated* call can be emitted at all: the emission indexes its operands directly, and it has no
  static method for a partial-application closure chain to end at (`NativePartialApplication`'s levels call one). So
  `digits.map(show)` — handing `show` on as a function — took the compiler down with a `NoSuchElementException` off an
  empty argument list. It is a hard error at the definition now, naming the fix (`digits.map(n -> show(n))`, which is
  saturated and costs nothing). The same shape for an ordinary *native* is supported, so this is a gap in the backend
  rather than a rule of the language; closing it means reaching the inline emission from a closure frame, whose
  operands are already on the stack, or emitting the intrinsic's full-arity method beside its inline uses.

- **A8 — a guarded return cannot carry its author's message**, because the compile-track `Throw` went with the
  carrier (the reversal recorded at F5). Accepted 2026-09-09 rather than fixed; the route back is keying `Throw`'s
  compile-time frame on a fixed marker the way `Abort` keys on `Aborted`, at the cost of two instantiations sharing
  one frame.

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

**Dissolved at the flag day.** There is no canonical carrier for an effect to lack; a stored
`{Console} Unit` is a thunk bound where it was written. Kept as a number only because §12 and Part III cite
it; the Part I limitation it answered is gone with the rewrite (A3).

### D5 — a lambda body at a rowless arrow slot

**Closed 2026-09-08.** The walk crosses a lambda boundary iff the lambda's slot has a row (§9.4). A rowless
lambda is a barrier to received bindings only; it may bind and discharge locally, and must discharge everything
it performs. It is now stated in Part I as the third bullet of rule 4, rather than as a limitation.

### D7 — can the post-mono accounting verifier retire?

Under names the post-mono check is "received bindings consulted ⊆ declared row", a subset of the mono key
that the pre-mono scope check already establishes lexically. The expectation is **yes, after the flag day**
(A5), by the §8 method: keep it through the flag day as the codegen precondition, trace it, retire it when
it fires on nothing. Not before, and not on the argument alone.

**The trace is armed** (F6, 2026-09-09): `EffectAccountingProcessor` logs every non-empty derivation, so the question
is now answered by reading a build rather than by argument. One thing to weigh when it is: the processor has since
gained a **second** job — A6's rejection of a supplied row entry whose argument nothing determines — and that one is
not a shadow of anything. Retiring the subset check does not retire the processor.

**Measured 2026-09-09, and it is weaker than the argument above assumes.** The derivation sees a *reference that
forwards a received binding to a callee declaring that ability as a row entry*, which catches **propagation through a
declaring callee** — `main` calling `{Inf, Console} loopForever` at its own bindings forwards both. It does **not** see
a **direct operation call**: by the time a body is monomorphic, `AbilityResolver` has rewritten `printLine` into the
*implementation* method, and an implementation method declares no row — the row is on the ability's member, which is no
longer what the body names. So `loopForever`, whose whole body is `forever(printLine(…))`, derives nothing, and neither
does a `main` that performs `Console` directly. That is not a hole (the pre-mono scope check reports an uncovered
effect at the reference, for an operation and a declaring callee alike, and is complete before monomorphization), but
it does mean the remaining coverage is a strict subset of the scope check's, with no case of its own. The measurement
is pinned as assertions in `jvm/…/EffectAccountingDerivationTest` so it cannot drift while the decision is open.

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
| `effects-as-channel.md` (v2, "uniform carriers") | the first shipped design: the carrier as a type argument the *checker solves* | `§0`–`§13`, `finding N`, `U1`/`U4-x` | superseded in direction and in code. What it got right and still holds: the **channel** (§3.3) and rows never flowing into types. Its carrier-specific results — carrier-ness by tag, `Id` without `Suspend[Id]`, the payload/row rendering inverter — went with the carrier. What it got wrong is §12's first entry |
| `effects-as-rows.md` (v3) | the previous landed design + its A.1–A.11 record: the elaborator writes the carrier | `§1`–`§9`, `A.x`, `R1`–`R6` | its **user model** is Part I §1–§2 and its **standing rules** are §5, both largely intact; its mechanism (the elaborator, the derivation spec, the whitelist) is superseded by §9. §8's gate method is its A.9.4 |
| `effect-row-tails.md` | pinned rows as the one spelling of a carrier stack | prose only | no live content: a stored computation is a thunk (§2.3) and D4 dissolved |
| `testing-effects.md` | substituting effect implementations | `L1`–`L3`, `§2.x` | §6 — the question is the same and the answer changed: a name, not a carrier |
| `effects-v5-one-carrier.md` | rows as constraints on one carrier — the subtraction from v3 | `§4 step N`, `§5 Q1`–`Q4`, `§7` | §2.1 (step 1) and §2.2 (step 2) both survive v6 unchanged in meaning; its §7 (an ability requiring abilities) has no v6 spelling (§2.4); step 4 and Q1 are §12 |
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
