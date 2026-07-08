---
name: eliot-code
description: Use when writing, editing, or reviewing Eliot (`.els`) source code — application programs, examples, or library-layer code. Covers the full surface syntax (declarations, expressions, blocks, match, operators), the type system (Int bounds, auto, Coerce/Combine, return guards), the effect system (effect rows, direct style, discharge), the no-recursion rule, and the idiomatic-style chapter (blocks + dot chains, subject-last design, direct style). For deciding WHICH layer a file goes in use eliot-layers; for `/** */` doc comments use eliot-apidoc.
---

# Writing Eliot code

Eliot is a functional, strongly-typed language: whole-program compilation monomorphized from `main`,
types are ordinary values (λ\*, `Type : Type`), **total by default** (no recursion or loops in user
code), with algebraic effects in direct style. Files use the `.els` extension; one file = one module.

Verify anything non-trivial by compiling and running a real program:

```bash
./mill examples.run jvm exe-jar examples/src/ -m HelloWorld   # builds target/HelloWorld.jar
java -jar target/HelloWorld.jar
```

## File & module structure

- A file is a module; `eliot/lang/String.els` is module `eliot.lang.String`. Declarations may appear
  in any order, imports included.
- `import eliot.effect.Console` — dotted lowercase packages + one Uppercase module name, all on one
  source line. An import brings in every name the module declares (merged across layers).
- **Auto-imported** (no import needed): `eliot.lang.{Function, Unit, String, BigInteger, IO, Int, Runtime}`
  and the bare name `Type`. *Everything else needs an import* — including `Bool`, `Option`, `Either`,
  `Pair`, `Guard`, and every effect (`import eliot.effect.Console`, `.State`, `.Throw`, …).
- **Name resolution is per-file**: a file sees its own declarations + its imports — *never* names
  declared in a sibling file of the same module. When a file hosting an `implement` needs the
  `ability` declared in a sibling layer/file, it re-declares (copies) it; the merge verifies the
  copies agree. This duplication is sanctioned — see eliot-layers.
- Comments: `//` line, `/* */` block (not nested), `/** */` doc comment (see eliot-apidoc).

## Declarations

### `def` — named values ("functions")

```eliot
def main: IO[Unit] = printLine("Hello World!")           // no params: an ordinary value
def describe(x: Int): String = intToString(x)            // body ⇒ concrete
def printLine(s: String): IO[Unit]                       // no body ⇒ abstract (a layer/native supplies it)
private def readLineInternal: String                     // module-private
def showAnything[A ~ Show](thing: A): String = show(thing)
```

Full shape: `[opaque] [private] [FIXITY] def name[GENERICS](args): ReturnType [= body]`.
The return type is **mandatory**; the body is optional (absent = abstract declaration). Argument
lists are optional (`def state: F[S]`). Functions are curried values: `def f(a: A, b: B): C` *is* a
value of type `A => B => C`, so partial application `f(a)` is a plain `B => C`.

### `type` — abstract types and aliases

```eliot
type String                                  // abstract: no representation chosen here
type IO[A]                                   // abstract, generic
type Byte = Int[-128, 127]                   // alias (a body makes it an alias, still platform-neutral)
infix right type =>[A, B] = Function[A, B]   // operator-named type alias
```

`opaque type X[…] = body` (platform layers only) keeps `X` definitionally distinct from its
representation body; a post-check pass unfolds it.

### `data` — concrete types with value constructors

```eliot
data Pair[A, B](first: A, second: B)                 // record: one constructor + field accessors
data Option[A] = None | Some(value: A)               // sum: constructors separated by |
data Database(url: String)
```

A `data` introduces the type constructor (`Pair[A, B]` — use with `[]`), the value constructor(s)
(`Pair(a, b)` — use with `()`), and one accessor per field, **subject-last**: `first(p)` / `p.first`.
Every `data` type is `match`-able automatically (no derive keyword exists). `data` never goes in the
abstract base layer (eliot-layers).

### `ability` / `implement` — typeclasses and instances

```eliot
ability Show[A] {
   def show(a: A): String
}

ability State[S, F[_]] {                    // multi-parameter; F[_] is a higher-kinded param
   def state: F[S]
   def putState(s: S): F[Unit]
}

ability AssociatedType[T] {
   type MagicType                           // associated type: an ordinary member returning Type
   def handle(value: T, param: MagicType): String
}

implement Show[Hello] {
   def show(a: Hello): String = "Hello World!"
}

implement[A ~ Show] Show[Box[A]] {          // conditional instance: needs Show[A]
   def show(box: Box[A]): String = show(box.content)
}

implement[G[_] ~ Suspend & Effect] Suspend[AbortCarrier[G]] { ... }   // & = several constraints on one binder
```

Rules users must know: an `implement` must live either in the ability's module or with the target
type's module; instances must be **unique per type combination** in the whole program (no overlap);
resolution happens fully at monomorphization (no runtime dictionaries). Constrain a generic with
`~`: `[A ~ Show]`, `[F[_] ~ Suspend & Effect]`; a bare `[A ~ Show]` applies the ability to `A` itself.

**Design rule — an ability is a minimal algebra.** Declare in the ability only the *primitive
operations* an implementor must give meaning to — the ones that cannot be derived from the others.
Every derived/convenience combinator goes **outside**, as a plain top-level function in the same
module: there it holds *by construction* for every instance (a law, not a hope), adds nothing to
the contract implementors must read, and nothing to the character-exact copy a layer re-declaring
the ability must carry. Nothing is lost by staying outside: same module and import, identical
generated code (monomorphization resolves both statically), and dot-chaining works on any function
(`.` is positional, not member-based). The stdlib model is `State`: `state`/`putState` are the
algebra; `updateState(f) = putState(f(state))` is a top-level derived function. Ability methods
*may* carry a default body (instances then need not implement them), but reserve that for the rare
cases that genuinely need a per-instance override point: an instance can implement it materially
better or stronger (e.g. one atomic read-modify-write), mutually-defaulting primitives offering
alternative minimal definitions, or extending an already-published ability without breaking its
instances. "It's convenient" is not one of these — convenience lives outside.

**What varies decides where it lives — the three axes (base-stdlib design).** Whether a stdlib
operation is an ability method, a top-level function, or *nothing at the language level*, follows from
which kind of variation it has. Keep them apart:

- **Per-platform** (JVM vs. ATtiny want different *bodies* for the same op) → the **layer merge**,
  which redefines *any* `def`, method or top-level alike. Per-platform difference **alone never
  justifies an ability** — `printLine` is redefined per layer without one.
- **Per-type** (`Int`/`String`/`Money` order differently) → **ability dispatch**, the only
  type-varying mechanism. This alone earns a place *inside* the ability.
- **Per-hardware efficiency of a *derivable* op** (a one-instruction `min`, `<`, a branchless select)
  → a **backend peephole on the derived form**, *not represented in the language*.

So the primitive filter is sharper than "cannot be derived": an op belongs in the ability **only if it
carries per-*type* meaning the primitive lacks.** A fast native instruction is per-hardware
*efficiency*, not per-type *meaning* — leave it a derived top-level function and let codegen recover
the instruction later. Worked case: `Ord[A]`'s sole primitive is `lessThanOrEqual`; `<`/`>`/`>=`,
`min`, `max` are top-level derivations (`min(a, b) = fold(a <= b, a, b)` is identical for every type),
so a native `min` is a codegen peephole, never a method. Two things *do* re-enter the ability: an op
needing the type's own structure the primitive doesn't expose, and per-instantiation
representation-**width** dispatch (`Int` compare differs byte vs. bignum) — but prefer *one* instance
selecting the width native by a compile-time `fold` over many guarded instances when the result grows
no bounds (comparison → `Bool`; contrast `IntArith`, whose result bounds grow). All of this governs the
**base stdlib only** — platform layers and examples freely write concrete, representation-specific bodies.

### Generic parameters

`[NAME]`, in UPPERCASE, with optional kind/type and constraints:

| Form | Meaning |
|---|---|
| `[A]` | ordinary type parameter (`A: Type`) |
| `[F[_]]` | higher-kinded, `Type -> Type` (also `[F[_[_]]]` etc.) |
| `[I: BigInteger]` | a *value*-typed parameter — types are values, so generics can be numbers, strings… |
| `[A ~ Show]`, `[G[_] ~ State[S] & Effect]` | ability constraints |
| `[auto MIN: BigInteger]` | `auto` = omittable at use sites; the compiler infers/calculates it |

Explicit type arguments are passed in brackets: `hello[1]`, `Box[String]`, `Person["John"]`,
`nativeWiden[LMin, LMax, min(LMin, RMin), max(LMax, RMax)](left)`. An *empty* `[]` forces the name
into the Type namespace where a bare name would resolve as a value (`JvmByte[]`).

### Operators and fixity

Any `def`/`type` may be named with an operator symbol (chars from `!#$%&*+./<=>?@\^|-~;`, excluding
the reserved `( ) [ ] { } , -> _ :: : ~ & =`). Fixity and precedence are declared on the definition,
before `def` (same line or the line above):

```eliot
infix def when[A](a: A, cond: Bool): Option[A] = ...     // infix, default left-associative
infix left
def |(lhs: Bool, rhs: Bool): Bool = rhs                  // fixity line above the def also works
infix left at +                                          // at = same precedence as +
def -(...) ...
infix left above +                                       // above = binds tighter than +
def *(...) ...
infix left below apply def .[A, B](a: A, f: Function[A, B]): B = f(a)
```

- Associativity: `left` (default) / `right` / `none`.
- Precedence is a **partial order** declared relative to other operators: `above X`, `below X`,
  `at X` (multiple allowed: `below apply above (*, /)`). Two unrelated operators in one expression
  is a compile error — relate them explicitly. `apply` (function application) is the ceiling:
  `above apply`/`at apply` are rejected; `below apply` is the "binds tightest" idiom.
- Alphanumeric names can be infix too: `infix def or(...)`, then `a or b`.
- `prefix` and `postfix` also exist.

## Expressions

### Application

```eliot
f(a, b)          // call — the ( MUST be adjacent to the name (no space)
f (a)            // two atoms: still applies f to (a), but by juxtaposition — this is
                 // what lets an infix operator take a parenthesized operand: a op (x)
f a b            // juxtaposition application, left-associative: (f a) b
f(a)(b)          // supplying a curried Function[A, Function[B, C]]
subject.f(a)     // dot: f(a, subject) — see the idiom chapter
```

Adjacency matters: `f(x)` attaches `(x)` as arguments; `f (x)` leaves `(x)` a separate operand
(needed when an infix operator's right operand is parenthesized, e.g. `parseBad catch (err -> err)`).
Juxtaposition IS application, so it binds tighter than **every** infix operator including `.` —
`printLine msg.content` is `content(printLine(msg))`, a type error; write `printLine(msg.content)`.
Never call `apply(f, a)` in ordinary code — it exists only so application can be passed as a value.

### Lambdas

```eliot
x -> body                    // one parameter
(x, y) -> body               // several parameters
_ -> body                    // discarded parameter
(s: String) -> body          // annotated parameter
err -> err                   // parenthesize when used as an infix operand: catch (err -> err)
```

Lambda parameters are non-recursive; `->` is reserved for lambdas and `case` arms — the *function
type* is spelled `A => B` (right-associative: `A => B => C` = `A => (B => C)`), never `A -> B`.

### Blocks and statements

A block `{ … }` is a newline-separated sequence of statements ending in a result expression. It
lowers to immediately-applied lambdas, and effect sequencing falls out automatically:

```eliot
def swap(next: String): {State[String]} String = {
  val old = state          // val binds a step's result (effectful steps are sequenced for you)
  putState(next)           // bare statement: performed, result discarded
  old                      // final line must be an expression, not a binding
}
```

- `val x = e` and typed `val x: T = e`. Annotating a `val` with the *carrier* type stores the
  computation instead of running it (`val program: IO[String] = readLine` — deliberate storage).
- A `val`'s right-hand side may itself be a block, or carry a trailing `match { … }`.
- Line joining inside a block: a line *starting* with an infix operator (`.bar`, `orElse x`) or a
  line *ending* with one merges with its neighbor — so multi-line dot chains work. A blank line
  never merges; a line never merges into a following `val`.
- Errors to expect: empty block; block ending in a binding; `val x = … x …` (self-reference).

### `match`

```eliot
def describe(m: Maybe[String]): String = m match {
  case Nothing  -> "empty"
  case Just(v)  -> v
}

def personName(t: Type): String = t match {
   case Person[name] -> name        // TYPE match: [] patterns match a Type value's constructor
   case _            -> "Unknown"
}
```

Patterns are: `Uppercase` constructor (with optional nested `(…)` sub-patterns, or `[…]` for type
patterns), lowercase `variable` (binds anything), and `_` wildcard. **There are no literal
patterns.** Exhaustiveness is checked. Pitfall: a lowercase name is a *binder*, so
`case nothing -> …` silently matches everything — constructors are always Uppercase.

### Branching and literals

- **There is no `if`/`then`/`else`.** Branch with `match`, with `fold(cond, whenTrue, whenFalse)` on
  `Bool`, or with the eliminators (`foldOption`, `foldEither`, `foldPair`). Eliot is **strict**:
  `fold` *selects* between two already-evaluated values — do not rely on a branch being skipped.
- Integer literals are decimal and have singleton types: `42 : Int[42, 42]`. A `-` glued to digits
  is a negative literal (`Int[-128, 127]` works), so binary subtraction needs spaces: `a - 1`,
  never `a-1`.
- String literals `"…"` with standard backslash escapes. No floats, chars, tuples, or string
  interpolation. Pairs are `Pair(a, b)`.
- Identifiers admit any Unicode letter; operator names are limited to the ASCII operator chars.

## Type system

- `type Int[auto MIN: BigInteger, auto MAX: BigInteger]` — the bounds ARE the type's identity:
  `Int[0, 255]` ≠ `Int[0, 1000]`. Aliases: `Byte`, `UnsignedByte`, `Short`, `UnsignedShort`,
  `Medium`, `UnsignedMedium`, `Long`, `UnsignedLong`.
- Arithmetic carries bounds in the result type (`+` gives `Int[add(LMin,RMin), add(LMax,RMax)]`,
  `*` binds tighter than `+`/`-`), so results grow instead of overflowing.
- **Widening is automatic** via the `Coerce[From, To]` ability, inserted by the checker wherever the
  types don't already match: `def count: UnsignedByte = 7` just works. `Coerce`/`Combine`
  (`import eliot.compiler.Coerce` / `.Combine`) are open extension points — implement them for your
  own types.
- **`Combine[A, B]`** computes the join for covariant meeting points (match arms, shared result
  types); for `Int` it spans both ranges.
- **`auto` in practice** — omit what the compiler can recover:
  - Parameter/field position ⇒ *generalized*: `def describe(x: Int): String` accepts every width;
    `data Counter(n: Int)` generalizes the field.
  - Return position ⇒ *calculated* from the body: `def double(x: Int): Int = x + x` publishes the
    exact result bounds. Style: bare returns for internal helpers, explicit ranges for public API.
  - A body-less (abstract) def cannot have a calculated return — spell it out. Same when the body
    never produces the value (only `raise`/`abort`): nothing grounds the bounds, so declare them
    (`{Abort} UnsignedShort`, not `{Abort} Int`).
- **Guarded returns** reject bad instantiations with your message (`import eliot.lang.Guard`):

  ```eliot
  def head[A, MIN, MAX](xs: Seq[A, MIN, MAX]): A when (MIN > 0) orError "head requires a non-empty Seq"
  ```

- Types are values: a `def` can return a `Type` and be *used* as a type
  (`def stringBox: Type = Box[String]` then `def x: stringBox = …`), generic args can be values
  (`Person["John"]`, `hello[1]`), and type expressions can compute (`Box[I.inc]`).
- **Use-site verification**: the whole program is checked monomorphized-from-`main`. A latent
  problem in a generic definition (a failed `Coerce`, a rejected guard, a missing instance) surfaces
  as a hard error at the concrete *use site* — sound, never silent, but the message may point at a
  caller.

## Effect system

Declare effects as a set in braces before the return type; the result type stays the **plain value**:

```eliot
def echo: {Console} Unit = printLine(readLine)              // direct style: no flatMap, no lift
def rename(next: String): {Console, State[String]} Unit     // unordered set, one shared carrier
def parse(s: String): {Throw[String]} Tree = raise("not a tree")
```

`{E1, E2} A` desugars to one shared inferable carrier: `[auto F[_] ~ E1 & E2] F[A]`. Write the
`{…}` sugar; hand-write the `[G[_] ~ Effect]` form only in carrier-generic library code that must
*name* the carrier (a discharge combinator threading a `StateCarrier`/`AbortCarrier`/… ).

The sugar works in **any** type position, not just the return type — an **argument** may be an
effectful value too. All `{…}` occurrences in one signature collapse onto the *same* inferred
carrier, so an effectful-in/effectful-out combinator drops the hand-written binder entirely:

```eliot
def if[T](condition: Bool, value: {Abort} T): {Abort} T = fold(condition, value, abort)
// desugars to  def if[auto F[_] ~ Abort, T](condition: Bool, value: F[T]): F[T]
```

Here `value`'s effect row and the result row are one carrier — exactly what `if` needs (only the
taken branch's effect runs). Reach for the explicit `[F[_] ~ …]` form only when you genuinely name
the carrier type (see the discharge combinators above); prefer the `{…}` row everywhere else.

- **Direct style is the point.** An effectful call yields its plain value (`readLine : String` in a
  `{Console}` body); the checker inserts `flatMap`/`map`/`pure` for you, through blocks, arguments,
  dot chains, and user-defined combinators alike. Never hand-write the monadic plumbing in
  application code.
- **used ⊆ declared**: a body may only perform effects its signature declares
  ("performs the effect '…' but does not declare it"); performing any effect while declared pure is
  an error. Declaring an unused effect is fine.
- **The effects** (each import-required from `eliot.effect`):

  | Row | Operations | Discharge |
  |---|---|---|
  | `{Console}` | `printLine(s)`, `readLine` | run in `IO` |
  | `{Log}` | `log(s)` | run in `IO` |
  | `{Abort}` | `abort` (untyped short-circuit) | `runAbort` → `G[Option[A]]`; infix `orElse` fallback |
  | `{Throw[E]}` | `raise(err)` | `runThrow` → `G[Either[E, A]]`; infix `catch (e -> …)` |
  | `{State[S]}` | `state`, `putState(s)`, `updateState(f)` | `runStateToPair(p, s0)` → `G[Pair[A, S]]`; also `runStateToValue`, `runStateToFinalState` |
  | `{Dep[X]}` | `dependency` (type-dispatched; provide via `implement Dep[X, IO]`) | resolved at compile time |
  | `{Inf}` | `forever(step)` | never discharged — may reach `main` (server/firmware loop) |

  `Suspend` and the carriers (`AbortCarrier`, `ThrowCarrier`, `StateCarrier`) are plumbing —
  application code never names them.
- **`main` is concrete**: `def main: IO[Unit] = …` — the one place the carrier is pinned to `IO`.
  Keep business logic carrier-polymorphic (`{Console} Unit`, not `IO[Unit]`) so it runs on the
  production carrier *and* a pure test carrier (define an `Id` `data` + `Effect[Id]` instance, then
  `runId(runAbort(logic))` tests with no I/O).
- **The row wraps the plain value type, never a carrier**: `{Inf, Console} Unit`, NOT
  `{Inf} IO[Unit]` (that means `F[IO[Unit]]` — "Cannot resolve type."). A non-terminating program
  (verified pattern): `def serve: {Inf, Console} Unit = forever { printLine("tick") }` and
  `def main: IO[Unit] = serve`.
- **Discharge order decides interaction** for non-commuting effects: discharging `Abort` before
  `State` keeps the final state (`Pair[Option[A], S]`); the other order discards it
  (`Option[Pair[A, S]]`). See `examples/src/EffectsOrdering.els`.
- **A discharge wraps the computation expression** — pass the effectful call directly:
  `runStateToPair(logic orElse fallback, s0)`. Do NOT bind the effectful value first and discharge
  the binder (`val x = logic orElse f` then `runStateToPair(x, s0)`): the `val` binds the plain
  value direct-style and the remaining effects float upward, so `x` is not a carrier.
- **The cross-lift matrix is partial** (verified 2026-07): `State`+`Abort` compose in either
  discharge order, and every effect composes with the `Suspend`-riding ones (`Console`, `Log`). But
  `State`+`Throw` has no cross-lift instance yet — combining them fails with a missing-instance
  error ("does not implement ability …"). When you hit that, switch to `Abort`, or add the missing
  `implement` in the jvm layer.
- Recover-and-continue reads infix: `parseBad catch (err -> err)`, `safe orElse "<absent>"`.

## Total by default — no recursion

User code **cannot** recurse or loop: any cycle among value bodies is rejected
("Value 'X' is defined recursively."), lambdas can't see themselves, and there is no `fix`. Every
loop lives in a platform native reached through an eliminator or `forever`:

- Iterate/branch with the fold family: `fold` (Bool), `foldOption`, `foldEither`, `foldPair`,
  `match`, and container folds as they appear.
- Loop forever only via `{Inf}`'s `forever(step)` — the effect propagates to callers and may reach
  `main` undischarged.
- Recursive *types* are fine covariantly (`data Tree(left: Tree, right: Tree)`); negative recursion
  is rejected by strict positivity.

## Idiomatic Eliot

Terseness in service of readability: prefer the shortest form the reader parses in one glance, and
spend length only where it buys clarity.

### One-liners stay expressions; everything else is a block

A definition whose body is a single readable expression keeps the `= expr` form:

```eliot
def echo: {Console} Unit = printLine(readLine)
```

Anything with intermediate steps, more than one effectful action, or a value used twice becomes a
block with `val` statements — never a nest of parentheses or a hand-rolled `flatMap` tower:

```eliot
def main: IO[Unit] = {
  val outcome = runStateToPair(rename("after"), "before")
  printLine(outcome.second)
}
```

Judgement applies: a two-step body that reads perfectly inline can stay inline; a "one-liner" you
have to read twice should have been a block.

### The dot operator carries the flow

`subject.f(a).g(b)` is `g(b, f(a, subject))` — left-to-right, in run order, instead of inside-out.
Prefer it for any chain of transformations, including field access and zero-argument steps
(`box.content`, `dependency.url`, `p.second`):

```eliot
def logic: Box[String] = Box("Hello").filter("Expr").map(_ -> "Earth!").as("World!")
```

Long chains break across lines — a leading `.` continues the previous line, in `def` bodies and
block statements alike:

```eliot
def logic: Box[String] = Box("Hello")
  .filter("Expr")
  .map(_ -> "Earth!")
  .as("World!")
```

**Design consequence — subject last.** For a function to be dot-chainable, its subject (the value
it transforms) must be the **last** parameter: `def map[A, B](f: A => B, box: Box[A]): Box[B]`,
`def orElse[…](computation: …, fallback: A)`. This is the house style for *every* function that has
a natural subject, not just those you expect to chain — it is the opposite of the "receiver first"
intuition, so check parameter order on every new signature. Configuration/auxiliary arguments go
first, subject last.

### Direct style, always

Write effectful code as if it were pure and let the checker insert the machinery. `flatMap`/`map`/
`pure` appear in exactly one place: `implement Effect[YourCarrier]` blocks (and carrier plumbing in
platform layers). If application code contains `flatMap`, rewrite it as a block or a direct call:

```eliot
// NO:  flatMap(old -> flatMap(ignored -> pure(old), putState(next)), state)
// YES:
def swap(next: String): {State[String]} String = {
  val old = state
  putState(next)
  old
}
```

Discharge effects with the friendly combinators (`orElse`, `catch`, `runStateToPair`) at the
boundary — usually right before `main` — and keep everything above it in `{…}` rows.

### Application: `()`, juxtaposition, currying — never `apply`

- Normal calls use adjacent parens: `f(a, b)`. Juxtaposition `f a` is fine where it reads cleanly —
  single-argument calls in operator-flavored code (`forever { … }` passes a block argument this
  way). But never juxtapose an argument that carries its own dot chain or infix operator —
  application binds tightest, so revert to parens there.
- `apply` exists for technical reasons only (naming application as a value). Do not call it.
- Exploit currying for readability, not puzzle-making: pass a partially-applied function where a
  lambda would only shuffle arguments (`foldOption(o, error(msg), pure)` — not `v -> pure(v)`), and
  design multi-stage functions (`f(config)(subject)`) when call sites repeatedly fix the first part.
  If the reader must count arguments to see what's still missing, use a lambda instead.
- **Eta-reduce a lambda that only applies a function** — `x -> f(x)` *is* `f`. This catches the
  common *dot-projection* lambda too, because `pair.first` is just `first(pair)` (subject-last), so
  `pair -> pair.first` reduces to the bare accessor `first` (and currying makes `first`/`second`
  drop straight into a combinator slot). A projecting `map` then collapses to a dot chain over a
  bare accessor: write `runStateToPair(p, s0).map(first)`, not
  `map(pair -> pair.first, runStateToPair(p, s0))`.
- Point-free is welcome one step at a time (`pure`, `intToString`); avoid composing chains of
  nameless combinators.

### Signatures

- Function types are `A => B`, never `Function[A, B]`; lambdas are `x -> body`.
- Effect rows over concrete carriers: `{Console} Unit` in logic, `IO[Unit]` only at `main` and in
  platform instances.
- Bare `auto`-elided `Int` for internal helpers (tightest calculated bounds); explicit ranges
  (`Int[0, 1000]`, `UnsignedByte`) on public API as a stable contract.
- Guard partial functions at the type level (`when … orError "…"`) so misuse fails at compile time
  with your message, rather than documenting "don't call this with…".
- Naming: `lowerCamelCase` defs/params, `UpperCamelCase` types/constructors/modules, UPPERCASE
  generic parameters. Eliminators are named `foldX`.

### Worked example

Compiled and run against the real toolchain (prints `8080` on unparseable input):

```eliot
import eliot.effect.Console
import eliot.effect.State
import eliot.effect.Abort
import eliot.lang.Pair

// A stand-in parser that gives up. Explicit bounds: an abort-only body can't calculate them.
def parsePort(raw: String): {Abort} UnsignedShort = abort

def fallbackPort: UnsignedShort = 8080     // literal widens into the declared range via Coerce

// Direct style: read a line, remember it, parse it. Three effects, no plumbing.
def nextPort: {Console, State[String], Abort} UnsignedShort = {
  val raw = readLine
  putState(raw)
  parsePort(raw)
}

def main: IO[Unit] = {
  val result = runStateToPair(nextPort orElse fallbackPort, "<none>")   // discharge at the boundary
  printLine(intToString(result.first))
}
```

The shape is the point: blocks with `val` steps, direct effectful calls, subject-last dot access
(`result.first`), and discharge combinators wrapping the computation right at the boundary.

## Gotchas checklist

- `f(x)` call vs `f (x)` separate operand — adjacency decides.
- Juxtaposition binds tighter than `.` and every other infix op: `f x.g` is `g(f(x))` — parenthesize.
- Effect rows wrap the plain type: `{Console} Unit`, never `{Console} IO[Unit]`.
- `a-1` lexes as `a` `(-1)`; write `a - 1`.
- Lowercase in a `case` pattern is a binder, not a constructor reference; no literal patterns.
- `fold` and eliminator branches are **strict** — both arms evaluate.
- Every effect and most of `eliot.lang` (`Bool`, `Option`, `Either`, `Pair`, `Guard`) must be
  imported; only `Function/Unit/String/BigInteger/IO/Int/Runtime` + `Type` are ambient.
- Generic params UPPERCASE; def names must not start uppercase (`some`, not `Some` — uppercase
  names are constructors/types).
- Empty `[]` forces the Type namespace (`JvmByte[]` in a value position).
- A block must end in an expression; a `val` cannot reference itself; blank lines stop line-merging.
- Two infix operators with no declared precedence relation cannot share an expression.
- No recursion — restructure around folds/`match`/`forever`, or lift the loop into a native
  (platform layer).
- Parenthesize lambda operands of infix operators: `catch (err -> err)`.
- New `.els` files: pick the layer first (eliot-layers) — user programs and examples are plain
  files; `data`/natives never go in the abstract base.

## See also

- **eliot-layers** — which layer a `type`/`def`/`data`/`ability`/`implement` belongs in; the
  abstract↔concrete merge and its signature-equality traps.
- **eliot-apidoc** — writing `/** */` documentation that renders on the apidoc site and LSP hover.
- **eliot-monomorphize** — internals of the NbE checker (only needed when changing the compiler).
- CLAUDE.md cornerstones: Types Are Values; Platform-Independence via Layers; Use-Site
  Verification; Total by Default.
- `examples/src/` — one small program per feature (`Blocks`, `DotOperator`, `Effects*`, `Match`,
  `TypeValues`, `Ranges`, `Operators`); read these first when unsure of a form.
