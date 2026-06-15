# Claude Memory

## Project Overview

ELIOT is a functional, generic programming language for microcontrollers, implemented in Scala 3.

The project contains all parts of the compiler and the ELIOT standard library.

The compiler uses a plugin-based architecture with a fact-based compilation system and supports 
multiple backends (currently JVM).

## Build System

This project uses **Mill** (version 1.1.0+) as its build tool. Scala version: **3.7.4**.

Key dependencies: `cats-effect` (IO monad), `parsley-cats` (parser combinators), `ASM 9.9` (JVM bytecode in jvm module).

### Common Commands

```bash
# Compile all modules
./mill __.compile

# Run tests for all modules
./mill __.test

# Run tests for a specific module
./mill lang.test
./mill jvm.test
./mill eliotc.test

# Filter test output by class name (the -- flag is rejected by ScalaTest)
./mill lang.test 2>&1 | grep -v DEBUG | grep "ClassName"

# Format code with scalafmt
./mill mill.scalalib.scalafmt.ScalafmtModule/reformatAll __

# Clean build artifacts
./mill clean

# Run the compiler and generate and executable JAR file from the HelloWorld example
# The generate jar file will be under target/HelloWorld.jar
./mill examples.run jvm exe-jar examples/src/ -m HelloWorld

# To run the generated file, use java
java -jar target/HelloWorld.jar
```

### Module Structure

The project is organized into four main modules (defined in `build.mill`):

1. **eliotc** - Core compiler infrastructure (plugins, processors, feedback, utilities)
2. **lang** - Base compiler/language components (parsing, AST, type system, module system, resolution)
3. **jvm** - JVM backend (bytecode generation using ASM, JAR generation)
4. **examples** - Example ELIOT programs

## Architecture

### Plugin System

The compiler uses a **ServiceLoader-based plugin architecture**:

- Plugins are discovered via Java's `ServiceLoader` mechanism
- Each plugin implements `CompilerPlugin` (in eliotc module)
- Plugins can depend on other plugins and configure the compilation pipeline
- Two main plugins:
  - `LangPlugin` - Provides core language compilation (tokenization → AST → type checking)
  - `JvmPlugin` - Provides JVM bytecode generation and executable JAR output

### Compilation Pipeline

The compiler follows a **fact-based compilation model**:

1. **Facts** are pieces of immutable compilation data (tokens, AST, resolved types, etc.)
2. **Processors** (sometimes referred to as "Generators") compute facts from other facts on demand
3. **FactGenerator** orchestrates lazy fact computation with caching
4. Facts are identified by their keys, which are usually a subset of the fact's data

### Main components of processing pipeline

Each of these is a package in the "lang" module, roughly in order of processing:

1. source: Reading source files from the filesystem
2. token: Tokenizer
3. ast: Building the AST
4. core: Building the core language AST
5. module: Splitting working with modules into working with individual values. It also unifies similarly named modules from different paths.
6. resolve: Resolve all identifiers to fully qualified names or parameters.
7. matchdesugar: Desugar pattern-match expressions into function applications. Checks exhaustiveness and handles nested patterns, multiple cases, constructor patterns, and wildcards.
8. operator: Resolve infix operators with proper precedence and associativity. Transforms flat expressions into correctly structured function applications.
9. ability: Checks and returns a type-specific ability implementation.
10. monomorphize: Monomorphic type checker. Evaluates data type and value definitions into typed structures and checks all types at their usage with all instantiated values, using the single NbE evaluator. (This phase absorbed the former standalone `eval` phase, which was removed.)
11. used: Collects all the used value names starting at a given "main".
12. uncurry: Uncurries function calls, so its easier to generate on the backend.

### Error Handling

- Errors are reported through `CompilationIO` monad (in lang module)
- `SourcedError` tracks errors with source position information
- `User` and `Logging` traits provide user-facing messages and debug logging

## Testing

- Tests extend `AsyncFlatSpec` with `AsyncIOSpec` with `Matchers`
- Test files are in `<module>/test/src/` directories

## Language Cornerstone: Types Are Values (λ\*)

Eliot has **no internal distinction between the type level and the value level**. Type
constructors (`Int[..]`) and value constructors (`Int(..)`) are both ordinary named values
("functions"), and the type *of* a value is itself just another value. The only thing that makes
a computation "type-level" is *when* it is forced: type-level code happens to be evaluated by the
compiler before code generation, but that staging is incidental, not a difference in kind.
Formally this is a **non-stratified Pure Type System** — λ\* / "type-in-type" (`Type : Type`) —
where the compile-time/runtime split is a pure **phase / erasure** distinction and type checking is
implemented as **Normalisation by Evaluation** (the `monomorphize` package: one `Evaluator`, one
`SemValue` domain shared by types and values, `VType` as an ordinary value). This is deliberately
*not* "dependent types" as a bolted-on feature; dependency is merely a consequence of types being
values. Accepted trade-off: `Type : Type` is logically inconsistent (Girard's paradox) — fine for a
general-purpose language, with termination/totality handled separately rather than by a universe
hierarchy.

**Sanctioned sugar vs. required discipline.** Familiar surface distinctions are *intentional sugar*,
not violations: the `Qualifier.Type`/`Qualifier.Default` namespaces, the `[]` vs `()` call/pattern
syntax, and the restricted `Expression.typeParser` all collapse to "the same `FunctionDefinition`
with a different qualifier tag." What the cornerstone *requires* of every task: there is exactly one
evaluator and one value domain (never a second, weaker "compile-time" interpreter); type equality is
definitional (force/normalise via that evaluator, then compare) rather than a parallel bespoke
mechanism; and kind/arity metadata (e.g. `RoleHint.TypeConstructor`) stays out of semantic phases.
See `docs/cornerstone-fidelity-plan.md` for the remaining clean-ups.

## Language Cornerstone: Platform-Independence via Layers

Eliot targets everything from an ATtiny to the JVM, so the language and its base stdlib commit to **no
platform assumptions** — not even the size of an `Int`. The base layer is therefore written *purely
abstractly*: it may only declare **`type` definitions without a body** and **`def`s without a body**
(signature only). It never says *how* anything is represented or computed.

- `type Int[MIN: BigInteger, MAX: BigInteger]` — an abstract type; no value constructor, no chosen width.
- `def println(s: String): IO[Unit]` — an abstract function; signature only, no implementation.
- `type Byte = Int[-128, 127]` — a `type` *with* a body is just an alias; still platform-neutral.

A `type X = ...` (alias) and a body-less `type X` differ only by the presence of a body; a `data X(...)`
declaration is the *concrete* form that additionally introduces a value constructor. **The base stdlib
must avoid `data` and avoid `def` bodies** — those belong to platform layers (see
[[feedback_stdlib_platform_independent]]).

**Layers = redefinition, not inheritance.** A platform "implements" an abstract definition by simply
*defining the same name again*, in its own root path, with a body. There is no `extends`, `override`
keyword, or instance mechanism — co-located definitions of the same qualified name across root paths are
**merged**, preferring the concrete one. Example: base declares `type IO[A]` and `def println(...): IO[Unit]`
(abstract); the `jvm` layer redefines `data IO[A](block: Function[Unit, A])` and
`def println(s) = IO(_ -> printlnInternal(s))` (concrete). The compiler unifies them into a single value.

**How it works mechanically** (the `source` + `module` packages):
- The compiler is given multiple **root paths** (CLI roots + classpath resources). `PathScanner`
  (`source/scan/PathScanner.scala`) resolves a module path against *all* roots and returns *every*
  matching file as one `PathScan`.
- `ModuleNamesProcessor` extracts names per file; `UnifiedModuleNamesProcessor` flattens the name sets of
  all files for a module into one.
- For each name, `UnifiedModuleValueProcessor.scala` collects the `ModuleValue` from every file that
  defines it and calls `unifyValues`. The abstract/concrete distinction is carried by
  `core/fact/NamedValue.scala`'s `runtime: Option[Sourced[Expression]]` — `None` = abstract (no body),
  `Some` = concrete. Unification rules:
  - **Prefer the implementation**: pick the value whose `runtime.isDefined`; if none, keep the abstract one.
  - **Reject conflicts**: more than one implementation → "Has multiple implementations."; differing
    signatures → "Has multiple different definitions." (`signatureEquality` must hold across all layers, so
    a layer may add a body but must not change the signature).

So "implementing the abstract stdlib for a platform" = adding a layer (a root path) that re-declares the
same names with bodies / as `data`. The base stays universal; each platform fills in representation and
behaviour. (`data` desugars to an abstract type-constructor `FunctionDefinition` plus value-constructor
functions via `DataDefinitionDesugarer`, so even concrete types reduce to the same `NamedValue` model.)

## Language Overview

Eliot is a functional, strongly-typed language, with whole-application compilation
with monomorphization.

The main building blocks of the language are:
- Named values: These are a generalization of "functions", because functions are represented in their fully 
                curried form as "a -> b -> c", so it's a lambda value. They are "named" because they are not
                anonymous and can be referenced ("called") from elsewhere.
- Data: Data is completely represented as values in the core model and has no own unique representation. 
        Data defines a value constructor "function" (if not abstract), and a type constructor function. Both are
        "normal" values / "functions".
- Abilities and ability implementations: These are typeclasses and typeclass instances, with multi-parameter capability. Ability implementations
        must be defined either in the module where the ability is defined or with the type they are defined for.
        Ability implementations must be unique for a given type combination in the whole search space, with no overlap.
        References to ability values are fully resolved during monomorphization, they are not passed around in
        structures as in some other languages.

## Compiler Change Patterns

- When making a field optional in `resolve.fact.Expression` case classes, update ALL pattern matches across
  the codebase (eval, monomorphize phases) **and** test files that directly construct those cases.
- There is a **single** evaluator (`monomorphize/eval/Evaluator.scala`, NbE over `SemValue`); the
  old separate `eval`/`interpret` compile-time evaluator was removed. Unresolved/stuck terms surface
  as explicit errors at quote time (`PostDrainQuoter`, "Cannot resolve type."), never via a silent
  `Type` fallback. Do not reintroduce a second compile-time evaluator (see the Language Cornerstone).

## Development Notes

- The language is still in active development (see TODO file for planned features)
- Current focus is on microcontroller targets, though JVM is the implemented backend
- The type system aims to support dependent types and compile-time guarantees about resource usage
- The standard library (`stdlib/`) is currently minimal
