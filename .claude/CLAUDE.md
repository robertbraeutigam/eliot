# Claude Memory

## Project Overview

ELIOT is a functional, generic programming language for microcontrollers, implemented in Scala 3.

The project contains all parts of the compiler and the ELIOT standard library.

The compiler uses a plugin-based architecture with a fact-based compilation system and supports 
multiple backends (currently JVM).

## Build System

This project uses **Mill** (version 1.1.0+) as its build tool.

### Common Commands

```bash
# Compile all modules
mill __.compile

# Run tests for all modules
mill __.test

# Run tests for a specific module
mill lang.test
mill jvm.test
mill eliotc.test

# Run a single test class
mill lang.test -- "com.vanillasource.eliot.eliotc.ProcessorTest"

# Format code with scalafmt
mill mill.scalalib.scalafmt.ScalafmtModule/reformatAll __

# Clean build artifacts
mill clean

# Run the compiler and generate and executable JAR file from the HelloWorld example
# The generate jar file will be under target/HelloWorld.jar
mill examples.run jvm exe-jar examples/src/ -m HelloWorld

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
  - `BasePlugin` - Provides core language compilation (tokenization → AST → type checking)
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
7. symbolic: Symbolic type checker. Checks types as far as possible with generic parameters (symbols).
8. implementation: Checks and returns a type-specific ability implementation.
9. abilitycheck: Check abilities can be proven to exist. Also resolves abilities directly if all types are already known.
10. monomorphize: Monomorphic type checker. Checks all types at their usage with all instantiated values.
11. used: Collects all the used value names starting at a given "main".
12. uncurry: Uncurries function calls, so its easier to generate on the backend.

### Error Handling

- Errors are reported through `CompilationIO` monad (in lang module)
- `SourcedError` tracks errors with source position information
- `User` and `Logging` traits provide user-facing messages and debug logging

## Testing

- Tests use ScalaTest with `AnyFunSuite` style
- Tests use extend AsyncFlatSpec with AsyncIOSpec with should.Matchers
- Test files are in `<module>/test/src/` directories

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

## Development Notes

- The language is still in active development (see TODO file for planned features)
- Current focus is on microcontroller targets, though JVM is the implemented backend
- The type system aims to support dependent types and compile-time guarantees about resource usage
- The standard library (`stdlib/`) is currently minimal
