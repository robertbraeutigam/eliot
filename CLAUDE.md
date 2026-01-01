# Claude Memory

## Project Overview

ELIOT is a functional, generic programming language for microcontrollers, implemented in Scala 3.

The project contains all parts of the compiler, the ELIOT standard library.

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
mill base.test
mill jvm.test
mill eliotc.test

# Run a single test class
mill base.test -- "com.vanillasource.eliot.eliotc.ProcessorTest"

# Format code with scalafmt
mill mill.scalalib.scalafmt.ScalafmtModule/reformatAll __

# Clean build artifacts
mill clean

# Run the compiler and generate and executable JAR file from the HelloWorld example
# The generate jar file will be under target/HelloWorld.jar
mill examples.run jvm exe-jar examples/src/ stdlib/src/ jvm/lib/ -m HelloWorld
```

### Module Structure

The project is organized into four main modules (defined in `build.mill`):

1. **eliotc** - Core compiler infrastructure (plugins, processors, feedback, utilities)
2. **base** - Base compiler components (parsing, AST, type system, module system, resolution)
3. **jvm** - JVM backend (bytecode generation using ASM, JAR generation)
4. **examples** - Example ELIOT programs

**Dependency chain:** `examples` → `jvm` → `base` → `eliotc`

All modules use Scala 3.7.4 and share common dependencies: Cats Effect, Parsley (parser combinators), log4j2, and ScalaTest.

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

### Error Handling

- Errors are reported through `CompilationIO` monad (in base module)
- `SourcedError` tracks errors with source position information
- `User` and `Logging` traits provide user-facing messages and debug logging

## Testing

- Tests use ScalaTest with `AnyFunSuite` style
- Tests use extend AsyncFlatSpec with AsyncIOSpec with should.Matchers
- Test files are in `<module>/test/src/` directories

## Development Notes

- The language is still in active development (see TODO file for planned features)
- Current focus is on microcontroller targets, though JVM is the implemented backend
- The type system aims to support dependent types and compile-time guarantees about resource usage
- The standard library (`stdlib/`) is currently minimal
