# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

ELIOT is a functional, generic programming language for microcontrollers, implemented in Scala 3. The compiler uses a plugin-based architecture with a fact-based compilation system and supports multiple backends (currently JVM).

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

# Compile and run the compiler (examples module)
mill examples.run -- jvm exe-jar -m <module-name> -o target <source-paths>
```

### Module Structure

The project is organized into four main modules (defined in `build.mill`):

1. **eliotc** - Core compiler infrastructure (plugins, processors, feedback, utilities)
2. **base** - Base compiler components (parsing, AST, type system, module system, resolution)
3. **jvm** - JVM backend (bytecode generation using ASM, JAR generation)
4. **examples** - Entry point for running the compiler (depends on eliotc and jvm)

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

1. **Facts** are pieces of compilation data (tokens, AST, resolved types, etc.)
2. **Processors** compute facts from other facts on demand
3. **FactGenerator** orchestrates lazy fact computation with caching
4. Facts are identified by `CompilerFactKey` instances

The base compilation pipeline (from `BasePlugin.initialize`):

1. `SourceContentReader` - Read source file contents
2. `PathScanner` - Scan directories for source files
3. `Tokenizer` - Tokenize source into tokens
4. `ASTParser` - Parse tokens into AST
5. `ErrorReporter` - Report syntax errors
6. `DesugarProcessor` - Desugar syntax
7. `ModuleProcessor`, `ModuleNamesProcessor` - Extract module information
8. `UnifiedModuleNamesProcessor`, `UnifiedModuleDataProcessor`, `UnifiedModuleFunctionProcessor` - Unify module data across files
9. `FunctionResolver`, `TypeResolver` - Resolve symbol references
10. `TypeCheckProcessor` - Type check the program
11. `UsedSymbolsProcessor` - Track used symbols

The JVM plugin adds:
- `JvmClassGenerator` - Generate JVM bytecode using ASM
- `JvmProgramGenerator` - Package into executable JAR

### Key Packages

- `com.vanillasource.eliot.eliotc.compiler` - Main compiler entry point
- `com.vanillasource.eliot.eliotc.plugin` - Plugin system and configuration
- `com.vanillasource.eliot.eliotc.processor` - Processor infrastructure
- `com.vanillasource.eliot.eliotc.ast` - Abstract Syntax Tree definitions
- `com.vanillasource.eliot.eliotc.token` - Tokenization
- `com.vanillasource.eliot.eliotc.typesystem` - Type checking and inference
- `com.vanillasource.eliot.eliotc.resolve` - Symbol resolution
- `com.vanillasource.eliot.eliotc.module` - Module system
- `com.vanillasource.eliot.eliotc.jvm.asm` - Low-level JVM bytecode generation
- `com.vanillasource.eliot.eliotc.jvm.classgen` - Higher-level class generation
- `com.vanillasource.parser` - Custom parser infrastructure

### Error Handling

- Errors are reported through `CompilationIO` monad (in base module)
- `SourcedError` tracks errors with source position information
- `User` and `Logging` traits provide user-facing messages and debug logging

## Testing

- Tests use ScalaTest with `AnyFunSuite` style
- Test files are in `<module>/test/src/` directories
- Key test files:
  - `ProcessorTest.scala` - Tests processor infrastructure
  - `ASTParserTest.scala` - Tests AST parsing
  - `TypeCheckProcessorTest.scala` - Tests type checking
  - `FunctionResolverTest.scala` - Tests symbol resolution
  - `TokenizerTest.scala` - Tests tokenization

## Development Notes

- The language is still in active development (see TODO file for planned features)
- Current focus is on microcontroller targets, though JVM is the implemented backend
- The type system aims to support dependent types and compile-time guarantees about resource usage
- The standard library (`stdlib/`) is currently minimal
