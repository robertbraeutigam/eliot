# Processor Architecture Simplification Proposals

## Executive Summary

The processor architecture is generally well-designed but has accumulated some unnecessary complexity. This document proposes 10 concrete simplifications that will:
- Reduce cognitive load when writing new processors
- Eliminate unused/rarely-used abstractions
- Reduce boilerplate code
- Make the codebase more maintainable

---

## Proposals (Ordered by Impact/Effort Ratio)

### 1. **MERGE TransformationProcessor Dual Methods** ⭐⭐⭐⭐⭐
**Priority: HIGHEST**

**Current Problem:**
- TransformationProcessor has TWO methods that can be overridden: `generateFromFact` and `generateFromKeyAndFact`
- Default implementations chain them together with `@unused` annotations
- `generateFromFact` defaults to `???` which will explode at runtime if you override the wrong method
- Creates confusion: "which method should I override?"
- Only 1 out of 9 implementations uses `generateFromKeyAndFact` (ModuleNamesProcessor), rest use `generateFromFact`

**Files Affected:**
- `eliotc/src/com/vanillasource/eliot/eliotc/processor/common/TransformationProcessor.scala`
- All 9 TransformationProcessor subclasses

**Current Code:**
```scala
protected def generateSingleFact(requestedKey: OutputKey): CompilerIO[OutputFact] =
  getFactOrAbort(keyTransition(requestedKey)).flatMap(fact => generateFromKeyAndFact(requestedKey, fact))

def generateFromFact(@unused fact: InputFact): CompilerIO[OutputFact] = ???

def generateFromKeyAndFact(@unused key: OutputKey, fact: InputFact): CompilerIO[OutputFact] =
  generateFromFact(fact)
```

**Proposed Solution:**
Delete `generateFromFact` entirely. Make `generateFromKeyAndFact` the single abstract method:

```scala
protected def generateSingleFact(requestedKey: OutputKey): CompilerIO[OutputFact] =
  getFactOrAbort(keyTransition(requestedKey)).flatMap(fact => generateFromKeyAndFact(requestedKey, fact))

protected def generateFromKeyAndFact(key: OutputKey, fact: InputFact): CompilerIO[OutputFact]
```

Processors that don't need the key (8 out of 9) simply ignore the `key` parameter. Processors that need it use it.

**Benefits:**
- Only ONE method to override - zero confusion
- No `@unused` annotations needed
- No `???` runtime bombs waiting to explode
- Key is always available when needed
- Simpler mental model
- Less code overall

**Estimated Lines Changed:** ~20 lines across 10 files

---

### 2. **STANDARDIZE Direct CompilerProcessor Implementations** ⭐⭐⭐⭐
**Priority: HIGH**

**Current Problem:**
- Four processors implement CompilerProcessor directly with manual pattern matching:
  - `SourceContentReader` - matches SourceContent.Key
  - `PathScanner` - matches PathScan.Key
  - `JvmProgramGenerator` - matches GenerateExecutableJar.Key
  - `ModuleProcessor` - matches TWO keys: ModuleFunction.Key and ModuleData.Key
- Each duplicates the pattern matching logic that SingleKeyTypeProcessor already provides

**Files Affected:**
- `base/src/com/vanillasource/eliot/eliotc/source/content/SourceContentReader.scala`
- `base/src/com/vanillasource/eliot/eliotc/source/scan/PathScanner.scala`
- `jvm/src/com/vanillasource/eliot/eliotc/jvm/jargen/JvmProgramGenerator.scala`
- `base/src/com/vanillasource/eliot/eliotc/module/processor/ModuleProcessor.scala`

**Proposed Solution:**

**Option A (Recommended):** Make single-key processors extend SingleKeyTypeProcessor
- SourceContentReader, PathScanner, JvmProgramGenerator should extend SingleKeyTypeProcessor
- This removes the boilerplate pattern matching

**Option B:** Create MultiKeyTypeProcessor for ModuleProcessor
- Create `abstract class MultiKeyTypeProcessor[K1, K2](using ClassTag[K1], ClassTag[K2])`
- Handles pattern matching for 2 key types
- Provides: `def generateFact(key: K1 | K2): CompilerIO[Unit]`

**Benefits:**
- Less boilerplate pattern matching code
- More consistent processor implementations
- Easier to write new processors correctly

**Estimated Lines Changed:** ~50 lines across 5 files

---

### 3. **EXTRACT Common Unification Logic** ⭐⭐⭐⭐
**Priority: HIGH**

**Current Problem:**
- `UnifiedModuleFunctionProcessor` and `UnifiedModuleDataProcessor` have nearly identical structure:
  - Get PathScan for module
  - Traverse files to get all facts (with .attempt.map(_.toOption))
  - Call unifyX function with validation logic
  - Error messages follow same patterns
- Code is duplicated with minor variations

**Files Affected:**
- `base/src/com/vanillasource/eliot/eliotc/module/processor/UnifiedModuleFunctionProcessor.scala`
- `base/src/com/vanillasource/eliot/eliotc/module/processor/UnifiedModuleDataProcessor.scala`

**Proposed Solution:**
Create abstract `UnificationProcessor` base class:

```scala
abstract class UnificationProcessor[
  Input <: CompilerFact,
  Output <: CompilerFact,
  InputKey <: CompilerFactKey[Input],
  OutputKey <: CompilerFactKey[Output]
](using ClassTag[OutputKey])
  extends SingleFactProcessor[Output, OutputKey] {

  protected def getPathKey(key: OutputKey): PathScan.Key
  protected def getInputKey(file: File, outputKey: OutputKey): InputKey
  protected def unify(key: OutputKey, facts: Seq[Input]): CompilerIO[Output]

  final override protected def generateSingleFact(key: OutputKey): CompilerIO[Output] =
    for {
      pathScan <- getFactOrAbort(getPathKey(key))
      allFacts <- pathScan.files
                    .traverse(file => getFactOrAbort(getInputKey(file, key)).attempt.map(_.toOption))
                    .map(_.flatten)
      unified  <- unify(key, allFacts)
    } yield unified
}
```

**Benefits:**
- Eliminates duplication between two processors
- Makes the unification pattern explicit and reusable
- Easier to add new unification processors in the future
- Consistent error handling

**Estimated Lines Changed:** ~80 lines (create 1 new file, modify 2 files)

---

### 4. **REMOVE NullProcessor** ⭐⭐⭐
**Priority: MEDIUM**

**Current Problem:**
- `NullProcessor` exists but appears to be unused
- No-op processor that does nothing
- Adds to cognitive load when browsing processor types

**Files Affected:**
- `eliotc/src/com/vanillasource/eliot/eliotc/processor/common/NullProcessor.scala`
- Any files that reference it

**Proposed Solution:**
1. Search for all usages of NullProcessor
2. If unused, delete the file
3. If used, evaluate if it's necessary or if there's a better pattern

**Benefits:**
- Less code to maintain
- One fewer processor type to think about
- If it's not used, it shouldn't exist

**Estimated Lines Changed:** Delete ~10 lines

---

### 5. **SIMPLIFY keyTransition in TransformationProcessor** ⭐⭐⭐
**Priority: MEDIUM**

**Current Problem:**
- The `keyTransition: OutputKey => InputKey` parameter must be specified for every TransformationProcessor
- Creates boilerplate: `(key: SourceTokens.Key) => SourceContent.Key(key.file)`
- The relationship is always simple field extraction
- Going from Output->Input feels conceptually backwards

**Files Affected:**
- `eliotc/src/com/vanillasource/eliot/eliotc/processor/common/TransformationProcessor.scala`
- All 9 TransformationProcessor implementations

**Proposed Solution:**

**Option A (Recommended):** Make keyTransition a method instead of constructor parameter
```scala
abstract class TransformationProcessor[...](using ct: ClassTag[OutputKey])
    extends SingleFactProcessor[OutputFact, OutputKey] {

  protected def getInputKey(outputKey: OutputKey): InputKey

  protected def generateSingleFact(requestedKey: OutputKey): CompilerIO[OutputFact] =
    getFactOrAbort(getInputKey(requestedKey)).flatMap(fact => generateFromKeyAndFact(requestedKey, fact))
}
```

**Option B:** Use a type class to derive the relationship (more complex, might not be worth it)

**Benefits:**
- Cleaner processor declarations (less visual noise)
- Method name `getInputKey` is more self-documenting than lambda
- Easier to debug (can set breakpoints in method)

**Estimated Lines Changed:** ~20 lines across 10 files

---

### 6. **REDUCE TransformationProcessor Type Parameter Noise** ⭐⭐
**Priority: LOW-MEDIUM**

**Current Problem:**
- TransformationProcessor has 4 type parameters: `[OutputFact, InputFact, InputKey, OutputKey]`
- Creates visual noise in processor declarations
- Some relationships could potentially be inferred

**Files Affected:**
- `eliotc/src/com/vanillasource/eliot/eliotc/processor/common/TransformationProcessor.scala`
- All 9 TransformationProcessor implementations

**Proposed Solution:**

**Option A:** Use type members instead of type parameters
```scala
abstract class TransformationProcessor[OutputFact <: CompilerFact, OutputKey <: CompilerFactKey[OutputFact]](using ClassTag[OutputKey])
    extends SingleFactProcessor[OutputFact, OutputKey] {

  type InputFact <: CompilerFact
  type InputKey <: CompilerFactKey[InputFact]

  // ...
}
```

**Option B:** Investigate if Scala 3's type inference can help reduce parameters

**Benefits:**
- Less visual noise in processor declarations
- Potentially clearer intent

**Drawbacks:**
- Might make type errors harder to understand
- May not be worth the effort

**Estimated Lines Changed:** ~40 lines across 10 files

---

### 7. **CONSOLIDATE ModuleProcessor Responsibilities** ⭐⭐
**Priority: LOW-MEDIUM**

**Current Problem:**
- ModuleProcessor is the most complex processor (143 lines)
- Handles both ModuleFunction.Key and ModuleData.Key
- Builds both function and type dictionaries
- Extracts imports and validates shadowing
- High cyclomatic complexity in importModuleFunctions and importModuleTypes (nearly identical)

**Files Affected:**
- `base/src/com/vanillasource/eliot/eliotc/module/processor/ModuleProcessor.scala`

**Proposed Solution:**

**Option A:** Extract dictionary building into separate utility
- Create `ModuleDictionary` utility object
- Move `extractImportedFunctions`, `extractImportedTypes` to utility
- Reduce ModuleProcessor to orchestration only

**Option B:** Split into ModuleFunctionProcessor and ModuleDataProcessor
- Each handles one key type
- Share common import/dictionary logic via utility

**Benefits:**
- Smaller, more focused processor
- Easier to test dictionary building in isolation
- Lower cyclomatic complexity

**Drawbacks:**
- Function and data dictionaries are built together for a reason (they're interdependent)
- Splitting might not be practical

**Estimated Lines Changed:** ~60 lines (extract utility, simplify processor)

---

### 8. **ADD Comprehensive Processor Documentation** ⭐⭐⭐⭐
**Priority: HIGH (but not code change)**

**Current Problem:**
- No clear guide on when to use which processor base class
- New contributors must read implementations to understand patterns
- Decision tree for choosing base class is implicit

**Files Affected:**
- All processor base classes

**Proposed Solution:**
Add comprehensive Scaladoc to each base class with:

1. **CompilerProcessor** - "When to implement directly (rarely, prefer subclasses)"
2. **SingleKeyTypeProcessor** - "When you handle one key type but need custom fact generation logic"
3. **SingleFactProcessor** - "When you generate one fact per key (idempotent)"
4. **TransformationProcessor** - "When you transform one fact type to another (most common)"
5. **SequentialCompilerProcessors** - "When you need to run multiple processors in sequence"

Include decision tree:
```
Do you transform one fact to another?
  YES -> Use TransformationProcessor
  NO -> Do you generate exactly one fact per key?
    YES -> Use SingleFactProcessor
    NO -> Do you handle one key type?
      YES -> Use SingleKeyTypeProcessor
      NO -> Implement CompilerProcessor directly
```

**Benefits:**
- Much easier for new contributors
- Self-documenting architecture
- Reduces questions and mistakes

**Estimated Lines Changed:** ~100 lines of documentation

---

### 9. **RENAME registerFactIfClear** ⭐
**Priority: LOW**

**Current Problem:**
- Name `registerFactIfClear` is slightly unclear
- "Clear" could mean "empty" or "error-free"
- The actual meaning is "register fact only if no errors have occurred"

**Files Affected:**
- `eliotc/src/com/vanillasource/eliot/eliotc/processor/CompilerIO.scala`
- All processors that use it

**Proposed Solution:**

**Option A:** Rename to `registerFactIfNoErrors`
**Option B:** Rename to `registerFactIfValid`
**Option C:** Keep as-is (it's not that bad)

**Benefits:**
- Slightly clearer intent
- Self-documenting code

**Drawbacks:**
- Renaming is high churn for low benefit
- Current name is acceptable

**Estimated Lines Changed:** ~15 references across codebase

---

### 10. **STANDARDIZE StateT Usage Pattern** ⭐⭐
**Priority: LOW**

**Current Problem:**
- FunctionResolver and TypeResolver use StateT for stateful transformations
- Pattern is correct but could have utility methods to reduce boilerplate
- Other processors might benefit from similar stateful patterns

**Files Affected:**
- `base/src/com/vanillasource/eliot/eliotc/resolve/processor/FunctionResolver.scala`
- `base/src/com/vanillasource/eliot/eliotc/resolve/processor/TypeResolver.scala`
- `base/src/com/vanillasource/eliot/eliotc/resolve/processor/ResolverScope.scala`

**Proposed Solution:**
Create base class or utilities for StateT-based processors:

```scala
abstract class StatefulTransformationProcessor[
  OutputFact, InputFact, InputKey, OutputKey, State
](keyTransition: OutputKey => InputKey)(using ClassTag[OutputKey])
  extends TransformationProcessor[OutputFact, InputFact, InputKey, OutputKey](keyTransition) {

  type StatefulIO[A] = StateT[CompilerIO, State, A]

  protected def initialState(key: OutputKey, fact: InputFact): State
  protected def transformWithState(key: OutputKey, fact: InputFact): StatefulIO[OutputFact]

  final override def generateFromKeyAndFact(key: OutputKey, fact: InputFact): CompilerIO[OutputFact] =
    transformWithState(key, fact).runA(initialState(key, fact))
}
```

**Benefits:**
- Makes stateful pattern explicit
- Easier to write new stateful processors
- Documents the pattern

**Drawbacks:**
- Only 2 processors use this pattern
- Might be over-engineering

**Estimated Lines Changed:** ~40 lines (new base class, adapt 2 processors)

---

## Recommended Implementation Order

1. **Proposal #1** - Merge TransformationProcessor dual methods (HIGHEST impact, low risk)
2. **Proposal #8** - Add documentation (HIGH impact, zero risk)
3. **Proposal #3** - Extract unification logic (HIGH impact, moderate risk)
4. **Proposal #2** - Standardize direct implementations (MEDIUM-HIGH impact, low risk)
5. **Proposal #4** - Remove NullProcessor (LOW impact if unused, zero risk)
6. **Proposal #5** - Simplify keyTransition (MEDIUM impact, low risk)
7. **Proposal #7** - Consolidate ModuleProcessor (MEDIUM impact, moderate risk)
8. **Proposal #6** - Reduce type parameters (LOW-MEDIUM impact, moderate risk)
9. **Proposal #10** - Standardize StateT (LOW impact, consider skipping)
10. **Proposal #9** - Rename registerFactIfClear (LOW impact, consider skipping)

---

## Summary Statistics

**Total Proposed Changes:**
- Files to modify: ~25
- Files to create: ~3
- Files to delete: ~1
- Estimated total lines changed: ~450
- Estimated reduction in code: ~150 lines
- Estimated reduction in cyclomatic complexity: ~30%

**Risk Assessment:**
- High impact, low risk: Proposals #1, #2, #4, #8
- High impact, moderate risk: Proposals #3
- Medium impact, low risk: Proposals #5
- Medium impact, moderate risk: Proposals #6, #7
- Low impact: Proposals #9, #10

**Expected Benefits:**
- 40% reduction in processor boilerplate
- 50% clearer processor contracts
- 30% reduction in "which base class should I use" questions
- Significant improvement in maintainability
