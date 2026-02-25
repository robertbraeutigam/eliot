# Plan: Fixity Declarations and Operator Expression Parsing

## Context

ELIOT currently has no operator precedence or fixity system. All functions use prefix syntax (`f a b` or `f(a, b)`), and symbol tokens cannot be function names. This plan adds fixity declarations and a two-phase expression system: flat parsing at AST level, operator/application resolution in a dedicated processor.

**Key principle**: Operators ARE functions. Fixity is metadata on any named value. No fundamental differentiation.

## Defaults and Built-in Rules

- **Default fixity**: None (regular function, callable only via prefix `f a b` or parenthesized `f(a, b)`)
- **Default associativity**: `left` (when `infix` is declared without `left`/`right`/`none`)
- **Built-in precedence hierarchy** (strongest first): `postfix` > `prefix` > `infix` — no `above`/`below` needed to mix fixity types

## Syntax Examples

```eliot
def +(a: Int, b: Int): Int                          // symbol name, no fixity
prefix def !(a: Bool): Bool                         // explicit prefix (enables operator syntax)
infix left def +(a: Int, b: Int): Int               // infix left-associative
infix def or(a: Bool, b: Bool): Bool                // identifier infix, default left-assoc
infix right above(+, -) def **(a: Int, b: Int): Int // infix right, above + and -
postfix def ++(x: Int): Int                         // postfix

// Expressions:
def main: Bool = a or b           // infix identifier: or(a, b)
def main: Int = a + b * c         // precedence: a + (b * c)
def main: Int = !x + y            // prefix > infix: (!(x)) + y
def main: Int = x++ + y           // postfix > infix: (x++) + y
def main: Int = +(a, b)           // parenthesized call always works
```

## Core Design: Flat Expressions

Since both identifiers (`or`) and symbols (`+`) can be operators, the parser cannot determine roles at parse time. ALL multi-atom expressions are parsed as **flat sequences**:

```
a or b        → FlatExpression([a, or, b])
f a b         → FlatExpression([f, a, b])
f a + g b     → FlatExpression([f, a, +, g, b])
a + b * c     → FlatExpression([a, +, b, *, c])
```

Parenthesized calls are unambiguous and parsed immediately:
```
f(a, b)       → FunctionApplication(f, [a, b])  — single atom, no flat
f(a) + g(b)   → FlatExpression([f(a), +, g(b)])
```

The **OperatorResolverProcessor** (new, dedicated) resolves flat expressions by:
1. Looking up fixity for each name
2. Classifying atoms as operators or operands by fixity + position
3. Grouping consecutive operands into function applications (space-based)
4. Applying operator precedence to build nested `FunctionApplication`

## Implementation Steps

### Step 1: Tokenizer — Make `::` a hard operator

**File**: `lang/src/.../token/TokenParser.scala`
- Add `"::"` to `hardOperators` and `standaloneSymbolParser`
- Ensures `Module::+` tokenizes as `[Module, ::, +]` not `[Module, ::+]`

### Step 2: AST types for fixity and precedence

**New file**: `lang/src/.../ast/fact/Fixity.scala`
```scala
sealed trait Fixity
object Fixity {
  case object Prefix extends Fixity
  case class Infix(associativity: Associativity) extends Fixity
  case object Postfix extends Fixity
}
sealed trait Associativity
object Associativity {
  case object Left extends Associativity
  case object Right extends Associativity
  case object None extends Associativity
}
```

**New file**: `lang/src/.../ast/fact/PrecedenceDeclaration.scala`
```scala
case class PrecedenceDeclaration(relation: PrecedenceRelation, targets: Seq[Sourced[String]])
sealed trait PrecedenceRelation
object PrecedenceRelation {
  case object Above extends PrecedenceRelation
  case object Below extends PrecedenceRelation
  case object At extends PrecedenceRelation
}
```

### Step 3: Primitives helpers

**File**: `lang/src/.../ast/fact/Primitives.scala`

Add `isUserOperator` (symbols excluding structural: `( ) [ ] { } , -> _ :: : ~ & . =`) and `identifierWith(name)` for matching identifier content.

### Step 4: FunctionDefinition — fixity and symbol names

**File**: `lang/src/.../ast/fact/FunctionDefinition.scala`

1. Add `fixity: Option[Fixity]` and `precedence: Seq[PrecedenceDeclaration]` fields
2. Parse optional fixity before `def`, wrapped in `.atomic()`:
   - `prefix | infix [left|right|none] | postfix` (identifiers matched by content, not keywords)
   - Then `above|below|at name|(name, name, ...)` zero or more times
   - Then `def`
3. Function name: `acceptIfAll(isIdentifier, isLowerCase) or acceptIf(isUserOperator)`
4. Default infix associativity: `Left`

### Step 5: Expression parser — flat expressions

**File**: `lang/src/.../ast/fact/Expression.scala`

Add `FlatExpression(parts: Seq[Sourced[Expression]])` case.

Restructure the parser — **no more space-based application at AST level**:

```scala
override def parser = for {
  first <- sourced(atom)
  rest  <- sourced(atom).anyTimes()
} yield if (rest.isEmpty) first.value else FlatExpression(first +: rest)

// An atom is one indivisible expression unit
val atom = functionLiteral.atomic() or
           parenthesizedExpr.atomic() or
           namedRefOrCall or        // identifier/symbol with optional [typeArgs] and optional (args)
           integerLiteral or
           stringLiteral

// Named reference/call: [module::]name[typeArgs][(args)]
// Parenthesized args make it a FunctionApplication; otherwise bare reference
val namedRefOrCall = for {
  module   <- (moduleParser <* symbol("::")).atomic().optional()
  name     <- acceptIf(isIdentifierOrSymbol, "name")
  typeArgs <- optionalBracketedCommaSeparatedItems("[", component[TypeReference], "]")
  args     <- bracketedCommaSeparatedItems("(", sourced(parser), ")").optional()
} yield FunctionApplication(module, name, typeArgs, args.getOrElse(Seq.empty))

val parenthesizedExpr = parser.between(symbol("("), symbol(")"))
```

Key behaviors:
- `f a b` → `FlatExpression([f, a, b])` — `a` and `b` are separate atoms, NOT space args
- `f(a, b)` → `FunctionApplication(f, [a, b])` — parenthesized, single atom
- `(a + b) * c` → `FlatExpression([(a+b), *, c])` — grouped sub-expression
- `a or b` → `FlatExpression([a, or, b])` — identifier infix, resolved later
- Single atom `f` → just `FunctionApplication(f, [])` — no flat wrapper

### Step 6: Core phase changes

**New files**: `core/fact/Fixity.scala`, `core/fact/PrecedenceDeclaration.scala` — mirror AST types

**File**: `lang/src/.../core/fact/NamedValue.scala`
- Add `fixity: Option[Fixity]`, `precedence: Seq[PrecedenceDeclaration]`

**File**: `lang/src/.../core/fact/Expression.scala`
- Add `FlatExpression(parts: Seq[Sourced[TypeStack[Expression]]])` case
- Update `structuralEquality`, `Show`

**File**: `lang/src/.../core/processor/CoreProcessor.scala`
- `transformFunction`: copy fixity/precedence from AST to core NamedValue
- `toBodyExpression`: new case for `SourceExpression.FlatExpression` — convert each part to core expression, wrap in `TypeStack.of`

### Step 7: Resolve phase — pass through flat expressions

**File**: `lang/src/.../resolve/fact/Expression.scala`
- Add `FlatExpression(parts: Seq[Sourced[TypeStack[Expression]]])` case

**New files**: `resolve/fact/Fixity.scala`, `resolve/fact/PrecedenceDeclaration.scala` (uses `ValueFQN` for targets)

**File**: `lang/src/.../resolve/fact/ResolvedValue.scala`
- Add `fixity: Option[Fixity]`, `precedence: Seq[ResolvedPrecedenceDeclaration]`

**File**: `lang/src/.../resolve/processor/ValueResolver.scala`
- Pass through `FlatExpression`: resolve each part's sub-expressions but keep flat structure
- Resolve fixity/precedence on the value itself (convert precedence target names to ValueFQNs)

### Step 8: OperatorResolverProcessor (the bulk logic)

**New file**: `lang/src/.../resolve/fact/OperatorResolvedValue.scala`
- Same fields as `ResolvedValue` but guaranteed: no `FlatExpression` in runtime
- Key: `OperatorResolvedValue.Key(vfqn)`

**New file**: `lang/src/.../resolve/processor/OperatorResolverProcessor.scala`

```scala
class OperatorResolverProcessor
    extends TransformationProcessor[ResolvedValue.Key, OperatorResolvedValue.Key](
      key => ResolvedValue.Key(key.vfqn)
    )
```

**Resolution algorithm** for each `FlatExpression`:

1. **Look up fixity** for each atom that is a value/parameter reference

2. **Classify by position** (greedy left-to-right scan):
   - State `ExpectingOperand`:
     - Atom with `prefix` fixity → PrefixOp, stay in ExpectingOperand
     - Otherwise → Operand, switch to `ExpectingOperatorOrMore`
   - State `ExpectingOperatorOrMore`:
     - Atom with `infix` fixity → InfixOp, switch to ExpectingOperand
     - Atom with `postfix` fixity → PostfixOp, stay in ExpectingOperatorOrMore
     - Atom with `prefix` fixity → close operand group, PrefixOp, switch to ExpectingOperand
     - Atom with no fixity → accumulate into current operand group

3. **Group consecutive operands** into function applications (curried): `[f, a, b]` → `f(a)(b)`

4. **Apply prefix/postfix operators** (bind tighter than infix by default):
   - Postfix first (strongest): `Operand PostfixOp` → `FunctionApp(op, operand)`
   - Then prefix: `PrefixOp Operand` → `FunctionApp(op, operand)`

5. **Apply infix operators** with precedence-climbing:
   - Built-in: all infix operators are weaker than prefix/postfix
   - Within infix: use `above`/`below`/`at` declarations to determine order
   - `at` = same precedence level (must have same associativity)
   - `above`/`below` = strict ordering between levels
   - Validate: all pairs of infix operators in this expression have transitive ordering (error if not)
   - Infix `op` between `a` and `b`: `FunctionApp(FunctionApp(ValueRef(op), a), b)` (curried)
   - Left-assoc: `a + b + c` → `(a + b) + c`
   - Right-assoc: `a :: b :: c` → `a :: (b :: c)`
   - Non-assoc: `a == b == c` → error

**Register** in `LangPlugin.scala` after `ValueResolver()`.

### Step 9: Downstream processor updates (mechanical)

Change input key from `ResolvedValue.Key` to `OperatorResolvedValue.Key` in:

1. `symbolic/processor/SymbolicTypeCheckProcessor.scala` — TransformationProcessor input
2. `eval/processor/ExistingNamedValueEvaluator.scala` — TransformationProcessor input
3. `eval/processor/DataTypeEvaluator.scala` — TransformationProcessor input
4. `symbolic/processor/BodyTypeInferrer.scala` — `getFactOrAbort` call
5. `abilitycheck/AbilityCheckProcessor.scala` — `getFact` call

`OperatorResolvedValue` has the same fields as `ResolvedValue`, so access patterns don't change.

### Step 10: Verify remaining downstream phases

No changes needed to: monomorphize, implementation, used, uncurry, JVM. These consume `TypeCheckedValue`, `NamedEvaluable`, etc.

## Tests

**TokenizerTest**: `::` hard operator tokenization

**ASTParserTest**:
- `def +(a: Int, b: Int): Int = a` — symbol function name
- `infix left def +(a: Int, b: Int): Int = a` — fixity parsing
- `infix def or(a: Bool, b: Bool): Bool` — identifier infix, default left
- `def a: T = b + c` — flat expression with symbol op
- `def a: T = a or b` — flat expression with identifier op
- `def a: T = f a b` — flat expression (space app deferred)
- `def a: T = (a + b) * c` — parenthesized grouping
- `def a: T = f(a + b)` — flat inside paren call args

**CoreProcessorTest**: FlatExpression passes through, fixity on NamedValue

**OperatorResolverProcessorTest** (new):
- Space app: `f a b` → `f(a)(b)`
- Simple infix: `a + b` → `+(a, b)`
- Identifier infix: `a or b` → `or(a, b)`
- Precedence: `a + b * c` with `*` above `+` → `a + (b * c)`
- Left assoc: `a + b + c` → `(a + b) + c`
- Right assoc: `a :: b :: c` → `a :: (b :: c)`
- Built-in hierarchy: `! x + y` → `(! x) + y` (prefix > infix, no `above` needed)
- Built-in hierarchy: `x ++ + y` → `(x ++) + y` (postfix > infix)
- Mixed: `f a + g b` → `+(f(a), g(b))`
- Error: two infix ops without relative precedence
- Error: cycle in precedence
- Error: non-assoc chained: `a == b == c`
- Parenthesized call always works: `+(a, b)` regardless of fixity

## Known Limitations (v1)

1. **Same name can't have both prefix and infix fixity**: Use different names.
2. **No fixity = only prefix/parenthesized call**: Must declare fixity to use as operator.

## Verification

```bash
./mill __.compile
./mill lang.test
./mill __.test
```

## File Summary

### New files (~11)
- `lang/src/.../ast/fact/Fixity.scala`, `PrecedenceDeclaration.scala`
- `lang/src/.../core/fact/Fixity.scala`, `PrecedenceDeclaration.scala`
- `lang/src/.../resolve/fact/Fixity.scala`, `PrecedenceDeclaration.scala`
- `lang/src/.../resolve/fact/OperatorResolvedValue.scala`
- `lang/src/.../resolve/processor/OperatorResolverProcessor.scala`
- `lang/test/src/.../resolve/OperatorResolverProcessorTest.scala`

### Modified files (~12)
- `lang/src/.../token/TokenParser.scala` — `::` as hard operator
- `lang/src/.../ast/fact/Primitives.scala` — `isUserOperator`, `identifierWith`
- `lang/src/.../ast/fact/FunctionDefinition.scala` — fixity fields + parser
- `lang/src/.../ast/fact/Expression.scala` — FlatExpression + restructured parser
- `lang/src/.../core/fact/NamedValue.scala` — fixity fields
- `lang/src/.../core/fact/Expression.scala` — FlatExpression case
- `lang/src/.../core/processor/CoreProcessor.scala` — pass through fixity + flat exprs
- `lang/src/.../resolve/fact/Expression.scala` — FlatExpression case
- `lang/src/.../resolve/fact/ResolvedValue.scala` — fixity fields
- `lang/src/.../resolve/processor/ValueResolver.scala` — pass through flat + resolve fixity
- `lang/src/.../plugin/LangPlugin.scala` — register OperatorResolverProcessor

### Downstream mechanical updates (5)
- `symbolic/processor/SymbolicTypeCheckProcessor.scala`
- `eval/processor/ExistingNamedValueEvaluator.scala`
- `eval/processor/DataTypeEvaluator.scala`
- `symbolic/processor/BodyTypeInferrer.scala`
- `abilitycheck/AbilityCheckProcessor.scala`

### Test files to modify
- `lang/test/src/.../token/TokenizerTest.scala`
- `lang/test/src/.../ast/processor/ASTParserTest.scala`
- `lang/test/src/.../core/processor/CoreProcessorTest.scala`
