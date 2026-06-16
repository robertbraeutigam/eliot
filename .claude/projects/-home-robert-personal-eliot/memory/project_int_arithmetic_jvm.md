---
name: project_int_arithmetic_jvm
description: Phase 5 done — Int +/-/* compute on JVM via inline intrinsics; alias-parser & negative-literal fixes
metadata:
  type: project
---

Phase 5 of [[project_renormalize_refires_natives]]'s plan is DONE: `Int`
`+`/`-`/`*` type-check with dependent bounds AND run on the JVM. `intToString(integerLiteral[2] +
integerLiteral[3] * integerLiteral[4])` prints `14`.

**JVM realisation = inline intrinsics, not generated methods.** `jvm`'s `Intrinsics` object lists the
well-known FQNs (`+`/`-`/`*`/`integerLiteral`/`intToString`); `ExpressionCodeGenerator.generateIntrinsic`
emits them inline (LADD/LSUB/LMUL with unbox/rebox; integerLiteral pushes its type-arg constant;
intToString = Long.toString), and `JvmClassGenerator.createModuleMethod` skips them. `Int`→`java.lang.Long`
in `NativeType.types`. Compile-time bound natives in `SystemNativesProcessor`: `add`/`subtract` (2-arg) and
`multiplyMin`/`multiplyMax` (4-arg corner-product min/max).

**Two latent bugs fixed (will bite again if reverted):**
- **Type-alias body must use `Expression.typeParser`, not `component[Expression]`** (`TypeAliasDefinition`).
  The greedy full parser eats the *next* definition's leading `infix`/`left`/… identifiers (not keywords),
  silently dropping its fixity. Surfaced when `+` (infix) followed the width aliases with no plain `def`
  between.
- **Negative integer literals**: `TokenParser.negativeIntegerLiteral` (atomic `-` glued to digits, before
  the symbol parser). Binary `a - b` therefore needs spaces. Needed for signed width aliases
  `type Long = Int[-9223…, …]`.

**`Int`/`Runtime` are NOT auto-imported** (not in `defaultSystemModules`) — doing so breaks every
`ProcessorTest` lacking a stub. Use `import eliot.lang.Int` / `eliot.lang.Runtime` explicitly until Phase 6
(literal desugar) makes them ambient with matching test stubs. `stdlib/Runtime.els` imports `Int`.
