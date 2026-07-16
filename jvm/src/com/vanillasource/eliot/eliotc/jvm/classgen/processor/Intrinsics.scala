package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}

/** Backend intrinsics: body-less stdlib leaves that the JVM realises by emitting bytecode *inline at the call site*
  * rather than as a generated static method. They are recognised here by their well-known FQN (or, for the `Int`
  * ability-impl leaves, their ability-impl qualifier — [[numericIntArith]]/[[compareIntOrdering]]), emitted inline by
  * [[ExpressionCodeGenerator]], and excluded from `JvmClassGenerator`'s body-less "Function not implemented." method
  * generation.
  *
  * `intToString` renders via `Long.toString`. The width-agnostic arithmetic and ordering leaves *are* the `Int` ability
  * instance methods — the `Numeric[Int]` `add`/`subtract`/`multiply` and the `Compare[Int]` `lessThanOrEqual`, with no
  * separate `nativeAdd`/`intLessThanOrEqual` def — each one unbox/op/rebox instruction group whose width is read from the
  * operand/result representations, so it is cheapest inline.
  *
  * Integer *literals* are NOT intrinsics: `integerLiteral[V]` is rewritten into a plain `MonomorphicExpression.
  * IntegerLiteral(V)` at the `lang` readback boundary (`PostDrainQuoter`), so it reaches codegen as an ordinary
  * integer-literal node (`ExpressionCodeGenerator.createExpressionCode`'s `IntegerLiteral` arm) and never as a call.
  */
object Intrinsics {

  private def langInt(name: String): ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "Int"), QualifiedName(name, Qualifier.Default))

  private def langBool(name: String): ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "Bool"), QualifiedName(name, Qualifier.Default))

  /** The `Bool` primitives and operators, emitted inline over the `java.lang.Boolean` representation (see
    * [[NativeType]]): the nullary constants `true`/`false` (a `GETSTATIC` of `Boolean.TRUE`/`FALSE`), the eliminator
    * `fold(cond, whenTrue, whenFalse)` (a branch that emits only the taken arm), and the logical `&&`/`||`/`!`. These
    * are body-less in the base `Bool.els`; the compiler-side reductions (`SystemNativesProcessor`/`StdlibNativesProcessor`)
    * fold them away whenever every operand is a compile-time constant, so an inline emission is reached only for a
    * genuinely runtime `Bool` (one derived from an effect result, e.g. `readLine == "yes"`).
    */
  val boolTrueFQN: ValueFQN  = langBool("true")
  val boolFalseFQN: ValueFQN = langBool("false")
  val boolFoldFQN: ValueFQN  = langBool("fold")
  val boolAndFQN: ValueFQN   = langBool("&&")
  val boolOrFQN: ValueFQN    = langBool("||")
  val boolNotFQN: ValueFQN   = langBool("!")

  /** `intToString(value): String` — realised as `Long.toString`. */
  val intToStringFQN: ValueFQN = langInt("intToString")

  val boolOps: Set[ValueFQN] =
    Set(boolTrueFQN, boolFalseFQN, boolFoldFQN, boolAndFQN, boolOrFQN, boolNotFQN)

  val all: Set[ValueFQN] =
    Set(intToStringFQN) ++ boolOps

  /** Whether `vfqn` is the `Int`-module instance method `name` of ability `ability` — an `Int` ability-impl leaf carried
    * body-less *directly* on its instance in the base `Int.els` (no `nativeAdd`/`intLessThanOrEqual` indirection).
    * Recognised by the ability-impl qualifier rather than a plain name, since an instance method's FQN is `add#Numeric,Int`
    * (a [[Qualifier.AbilityImplementation]]), not a plain [[Qualifier.Default]]. Module `Int` has exactly one instance of
    * each such ability, so the pattern string need not be matched.
    */
  private def intAbilityImpl(vfqn: ValueFQN, ability: String, names: Set[String]): Boolean =
    vfqn.moduleName == ModuleName(defaultSystemPackage, "Int") &&
      names.contains(vfqn.name.name) &&
      (vfqn.name.qualifier match {
        case Qualifier.AbilityImplementation(`ability`, _) => true
        case _                                             => false
      })

  /** The `Numeric[Int]` arithmetic methods (`add`/`subtract`/`multiply`) — the width-agnostic arithmetic leaves. Each
    * takes its operands at whatever representation their bounds lowered to and produces a result at its own true-bound
    * representation; the emission reads those representations to pick the working machine type (`long` or `BigInteger`)
    * and instruction, so one leaf covers every width and a microcontroller backend would select width-specific
    * instructions from the same lowered representations.
    */
  def numericIntArith(vfqn: ValueFQN): Boolean =
    intAbilityImpl(vfqn, "Numeric", Set("add", "subtract", "multiply"))

  /** The `Compare[Int]` ordering method (`lessThanOrEqual`) — the ordering leaf. One leaf covers every width: the result
    * is a `Bool` (no result-width growth to dispatch on), and the emission picks its working representation (primitive
    * `long` via `LCMP`, or `BigInteger.compareTo`) from the operands' lowered representations.
    */
  def compareIntOrdering(vfqn: ValueFQN): Boolean =
    intAbilityImpl(vfqn, "Compare", Set("lessThanOrEqual"))

  def isIntrinsic(vfqn: ValueFQN): Boolean =
    all.contains(vfqn) || numericIntArith(vfqn) || compareIntOrdering(vfqn)
}
