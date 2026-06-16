package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}

/** Backend intrinsics: body-less stdlib `def`s that the JVM realises by emitting bytecode *inline at the call site*
  * rather than as a generated static method. They are recognised here by their well-known FQN, emitted inline by
  * [[ExpressionCodeGenerator]], and excluded from `JvmClassGenerator`'s body-less "Function not implemented." method
  * generation.
  *
  * `intToString` renders via `Long.toString`. The arithmetic *leaves* (`nativeAdd…`/`nativeSubtract…`/
  * `nativeMultiply…`) and the representation converter `nativeWiden` are the Phase-4 platform primitives the
  * `+`/`-`/`*` width dispatch in `Int.els` resolves to; each is one fixed unbox/op/rebox instruction group, so it is
  * cheapest inline.
  *
  * Integer *literals* are NOT intrinsics: `integerLiteral[V]` is rewritten into a plain `MonomorphicExpression.
  * IntegerLiteral(V)` at the `lang` readback boundary (`PostDrainQuoter`), so it reaches codegen as an ordinary
  * integer-literal node (`ExpressionCodeGenerator.createExpressionCode`'s `IntegerLiteral` arm) and never as a call.
  */
object Intrinsics {

  private def langInt(name: String): ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "Int"), QualifiedName(name, Qualifier.Default))

  /** `intToString(value): String` — realised as `Long.toString`. */
  val intToStringFQN: ValueFQN = langInt("intToString")

  /** `nativeWiden(value): Int[…]` — value-preserving representation conversion (unbox the source wrapper, rebox at the
    * target wrapper). Used by the `Coerce[Int, Int]` instance and by the `+`/`-`/`*` width dispatch to equalize operand
    * representations and to narrow an over-wide leaf result back to the declared bound.
    */
  val nativeWidenFQN: ValueFQN = langInt("nativeWiden")

  /** The operand→result width suffixes naming the Phase-4 arithmetic leaves (see `Int.els`). */
  private val widthSuffixes: Seq[String] =
    Seq(
      "ByteToByte",
      "ByteToShort",
      "ShortToShort",
      "ShortToInt",
      "IntToInt",
      "IntToLong",
      "LongToLong",
      "LongToBigInteger",
      "BigIntegerToBigInteger"
    )

  /** The arithmetic leaf natives grouped by operator. Each takes both operands at a single representation (the dispatch
    * widens them first) and produces a result whose representation its name encodes, so a microcontroller backend can
    * pick width-specific instructions. On the JVM all leaves of one operator collapse to the same code — unbox the
    * operands, apply the operator in the common working representation, rebox at the result representation — so they are
    * grouped here only by operator.
    */
  val addLeaves: Set[ValueFQN]      = widthSuffixes.map(s => langInt("nativeAdd" + s)).toSet
  val subtractLeaves: Set[ValueFQN] = widthSuffixes.map(s => langInt("nativeSubtract" + s)).toSet
  val multiplyLeaves: Set[ValueFQN] = widthSuffixes.map(s => langInt("nativeMultiply" + s)).toSet

  val all: Set[ValueFQN] =
    Set(intToStringFQN, nativeWidenFQN) ++ addLeaves ++ subtractLeaves ++ multiplyLeaves

  def isIntrinsic(vfqn: ValueFQN): Boolean = all.contains(vfqn)
}
