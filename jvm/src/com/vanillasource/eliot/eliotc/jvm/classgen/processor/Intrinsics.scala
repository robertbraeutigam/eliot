package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}

/** Backend intrinsics: abstract (body-less) stdlib `def`s that the JVM realises by emitting bytecode *inline at the
  * call site* rather than as a generated static method.
  *
  * Two of them cannot be ordinary [[NativeImplementation]] methods at all: `integerLiteral[V]` takes no value
  * parameters and must materialise the per-`V` constant from its (compile-time) type argument; and the arithmetic
  * operators are simplest as a single inline `LADD`/`LSUB`/`LMUL` (with unbox/rebox) — a generated method would have
  * to match the call-site name mangling exactly. So all of them are recognised here by their well-known FQN, emitted
  * inline by [[ExpressionCodeGenerator]], and excluded from `JvmClassGenerator`'s body-less "Function not implemented."
  * method generation. See `docs/int-min-max-plan.md` ("Phase 5 — Runtime arithmetic").
  */
object Intrinsics {

  private def langInt(name: String): ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "Int"), QualifiedName(name, Qualifier.Default))

  private def langRuntime(name: String): ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "Runtime"), QualifiedName(name, Qualifier.Default))

  /** `integerLiteral[V]: Int[V, V]` — materialises the constant `V` (from its type argument) as a boxed `Long`. */
  val integerLiteralFQN: ValueFQN = langRuntime("integerLiteral")

  /** The dependent-bounds arithmetic operators on `Int`, realised as `LADD`/`LSUB`/`LMUL` on `Long`. */
  val plusFQN: ValueFQN  = langInt("+")
  val minusFQN: ValueFQN = langInt("-")
  val timesFQN: ValueFQN = langInt("*")

  /** `intToString(value): String` — realised as `Long.toString`. */
  val intToStringFQN: ValueFQN = langInt("intToString")

  val all: Set[ValueFQN] = Set(integerLiteralFQN, plusFQN, minusFQN, timesFQN, intToStringFQN)

  def isIntrinsic(vfqn: ValueFQN): Boolean = all.contains(vfqn)
}
