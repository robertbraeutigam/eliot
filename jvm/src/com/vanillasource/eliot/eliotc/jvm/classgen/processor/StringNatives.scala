package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType.systemLangType
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.{ClassGenerator, JvmIdentifier}
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, QualifiedName, Qualifier, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.CompilerIO
import org.objectweb.asm.Opcodes.*
import org.objectweb.asm.{Label, MethodVisitor}

/** The JVM leaf natives behind the `eliot.lang.String` operations, every one of them a direct delegation to the
  * corresponding `java.lang.String` method.
  *
  * All of them are pure and fixed-signature, so each is a plain exact-FQN native method (the `Eq`/`Compare`/`Combine`
  * leaves are the ability-implementation variant, emitted under the impl method's mangled name). Each has a
  * compile-time twin in `stdlib`'s `StringReductions`, which must compute *the same* answer: the pair is what lets a
  * string operation run indifferently on the compiler track (folding a `where` guard, an `auto` bound) and at runtime.
  * Two details keep the two in step and are load-bearing rather than stylistic:
  *
  *   - '''case conversion is locale-independent''' (`Locale.ROOT`, not the default-locale `toUpperCase()`), so a build
  *     machine's locale can never make the compile-time answer differ from the runtime one;
  *   - '''the index-taking operations are total''' — `substring` clamps both indices into `0..length` (raising `end` to
  *     `start` when inverted) and `repeat` clamps its count at zero, so neither can throw where the compile-time twin
  *     would happily produce a value. `indexOfInternal` keeps Java's negative "not found" index, which the `private`
  *     Eliot leaf never exposes: `indexOf` turns it into an `{Abort}`.
  *
  * `Int` parameters and returns cross the ⊤/bignum boundary as `java.math.BigInteger` (see `NativeType`), so an index
  * arrives at full precision and is narrowed to a machine `int` only *after* clamping — a wildly out-of-range index
  * cannot wrap around into a valid one.
  */
object StringNatives {

  private def stringFqn(name: String): ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "String"), QualifiedName(name, Qualifier.Default))

  private val stringType: ValueFQN = systemLangType("String")
  private val boolType: ValueFQN   = systemLangType("Bool")
  // The `eliot.lang.Int` type, whose ⊤/boundary representation is `java.math.BigInteger` (`NativeType.types`).
  private val intType: ValueFQN    = systemLangType("Int")

  private val JString     = "java/lang/String"
  private val JBoolean    = "java/lang/Boolean"
  private val JObject     = "java/lang/Object"
  private val JBigInteger = "java/math/BigInteger"
  private val JLocale     = "java/util/Locale"
  private val JCharSeq    = "java/lang/CharSequence"

  /** The leaf natives keyed by their `Default`-qualified FQN, folded into [[NativeImplementation.implementations]]. */
  val implementations: Seq[(ValueFQN, NativeImplementation)] = Seq(
    entry("length", Seq(stringType), intType)(length),
    entry("isEmpty", Seq(stringType), boolType)(isEmpty),
    entry("isBlank", Seq(stringType), boolType)(isBlank),
    entry("startsWith", Seq(stringType, stringType), boolType)(startsWith),
    entry("endsWith", Seq(stringType, stringType), boolType)(endsWith),
    entry("contains", Seq(stringType, stringType), boolType)(contains),
    entry("indexOfInternal", Seq(stringType, stringType), intType)(indexOf),
    entry("substring", Seq(intType, intType, stringType), stringType)(substring),
    entry("trim", Seq(stringType), stringType)(trim),
    entry("toUpperCase", Seq(stringType), stringType)(toUpperCase),
    entry("toLowerCase", Seq(stringType), stringType)(toLowerCase),
    entry("replace", Seq(stringType, stringType, stringType), stringType)(replace),
    entry("repeat", Seq(intType, stringType), stringType)(repeat)
  )

  /** The runtime ability-impl natives, folded into [[NativeImplementation.abilityImplementations]]: the three `String`
    * instances whose single method *is* a `java.lang.String` call.
    */
  val abilityImplementations: Seq[(String, String, String, JvmIdentifier => NativeImplementation)] = Seq(
    ("Eq", "equals", "String", implEntry(Seq(stringType, stringType), boolType)(equals)),
    ("Compare", "lessThanOrEqual", "String", implEntry(Seq(stringType, stringType), boolType)(lessThanOrEqual)),
    ("Combine", "combine", "String", implEntry(Seq(stringType, stringType), stringType)(combine))
  )

  /** A fixed-signature native emitted under its own simple name (the name every call site mangles to, since these
    * leaves are non-generic and `Default`-qualified).
    */
  private def entry(name: String, params: Seq[ValueFQN], ret: ValueFQN)(
      body: MethodVisitor => Unit
  ): (ValueFQN, NativeImplementation) =
    stringFqn(name) -> new NativeImplementation {
      override def generateMethod(cg: ClassGenerator): CompilerIO[Unit] =
        cg.createMethod[CompilerIO](JvmIdentifier(name), params, ret).use(_.runNative(body))
    }

  /** A native attached directly to an ability-implementation method: same shape as [[entry]], except the method name is
    * the impl method's *mangled* name, supplied per instantiation by the class generator.
    */
  private def implEntry(params: Seq[ValueFQN], ret: ValueFQN)(
      body: MethodVisitor => Unit
  ): JvmIdentifier => NativeImplementation = methodName =>
    new NativeImplementation {
      override def generateMethod(cg: ClassGenerator): CompilerIO[Unit] =
        cg.createMethod[CompilerIO](methodName, params, ret).use(_.runNative(body))
    }

  // --------------------------------------------------------------------------------------------------------------
  // Measures and predicates
  // --------------------------------------------------------------------------------------------------------------

  private def length(mv: MethodVisitor): Unit = {
    mv.visitVarInsn(ALOAD, 0)
    mv.visitMethodInsn(INVOKEVIRTUAL, JString, "length", "()I", false)
    intToBigInteger(mv)
  }

  private def isEmpty(mv: MethodVisitor): Unit = {
    mv.visitVarInsn(ALOAD, 0)
    mv.visitMethodInsn(INVOKEVIRTUAL, JString, "isEmpty", "()Z", false)
    boxBool(mv)
  }

  private def isBlank(mv: MethodVisitor): Unit = {
    mv.visitVarInsn(ALOAD, 0)
    mv.visitMethodInsn(INVOKEVIRTUAL, JString, "isBlank", "()Z", false)
    boxBool(mv)
  }

  private def startsWith(mv: MethodVisitor): Unit = {
    mv.visitVarInsn(ALOAD, 1)                                        // the subject is the last parameter
    mv.visitVarInsn(ALOAD, 0)                                        // the prefix
    mv.visitMethodInsn(INVOKEVIRTUAL, JString, "startsWith", s"(L$JString;)Z", false)
    boxBool(mv)
  }

  private def endsWith(mv: MethodVisitor): Unit = {
    mv.visitVarInsn(ALOAD, 1)
    mv.visitVarInsn(ALOAD, 0)
    mv.visitMethodInsn(INVOKEVIRTUAL, JString, "endsWith", s"(L$JString;)Z", false)
    boxBool(mv)
  }

  private def contains(mv: MethodVisitor): Unit = {
    mv.visitVarInsn(ALOAD, 1)
    mv.visitVarInsn(ALOAD, 0)
    mv.visitMethodInsn(INVOKEVIRTUAL, JString, "contains", s"(L$JCharSeq;)Z", false)
    boxBool(mv)
  }

  private def indexOf(mv: MethodVisitor): Unit = {
    mv.visitVarInsn(ALOAD, 1)
    mv.visitVarInsn(ALOAD, 0)
    mv.visitMethodInsn(INVOKEVIRTUAL, JString, "indexOf", s"(L$JString;)I", false)
    intToBigInteger(mv)
  }

  // --------------------------------------------------------------------------------------------------------------
  // Transformations
  // --------------------------------------------------------------------------------------------------------------

  /** `substring(start, end, s)`, made total: `start` and `end` are clamped into `0..s.length()` in `BigInteger`
    * arithmetic (so no index can wrap while being narrowed) and `end` is raised to `start` when it would precede it.
    */
  private def substring(mv: MethodVisitor): Unit = {
    val n      = 3
    val nBig   = 4
    val lo     = 5
    val hi     = 6
    val ordered = new Label()
    mv.visitVarInsn(ALOAD, 2)
    mv.visitMethodInsn(INVOKEVIRTUAL, JString, "length", "()I", false)
    mv.visitVarInsn(ISTORE, n)
    mv.visitVarInsn(ILOAD, n)
    intToBigInteger(mv)
    mv.visitVarInsn(ASTORE, nBig)
    clampIndex(mv, 0, nBig)
    mv.visitVarInsn(ISTORE, lo)
    clampIndex(mv, 1, nBig)
    mv.visitVarInsn(ISTORE, hi)
    mv.visitVarInsn(ILOAD, hi)
    mv.visitVarInsn(ILOAD, lo)
    mv.visitJumpInsn(IF_ICMPGE, ordered)
    mv.visitVarInsn(ILOAD, lo)
    mv.visitVarInsn(ISTORE, hi)                                      // an inverted range collapses to empty
    mv.visitLabel(ordered)
    mv.visitVarInsn(ALOAD, 2)
    mv.visitVarInsn(ILOAD, lo)
    mv.visitVarInsn(ILOAD, hi)
    mv.visitMethodInsn(INVOKEVIRTUAL, JString, "substring", s"(II)L$JString;", false)
  }

  private def trim(mv: MethodVisitor): Unit = {
    mv.visitVarInsn(ALOAD, 0)
    mv.visitMethodInsn(INVOKEVIRTUAL, JString, "strip", s"()L$JString;", false)
  }

  private def toUpperCase(mv: MethodVisitor): Unit = {
    mv.visitVarInsn(ALOAD, 0)
    mv.visitFieldInsn(GETSTATIC, JLocale, "ROOT", s"L$JLocale;")
    mv.visitMethodInsn(INVOKEVIRTUAL, JString, "toUpperCase", s"(L$JLocale;)L$JString;", false)
  }

  private def toLowerCase(mv: MethodVisitor): Unit = {
    mv.visitVarInsn(ALOAD, 0)
    mv.visitFieldInsn(GETSTATIC, JLocale, "ROOT", s"L$JLocale;")
    mv.visitMethodInsn(INVOKEVIRTUAL, JString, "toLowerCase", s"(L$JLocale;)L$JString;", false)
  }

  private def replace(mv: MethodVisitor): Unit = {
    mv.visitVarInsn(ALOAD, 2)
    mv.visitVarInsn(ALOAD, 0)
    mv.visitVarInsn(ALOAD, 1)
    mv.visitMethodInsn(INVOKEVIRTUAL, JString, "replace", s"(L$JCharSeq;L$JCharSeq;)L$JString;", false)
  }

  /** `repeat(count, s)`, made total: `count` is clamped into `0..Integer.MAX_VALUE` before narrowing, so a negative
    * count yields `""` instead of Java's exception.
    */
  private def repeat(mv: MethodVisitor): Unit = {
    mv.visitVarInsn(ALOAD, 1)
    mv.visitVarInsn(ALOAD, 0)
    mv.visitFieldInsn(GETSTATIC, JBigInteger, "ZERO", s"L$JBigInteger;")
    mv.visitMethodInsn(INVOKEVIRTUAL, JBigInteger, "max", s"(L$JBigInteger;)L$JBigInteger;", false)
    mv.visitLdcInsn(java.lang.Long.valueOf(Integer.MAX_VALUE.toLong))
    mv.visitMethodInsn(INVOKESTATIC, JBigInteger, "valueOf", s"(J)L$JBigInteger;", false)
    mv.visitMethodInsn(INVOKEVIRTUAL, JBigInteger, "min", s"(L$JBigInteger;)L$JBigInteger;", false)
    mv.visitMethodInsn(INVOKEVIRTUAL, JBigInteger, "intValue", "()I", false)
    mv.visitMethodInsn(INVOKEVIRTUAL, JString, "repeat", s"(I)L$JString;", false)
  }

  // --------------------------------------------------------------------------------------------------------------
  // Ability-impl leaves
  // --------------------------------------------------------------------------------------------------------------

  private def equals(mv: MethodVisitor): Unit = {
    mv.visitVarInsn(ALOAD, 0)
    mv.visitVarInsn(ALOAD, 1)
    mv.visitMethodInsn(INVOKEVIRTUAL, JString, "equals", s"(L$JObject;)Z", false)
    boxBool(mv)
  }

  /** `Compare[String]::lessThanOrEqual` — `a.compareTo(b) <= 0`, i.e. the lexicographic order of `java.lang.String`. */
  private def lessThanOrEqual(mv: MethodVisitor): Unit = {
    val whenFalse = new Label()
    val end       = new Label()
    mv.visitVarInsn(ALOAD, 0)
    mv.visitVarInsn(ALOAD, 1)
    mv.visitMethodInsn(INVOKEVIRTUAL, JString, "compareTo", s"(L$JString;)I", false)
    mv.visitJumpInsn(IFGT, whenFalse)
    mv.visitFieldInsn(GETSTATIC, JBoolean, "TRUE", s"L$JBoolean;")
    mv.visitJumpInsn(GOTO, end)
    mv.visitLabel(whenFalse)
    mv.visitFieldInsn(GETSTATIC, JBoolean, "FALSE", s"L$JBoolean;")
    mv.visitLabel(end)
  }

  private def combine(mv: MethodVisitor): Unit = {
    mv.visitVarInsn(ALOAD, 0)
    mv.visitVarInsn(ALOAD, 1)
    mv.visitMethodInsn(INVOKEVIRTUAL, JString, "concat", s"(L$JString;)L$JString;", false)
  }

  // --------------------------------------------------------------------------------------------------------------
  // Shared fragments
  // --------------------------------------------------------------------------------------------------------------

  /** Box the `int` on the stack into the `java.math.BigInteger` an `Int` crosses a method boundary as. */
  private def intToBigInteger(mv: MethodVisitor): Unit = {
    mv.visitInsn(I2L)
    mv.visitMethodInsn(INVOKESTATIC, JBigInteger, "valueOf", s"(J)L$JBigInteger;", false)
  }

  private def boxBool(mv: MethodVisitor): Unit =
    mv.visitMethodInsn(INVOKESTATIC, JBoolean, "valueOf", s"(Z)L$JBoolean;", false)

  /** Leave `min(max(argument, 0), length)` on the stack as a machine `int`. The clamp runs in `BigInteger` and only the
    * already-in-range result is narrowed, so an index far outside `int` cannot wrap into a valid position.
    */
  private def clampIndex(mv: MethodVisitor, argumentSlot: Int, lengthSlot: Int): Unit = {
    mv.visitVarInsn(ALOAD, argumentSlot)
    mv.visitFieldInsn(GETSTATIC, JBigInteger, "ZERO", s"L$JBigInteger;")
    mv.visitMethodInsn(INVOKEVIRTUAL, JBigInteger, "max", s"(L$JBigInteger;)L$JBigInteger;", false)
    mv.visitVarInsn(ALOAD, lengthSlot)
    mv.visitMethodInsn(INVOKEVIRTUAL, JBigInteger, "min", s"(L$JBigInteger;)L$JBigInteger;", false)
    mv.visitMethodInsn(INVOKEVIRTUAL, JBigInteger, "intValue", "()I", false)
  }
}
