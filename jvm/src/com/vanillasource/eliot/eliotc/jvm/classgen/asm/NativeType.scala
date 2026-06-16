package com.vanillasource.eliot.eliotc.jvm.classgen.asm

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.module.fact.ModuleName
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import org.objectweb.asm.Type

trait NativeType {
  def javaClass: Class[?]
}

object NativeType {
  def systemLangType(typeName: String): ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, typeName), QualifiedName(typeName, Qualifier.Default))

  // JVM representation types live together in the `eliot.lang.Jvm` module (see `Jvm.els`), so unlike `systemLangType`
  // the module name is fixed (`Jvm`) and only the value name varies. `Qualifier.Default` matches the qualifier that
  // type FQNs carry by the time they reach this map (`CommonPatterns.stripDataTypeSuffix`).
  def jvmRepresentationType(typeName: String): ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, "Jvm"), QualifiedName(typeName, Qualifier.Default))

  val systemFunctionValue: ValueFQN = systemLangType("Function")
  val systemAnyValue: ValueFQN      = systemLangType("Any")
  val systemUnitValue: ValueFQN     = systemLangType("Unit")
  val systemTypeValue: ValueFQN     = systemLangType("Type")

  val types: Map[ValueFQN, NativeType] = Map.from(
    Seq(
      (systemLangType("String"), eliot_lang_String),
      (systemLangType("Function"), eliot_lang_Function),
      (systemLangType("Unit"), eliot_lang_Unit),
      (systemLangType("Any"), eliot_lang_Any),
      (systemLangType("BigInteger"), eliot_lang_BigInteger),
      // The fixed set of JVM representation types `Int[MIN, MAX]` can lower to (see `Jvm.els`). These five entries are
      // the entire backend "type knowledge" of integer widths; all width *policy* (which range picks which) lives in
      // Eliot's opaque `Int` body (Phase 2) and the unfold pass (Phase 3).
      (jvmRepresentationType("JvmByte"), eliot_lang_JvmByte),
      (jvmRepresentationType("JvmShort"), eliot_lang_JvmShort),
      (jvmRepresentationType("JvmInt"), eliot_lang_JvmInt),
      (jvmRepresentationType("JvmLong"), eliot_lang_JvmLong),
      (jvmRepresentationType("JvmBigInteger"), eliot_lang_JvmBigInteger),
      // `Int[MIN, MAX]` is still represented at runtime by a boxed `java.lang.Long` (everything maps to Long). This
      // mapping is removed in Phase 3, once the post-checking unfold pass lowers `Int` through its opaque body to one
      // of the `Jvm*` representation types above; until then `Int` must stay native-mapped or codegen breaks.
      (systemLangType("Int"), eliot_lang_Int)
    )
  )

  def javaSignatureName(vfqn: ValueFQN): String =
    types.get(vfqn).map(_.javaClass.descriptorString()).getOrElse(convertToJavaName(vfqn))

  def javaInternalName(vfqn: ValueFQN): String =
    types
      .get(vfqn)
      .map(cl => Type.getInternalName(cl.javaClass))
      .getOrElse(convertToNestedClassName(vfqn))

  private def convertToJavaName(vfqn: ValueFQN): String =
    // All data classes are nested classes inside the class denoted by the "module"!
    "L" + convertToNestedClassName(vfqn) + ";"

  def convertToMainClassName(moduleName: ModuleName): String =
    moduleName.packages.appended(moduleName.name).map(JvmIdentifier.encode(_).value).mkString("/")

  def convertToNestedClassName(vfqn: ValueFQN): String = {
    val prefix = vfqn.name.qualifier match {
      case Qualifier.Type => "type$"
      case _              => ""
    }
    convertToMainClassName(vfqn.moduleName) + "$" + prefix + JvmIdentifier.encode(vfqn.name.name).value
  }

  def convertToSignatureString(parameterTypes: Seq[ValueFQN], resultType: ValueFQN): String =
    s"(${parameterTypes.map(javaSignatureName).mkString})${javaSignatureName(resultType)}"

  def convertToCtorSignatureString(parameterTypes: Seq[ValueFQN]): String =
    s"(${parameterTypes.map(javaSignatureName).mkString})V"

  def convertToApplySignatureString(parameterTypes: Seq[ValueFQN], resultType: ValueFQN): String =
    s"(${parameterTypes.map(javaSignatureName).map(t => if (t === "V") "Ljava/lang/Void;" else t).mkString})${
        if (resultType === systemUnitValue) "Ljava/lang/Void;" else javaSignatureName(resultType)
      }"

  private def eliot_lang_String: NativeType = new NativeType {
    override def javaClass: Class[?] = classOf[java.lang.String]
  }

  // We compile Unit to Void not "void", because there's just too many random exceptions
  // that we can't (don't want to) handle. We assume that previous optimizations will mostly get
  // rid of these anyway.
  private def eliot_lang_Unit: NativeType = new NativeType {
    override def javaClass: Class[?] = classOf[java.lang.Void]
  }

  // TODO: This is not a "real" eliot type, just there to map to Object
  private def eliot_lang_Any: NativeType = new NativeType {
    override def javaClass: Class[?] = classOf[java.lang.Object]
  }

  private def eliot_lang_BigInteger: NativeType = new NativeType {
    override def javaClass: Class[?] = classOf[java.lang.Long]
  }

  private def eliot_lang_Int: NativeType = new NativeType {
    override def javaClass: Class[?] = classOf[java.lang.Long]
  }

  private def eliot_lang_JvmByte: NativeType = new NativeType {
    override def javaClass: Class[?] = classOf[java.lang.Byte]
  }

  private def eliot_lang_JvmShort: NativeType = new NativeType {
    override def javaClass: Class[?] = classOf[java.lang.Short]
  }

  private def eliot_lang_JvmInt: NativeType = new NativeType {
    override def javaClass: Class[?] = classOf[java.lang.Integer]
  }

  private def eliot_lang_JvmLong: NativeType = new NativeType {
    override def javaClass: Class[?] = classOf[java.lang.Long]
  }

  private def eliot_lang_JvmBigInteger: NativeType = new NativeType {
    override def javaClass: Class[?] = classOf[java.math.BigInteger]
  }

  private def eliot_lang_Function: NativeType = new NativeType {
    override def javaClass: Class[?] = classOf[java.util.function.Function[?, ?]]
  }
}
