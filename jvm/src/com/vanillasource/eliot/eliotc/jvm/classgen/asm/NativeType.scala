package com.vanillasource.eliot.eliotc.jvm.classgen.asm

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module2.fact.ModuleName
import com.vanillasource.eliot.eliotc.module2.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.module2.fact.ValueFQN
import org.objectweb.asm.Type

trait NativeType {
  def javaClass: Class[?]
}

object NativeType {
  def systemLangValue(typeName: String): ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, typeName), typeName)

  val systemFunctionValue: ValueFQN = systemLangValue("Function")
  val systemAnyValue: ValueFQN      = systemLangValue("Any")
  val systemUnitValue: ValueFQN     = systemLangValue("Unit")

  val types: Map[ValueFQN, NativeType] = Map.from(
    Seq(
      (systemLangValue("String"), eliot_lang_String),
      (systemLangValue("Function"), eliot_lang_Function),
      (systemLangValue("Unit"), eliot_lang_Unit),
      (systemLangValue("Any"), eliot_lang_Any)
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
    moduleName.packages.appended(moduleName.name).mkString("/")

  def convertToNestedClassName(vfqn: ValueFQN): String =
    convertToMainClassName(vfqn.moduleName) + "$" + vfqn.name

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

  private def eliot_lang_Function: NativeType = new NativeType {
    override def javaClass: Class[?] = classOf[java.util.function.Function[?, ?]]
  }
}
