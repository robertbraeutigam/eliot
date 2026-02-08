package com.vanillasource.eliot.eliotc.jvm.classgen.asm

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ModuleName
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import org.objectweb.asm.Type

trait NativeType {
  def javaClass: Class[?]
}

object NativeType {
  def systemLangType(typeName: String): ValueFQN =
    ValueFQN(ModuleName(defaultSystemPackage, typeName), typeName + "$DataType")

  val systemFunctionValue: ValueFQN = systemLangType("Function")
  val systemAnyValue: ValueFQN      = systemLangType("Any")
  val systemUnitValue: ValueFQN     = systemLangType("Unit")

  val types: Map[ValueFQN, NativeType] = Map.from(
    Seq(
      (systemLangType("String"), eliot_lang_String),
      (systemLangType("Function"), eliot_lang_Function),
      (systemLangType("Unit"), eliot_lang_Unit),
      (systemLangType("Any"), eliot_lang_Any)
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
