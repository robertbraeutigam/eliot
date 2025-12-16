package com.vanillasource.eliot.eliotc.jvm.asm

import com.vanillasource.eliot.eliotc.module.fact.TypeFQN.systemLangType
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, TypeFQN}

trait NativeType {
  def javaSignatureName: String
}

object NativeType {
  val types: Map[TypeFQN, NativeType] = Map.from(
    Seq(
      (systemLangType("String"), eliot_lang_String),
      (systemLangType("Function"), eliot_lang_Function),
      (systemLangType("Unit"), eliot_lang_Unit)
    )
  )

  def javaSignatureName(typeFqn: TypeFQN): String =
    types.get(typeFqn).map(_.javaSignatureName).getOrElse(convertToJavaName(typeFqn))

  def convertToJavaName(typeFQN: TypeFQN): String =
    // All data classes are nested classes inside the class denoted by the "module"!
    "L" + convertToNestedClassName(typeFQN) + ";"

  def convertToMainClassName(moduleName: ModuleName): String =
    moduleName.packages.appended(moduleName.name).mkString("/")

  def convertToNestedClassName(typeFQN: TypeFQN): String =
    convertToMainClassName(typeFQN.moduleName) + "$" + typeFQN.typeName

  def convertToSignatureString(signatureTypes: Seq[TypeFQN]): String =
    s"(${signatureTypes.init.map(javaSignatureName).mkString})${javaSignatureName(signatureTypes.last)}"

  private def eliot_lang_String: NativeType = new NativeType {
    override def javaSignatureName: String = "Ljava/lang/String;"
  }

  private def eliot_lang_Unit: NativeType = new NativeType {
    override def javaSignatureName: String = "V"
  }

  private def eliot_lang_Function: NativeType = new NativeType {
    override def javaSignatureName: String = "Ljava/lang/Function;"
  }
}
