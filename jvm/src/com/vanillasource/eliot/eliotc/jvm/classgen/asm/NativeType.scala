package com.vanillasource.eliot.eliotc.jvm.classgen.asm

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.module.fact.TypeFQN.{systemLangType, systemUnitType}
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, TypeFQN}

trait NativeType {
  def javaSignatureName: String
}

object NativeType {
  val types: Map[TypeFQN, NativeType] = Map.from(
    Seq(
      (systemLangType("String"), eliot_lang_String),
      (systemLangType("Function"), eliot_lang_Function),
      (systemLangType("Unit"), eliot_lang_Unit),
      (TypeFQN(ModuleName(Seq("eliot", "java", "lang"), "Array"), "Array"), eliot_java_lang_Array)
    )
  )

  def javaSignatureName(typeFqn: TypeFQN): String =
    types.get(typeFqn).map(_.javaSignatureName).getOrElse(convertToJavaName(typeFqn))

  private def convertToJavaName(typeFQN: TypeFQN): String =
    // All data classes are nested classes inside the class denoted by the "module"!
    "L" + convertToNestedClassName(typeFQN) + ";"

  def convertToMainClassName(moduleName: ModuleName): String =
    moduleName.packages.appended(moduleName.name).mkString("/")

  def convertToNestedClassName(typeFQN: TypeFQN): String =
    convertToMainClassName(typeFQN.moduleName) + "$" + typeFQN.typeName

  def convertToSignatureString(parameterTypes: Seq[TypeFQN], resultType: TypeFQN): String =
    s"(${parameterTypes.map(javaSignatureName).mkString})${javaSignatureName(resultType)}"

  def convertToApplySignatureString(parameterTypes: Seq[TypeFQN], resultType: TypeFQN): String =
    s"(${parameterTypes.map(javaSignatureName).map(t => if (t === "V") "Ljava/lang/Void;" else t).mkString})${
        if (resultType === systemUnitType) "Ljava/lang/Void;" else javaSignatureName(resultType)
      }"

  private def eliot_java_lang_Array: NativeType = new NativeType {
    override def javaSignatureName: String = "[Ljava/lang/Object;"
  }

  private def eliot_lang_String: NativeType = new NativeType {
    override def javaSignatureName: String = "Ljava/lang/String;"
  }

  private def eliot_lang_Unit: NativeType = new NativeType {
    override def javaSignatureName: String = "V"
  }

  private def eliot_lang_Function: NativeType = new NativeType {
    override def javaSignatureName: String = "Ljava/util/function/Function;"
  }
}
