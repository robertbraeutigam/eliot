package com.vanillasource.eliot.eliotc.jvm

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, TypeFQN}

trait NativeType {
  def javaSignatureName: String
}

object NativeType {
  val types: Map[TypeFQN, NativeType] = Map.from(
    Seq(
      (systemLangType("String", "String"), eliot_lang_String),
      (systemLangType("Unit", "Unit"), eliot_lang_Unit)
    )
  )

  def javaSignatureName(typeFqn: TypeFQN): String =
    types.get(typeFqn).map(_.javaSignatureName).getOrElse(convertToJavaName(typeFqn))

  private def convertToJavaName(typeFQN: TypeFQN): String =
    if (typeFQN.typeName === typeFQN.moduleName.name) {
      // Type called as module, so this should be "top level" type
      typeFQN.moduleName.packages.appended(typeFQN.typeName).mkString("L", "/", ";")
    } else {
      // Type should be a static internal type
      typeFQN.moduleName.packages.appended(typeFQN.moduleName.name).appended(typeFQN.typeName).mkString("L", "/", ";")
    }

  private def systemLangType(moduleName: String, typeName: String): TypeFQN =
    TypeFQN(ModuleName(defaultSystemPackage, moduleName), typeName)

  private def eliot_lang_String: NativeType = new NativeType {
    override def javaSignatureName: String = "Ljava/lang/String;"
  }

  private def eliot_lang_Unit: NativeType = new NativeType {
    override def javaSignatureName: String = "V"
  }
}
