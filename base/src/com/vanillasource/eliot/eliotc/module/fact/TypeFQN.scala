package com.vanillasource.eliot.eliotc.module.fact

import cats.syntax.all.*
import cats.{Eq, Show}
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.{defaultSystemPackage, systemFunctionModuleName}

case class TypeFQN(moduleName: ModuleName, typeName: String)

object TypeFQN {
  given fullyQualified: Show[TypeFQN] = (tfqn: TypeFQN) => s"${tfqn.moduleName.show}.${tfqn.typeName}"

  given unqualified: Show[TypeFQN] = (tfqn: TypeFQN) => tfqn.typeName

  given Eq[TypeFQN] = (self: TypeFQN, other: TypeFQN) => self == other

  def systemLangType(typeName: String): TypeFQN =
    TypeFQN(ModuleName(defaultSystemPackage, typeName), typeName)

  val systemFunctionType: TypeFQN = systemLangType("Function")

  val systemAnyType: TypeFQN = systemLangType("Any")

  val systemUnitType: TypeFQN = systemLangType("Unit")
}
