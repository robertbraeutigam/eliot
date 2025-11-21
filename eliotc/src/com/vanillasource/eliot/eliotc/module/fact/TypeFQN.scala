package com.vanillasource.eliot.eliotc.module.fact

import cats.syntax.all.*
import cats.{Eq, Show}
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.systemFunctionModuleName

case class TypeFQN(moduleName: ModuleName, typeName: String)

object TypeFQN {
  given fullyQualified: Show[TypeFQN] = (tfqn: TypeFQN) => s"${tfqn.moduleName.show}.${tfqn.typeName}"

  given unqualified: Show[TypeFQN] = (tfqn: TypeFQN) => tfqn.typeName

  given Eq[TypeFQN] = (self: TypeFQN, other: TypeFQN) => self == other

  val systemFunctionType: TypeFQN = TypeFQN(systemFunctionModuleName, "Function")

  val systemAnyType: TypeFQN = TypeFQN(systemFunctionModuleName, "Any")
}
