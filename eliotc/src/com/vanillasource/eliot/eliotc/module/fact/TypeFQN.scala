package com.vanillasource.eliot.eliotc.module.fact

import cats.syntax.all.*
import cats.{Eq, Show}
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.systemFunctionModuleName

case class TypeFQN(moduleName: ModuleName, typeName: String)

object TypeFQN {
  given Show[TypeFQN] = (tfqn: TypeFQN) => s"${tfqn.moduleName.show}.${tfqn.typeName}"

  given Eq[TypeFQN] = (self: TypeFQN, other: TypeFQN) => self == other

  val systemFunctionType: TypeFQN = TypeFQN(systemFunctionModuleName, "Function")
}
