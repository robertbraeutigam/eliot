package com.vanillasource.eliot.eliotc.module.fact

import cats.syntax.all.*
import cats.{Eq, Show}

case class TypeFQN(moduleName: ModuleName, typeName: String)

object TypeFQN {
  given Show[TypeFQN] = (tfqn: TypeFQN) => s"${tfqn.moduleName.show}.${tfqn.typeName}"

  given Eq[TypeFQN] = (self: TypeFQN, other: TypeFQN) => self == other
}
