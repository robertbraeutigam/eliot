package com.vanillasource.eliot.eliotc.module

import cats.Show
import cats.syntax.all.*

case class TypeFQN(moduleName: ModuleName, typeName: String)

object TypeFQN {
  given Show[TypeFQN] = (tfqn: TypeFQN) => s"${tfqn.moduleName.show}.${tfqn.typeName}"
}
