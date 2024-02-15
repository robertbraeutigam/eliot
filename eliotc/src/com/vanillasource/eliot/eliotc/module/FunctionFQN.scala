package com.vanillasource.eliot.eliotc.module

import cats.Show
import cats.syntax.all.*

case class FunctionFQN(moduleName: ModuleName, functionName: String)

object FunctionFQN {
  given Show[FunctionFQN] = (ffqn: FunctionFQN) => s"${ffqn.moduleName.show}.${ffqn.functionName}"
}
