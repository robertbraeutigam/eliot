package com.vanillasource.eliot.eliotc.module.fact

import cats.Show
import cats.kernel.Eq
import cats.syntax.all.*

case class FunctionFQN(moduleName: ModuleName, functionName: String)

object FunctionFQN {
  given Show[FunctionFQN] = (ffqn: FunctionFQN) => s"${ffqn.moduleName.show}.${ffqn.functionName}"

  given Eq[FunctionFQN] = Eq.fromUniversalEquals

}
