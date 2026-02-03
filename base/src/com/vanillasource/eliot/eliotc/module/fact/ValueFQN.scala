package com.vanillasource.eliot.eliotc.module.fact

import cats.kernel.Eq
import cats.Show
import cats.syntax.all.*

case class ValueFQN(moduleName: ModuleName, name: String)

object ValueFQN {
  given Show[ValueFQN] = vfqn => s"${vfqn.moduleName.show}::${vfqn.name}"

  given Eq[ValueFQN] = Eq.fromUniversalEquals
}
