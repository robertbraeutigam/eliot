package com.vanillasource.eliot.eliotc.module.fact

import cats.kernel.Eq
import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}

case class ValueFQN(moduleName: ModuleName, name: QualifiedName)

object ValueFQN {
  given Show[ValueFQN] = vfqn => s"${vfqn.moduleName.show}::${vfqn.name.show}"

  given Eq[ValueFQN] = Eq.fromUniversalEquals

  val applyFQN: ValueFQN = ValueFQN(ModuleName.systemFunctionModuleName, QualifiedName("apply", Qualifier.Default))
}
