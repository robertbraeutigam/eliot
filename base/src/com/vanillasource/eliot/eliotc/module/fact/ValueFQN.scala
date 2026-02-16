package com.vanillasource.eliot.eliotc.module.fact

import cats.kernel.Eq
import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.QualifiedName

case class ValueFQN(moduleName: ModuleName, name: QualifiedName)

object ValueFQN {
  given Show[ValueFQN] = vfqn => s"${vfqn.moduleName.show}::${vfqn.name.show}"

  given Eq[ValueFQN] = Eq.fromUniversalEquals
}
