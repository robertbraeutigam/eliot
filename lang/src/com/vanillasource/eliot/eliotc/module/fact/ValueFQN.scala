package com.vanillasource.eliot.eliotc.module.fact

import cats.kernel.Eq
import cats.Show
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}

case class ValueFQN(moduleName: ModuleName, name: QualifiedName)

object ValueFQN {
  given Show[ValueFQN] = vfqn => s"${vfqn.moduleName.show}::${vfqn.name.show}"

  given Eq[ValueFQN] = Eq.fromUniversalEquals

  val applyFQN: ValueFQN = ValueFQN(ModuleName.systemFunctionModuleName, QualifiedName("apply", Qualifier.Default))

  /** True when the FQN refers to a declaration inside an ability block whose name starts with an uppercase letter,
    * indicating an associated type rather than an abstract method. Abstract associated types have no runtime body; the
    * concrete value comes from the ability impl and is resolved post-drain.
    */
  def isAbstractAbilityType(fqn: ValueFQN): Boolean =
    fqn.name.qualifier match {
      case _: Qualifier.Ability => fqn.name.name.headOption.exists(_.isUpper)
      case _                    => false
    }
}
