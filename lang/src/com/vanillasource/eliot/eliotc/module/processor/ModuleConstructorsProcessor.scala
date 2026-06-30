package com.vanillasource.eliot.eliotc.module.processor

import cats.kernel.Order.catsKernelOrderingForOrder
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.RoleHint
import com.vanillasource.eliot.eliotc.module.fact.{
  ModuleConstructors,
  Qualifier,
  UnifiedModuleNames,
  UnifiedModuleValue,
  ValueFQN
}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor

/** Builds [[ModuleConstructors]] for a module: enumerates its `Qualifier.Default` upper-cased names, keeps those whose
  * [[UnifiedModuleValue]] carries a [[RoleHint.ValueConstructor]] hint, and groups them by their data type in source
  * declaration order. This is the one place that turns the raw [[UnifiedModuleNames]] name set into a constructor index.
  */
class ModuleConstructorsProcessor extends SingleFactProcessor[ModuleConstructors.Key] {

  override protected def generateSingleFact(key: ModuleConstructors.Key): CompilerIO[ModuleConstructors] =
    for {
      moduleNames     <- getFactOrAbort(UnifiedModuleNames.Key(key.moduleName, key.platform))
      constructorVfqns = moduleNames.names.keys
                           .filter(qn => qn.qualifier == Qualifier.Default && qn.name.head.isUpper)
                           .map(qn => ValueFQN(key.moduleName, qn))
                           .toSeq
      typeConstructors = moduleNames.names.keys
                           .filter(qn => qn.qualifier == Qualifier.Type && qn.name.head.isUpper)
                           .map(qn => ValueFQN(key.moduleName, qn))
                           .toSeq
      classified      <- constructorVfqns.traverseFilter { vfqn =>
                           getFactOrAbort(UnifiedModuleValue.Key(vfqn, key.platform)).map { umv =>
                             umv.namedValue.roleHint match {
                               case RoleHint.ValueConstructor(dataType, _) =>
                                 Some((dataType, vfqn, umv.namedValue.qualifiedName.range.from))
                               case _                                      => None
                             }
                           }
                         }
    } yield ModuleConstructors(
      key.moduleName,
      classified.groupBy(_._1).view.mapValues(_.sortBy(_._3).map(_._2)).toMap,
      typeConstructors,
      key.platform
    )
}
