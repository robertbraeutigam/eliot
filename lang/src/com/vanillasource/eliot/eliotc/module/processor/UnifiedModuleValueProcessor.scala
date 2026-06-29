package com.vanillasource.eliot.eliotc.module.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.NamedValue
import com.vanillasource.eliot.eliotc.module.fact.{ModuleNames, ModuleValue, UnifiedModuleValue, ValueFQN}
import com.vanillasource.eliot.eliotc.platform.Platform
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced.{compilerError, compilerAbort}
import com.vanillasource.eliot.eliotc.source.scan.PathScan

class UnifiedModuleValueProcessor extends SingleFactProcessor[UnifiedModuleValue.Key] {

  override protected def generateSingleFact(key: UnifiedModuleValue.Key): CompilerIO[UnifiedModuleValue] =
    for {
      pathScan     <- getFactOrAbort(PathScan.Key(key.vfqn.moduleName.toPath, key.platform))
      allNames     <- pathScan.files.traverse(file => getFactOrAbort(ModuleNames.Key(file)).map(file -> _))
      filesWithName = allNames.collect { case (file, names) if names.names.value.contains(key.vfqn.name) => file }
      allValues    <- filesWithName.traverse(file => getFactOrAbort(ModuleValue.Key(file, key.vfqn, key.platform)))
      _            <- compilerAbort(allNames.head._2.names.as(s"Could not find '${key.vfqn.name.show}'."))
                        .whenA(allValues.isEmpty)
      unifiedValue <- unifyValues(key.vfqn, allValues, key.platform)
    } yield unifiedValue

  private def unifyValues(
      vfqn: ValueFQN,
      values: Seq[ModuleValue],
      platform: Platform
  ): CompilerIO[UnifiedModuleValue] =
    if (values.isEmpty) {
      abort
    } else if (hasMoreImplementations(values)) {
      compilerError(values.head.namedValue.qualifiedName.as("Has multiple implementations.")) *> abort
    } else if (!hasSameSignatures(values)) {
      compilerError(values.head.namedValue.qualifiedName.as("Has multiple different definitions.")) *> abort
    } else {
      val implementedValue = values.find(_.namedValue.runtime.isDefined).getOrElse(values.head)

      UnifiedModuleValue(
        implementedValue.vfqn,
        implementedValue.dictionary,
        implementedValue.namedValue,
        implementedValue.privateNames,
        platform
      ).pure[CompilerIO]
    }

  private def hasMoreImplementations(values: Seq[ModuleValue]): Boolean =
    values.count(_.namedValue.runtime.isDefined) > 1

  private def hasSameSignatures(values: Seq[ModuleValue]): Boolean = {
    val first = values.head

    values.tail.forall(v => NamedValue.signatureEquality.eqv(first.namedValue, v.namedValue))
  }
}
