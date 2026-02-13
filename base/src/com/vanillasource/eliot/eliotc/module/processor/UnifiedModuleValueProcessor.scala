package com.vanillasource.eliot.eliotc.module.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.NamedValue
import com.vanillasource.eliot.eliotc.module.fact.{ModuleNames, ModuleValue, UnifiedModuleValue, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError
import com.vanillasource.eliot.eliotc.source.scan.PathScan

import java.nio.file.{Path, Paths}

class UnifiedModuleValueProcessor extends SingleFactProcessor[UnifiedModuleValue.Key] {

  override protected def generateSingleFact(key: UnifiedModuleValue.Key): CompilerIO[UnifiedModuleValue] =
    for {
      pathScan     <- getFactOrAbort(PathScan.Key(pathName(key.vfqn.moduleName)))
      allNames     <- pathScan.files.traverse(file => getFactOrAbort(ModuleNames.Key(file)).map(file -> _))
      filesWithName = allNames.collect { case (file, names) if names.names.contains(key.vfqn.name) => file }
      allValues    <- filesWithName
                        .traverse(file => getFact(ModuleValue.Key(file, key.vfqn)))
                        .map(_.flatten)
      unifiedValue <- unifyValues(key.vfqn, allValues)
    } yield unifiedValue

  private def unifyValues(vfqn: ValueFQN, values: Seq[ModuleValue]): CompilerIO[UnifiedModuleValue] =
    if (values.isEmpty) {
      abort[UnifiedModuleValue]
    } else if (hasMoreImplementations(values)) {
      compilerError(values.head.namedValue.qualifiedName.as("Has multiple implementations.")) *> abort[
        UnifiedModuleValue
      ]
    } else if (!hasSameSignatures(values)) {
      compilerError(values.head.namedValue.qualifiedName.as("Has multiple different definitions.")) *> abort[
        UnifiedModuleValue
      ]
    } else {
      val implementedValue = values.find(_.namedValue.runtime.isDefined).getOrElse(values.head)

      UnifiedModuleValue(
        implementedValue.vfqn,
        implementedValue.dictionary,
        implementedValue.namedValue
      ).pure[CompilerIO]
    }

  private def hasMoreImplementations(values: Seq[ModuleValue]): Boolean =
    values.count(_.namedValue.runtime.isDefined) > 1

  private def hasSameSignatures(values: Seq[ModuleValue]): Boolean = {
    val first = values.head

    values.tail.forall(v => NamedValue.signatureEquality.eqv(first.namedValue, v.namedValue))
  }

  private def pathName(name: com.vanillasource.eliot.eliotc.module.fact.ModuleName): Path =
    (name.packages ++ Seq(name.name + ".els")).foldLeft(Paths.get(""))(_ `resolve` _)
}
