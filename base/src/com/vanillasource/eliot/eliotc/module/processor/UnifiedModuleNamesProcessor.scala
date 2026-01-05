package com.vanillasource.eliot.eliotc.module.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ModuleNames, UnifiedModuleNames}
import com.vanillasource.eliot.eliotc.module.processor.ExtractSymbols.pathName
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor
import com.vanillasource.eliot.eliotc.source.scan.PathScan

class UnifiedModuleNamesProcessor extends SingleFactProcessor[UnifiedModuleNames.Key] with Logging {
  override protected def generateSingleFact(key: UnifiedModuleNames.Key): CompilerIO[UnifiedModuleNames] =
    for {
      pathScan <- getFactOrAbort(PathScan.Key(pathName(key.moduleName)))
      allNames <- pathScan.files.traverse(file => getFactOrAbort(ModuleNames.Key(file)))
    } yield UnifiedModuleNames(
      key.moduleName,
      allNames.map(_.functionNames).flatten.toSet,
      allNames.map(_.typeNames).flatten.toSet
    )
}
