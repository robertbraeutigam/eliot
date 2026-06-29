package com.vanillasource.eliot.eliotc.module.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{ModuleNames, UnifiedModuleNames}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor
import com.vanillasource.eliot.eliotc.source.scan.PathScan

class UnifiedModuleNamesProcessor extends SingleFactProcessor[UnifiedModuleNames.Key] {

  override protected def generateSingleFact(key: UnifiedModuleNames.Key): CompilerIO[UnifiedModuleNames] =
    for {
      pathScan <- getFactOrAbort(PathScan.Key(key.moduleName.toPath, key.platform))
      allNames <- pathScan.files.traverse(uri => getFactOrAbort(ModuleNames.Key(uri)))
    } yield UnifiedModuleNames(key.moduleName, allNames.flatMap(_.names.value).toMap, key.platform)
}
