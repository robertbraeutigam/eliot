package com.vanillasource.eliot.eliotc.module.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ModuleNames, UnifiedModuleNames}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleFactProcessor
import com.vanillasource.eliot.eliotc.source.scan.PathScan

import java.nio.file.{Path, Paths}

class UnifiedModuleNamesProcessor extends SingleFactProcessor[UnifiedModuleNames.Key] {

  override protected def generateSingleFact(key: UnifiedModuleNames.Key): CompilerIO[UnifiedModuleNames] =
    for {
      pathScan <- getFactOrAbort(PathScan.Key(pathName(key.moduleName)))
      allNames <- pathScan.files.traverse(uri => getFactOrAbort(ModuleNames.Key(uri)))
    } yield UnifiedModuleNames(key.moduleName, allNames.flatMap(_.names).toSet)

  private def pathName(name: ModuleName): Path =
    (name.packages ++ Seq(name.name + ".els")).foldLeft(Paths.get(""))(_ `resolve` _)
}
