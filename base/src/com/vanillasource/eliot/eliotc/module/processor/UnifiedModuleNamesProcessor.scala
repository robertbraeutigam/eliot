package com.vanillasource.eliot.eliotc.module.processor

import cats.Monad
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ModuleNames, UnifiedModuleNames}
import com.vanillasource.eliot.eliotc.module.processor.ExtractSymbols.pathName
import com.vanillasource.eliot.eliotc.processor.{CompilerFactKey, CompilerProcessor}
import com.vanillasource.eliot.eliotc.source.scan.PathScan

class UnifiedModuleNamesProcessor extends CompilerProcessor with Logging {
  override def generate(factKey: CompilerFactKey[?]): CompilerIO[Unit] =
    factKey match {
      case UnifiedModuleNames.Key(moduleName) => unifyModules(moduleName)
      case _                                  => Monad[CompilerIO].unit
    }

  private def unifyModules(name: ModuleName): CompilerIO[Unit] =
    for {
      pathScan <- getFactOrAbort(PathScan.Key(pathName(name)))
      allNames <- pathScan.files.traverse(file => getFactOrAbort(ModuleNames.Key(file)))
      _        <- registerFactIfClear(
                    UnifiedModuleNames(
                      name,
                      allNames.map(_.functionNames).flatten.toSet,
                      allNames.map(_.typeNames).flatten.toSet
                    )
                  )
    } yield ()
}
