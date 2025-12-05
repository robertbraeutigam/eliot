package com.vanillasource.eliot.eliotc.module.processor

import cats.effect.IO
import com.vanillasource.eliot.eliotc.CompilationProcess
import com.vanillasource.eliot.eliotc.module.fact.ModuleNames
import com.vanillasource.eliot.eliotc.module.processor.ExtractSymbols.*
import com.vanillasource.eliot.eliotc.processor.OneToOneProcessor
import com.vanillasource.eliot.eliotc.sugar.DesugaredSourceAST

class ModuleNamesProcessor
    extends OneToOneProcessor((key: ModuleNames.Key) => DesugaredSourceAST.Key(pathName(key.moduleName))) {
  override def generateFromKeyAndFact(key: ModuleNames.Key, fact: DesugaredSourceAST)(using
      process: CompilationProcess
  ): IO[Unit] =
    for {
      localFunctions <- extractLocalFunctions(fact.sourcedAst.value.functionDefinitions)
      localTypes     <- extractLocalTypes(fact.sourcedAst.value.typeDefinitions)
      _              <- process.registerFact(ModuleNames(key.moduleName, localFunctions.keySet, localTypes.keySet))
    } yield ()
}
