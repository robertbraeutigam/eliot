package com.vanillasource.eliot.eliotc.module.processor

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.module.fact.ModuleNames
import com.vanillasource.eliot.eliotc.module.processor.ExtractSymbols.*
import com.vanillasource.eliot.eliotc.sugar.DesugaredSourceAST
import com.vanillasource.eliot.eliotc.processor.impl.OneToOneProcessor

class ModuleNamesProcessor extends OneToOneProcessor((key: ModuleNames.Key) => DesugaredSourceAST.Key(key.file)) {
  override def generateFromKeyAndFact(key: ModuleNames.Key, fact: DesugaredSourceAST): CompilerIO[Unit] =
    for {
      localFunctions <- extractLocalFunctions(fact.sourcedAst.value.functionDefinitions)
      localTypes     <- extractLocalTypes(fact.sourcedAst.value.typeDefinitions)
      _              <- registerFactIfClear(ModuleNames(key.file, localFunctions.keySet, localTypes.keySet))
    } yield ()
}
