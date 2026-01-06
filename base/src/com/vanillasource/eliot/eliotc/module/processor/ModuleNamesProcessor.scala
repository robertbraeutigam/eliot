package com.vanillasource.eliot.eliotc.module.processor

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.module.fact.ModuleNames
import com.vanillasource.eliot.eliotc.module.processor.ExtractSymbols.*
import com.vanillasource.eliot.eliotc.datafunctions.DataFunctionsSourceAST
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor

class ModuleNamesProcessor
    extends TransformationProcessor[DataFunctionsSourceAST.Key, ModuleNames.Key](key => DataFunctionsSourceAST.Key(key.file)) {

  override protected def generateFromKeyAndFact(key: ModuleNames.Key, fact: DataFunctionsSourceAST): CompilerIO[ModuleNames] =
    for {
      localFunctions <- extractLocalFunctions(fact.sourcedAst.value.functionDefinitions)
      localTypes     <- extractLocalTypes(fact.sourcedAst.value.typeDefinitions)
    } yield ModuleNames(key.file, localFunctions.keySet, localTypes.keySet)
}
