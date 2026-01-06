package com.vanillasource.eliot.eliotc.module.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.AST
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.*
import com.vanillasource.eliot.eliotc.module.fact.*
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemModules
import com.vanillasource.eliot.eliotc.module.processor.ExtractSymbols.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.datafunctions.DataFunctionsSourceAST

import java.io.File

class ModuleFunctionProcessor(systemModules: Seq[ModuleName] = defaultSystemModules)
    extends SingleKeyTypeProcessor[ModuleFunction.Key]
    with Logging {

  override protected def generateFact(key: ModuleFunction.Key): CompilerIO[Unit] =
    getFactOrAbort(DataFunctionsSourceAST.Key(key.file)).flatMap { fact =>
      processImpl(key.file, key.ffqn.moduleName, fact.sourcedAst)
    }

  private def processImpl(file: File, moduleName: ModuleName, sourcedAst: com.vanillasource.eliot.eliotc.source.content.Sourced[AST]): CompilerIO[Unit] =
    for {
      localFunctions    <- extractLocalFunctions(sourcedAst.value.functionDefinitions)
      localTypes        <- extractLocalTypes(sourcedAst.value.typeDefinitions)
      importedModules    = extractImportedModules(moduleName, sourcedAst.as(sourcedAst.value.importStatements), systemModules)
      importedFunctions <- extractImportedFunctions(importedModules, localFunctions.keySet)
      importedTypes     <- extractImportedTypes(importedModules, localTypes.keySet)
      functionDictionary =
        importedFunctions ++ localFunctions.keySet.map(name => (name, FunctionFQN(moduleName, name))).toMap
      typeDictionary     =
        importedTypes ++ localTypes.keySet.map(name => (name, TypeFQN(moduleName, name))).toMap
      _                 <- localFunctions
                             .map { (name, definition) =>
                               registerFactIfClear(
                                 ModuleFunction(file, FunctionFQN(moduleName, name), functionDictionary, typeDictionary, definition)
                               )
                             }
                             .toSeq
                             .sequence_
    } yield ()
}
