package com.vanillasource.eliot.eliotc.module.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.*
import com.vanillasource.eliot.eliotc.module.fact.*
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemModules
import com.vanillasource.eliot.eliotc.module.processor.ExtractSymbols.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.sugar.DesugaredSourceAST

import java.io.File

class ModuleDataProcessor(systemModules: Seq[ModuleName] = defaultSystemModules)
    extends SingleKeyTypeProcessor[ModuleData.Key]
    with Logging {

  override protected def generateFact(key: ModuleData.Key): CompilerIO[Unit] =
    getFactOrAbort(DesugaredSourceAST.Key(key.file)).flatMap { fact =>
      processImpl(key.file, key.tfqn.moduleName, fact.sourcedAst)
    }

  private def processImpl(file: File, moduleName: ModuleName, sourcedAst: com.vanillasource.eliot.eliotc.source.content.Sourced[com.vanillasource.eliot.eliotc.ast.AST]): CompilerIO[Unit] =
    for {
      localFunctions    <- extractLocalFunctions(sourcedAst.value.functionDefinitions)
      localTypes        <- extractLocalTypes(sourcedAst.value.typeDefinitions)
      importedModules    = extractImportedModules(moduleName, sourcedAst.as(sourcedAst.value.importStatements), systemModules)
      importedFunctions <- extractImportedFunctions(importedModules, localFunctions.keySet)
      importedTypes     <- extractImportedTypes(importedModules, localTypes.keySet)
      typeDictionary     =
        importedTypes ++ localTypes.keySet.map(name => (name, TypeFQN(moduleName, name))).toMap
      _                 <- localTypes
                             .map { (name, definition) =>
                               registerFactIfClear(
                                 ModuleData(file, TypeFQN(moduleName, name), typeDictionary, definition)
                               )
                             }
                             .toSeq
                             .sequence_
    } yield ()
}
