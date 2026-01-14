package com.vanillasource.eliot.eliotc.module2.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{CoreAST, NamedValue}
import com.vanillasource.eliot.eliotc.module2.fact.*
import com.vanillasource.eliot.eliotc.module2.fact.ModuleName.defaultSystemModules
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

import java.io.File

class ModuleValueProcessor(systemModules: Seq[ModuleName] = defaultSystemModules)
    extends SingleKeyTypeProcessor[ModuleValue.Key] {

  override protected def generateFact(key: ModuleValue.Key): CompilerIO[Unit] =
    getFactOrAbort(CoreAST.Key(key.file)).flatMap { coreAST =>
      processImpl(key.file, key.vfqn.moduleName, coreAST)
    }

  private def processImpl(file: File, moduleName: ModuleName, coreAST: CoreAST): CompilerIO[Unit] =
    for {
      localNames      <- extractLocalNames(coreAST.ast.value.namedValues)
      importedModules  = extractImportedModules(moduleName, coreAST.ast.as(coreAST.ast.value.importStatements), systemModules)
      importedNames   <- extractImportedNames(importedModules, localNames.keySet)
      dictionary       = importedNames ++ localNames.keySet.map(name => (name, ValueFQN(moduleName, name))).toMap
      _               <- localNames
                           .map { (name, namedValue) =>
                             registerFactIfClear(ModuleValue(file, ValueFQN(moduleName, name), dictionary, namedValue))
                           }
                           .toSeq
                           .sequence_
    } yield ()

  private def extractLocalNames(namedValues: Seq[NamedValue]): CompilerIO[Map[String, NamedValue]] =
    namedValues.foldLeftM(Map.empty[String, NamedValue]) { (acc, nv) =>
      val name = nv.name.value

      if (acc.contains(name)) {
        compilerError(nv.name.as("Name was already defined in this module.")).as(acc)
      } else {
        (acc + (name -> nv)).pure[CompilerIO]
      }
    }

  private def extractImportedModules(
      moduleName: ModuleName,
      sourcedImports: Sourced[Seq[com.vanillasource.eliot.eliotc.ast.fact.ImportStatement]],
      systemModules: Seq[ModuleName]
  ): Seq[Sourced[ModuleName]] =
    sourcedImports.value
      .map(importStatement => importStatement.outline.as(ModuleName.fromImportStatement(importStatement)))
      .prependedAll(systemModules.filter(_ =!= moduleName).map(sourcedImports.as(_)))

  private def extractImportedNames(
      importedModules: Seq[Sourced[ModuleName]],
      localNames: Set[String]
  ): CompilerIO[Map[String, ValueFQN]] =
    importedModules.foldLeftM(Map.empty[String, ValueFQN])((acc, m) => importModuleNames(localNames, acc, m))

  private def importModuleNames(
      localNames: Set[String],
      importedNames: Map[String, ValueFQN],
      module: Sourced[ModuleName]
  ): CompilerIO[Map[String, ValueFQN]] =
    for {
      maybeModuleNames <- getFactOrAbort(UnifiedModuleNames.Key(module.value)).attempt
      result           <- maybeModuleNames match {
                            case Right(moduleNames) =>
                              val shadowingLocal    = moduleNames.names.intersect(localNames)
                              val shadowingImported = moduleNames.names.intersect(importedNames.keySet)

                              if (shadowingLocal.nonEmpty) {
                                compilerError(module.as(s"Imported names shadow local names: ${shadowingLocal.mkString(", ")}"))
                                  .as(importedNames)
                              } else if (shadowingImported.nonEmpty) {
                                compilerError(
                                  module.as(
                                    s"Imported names shadow other imported names: ${shadowingImported.flatMap(importedNames.get).mkString(", ")}"
                                  )
                                ).as(importedNames)
                              } else {
                                (importedNames ++ moduleNames.names.map(name => (name, ValueFQN(moduleNames.moduleName, name))).toMap)
                                  .pure[CompilerIO]
                              }
                            case Left(_)            =>
                              compilerError(module.as("Could not find imported module.")).as(importedNames)
                          }
    } yield result
}
