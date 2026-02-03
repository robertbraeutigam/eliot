package com.vanillasource.eliot.eliotc.module.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.CoreAST
import com.vanillasource.eliot.eliotc.module.fact.*
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemModules
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

class ModuleValueProcessor(systemModules: Seq[ModuleName] = defaultSystemModules)
    extends SingleKeyTypeProcessor[ModuleValue.Key] {

  override protected def generateFact(key: ModuleValue.Key): CompilerIO[Unit] =
    for {
      coreAST        <- getFactOrAbort(CoreAST.Key(key.file))
      moduleNames    <- getFactOrAbort(ModuleNames.Key(key.file))
      importedModules =
        extractImportedModules(key.vfqn.moduleName, coreAST.ast.as(coreAST.ast.value.importStatements), systemModules)
      importedNames  <- extractImportedNames(importedModules, moduleNames.names)
      dictionary      = importedNames ++ moduleNames.names.map(name => (name, ValueFQN(key.vfqn.moduleName, name))).toMap
      namedValuesMap  = coreAST.ast.value.namedValues.map(nv => nv.name.value -> nv).toMap
      _              <- moduleNames.names.toSeq
                          .flatMap(name => namedValuesMap.get(name).map(nv => (name, nv)))
                          .map { (name, namedValue) =>
                            registerFactIfClear(
                              ModuleValue(key.file, ValueFQN(key.vfqn.moduleName, name), dictionary, namedValue)
                            )
                          }
                          .sequence_
    } yield ()

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
                                (importedNames ++ moduleNames.names
                                  .map(name => (name, ValueFQN(moduleNames.moduleName, name)))
                                  .toMap)
                                  .pure[CompilerIO]
                              }
                            case Left(_)            =>
                              compilerError(module.as("Could not find imported module.")).as(importedNames)
                          }
    } yield result
}
