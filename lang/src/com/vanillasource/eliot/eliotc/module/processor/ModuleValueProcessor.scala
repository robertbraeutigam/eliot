package com.vanillasource.eliot.eliotc.module.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.Visibility
import com.vanillasource.eliot.eliotc.core.fact.QualifiedName
import com.vanillasource.eliot.eliotc.core.fact.CoreAST
import com.vanillasource.eliot.eliotc.module.fact.*
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemModules
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

class ModuleValueProcessor(systemModules: Seq[ModuleName] = defaultSystemModules)
    extends SingleKeyTypeProcessor[ModuleValue.Key] {

  private case class ImportResult(
      dictionary: Map[QualifiedName, ValueFQN],
      privateNames: Map[QualifiedName, ValueFQN]
  )

  override protected def generateFact(key: ModuleValue.Key): CompilerIO[Unit] =
    for {
      coreAST        <- getFactOrAbort(CoreAST.Key(key.uri))
      moduleNames    <- getFactOrAbort(ModuleNames.Key(key.uri))
      importedModules =
        extractImportedModules(key.vfqn.moduleName, coreAST.ast.as(coreAST.ast.value.importStatements), systemModules)
      importResult   <- extractImportedNames(importedModules, moduleNames.names.value.keySet)
      localDictionary = moduleNames.names.value.collect {
                          case (name, vis) if vis != Visibility.Qualified => (name, ValueFQN(key.vfqn.moduleName, name))
                        }
      dictionary      = importResult.dictionary ++ localDictionary
      namedValuesMap  = coreAST.ast.value.namedValues.map(nv => nv.qualifiedName.value -> nv).toMap
      _              <- moduleNames.names.value.keys.toSeq
                          .flatMap(name => namedValuesMap.get(name).map(nv => (name, nv)))
                          .map { (name, namedValue) =>
                            registerFactIfClear(
                              ModuleValue(
                                key.uri,
                                ValueFQN(key.vfqn.moduleName, name),
                                dictionary,
                                namedValue,
                                importResult.privateNames
                              )
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
      localNames: Set[QualifiedName]
  ): CompilerIO[ImportResult] =
    importedModules.foldLeftM(ImportResult(Map.empty, Map.empty))((acc, m) => importModuleNames(localNames, acc, m))

  private def importModuleNames(
      localNames: Set[QualifiedName],
      accumulated: ImportResult,
      module: Sourced[ModuleName]
  ): CompilerIO[ImportResult] =
    for {
      maybeModuleNames <- getFact(UnifiedModuleNames.Key(module.value))
      result           <- maybeModuleNames match {
                            case Some(moduleNames) =>
                              val publicNames       = moduleNames.names.collect {
                                case (name, Visibility.Public) => name
                              }.toSet
                              val privateNameSet    = moduleNames.names.collect {
                                case (name, Visibility.Private) => name
                              }.toSet
                              val shadowingLocal    = publicNames.intersect(localNames)
                              val shadowingImported = publicNames.intersect(accumulated.dictionary.keySet)

                              if (shadowingLocal.nonEmpty) {
                                compilerError(
                                  module.as(
                                    s"Imported names shadow local names: ${shadowingLocal.toSeq.map(_.show).mkString(", ")}"
                                  )
                                )
                                  .as(accumulated)
                              } else if (shadowingImported.nonEmpty) {
                                compilerError(
                                  module.as(
                                    s"Imported names shadow other imported names: ${shadowingImported.flatMap(accumulated.dictionary.get).mkString(", ")}"
                                  )
                                ).as(accumulated)
                              } else {
                                ImportResult(
                                  accumulated.dictionary ++ publicNames
                                    .map(name => (name, ValueFQN(moduleNames.moduleName, name)))
                                    .toMap,
                                  accumulated.privateNames ++ privateNameSet
                                    .map(name => (name, ValueFQN(moduleNames.moduleName, name)))
                                    .toMap
                                ).pure[CompilerIO]
                              }
                            case None              =>
                              compilerError(module.as(s"Could not find imported module."))
                                .as(accumulated)
                          }
    } yield result
}
