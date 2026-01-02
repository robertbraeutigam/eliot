package com.vanillasource.eliot.eliotc.module.processor

import cats.Monad
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.ast.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.*
import com.vanillasource.eliot.eliotc.module.fact.*
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemModules
import com.vanillasource.eliot.eliotc.module.processor.ExtractSymbols.{extractLocalFunctions, extractLocalTypes}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.processor.{CompilerFact, CompilerFactKey, CompilerProcessor}
import com.vanillasource.eliot.eliotc.sugar.DesugaredSourceAST

import java.io.File

class ModuleProcessor(systemModules: Seq[ModuleName] = defaultSystemModules) extends CompilerProcessor with Logging {
  override def generate(factKey: CompilerFactKey[?]): CompilerIO[Unit] = factKey match {
    case ModuleFunction.Key(file, FunctionFQN(moduleName, functionName)) => generateModule(file, moduleName)
    case ModuleData.Key(file, TypeFQN(moduleName, typeName))             => generateModule(file, moduleName)
    case _                                                               => Monad[CompilerIO].unit
  }

  private def generateModule(file: File, name: ModuleName): CompilerIO[Unit] =
    getFactOrAbort(DesugaredSourceAST.Key(file)).flatMap(fact => processImpl(file, name, fact.sourcedAst))

  private def processImpl(file: File, moduleName: ModuleName, sourcedAst: Sourced[AST]): CompilerIO[Unit] =
    for {
      localFunctions    <- extractLocalFunctions(sourcedAst.value.functionDefinitions)
      localTypes        <- extractLocalTypes(sourcedAst.value.typeDefinitions)
      importedModules    = extractImportedModules(moduleName, sourcedAst.as(sourcedAst.value.importStatements))
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
      _                 <- localTypes
                             .map { (name, definition) =>
                               registerFactIfClear(
                                 ModuleData(file, TypeFQN(moduleName, name), typeDictionary, definition)
                               )
                             }
                             .toSeq
                             .sequence_
    } yield ()

  private def extractImportedModules(
      moduleName: ModuleName,
      sourcedImports: Sourced[Seq[ImportStatement]]
  ): Seq[Sourced[ModuleName]] =
    sourcedImports.value
      .map(importStatement => importStatement.outline.as(ModuleName.fromImportStatement(importStatement)))
      .prependedAll(
        systemModules.filter(_ =!= moduleName).map(sourcedImports.as(_))
      )

  private def extractImportedFunctions(
      importedModules: Seq[Sourced[ModuleName]],
      localFunctionNames: Set[String]
  ): CompilerIO[Map[String, FunctionFQN]] =
    importedModules.foldLeftM(Map.empty[String, FunctionFQN])((acc, i) => importModuleFunctions(localFunctionNames, acc, i))

  private def importModuleFunctions(
      localFunctionNames: Set[String],
      importedFunctions: Map[String, FunctionFQN],
      module: Sourced[ModuleName]
  ): CompilerIO[Map[String, FunctionFQN]] =
    for {
      maybeModuleFunctions <- getFactOrAbort(UnifiedModuleNames.Key(module.value)).attempt
      result               <- maybeModuleFunctions match {
                                case Right(moduleFunctions) =>
                                  if (moduleFunctions.functionNames.intersect(localFunctionNames).nonEmpty) {
                                    compilerError(
                                      module.as(
                                        s"Imported functions shadow local functions: ${moduleFunctions.functionNames.intersect(localFunctionNames).mkString(", ")}"
                                      )
                                    ).as(importedFunctions)
                                  } else if (moduleFunctions.functionNames.intersect(importedFunctions.keySet).nonEmpty) {
                                    compilerError(
                                      module.as(
                                        s"Imported functions shadow other imported functions: ${moduleFunctions.functionNames.intersect(importedFunctions.keySet).flatMap(importedFunctions.get).mkString(", ")}"
                                      )
                                    ).as(importedFunctions)
                                  } else {
                                    (importedFunctions ++ moduleFunctions.functionNames
                                      .map(name => (name, FunctionFQN(moduleFunctions.moduleName, name)))
                                      .toMap).pure[CompilerIO]
                                  }
                                case Left(_)                =>
                                  compilerError(module.as("Could not find imported module.")).as(importedFunctions)
                              }
    } yield result

  private def extractImportedTypes(
      importedModules: Seq[Sourced[ModuleName]],
      localTypeNames: Set[String]
  ): CompilerIO[Map[String, TypeFQN]] =
    importedModules.foldLeftM(Map.empty[String, TypeFQN])((acc, i) => importModuleTypes(localTypeNames, acc, i))

  private def importModuleTypes(
      localTypeNames: Set[String],
      importedTypes: Map[String, TypeFQN],
      module: Sourced[ModuleName]
  ): CompilerIO[Map[String, TypeFQN]] =
    for {
      maybeModuleFunctions <- getFactOrAbort(UnifiedModuleNames.Key(module.value)).attempt
      result               <- maybeModuleFunctions match {
                                case Right(moduleFunctions) =>
                                  if (moduleFunctions.typeNames.intersect(localTypeNames).nonEmpty) {
                                    compilerError(
                                      module.as(
                                        s"Imported types shadow local type: ${moduleFunctions.typeNames.intersect(localTypeNames).mkString(", ")}"
                                      )
                                    ).as(importedTypes)
                                  } else if (moduleFunctions.typeNames.intersect(importedTypes.keySet).nonEmpty) {
                                    compilerError(
                                      module.as(
                                        s"Imported types shadow other imported types: ${moduleFunctions.typeNames.intersect(importedTypes.keySet).flatMap(importedTypes.get).mkString(", ")}"
                                      )
                                    ).as(importedTypes)
                                  } else {
                                    (importedTypes ++ moduleFunctions.typeNames
                                      .map(name => (name, TypeFQN(moduleFunctions.moduleName, name)))
                                      .toMap).pure[CompilerIO]
                                  }
                                // Note: the "could not find imported module" was already caught earlier, so don't issue the error again
                                case Left(_)                => importedTypes.pure[CompilerIO]
                              }
    } yield result
}
