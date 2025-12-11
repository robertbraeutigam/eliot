package com.vanillasource.eliot.eliotc.module.processor

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.CompilationProcess.{getFact, registerFact}
import com.vanillasource.eliot.eliotc.ast.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.*
import com.vanillasource.eliot.eliotc.module.fact.*
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemModules
import com.vanillasource.eliot.eliotc.module.processor.ExtractSymbols.{extractLocalFunctions, extractLocalTypes, pathName}
import com.vanillasource.eliot.eliotc.source.error.SourcedError.registerCompilerError
import com.vanillasource.eliot.eliotc.source.pos.Sourced
import com.vanillasource.eliot.eliotc.sugar.DesugaredSourceAST
import com.vanillasource.eliot.eliotc.util.CatsOps.*
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerFactKey, CompilerProcessor}

import java.io.File

class ModuleProcessor(systemModules: Seq[ModuleName] = defaultSystemModules) extends CompilerProcessor with Logging {
  override def generate(factKey: CompilerFactKey[?])(using CompilationProcess): IO[Unit] = factKey match {
    case ModuleFunction.Key(file, FunctionFQN(moduleName, functionName)) => generateModule(file, moduleName)
    case ModuleData.Key(file, TypeFQN(moduleName, typeName))             => generateModule(file, moduleName)
    case _                                                               => IO.unit
  }

  private def generateModule(file: File, name: ModuleName)(using CompilationProcess): IO[Unit] =
    getFact(DesugaredSourceAST.Key(file))
      .flatMap(_.traverse_(fact => processFact(name, fact)))

  private def processFact(moduleName: ModuleName, fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match {
    case DesugaredSourceAST(file, sourcedAst) => processImpl(file, moduleName, sourcedAst)
    case _                                    => IO.unit
  }

  private def processImpl(file: File, moduleName: ModuleName, sourcedAst: Sourced[AST])(using
      CompilationProcess
  ): IO[Unit] =
    for {
      localFunctions    <- extractLocalFunctions(sourcedAst.value.functionDefinitions)
      localTypes        <- extractLocalTypes(sourcedAst.value.typeDefinitions)
      importedModules   <- extractImportedModules(moduleName, sourcedAst.as(sourcedAst.value.importStatements)).pure[IO]
      importedFunctions <- extractImportedFunctions(importedModules, localFunctions.keySet)
      importedTypes     <- extractImportedTypes(importedModules, localTypes.keySet)
      _                 <- debug[IO](s"for ${moduleName.show} read function names: ${localFunctions.keySet
                               .mkString(", ")}, type names: ${localTypes.keySet
                               .mkString(", ")}, imported functions: ${importedFunctions.keySet
                               .mkString(", ")}, imported types: ${importedTypes.keySet.mkString(", ")}")
      functionDictionary =
        importedFunctions ++ localFunctions.keySet.map(name => (name, FunctionFQN(moduleName, name))).toMap
      typeDictionary     =
        importedTypes ++ localTypes.keySet.map(name => (name, TypeFQN(moduleName, name))).toMap
      _                 <- localFunctions
                             .map { (name, definition) =>
                               registerFact(
                                 ModuleFunction(file, FunctionFQN(moduleName, name), functionDictionary, typeDictionary, definition)
                               )
                             }
                             .toSeq
                             .sequence_
      _                 <- localTypes
                             .map { (name, definition) =>
                               registerFact(
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
  )(using CompilationProcess): IO[Map[String, FunctionFQN]] =
    importedModules.foldM(Map.empty[String, FunctionFQN])((acc, i) => importModuleFunctions(localFunctionNames, acc, i))

  private def importModuleFunctions(
      localFunctionNames: Set[String],
      importedFunctions: Map[String, FunctionFQN],
      module: Sourced[ModuleName]
  )(using CompilationProcess): IO[Map[String, FunctionFQN]] = {
    val extractedImport = for {
      moduleFunctions <- getFact(UnifiedModuleNames.Key(module.value)).toOptionT
      result          <-
        if (moduleFunctions.functionNames.intersect(localFunctionNames).nonEmpty) {
          registerCompilerError(
            module.as(
              s"Imported functions shadow local functions: ${moduleFunctions.functionNames.intersect(localFunctionNames).mkString(", ")}"
            )
          ).liftOptionTNone
        } else if (moduleFunctions.functionNames.intersect(importedFunctions.keySet).nonEmpty) {
          registerCompilerError(
            module.as(
              s"Imported functions shadow other imported functions: ${moduleFunctions.functionNames.intersect(importedFunctions.keySet).flatMap(importedFunctions.get).mkString(", ")}"
            )
          ).liftOptionTNone
        } else {
          IO.pure(
            importedFunctions ++ moduleFunctions.functionNames
              .map(name => (name, FunctionFQN(moduleFunctions.moduleName, name)))
              .toMap
          ).liftOptionT
        }
    } yield result

    extractedImport.getOrElseF {
      registerCompilerError(module.as("Could not find imported module.")).as(importedFunctions)
    }
  }

  private def extractImportedTypes(
      importedModules: Seq[Sourced[ModuleName]],
      localTypeNames: Set[String]
  )(using CompilationProcess): IO[Map[String, TypeFQN]] =
    importedModules.foldM(Map.empty[String, TypeFQN])((acc, i) => importModuleTypes(localTypeNames, acc, i))

  private def importModuleTypes(
      localTypeNames: Set[String],
      importedTypes: Map[String, TypeFQN],
      module: Sourced[ModuleName]
  )(using CompilationProcess): IO[Map[String, TypeFQN]] = {
    val extractedImport = for {
      moduleFunctions <- getFact(UnifiedModuleNames.Key(module.value)).toOptionT
      result          <-
        if (moduleFunctions.typeNames.intersect(localTypeNames).nonEmpty) {
          registerCompilerError(
            module.as(
              s"Imported types shadow local type: ${moduleFunctions.typeNames.intersect(localTypeNames).mkString(", ")}"
            )
          ).liftOptionTNone
        } else if (moduleFunctions.typeNames.intersect(importedTypes.keySet).nonEmpty) {
          registerCompilerError(
            module.as(
              s"Imported types shadow other imported types: ${moduleFunctions.typeNames.intersect(importedTypes.keySet).flatMap(importedTypes.get).mkString(", ")}"
            )
          ).liftOptionTNone
        } else {
          IO.pure(
            importedTypes ++ moduleFunctions.typeNames
              .map(name => (name, TypeFQN(moduleFunctions.moduleName, name)))
              .toMap
          ).liftOptionT
        }
    } yield result

    // Note: the "could not find imported module was already caught earlier, so don't issue the error again
    extractedImport.getOrElse(importedTypes)
  }
}
