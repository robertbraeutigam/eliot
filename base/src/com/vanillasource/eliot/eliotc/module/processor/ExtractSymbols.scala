package com.vanillasource.eliot.eliotc.module.processor

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.{DataDefinition, FunctionDefinition, ImportStatement}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName, TypeFQN, UnifiedModuleNames}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

import java.nio.file.{Path, Paths}

object ExtractSymbols {
  def extractLocalFunctions(
      functionDefinitions: Seq[FunctionDefinition]
  ): CompilerIO[Map[String, FunctionDefinition]] =
    functionDefinitions.foldLeftM(Map.empty[String, FunctionDefinition])((acc, d) => extractLocalFunction(acc, d))

  def extractLocalTypes(definitions: Seq[DataDefinition]): CompilerIO[Map[String, DataDefinition]] =
    definitions.foldLeftM(Map.empty[String, DataDefinition])((acc, d) => extractLocalType(acc, d))

  def pathName(name: ModuleName): Path =
    (name.packages ++ Seq(name.name + ".els")).foldLeft(Paths.get(""))(_ `resolve` _)

  private def extractLocalType(
      previousTypes: Map[String, DataDefinition],
      current: DataDefinition
  ): CompilerIO[Map[String, DataDefinition]] = current.name.value match
    case ty if previousTypes.contains(ty) =>
      compilerError(current.name.as("Type was already defined in this module.")).as(previousTypes)
    case ty if !ty.charAt(0).isUpper      =>
      compilerError(current.name.as("Type name must start with upper case character."))
        .as(previousTypes)
    case ty                               => (previousTypes ++ Map((ty, current))).pure[CompilerIO]

  private def extractLocalFunction(
      previousFunctions: Map[String, FunctionDefinition],
      current: FunctionDefinition
  ): CompilerIO[Map[String, FunctionDefinition]] = current.name.value match
    case fn if previousFunctions.contains(fn)                                  =>
      compilerError(current.name.as("Function was already defined in this module.")).as(previousFunctions)
    case _ if current.args.map(_.name.value).toSet.size != current.args.length =>
      val duplicateName = current.args.groupBy(_.name.value).collectFirst {
        case (_, list) if list.length > 1 => list.head
      }
      compilerError(duplicateName.get.name.as("Duplicate parameter name."))
        .as(previousFunctions)
    case fn                                                                    =>
      (previousFunctions ++ Map((fn, current))).pure[CompilerIO]

  def extractImportedModules(
      moduleName: ModuleName,
      sourcedImports: Sourced[Seq[ImportStatement]],
      systemModules: Seq[ModuleName]
  ): Seq[Sourced[ModuleName]] =
    sourcedImports.value
      .map(importStatement => importStatement.outline.as(ModuleName.fromImportStatement(importStatement)))
      .prependedAll(
        systemModules.filter(_ =!= moduleName).map(sourcedImports.as(_))
      )

  def extractImportedFunctions(
      importedModules: Seq[Sourced[ModuleName]],
      localFunctionNames: Set[String]
  ): CompilerIO[Map[String, FunctionFQN]] =
    importedModules.foldLeftM(Map.empty[String, FunctionFQN])((acc, i) =>
      importModuleFunctions(localFunctionNames, acc, i)
    )

  def extractImportedTypes(
      importedModules: Seq[Sourced[ModuleName]],
      localTypeNames: Set[String]
  ): CompilerIO[Map[String, TypeFQN]] =
    importedModules.foldLeftM(Map.empty[String, TypeFQN])((acc, i) => importModuleTypes(localTypeNames, acc, i))

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
