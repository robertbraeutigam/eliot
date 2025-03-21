package com.vanillasource.eliot.eliotc.module.processor

import cats.data.OptionT
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.*
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.systemFunctionModuleName
import com.vanillasource.eliot.eliotc.module.fact.{
  FunctionFQN,
  ModuleData,
  ModuleFunction,
  ModuleName,
  ModuleNames,
  TypeFQN
}
import com.vanillasource.eliot.eliotc.source.SourcedError.registerCompilerError
import com.vanillasource.eliot.eliotc.source.{PositionRange, Sourced}
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}
import com.vanillasource.util.CatsOps.*

import java.io.File
import java.util.Locale
import scala.jdk.CollectionConverters.IteratorHasAsScala

class ModuleProcessor(systemModules: Seq[ModuleName] = defaultSystemModules) extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match {
    case SourceAST(file, rootPath, ast) => process(file, rootPath, ast).getOrUnit
    case _                              => IO.unit
  }

  private def process(file: File, rootPath: File, ast: AST)(using CompilationProcess): OptionT[IO, Unit] =
    for {
      moduleName <- determineModuleName(file, rootPath).toOptionT
      _          <- process(file: File, moduleName, ast).liftOptionT
    } yield ()

  private def process(file: File, moduleName: ModuleName, ast: AST)(using
      process: CompilationProcess
  ): IO[Unit] = for {
    localFunctions    <- extractLocalFunctions(ast.functionDefinitions)
    localTypes        <- extractLocalTypes(ast.typeDefinitions)
    _                 <- process.registerFact(ModuleNames(moduleName, localFunctions.keySet, localTypes.keySet))
    importedModules   <- extractImportedModules(file, moduleName, ast.importStatements).pure[IO]
    importedFunctions <- extractImportedFunctions(importedModules, localFunctions.keySet)
    importedTypes     <- extractImportedTypes(importedModules, localTypes.keySet)
    _                 <- debug(s"for ${moduleName.show} read function names: ${localFunctions.keySet
                             .mkString(", ")}, type names: ${localTypes.keySet
                             .mkString(", ")}, imported functions: ${importedFunctions.keySet
                             .mkString(", ")}, imported types: ${importedTypes.keySet.mkString(", ")}")
    functionDictionary =
      importedFunctions ++ localFunctions.keySet.map(name => (name, FunctionFQN(moduleName, name))).toMap
    typeDictionary     =
      importedTypes ++ localTypes.keySet.map(name => (name, TypeFQN(moduleName, name))).toMap
    _                 <- localFunctions
                           .map { (name, definition) =>
                             process.registerFact(
                               ModuleFunction(FunctionFQN(moduleName, name), functionDictionary, typeDictionary, definition)
                             )
                           }
                           .toSeq
                           .sequence_
    _                 <- localTypes
                           .map { (name, definition) =>
                             process.registerFact(
                               ModuleData(TypeFQN(moduleName, name), typeDictionary, definition)
                             )
                           }
                           .toSeq
                           .sequence_
  } yield ()

  private def extractImportedModules(
      file: File,
      moduleName: ModuleName,
      imports: Seq[ImportStatement]
  ): Seq[Sourced[ModuleName]] =
    imports
      .map(importStatement => importStatement.outline.as(ModuleName.fromImportStatement(importStatement)))
      .prependedAll(
        systemModules.filter(_ =!= moduleName).map(sm => Sourced(file, PositionRange.zero, sm))
      )

  private def extractImportedFunctions(
      importedModules: Seq[Sourced[ModuleName]],
      localFunctionNames: Set[String]
  )(using process: CompilationProcess): IO[Map[String, FunctionFQN]] =
    importedModules.foldM(Map.empty[String, FunctionFQN])((acc, i) => importModuleFunctions(localFunctionNames, acc, i))

  private def importModuleFunctions(
      localFunctionNames: Set[String],
      importedFunctions: Map[String, FunctionFQN],
      module: Sourced[ModuleName]
  )(using process: CompilationProcess): IO[Map[String, FunctionFQN]] = {
    val extractedImport = for {
      moduleFunctions <- process.getFact(ModuleNames.Key(module.value)).toOptionT
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

  private def extractLocalTypes(definitions: Seq[DataDefinition])(using
      process: CompilationProcess
  ): IO[Map[String, DataDefinition]] =
    definitions.foldM(Map.empty[String, DataDefinition])((acc, d) => extractLocalType(acc, d))

  private def extractLocalType(
      previousTypes: Map[String, DataDefinition],
      current: DataDefinition
  )(using process: CompilationProcess): IO[Map[String, DataDefinition]] = current.name.value match
    case ty if previousTypes.contains(ty) =>
      registerCompilerError(current.name.as("Type was already defined in this module.")).as(previousTypes)
    case ty if !ty.charAt(0).isUpper      =>
      registerCompilerError(current.name.as("Type name must start with upper case character."))
        .as(previousTypes)
    case ty                               => (previousTypes ++ Map((ty, current))).pure

  private def extractLocalFunctions(
      functionDefinitions: Seq[FunctionDefinition]
  )(using process: CompilationProcess): IO[Map[String, FunctionDefinition]] =
    functionDefinitions.foldM(Map.empty[String, FunctionDefinition])((acc, d) => extractLocalFunction(acc, d))

  private def extractLocalFunction(
      previousFunctions: Map[String, FunctionDefinition],
      current: FunctionDefinition
  )(using process: CompilationProcess): IO[Map[String, FunctionDefinition]] = current.name.value match
    case fn if previousFunctions.contains(fn)                                  =>
      registerCompilerError(current.name.as("Function was already defined in this module.")).as(previousFunctions)
    case fn if !fn.charAt(0).isLower                                           =>
      registerCompilerError(current.name.as("Function name must start with lower case character."))
        .as(previousFunctions)
    case _ if current.args.map(_.name.value).toSet.size != current.args.length =>
      val duplicateName = current.args.groupBy(_.name.value).collectFirst {
        case (_, list) if list.length > 1 => list.head
      }
      registerCompilerError(duplicateName.get.name.as("Duplicate parameter name."))
        .as(previousFunctions)
    case fn                                                                    => (previousFunctions ++ Map((fn, current))).pure

  private def extractImportedTypes(
      importedModules: Seq[Sourced[ModuleName]],
      localTypeNames: Set[String]
  )(using process: CompilationProcess): IO[Map[String, TypeFQN]] =
    importedModules.foldM(Map.empty[String, TypeFQN])((acc, i) => importModuleTypes(localTypeNames, acc, i))

  private def importModuleTypes(
      localTypeNames: Set[String],
      importedTypes: Map[String, TypeFQN],
      module: Sourced[ModuleName]
  )(using process: CompilationProcess): IO[Map[String, TypeFQN]] = {
    val extractedImport = for {
      moduleFunctions <- process.getFact(ModuleNames.Key(module.value)).toOptionT
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

  private def determineModuleName(file: File, rootPath: File)(using
      process: CompilationProcess
  ): IO[Option[ModuleName]] = {
    val candidateName = file.getName
    candidateName match
      case ""                       => registerCompilerError(file, "Module name is empty.").as(None)
      case s if !s.endsWith(".els") => registerCompilerError(file, "File for module does not end in '.els'.").as(None)
      case s if s.charAt(0).isLower => registerCompilerError(file, "Module name must be capitalized.").as(None)
      case s                        =>
        val moduleSegments = rootPath.toPath
          .normalize()
          .relativize(Option(file.getParentFile).getOrElse(File(".")).toPath.normalize())
          .iterator()
          .asScala
          .map(_.toString)
          .filter(_.nonEmpty)
          .toSeq

        if (moduleSegments.exists(s => s =!= s.toLowerCase(Locale.ROOT))) {
          registerCompilerError(file, "Directory names, as they are package names, must be lower case.").as(None)
        } else if (moduleSegments.exists(_ === "src")) {
          registerCompilerError(file, "Package names must not contain 'src'. Please check root paths.").as(None)
        } else {
          Some(ModuleName(moduleSegments, s.substring(0, s.length - 4))).pure
        }
  }
}

object ModuleProcessor {
  private val defaultSystemModules = Seq(
    systemFunctionModuleName
  )
}
