package com.vanillasource.eliot.eliotc.module

import cats.data.OptionT
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.{AST, FunctionDefinition, ImportStatement, SourceAST}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.source.SourcedError.registerCompilerError
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}
import com.vanillasource.util.CatsOps.*

import java.io.File

class ModuleProcessor extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match {
    case SourceAST(file, ast) => process(file, ast).getOrUnit
    case _                    => IO.unit
  }

  private def process(file: File, ast: AST)(using CompilationProcess): OptionT[IO, Unit] =
    for {
      moduleName <- determineModuleName(file).toOptionT
      _          <- processFunctions(moduleName, ast).liftOptionT
    } yield ()

  private def processFunctions(moduleName: ModuleName, ast: AST)(using process: CompilationProcess): IO[Unit] = for {
    localFunctions    <- extractFunctions(ast.functionDefinitions)
    _                 <- process.registerFact(ModuleFunctionsNames(moduleName, localFunctions.keySet))
    importedFunctions <- extractImportedFunctions(localFunctions.keySet, ast.importStatements)
    _                 <- debug(s"read function names for ${moduleName.show}: ${localFunctions.keySet
                             .mkString(", ")}, imported functions: ${importedFunctions.keySet.mkString(", ")}")
    functionDictionary =
      importedFunctions ++ localFunctions.keySet.map(name => (name, FunctionFQN(moduleName, name))).toMap
    _                 <- localFunctions
                           .map { (name, definition) =>
                             process.registerFact(ModuleFunction(FunctionFQN(moduleName, name), functionDictionary, definition))
                           }
                           .toSeq
                           .sequence_
  } yield ()

  private def extractImportedFunctions(
      localFunctionNames: Set[String],
      imports: Seq[ImportStatement]
  )(using process: CompilationProcess): IO[Map[String, FunctionFQN]] =
    imports.foldM(Map.empty[String, FunctionFQN])((acc, i) => extractImport(localFunctionNames, acc, i))

  private def extractImport(
      localFunctionNames: Set[String],
      importedFunctions: Map[String, FunctionFQN],
      statement: ImportStatement
  )(using process: CompilationProcess): IO[Map[String, FunctionFQN]] = {
    val extractedImport = for {
      moduleFunctions <- process.getFact(ModuleFunctionsNames.Key(ModuleName.fromImportStatement(statement))).toOptionT
      result          <-
        if (moduleFunctions.functionNames.intersect(localFunctionNames).nonEmpty) {
          registerCompilerError(
            statement.outline.as(
              s"Imported functions shadow local functions: ${moduleFunctions.functionNames.intersect(localFunctionNames).mkString(", ")}"
            )
          ).liftOptionTNone
        } else if (moduleFunctions.functionNames.intersect(importedFunctions.keySet).nonEmpty) {
          registerCompilerError(
            statement.outline.as(
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
      registerCompilerError(statement.outline.as("Could not find imported module.")).as(importedFunctions)
    }
  }

  private def extractFunctions(
      functionDefinitions: Seq[FunctionDefinition]
  )(using process: CompilationProcess): IO[Map[String, FunctionDefinition]] =
    functionDefinitions.foldM(Map.empty[String, FunctionDefinition])((acc, d) => extractFunction(acc, d))

  private def extractFunction(
      previousFunctions: Map[String, FunctionDefinition],
      current: FunctionDefinition
  )(using process: CompilationProcess): IO[Map[String, FunctionDefinition]] = current.name.value.content match
    case fn if previousFunctions.contains(fn) =>
      registerCompilerError(current.name.as("Function was already defined in this module.")).as(previousFunctions)
    case fn if !fn.charAt(0).isLower          =>
      registerCompilerError(current.name.as("Function name must start with lower case character."))
        .as(previousFunctions)
    case fn                                   => (previousFunctions ++ Map((fn, current))).pure

  private def determineModuleName(file: File)(using process: CompilationProcess): IO[Option[ModuleName]] = {
    // TODO: needs to parse packages to determine module path
    val candidateName = file.getName
    candidateName match
      case ""                       => registerCompilerError(file, "Module name is empty.").as(None)
      case s if !s.endsWith(".els") => registerCompilerError(file, "File for module does not end in '.els'.").as(None)
      case s if s.charAt(0).isLower => registerCompilerError(file, "Module name must be capitalized.").as(None)
      case s                        => Some(ModuleName(Seq.empty, s.substring(0, s.length - 4))).pure
  }
}
