package com.vanillasource.eliot.eliotc.module

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.{AST, FunctionDefinition, SourceAST}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.source.SourcedError.compilerError
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}

import java.io.File

class ModuleProcessor extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match {
    case SourceAST(file, ast) => process(file, ast)
    case _                    => IO.unit
  }

  private def process(file: File, ast: AST)(using CompilationProcess): IO[Unit] =
    for {
      moduleName <- determineModuleName(file)
      _          <- moduleName match
                      case Some(mn) => processFunctions(file, mn, ast)
                      case None     => IO.unit
    } yield ()

  private def processFunctions(file: File, moduleName: ModuleName, ast: AST)(using
      process: CompilationProcess
  ): IO[Unit] = for {
    functions <-
      ast.functionDefinitions.foldM(Map.empty[String, FunctionDefinition])((acc, d) => extractFunction(file, acc, d))
    _         <- process.registerFact(ModuleFunctionsNames(moduleName, functions.keySet))
    _         <- debug(s"read function names for ${moduleName}: ${functions.keySet.mkString(", ")}")
  } yield ()

  private def extractFunction(
      file: File,
      previousFunctions: Map[String, FunctionDefinition],
      current: FunctionDefinition
  )(using process: CompilationProcess): IO[Map[String, FunctionDefinition]] = current.name.value.content match
    case fn if previousFunctions.contains(fn) =>
      compilerError(file, current.name.as("Function was already defined in this module.")).as(previousFunctions)
    case fn if !fn.charAt(0).isLower          =>
      compilerError(file, current.name.as("Function name must start with lower case character.")).as(previousFunctions)
    case fn                                   => (previousFunctions ++ Map((fn, current))).pure

  private def determineModuleName(file: File)(using process: CompilationProcess): IO[Option[ModuleName]] = {
    // TODO: needs to parse packages to determine module path
    val candidateName = file.getName
    candidateName match
      case ""                       => compilerError(file, "Module name is empty.").as(None)
      case s if !s.endsWith(".els") => compilerError(file, "File for module does not end in '.els'.").as(None)
      case s if s.charAt(0).isLower => compilerError(file, "Module name must be capitalized.").as(None)
      case s                        => Some(ModuleName(Seq.empty, s.substring(0, s.length - 4))).pure
  }
}
