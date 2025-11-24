package com.vanillasource.eliot.eliotc.sugar

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.{
  AST,
  ArgumentDefinition,
  DataDefinition,
  FunctionDefinition,
  SourceAST,
  TypeReference
}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}

import java.io.File

class DesugarProcessor extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match {
    case SourceAST(file, rootPath, ast) => process(file, rootPath, ast)
    case _                              => IO.unit
  }

  private def process(file: File, rootPath: File, ast: AST)(using process: CompilationProcess): IO[Unit] =
    process.registerFact(DesugaredSourceAST(file, rootPath, desugar(ast)))

  private def desugar(ast: AST): AST = AST(
    ast.importStatements,
    generateTypeFunctions(ast.typeDefinitions) ++ ast.functionDefinitions,
    ast.typeDefinitions
  )

  private def generateTypeFunctions(dataDefinitions: Seq[DataDefinition]): Seq[FunctionDefinition] =
    dataDefinitions.flatMap { dataDefinition =>
      // Constructor
      Seq(
        FunctionDefinition(
          dataDefinition.name,
          dataDefinition.genericParameters,
          dataDefinition.arguments,
          TypeReference(
            dataDefinition.name,
            dataDefinition.genericParameters.map(gp => TypeReference(gp.name, gp.genericParameters))
          ),
          None
        )
      ) ++ dataDefinition.arguments.map { field =>
        FunctionDefinition(
          field.name,
          dataDefinition.genericParameters,
          Seq(
            ArgumentDefinition(
              dataDefinition.name.as("obj"),
              TypeReference(
                dataDefinition.name,
                dataDefinition.genericParameters.map(gp => TypeReference(gp.name, gp.genericParameters))
              )
            )
          ),
          field.typeReference,
          None
        )
      }
    }
}
