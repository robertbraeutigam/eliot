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
import com.vanillasource.eliot.eliotc.source.pos.Sourced
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerFactKey, CompilerProcessor}

import java.nio.file.Path

class DesugarProcessor extends CompilerProcessor with Logging {
  override def generate(factKey: CompilerFactKey[_])(using process: CompilationProcess): IO[Unit] = factKey match {
    case DesugaredSourceAST.Key(path) =>
      process.getFact(SourceAST.Key(path)).flatMap(_.traverse_(processFact))
    case _                            => IO.unit
  }

  private def processFact(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match {
    case SourceAST(path, sourcedAst) => process(path, sourcedAst)
    case _                           => IO.unit
  }

  private def process(file: Path, sourcedAst: Sourced[AST])(using process: CompilationProcess): IO[Unit] =
    process.registerFact(DesugaredSourceAST(file, sourcedAst.as(desugar(sourcedAst.value))))

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
