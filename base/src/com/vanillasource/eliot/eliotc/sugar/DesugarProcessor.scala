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
import com.vanillasource.eliot.eliotc.processor.OneToOneProcessor
import com.vanillasource.eliot.eliotc.source.pos.Sourced
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerFactKey, CompilerProcessor}

import java.nio.file.Path

class DesugarProcessor
    extends OneToOneProcessor((key: DesugaredSourceAST.Key) => SourceAST.Key(key.path))
    with Logging {

  override def generateFromFact(sourceAst: SourceAST)(using process: CompilationProcess): IO[Unit] =
    process.registerFact(
      DesugaredSourceAST(sourceAst.path, sourceAst.asts.map(ast => ast.as(desugar(ast.value))))
    )

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
