package com.vanillasource.eliot.eliotc.sugar

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilationProcess.registerFact
import com.vanillasource.eliot.eliotc.ast.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.processor.CompilationProcess
import com.vanillasource.eliot.eliotc.source.pos.Sourced
import com.vanillasource.eliot.eliotc.processor.impl.OneToOneProcessor

class DesugarProcessor
    extends OneToOneProcessor((key: DesugaredSourceAST.Key) => SourceAST.Key(key.file))
    with Logging {

  override def generateFromFact(sourceAst: SourceAST)(using CompilationProcess): IO[Unit] =
    registerFact(DesugaredSourceAST(sourceAst.file, sourceAst.ast.as(desugar(sourceAst.ast.value))))

  private def desugar(ast: AST): AST = AST(
    ast.importStatements,
    generateTypeFunctions(ast.typeDefinitions) ++ ast.functionDefinitions,
    ast.typeDefinitions
  )

  private def generateTypeFunctions(dataDefinitions: Seq[DataDefinition]): Seq[FunctionDefinition] =
    dataDefinitions
      .filter(_.fields.isDefined)
      .flatMap { dataDefinition =>
        // Constructor
        Seq(
          FunctionDefinition(
            dataDefinition.name,
            dataDefinition.genericParameters,
            dataDefinition.fields.get,
            TypeReference(
              dataDefinition.name,
              dataDefinition.genericParameters.map(gp => TypeReference(gp.name, gp.genericParameters))
            ),
            None
          )
        ) ++ dataDefinition.fields.get.map { field =>
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
