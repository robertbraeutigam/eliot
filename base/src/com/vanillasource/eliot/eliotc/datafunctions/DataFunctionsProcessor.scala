package com.vanillasource.eliot.eliotc.datafunctions

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.ast.*
import com.vanillasource.eliot.eliotc.ast.fact.{
  AST,
  ArgumentDefinition,
  DataDefinition,
  FunctionDefinition,
  SourceAST,
  TypeReference
}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor

/** Adds implied constructor function and accessor functions to all data fields.
  */
class DataFunctionsProcessor
    extends TransformationProcessor[SourceAST.Key, DesugaredSourceAST.Key](key => SourceAST.Key(key.file))
    with Logging {

  override protected def generateFromKeyAndFact(
      key: DesugaredSourceAST.Key,
      sourceAst: SourceAST
  ): CompilerIO[DesugaredSourceAST] =
    DesugaredSourceAST(sourceAst.file, sourceAst.ast.as(desugar(sourceAst.ast.value))).pure[CompilerIO]

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
