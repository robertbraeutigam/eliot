package com.vanillasource.eliot.eliotc.core.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.{FunctionDefinition, SourceAST}
import com.vanillasource.eliot.eliotc.core.fact.{AST as CoreASTData, *}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor

/** Converts the source AST to the core AST.
  *
  * This processor performs several transformations:
  *   - Curries all functions to have exactly one argument and one result
  *   - Converts data definitions into constructor and accessor functions
  *   - Represents all types as levels in ExpressionStacks
  */
class CoreProcessor
    extends TransformationProcessor[SourceAST.Key, CoreAST.Key](key => SourceAST.Key(key.uri))
    with Logging {

  override protected def generateFromKeyAndFact(
      key: CoreAST.Key,
      sourceAst: SourceAST
  ): CompilerIO[CoreAST] = {
    val sourceAstData = sourceAst.ast.value
    val allFunctions  = sourceAstData.functionDefinitions ++
      sourceAstData.typeDefinitions.flatMap(DataDefinitionDesugarer.desugar)
    val coreAstData   = CoreASTData(
      sourceAstData.importStatements,
      allFunctions.map(transformFunction)
    )

    debug[CompilerIO](
      s"Core functions in ${key.uri}: ${coreAstData.namedValues.map(_.show).mkString(", ")}"
    ) >>
      CoreAST(sourceAst.uri, sourceAst.ast.as(coreAstData)).pure[CompilerIO]
  }

  private def transformFunction(function: FunctionDefinition): NamedValue = {
    import CoreExpressionConverter.*
    val curriedType  = curriedFunctionType(function.args, function.typeDefinition, function.genericParameters)
    val curriedValue = function.body.map(body => buildCurriedBody(function.args, body))
    val typeStack    = TypeStack.of(curriedType.value)
    val constraints  = function.genericParameters
      .map(gp =>
        gp.name.value -> gp.abilityConstraints.map(c =>
          NamedValue.CoreAbilityConstraint(c.abilityName, c.typeParameters.map(toTypeExpression(_).value))
        )
      )
      .filter(_._2.nonEmpty)
      .toMap
    NamedValue(
      convertQualifiedName(function.name),
      curriedValue,
      typeStack,
      constraints,
      function.fixity,
      function.precedence.map(convertPrecedenceDeclaration),
      function.visibility
    )
  }
}
