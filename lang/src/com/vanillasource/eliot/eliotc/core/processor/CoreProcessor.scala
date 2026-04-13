package com.vanillasource.eliot.eliotc.core.processor

import cats.data.NonEmptySeq
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.{Expression as SourceExpression, FunctionDefinition, SourceAST}
import com.vanillasource.eliot.eliotc.core.fact.{AST as CoreASTData, Expression as CoreExpression, *}
import com.vanillasource.eliot.eliotc.source.content.Sourced
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

  /** Builds the kind expression for a generic function's type stack. For generic parameters [A: K1, B: K2, ...], the
    * kind is Function(K1, Function(K2, ..., Type)).
    */
  private def buildKindExpression(function: FunctionDefinition): CoreExpression = {
    import CoreExpressionConverter.*
    import CoreExpression.*
    val s = function.name
    function.genericParameters
      .foldRight[Sourced[CoreExpression]](
        s.as(NamedValueReference(s.as(QualifiedName("Type", Qualifier.Type))))
      ) { (param, acc) =>
        val kindType    = convertExpression(param.typeRestriction, typeContext = true)
        val functionRef = s.as(NamedValueReference(s.as(QualifiedName("Function", Qualifier.Type))))
        s.as(FunctionApplication(s.as(FunctionApplication(functionRef, kindType)), acc))
      }
      .value
  }

  private def transformFunction(function: FunctionDefinition): NamedValue = {
    import CoreExpressionConverter.*
    val curriedType  = curriedFunctionType(function.args, function.typeDefinition, function.genericParameters)
    val isTypeBody   = function.typeDefinition.value match {
      case SourceExpression.FunctionApplication(None, fnName, Seq(), Seq()) => fnName.value == "Type"
      case _                                                               => false
    }
    val curriedValue = function.body.map(body => buildCurriedBody(function.args, body, isTypeBody))
    val typeStack    =
      if (function.genericParameters.nonEmpty)
        TypeStack(NonEmptySeq.of(curriedType.value, buildKindExpression(function)))
      else TypeStack.of(curriedType.value)
    val constraints  = function.genericParameters
      .map(gp =>
        gp.name.value -> gp.abilityConstraints.map(c =>
          NamedValue.CoreAbilityConstraint(
            c.abilityName,
            c.typeParameters.map(te => convertExpression(te, typeContext = true).value)
          )
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
