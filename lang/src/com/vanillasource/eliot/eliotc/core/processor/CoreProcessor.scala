package com.vanillasource.eliot.eliotc.core.processor

import cats.data.NonEmptySeq
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.{
  Expression as SourceExpression,
  FunctionDefinition,
  SourceAST
}
import com.vanillasource.eliot.eliotc.core.fact.{AST as CoreASTData, Expression as CoreExpression, *}
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
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
    // Track per-ability implementation-index counters. User-written impls have already been
    // assigned indices at AST assembly time; we continue numbering from the max used per ability
    // so synthetic PatternMatch/TypeMatch impls don't collide with any user-defined ones.
    val counters      = scala.collection.mutable.Map.empty[String, Int]
    sourceAstData.functionDefinitions.foreach { f =>
      f.name.value.qualifier match {
        case Qualifier.AbilityImplementation(name, idx) =>
          counters.update(name.value, math.max(counters.getOrElse(name.value, -1), idx) + 1)
        case _                                          => ()
      }
    }
    val desugaredFromData: Seq[(FunctionDefinition, RoleHint)] = sourceAstData.typeDefinitions.flatMap { definition =>
      val pmIdx = counters.getOrElse("PatternMatch", 0)
      counters.update("PatternMatch", pmIdx + 1)
      val tmIdx = counters.getOrElse("TypeMatch", 0)
      counters.update("TypeMatch", tmIdx + 1)
      DataDefinitionDesugarer.desugar(definition, pmIdx, tmIdx)
    }
    val allFunctions  = sourceAstData.functionDefinitions.map(_ -> RoleHint.NoHint) ++ desugaredFromData
    val coreAstData   = CoreASTData(
      sourceAstData.importStatements,
      // Effect-set sugar (`{E} A`) is collapsed onto a single inferable carrier before the function is converted, so
      // everything downstream sees ordinary HKT-constrained generics (see EffectSugarDesugarer).
      allFunctions.map { case (fd, hint) => transformFunction(EffectSugarDesugarer.desugar(fd), hint) }
    )

    // Strict-positivity check (termination precondition #2): reject any `data` whose own type constructor appears in a
    // contravariant position of a constructor field (the negative-recursive-datatype route to `Y`). See
    // StrictPositivityChecker. Errors are reported here but the CoreAST is still produced so other checks proceed.
    val positivityErrors = sourceAstData.typeDefinitions.flatMap(StrictPositivityChecker.check)

    positivityErrors.traverse_(message => Sourced.compilerError(message)) >>
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

  private def transformFunction(function: FunctionDefinition, roleHint: RoleHint): NamedValue = {
    import CoreExpressionConverter.*
    val curriedType  = curriedFunctionType(function.args, function.typeDefinition, function.genericParameters)
    val isTypeBody   = function.typeDefinition.value match {
      // A bare `Type` return type (no value args, no — or empty — generic brackets). `genericArguments` is now an
      // `Option`: `None` = no brackets, `Some(Seq())` = `Type[]`; both count, neither carries args.
      case SourceExpression.FunctionApplication(None, fnName, genArgs, Seq()) if genArgs.forall(_.isEmpty) =>
        fnName.value == "Type"
      case _                                                                                               => false
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
    // Leading `auto`-marked binder count, scanning curried binders front-to-back (generic-parameter prefix first, then
    // value args) and stopping at the first non-`auto`. For `type Int[auto MIN, auto MAX]` (whose params are value
    // args) this is 2; for `type IO[A]` it is 0.
    val inferableArity = (function.genericParameters.map(_.inferable) ++ function.args.map(_.inferable))
      .takeWhile(identity)
      .size
    NamedValue(
      function.name,
      curriedValue,
      typeStack,
      constraints,
      function.fixity,
      function.precedence.map(convertPrecedenceDeclaration),
      function.visibility,
      roleHint,
      function.opaque,
      inferableArity,
      function.dischargedEffects
    )
  }
}
