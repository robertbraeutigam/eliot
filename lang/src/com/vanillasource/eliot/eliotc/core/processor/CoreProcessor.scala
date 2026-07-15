package com.vanillasource.eliot.eliotc.core.processor

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
    // Each data definition's synthetic PatternMatch/TypeMatch implementations are keyed by the data type's name (a
    // stable per-module identity), so no cross-cutting index bookkeeping is needed — they cannot collide with each
    // other or with user implementations (which are keyed by their own `(ability, pattern)`).
    val desugaredFromData: Seq[(FunctionDefinition, RoleHint)] =
      sourceAstData.typeDefinitions.flatMap(DataDefinitionDesugarer.desugar)
    // Meta-slot brace on a type declaration (`type Int {range: …}`) → the type's `^Meta` constructor
    // (bounds-as-refinements §4.2, Step 4a). Type aliases are `FunctionDefinition`s, so this runs over them.
    val desugaredMetaConstructors: Seq[(FunctionDefinition, RoleHint)] =
      sourceAstData.functionDefinitions.flatMap(MetaConstructorDesugarer.desugar)
    // Return-position transfer brace on a def (`def add(…): Int {a.range + b.range}`) → the def's `^Meta` transfer
    // companion (bounds-as-refinements §4.2, Step 4b).
    val desugaredMetaTransfers: Seq[(FunctionDefinition, RoleHint)] =
      sourceAstData.functionDefinitions.flatMap(MetaTransferDesugarer.desugar)
    // `where <predicate>` precondition on a def (`def useByte(x: Int): Int where withinByte(range(x))`) → the def's
    // `^Where` companion (bounds-as-refinements §4.3), the `Bool` the refinement channel demands at each call site.
    val desugaredWhereCompanions: Seq[(FunctionDefinition, RoleHint)] =
      sourceAstData.functionDefinitions.flatMap(MetaWhereDesugarer.desugar)
    val allFunctions  =
      sourceAstData.functionDefinitions.map(_ -> RoleHint.NoHint) ++
        desugaredFromData ++ desugaredMetaConstructors ++ desugaredMetaTransfers ++ desugaredWhereCompanions
    val coreAstData   = CoreASTData(
      sourceAstData.importStatements,
      // Effect-set sugar (`{E} A`) is collapsed onto a single inferable carrier before the function is converted, so
      // everything downstream sees ordinary HKT-constrained generics (see EffectSugarDesugarer). Each definition splits
      // at birth into its `Runtime` twin and its `Signature` twin (see [[transformFunction]]).
      allFunctions.flatMap { case (fd, hint) => transformFunction(EffectSugarDesugarer.desugar(fd), hint) }
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

  /** Split one definition into its two twins (the signature split):
    *   - the **`Runtime` twin** — today's value: its `runtime` body (`None` iff abstract) and its `signature`
    *     (type) expression, rendering and behaving byte-identically to the pre-split `NamedValue`;
    *   - the **`Signature` twin** — a plain named value whose *body* is the signature expression (always present, never
    *     abstract), sharing the same generic binders (`paramConstraints`). Its own signature is the derived kind, which
    *     is never minted or stored — so its `signature` slot carries an inert placeholder (the body expression) and is
    *     not read until the signature twin gets its own monomorphization (a later step). It is compile-time-only and
    *     participates in name resolution through exactly the same machinery as any other value; nothing resolves *into*
    *     it yet.
    *
    * `dischargedEffects` and the constructor-shape `roleHint` stay on the `Runtime` twin (the effect phase and match
    * reconstruction read the runtime twin today); the signature twin carries neither.
    */
  private def transformFunction(function: FunctionDefinition, roleHint: RoleHint): Seq[NamedValue] = {
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
    val precedence   = function.precedence.map(convertPrecedenceDeclaration)
    val runtimeTwin  = NamedValue(
      function.name,
      curriedValue,
      curriedType,
      constraints,
      function.fixity,
      precedence,
      function.visibility,
      roleHint,
      inferableArity,
      function.dischargedEffects
    )
    // The signature twin: its body IS the signature expression (always present). Its own signature (the kind) is
    // derived on demand and never stored, so the slot repeats the body as an inert placeholder. Same binders,
    // visibility, and fixity as the runtime twin; no `roleHint`/`dischargedEffects` (those describe the runtime value).
    val signatureTwin = NamedValue(
      function.name.map(_.signatureTwin),
      Some(curriedType),
      curriedType,
      constraints,
      function.fixity,
      precedence,
      function.visibility,
      RoleHint.NoHint,
      inferableArity,
      Seq.empty
    )
    Seq(runtimeTwin, signatureTwin)
  }
}
