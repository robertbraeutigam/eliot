package com.vanillasource.eliot.eliotc.core.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.GenericParameter.Arity
import com.vanillasource.eliot.eliotc.ast.fact.{
  ArgumentDefinition,
  DataDefinition,
  FunctionDefinition,
  GenericParameter,
  SourceAST,
  TypeReference,
  Fixity as AstFixity,
  PrecedenceDeclaration as AstPrecedenceDeclaration,
  LambdaParameterDefinition as SourceLambdaParameter,
  ArgumentDefinition as SourceArgument,
  Expression as SourceExpression,
  QualifiedName as AstQualifiedName,
  Qualifier as AstQualifier
}
import com.vanillasource.eliot.eliotc.core.fact.{AST as CoreASTData, *}
import com.vanillasource.eliot.eliotc.core.fact.Expression.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced

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
    val coreAstData   = CoreASTData(
      sourceAstData.importStatements,
      transformFunctions(sourceAstData.functionDefinitions) ++ transformDataDefinitions(sourceAstData.typeDefinitions)
    )

    debug[CompilerIO](
      s"Core functions in ${key.uri}: ${coreAstData.namedValues.map(_.show).mkString(", ")}"
    ) >>
      CoreAST(sourceAst.uri, sourceAst.ast.as(coreAstData)).pure[CompilerIO]
  }

  private def transformFunctions(functions: Seq[FunctionDefinition]): Seq[NamedValue] =
    functions.map(transformFunction)

  private def transformFunction(function: FunctionDefinition): NamedValue = {
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
      curriedValue.map(_.value),
      typeStack,
      constraints,
      function.fixity,
      function.precedence.map(convertPrecedenceDeclaration)
    )
  }

  private def convertQualifiedName(name: Sourced[AstQualifiedName]): Sourced[QualifiedName] =
    name.map(n => QualifiedName(n.name, convertQualifier(n.qualifier)))

  private def convertQualifier(qualifier: AstQualifier): Qualifier =
    qualifier match {
      case AstQualifier.Default                           => Qualifier.Default
      case AstQualifier.Type                              => Qualifier.Type
      case AstQualifier.Ability(n)                        => Qualifier.Ability(n)
      case AstQualifier.AbilityImplementation(n, pattern) =>
        Qualifier.AbilityImplementation(
          n,
          pattern.map(toTypeExpression(_).value)
        )
    }

  /** Builds a curried function type. So f[A, B](d: D, e: E): F type becomes: A -> B -> Function^Type(D,
    * Function^Type(E, F)). Note: f[A, M[_]]... becomes: A -> M -> ..., where M has a type expression on it: X -> Y \->
    * Function^Type(X, Y)
    */
  private def curriedFunctionType(
      args: Seq[SourceArgument],
      returnType: TypeReference,
      genericParams: Seq[GenericParameter]
  ): Sourced[Expression] = {
    val withArgs = args.foldRight[Sourced[Expression]](toTypeExpression(returnType)) { (arg, acc) =>
      val argType     = toTypeExpression(arg.typeReference)
      val functionRef =
        arg.name.as(NamedValueReference(arg.name.as(QualifiedName("Function", Qualifier.Type))))
      val withArgType = arg.name.as(
        FunctionApplication(
          functionRef.map(TypeStack.of),
          argType.map(TypeStack.of)
        )
      )
      arg.name.as(
        FunctionApplication(
          withArgType.map(TypeStack.of),
          acc.map(TypeStack.of)
        )
      )
    }
    genericParams.foldRight[Sourced[Expression]](withArgs) { (param, acc) =>
      val kindType = toKindExpression(param.name, param.arity)
      param.name.as(FunctionLiteral(param.name, Some(TypeStack.of(kindType.value)), acc.map(TypeStack.of)))
    }
  }

  /** Builds a kind expression for a generic parameter. For simple types (no params), returns Type. For higher-kinded
    * types, returns a curried function of kinds ending in Type.
    *
    * Examples:
    *   - _: Type
    *   - F[_]: Function[Type, Type]
    *   - F[_, _]: Function[Type, Function[Type, Type]]
    *   - F[_[_]]: Function[Function[Type, Type], Type] (F takes a type constructor)
    *   - F[_[_, _]]: Function[Function[Type, Function[Type, Type]], Type]
    */
  // FIXME: doesn't need empty case
  private def toKindExpression(source: Sourced[?], arity: Arity): Sourced[Expression] =
    if (arity.parameters.isEmpty) {
      source.as(NamedValueReference(source.as(QualifiedName("Type", Qualifier.Default))))
    } else {
      val typeRef = source.as(NamedValueReference(source.as(QualifiedName("Type", Qualifier.Default))))
      arity.parameters.foldRight[Sourced[Expression]](typeRef) { (param, acc) =>
        // Recursively compute the kind of this parameter based on its own nested generic params
        val paramKind = toKindExpression(source, param)
        buildFunctionKind(source, paramKind, acc)
      }
    }

  /** Builds Function[argKind, resultKind] expression. */
  private def buildFunctionKind(
      source: Sourced[?],
      argKind: Sourced[Expression],
      resultKind: Sourced[Expression]
  ): Sourced[Expression] = {
    val functionRef = source.as(NamedValueReference(source.as(QualifiedName("Function", Qualifier.Type))))
    val withArgType = source.as(
      FunctionApplication(
        functionRef.map(TypeStack.of),
        argKind.map(TypeStack.of)
      )
    )
    source.as(
      FunctionApplication(
        withArgType.map(TypeStack.of),
        resultKind.map(TypeStack.of)
      )
    )
  }

  /** Converts type references to type expressions. Type references are in the form of: A[B[C...],...], which is
    * converted into an expression: A(B(C...),...), so function applications.
    */
  private def toTypeExpression(reference: TypeReference): Sourced[Expression] =
    reference.genericParameters.foldLeft[Sourced[Expression]](
      reference.typeName.as(NamedValueReference(reference.typeName.map(n => QualifiedName(n, Qualifier.Type))))
    ) { (acc, ref) =>
      ref.typeName.as(
        FunctionApplication(acc.map(TypeStack.of), toTypeExpression(ref).map(TypeStack.of))
      )
    }

  /** Converts the body into core expression and embeds it as a lambda with the "function" parameters.
    */
  private def buildCurriedBody(
      args: Seq[SourceArgument],
      value: Sourced[SourceExpression]
  ): Sourced[Expression] =
    args.foldRight(toBodyExpression(value)) { (arg, acc) =>
      arg.name.as(
        FunctionLiteral(
          arg.name,
          Some(TypeStack.of(toTypeExpression(arg.typeReference).value)),
          acc.map(TypeStack.of)
        )
      )
    }

  private def toBodyExpression(expr: Sourced[SourceExpression]): Sourced[Expression] =
    expr.value match {
      case SourceExpression.FunctionApplication(moduleName, fnName, genericArgs, args) =>
        curryApplication(
          expr.as(
            NamedValueReference(
              fnName.map(n => QualifiedName(n, Qualifier.Default)),
              moduleName,
              genericArgs.map(toTypeExpression)
            )
          ),
          args
        )
      case SourceExpression.FunctionLiteral(params, body)                              =>
        curryLambda(params, toBodyExpression(body), expr)
      case SourceExpression.IntegerLiteral(lit)                                        =>
        expr.as(IntegerLiteral(lit))
      case SourceExpression.StringLiteral(lit)                                         =>
        expr.as(StringLiteral(lit))
      case SourceExpression.FlatExpression(Seq(single))                                =>
        toBodyExpression(single)
      case SourceExpression.FlatExpression(parts)                                      =>
        expr.as(FlatExpression(parts.map(p => p.as(TypeStack.of(toBodyExpression(p).value)))))
    }

  private def convertPrecedenceDeclaration(pd: AstPrecedenceDeclaration): PrecedenceDeclaration =
    PrecedenceDeclaration(pd.relation, pd.targets)

  /** Curries lambda expressions into core format. In core, lambdas have exactly one argument, so a lambda: (a,b,c) ->
    * body, needs to be converted into: a -> b -> c -> body.
    */
  private def curryLambda(
      params: Seq[SourceLambdaParameter],
      body: Sourced[Expression],
      sourcedContext: Sourced[?]
  ): Sourced[Expression] =
    params.foldRight(body) { (param, acc) =>
      param.name.as(
        FunctionLiteral(
          param.name,
          param.typeReference.map(toTypeExpression(_).value).map(TypeStack.of),
          acc.map(TypeStack.of)
        )
      )
    }

  /** Convert function applications into core format. Applications need to be curried, so: f(a, b, c) becomes:
    * f(a)(b)(c)
    */
  private def curryApplication(
      target: Sourced[Expression],
      args: Seq[Sourced[SourceExpression]]
  ): Sourced[Expression] =
    args.foldLeft(target) { (acc, arg) =>
      arg.as(
        FunctionApplication(acc.map(TypeStack.of), toBodyExpression(arg).map(TypeStack.of))
      )
    }

  private def transformDataDefinitions(definitions: Seq[DataDefinition]): Seq[NamedValue] =
    definitions.flatMap(transformDataDefinition)

  /** Transform data definitions into the core model. Data definitions don't have any special mechanisms in the core
    * model, they need to be translated into "normal" named values. Specifically 3 things:
    *   - Abstract function that returns the type of the type. Example: IO[A] will have type: A -> Type
    *   - Abstract constructor function. So "data Person(name: String, age: Int)" type becomes: Function(String,
    *     Function(Int, DataType))
    *   - Abstract accessor functions for all fields. Types are Function(DataType, FieldType)
    */
  private def transformDataDefinition(definition: DataDefinition): Seq[NamedValue] =
    createTypeFunction(definition) ++ createConstructor(definition) ++ createAccessors(definition)

  private def createTypeFunction(definition: DataDefinition): Seq[NamedValue] =
    Seq(
      transformFunction(
        FunctionDefinition(
          definition.name.map(n => AstQualifiedName(n, AstQualifier.Type)),
          definition.genericParameters,
          Seq.empty,
          TypeReference(definition.name.as("Type"), Seq.empty),
          None
        )
      )
    )

  /** Note: we only create a constructor if fields are present. Else the data type is abstract and we can't create it
    * anyway.
    *
    * The body is a self-referential call: Box(fieldA)(fieldB)... This is never evaluated - JvmClassGenerator recognizes
    * constructors and generates native bytecode. The purpose is to preserve field names as lambda parameter names
    * through the pipeline.
    */
  private def createConstructor(definition: DataDefinition): Seq[NamedValue] = {
    definition.fields
      .map(fields =>
        Seq(
          transformFunction(
            FunctionDefinition(
              definition.name.map(n => AstQualifiedName(n, AstQualifier.Default)), // Constructor name
              definition.genericParameters,
              fields,
              TypeReference(
                definition.name, // Type name
                definition.genericParameters.map(gp => TypeReference(gp.name, Seq.empty))
              ),
              Some(
                definition.name.as(
                  SourceExpression.FunctionApplication(
                    None,
                    definition.name,
                    Seq.empty,
                    fields.map(f => f.name.as(SourceExpression.FunctionApplication(None, f.name, Seq.empty, Seq.empty)))
                  )
                )
              )
            )
          )
        )
      )
      .getOrElse(Seq.empty)
  }

  private def createAccessors(definition: DataDefinition): Seq[NamedValue] =
    definition.fields
      .getOrElse(Seq.empty)
      .map { field =>
        transformFunction(
          FunctionDefinition(
            field.name.map(n => AstQualifiedName(n, AstQualifier.Default)),
            definition.genericParameters,
            Seq(
              ArgumentDefinition(
                field.name.as("obj"),
                TypeReference(
                  definition.name,
                  definition.genericParameters.map(gp => TypeReference(gp.name, Seq.empty))
                )
              )
            ),
            field.typeReference,
            None
          )
        )
      }
}
