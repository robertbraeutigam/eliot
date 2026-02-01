package com.vanillasource.eliot.eliotc.core.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.{
  ArgumentDefinition,
  DataDefinition,
  FunctionDefinition,
  GenericParameter,
  SourceAST,
  TypeReference,
  ArgumentDefinition as SourceArgument,
  Expression as SourceExpression
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
    extends TransformationProcessor[SourceAST.Key, CoreAST.Key](key => SourceAST.Key(key.file))
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
      s"Core functions in ${key.file}: ${coreAstData.namedValues.map(_.show).mkString(", ")}"
    ) >>
      CoreAST(sourceAst.file, sourceAst.ast.as(coreAstData)).pure[CompilerIO]
  }

  private def transformFunctions(functions: Seq[FunctionDefinition]): Seq[NamedValue] =
    functions.map(transformFunction)

  private def transformFunction(function: FunctionDefinition): NamedValue = {
    val curriedType  = curriedFunctionType(function.args, function.typeDefinition, function.genericParameters)
    val curriedValue = function.body.map(body => buildCurriedBody(function.args, body))
    val typeStack    = TypeStack.of(curriedType.value)
    NamedValue(function.name, curriedValue.map(_.value), typeStack)
  }

  /** Builds a curried function type. So f[A, B](d: D, e: E): F type becomes: A -> B -> Function$DataType(D,
    * Function$DataType(E, F)). Note: f[A, M[_]]... becomes: A -> M -> ..., where M has a type expression on it: X -> Y
    * \-> Function$DataType(X, Y)
    */
  private def curriedFunctionType(
      args: Seq[SourceArgument],
      returnType: TypeReference,
      genericParams: Seq[GenericParameter]
  ): Sourced[Expression] = {
    val withArgs = args.foldRight[Sourced[Expression]](toTypeExpression(returnType)) { (arg, acc) =>
      val argType     = toTypeExpression(arg.typeReference)
      val functionRef =
        arg.name.as(NamedValueReference(arg.name.as("Function")))
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
      // TODO: Generic parametes are either Type or some Function requiring other Types and return a Type
      param.name.as(FunctionLiteral(param.name, TypeStack.empty, acc.map(TypeStack.of)))
    }
  }

  /** Converts type references to type expressions. Type references are in the form of: A[B[C...],...], which is
    * converted into an expression: A(B(C...),...), so function applications.
    */
  private def toTypeExpression(reference: TypeReference): Sourced[Expression] =
    reference.genericParameters.foldLeft[Sourced[Expression]](
      reference.typeName.as(NamedValueReference(reference.typeName))
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
          TypeStack.of(toTypeExpression(arg.typeReference).value),
          acc.map(TypeStack.of)
        )
      )
    }

  private def toBodyExpression(expr: Sourced[SourceExpression]): Sourced[Expression] =
    expr.value match {
      case SourceExpression.FunctionApplication(name, args)                        =>
        curryApplication(expr.as(NamedValueReference(name, None)), args)
      case SourceExpression.QualifiedFunctionApplication(moduleName, fnName, args) =>
        curryApplication(expr.as(NamedValueReference(fnName, Some(moduleName))), args)
      case SourceExpression.FunctionLiteral(params, body)                          =>
        curryLambda(params, toBodyExpression(body), expr)
      case SourceExpression.IntegerLiteral(lit)                                    =>
        expr.as(IntegerLiteral(lit))
      case SourceExpression.StringLiteral(lit)                                     =>
        expr.as(StringLiteral(lit))
    }

  /** Curries lambda expressions into core format. In core, lambdas have exactly one argument, so a lambda: (a,b,c) ->
    * body, needs to be converted into: a -> b -> c -> body.
    */
  private def curryLambda(
      params: Seq[SourceArgument],
      body: Sourced[Expression],
      sourcedContext: Sourced[?]
  ): Sourced[Expression] =
    params.foldRight(body) { (param, acc) =>
      param.name.as(
        FunctionLiteral(
          param.name,
          TypeStack.of(toTypeExpression(param.typeReference).value),
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
          definition.name.map(_ + "$DataType"),
          Seq.empty,
          definition.genericParameters.map(gp =>
            ArgumentDefinition(gp.name, TypeReference(definition.name.as("Type"), gp.genericParameters))
          ),
          TypeReference(definition.name.as("Type"), Seq.empty),
          None
        )
      )
    )

  /** Note: we only create a constructor if fields are present. Else the data type is abstract and we can't create it
    * anyway.
    */
  private def createConstructor(definition: DataDefinition): Seq[NamedValue] = {
    definition.fields
      .map(fields =>
        Seq(
          transformFunction(
            FunctionDefinition(
              definition.name,
              definition.genericParameters,
              fields,
              TypeReference(definition.name.map(_ + "$DataType"), Seq.empty), // The "type" name value
              None
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
            field.name,
            definition.genericParameters,
            Seq(
              ArgumentDefinition(field.name.as("obj"), TypeReference(definition.name.map(_ + "$DataType"), Seq.empty))
            ),
            field.typeReference,
            None
          )
        )
      }
}
