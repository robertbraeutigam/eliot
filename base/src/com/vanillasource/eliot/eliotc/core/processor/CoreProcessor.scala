package com.vanillasource.eliot.eliotc.core.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.ast.fact.{
  ArgumentDefinition as SourceArgument,
  DataDefinition,
  Expression as SourceExpression,
  FunctionDefinition,
  GenericParameter,
  SourceAST,
  TypeReference
}
import com.vanillasource.eliot.eliotc.core.fact.{AST as CoreASTData, *}
import com.vanillasource.eliot.eliotc.core.fact.Expression.*
import com.vanillasource.eliot.eliotc.core.fact.ExpressionStack.ExpressionStack
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
    CoreAST(sourceAst.file, sourceAst.ast.as(coreAstData)).pure[CompilerIO]
  }

  private def transformFunctions(functions: Seq[FunctionDefinition]): Seq[NamedValue] =
    functions.map(transformFunction)

  private def transformFunction(fn: FunctionDefinition): NamedValue = {
    val curriedType  = buildCurriedType(fn.args, fn.typeDefinition, fn.genericParameters)
    val curriedValue = fn.body.map(body => buildCurriedBody(fn.args, body))
    NamedValue(fn.name, fn.name.as(curriedType), curriedValue)
  }

  /** Builds a curried type: (A, B) -> C becomes Function[A, Function[B, C]] */
  private def buildCurriedType(
      args: Seq[SourceArgument],
      returnType: TypeReference,
      genericParams: Seq[GenericParameter]
  ): ExpressionStack = {
    val returnTypeStack = typeReferenceToStack(returnType)
    args.foldRight(returnTypeStack) { (arg, accType) =>
      val argTypeStack = typeReferenceToStack(arg.typeReference)
      buildFunctionType(argTypeStack, accType)
    }
  }

  /** Builds Function[A, B] type as an expression stack */
  private def buildFunctionType(argType: ExpressionStack, returnType: ExpressionStack): ExpressionStack = {
    // Function type is represented as applying two type arguments to the Function type constructor
    // Function[A, B] = (Function A) B
    val functionRef = Seq(NamedValueReference(Sourced(null, null, "Function"), None))
    val withArg     = Seq(FunctionApplication(Sourced(null, null, functionRef), Sourced(null, null, argType)))
    Seq(FunctionApplication(Sourced(null, null, withArg), Sourced(null, null, returnType)))
  }

  /** Converts a source TypeReference to an ExpressionStack */
  private def typeReferenceToStack(typeRef: TypeReference): ExpressionStack =
    if (typeRef.genericParameters.isEmpty) {
      Seq(NamedValueReference(typeRef.typeName, None))
    } else {
      // Apply type parameters one by one: T[A, B] = ((T A) B)
      typeRef.genericParameters.foldLeft(Seq[Expression](NamedValueReference(typeRef.typeName, None))) { (acc, param) =>
        val paramStack = typeReferenceToStack(param)
        Seq(FunctionApplication(typeRef.typeName.as(acc), typeRef.typeName.as(paramStack)))
      }
    }

  /** Builds curried body: (a, b) -> body becomes a -> (b -> body) */
  private def buildCurriedBody(
      args: Seq[SourceArgument],
      body: Sourced[SourceExpression]
  ): Sourced[Expression] =
    curryLambda(args, transformExpressionToStack(body), body)

  /** Curries parameters into nested FunctionLiterals */
  private def curryLambda(
      params: Seq[SourceArgument],
      body: Sourced[ExpressionStack],
      sourcedContext: Sourced[?]
  ): Sourced[Expression] =
    params
      .foldRight(body) { (param, accBody) =>
        val paramTypeStack = typeReferenceToStack(param.typeReference)
        sourcedContext.as(Seq(FunctionLiteral(param.name, param.name.as(paramTypeStack), accBody)))
      }
      .map(_.head)

  /** Curries arguments into nested FunctionApplications */
  private def curryApplication(
      base: Expression,
      args: Seq[Sourced[SourceExpression]],
      sourcedContext: Sourced[?]
  ): Sourced[Expression] =
    args.foldLeft(sourcedContext.as(base)) { (acc, arg) =>
      val argStack = transformExpressionToStack(arg)
      sourcedContext.as(FunctionApplication(acc.map(e => Seq(e)), argStack))
    }

  /** Transforms source expressions to core expressions with ExpressionStacks */
  private def transformExpression(expr: Sourced[SourceExpression]): Sourced[Expression] =
    expr.value match {
      case SourceExpression.FunctionApplication(name, args) =>
        curryApplication(NamedValueReference(name, None), args, expr)

      case SourceExpression.QualifiedFunctionApplication(moduleName, fnName, args) =>
        curryApplication(NamedValueReference(fnName, Some(moduleName)), args, expr)

      case SourceExpression.FunctionLiteral(params, body) =>
        curryLambda(params, transformExpressionToStack(body), expr)

      case SourceExpression.IntegerLiteral(lit) =>
        expr.as(IntegerLiteral(lit))

      case SourceExpression.StringLiteral(lit) =>
        expr.as(StringLiteral(lit))
    }

  /** Transforms an expression into an ExpressionStack (value at bottom, no type layers yet) */
  private def transformExpressionToStack(expr: Sourced[SourceExpression]): Sourced[ExpressionStack] = {
    val coreExpr = transformExpression(expr)
    coreExpr.map(e => Seq(e))
  }

  private def transformDataDefinitions(dataDefinitions: Seq[DataDefinition]): Seq[NamedValue] =
    dataDefinitions.flatMap(transformDataDefinition)

  /** Transforms a data definition into type value, constructor, and accessors */
  private def transformDataDefinition(dataDef: DataDefinition): Seq[NamedValue] = {
    val typeValue = createTypeValue(dataDef)

    dataDef.fields match {
      case Some(fields) =>
        val constructor = createConstructor(dataDef, fields)
        val accessors   = fields.map(field => createAccessor(dataDef, field))
        Seq(typeValue, constructor) ++ accessors
      case None         =>
        Seq(typeValue)
    }
  }

  /** Creates a named value for the type itself */
  private def createTypeValue(dataDef: DataDefinition): NamedValue = {
    // The type's "type" is Type (or a higher-kinded type for generics)
    // For now, represent it as a simple Type reference
    val typeStack: ExpressionStack = if (dataDef.genericParameters.isEmpty) {
      Seq(NamedValueReference(dataDef.name.as("Type"), None))
    } else {
      // For generic types like Option[A], the kind is Type -> Type
      // Build: Type -> Type -> ... -> Type
      dataDef.genericParameters.foldRight(Seq[Expression](NamedValueReference(dataDef.name.as("Type"), None))) {
        (_, accType) =>
          val typeRef = Seq(NamedValueReference(dataDef.name.as("Type"), None))
          Seq(FunctionApplication(dataDef.name.as(typeRef), dataDef.name.as(accType)))
      }
    }
    // Abstract type - no value
    NamedValue(dataDef.name, dataDef.name.as(typeStack), None)
  }

  /** Creates a constructor function: MyType(a: A, b: B) -> MyType */
  private def createConstructor(dataDef: DataDefinition, fields: Seq[SourceArgument]): NamedValue = {
    val returnType  = buildDataTypeReference(dataDef)
    val curriedType = buildCurriedType(fields, returnType, dataDef.genericParameters)
    // Constructor is abstract (implemented externally)
    NamedValue(dataDef.name, dataDef.name.as(curriedType), None)
  }

  /** Creates an accessor function: field(obj: MyType) -> FieldType */
  private def createAccessor(dataDef: DataDefinition, field: SourceArgument): NamedValue = {
    val objArg      = SourceArgument(
      dataDef.name.as("obj"),
      buildDataTypeReference(dataDef)
    )
    val curriedType = buildCurriedType(Seq(objArg), field.typeReference, dataDef.genericParameters)
    // Accessor is abstract (implemented externally)
    NamedValue(field.name, field.name.as(curriedType), None)
  }

  /** Builds a TypeReference for the data type with its generic parameters */
  private def buildDataTypeReference(dataDef: DataDefinition): TypeReference =
    TypeReference(
      dataDef.name,
      dataDef.genericParameters.map(gp => TypeReference(gp.name, gp.genericParameters))
    )
}
