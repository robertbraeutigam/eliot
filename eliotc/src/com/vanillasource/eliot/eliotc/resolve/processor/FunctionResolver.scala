package com.vanillasource.eliot.eliotc.resolve.processor

import cats.data.OptionT
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleData, ModuleFunction, TypeFQN}
import com.vanillasource.eliot.eliotc.resolve.fact.*
import com.vanillasource.eliot.eliotc.resolve.fact.Expression.FunctionLiteral
import com.vanillasource.eliot.eliotc.resolve.fact.GenericParameter.UniversalGenericParameter
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.*
import com.vanillasource.eliot.eliotc.resolve.processor.ResolverScope.*
import com.vanillasource.eliot.eliotc.source.error.CompilationIO.*
import com.vanillasource.eliot.eliotc.source.pos.Sourced
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerFactKey, CompilerProcessor, ast}

class FunctionResolver extends CompilerProcessor with Logging {
  override def generate(factKey: CompilerFactKey)(using process: CompilationProcess): IO[Unit] = factKey match {
    case ResolvedFunction.Key(ffqn) =>
      process.getFact(ModuleFunction.Key(ffqn)).flatMap(_.traverse_(processFact))
    case _                          => IO.unit
  }
  private def processFact(fact: CompilerFact)(using CompilationProcess): IO[Unit]              = fact match
    case ModuleFunction(
          ffqn,
          functionDictionary,
          typeDictionary,
          ast.FunctionDefinition(name, genericParameters, args, typeDefinition, body)
        ) =>
      process(
        ffqn,
        functionDictionary,
        typeDictionary,
        name,
        genericParameters,
        args,
        typeDefinition,
        body
      ).runCompilation_()
    case _ => IO.unit

  private def process(
      ffqn: FunctionFQN,
      functionDictionary: Map[String, FunctionFQN],
      typeDictionary: Map[String, TypeFQN],
      name: Sourced[String],
      genericParameters: Seq[ast.GenericParameter],
      args: Seq[ast.ArgumentDefinition],
      typeReference: ast.TypeReference,
      body: Option[Sourced[ast.Expression]]
  )(using process: CompilationProcess): CompilationIO[Unit] = {
    val scope = ResolverScope(
      functionDictionary,
      typeDictionary,
      genericParameters.map(gp => gp.name.value -> gp).toMap,
      args.map(arg => arg.name.value -> arg).toMap
    )

    val resolveProgram = for {
      resolvedBodyMaybe         <- body.traverse(resolveExpression)
      returnType                <- TypeResolver.resolveType(typeReference)
      argumentTypes             <- args.map(_.typeReference).traverse(TypeResolver.resolveType)
      argumentDefinitions        = args.zip(argumentTypes).map((argDef, argType) => ArgumentDefinition(argDef.name, argType))
      resolvedGenericParameters <-
        genericParameters.traverse(genericParameter =>
          genericParameter.genericParameters
            .traverse(TypeResolver.resolveType)
            .map(resolvedGenericTypes => UniversalGenericParameter(genericParameter.name, resolvedGenericTypes))
        )
      _                         <-
        process
          .registerFact(
            ResolvedFunction(
              ffqn,
              FunctionDefinition(
                name,
                resolvedGenericParameters,
                // Unroll return type into Function[A, Function[B, ...]]
                argumentDefinitions.foldRight(returnType)((arg, typ) =>
                  DirectTypeReference(
                    typeReference.typeName.as(TypeFQN.systemFunctionType),
                    Seq(arg.typeReference, typ)
                  )
                ),
                // Unroll body to use function literals: arg1 -> arg2 -> ... -> body
                resolvedBodyMaybe.map(resolvedBody =>
                  argumentDefinitions.foldRight(resolvedBody)((arg, expr) => expr.as(FunctionLiteral(arg, expr)))
                )
              )
            )
          )
          .liftToCompilationIO
          .liftToScoped
    } yield ()

    resolveProgram.runS(scope).void
  }

  private def resolveExpression(
      expr: Sourced[ast.Expression]
  )(using process: CompilationProcess): ScopedIO[Sourced[Expression]] =
    expr.value match {
      case ast.Expression.FunctionApplication(s @ Sourced(_, _, name), args) =>
        isValueVisible(name).ifM(
          expr.as(Expression.ParameterReference(s.as(name))).pure,
          getFunction(name).flatMap {
            case Some(ffqn) =>
              for {
                newArgs <- args.traverse(resolveExpression)
              } yield expr.as(
                newArgs.foldRight[Expression](Expression.ValueReference(s.as(ffqn)))((arg, expr) =>
                  Expression.FunctionApplication(s.as(expr), arg)
                )
              )
            case None       => compilerAbort(s.as(s"Function not defined.")).liftToScoped
          }
        )
      case ast.Expression.FunctionLiteral(parameters, body)                  =>
        for {
          resolvedParameters <-
            parameters
              .traverse(arg =>
                TypeResolver
                  .resolveType(arg.typeReference)
                  .map(resolvedType => ArgumentDefinition(arg.name, resolvedType))
              )
          _                  <- parameters.traverse(addVisibleValue)
          resolvedBody       <- resolveExpression(body)
        } yield resolvedParameters.foldRight(resolvedBody)((arg, expr) =>
          expr.as(Expression.FunctionLiteral(arg, expr))
        )
      case ast.Expression.IntegerLiteral(s @ Sourced(_, _, value))           =>
        expr.as(Expression.IntegerLiteral(s.as(BigInt(value)))).pure
      case ast.Expression.StringLiteral(s @ Sourced(_, _, value))            =>
        expr.as(Expression.StringLiteral(s.as(value))).pure
    }
}
