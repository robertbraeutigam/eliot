package com.vanillasource.eliot.eliotc.resolve.processor

import cats.data.OptionT
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.{FunctionFQN, ModuleData, ModuleFunction, TypeFQN}
import com.vanillasource.eliot.eliotc.resolve.fact.GenericParameter.UniversalGenericParameter
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.*
import com.vanillasource.eliot.eliotc.resolve.fact.*
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.source.SourcedError.registerCompilerError
import com.vanillasource.eliot.eliotc.token.Token
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor, ast}
import com.vanillasource.util.CatsOps.*

class FunctionResolver extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match
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
      ).value.void
    case _ => IO.unit

  private def process(
      ffqn: FunctionFQN,
      functionDictionary: Map[String, FunctionFQN],
      typeDictionary: Map[String, TypeFQN],
      name: Sourced[Token],
      genericParameters: Seq[ast.GenericParameter],
      args: Seq[ast.ArgumentDefinition],
      typeReference: ast.TypeReference,
      body: Option[ast.Expression]
  )(using process: CompilationProcess): OptionT[IO, Unit] = {
    val scope = ResolverScope(
      functionDictionary,
      typeDictionary,
      genericParameters.map(gp => gp.name.value.content -> gp).toMap,
      args.map(arg => arg.name.value.content -> arg).toMap
    )

    for {
      resolvedBody              <- body.traverse(expr => resolveExpression(scope, expr))
      returnType                <- resolveType(scope, typeReference)
      argumentTypes             <- args.map(_.typeReference).traverse(tr => resolveType(scope, tr))
      resolvedGenericParameters <-
        genericParameters.traverse(genericParameter =>
          genericParameter.genericParameters
            .traverse(typeReference => resolveType(scope, typeReference))
            .map(resolvedGenericTypes =>
              UniversalGenericParameter(genericParameter.name.map(_.content), resolvedGenericTypes)
            )
        )
      _                         <-
        process
          .registerFact(
            ResolvedFunction(
              ffqn,
              FunctionDefinition(
                name.map(_.content),
                resolvedGenericParameters,
                args
                  .zip(argumentTypes)
                  .map((argDef, argType) => ArgumentDefinition(argDef.name.map(_.content), argType)),
                returnType,
                resolvedBody
              )
            )
          )
          .liftOptionT
    } yield ()
  }

  private def resolveType(
      scope: ResolverScope,
      reference: ast.TypeReference
  )(using
      process: CompilationProcess
  ): OptionT[IO, TypeReference] = for {
    resolvedGenericParameters <-
      reference.genericParameters.traverse(param => resolveType(scope, param))
    resolvedType              <- scope.visibleGenericTypes.get(reference.typeName.value.content) match
                                   case Some(genericParameter) =>
                                     if (genericParameter.genericParameters.length =!= resolvedGenericParameters.length) {
                                       registerCompilerError(
                                         reference.typeName.as("Incorrect number of generic parameters for type.")
                                       ).liftOptionTNone
                                     } else {
                                       GenericTypeReference(genericParameter.name.map(_.content), resolvedGenericParameters)
                                         .pure[IO]
                                         .liftOptionT
                                     }
                                   case None                   =>
                                     scope.typeDictionary.get(reference.typeName.value.content) match
                                       case Some(typeFqn) =>
                                         for {
                                           dataDefinition <- process.getFact(ModuleData.Key(typeFqn)).toOptionT
                                           _              <-
                                             registerCompilerError(
                                               reference.typeName.as("Incorrect number of generic parameters for type.")
                                             ).liftOptionTNone
                                               .whenA(
                                                 dataDefinition.dataDefinition.genericParameters.length =!= resolvedGenericParameters.length
                                               )
                                         } yield DirectTypeReference(reference.typeName.as(typeFqn), resolvedGenericParameters)
                                       case None          => registerCompilerError(reference.typeName.as("Type not defined.")).liftOptionTNone
  } yield resolvedType

  private def resolveExpression(
      scope: ResolverScope,
      expr: ast.Expression
  )(using
      process: CompilationProcess
  ): OptionT[IO, Expression] =
    expr match {
      case ast.Expression.FunctionApplication(s @ Sourced(_, _, token), _) if scope.isValueVisible(token.content) =>
        Expression.ParameterReference(s.as(token.content)).pure
      case ast.Expression.FunctionApplication(s @ Sourced(_, _, token), args)                                     =>
        scope.functionDictionary.get(token.content) match
          case Some(ffqn) =>
            for {
              newArgs <- args.traverse(arg => resolveExpression(scope, arg))
            } yield Expression.FunctionApplication(s.as(ffqn), newArgs)
          case None       => registerCompilerError(s.as(s"Function not defined.")).liftOptionTNone
      case ast.Expression.FunctionLiteral(parameters, body)                                                       =>
        for {
          resolvedParameters <-
            parameters
              .traverse(arg =>
                resolveType(scope, arg.typeReference).map(resolvedType =>
                  ArgumentDefinition(arg.name.map(_.content), resolvedType)
                )
              )
          resolvedExpression <- resolveExpression(scope, body)
        } yield Expression.FunctionLiteral(resolvedParameters, resolvedExpression)
      case ast.Expression.IntegerLiteral(s @ Sourced(_, _, Token.IntegerLiteral(value)))                          =>
        Expression.IntegerLiteral(s.as(value)).pure
      case ast.Expression.IntegerLiteral(s)                                                                       =>
        registerCompilerError(s.as(s"Internal compiler error, not parsed as an integer literal.")).liftOptionTNone
    }
}
