package com.vanillasource.eliot.eliotc.resolve

import cats.data.OptionT
import com.vanillasource.util.CatsOps.*
import com.vanillasource.eliot.eliotc.ast
import cats.effect.IO
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.source.Sourced
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.{FunctionFQN, ModuleFunction, TypeFQN}
import com.vanillasource.eliot.eliotc.resolve.TypeReference.{DirectTypeReference, GenericTypeReference}
import com.vanillasource.eliot.eliotc.source.SourcedError.registerCompilerError
import com.vanillasource.eliot.eliotc.token.Token
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}

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
        genericParameters.groupMapReduce(_.value.content)(_.map(_.content))((left, _) => left),
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
      genericParameters: Map[String, Sourced[String]],
      args: Seq[ast.ArgumentDefinition],
      typeReference: ast.TypeReference,
      body: Option[ast.Expression]
  )(using process: CompilationProcess): OptionT[IO, Unit] = for {
    resolvedBody  <- body.map(expr => resolveExpression(functionDictionary, expr, args)).sequence
    returnType    <- resolveType(typeReference, genericParameters, typeDictionary)
    argumentTypes <- args.map(_.typeReference).map(tr => resolveType(tr, genericParameters, typeDictionary)).sequence
    _             <-
      process
        .registerFact(
          ResolvedFunction(
            ffqn,
            FunctionDefinition(
              name.map(_.content),
              genericParameters.values.toSeq,
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

  private def resolveType(
      reference: ast.TypeReference,
      genericParameters: Map[String, Sourced[String]],
      typeDictionary: Map[String, TypeFQN]
  )(using
      process: CompilationProcess
  ): OptionT[IO, TypeReference] =
    genericParameters.get(reference.typeName.value.content) match
      case Some(genericParameter) => GenericTypeReference(genericParameter).pure
      case None                   =>
        typeDictionary.get(reference.typeName.value.content) match
          case Some(typeFQN) => DirectTypeReference(reference.typeName.as(typeFQN)).pure
          case None          => registerCompilerError(reference.typeName.as("Type not defined.")).liftOptionTNone

  private def resolveExpression(
      dictionary: Map[String, FunctionFQN],
      expr: ast.Expression,
      callArgs: Seq[ast.ArgumentDefinition]
  )(using
      process: CompilationProcess
  ): OptionT[IO, Expression] =
    expr match {
      case ast.Expression.FunctionApplication(s @ Sourced(_, _, token), _)
          if callArgs.map(_.name.value.content).contains(token.content) =>
        Expression.ParameterReference(s.as(token.content)).pure
      case ast.Expression.FunctionApplication(s @ Sourced(_, _, token), args)            =>
        dictionary.get(token.content) match
          case Some(ffqn) =>
            for {
              newArgs <- args.map(arg => resolveExpression(dictionary, arg, callArgs)).sequence
            } yield Expression.FunctionApplication(s.as(ffqn), newArgs)
          case None       => registerCompilerError(s.as(s"Function not defined.")).liftOptionTNone
      case ast.Expression.IntegerLiteral(s @ Sourced(_, _, Token.IntegerLiteral(value))) =>
        Expression.IntegerLiteral(s.as(value)).pure
      case ast.Expression.IntegerLiteral(s)                                              =>
        registerCompilerError(s.as(s"Internal compiler error, not parsed as an integer literal.")).liftOptionTNone
    }
}
