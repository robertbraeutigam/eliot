package com.vanillasource.eliot.eliotc.resolve

import cats.data.OptionT
import com.vanillasource.util.CatsOps.*
import com.vanillasource.eliot.eliotc.ast
import cats.effect.IO
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.source.Sourced
import cats.syntax.all.*
import com.vanillasource.collections.Tree
import com.vanillasource.eliot.eliotc.module.{FunctionFQN, ModuleFunction, TypeFQN}
import com.vanillasource.eliot.eliotc.source.SourcedError.registerCompilerError
import com.vanillasource.eliot.eliotc.token.Token
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}

class FunctionResolver extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match
    case ModuleFunction(
          ffqn,
          functionDictionary,
          typeDictionary,
          ast.FunctionDefinition(name, args, typeDefinition, body)
        ) =>
      process(ffqn, functionDictionary, typeDictionary, name, args, typeDefinition, body).value.void
    case _ => IO.unit

  private def process(
      ffqn: FunctionFQN,
      functionDictionary: Map[String, FunctionFQN],
      typeDictionary: Map[String, TypeFQN],
      name: Sourced[Token],
      args: Seq[ast.ArgumentDefinition],
      typeReference: ast.TypeReference,
      body: Tree[ast.Expression]
  )(using process: CompilationProcess): OptionT[IO, Unit] = for {
    tree          <- body.map(expr => resolveExpression(functionDictionary, expr)).sequence
    returnType    <- resolveType(typeReference, typeDictionary)
    argumentTypes <- args.map(_.typeReference).map(tr => resolveType(tr, typeDictionary)).sequence
    _             <-
      process
        .registerFact(
          ResolvedFunction(
            ffqn,
            FunctionDefinition(
              name.map(_.content),
              args
                .zip(argumentTypes)
                .map((argDef, argType) => ArgumentDefinition(argDef.name.map(_.content), argType)),
              returnType,
              tree
            )
          )
        )
        .liftOptionT
  } yield ()

  private def resolveType(reference: ast.TypeReference, typeDictionary: Map[String, TypeFQN])(using
      process: CompilationProcess
  ): OptionT[IO, Sourced[TypeFQN]] = typeDictionary.get(reference.typeName.value.content) match
    case Some(typeFQN) => reference.typeName.as(typeFQN).pure
    case None          => registerCompilerError(reference.typeName.as("Type not defined.")).liftOptionTNone

  private def resolveExpression(dictionary: Map[String, FunctionFQN], expr: ast.Expression)(using
      process: CompilationProcess
  ): OptionT[IO, Expression] =
    expr match
      case ast.Expression.FunctionApplication(s @ Sourced(_, _, token))                  =>
        dictionary.get(token.content) match
          case Some(ffqn) => Expression.FunctionApplication(s.as(ffqn)).pure
          case None       => registerCompilerError(s.as(s"Function not defined.")).liftOptionTNone
      case ast.Expression.IntegerLiteral(s @ Sourced(_, _, Token.IntegerLiteral(value))) =>
        Expression.IntegerLiteral(s.as(value)).pure
      case ast.Expression.IntegerLiteral(s)                                              =>
        registerCompilerError(s.as(s"Internal compiler error, not parsed as an integer literal.")).liftOptionTNone
}
