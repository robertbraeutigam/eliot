package com.vanillasource.eliot.eliotc.resolve

import com.vanillasource.eliot.eliotc.ast
import cats.effect.IO
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.source.Sourced
import cats.syntax.all.*
import com.vanillasource.collections.Tree
import com.vanillasource.eliot.eliotc.module.{FunctionFQN, ModuleFunction}
import com.vanillasource.eliot.eliotc.source.SourcedError.compilerError
import com.vanillasource.eliot.eliotc.token.Token
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}

class FunctionResolver extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match
    case ModuleFunction(ffqn, dictionary, functionDefinition) => process(ffqn, dictionary, functionDefinition)
    case _                                                    => IO.unit

  private def process(
      ffqn: FunctionFQN,
      dictionary: Map[String, FunctionFQN],
      definition: ast.FunctionDefinition
  )(using process: CompilationProcess): IO[Unit] = definition.body match
    case ast.FunctionBody.Native(nativeKeyword, args) =>
      process.registerFact(ResolvedFunction(ffqn, FunctionBody.Native(nativeKeyword.void, args.map(_.map(_.content)))))
    case ast.FunctionBody.NonNative(args, body)       => resolveNonNativeFunction(args, body)

  private def resolveNonNativeFunction(args: Seq[Sourced[Token]], body: Tree[ast.Expression]): IO[Unit] = ???

  private def resolveExpression(dictionary: Map[String, FunctionFQN], expr: ast.Expression)(using
      process: CompilationProcess
  ): IO[Option[Expression]] =
    expr match
      case ast.Expression.FunctionApplication(functionName)                              => ???
      case ast.Expression.IntegerLiteral(s @ Sourced(_, _, Token.IntegerLiteral(value))) =>
        Some(Expression.IntegerLiteral(s.as(value))).pure
      case ast.Expression.IntegerLiteral(s)                                              =>
        compilerError(s.as(s"Internal compiler error, not parsed as an integer literal.")) >> None.pure
}
