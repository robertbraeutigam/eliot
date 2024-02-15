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
    case ast.FunctionBody.NonNative(args, body)       => resolveNonNativeFunction(ffqn, dictionary, args, body)

  private def resolveNonNativeFunction(
      ffqn: FunctionFQN,
      dictionary: Map[String, FunctionFQN],
      args: Seq[Sourced[Token]],
      body: Tree[ast.Expression]
  )(using process: CompilationProcess): IO[Unit] = for {
    _          <- debug(s"resolving $ffqn")
    optionTree <- body.map(expr => resolveExpression(dictionary, expr)).sequence
  } yield {
    optionTree.sequence match
      case Some(tree) =>
        debug(s"resolved $ffqn to: $tree") >>
          process.registerFact(ResolvedFunction(ffqn, FunctionBody.NonNative(args.map(_.map(_.content)), tree)))
      case None       => IO.unit
  }

  private def resolveExpression(dictionary: Map[String, FunctionFQN], expr: ast.Expression)(using
      process: CompilationProcess
  ): IO[Option[Expression]] =
    expr match
      case ast.Expression.FunctionApplication(s @ Sourced(_, _, token))                  =>
        dictionary.get(token.content) match
          case Some(ffqn) => Some(Expression.FunctionApplication(s.as(ffqn))).pure
          case None       => compilerError(s.as(s"Function not defined.")) >> None.pure
      case ast.Expression.IntegerLiteral(s @ Sourced(_, _, Token.IntegerLiteral(value))) =>
        Some(Expression.IntegerLiteral(s.as(value))).pure
      case ast.Expression.IntegerLiteral(s)                                              =>
        compilerError(s.as(s"Internal compiler error, not parsed as an integer literal.")) >> None.pure
}
