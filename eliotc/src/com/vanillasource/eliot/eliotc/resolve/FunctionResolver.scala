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
  )(using process: CompilationProcess): IO[Unit] = definition match
    case ast.FunctionDefinition(name, args, ast.FunctionBody.Native(nativeKeyword)) =>
      process.registerFact(
        ResolvedFunction(
          ffqn,
          FunctionDefinition(name.map(_.content), args.map(_.map(_.content)), FunctionBody.Native(nativeKeyword.void))
        )
      )
    case ast.FunctionDefinition(name, args, ast.FunctionBody.NonNative(body))       =>
      resolveNonNativeFunction(ffqn, dictionary, name, args, body)

  private def resolveNonNativeFunction(
      ffqn: FunctionFQN,
      dictionary: Map[String, FunctionFQN],
      name: Sourced[Token],
      args: Seq[Sourced[Token]],
      body: Tree[ast.Expression]
  )(using process: CompilationProcess): IO[Unit] = for {
    optionTree <- body.map(expr => resolveExpression(dictionary, expr)).sequence
    _          <- optionTree.sequence match
                    case Some(tree) =>
                      debug(s"resolved ${ffqn.show} to: ${tree.show}") >>
                        process.registerFact(
                          ResolvedFunction(
                            ffqn,
                            FunctionDefinition(name.map(_.content), args.map(_.map(_.content)), FunctionBody.NonNative(tree))
                          )
                        )
                    case None       => IO.unit
  } yield ()

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
