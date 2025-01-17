package com.vanillasource.eliot.eliotc.resolve

import com.vanillasource.eliot.eliotc.ast
import cats.effect.IO
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.source.Sourced
import cats.syntax.all.*
import com.vanillasource.collections.Tree
import com.vanillasource.eliot.eliotc.module.{FunctionFQN, ModuleFunction}
import com.vanillasource.eliot.eliotc.source.SourcedError.registerCompilerError
import com.vanillasource.eliot.eliotc.token.Token
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}

class FunctionResolver extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match
    case ModuleFunction(ffqn, dictionary, ast.FunctionDefinition(name, args, typeDefinition, body)) =>
      process(ffqn, dictionary, name, args, typeDefinition, body)
    case _                                                                                          => IO.unit

  private def process(
      ffqn: FunctionFQN,
      dictionary: Map[String, FunctionFQN],
      name: Sourced[Token],
      args: Seq[ast.ArgumentDefinition],
      typeDefinition: ast.TypeReference,
      body: Tree[ast.Expression]
  )(using process: CompilationProcess): IO[Unit] = for {
    optionTree <- body.map(expr => resolveExpression(dictionary, expr)).sequence
    _          <- optionTree.sequence match
                    case Some(tree) =>
                      debug(s"resolved ${ffqn.show} (${typeDefinition.show}) to: ${tree.show}") >>
                        process.registerFact(
                          ResolvedFunction(
                            ffqn,
                            FunctionDefinition(
                              name.map(_.content),
                              args.map(argDef =>
                                ArgumentDefinition(
                                  argDef.name.map(_.content),
                                  TypeReference(argDef.typeDefinition.typeName.map(_.content))
                                )
                              ),
                              TypeReference(typeDefinition.typeName.map(_.content)),
                              tree
                            )
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
          case None       => registerCompilerError(s.as(s"Function not defined.")) >> None.pure
      case ast.Expression.IntegerLiteral(s @ Sourced(_, _, Token.IntegerLiteral(value))) =>
        Some(Expression.IntegerLiteral(s.as(value))).pure
      case ast.Expression.IntegerLiteral(s)                                              =>
        registerCompilerError(s.as(s"Internal compiler error, not parsed as an integer literal.")) >> None.pure
}
