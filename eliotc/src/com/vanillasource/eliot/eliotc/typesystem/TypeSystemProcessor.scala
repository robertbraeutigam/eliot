package com.vanillasource.eliot.eliotc.typesystem

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.collections.Tree
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.FunctionFQN
import com.vanillasource.eliot.eliotc.resolve.Expression.{FunctionApplication, IntegerLiteral}
import com.vanillasource.eliot.eliotc.resolve.FunctionBody.NonNative
import com.vanillasource.eliot.eliotc.resolve.{Expression, FunctionDefinition, ResolvedFunction}
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.source.SourcedError.compilerError
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}

class TypeSystemProcessor extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match
    case ResolvedFunction(_, FunctionDefinition(_, _, NonNative(body))) => checkCallArities(body)
    case _                                                              => IO.unit

  private def checkCallArities(body: Tree[Expression])(using CompilationProcess): IO[Unit] = body match
    case Tree.Empty()                                       => IO.unit
    case Tree.Node(FunctionApplication(sourcedFfqn), nodes) =>
      nodes.map(checkCallArities).sequence_ >> checkCallArity(sourcedFfqn, nodes)
    case Tree.Node(IntegerLiteral(_), _)                    => IO.unit

  private def checkCallArity(sourcedFfqn: Sourced[FunctionFQN], nodes: Seq[Tree[Expression]])(using
      process: CompilationProcess
  ): IO[Unit] = for {
    functionDefinitionMaybe <- process.getFact(ResolvedFunction.Key(sourcedFfqn.value))
    _                       <- functionDefinitionMaybe match
                                 case Some(functionDefinition) =>
                                   val calledWithCount  = nodes.length
                                   val definedWithCount = functionDefinition.definition.args.length

                                   compilerError(
                                     sourcedFfqn.as(
                                       s"Function is called with $calledWithCount parameters, but needs $definedWithCount."
                                     )
                                   ).whenA(calledWithCount =!= definedWithCount)
                                 case None                     => IO.unit // Should never happen
  } yield ()
}
