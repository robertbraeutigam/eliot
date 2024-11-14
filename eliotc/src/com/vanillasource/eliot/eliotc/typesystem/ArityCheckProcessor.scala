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

class ArityCheckProcessor extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using processor: CompilationProcess): IO[Unit] = fact match
    case ResolvedFunction(ffqn, functionDefinition @ FunctionDefinition(_, _, _, NonNative(body))) =>
      checkCallArities(body).ifM(
        processor.registerFact(ArityCheckedFunction(ffqn, functionDefinition)),
        IO.unit
      )
    case _                                                                                         => IO.unit

  /** @return
    *   True, iff check all checks complete and no problems found.
    */
  private def checkCallArities(body: Tree[Expression])(using CompilationProcess): IO[Boolean] = body match
    case Tree.Empty()                                       => true.pure
    case Tree.Node(FunctionApplication(sourcedFfqn), nodes) =>
      for {
        recursiveResults <- nodes.map(checkCallArities).sequence
        result           <- checkCallArity(sourcedFfqn, nodes)
      } yield recursiveResults.fold(true)(_ & _) & result
    case Tree.Node(IntegerLiteral(_), _)                    => true.pure

  /** @return
    *   True, iff check is complete and no problems found.
    */
  private def checkCallArity(sourcedFfqn: Sourced[FunctionFQN], nodes: Seq[Tree[Expression]])(using
      process: CompilationProcess
  ): IO[Boolean] = for {
    functionDefinitionMaybe <- process.getFact(ResolvedFunction.Key(sourcedFfqn.value))
    result                  <- functionDefinitionMaybe match
                                 case Some(functionDefinition) =>
                                   val calledWithCount  = nodes.length
                                   val definedWithCount = functionDefinition.definition.arguments.length

                                   compilerError(
                                     sourcedFfqn.as(
                                       s"Function is called with $calledWithCount parameters, but needs $definedWithCount."
                                     )
                                   )
                                     .whenA(calledWithCount =!= definedWithCount)
                                     .as(calledWithCount === definedWithCount)
                                 case None                     =>
                                   // This should not happen, since we're after the resolve step
                                   compilerError(
                                     sourcedFfqn.as(
                                       s"Could not resolve function."
                                     )
                                   ).as(false)

  } yield result
}
