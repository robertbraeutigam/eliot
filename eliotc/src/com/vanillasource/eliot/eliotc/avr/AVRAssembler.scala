package com.vanillasource.eliot.eliotc.avr

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.collections.Tree
import com.vanillasource.collections.Tree.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.FunctionFQN
import com.vanillasource.eliot.eliotc.output.Output
import com.vanillasource.eliot.eliotc.resolve.ResolvedError.compilerError
import com.vanillasource.eliot.eliotc.resolve.{Expression, FunctionBody, ResolvedFunction}
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor, Init, avr}

class AVRAssembler(mainFQN: FunctionFQN) extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match
    case Init => writeAssembledFunction(mainFQN)
    case _    => IO.unit

  private def writeAssembledFunction(mainFFQN: FunctionFQN)(using process: CompilationProcess): IO[Unit] = for {
    functions <- assembleFunction(mainFFQN)
    _         <- functions match
                   case Some(value) =>
                     debug(s"assembled avr functions for ${mainFFQN.show}: ${value.map(_.ffqn.show).mkString(", ")}") >>
                       process.registerFact(new Output(mainFFQN.moduleName, value.map(_.instr).combineAll.generateBytes()))
                   case None        => IO.unit
  } yield ()

  private def assembleFunction(
      ffqn: FunctionFQN
  )(using process: CompilationProcess): IO[Option[Seq[CompiledFunction]]] =
    for {
      callTree  <- distinctCallTree(ffqn, Set.empty)
      functions <- callTree match
                     case Some(tree) =>
                       tree
                         .map { calledFfqn =>
                           process.getFact(CompiledFunction.Key(calledFfqn)).flatMap {
                             case Some(compiledFact) => Some(compiledFact).pure[IO]
                             case None               =>
                               compilerError(
                                 calledFfqn,
                                 "Could not find implementation for function."
                               ) >> Option.empty[avr.CompiledFunction].pure[IO]
                           }
                         }
                         .toSeqBreadthFirst
                         .sequence
                         .map(_.sequence)
                     case None       => None.pure[IO]
    } yield functions

  private def distinctCallTree(ffqn: FunctionFQN, alreadyCollected: Set[FunctionFQN])(using
      process: CompilationProcess
  ): IO[Option[Tree[FunctionFQN]]] = if (alreadyCollected.contains(ffqn)) {
    Some(Tree.empty()).pure[IO]
  } else {
    for {
      resolvedFunction    <- process.getFact(ResolvedFunction.Key(ffqn))
      resolvedCalledTrees <- resolvedFunction match
                               case Some(value) =>
                                 allCalledFunctionsOfBody(value.definition.body)
                                   .foldM(
                                     (alreadyCollected + ffqn, Option(Seq.empty[Tree[FunctionFQN]]))
                                   ) { case ((coll, trees), calledFfqn) =>
                                     distinctCallTree(calledFfqn, coll).map(tree =>
                                       (coll + calledFfqn, trees.map2(tree)(_ :+ _))
                                     )
                                   }
                                   .map(_._2)
                               case None        =>
                                 error(
                                   s"could not find resolved function ${ffqn.show}, this should not happen, since we already have a resolved function"
                                 ) >> None.pure[IO]
    } yield resolvedCalledTrees.map(calledTrees => Tree(ffqn, calledTrees))
  }

  private def allCalledFunctionsOfBody(body: FunctionBody): Seq[FunctionFQN] = body match
    case FunctionBody.Native(keyword) => Seq.empty
    case FunctionBody.NonNative(tree) =>
      tree.toList.collect { case Expression.FunctionApplication(ffqn) => ffqn.value }
}
