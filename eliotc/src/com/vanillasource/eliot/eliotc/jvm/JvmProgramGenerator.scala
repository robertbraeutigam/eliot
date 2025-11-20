package com.vanillasource.eliot.eliotc.jvm

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.FunctionFQN
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.used.UsedSymbols

class JvmProgramGenerator extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] =
    fact match
      case UsedSymbols(usedFunctions) => generateAllClasses(usedFunctions)
      case _                          => IO.unit

  private def generateAllClasses(
      usedFunctions: Map[FunctionFQN, Sourced[_]]
  )(using process: CompilationProcess): IO[Unit] = {
    val groupedFunctions = usedFunctions.toSeq.groupBy(_._1.moduleName)

    for {
      _            <- groupedFunctions.toSeq
                        .traverse_((moduleName, usedFunctions) => process.registerFact(GenerateClass(moduleName, usedFunctions)))
      classesMaybe <-
        groupedFunctions.keys.toSeq.traverse(moduleName => process.getFact(GeneratedClass.Key(moduleName)))
      _            <- classesMaybe.sequence.traverse_(generateJarFile) // Skips jar if not all modules got bytecode
    } yield ()
  }

  private def generateJarFile(allClasses: Seq[GeneratedClass]): IO[Unit] = ???
}
