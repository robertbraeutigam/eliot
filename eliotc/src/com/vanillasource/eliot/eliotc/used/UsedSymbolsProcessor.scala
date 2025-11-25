package com.vanillasource.eliot.eliotc.used

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, TypeFQN}
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, FunctionDefinition}
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.typesystem.TypeCheckedFunction
import com.vanillasource.eliot.eliotc.used.UsedSymbolsState.*
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}

class UsedSymbolsProcessor(mainFunction: FunctionFQN) extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] =
    fact match
      case TypeCheckedFunction(ffqn, definition) if ffqn === mainFunction =>
        processMain(ffqn, definition)
      case _                                                              => IO.unit

  private def processMain(ffqn: FunctionFQN, definition: FunctionDefinition)(using
      process: CompilationProcess
  ): IO[Unit] =
    for {
      usedSymbols <-
        (processDefinition(definition) >> addFunctionUsed(definition.name.as(ffqn))).runS(UsedSymbolsState())
      _           <- debug(s"Used functions: ${usedSymbols.usedFunctions.keys.map(_.show).mkString(", ")}")
      _           <- debug(s"Used types: ${usedSymbols.usedTypes.keys.map(TypeFQN.fullyQualified.show(_)).mkString(", ")}")
      _           <- process.registerFact(getUsedSymbols(usedSymbols))
    } yield ()

  private def processDefinition(definition: FunctionDefinition)(using
      CompilationProcess
  ): UsedSymbolsIO[Unit] =
    definition.body match {
      case Some(Sourced(_, _, expression)) => processExpression(expression)
      case None                            =>
        IO.raiseError(new IllegalStateException("Should not happen, body of type-checked function is empty."))
          .liftToUsedSymbols
    }

  // FIXME: does not work when processing recursive functions, becomes endless loop
  private def processExpression(expression: Expression)(using process: CompilationProcess): UsedSymbolsIO[Unit] =
    expression match {
      case Expression.FunctionApplication(Sourced(_, _, target), Sourced(_, _, argument)) =>
        processExpression(target) >> processExpression(argument)
      case Expression.IntegerLiteral(integerLiteral)                                      =>
        IO.unit.liftToUsedSymbols
      case Expression.StringLiteral(stringLiteral)                                        =>
        IO.unit.liftToUsedSymbols
      case Expression.ParameterReference(parameterName)                                   =>
        IO.unit.liftToUsedSymbols
      case Expression.ValueReference(sourcedFfqn @ Sourced(_, _, ffqn))                   =>
        for {
          loadedFunctionMaybe <- process.getFact(TypeCheckedFunction.Key(ffqn)).liftToUsedSymbols
          _                   <- addFunctionUsed(sourcedFfqn) >> loadedFunctionMaybe.traverse_(t => processDefinition(t.definition))
        } yield ()
      case Expression.FunctionLiteral(_, Sourced(_, _, body))                             =>
        processExpression(body)
    }
}
