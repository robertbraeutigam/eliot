package com.vanillasource.eliot.eliotc.used

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.FunctionFQN
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, FunctionDefinition}
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.typesystem.TypeCheckedFunction
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
      usedFunctions <- processDefinition(definition).map(_ + ((ffqn, definition.name)))
      _             <- debug(s"Used functions: ${usedFunctions.keys.map(_.show).mkString(", ")}")
      _             <- process.registerFact(UsedSymbols(usedFunctions))
    } yield ()

  private def processDefinition(definition: FunctionDefinition)(using
      CompilationProcess
  ): IO[Map[FunctionFQN, Sourced[_]]] =
    definition.body match {
      case Some(Sourced(_, _, expression)) => processExpression(expression)
      case None                            =>
        IO.raiseError(new IllegalStateException("Should not happen, body of type-checked function is empty."))
    }

  // FIXME: not safe when recursive!
  private def processExpression(
      expression: Expression
  )(using process: CompilationProcess): IO[Map[FunctionFQN, Sourced[_]]] =
    expression match {
      case Expression.FunctionApplication(Sourced(_, _, target), Sourced(_, _, argument)) =>
        for {
          targetResult   <- processExpression(target)
          argumentResult <- processExpression(argument)
        } yield targetResult ++ argumentResult
      case Expression.IntegerLiteral(integerLiteral)                                      =>
        IO.pure(Map.empty)
      case Expression.StringLiteral(stringLiteral)                                        =>
        IO.pure(Map.empty)
      case Expression.ParameterReference(parameterName)                                   =>
        IO.pure(Map.empty)
      case Expression.ValueReference(sourcedFfqn @ Sourced(_, _, ffqn))                   =>
        for {
          loadedFunctionMaybe <- process.getFact(TypeCheckedFunction.Key(ffqn))
          usedFunctions       <- loadedFunctionMaybe match {
                                   case Some(loadedFunction) =>
                                     processDefinition(loadedFunction.definition).map(_ + ((ffqn, sourcedFfqn)))
                                   case None                 =>
                                     // Function not type checked. We assume that the platform has it,
                                     // or the platform will issue "linking" error
                                     IO.pure(Map((ffqn, sourcedFfqn)))
                                 }
        } yield usedFunctions
      case Expression.FunctionLiteral(_, Sourced(_, _, body))                             =>
        processExpression(body)
    }
}
