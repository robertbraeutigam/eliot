package com.vanillasource.eliot.eliotc.used

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName, ModuleNames}
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, FunctionDefinition}
import com.vanillasource.eliot.eliotc.source.CompilationIO.*
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.typesystem.TypeCheckedFunction

class UsedSymbolsProcessor(mainFunction: FunctionFQN) extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] =
    fact match
      case TypeCheckedFunction(ffqn, definition) if ffqn === mainFunction =>
        processMain(ffqn, definition).runCompilation_()
      case _                                                              => IO.unit

  private def processMain(ffqn: FunctionFQN, definition: FunctionDefinition)(using
      process: CompilationProcess
  ): CompilationIO[Unit] =
    for {
      usedFunctions <- processDefinition(ffqn, definition)
      _             <- debug(s"Used functions: ${usedFunctions.map(_.show).mkString(", ")}").liftIfNoErrors
      _             <- process.registerFact(UsedSymbols(usedFunctions)).liftIfNoErrors
    } yield ()

  private def processDefinition(ffqn: FunctionFQN, definition: FunctionDefinition)(using
      CompilationProcess
  ): CompilationIO[Set[FunctionFQN]] =
    definition.body match {
      case Some(Sourced(_, _, expression)) => processExpression(expression).map(_ + ffqn)
      case None                            =>
        IO.raiseError(new IllegalStateException("Should not happen, body of type-checked function is empty."))
          .liftToCompilationIO
    }

  // FIXME: not safe when recursive!
  private def processExpression(
      expression: Expression
  )(using process: CompilationProcess): CompilationIO[Set[FunctionFQN]] =
    expression match {
      case Expression.FunctionApplication(Sourced(_, _, target), Sourced(_, _, argument)) =>
        for {
          targetResult   <- processExpression(target)
          argumentResult <- processExpression(argument)
        } yield targetResult ++ argumentResult
      case Expression.IntegerLiteral(integerLiteral)                                      =>
        IO.pure(Set.empty).liftToCompilationIO
      case Expression.StringLiteral(stringLiteral)                                        =>
        IO.pure(Set.empty).liftToCompilationIO
      case Expression.ParameterReference(parameterName)                                   =>
        IO.pure(Set.empty).liftToCompilationIO
      case Expression.ValueReference(s @ Sourced(_, _, ffqn))                             =>
        for {
          loadedFunctionMaybe <- process.getFact(TypeCheckedFunction.Key(ffqn)).liftToCompilationIO
          usedFunctions       <- loadedFunctionMaybe match {
                                   case Some(loadedFunction) =>
                                     processDefinition(loadedFunction.ffqn, loadedFunction.definition)
                                   case None                 =>
                                     // Function not type checked. We assume that the platform has it,
                                     // or the platform will issue "linking" error
                                     IO.pure(Set(ffqn)).liftToCompilationIO
                                 }
        } yield usedFunctions
      case Expression.FunctionLiteral(_, Sourced(_, _, body))                             =>
        processExpression(body)
    }
}
