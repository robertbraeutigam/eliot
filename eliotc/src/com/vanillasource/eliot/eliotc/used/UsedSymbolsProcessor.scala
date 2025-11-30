package com.vanillasource.eliot.eliotc.used

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, TypeFQN}
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.*
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, FunctionDefinition, ResolvedFunction, TypeReference}
import com.vanillasource.eliot.eliotc.source.pos.Sourced
import com.vanillasource.eliot.eliotc.used.UsedSymbolsState.*
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerFactKey, CompilerProcessor}

class UsedSymbolsProcessor(mainFunction: FunctionFQN) extends CompilerProcessor with Logging {
  override def generate(factKey: CompilerFactKey[_])(using process: CompilationProcess): IO[Unit] = factKey match {
    case UsedSymbols.Key() =>
      process.getFact(ResolvedFunction.Key(mainFunction)).flatMap(_.traverse_(processFact))
    case _                 => IO.unit
  }

  private def processFact(fact: CompilerFact)(using CompilationProcess): IO[Unit] =
    fact match
      case ResolvedFunction(ffqn, definition) if ffqn === mainFunction =>
        processMain(ffqn, definition)
      case _                                                           => IO.unit

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
    for {
      _ <- processTypeReference(definition.valueType)
      _ <- definition.genericParameters
             .flatMap(_.genericParameters)
             .traverse_(processTypeReference)
      _ <- definition.body.traverse_(sourcedBody => processExpression(sourcedBody.value))
    } yield ()

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
        isFunctionUsed(ffqn).ifM(
          IO.unit.liftToUsedSymbols,
          // Only recurse if not already used
          for {
            loadedFunctionMaybe <- process.getFact(ResolvedFunction.Key(ffqn)).liftToUsedSymbols
            _                   <- addFunctionUsed(sourcedFfqn) >> loadedFunctionMaybe.traverse_(t => processDefinition(t.definition))
          } yield ()
        )
      case Expression.FunctionLiteral(_, Sourced(_, _, body))                             =>
        processExpression(body)
    }

  private def processTypeReference(reference: TypeReference)(using process: CompilationProcess): UsedSymbolsIO[Unit] = {
    reference match {
      case DirectTypeReference(dataType, genericParameters) =>
        isTypeUsed(dataType.value).ifM(
          IO.unit.liftToUsedSymbols,
          addTypeUsed(dataType) >> genericParameters.traverse_(processTypeReference)
        )
      case GenericTypeReference(name, genericParameters)    =>
        genericParameters.traverse_(processTypeReference)
    }
  }
}
