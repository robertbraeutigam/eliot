package com.vanillasource.eliot.eliotc.used

import cats.effect.IO
import cats.syntax.all.*
import cats.Monad
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, TypeFQN}
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.*
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, FunctionDefinition, ResolvedFunction, TypeReference}
import com.vanillasource.eliot.eliotc.used.UsedSymbolsState.*
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor

class UsedSymbolsProcessor
    extends TransformationProcessor[ResolvedFunction.Key, UsedSymbols.Key](key => ResolvedFunction.Key(key.ffqn))
    with Logging {

  override protected def generateFromKeyAndFact(key: UsedSymbols.Key, resolvedMainFunction: ResolvedFunction): CompilerIO[UsedSymbols] =
    for {
      usedSymbols <-
        (processDefinition(resolvedMainFunction.definition) >> addFunctionUsed(
          resolvedMainFunction.definition.name.as(resolvedMainFunction.ffqn)
        )).runS(UsedSymbolsState())
    } yield getUsedSymbols(resolvedMainFunction.ffqn, usedSymbols)

  private def processDefinition(definition: FunctionDefinition): UsedSymbolsIO[Unit] =
    for {
      _ <- processTypeReference(definition.valueType)
      _ <- definition.genericParameters
             .flatMap(_.genericParameters)
             .traverse_(processTypeReference)
      _ <- definition.body.traverse_(sourcedBody => processExpression(sourcedBody.value))
    } yield ()

  private def processExpression(expression: Expression): UsedSymbolsIO[Unit] =
    expression match {
      case Expression.FunctionApplication(Sourced(_, _, target), Sourced(_, _, argument)) =>
        processExpression(target) >> processExpression(argument)
      case Expression.IntegerLiteral(integerLiteral)                                      =>
        Monad[CompilerIO].unit.liftToUsedSymbols
      case Expression.StringLiteral(stringLiteral)                                        =>
        Monad[CompilerIO].unit.liftToUsedSymbols
      case Expression.ParameterReference(parameterName)                                   =>
        Monad[CompilerIO].unit.liftToUsedSymbols
      case Expression.ValueReference(sourcedFfqn @ Sourced(_, _, ffqn))                   =>
        isFunctionUsed(ffqn).ifM(
          Monad[CompilerIO].unit.liftToUsedSymbols,
          // Only recurse if not already used
          for {
            loadedFunctionMaybe <- getFactOrAbort(ResolvedFunction.Key(ffqn)).attempt.map(_.toOption).liftToUsedSymbols
            _                   <- addFunctionUsed(sourcedFfqn) >> loadedFunctionMaybe.traverse_(t => processDefinition(t.definition))
          } yield ()
        )
      case Expression.FunctionLiteral(_, Sourced(_, _, body))                             =>
        processExpression(body)
    }

  private def processTypeReference(reference: TypeReference): UsedSymbolsIO[Unit] =
    reference match {
      case DirectTypeReference(dataType, genericParameters) =>
        isTypeUsed(dataType.value).ifM(
          Monad[CompilerIO].unit.liftToUsedSymbols,
          addTypeUsed(dataType) >> genericParameters.traverse_(processTypeReference)
        )
      case GenericTypeReference(name, genericParameters)    =>
        genericParameters.traverse_(processTypeReference)
    }
}
