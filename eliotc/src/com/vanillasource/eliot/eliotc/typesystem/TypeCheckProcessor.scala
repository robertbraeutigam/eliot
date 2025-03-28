package com.vanillasource.eliot.eliotc.typesystem

import cats.data.StateT
import cats.effect.IO
import cats.implicits.*
import cats.kernel.Monoid
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName, TypeFQN}
import com.vanillasource.eliot.eliotc.resolve.fact.Expression.*
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.DirectTypeReference
import com.vanillasource.eliot.eliotc.resolve.fact.*
import com.vanillasource.eliot.eliotc.source.CompilationIO.*
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.typesystem.TypeUnification.*
import com.vanillasource.eliot.eliotc.typesystem.UniqueGenericNames.*
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}

class TypeCheckProcessor extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match
    case ResolvedFunction(
          ffqn,
          functionDefinition @ FunctionDefinition(_, _, _, Some(body))
        ) =>
      process(ffqn, functionDefinition, body).runCompilation_()
    case _ => IO.unit

  private def process(
      ffqn: FunctionFQN,
      functionDefinition: FunctionDefinition,
      body: Sourced[Expression]
  )(using process: CompilationProcess): CompilationIO[Unit] = {
    val typeGraph = genericParameters(functionDefinition.genericParameters)

    for {
      constructedTypeGraph <-
        constructTypeGraph(functionDefinition.valueType, body.value)
          .runA(UniqueGenericNames())
      fullTypeGraph         = typeGraph combine constructedTypeGraph
      _                    <- debug(s"solving ${fullTypeGraph.show}").liftToCompilationIO
      _                    <- fullTypeGraph.solve()
      _                    <- process.registerFact(TypeCheckedFunction(ffqn, functionDefinition)).liftIfNoErrors
    } yield ()
  }

  private type TypeGraphIO[T] = StateT[CompilationIO, UniqueGenericNames, T]

  private def constructTypeGraph(
      parentTypeReference: TypeReference,
      expression: Expression
  )(using process: CompilationProcess): TypeGraphIO[TypeUnification] =
    (constructTypeGraphForParameterReference(parentTypeReference) orElse
      constructTypeGraphForIntegerLiteral(parentTypeReference) orElse
      constructTypeGraphForValueReference(parentTypeReference) orElse
      constructTypeGraphForFunctionApplication(parentTypeReference) orElse
      constructTypeGraphForFunctionLiteral(parentTypeReference))(expression)

  private def constructTypeGraphForParameterReference(parentTypeReference: TypeReference)(using
      process: CompilationProcess
  ): PartialFunction[Expression, TypeGraphIO[TypeUnification]] = { case ParameterReference(parameterName) =>
    for {
      parameterType <- getBoundType(parameterName.value)
    } yield assignment(parentTypeReference, parameterName.as(parameterType.get))
  }

  private def constructTypeGraphForValueReference(parentTypeReference: TypeReference)(using
      process: CompilationProcess
  ): PartialFunction[Expression, TypeGraphIO[TypeUnification]] = { case ValueReference(functionName) =>
    for {
      calledDefinitionMaybe <-
        StateT.liftF(
          process.getFact(ResolvedFunction.Key(functionName.value)).map(_.map(_.definition)).liftToCompilationIO
        )
      result                <- calledDefinitionMaybe match {
                                 case None             => Monoid[TypeUnification].empty.pure[TypeGraphIO]
                                 case Some(definition) =>
                                   for {
                                     uniqueValueType <- makeUnique[CompilationIO](definition.valueType)
                                   } yield assignment(parentTypeReference, functionName.as(uniqueValueType))
                               }
    } yield result
  }

  private def constructTypeGraphForIntegerLiteral(parentTypeReference: TypeReference)(using
      process: CompilationProcess
  ): PartialFunction[Expression, TypeGraphIO[TypeUnification]] = { case IntegerLiteral(integerLiteral) =>
    assignment(
      parentTypeReference,
      integerLiteral.as(
        DirectTypeReference(
          integerLiteral.as(TypeFQN(ModuleName(Seq("eliot", "lang"), "Number"), "Byte")),
          Seq.empty
        )
      )
    ).pure[TypeGraphIO]
  }

  private def constructTypeGraphForFunctionApplication(parentTypeReference: TypeReference)(using
      process: CompilationProcess
  ): PartialFunction[Expression, TypeGraphIO[TypeUnification]] = { case FunctionApplication(target, argument) =>
    for {
      argumentType        <- generateUniqueGeneric[CompilationIO](argument)
      returnType          <- generateUniqueGeneric[CompilationIO](target)
      targetUnification   <-
        constructTypeGraph(
          DirectTypeReference(target.as(TypeFQN.systemFunctionType), Seq(argumentType, returnType)),
          target.value
        )
      argumentUnification <- constructTypeGraph(argumentType, argument.value)
    } yield targetUnification |+| argumentUnification |+| assignment(parentTypeReference, target.as(returnType))
  }

  private def constructTypeGraphForFunctionLiteral(parentTypeReference: TypeReference)(using
      process: CompilationProcess
  ): PartialFunction[Expression, TypeGraphIO[TypeUnification]] = { case FunctionLiteral(parameter, body) =>
    for {
      returnType      <- generateUniqueGeneric[CompilationIO](parameter.name)
      _               <- boundType(parameter)
      bodyUnification <- constructTypeGraph(returnType, body.value)
    } yield bodyUnification |+|
      assignment(
        parentTypeReference,
        body.as(
          DirectTypeReference(
            parameter.name.as(TypeFQN.systemFunctionType),
            Seq(parameter.typeReference, returnType)
          )
        )
      )
  }
}
