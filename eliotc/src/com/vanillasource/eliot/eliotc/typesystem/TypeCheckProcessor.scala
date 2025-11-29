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
import com.vanillasource.eliot.eliotc.source.error.CompilationIO.*
import com.vanillasource.eliot.eliotc.source.pos.Sourced
import com.vanillasource.eliot.eliotc.typesystem.TypeUnification.*
import com.vanillasource.eliot.eliotc.typesystem.UniqueGenericNames.*
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerFactKey, CompilerProcessor}

class TypeCheckProcessor extends CompilerProcessor with Logging {
  override def generate(factKey: CompilerFactKey)(using process: CompilationProcess): IO[Unit] = factKey match {
    case TypeCheckedFunction.Key(ffqn) =>
      process.getFact(ResolvedFunction.Key(ffqn)).flatMap(_.traverse_(processFact))
    case _                             => IO.unit
  }

  private def processFact(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match
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
      expression: Expression,
      errorMessage: String = "Type mismatch."
  )(using process: CompilationProcess): TypeGraphIO[TypeUnification] =
    (constructTypeGraphForParameterReference(parentTypeReference, errorMessage) orElse
      constructTypeGraphForIntegerLiteral(parentTypeReference, errorMessage) orElse
      constructTypeGraphForStringLiteral(parentTypeReference, errorMessage) orElse
      constructTypeGraphForValueReference(parentTypeReference, errorMessage) orElse
      constructTypeGraphForFunctionApplication(parentTypeReference, errorMessage) orElse
      constructTypeGraphForFunctionLiteral(parentTypeReference, errorMessage))(expression)

  private def constructTypeGraphForParameterReference(parentTypeReference: TypeReference, errorMessage: String)(using
      process: CompilationProcess
  ): PartialFunction[Expression, TypeGraphIO[TypeUnification]] = { case ParameterReference(parameterName) =>
    for {
      parameterType <- getBoundType(parameterName.value)
    } yield assignment(parentTypeReference, parameterName.as(parameterType.get), errorMessage)
  }

  private def constructTypeGraphForValueReference(parentTypeReference: TypeReference, errorMessage: String)(using
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
                                   } yield assignment(parentTypeReference, functionName.as(uniqueValueType), errorMessage)
                               }
    } yield result
  }

  private def constructTypeGraphForIntegerLiteral(parentTypeReference: TypeReference, errorMessage: String)(using
      process: CompilationProcess
  ): PartialFunction[Expression, TypeGraphIO[TypeUnification]] = { case IntegerLiteral(integerLiteral) =>
    assignment(
      parentTypeReference,
      integerLiteral.as(
        DirectTypeReference(
          integerLiteral.as(TypeFQN(ModuleName(Seq("eliot", "lang"), "Number"), "Byte")),
          Seq.empty
        )
      ),
      errorMessage
    ).pure[TypeGraphIO]
  }

  private def constructTypeGraphForStringLiteral(parentTypeReference: TypeReference, errorMessage: String)(using
      process: CompilationProcess
  ): PartialFunction[Expression, TypeGraphIO[TypeUnification]] = { case StringLiteral(stringLiteral) =>
    assignment(
      parentTypeReference,
      stringLiteral.as(
        DirectTypeReference(
          stringLiteral.as(TypeFQN(ModuleName(Seq("eliot", "lang"), "String"), "String")),
          Seq.empty
        )
      ),
      errorMessage
    ).pure[TypeGraphIO]
  }

  private def constructTypeGraphForFunctionApplication(parentTypeReference: TypeReference, errorMessage: String)(using
      process: CompilationProcess
  ): PartialFunction[Expression, TypeGraphIO[TypeUnification]] = { case FunctionApplication(target, argument) =>
    for {
      argumentType        <- generateUniqueGeneric[CompilationIO](argument)
      returnType          <- generateUniqueGeneric[CompilationIO](target)
      targetUnification   <-
        constructTypeGraph(
          DirectTypeReference(target.as(TypeFQN.systemFunctionType), Seq(argumentType, returnType)),
          target.value,
          "Target of function application is not a Function. Possibly too many arguments."
        )
      argumentUnification <- constructTypeGraph(argumentType, argument.value)
    } yield targetUnification |+| argumentUnification |+| assignment(
      parentTypeReference,
      target.as(returnType),
      errorMessage
    )
  }

  private def constructTypeGraphForFunctionLiteral(parentTypeReference: TypeReference, errorMessage: String)(using
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
        ),
        errorMessage
      )
  }
}
