package com.vanillasource.eliot.eliotc.typesystem.processor

import cats.data.StateT
import cats.effect.IO
import cats.implicits.*
import cats.kernel.Monoid
import com.vanillasource.eliot.eliotc.CompilationProcess
import com.vanillasource.eliot.eliotc.CompilationProcess.{getFact, registerFact}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, TypeFQN}
import com.vanillasource.eliot.eliotc.processor.OneToOneProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.*
import com.vanillasource.eliot.eliotc.resolve.fact.Expression.*
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.DirectTypeReference
import com.vanillasource.eliot.eliotc.source.error.CompilationIO.*
import com.vanillasource.eliot.eliotc.source.pos.Sourced
import com.vanillasource.eliot.eliotc.typesystem.fact.{TypeCheckedFunction, TypedExpression, TypedFunctionDefinition}
import com.vanillasource.eliot.eliotc.typesystem.types.TypeUnification.*
import com.vanillasource.eliot.eliotc.typesystem.types.UniqueGenericNames.*
import com.vanillasource.eliot.eliotc.typesystem.types.{TypeUnification, TypeUnificationState, UniqueGenericNames}

class TypeCheckProcessor
    extends OneToOneProcessor((key: TypeCheckedFunction.Key) => ResolvedFunction.Key(key.ffqn))
    with Logging {

  override def generateFromFact(
      resolvedFunction: ResolvedFunction
  )(using CompilationProcess): IO[Unit] = {
    val functionDefinition = resolvedFunction.definition
    val typeGraph          = genericParameters(functionDefinition.genericParameters)

    functionDefinition.body match {
      case Some(body) =>
        val program = for {
          constructedTypeGraph <-
            constructTypeGraph(functionDefinition.valueType, body)
              .runA(UniqueGenericNames())
          fullTypeGraph         = typeGraph `combine` constructedTypeGraph
          _                    <- debug[CompilationIO](s"Solving ${fullTypeGraph.show}")
          solution             <- fullTypeGraph.solve()
          _                    <- fullTypeGraph.printTypes();
          _                    <-
            registerFact(
              TypeCheckedFunction(resolvedFunction.ffqn, enhanceWithTypes(functionDefinition, fullTypeGraph, solution))
            ).liftIfNoErrors
        } yield ()

        program.runCompilation_()
      case None       => IO.unit
    }
  }

  private def enhanceWithTypes(
      functionDefinition: FunctionDefinition,
      fullGraph: TypeUnification,
      solution: TypeUnificationState
  ): TypedFunctionDefinition =
    TypedFunctionDefinition(
      functionDefinition.name,
      functionDefinition.genericParameters,
      enhanceWithTypes(functionDefinition.body.get, fullGraph, solution)
    )

  private def enhanceWithTypes(
      expression: Sourced[Expression],
      fullGraph: TypeUnification,
      solution: TypeUnificationState
  ): Sourced[TypedExpression] = ???

  private type TypeGraphIO[T] = StateT[CompilationIO, UniqueGenericNames, T]

  private def constructTypeGraph(
      parentTypeReference: TypeReference,
      sourcedExpression: Sourced[Expression],
      errorMessage: String = "Type mismatch."
  )(using CompilationProcess): TypeGraphIO[TypeUnification] =
    (constructTypeGraphForParameterReference(parentTypeReference, errorMessage) orElse
      constructTypeGraphForIntegerLiteral(parentTypeReference, errorMessage) orElse
      constructTypeGraphForStringLiteral(parentTypeReference, errorMessage) orElse
      constructTypeGraphForValueReference(parentTypeReference, errorMessage) orElse
      constructTypeGraphForFunctionApplication(parentTypeReference, errorMessage) orElse
      constructTypeGraphForFunctionLiteral(parentTypeReference, errorMessage))(sourcedExpression)

  private def constructTypeGraphForParameterReference(
      parentTypeReference: TypeReference,
      errorMessage: String
  ): PartialFunction[Sourced[Expression], TypeGraphIO[TypeUnification]] = {
    case Sourced(_, _, ParameterReference(parameterName)) =>
      for {
        parameterType <- getBoundType(parameterName.value)
      } yield assignment(parentTypeReference, parameterName.as(parameterType.get), errorMessage)
  }

  private def constructTypeGraphForValueReference(parentTypeReference: TypeReference, errorMessage: String)(using
      CompilationProcess
  ): PartialFunction[Sourced[Expression], TypeGraphIO[TypeUnification]] = {
    case Sourced(_, _, ValueReference(functionName)) =>
      for {
        calledDefinitionMaybe <-
          StateT.liftF(
            getFact(ResolvedFunction.Key(functionName.value)).map(_.map(_.definition)).liftToCompilationIO
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

  private def constructTypeGraphForIntegerLiteral(
      parentTypeReference: TypeReference,
      errorMessage: String
  ): PartialFunction[Sourced[Expression], TypeGraphIO[TypeUnification]] = {
    case Sourced(_, _, IntegerLiteral(integerLiteral)) =>
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

  private def constructTypeGraphForStringLiteral(
      parentTypeReference: TypeReference,
      errorMessage: String
  ): PartialFunction[Sourced[Expression], TypeGraphIO[TypeUnification]] = {
    case Sourced(_, _, StringLiteral(stringLiteral)) =>
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
      CompilationProcess
  ): PartialFunction[Sourced[Expression], TypeGraphIO[TypeUnification]] = {
    case Sourced(_, _, FunctionApplication(target, argument)) =>
      for {
        argumentType        <- generateUniqueGeneric[CompilationIO](argument)
        returnType          <- generateUniqueGeneric[CompilationIO](target)
        targetUnification   <-
          constructTypeGraph(
            DirectTypeReference(target.as(TypeFQN.systemFunctionType), Seq(argumentType, returnType)),
            target,
            "Target of function application is not a Function. Possibly too many arguments."
          )
        argumentUnification <- constructTypeGraph(argumentType, argument)
      } yield targetUnification |+| argumentUnification |+| assignment(
        parentTypeReference,
        target.as(returnType),
        errorMessage
      )
  }

  private def constructTypeGraphForFunctionLiteral(parentTypeReference: TypeReference, errorMessage: String)(using
      CompilationProcess
  ): PartialFunction[Sourced[Expression], TypeGraphIO[TypeUnification]] = {
    case Sourced(_, _, FunctionLiteral(parameter, body)) =>
      for {
        returnType      <- generateUniqueGeneric[CompilationIO](parameter.name)
        _               <- boundType(parameter)
        bodyUnification <- constructTypeGraph(returnType, body)
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
