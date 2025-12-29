package com.vanillasource.eliot.eliotc.typesystem.processor

import cats.data.StateT
import cats.effect.IO
import cats.implicits.*
import cats.kernel.Monoid
import com.vanillasource.eliot.eliotc.processor.CompilationProcess.{getFact, registerFact}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, TypeFQN}
import com.vanillasource.eliot.eliotc.processor.CompilationProcess
import com.vanillasource.eliot.eliotc.resolve.fact.*
import com.vanillasource.eliot.eliotc.resolve.fact.Expression.*
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.DirectTypeReference
import com.vanillasource.eliot.eliotc.source.error.CompilationIO.*
import com.vanillasource.eliot.eliotc.source.pos.Sourced
import com.vanillasource.eliot.eliotc.typesystem.fact.{TypeCheckedFunction, TypedExpression, TypedFunctionDefinition}
import com.vanillasource.eliot.eliotc.typesystem.types.TypeUnification.*
import com.vanillasource.eliot.eliotc.typesystem.types.UniqueGenericNames.*
import com.vanillasource.eliot.eliotc.typesystem.types.{TypeUnification, TypeUnificationState, UniqueGenericNames}
import com.vanillasource.eliot.eliotc.processor.impl.OneToOneProcessor

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
          typedDefinition      <- enhanceWithTypes(functionDefinition, fullTypeGraph, solution)
          _                    <- typedDefinition.debugExpressionTypes.liftToCompilationIO
          _                    <-
            registerFact(
              TypeCheckedFunction(resolvedFunction.ffqn, typedDefinition)
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
  )(using CompilationProcess): CompilationIO[TypedFunctionDefinition] =
    enhanceWithTypes(functionDefinition.body.get, fullGraph, solution).map { typedBody =>
      TypedFunctionDefinition(
        functionDefinition.name,
        functionDefinition.genericParameters,
        typedBody
      )
    }

  private def enhanceWithTypes(
      expression: Sourced[Expression],
      fullGraph: TypeUnification,
      solution: TypeUnificationState
  )(using CompilationProcess): CompilationIO[Sourced[TypedExpression]] =
    fullGraph.getSourceType(expression) match {
      case Some(sourceType) =>
        convertExpression(expression.value, fullGraph, solution).map { typedExpr =>
          expression.as(
            TypedExpression(
              // TODO: This is not always the "fully" solved type
              solution.getCurrentType(sourceType),
              typedExpr
            )
          )
        }
      case None             => compilerAbort(expression.as("No type found for expression."))
    }

  private def convertExpression(
      expression: Expression,
      fullGraph: TypeUnification,
      solution: TypeUnificationState
  )(using CompilationProcess): CompilationIO[TypedExpression.Expression] =
    expression match {
      case FunctionApplication(target, argument) =>
        for {
          typedTarget   <- enhanceWithTypes(target, fullGraph, solution)
          typedArgument <- enhanceWithTypes(argument, fullGraph, solution)
        } yield TypedExpression.FunctionApplication(typedTarget, typedArgument)
      case IntegerLiteral(integerLiteral)        =>
        TypedExpression.IntegerLiteral(integerLiteral).pure[CompilationIO]
      case StringLiteral(stringLiteral)          =>
        TypedExpression.StringLiteral(stringLiteral).pure[CompilationIO]
      case ParameterReference(parameterName)     =>
        TypedExpression.ParameterReference(parameterName).pure[CompilationIO]
      case ValueReference(valueName)             =>
        TypedExpression.ValueReference(valueName).pure[CompilationIO]
      case FunctionLiteral(parameter, body)      =>
        enhanceWithTypes(body, fullGraph, solution).map { typedBody =>
          TypedExpression.FunctionLiteral(parameter, typedBody)
        }
    }

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
    case s @ Sourced(_, _, FunctionApplication(target, argument)) =>
      for {
        argumentType        <- generateUniqueGeneric[CompilationIO](argument)
        returnType          <- generateUniqueGeneric[CompilationIO](s)
        targetUnification   <-
          constructTypeGraph(
            DirectTypeReference(target.as(TypeFQN.systemFunctionType), Seq(argumentType, returnType)),
            target,
            "Target of function application is not a Function. Possibly too many arguments."
          )
        argumentUnification <- constructTypeGraph(argumentType, argument)
      } yield targetUnification |+| argumentUnification |+| assignment(
        parentTypeReference,
        s.as(returnType),
        errorMessage
      )
  }

  private def constructTypeGraphForFunctionLiteral(parentTypeReference: TypeReference, errorMessage: String)(using
      CompilationProcess
  ): PartialFunction[Sourced[Expression], TypeGraphIO[TypeUnification]] = {
    case s @ Sourced(_, _, FunctionLiteral(parameter, body)) =>
      for {
        returnType      <- generateUniqueGeneric[CompilationIO](s)
        _               <- boundType(parameter)
        bodyUnification <- constructTypeGraph(returnType, body)
      } yield bodyUnification |+|
        assignment(
          parentTypeReference,
          s.as(
            DirectTypeReference(
              s.as(TypeFQN.systemFunctionType),
              Seq(parameter.typeReference, returnType)
            )
          ),
          errorMessage
        )
  }
}
