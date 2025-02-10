package com.vanillasource.eliot.eliotc.typesystem

import cats.effect.IO
import cats.syntax.all.*
import cats.implicits.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.{FunctionFQN, ModuleName, TypeFQN}
import com.vanillasource.eliot.eliotc.resolve.Expression.{FunctionApplication, IntegerLiteral, ParameterReference}
import com.vanillasource.eliot.eliotc.resolve.TypeReference.DirectTypeReference
import com.vanillasource.eliot.eliotc.resolve.{
  ArgumentDefinition,
  Expression,
  FunctionDefinition,
  GenericParameter,
  ResolvedFunction,
  TypeReference
}
import com.vanillasource.eliot.eliotc.source.CompilationIO.*
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}

class TypeCheckProcessor extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match
    case ResolvedFunction(
          ffqn,
          functionDefinition @ FunctionDefinition(_, _, _, _, Some(body))
        ) =>
      process(ffqn, functionDefinition, body).runCompilation_()
    case _ => IO.unit

  private def process(
      ffqn: FunctionFQN,
      functionDefinition: FunctionDefinition,
      body: Expression
  )(using process: CompilationProcess): CompilationIO[Unit] = for {
    inference          <- TypeInference.forType(
                            functionDefinition.genericParameters.groupMapReduce(_.name.value)(identity)((left, _) => left),
                            functionDefinition.returnType
                          )
    parameterInference <-
      functionDefinition.arguments.traverse(argumentDefinition =>
        inference.inferTypeFor(argumentDefinition.typeReference).map(i => (argumentDefinition.name.value, i))
      )
    _                  <- checkTypes(
                            inference,
                            body,
                            parameterInference.groupMapReduce(_._1)(_._2)((left, _) => left)
                          )
    _                  <- process.registerFact(TypeCheckedFunction(ffqn, functionDefinition)).liftIfNoErrors
  } yield ()

  private def checkTypes(
      currentTypeInference: TypeInference,
      expression: Expression,
      parameterInferences: Map[String, TypeInference]
  )(using process: CompilationProcess): CompilationIO[Unit] =
    expression match
      case ParameterReference(parameterName)            =>
        currentTypeInference.receivesFrom(parameterName, parameterInferences(parameterName.value)).void
      case IntegerLiteral(integerLiteral)               =>
        currentTypeInference
          .receivesFrom(DirectTypeReference(integerLiteral.as(TypeFQN(ModuleName(Seq("eliot"), "Number"), "Byte"))))
          .void
      case FunctionApplication(functionName, arguments) =>
        for {
          calledDefinitionMaybe <-
            process.getFact(ResolvedFunction.Key(functionName.value)).map(_.map(_.definition)).liftToCompilationIO
          _                     <- calledDefinitionMaybe match {
                                     case None             => ().pure[CompilationIO]
                                     case Some(definition) =>
                                       checkFunctionApplicationTypes(
                                         functionName,
                                         currentTypeInference,
                                         definition,
                                         arguments,
                                         parameterInferences
                                       )
                                   }
        } yield ()

  private def checkFunctionApplicationTypes(
      functionName: Sourced[FunctionFQN],
      previousTypeInference: TypeInference,
      functionDefinition: FunctionDefinition,
      arguments: Seq[Expression],
      parameterTypes: Map[String, TypeInference]
  )(using process: CompilationProcess): CompilationIO[Unit] = for {
    currentTypeInference <-
      previousTypeInference.receivesFrom(functionDefinition.returnType.sourcedAt(functionName))
    _                    <- if (arguments.length =!= functionDefinition.arguments.length) {
                              compilerError(
                                functionName.as(
                                  s"Function is called with ${arguments.length} parameters, but needs ${functionDefinition.arguments.length}."
                                )
                              )
                            } else {
                              functionDefinition.arguments
                                .zip(arguments)
                                .map { (argumentDefinition, expression) =>
                                  for {
                                    currentInference <-
                                      currentTypeInference.inferTypeFor(argumentDefinition.typeReference)
                                    _                <- checkTypes(currentInference, expression, parameterTypes)
                                  } yield ()
                                }
                                .sequence_
                            }
  } yield ()
}
