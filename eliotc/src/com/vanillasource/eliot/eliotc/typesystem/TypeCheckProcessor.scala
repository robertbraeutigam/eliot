package com.vanillasource.eliot.eliotc.typesystem

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.{FunctionFQN, ModuleName, TypeFQN}
import com.vanillasource.eliot.eliotc.resolve.Expression.{FunctionApplication, IntegerLiteral, ParameterReference}
import com.vanillasource.eliot.eliotc.resolve.{ArgumentDefinition, Expression, FunctionDefinition, ResolvedFunction}
import com.vanillasource.eliot.eliotc.source.CompilationIO.*
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}

class TypeCheckProcessor extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match
    case ResolvedFunction(
          ffqn,
          functionDefinition @ FunctionDefinition(_, parameters, typeReference, Some(body))
        ) =>
      process(ffqn, parameters, functionDefinition, typeReference, body).runCompilation_()
    case _ => IO.unit

  private def process(
      ffqn: FunctionFQN,
      parameters: Seq[ArgumentDefinition],
      functionDefinition: FunctionDefinition,
      returnType: Sourced[TypeFQN],
      body: Expression
  )(using process: CompilationProcess): CompilationIO[Unit] = for {
    bodyType <- checkRecursiveArgumentTypes(
                  body,
                  parameters.groupBy(_.name.value).map(e => (e._1, e._2.head))
                )
    _        <- checkReturnType(bodyType, returnType)
    _        <- process.registerFact(TypeCheckedFunction(ffqn, functionDefinition)).liftIfNoErrors
  } yield ()

  private def checkReturnType(
      bodyType: Option[Sourced[TypeFQN]],
      returnType: Sourced[TypeFQN]
  )(using process: CompilationProcess): CompilationIO[Unit] =
    bodyType match {
      case Some(bodyType) =>
        compilerError(
          returnType.as(
            s"Function body type is ${bodyType.value.show}, but function declared to return ${returnType.value.show}."
          )
        ).whenA(bodyType.value =!= returnType.value)
      case None           => ().pure[CompilationIO]
    }

  private def checkRecursiveArgumentTypes(
      expression: Expression,
      parameters: Map[String, ArgumentDefinition]
  )(using process: CompilationProcess): CompilationIO[Option[Sourced[TypeFQN]]] =
    expression match {
      case ParameterReference(parameterName)     =>
        IO.pure(parameters.get(parameterName.value).map(_.typeReference)).liftToCompilationIO
      case FunctionApplication(calledFfqn, args) =>
        for {
          calledDefinitionMaybe <-
            process.getFact(ResolvedFunction.Key(calledFfqn.value)).map(_.map(_.definition)).liftToCompilationIO
          _                     <- calledDefinitionMaybe match {
                                     case Some(calledDefinition) =>
                                       for {
                                         argTypes <- args.map(e => checkRecursiveArgumentTypes(e, parameters)).sequence
                                         _        <- checkSingleCallArgumentTypes(calledFfqn, calledDefinition, argTypes)
                                       } yield ()
                                     case _                      => None.pure[CompilationIO]
                                   }
        } yield calledDefinitionMaybe.map(_.returnType)
      case IntegerLiteral(value)                 =>
        IO.pure(Some(value.as(TypeFQN(ModuleName(Seq("eliot"), "Number"), "Byte"))))
          .liftToCompilationIO // TODO: Hardcoded for now
    }

  private def checkSingleCallArgumentTypes(
      sourcedFfqn: Sourced[FunctionFQN],
      functionDefinition: FunctionDefinition,
      calculatedArgumentTypes: Seq[Option[Sourced[TypeFQN]]]
  )(using process: CompilationProcess): CompilationIO[Unit] = {
    if (calculatedArgumentTypes.length =!= functionDefinition.arguments.length) {
      compilerError(
        sourcedFfqn.as(
          s"Function is called with ${calculatedArgumentTypes.length} parameters, but needs ${functionDefinition.arguments.length}."
        )
      )
    } else {
      // Check argument types one by one
      calculatedArgumentTypes
        .zip(functionDefinition.arguments)
        .collect { case (Some(calculatedType), argumentDefinition) =>
          compilerError(
            calculatedType.as(
              s"Expression has type ${calculatedType.value.show}, but needs: ${argumentDefinition.typeReference.value.show}."
            )
          ).whenA(calculatedType.value =!= argumentDefinition.typeReference.value)
        }
        .sequence_
    }
  }
}
