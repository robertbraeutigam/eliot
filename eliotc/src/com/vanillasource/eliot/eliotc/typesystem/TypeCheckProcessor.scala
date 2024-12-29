package com.vanillasource.eliot.eliotc.typesystem

import cats.data.OptionT
import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.collections.Tree
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.FunctionFQN
import com.vanillasource.eliot.eliotc.resolve.Expression.{FunctionApplication, IntegerLiteral}
import com.vanillasource.eliot.eliotc.resolve.FunctionBody.NonNative
import com.vanillasource.eliot.eliotc.resolve.{Expression, FunctionDefinition, ResolvedFunction, TypeDefinition}
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.source.SourcedError.compilerError
import com.vanillasource.eliot.eliotc.typesystem.TypeCheckProcessor.TypeCheck
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}
import com.vanillasource.util.CatsOps.*

class TypeCheckProcessor extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match
    case ResolvedFunction(
          ffqn,
          functionDefinition @ FunctionDefinition(_, _, typeDefinition, NonNative(body))
        ) =>
      process(ffqn, functionDefinition, typeDefinition, body).getOrUnit
    case _ => IO.unit

  private def process(
      ffqn: FunctionFQN,
      functionDefinition: FunctionDefinition,
      typeDefinition: TypeDefinition,
      body: Tree[Expression]
  )(using process: CompilationProcess): OptionT[IO, Unit] = {
    for {
      topType <- checkCallTypes(body)
      _       <- if (topType.typeName.value === typeDefinition.typeName.value) {
                   if (topType.success) {
                     process.registerFact(TypeCheckedFunction(ffqn, functionDefinition)).liftOptionT
                   } else {
                     OptionT.none[IO, Unit]
                   }
                 } else {
                   compilerError(
                     topType.typeName.as(
                       s"Expression type is ${topType.typeName.value}, but function declared to return ${typeDefinition.typeName.value}"
                     )
                   ).liftOptionT
                 }
    } yield ()
  }

  private def checkCallTypes(expression: Tree[Expression])(using CompilationProcess): OptionT[IO, TypeCheck] =
    expression match
      case Tree.Empty()                                       => OptionT.none
      case Tree.Node(FunctionApplication(sourcedFfqn), nodes) =>
        for {
          argumentTypes <- nodes.map(checkCallTypes).sequence
          typeCheck     <- checkCallType(sourcedFfqn, argumentTypes.map(_.typeName))
        } yield TypeCheck(argumentTypes.map(_.success).fold(true)(_ & _) & typeCheck.success, typeCheck.typeName)
      case Tree.Node(IntegerLiteral(value), _)                =>
        OptionT.pure(TypeCheck(true, value.as("Byte"))) // TODO: Hardcoded for now

  // TODO: issue error if call was not resolved! could be a random function that is not defined, TEST!
  private def checkCallType(sourcedFfqn: Sourced[FunctionFQN], calculatedArgumentTypes: Seq[Sourced[String]])(using
      process: CompilationProcess
  ): OptionT[IO, TypeCheck] = for {
    functionDefinition <- process.getFact(ResolvedFunction.Key(sourcedFfqn.value)).toOptionT
    success            <- if (calculatedArgumentTypes.length =!= functionDefinition.definition.arguments.length) {
                            compilerError(
                              sourcedFfqn.as(
                                s"Function is called with ${calculatedArgumentTypes.length} parameters, but needs ${functionDefinition.definition.arguments.length}."
                              )
                            ).as(false).liftOptionT
                          } else {
                            // Check argument types one by one
                            calculatedArgumentTypes
                              .zip(functionDefinition.definition.arguments)
                              .map { (calculatedType, argumentDefinition) =>
                                if (calculatedType.value === argumentDefinition.typeDefinition.typeName.value) {
                                  true.pure[IO]
                                } else {
                                  compilerError(
                                    calculatedType.as(
                                      s"Expression had type ${calculatedType.value}, but needed: ${argumentDefinition.typeDefinition.typeName.value}"
                                    )
                                  ).as(false)
                                }
                              }
                              .sequence
                              .map(_.fold(true)(_ & _)) // True, iff all checks are success
                              .liftOptionT
                          }
  } yield TypeCheck(success, sourcedFfqn.as(functionDefinition.definition.typeDefinition.typeName.value))
}

object TypeCheckProcessor {
  private case class TypeCheck(success: Boolean, typeName: Sourced[String])
}
