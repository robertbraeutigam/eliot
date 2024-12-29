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
      _       <- if (topType.value === typeDefinition.typeName.value) {
                   process.registerFact(TypeCheckedFunction(ffqn, functionDefinition)).liftOptionT
                 } else {
                   compilerError(
                     topType.as(
                       s"Expression type is ${topType.value}, but function declared to return ${typeDefinition.typeName.value}"
                     )
                   ).liftOptionT
                 }
    } yield ()
  }

  // TODO: just return case class with type name and whether there was an error!
  private def checkCallTypes(expression: Tree[Expression])(using CompilationProcess): OptionT[IO, Sourced[String]] =
    expression match
      case Tree.Empty()                                       => OptionT.none
      case Tree.Node(FunctionApplication(sourcedFfqn), nodes) =>
        for {
          argumentTypes <- nodes.map(checkCallTypes).sequence
          returnType    <- checkCallType(sourcedFfqn, argumentTypes)
        } yield sourcedFfqn.as(returnType)
      case Tree.Node(IntegerLiteral(value), _)                => OptionT.pure(value.as("Byte")) // TODO: Hardcoded for now

  // TODO: issue error if call was not resolved! could be a random function that is not defined
  private def checkCallType(sourcedFfqn: Sourced[FunctionFQN], calculatedArgumentTypes: Seq[Sourced[String]])(using
      process: CompilationProcess
  ): OptionT[IO, String] = for {
    functionDefinition <- process.getFact(ResolvedFunction.Key(sourcedFfqn.value)).toOptionT
    _                  <- if (calculatedArgumentTypes.length =!= functionDefinition.definition.arguments.length) {
                            compilerError(
                              sourcedFfqn.as(
                                s"Function is called with ${calculatedArgumentTypes.length} parameters, but needs ${functionDefinition.definition.arguments.length}."
                              )
                            ).liftOptionT
                          } else {
                            // Check argument types one by one
                            calculatedArgumentTypes
                              .zip(functionDefinition.definition.arguments)
                              .map { (calculatedType, argumentDefinition) =>
                                compilerError(
                                  calculatedType.as(
                                    s"Expression had type ${calculatedType.value}, but needed: ${argumentDefinition.typeDefinition.typeName.value}"
                                  )
                                ).whenA(calculatedType.value =!= argumentDefinition.typeDefinition.typeName.value)
                              }
                              .sequence_
                              .liftOptionT
                          }
  } yield functionDefinition.definition.typeDefinition.typeName.value
}
