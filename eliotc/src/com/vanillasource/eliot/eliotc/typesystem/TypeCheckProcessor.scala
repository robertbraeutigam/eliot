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
      topType <- checkCallTypes(body).toOptionT
      _       <- if (topType.value === typeDefinition.typeName.value) {
                   process.registerFact(TypeCheckedFunction(ffqn, functionDefinition)).liftOptionT
                 } else {
                   compilerError(
                     topType.as(
                       s"Return type is ${topType.value}, but function declared to return ${typeDefinition.typeName.value}"
                     )
                   ).liftOptionT
                 }
    } yield ()
  }

  /** @return
    *   True, iff check all checks complete and no problems found.
    */
  private def checkCallTypes(body: Tree[Expression])(using CompilationProcess): IO[Option[Sourced[String]]] = body match
    case Tree.Empty()                                       => None.pure
    case Tree.Node(FunctionApplication(sourcedFfqn), nodes) =>
      for {
        recursiveResults <- nodes.map(checkCallTypes).sequence
        result           <- checkCallType(sourcedFfqn, recursiveResults)
      } yield result.map(sourcedFfqn.as(_))
    case Tree.Node(IntegerLiteral(value), _)                => Some(value.as("Byte")).pure // TODO: Hardcoded for now

  /** @return
    *   True, iff check is complete and no problems found.
    */
  private def checkCallType(sourcedFfqn: Sourced[FunctionFQN], calculatedArgumentTypes: Seq[Option[Sourced[String]]])(
      using process: CompilationProcess
  ): IO[Option[String]] = for {
    functionDefinitionMaybe <- process.getFact(ResolvedFunction.Key(sourcedFfqn.value))
    result                  <- functionDefinitionMaybe match
                                 case Some(functionDefinition) =>
                                   for {
                                     _ <- if (calculatedArgumentTypes.length =!= functionDefinition.definition.arguments.length) {
                                            compilerError(
                                              sourcedFfqn.as(
                                                s"Function is called with ${calculatedArgumentTypes.length} parameters, but needs ${functionDefinition.definition.arguments.length}."
                                              )
                                            )
                                          } else {
                                            // Check argument types one by one
                                            calculatedArgumentTypes
                                              .zip(functionDefinition.definition.arguments)
                                              .map { (calculatedTypeMaybe, argumentDefinition) =>
                                                calculatedTypeMaybe match
                                                  case Some(calculatedType) =>
                                                    compilerError(
                                                      calculatedType.as(
                                                        s"Expression had type ${calculatedType.value}, but needed: ${argumentDefinition.typeDefinition.typeName.value}"
                                                      )
                                                    ).whenA(calculatedType.value =!= argumentDefinition.typeDefinition.typeName.value)
                                                  case None                 => IO.unit // No calculated type, so error is there
                                              }
                                              .sequence_
                                          }
                                   } yield Some(functionDefinition.definition.typeDefinition.typeName.value)
                                 case None                     =>
                                   // This should not happen, since we're after the resolve step
                                   compilerError(
                                     sourcedFfqn.as(
                                       s"Could not resolve function."
                                     )
                                   ).as(None)
  } yield result

}
