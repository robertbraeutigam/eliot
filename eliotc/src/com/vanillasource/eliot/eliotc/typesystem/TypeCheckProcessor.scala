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
import com.vanillasource.eliot.eliotc.source.CompilationIO.*
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}
import com.vanillasource.util.CatsOps.*

class TypeCheckProcessor extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match
    case ResolvedFunction(
          ffqn,
          functionDefinition @ FunctionDefinition(_, _, typeDefinition, NonNative(body))
        ) =>
      process(ffqn, functionDefinition, typeDefinition, body)
    case _ => IO.unit

  private def process(
      ffqn: FunctionFQN,
      functionDefinition: FunctionDefinition,
      typeDefinition: TypeDefinition,
      body: Tree[Expression]
  )(using process: CompilationProcess): IO[Unit] =
    checkCallTypes(body)
      .foreachF { topType =>
        if (topType.value === typeDefinition.typeName.value) {
          process.registerFact(TypeCheckedFunction(ffqn, functionDefinition)).liftToCompilationIO.ifNoErrors
        } else {
          compilerError(
            topType.as(
              s"Expression type is ${topType.value}, but function declared to return ${typeDefinition.typeName.value}"
            )
          )
        }
      }
      .runCompilation_()

  private def checkCallTypes(expression: Tree[Expression])(using
      CompilationProcess
  ): OptionT[CompilationIO, Sourced[String]] = expression match
    case Tree.Empty()                                       => OptionT.none
    case Tree.Node(FunctionApplication(sourcedFfqn), nodes) =>
      for {
        argumentTypes <- nodes.map(checkCallTypes).sequence
        returnType    <- checkCallType(sourcedFfqn, argumentTypes)
      } yield returnType
    case Tree.Node(IntegerLiteral(value), _)                =>
      OptionT.pure(value.as("Byte")) // TODO: Hardcoded for now

  private def checkCallType(sourcedFfqn: Sourced[FunctionFQN], calculatedArgumentTypes: Seq[Sourced[String]])(using
      process: CompilationProcess
  ): OptionT[CompilationIO, Sourced[String]] = for {
    functionDefinition <- process.getFact(ResolvedFunction.Key(sourcedFfqn.value)).liftToCompilationIO.toOptionT
    _                  <- checkArgumentTypes(sourcedFfqn, functionDefinition, calculatedArgumentTypes)
  } yield sourcedFfqn.as(functionDefinition.definition.typeDefinition.typeName.value)

  private def checkArgumentTypes(
      sourcedFfqn: Sourced[FunctionFQN],
      functionDefinition: ResolvedFunction,
      calculatedArgumentTypes: Seq[Sourced[String]]
  )(using process: CompilationProcess): OptionT[CompilationIO, Unit] = {
    if (calculatedArgumentTypes.length =!= functionDefinition.definition.arguments.length) {
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
          if (calculatedType.value === argumentDefinition.typeDefinition.typeName.value) {
            ().pure[CompilationIO]
          } else {
            compilerError(
              calculatedType.as(
                s"Expression had type ${calculatedType.value}, but needed: ${argumentDefinition.typeDefinition.typeName.value}"
              )
            )
          }
        }
        .sequence_
        .liftOptionT
    }
  }
}
