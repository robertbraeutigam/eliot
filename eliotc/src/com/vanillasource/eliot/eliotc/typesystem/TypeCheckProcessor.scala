package com.vanillasource.eliot.eliotc.typesystem

import cats.data.{IndexedStateT, OptionT}
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
      process(ffqn, functionDefinition, typeDefinition, body).runCompilation_()
    case _ => IO.unit

  private def process(
      ffqn: FunctionFQN,
      functionDefinition: FunctionDefinition,
      typeDefinition: TypeDefinition,
      body: Tree[Expression]
  )(using process: CompilationProcess): CompilationIO[Unit] = for {
    treeWithTypes <- treeWithExpressionTypes(body).liftToCompilationIO
    _             <- checkReturnType(treeWithTypes, typeDefinition)
    _             <- checkAllArgumentTypes(treeWithTypes)
    _             <- process.registerFact(TypeCheckedFunction(ffqn, functionDefinition)).liftIfNoErrors
  } yield ()

  private def treeWithExpressionTypes(body: Tree[Expression])(using
      process: CompilationProcess
  ): IO[Tree[(Expression, Option[Sourced[String]])]] =
    body.map(e => typeOf(e).map((e, _))).sequence

  /** Determine the type of the single expression atom.
    */
  private def typeOf(expression: Expression)(using
      process: CompilationProcess
  ): IO[Option[Sourced[String]]] = expression match
    case FunctionApplication(functionName) =>
      process.getFact(ResolvedFunction.Key(functionName.value)).map(_.map(_.definition.typeDefinition.typeName))
    case IntegerLiteral(value)             => IO.pure(Some(value.as("Byte"))) // TODO: Hardcoded for now

  private def checkReturnType(
      expression: Tree[(Expression, Option[Sourced[String]])],
      definition: TypeDefinition
  )(using process: CompilationProcess): CompilationIO[Unit] =
    expression.head
      .flatMap(_._2)
      .map { topType =>
        compilerError(
          topType.as(
            s"Expression type is ${topType.value}, but function declared to return ${definition.typeName.value}."
          )
        ).whenA(topType.value =!= definition.typeName.value)
      }
      .getOrElse(().pure[CompilationIO])

  private def checkAllArgumentTypes(value: Tree[(Expression, Option[Sourced[String]])])(using
      process: CompilationProcess
  ): CompilationIO[Unit] = value.foreachWithChildrenF {
    case ((FunctionApplication(calledFfqn), _), arguments) =>
      (for {
        functionDefinition <- process.getFact(ResolvedFunction.Key(calledFfqn.value)).liftToCompilationIO.toOptionT
        _                  <- checkSingleCallArgumentTypes(calledFfqn, functionDefinition, arguments.map(_.flatMap(_._2))).liftOptionT
      } yield ()).value.void
    case _                                                 => ().pure[CompilationIO]
  }

  private def checkSingleCallArgumentTypes(
      sourcedFfqn: Sourced[FunctionFQN],
      functionDefinition: ResolvedFunction,
      calculatedArgumentTypes: Seq[Option[Sourced[String]]]
  )(using process: CompilationProcess): CompilationIO[Unit] = {
    if (calculatedArgumentTypes.length =!= functionDefinition.definition.arguments.length) {
      compilerError(
        sourcedFfqn.as(
          s"Function is called with ${calculatedArgumentTypes.length} parameters, but needs ${functionDefinition.definition.arguments.length}."
        )
      )
    } else {
      // Check argument types one by one
      calculatedArgumentTypes
        .zip(functionDefinition.definition.arguments)
        .collect { case (Some(calculatedType), argumentDefinition) =>
          if (calculatedType.value === argumentDefinition.typeDefinition.typeName.value) {
            ().pure[CompilationIO]
          } else {
            compilerError(
              calculatedType.as(
                s"Expression had type ${calculatedType.value}, but needed: ${argumentDefinition.typeDefinition.typeName.value}."
              )
            )
          }
        }
        .sequence_
    }
  }
}
