package com.vanillasource.eliot.eliotc.typesystem

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.{FunctionFQN, ModuleName, TypeFQN}
import com.vanillasource.eliot.eliotc.resolve.Expression.{FunctionApplication, IntegerLiteral}
import com.vanillasource.eliot.eliotc.resolve.{Expression, FunctionDefinition, ResolvedFunction}
import com.vanillasource.eliot.eliotc.source.CompilationIO.*
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}
import com.vanillasource.util.CatsOps.*

class TypeCheckProcessor extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match
    case ResolvedFunction(
          ffqn,
          functionDefinition @ FunctionDefinition(_, _, typeReference, Some(body))
        ) =>
      process(ffqn, functionDefinition, typeReference, body).runCompilation_()
    case _ => IO.unit

  private def process(
      ffqn: FunctionFQN,
      functionDefinition: FunctionDefinition,
      returnType: Sourced[TypeFQN],
      body: Expression
  )(using process: CompilationProcess): CompilationIO[Unit] = for {
    treeWithTypes <- treeWithExpressionTypes(body).liftToCompilationIO
    _             <- checkReturnType(treeWithTypes, returnType)
    _             <- checkAllArgumentTypes(treeWithTypes)
    _             <- process.registerFact(TypeCheckedFunction(ffqn, functionDefinition)).liftIfNoErrors
  } yield ()

  private def treeWithExpressionTypes(body: Option[Expression])(using
      process: CompilationProcess
  ): IO[Tree[(Expression, Option[Sourced[TypeFQN]])]] =
    body.map(e => typeOf(e).map((e, _))).sequence

  /** Determine the type of the single expression atom.
    */
  private def typeOf(expression: Expression)(using
      process: CompilationProcess
  ): IO[Option[Sourced[TypeFQN]]] = expression match
    case FunctionApplication(functionName) =>
      process.getFact(ResolvedFunction.Key(functionName.value)).map(_.map(_.definition.returnType))
    case IntegerLiteral(value)             =>
      IO.pure(Some(value.as(TypeFQN(ModuleName(Seq("eliot"), "Number"), "Byte")))) // TODO: Hardcoded for now

  private def checkReturnType(
      expression: Tree[(Expression, Option[Sourced[TypeFQN]])],
      returnType: Sourced[TypeFQN]
  )(using process: CompilationProcess): CompilationIO[Unit] =
    expression.head
      .flatMap(_._2)
      .map { topType =>
        compilerError(
          returnType.as(
            s"Function body type is ${topType.value.show}, but function declared to return ${returnType.value.show}."
          )
        ).whenA(topType.value =!= returnType.value)
      }
      .getOrElse(().pure[CompilationIO])

  private def checkAllArgumentTypes(value: Tree[(Expression, Option[Sourced[TypeFQN]])])(using
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
      calculatedArgumentTypes: Seq[Option[Sourced[TypeFQN]]]
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
          if (calculatedType.value === argumentDefinition.typeReference.value) {
            ().pure[CompilationIO]
          } else {
            compilerError(
              calculatedType.as(
                s"Expression has type ${calculatedType.value.show}, but needs: ${argumentDefinition.typeReference.value.show}."
              )
            )
          }
        }
        .sequence_
    }
  }
}
