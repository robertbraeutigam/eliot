package com.vanillasource.eliot.eliotc.typesystem

import cats.effect.IO
import cats.implicits.*
import cats.kernel.Monoid
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.{FunctionFQN, ModuleName, TypeFQN}
import com.vanillasource.eliot.eliotc.resolve.fact.Expression.{FunctionApplication, IntegerLiteral, ParameterReference}
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.DirectTypeReference
import com.vanillasource.eliot.eliotc.resolve.fact.{Expression, FunctionDefinition, ResolvedFunction, TypeReference}
import com.vanillasource.eliot.eliotc.source.CompilationIO.*
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.typesystem.TypeUnification.{assignment, genericParameters}
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
  )(using process: CompilationProcess): CompilationIO[Unit] = {
    val typeGraph      = genericParameters(functionDefinition.genericParameters)
    val parameterTypes = functionDefinition.arguments.groupMapReduce(_.name.value)(_.typeReference)((left, _) => left)

    for {
      constructedTypeGraph <- constructTypeGraphs("", functionDefinition.returnType, parameterTypes, body)
      fullTypeGraph         = typeGraph combine constructedTypeGraph
      _                    <- debug(s"solving ${fullTypeGraph.show}").liftToCompilationIO
      _                    <- fullTypeGraph.solve()
      _                    <- process.registerFact(TypeCheckedFunction(ffqn, functionDefinition)).liftIfNoErrors
    } yield ()
  }

  private def constructTypeGraphs(
      namespace: String,
      parentTypeReference: TypeReference, // Type this expression goes into
      parameterTypes: Map[String, TypeReference],
      expression: Expression
  )(using process: CompilationProcess): CompilationIO[TypeUnification] =
    expression match
      case ParameterReference(parameterName)            =>
        assignment(parentTypeReference, parameterTypes(parameterName.value)).pure[CompilationIO]
      case IntegerLiteral(integerLiteral)               =>
        // TODO: we hardcode the integer literal type here, fix this later
        assignment(
          parentTypeReference,
          DirectTypeReference(integerLiteral.as(TypeFQN(ModuleName(Seq("eliot"), "Number"), "Byte")), Seq.empty)
        ).pure[CompilationIO]
      case FunctionApplication(functionName, arguments) =>
        for {
          calledDefinitionMaybe <-
            process.getFact(ResolvedFunction.Key(functionName.value)).map(_.map(_.definition)).liftToCompilationIO
          result                <- calledDefinitionMaybe match {
                                     case None             => Monoid[TypeUnification].empty.pure[CompilationIO]
                                     case Some(definition) =>
                                       checkFunctionApplicationTypes(
                                         namespace + s"#${functionName.value.show}#",
                                         parentTypeReference,
                                         parameterTypes,
                                         functionName,
                                         definition,
                                         arguments
                                       )
                                   }
        } yield result

  private def checkFunctionApplicationTypes(
      namespace: String,
      parentTypeReference: TypeReference, // Type this expression goes into
      parameterTypes: Map[String, TypeReference],
      functionName: Sourced[FunctionFQN],
      functionDefinition: FunctionDefinition,
      arguments: Seq[Expression]
  )(using process: CompilationProcess): CompilationIO[TypeUnification] = {
    val baseGraph =
      genericParameters(functionDefinition.genericParameters.map(_.shiftToNamespace(namespace).instantiate())) |+|
        assignment(
          parentTypeReference,
          functionDefinition.returnType.sourcedAt(functionName).shiftGenericToNamespace(namespace)
        )

    if (arguments.length =!= functionDefinition.arguments.length) {
      compilerError(
        functionName.as(
          s"Function is called with ${arguments.length} parameters, but needs ${functionDefinition.arguments.length}."
        )
      ).as(Monoid[TypeUnification].empty)
    } else {
      functionDefinition.arguments
        .zip(arguments)
        .zipWithIndex
        .map { case ((argumentDefinition, expression), index) =>
          constructTypeGraphs(
            s"$namespace$index",
            argumentDefinition.typeReference.shiftGenericToNamespace(namespace),
            parameterTypes,
            expression
          )
        }
        .sequence
        .map(_.fold(baseGraph)(_ combine _))
    }
  }
}
