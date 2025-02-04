package com.vanillasource.eliot.eliotc.typesystem

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.{FunctionFQN, ModuleName, TypeFQN}
import com.vanillasource.eliot.eliotc.resolve.Expression.{FunctionApplication, IntegerLiteral, ParameterReference}
import com.vanillasource.eliot.eliotc.resolve.{
  ArgumentDefinition,
  Expression,
  FunctionDefinition,
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
          functionDefinition @ FunctionDefinition(_, genericParameters, parameters, typeReference, Some(body))
        ) =>
      process(ffqn, genericParameters, parameters, functionDefinition, typeReference, body).runCompilation_()
    case _ => IO.unit

  private def process(
      ffqn: FunctionFQN,
      genericParameters: Seq[Sourced[String]],
      parameters: Seq[ArgumentDefinition],
      functionDefinition: FunctionDefinition,
      returnType: TypeReference,
      body: Expression
  )(using process: CompilationProcess): CompilationIO[Unit] = ???
}
