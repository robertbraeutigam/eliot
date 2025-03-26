package com.vanillasource.eliot.eliotc.typesystem

import cats.data.StateT
import cats.effect.IO
import cats.implicits.*
import cats.kernel.Monoid
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleName, TypeFQN}
import com.vanillasource.eliot.eliotc.resolve.fact.Expression.*
import com.vanillasource.eliot.eliotc.resolve.fact.GenericParameter.ExistentialGenericParameter
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.{DirectTypeReference, GenericTypeReference}
import com.vanillasource.eliot.eliotc.resolve.fact.{
  Expression,
  FunctionDefinition,
  GenericParameter,
  ResolvedFunction,
  TypeReference
}
import com.vanillasource.eliot.eliotc.source.CompilationIO.*
import com.vanillasource.eliot.eliotc.source.Sourced
import com.vanillasource.eliot.eliotc.typesystem.TypeUnification.*
import com.vanillasource.eliot.eliotc.typesystem.UniqueGenericNames.*
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor}

class TypeCheckProcessor extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match
    case ResolvedFunction(
          ffqn,
          functionDefinition @ FunctionDefinition(_, _, _, Some(body))
        ) =>
      process(ffqn, functionDefinition, body).runCompilation_()
    case _ => IO.unit

  private def process(
      ffqn: FunctionFQN,
      functionDefinition: FunctionDefinition,
      body: Expression
  )(using process: CompilationProcess): CompilationIO[Unit] = {
    val typeGraph = genericParameters(functionDefinition.genericParameters)

    for {
      constructedTypeGraph <-
        constructTypeGraphs("", functionDefinition.valueType, body)
          .runA(UniqueGenericNames())
      fullTypeGraph         = typeGraph combine constructedTypeGraph
      _                    <- debug(s"solving ${fullTypeGraph.show}").liftToCompilationIO
      _                    <- fullTypeGraph.solve()
      _                    <- process.registerFact(TypeCheckedFunction(ffqn, functionDefinition)).liftIfNoErrors
    } yield ()
  }

  private type TypeGraphIO[T] = StateT[CompilationIO, UniqueGenericNames, T]

  private def constructTypeGraphs(
      namespace: String,
      parentTypeReference: TypeReference, // Type this expression goes into
      expression: Expression
  )(using process: CompilationProcess): TypeGraphIO[TypeUnification] =
    expression match
      case ParameterReference(parameterName)     =>
        for {
          parameterType <- getBoundType(parameterName.value)
        } yield assignment(parentTypeReference, parameterName.as(parameterType))
      case ValueReference(functionName)          =>
        for {
          calledDefinitionMaybe <-
            StateT.liftF(
              process.getFact(ResolvedFunction.Key(functionName.value)).map(_.map(_.definition)).liftToCompilationIO
            )
          result                <- calledDefinitionMaybe match {
                                     case None             => Monoid[TypeUnification].empty.pure[TypeGraphIO]
                                     case Some(definition) =>
                                       for {
                                         uniqueValueType <- makeUnique[CompilationIO](definition.valueType)
                                       } yield assignment(parentTypeReference, functionName.as(uniqueValueType))
                                   }
        } yield result
      case IntegerLiteral(integerLiteral)        =>
        assignment(
          parentTypeReference,
          integerLiteral.as(
            DirectTypeReference(
              integerLiteral.as(TypeFQN(ModuleName(Seq("eliot", "lang"), "Number"), "Byte")),
              Seq.empty
            )
          )
        ).pure[TypeGraphIO]
      case FunctionApplication(target, argument) =>
        val argumentType = argument.as(namespace + "$AppArg")
        val returnType   = target.as(namespace + "$AppRet")

        for {
          targetUnification   <-
            constructTypeGraphs(
              namespace + "$Target",
              DirectTypeReference(
                target.as(TypeFQN.systemFunctionType),
                Seq(GenericTypeReference(argumentType, Seq.empty), GenericTypeReference(returnType, Seq.empty))
              ),
              target.value
            )
          argumentUnification <-
            constructTypeGraphs(
              namespace + "$Argument",
              GenericTypeReference(argumentType, Seq.empty),
              argument.value
            )
        } yield targetUnification |+| argumentUnification |+| assignment(
          parentTypeReference,
          target.as(GenericTypeReference(returnType, Seq.empty))
        )
      case FunctionLiteral(parameter, body)      =>
        val functionReturnGenericTypeName = parameter.name.as(namespace + "$LitResult")

        for {
          _               <- boundType(parameter)
          bodyUnification <- constructTypeGraphs(
                               namespace + "$LitBody",
                               GenericTypeReference(functionReturnGenericTypeName, Seq.empty),
                               body.value
                             )
        } yield bodyUnification |+|
          assignment(
            parentTypeReference,
            Sourced
              .outline(
                Seq(parameter.name, body)
              ) // TODO: this is a hack for the expression not being Sourced
              .as(
                DirectTypeReference(
                  parameter.name.as(TypeFQN.systemFunctionType),
                  Seq(
                    parameter.typeReference,
                    GenericTypeReference(functionReturnGenericTypeName, Seq.empty)
                  )
                )
              )
          )

}
