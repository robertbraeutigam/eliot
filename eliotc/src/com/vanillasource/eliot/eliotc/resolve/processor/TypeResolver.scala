package com.vanillasource.eliot.eliotc.resolve.processor

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.{CompilationProcess, CompilerFact, CompilerProcessor, ast}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{FunctionFQN, ModuleData, ModuleFunction, TypeFQN}
import com.vanillasource.eliot.eliotc.resolve.fact.GenericParameter.UniversalGenericParameter
import com.vanillasource.eliot.eliotc.resolve.fact.{ArgumentDefinition, DataDefinition, ResolvedData, TypeReference}
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.{DirectTypeReference, GenericTypeReference}
import com.vanillasource.eliot.eliotc.resolve.processor.ResolverScope.*
import com.vanillasource.eliot.eliotc.source.CompilationIO.*
import com.vanillasource.eliot.eliotc.source.Sourced

class TypeResolver extends CompilerProcessor with Logging {
  override def process(fact: CompilerFact)(using CompilationProcess): IO[Unit] = fact match
    case ModuleData(
          tfqn,
          typeDictionary,
          ast.DataDefinition(name, genericParameters, args)
        ) =>
      process(tfqn, typeDictionary, name, genericParameters, args).runCompilation_()
    case _ => IO.unit

  private def process(
      tfqn: TypeFQN,
      typeDictionary: Map[String, TypeFQN],
      name: Sourced[String],
      genericParameters: Seq[ast.GenericParameter],
      args: Seq[ast.ArgumentDefinition]
  )(using process: CompilationProcess): CompilationIO[Unit] = {
    val scope = ResolverScope(
      Map.empty,
      typeDictionary,
      genericParameters.map(gp => gp.name.value -> gp).toMap,
      args.map(arg => arg.name.value -> arg).toMap
    )

    val resolveProgram = for {
      resolvedGenericParameters <-
        genericParameters.traverse(genericParameter =>
          genericParameter.genericParameters
            .traverse(TypeResolver.resolveType)
            .map(resolvedGenericTypes => UniversalGenericParameter(genericParameter.name, resolvedGenericTypes))
        )
      resolvedArguments         <-
        args.traverse { argumentDefinition =>
          TypeResolver
            .resolveType(argumentDefinition.typeReference)
            .map(resolvedTypeReference => ArgumentDefinition(argumentDefinition.name, resolvedTypeReference))
        }
      _                         <-
        process
          .registerFact(
            ResolvedData(
              tfqn,
              DataDefinition(
                name,
                resolvedGenericParameters,
                resolvedArguments
              )
            )
          )
          .liftToCompilationIO
          .liftToScoped

    } yield ()

    resolveProgram.runS(scope).void
  }
}

object TypeResolver {
  def resolveType(reference: ast.TypeReference)(using process: CompilationProcess): ScopedIO[TypeReference] =
    for {
      resolvedGenericParameters <- reference.genericParameters.traverse(resolveType)
      resolvedType              <- getGenericParameter(reference.typeName.value).flatMap {
                                     case Some(genericParameter) =>
                                       if (genericParameter.genericParameters.length =!= resolvedGenericParameters.length) {
                                         compilerAbort(
                                           reference.typeName.as("Incorrect number of generic parameters for type.")
                                         ).liftToScoped
                                       } else {
                                         GenericTypeReference(genericParameter.name, resolvedGenericParameters)
                                           .pure[ScopedIO]
                                       }
                                     case None                   =>
                                       getType(reference.typeName.value).flatMap {
                                         case Some(typeFqn) =>
                                           for {
                                             dataDefinition <-
                                               process.getFact(ModuleData.Key(typeFqn)).liftOptionToCompilationIO.liftToScoped
                                             _              <-
                                               compilerAbort(
                                                 reference.typeName.as("Incorrect number of generic parameters for type.")
                                               ).liftToScoped.whenA(
                                                 dataDefinition.dataDefinition.genericParameters.length =!= resolvedGenericParameters.length
                                               )
                                           } yield DirectTypeReference(reference.typeName.as(typeFqn), resolvedGenericParameters)
                                         case None          => compilerAbort(reference.typeName.as("Type not defined.")).liftToScoped
                                       }
                                   }
    } yield resolvedType
}
