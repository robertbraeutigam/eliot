package com.vanillasource.eliot.eliotc.resolve.processor

import cats.Monad
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.{TypeFQN, UnifiedModuleData}
import com.vanillasource.eliot.eliotc.processor.OneToOneProcessor
import com.vanillasource.eliot.eliotc.resolve.fact.GenericParameter.UniversalGenericParameter
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.{DirectTypeReference, GenericTypeReference}
import com.vanillasource.eliot.eliotc.resolve.fact.{ArgumentDefinition, DataDefinition, ResolvedData, TypeReference}
import com.vanillasource.eliot.eliotc.resolve.processor.ResolverScope.*
import com.vanillasource.eliot.eliotc.source.error.CompilationIO.*
import com.vanillasource.eliot.eliotc.source.pos.Sourced
import com.vanillasource.eliot.eliotc.{CompilationProcess, ast}

class TypeResolver[F[_]: Monad]
    extends OneToOneProcessor((key: ResolvedData.Key) => UnifiedModuleData.Key(key.tfqn))
    with Logging {
  override def generateFromFact(moduleData: UnifiedModuleData)(using process: CompilationProcess[F]): F[Unit] = {
    val genericParameters = moduleData.dataDefinition.genericParameters
    val fields            = moduleData.dataDefinition.fields
    val name              = moduleData.dataDefinition.name

    val scope = ResolverScope(
      Map.empty,
      moduleData.typeDictionary,
      genericParameters.map(gp => gp.name.value -> gp).toMap,
      fields.getOrElse(Seq.empty).map(arg => arg.name.value -> arg).toMap
    )

    val resolveProgram = for {
      resolvedGenericParameters <-
        genericParameters.traverse(genericParameter =>
          genericParameter.genericParameters
            .traverse(TypeResolver.resolveType)
            .map(resolvedGenericTypes => UniversalGenericParameter(genericParameter.name, resolvedGenericTypes))
        )
      resolvedFields            <- fields match {
                                     case Some(fs) =>
                                       fs.traverse { argumentDefinition =>
                                         TypeResolver
                                           .resolveType(argumentDefinition.typeReference)
                                           .map(resolvedTypeReference =>
                                             ArgumentDefinition(argumentDefinition.name, resolvedTypeReference)
                                           )
                                       }.map(Some(_))
                                     case None     => None.pure[F].liftToCompilationIO.liftToScoped
                                   }
      _                         <-
        process
          .registerFact(
            ResolvedData(
              moduleData.tfqn,
              DataDefinition(
                name,
                resolvedGenericParameters,
                resolvedFields
              )
            )
          )
          .liftToCompilationIO
          .liftToScoped

    } yield ()

    resolveProgram.runS(scope).void.runCompilation_()
  }
}

object TypeResolver {
  def resolveType(reference: ast.TypeReference)(using process: CompilationProcess[F]): ScopedIO[TypeReference] =
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
                                               process.getFact(UnifiedModuleData.Key(typeFqn)).liftOptionToCompilationIO.liftToScoped
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
