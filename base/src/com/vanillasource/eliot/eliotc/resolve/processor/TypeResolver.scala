package com.vanillasource.eliot.eliotc.resolve.processor

import cats.effect.IO
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.module.fact.UnifiedModuleData
import com.vanillasource.eliot.eliotc.resolve.fact.GenericParameter.UniversalGenericParameter
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.{DirectTypeReference, GenericTypeReference}
import com.vanillasource.eliot.eliotc.resolve.fact.{ArgumentDefinition, DataDefinition, ResolvedData, TypeReference}
import com.vanillasource.eliot.eliotc.resolve.processor.ResolverScope.*
import com.vanillasource.eliot.eliotc.ast
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.processor.common.TransformationProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerError

class TypeResolver extends TransformationProcessor((key: ResolvedData.Key) => UnifiedModuleData.Key(key.tfqn)) with Logging {
  override def generateFromFact(moduleData: UnifiedModuleData): CompilerIO[Unit] = {
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
                                     case None     => None.pure[ScopedIO]
                                   }
      _                         <-
        registerFactIfClear(
          ResolvedData(
            moduleData.tfqn,
            DataDefinition(
              name,
              resolvedGenericParameters,
              resolvedFields
            )
          )
        ).liftToScoped

    } yield ()

    resolveProgram.runS(scope).void
  }
}

object TypeResolver {
  def resolveType(reference: ast.TypeReference): ScopedIO[TypeReference] =
    for {
      resolvedGenericParameters <- reference.genericParameters.traverse(resolveType)
      resolvedType              <- getGenericParameter(reference.typeName.value).flatMap {
                                     case Some(genericParameter) =>
                                       if (genericParameter.genericParameters.length =!= resolvedGenericParameters.length) {
                                         (compilerError(
                                           reference.typeName.as("Incorrect number of generic parameters for type.")
                                         ) *> abort[TypeReference]).liftToScoped
                                       } else {
                                         GenericTypeReference(genericParameter.name, resolvedGenericParameters)
                                           .pure[ScopedIO]
                                       }
                                     case None                   =>
                                       getType(reference.typeName.value).flatMap {
                                         case Some(typeFqn) =>
                                           for {
                                             dataDefinition <-
                                               getFactOrAbort(UnifiedModuleData.Key(typeFqn)).liftToScoped
                                             _              <-
                                               (compilerError(
                                                 reference.typeName.as("Incorrect number of generic parameters for type.")
                                               ) *> abort[Unit]).liftToScoped.whenA(
                                                 dataDefinition.dataDefinition.genericParameters.length =!= resolvedGenericParameters.length
                                               )
                                           } yield DirectTypeReference(reference.typeName.as(typeFqn), resolvedGenericParameters)
                                         case None          =>
                                           (compilerError(reference.typeName.as("Type not defined.")) *> abort[
                                             TypeReference
                                           ]).liftToScoped
                                       }
                                   }
    } yield resolvedType
}
