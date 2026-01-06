package com.vanillasource.eliot.eliotc.jvm.classgen.asm

import cats.effect.Sync
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.TypeFQN
import com.vanillasource.eliot.eliotc.module.fact.TypeFQN.{systemAnyType, systemUnitType}
import com.vanillasource.eliot.eliotc.resolve.fact.{ArgumentDefinition, TypeReference}
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.{DirectTypeReference, GenericTypeReference}

object CommonPatterns {
  def simpleType(typeReference: TypeReference): TypeFQN =
    typeReference match {
      case DirectTypeReference(dataType, genericParameters) => dataType.value
      case GenericTypeReference(name, genericParameters)    => systemAnyType
    }

  extension (classGenerator: ClassGenerator) {
    def addDataFieldsAndCtor[F[_]: Sync](fields: Seq[ArgumentDefinition]): F[Unit] =
      for {
        _ <- fields.traverse_ { argumentDefinition =>
               argumentDefinition.typeReference match {
                 case DirectTypeReference(dataType, genericParameters) =>
                   classGenerator.createField[F](argumentDefinition.name.value, dataType.value)
                 case GenericTypeReference(name, genericParameters)    =>
                   classGenerator.createField[F](argumentDefinition.name.value, systemAnyType)
               }
             }
        // Define constructor
        _ <-
          classGenerator
            .createMethod[F](
              "<init>",
              fields.map(_.typeReference).map(simpleType),
              systemUnitType
            )
            .use { methodGenerator =>
              for {
                // Call super.<init>
                _ <- methodGenerator.addLoadThis[F]()
                _ <- methodGenerator.addCallToObjectCtor[F]()
                // Set all this.field = field
                _ <- fields.zipWithIndex.traverse_ { (fieldDefinition, index) =>
                       for {
                         _ <- methodGenerator.addLoadThis[F]()
                         _ <-
                           methodGenerator
                             .addLoadVar[F](simpleType(fieldDefinition.typeReference), index + 1)
                         _ <- methodGenerator.addPutField[F](
                                fieldDefinition.name.value,
                                simpleType(fieldDefinition.typeReference)
                              )
                       } yield ()
                     }
              } yield ()
            }

      } yield ()
  }
}
