package com.vanillasource.eliot.eliotc.jvm.classgen.asm

import cats.effect.Sync
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName => OldModuleName, TypeFQN}
import com.vanillasource.eliot.eliotc.module.fact.TypeFQN.{systemAnyType, systemFunctionType, systemUnitType}
import com.vanillasource.eliot.eliotc.module2.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.resolve.fact.{ArgumentDefinition, TypeReference}
import com.vanillasource.eliot.eliotc.resolve.fact.TypeReference.{DirectTypeReference, GenericTypeReference}
import com.vanillasource.eliot.eliotc.uncurry2.fact.ParameterDefinition

object CommonPatterns {
  def simpleType(typeReference: TypeReference): TypeFQN =
    typeReference match {
      case DirectTypeReference(dataType, genericParameters) => dataType.value
      case GenericTypeReference(name, genericParameters)    => systemAnyType
    }

  def simpleType(expressionValue: ExpressionValue): TypeFQN =
    expressionValue match {
      case ExpressionValue.FunctionType(_, _) =>
        systemFunctionType
      case ExpressionValue.ConcreteValue(value) =>
        valueToTypeFQN(value)
      case ExpressionValue.ParameterReference(_, paramType) =>
        valueToTypeFQN(paramType)
      case _ =>
        systemAnyType
    }

  def valueToTypeFQN(value: Value): TypeFQN =
    value match {
      case Value.Structure(fields, _) =>
        fields.get("$typeName") match {
          case Some(Value.Direct(vfqn: ValueFQN, _)) =>
            valueFQNToTypeFQN(vfqn)
          case _ =>
            systemAnyType
        }
      case Value.Type =>
        systemAnyType
      case _ =>
        systemAnyType
    }

  def valueFQNToTypeFQN(vfqn: ValueFQN): TypeFQN =
    TypeFQN(
      OldModuleName(vfqn.moduleName.packages, vfqn.moduleName.name),
      vfqn.name
    )

  def moduleNameToOld(moduleName: ModuleName): OldModuleName =
    OldModuleName(moduleName.packages, moduleName.name)

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
            .createCtor[F](fields.map(_.typeReference).map(simpleType))
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

    def addDataFieldsAndCtor2[F[_]: Sync](fields: Seq[ParameterDefinition]): F[Unit] =
      for {
        _ <- fields.traverse_ { paramDefinition =>
               classGenerator.createField[F](paramDefinition.name.value, simpleType(paramDefinition.parameterType))
             }
        // Define constructor
        _ <-
          classGenerator
            .createCtor[F](fields.map(_.parameterType).map(simpleType))
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
                             .addLoadVar[F](simpleType(fieldDefinition.parameterType), index + 1)
                         _ <- methodGenerator.addPutField[F](
                                fieldDefinition.name.value,
                                simpleType(fieldDefinition.parameterType)
                              )
                       } yield ()
                     }
              } yield ()
            }

      } yield ()
  }
}
