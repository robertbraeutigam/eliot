package com.vanillasource.eliot.eliotc.jvm.classgen.asm

import cats.effect.Sync
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.uncurry2.fact.ParameterDefinition
import NativeType.{systemFunctionValue, systemAnyValue}

object CommonPatterns {
  def simpleType(expressionValue: ExpressionValue): ValueFQN =
    expressionValue match {
      case ExpressionValue.FunctionType(_, _) =>
        systemFunctionValue
      case ExpressionValue.ConcreteValue(value) =>
        valueToValueFQN(value)
      case ExpressionValue.ParameterReference(_, paramType) =>
        valueToValueFQN(paramType)
      case _ =>
        systemAnyValue
    }

  def valueToValueFQN(value: Value): ValueFQN =
    value match {
      case Value.Structure(fields, _) =>
        fields.get("$typeName") match {
          case Some(Value.Direct(vfqn: ValueFQN, _)) =>
            vfqn
          case _ =>
            systemAnyValue
        }
      case Value.Type =>
        systemAnyValue
      case _ =>
        systemAnyValue
    }

  extension (classGenerator: ClassGenerator) {
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
