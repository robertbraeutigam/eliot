package com.vanillasource.eliot.eliotc.jvm.classgen.asm

import cats.effect.Sync
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.Value
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.uncurry.fact.ParameterDefinition
import NativeType.{systemAnyValue, systemFunctionValue}
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.symbolic.fact.{QuantifiedType, SymbolicType}

object CommonPatterns {
  def simpleType(st: SymbolicType): ValueFQN =
    SymbolicType.stripLeadingApplications(st) match {
      case SymbolicType.FunctionType(_, _)  => systemFunctionValue
      case SymbolicType.TypeVariable(_)     => systemAnyValue
      case SymbolicType.TypeReference(vfqn) => stripDataTypeSuffix(vfqn)
      case _                                => systemAnyValue
    }

  def valueToValueFQN(value: Value): ValueFQN =
    value match {
      case Value.Structure(fields, _) =>
        fields.get("$typeName") match {
          case Some(Value.Direct(vfqn: ValueFQN, _)) =>
            vfqn
          case _                                     =>
            systemAnyValue
        }
      case _                          =>
        systemAnyValue
    }

  private def stripDataTypeSuffix(valueFQN: ValueFQN): ValueFQN =
    ValueFQN(valueFQN.moduleName, QualifiedName(valueFQN.name.name, Qualifier.Default))

  def extractSignatureTypes(signature: QuantifiedType): (Seq[SymbolicType], SymbolicType) = {
    def loop(expr: SymbolicType, acc: Seq[SymbolicType]): (Seq[SymbolicType], SymbolicType) =
      expr match {
        case SymbolicType.FunctionType(paramType, returnType) => loop(returnType, acc :+ paramType)
        case _                                                => (acc, expr)
      }
    loop(signature.body, Seq.empty)
  }

  extension (classGenerator: ClassGenerator) {
    def addDataFieldsAndCtor[F[_]: Sync](fields: Seq[ParameterDefinition]): F[Unit] =
      for {
        _ <- fields.traverse_ { paramDefinition =>
               classGenerator.createField[F](
                 JvmIdentifier.encode(paramDefinition.name.value),
                 simpleType(paramDefinition.parameterType)
               )
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
                                JvmIdentifier.encode(fieldDefinition.name.value),
                                simpleType(fieldDefinition.parameterType)
                              )
                       } yield ()
                     }
              } yield ()
            }

      } yield ()
  }
}
