package com.vanillasource.eliot.eliotc.jvm.classgen.asm

import cats.effect.Sync
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.{Types, Value}
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.uncurry.fact.MonomorphicParameterDefinition
import NativeType.{systemAnyValue, systemFunctionValue}
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}

object CommonPatterns {

  def valueType(v: Value): ValueFQN =
    v match {
      case _ if v.asFunctionType.isDefined => systemFunctionValue
      case _                               =>
        v.typeFQN match {
          case Some(vfqn) if vfqn =!= Types.typeFQN => stripDataTypeSuffix(vfqn)
          case _                                    => systemAnyValue
        }
    }

  def extractValueSignatureTypes(signature: Value): (Seq[Value], Value) =
    signature.extractParamAndReturnTypes

  def constructorDataTypeValue(returnType: Value): Value =
    returnType.deepReturnType

  def constructorArityValue(returnType: Value): Int =
    returnType.functionArity

  def mangleSuffix(typeArgs: Seq[Value]): String =
    if (typeArgs.isEmpty) ""
    else "$" + typeArgs.map(v => valueType(v).name.name).mkString("$")

  def stripDataTypeSuffix(valueFQN: ValueFQN): ValueFQN =
    ValueFQN(valueFQN.moduleName, QualifiedName(valueFQN.name.name, Qualifier.Default))

  extension (classGenerator: ClassGenerator) {
    def addMonomorphicDataFieldsAndCtor[F[_]: Sync](fields: Seq[MonomorphicParameterDefinition]): F[Unit] =
      for {
        _ <- fields.traverse_ { paramDefinition =>
               classGenerator.createField[F](
                 JvmIdentifier.encode(paramDefinition.name.value),
                 valueType(paramDefinition.parameterType)
               )
             }
        _ <-
          classGenerator
            .createCtor[F](fields.map(_.parameterType).map(valueType))
            .use { methodGenerator =>
              for {
                _ <- methodGenerator.addLoadThis[F]()
                _ <- methodGenerator.addCallToObjectCtor[F]()
                _ <- fields.zipWithIndex.traverse_ { (fieldDefinition, index) =>
                       for {
                         _ <- methodGenerator.addLoadThis[F]()
                         _ <-
                           methodGenerator
                             .addLoadVar[F](valueType(fieldDefinition.parameterType), index + 1)
                         _ <- methodGenerator.addPutField[F](
                                JvmIdentifier.encode(fieldDefinition.name.value),
                                valueType(fieldDefinition.parameterType)
                              )
                       } yield ()
                     }
              } yield ()
            }
      } yield ()

  }
}
