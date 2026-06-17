package com.vanillasource.eliot.eliotc.jvm.classgen.asm

import cats.effect.Sync
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.uncurry.fact.MonomorphicParameterDefinition
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}

object CommonPatterns {

  /** The JVM type FQN a value erases to. The erasure logic itself is the platform-independent
    * [[GroundValue.carrierFQN]] in `lang`; this is only the local convenience name, holding no backend-specific logic.
    * The backend's sole erasure knowledge is the FQN → `java.lang.*` map in [[NativeType.types]].
    */
  def valueType(v: GroundValue): ValueFQN = v.carrierFQN

  def extractValueSignatureTypes(signature: GroundValue): (Seq[GroundValue], GroundValue) =
    signature.extractParamAndReturnTypes

  def constructorDataTypeValue(returnType: GroundValue): GroundValue =
    returnType.deepReturnType

  def constructorArityValue(returnType: GroundValue): Int =
    returnType.functionArity

  def mangleSuffix(typeArgs: Seq[GroundValue]): String =
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
