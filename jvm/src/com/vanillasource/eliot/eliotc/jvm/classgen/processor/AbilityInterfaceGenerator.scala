package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import cats.effect.Sync
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.{ClassGenerator, JvmIdentifier}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.CommonPatterns.{extractSignatureTypes, simpleType}
import com.vanillasource.eliot.eliotc.jvm.classgen.fact.ClassFile
import com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedValue

object AbilityInterfaceGenerator {

  def createAbilityInterface[F[_]: Sync](
      outerClassGenerator: ClassGenerator,
      abilityName: String,
      methods: Seq[UncurriedValue]
  ): F[ClassFile] =
    for {
      interfaceGenerator <-
        outerClassGenerator.createInnerInterfaceGenerator[F](JvmIdentifier.encode(abilityName + "$vtable"))
      _                  <- methods.traverse_ { method =>
                              val (paramTypes, returnType) = extractSignatureTypes(method.signature)
                              interfaceGenerator.createAbstractMethod[F](
                                JvmIdentifier.encode(method.vfqn.name.name),
                                paramTypes.map(simpleType),
                                simpleType(returnType)
                              )
                            }
      classFile          <- interfaceGenerator.generate[F]()
    } yield classFile

}
