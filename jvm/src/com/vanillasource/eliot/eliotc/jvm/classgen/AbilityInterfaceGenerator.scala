package com.vanillasource.eliot.eliotc.jvm.classgen

import cats.effect.Sync
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.ClassGenerator
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.CommonPatterns.simpleType
import com.vanillasource.eliot.eliotc.jvm.classgen.fact.ClassFile
import com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedValue

object AbilityInterfaceGenerator {

  def createAbilityInterface[F[_]: Sync](
      outerClassGenerator: ClassGenerator,
      abilityName: String,
      methods: Seq[UncurriedValue]
  ): F[ClassFile] =
    for {
      interfaceGenerator <- outerClassGenerator.createInnerInterfaceGenerator[F](abilityName + "$vtable")
      _                  <- methods.traverse_ { method =>
                              val (paramTypes, returnType) = extractSignatureTypes(method.signature)
                              interfaceGenerator.createAbstractMethod[F](
                                method.vfqn.name.name,
                                paramTypes.map(simpleType),
                                simpleType(returnType)
                              )
                            }
      classFile          <- interfaceGenerator.generate[F]()
    } yield classFile

  private def extractSignatureTypes(signature: ExpressionValue): (Seq[ExpressionValue], ExpressionValue) = {
    def loop(expr: ExpressionValue, acc: Seq[ExpressionValue]): (Seq[ExpressionValue], ExpressionValue) =
      expr match {
        case ExpressionValue.FunctionType(paramType, returnType) => loop(returnType, acc :+ paramType)
        case _                                                   => (acc, expr)
      }
    loop(ExpressionValue.stripUniversalTypeIntros(signature), Seq.empty)
  }
}
