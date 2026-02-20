package com.vanillasource.eliot.eliotc.jvm.classgen

import cats.effect.Sync
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.ClassGenerator
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.CommonPatterns.simpleType
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType
import com.vanillasource.eliot.eliotc.jvm.classgen.fact.ClassFile
import com.vanillasource.eliot.eliotc.module.fact.ValueFQN
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
import com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedValue

object AbilityImplGenerator {

  def createAbilityImpl[F[_]: Sync](
      outerClassGenerator: ClassGenerator,
      abilityFQN: AbilityFQN,
      typeArgs: Seq[ExpressionValue],
      methodPairs: Seq[(UncurriedValue, UncurriedValue)]
  ): F[ClassFile] = {
    val innerClassName = singletonInnerName(abilityFQN, typeArgs)
    val interfaceVfqn  = abilityInterfaceVfqn(abilityFQN)
    val interfaceName  = NativeType.convertToNestedClassName(interfaceVfqn)
    val singletonVfqn  = ValueFQN(outerClassGenerator.moduleName, QualifiedName(innerClassName, Qualifier.Default))

    for {
      singletonCg <- outerClassGenerator.createInnerClassGenerator[F](innerClassName, Seq(interfaceName))
      _           <- singletonCg.createStaticFinalField[F]("INSTANCE", interfaceVfqn)
      _           <- singletonCg.createCtor[F](Seq.empty).use { ctor =>
                       ctor.addLoadThis[F]() >> ctor.addCallToObjectCtor[F]()
                     }
      _           <- singletonCg.createStaticInit[F]().use { clinit =>
                       clinit.addNew[F](singletonVfqn) >>
                         clinit.addCallToCtor[F](singletonVfqn, Seq.empty) >>
                         clinit.addPutStaticField[F]("INSTANCE", interfaceVfqn)
                     }
      _           <- methodPairs.traverse_ { (abilityMethod, implMethod) =>
                       val (ifaceParams, ifaceReturn) = extractSignatureTypes(abilityMethod.signature)
                       val (implParams, implReturn)   = extractSignatureTypes(implMethod.signature)
                       singletonCg
                         .createPublicInstanceMethod[F](
                           abilityMethod.vfqn.name.name,
                           ifaceParams.map(simpleType),
                           simpleType(ifaceReturn)
                         )
                         .use { bridge =>
                           ifaceParams.zipWithIndex.traverse_ { (ifaceParam, idx) =>
                             bridge.addLoadVar[F](simpleType(ifaceParam), idx + 1) >>
                               bridge
                                 .addCastTo[F](simpleType(implParams(idx)))
                                 .whenA(simpleType(implParams(idx)) =!= simpleType(ifaceParam))
                           } >> bridge.addCallTo[F](implMethod.vfqn, implParams.map(simpleType), simpleType(implReturn))
                         }
                     }
      classFile   <- singletonCg.generate[F]()
    } yield classFile
  }

  private def abilityInterfaceVfqn(abilityFQN: AbilityFQN): ValueFQN =
    ValueFQN(abilityFQN.moduleName, QualifiedName(abilityFQN.abilityName + "$vtable", Qualifier.Default))

  private def singletonInnerName(abilityFQN: AbilityFQN, typeArgs: Seq[ExpressionValue]): String =
    abilityFQN.abilityName + "$" + typeArgs.map(arg => simpleType(arg).name.name).mkString("$") + "$impl"

  private def extractSignatureTypes(signature: ExpressionValue): (Seq[ExpressionValue], ExpressionValue) = {
    def loop(expr: ExpressionValue, acc: Seq[ExpressionValue]): (Seq[ExpressionValue], ExpressionValue) =
      expr match {
        case ExpressionValue.FunctionType(paramType, returnType) => loop(returnType, acc :+ paramType)
        case _                                                   => (acc, expr)
      }
    loop(signature, Seq.empty)
  }
}
