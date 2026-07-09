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
    else "$" + typeArgs.map(mangleTypeArgument).mkString("$")

  /** Mangle a single monomorphic type argument into a JVM-identifier token, recursing through type-constructor
    * application so that structurally-distinct instantiations sharing a head constructor get distinct names.
    *
    * The head token is the argument's erased carrier ([[valueType]]); the type-constructor arguments applied to it are
    * appended recursively. This is required because an effect carrier can appear at different *nesting depths* —
    * `AbortCarrier[IO]` for a top-level `{Abort}` computation versus `AbortCarrier[AbortCarrier[IO]]` for one nested
    * inside another `{Abort}` function (e.g. every `if` used inside another effectful body). Both share the head
    * `AbortCarrier` and, once carriers erase to reference types, the *same* JVM descriptor, yet they are genuinely
    * different specializations with different bodies (`AbortCarrier(IO(..))` vs `AbortCarrier(AbortCarrier(..))`). A
    * head-only suffix collapses them onto one method name; [[JvmClassGenerator]]'s signature-dedup then treats them as
    * byte-identical and silently drops one body — a whole-program miscompile surfacing as a runtime `ClassCastException`
    * in the wrong-carrier accessor. Recursing on the applied arguments keeps the depth distinct.
    *
    * Value-level (`Direct`) arguments — e.g. the `Int[MIN, MAX]` refinement bounds — carry no carrier structure and are
    * dropped, so `Int[0, 3]` and `Int[0, 5]` still mangle identically and legitimately share their one `Byte` method.
    */
  private def mangleTypeArgument(v: GroundValue): String =
    v match {
      case GroundValue.Structure(_, args, _) =>
        val structuralArgs = args.collect { case arg if !arg.isInstanceOf[GroundValue.Direct] => arg }
        (valueType(v).name.name +: structuralArgs.map(mangleTypeArgument)).mkString("$")
      case _                                 =>
        valueType(v).name.name
    }

  /** The JVM method name for a monomorphic value: its base name, plus an impl-disambiguator when the value is an
    * ability-implementation method, plus the type-argument suffix for generic instantiations.
    *
    * The impl-disambiguator is required because two implementations of the same ability share the method's *local* name
    * (`dependency`) and, when the method is return-type-dispatched into an erasing carrier, also share the erased JVM
    * descriptor: the `Dep` reader's native `Dep[X, DepCarrier[X, G]].dependency` and its cross-lift
    * `Dep[X2, DepCarrier[X1, G]].dependency` are both `() -> DepCarrier`. Without the implementation index in the name
    * they collide into one JVM method (a duplicate-method `ClassFormatError`, or one call silently binding to the wrong
    * impl). Implementations whose methods differ in their value-parameter descriptors (the common `Show[Hello]`/`Show[World]`
    * case) never collided, but folding the index in uniformly is harmless for them.
    */
  def mangledMethodName(vfqn: ValueFQN, typeArgs: Seq[GroundValue]): String =
    vfqn.name.name + implementationSuffix(vfqn.name.qualifier) + mangleSuffix(typeArgs)

  private def implementationSuffix(qualifier: Qualifier): String =
    qualifier match {
      // The implementation's identity is its `(ability, pattern)` key. The pattern string is not a legal JVM identifier
      // (it holds `[`, `,`, spaces), so it is encoded as a stable hex hash; deterministic per pattern, and the
      // monomorphized type-argument suffix appended by the caller further separates concrete instantiations.
      case Qualifier.AbilityImplementation(abilityName, pattern) =>
        "$" + abilityName + "$impl$" + Integer.toHexString(pattern.hashCode)
      case _                                                     => ""
    }

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
