package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import cats.effect.Sync
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.{ClassGenerator, JvmIdentifier}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.CommonPatterns.{addDataFieldsAndCtor, simpleType}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.ClassGenerator.createInterfaceGenerator
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType.{convertToNestedClassName, systemFunctionValue, systemTypeValue}
import com.vanillasource.eliot.eliotc.jvm.classgen.fact.ClassFile
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, ValueFQN}
import com.vanillasource.eliot.eliotc.module.fact.ModuleName.defaultSystemPackage
import com.vanillasource.eliot.eliotc.uncurry.fact.{ParameterDefinition, UncurriedValue}

import scala.annotation.tailrec

object DataClassGenerator {

  def isConstructor(valueFQN: ValueFQN): Boolean =
    valueFQN.name.qualifier === Qualifier.Default && valueFQN.name.name.charAt(0).isUpper

  def isTypeConstructor(valueFQN: ValueFQN): Boolean =
    valueFQN.name.qualifier === Qualifier.Type && valueFQN.name.name.charAt(0).isUpper

  @tailrec
  def constructorDataType(returnType: ExpressionValue): ExpressionValue =
    returnType match {
      case ExpressionValue.FunctionType(_, inner) => constructorDataType(inner)
      case other                                  => other
    }

  def constructorArity(returnType: ExpressionValue): Int =
    returnType match {
      case ExpressionValue.FunctionType(_, inner) => 1 + constructorArity(inner)
      case _                                      => 0
    }

  /** Single-constructor data: generates a concrete class, factory method, and optional eliminator. */
  def createSingleConstructorData[F[_]: Sync](
      outerClassGenerator: ClassGenerator,
      valueFQN: ValueFQN,
      uncurriedValue: UncurriedValue,
      handleWithUncurried: Option[UncurriedValue],
      eliminatorName: String
  ): F[Seq[ClassFile]] =
    for {
      cs <- createDataClassWithHandleWith(
              outerClassGenerator,
              valueFQN.name.name,
              uncurriedValue.parameters,
              Seq.empty,
              handleWithUncurried.map(hw => Seq((0, uncurriedValue.parameters, hw))),
              eliminatorName
            )
      _  <- createFactoryMethod(outerClassGenerator, valueFQN, uncurriedValue.parameters, valueFQN)
      _  <- handleWithUncurried.traverse_ { hw =>
              generateStaticHandleWith(outerClassGenerator, hw, valueFQN, isInterface = false, eliminatorName)
            }
    } yield cs

  /** Multi-constructor (union) data: generates an interface + implementation classes + factory methods + eliminator. */
  def createMultiConstructorData[F[_]: Sync](
      outerClassGenerator: ClassGenerator,
      typeVFQ: ValueFQN,
      ctors: Seq[(ValueFQN, UncurriedValue)],
      handleWithUncurried: Option[UncurriedValue],
      eliminatorName: String
  ): F[Seq[ClassFile]] =
    for {
      // Create the interface for the data type
      interfaceGen   <- outerClassGenerator.createInnerInterfaceGenerator[F](JvmIdentifier.encode(typeVFQ.name.name))
      // Add abstract eliminator method to the interface if used
      _              <- handleWithUncurried.traverse_ { hw =>
                          val handlerParams = hw.parameters.drop(1)
                          interfaceGen.createAbstractMethod[F](
                            JvmIdentifier.encode(eliminatorName),
                            handlerParams.map(_.parameterType).map(simpleType),
                            simpleType(hw.returnType)
                          )
                        }
      interfaceClass <- interfaceGen.generate[F]()
      interfaceName   = convertToNestedClassName(typeVFQ)
      // Sort constructors by source position to match definition order (= handleWith handler order)
      sortedCtors     = ctors.sortBy { case (_, uv) => (uv.name.range.from.line, uv.name.range.from.col) }
      ctorIndexMap    = sortedCtors.zipWithIndex.map { case ((vfqn, _), idx) => vfqn -> idx }.toMap
      // Create implementation classes and factory methods for each constructor
      implClasses    <- ctors.flatTraverse { case (vfqn, uncurriedValue) =>
                          val ctorIndex = ctorIndexMap.getOrElse(vfqn, 0)
                          for {
                            cs <- createDataClassWithHandleWith(
                                    outerClassGenerator,
                                    vfqn.name.name,
                                    uncurriedValue.parameters,
                                    Seq(interfaceName),
                                    handleWithUncurried.map(hw => Seq((ctorIndex, uncurriedValue.parameters, hw))),
                                    eliminatorName
                                  )
                            _  <- createFactoryMethod(outerClassGenerator, vfqn, uncurriedValue.parameters, typeVFQ)
                          } yield cs
                        }
      // Generate static eliminator method (delegates to INVOKEINTERFACE)
      _              <- handleWithUncurried.traverse_ { hw =>
                          generateStaticHandleWith(outerClassGenerator, hw, typeVFQ, isInterface = true, eliminatorName)
                        }
    } yield Seq(interfaceClass) ++ implClasses

  private def createFactoryMethod[F[_]: Sync](
      outerClassGenerator: ClassGenerator,
      constructorVfqn: ValueFQN,
      parameters: Seq[ParameterDefinition],
      returnType: ValueFQN
  ): F[Unit] =
    outerClassGenerator
      .createMethod[F](
        JvmIdentifier.encode(constructorVfqn.name.name),
        parameters.map(_.parameterType).map(simpleType),
        returnType
      )
      .use { methodGenerator =>
        for {
          _ <- methodGenerator.addNew[F](constructorVfqn)
          _ <- parameters.zipWithIndex.traverse_ { (fieldDef, index) =>
                 methodGenerator.addLoadVar[F](simpleType(fieldDef.parameterType), index)
               }
          _ <- methodGenerator.addCallToCtor[F](
                 constructorVfqn,
                 parameters.map(_.parameterType).map(simpleType)
               )
        } yield ()
      }

  /** Create a data class with fields, constructor, and optionally an eliminator instance method.
    * @param innerClassName
    *   The raw (un-encoded) inner class name. Will be encoded when passed to ASM.
    * @param handleWithInfo
    *   If present, Seq of (constructorIndex, constructorFields, handleWithUncurried) for generating eliminator
    *   override.
    */
  private def createDataClassWithHandleWith[F[_]: Sync](
      outerClassGenerator: ClassGenerator,
      innerClassName: String,
      fields: Seq[ParameterDefinition],
      javaInterfaces: Seq[String] = Seq.empty,
      handleWithInfo: Option[Seq[(Int, Seq[ParameterDefinition], UncurriedValue)]] = None,
      eliminatorName: String = ""
  ): F[Seq[ClassFile]] =
    for {
      innerClassWriter <-
        outerClassGenerator.createInnerClassGenerator[F](JvmIdentifier.encode(innerClassName), javaInterfaces)
      _                <- innerClassWriter.addDataFieldsAndCtor[F](fields)
      // Generate eliminator instance method override if requested
      _                <- handleWithInfo.traverse_ { infos =>
                            infos.traverse_ { case (ctorIndex, ctorFields, hw) =>
                              val handlerParams = hw.parameters.drop(1)
                              innerClassWriter
                                .createPublicInstanceMethod[F](
                                  JvmIdentifier.encode(eliminatorName),
                                  handlerParams.map(_.parameterType).map(simpleType),
                                  simpleType(hw.returnType)
                                )
                                .use { methodGenerator =>
                                  val handlerLocalIndex = ctorIndex + 1
                                  for {
                                    _ <- methodGenerator.addLoadVar[F](systemFunctionValue, handlerLocalIndex)
                                    _ <-
                                      if (ctorFields.isEmpty) {
                                        for {
                                          _ <- methodGenerator.addConstNull[F]()
                                          _ <- methodGenerator.addCallToApply[F]()
                                        } yield ()
                                      } else {
                                        ctorFields.zipWithIndex.traverse_ { (fieldDef, fieldIndex) =>
                                          val vfqn = ValueFQN(
                                            outerClassGenerator.moduleName,
                                            QualifiedName(innerClassName, Qualifier.Default)
                                          )
                                          for {
                                            _ <- methodGenerator.addLoadThis[F]()
                                            _ <- methodGenerator.addGetField[F](
                                                   JvmIdentifier.encode(fieldDef.name.value),
                                                   simpleType(fieldDef.parameterType),
                                                   vfqn
                                                 )
                                            _ <- methodGenerator.addCallToApply[F]()
                                            _ <- methodGenerator
                                                   .addCastTo[F](systemFunctionValue)
                                                   .whenA(fieldIndex < ctorFields.size - 1)
                                          } yield ()
                                        }
                                      }
                                  } yield ()
                                }
                            }
                          }
      classFile        <- innerClassWriter.generate[F]()
    } yield Seq(classFile)

  /** Generate a static eliminator method on the module class that delegates to virtual/interface dispatch. */
  private def generateStaticHandleWith[F[_]: Sync](
      outerClassGenerator: ClassGenerator,
      hw: UncurriedValue,
      dataTypeVfqn: ValueFQN,
      isInterface: Boolean,
      eliminatorName: String
  ): F[Unit] = {
    val allParamTypes = hw.parameters.map(_.parameterType).map(simpleType)
    val returnType    = simpleType(hw.returnType)
    val handlerParams = hw.parameters.drop(1)
    outerClassGenerator
      .createMethod[F](
        JvmIdentifier.encode(eliminatorName),
        allParamTypes,
        returnType
      )
      .use { methodGenerator =>
        for {
          _ <- methodGenerator.addLoadVar[F](simpleType(hw.parameters.head.parameterType), 0)
          _ <- handlerParams.zipWithIndex.traverse_ { (param, index) =>
                 methodGenerator.addLoadVar[F](simpleType(param.parameterType), index + 1)
               }
          _ <-
            if (isInterface) {
              methodGenerator.addCallToAbilityMethod[F](
                convertToNestedClassName(dataTypeVfqn),
                JvmIdentifier.encode(eliminatorName),
                handlerParams.map(_.parameterType).map(simpleType),
                returnType
              )
            } else {
              methodGenerator.addCallToVirtualMethod[F](
                convertToNestedClassName(dataTypeVfqn),
                JvmIdentifier.encode(eliminatorName),
                handlerParams.map(_.parameterType).map(simpleType),
                returnType
              )
            }
        } yield ()
      }
  }

  /** Create a type constructor data class that implements the external Type interface. */
  def createTypeConstructorData[F[_]: Sync](
      outerClassGenerator: ClassGenerator,
      constructorVfqn: ValueFQN,
      constructorUncurried: UncurriedValue
  ): F[Seq[ClassFile]] = {
    val typeInterfaceName = convertToNestedClassName(systemTypeValue)
    for {
      cs <- createDataClassWithHandleWith(
              outerClassGenerator,
              "type$" + constructorVfqn.name.name,
              constructorUncurried.parameters,
              Seq(typeInterfaceName),
              None,
              ""
            )
      _  <- createFactoryMethod(outerClassGenerator, constructorVfqn, constructorUncurried.parameters, systemTypeValue)
    } yield cs
  }

  /** Generate a static typeMatch method that uses instanceof dispatch. */
  def generateTypeMatch[F[_]: Sync](
      outerClassGenerator: ClassGenerator,
      typeMatchUncurried: UncurriedValue,
      constructorVfqn: ValueFQN,
      constructorFields: Seq[ParameterDefinition]
  ): F[Unit] = {
    val allParamTypes     = typeMatchUncurried.parameters.map(_.parameterType).map(simpleType)
    val returnType        = simpleType(typeMatchUncurried.returnType)
    val typeCtorClassName = convertToNestedClassName(constructorVfqn)
    outerClassGenerator
      .createMethod[F](
        JvmIdentifier.encode(typeMatchUncurried.vfqn.name.name),
        allParamTypes,
        returnType
      )
      .use { mg =>
        val elseLabel = mg.createLabel()
        val endLabel  = mg.createLabel()
        for {
          _ <- mg.addLoadVar[F](allParamTypes.head, 0)
          _ <- mg.addInstanceOf[F](typeCtorClassName)
          _ <- mg.addIfEq[F](elseLabel)
          // Match branch: apply matchCase handler to extracted fields
          _ <- mg.addLoadVar[F](systemFunctionValue, 1)
          _ <-
            if (constructorFields.isEmpty) {
              for {
                _ <- mg.addConstNull[F]()
                _ <- mg.addCallToApply[F]()
              } yield ()
            } else {
              constructorFields.zipWithIndex.traverse_ { (fieldDef, fieldIndex) =>
                for {
                  _ <- mg.addLoadVar[F](allParamTypes.head, 0)
                  _ <- mg.addCastTo[F](constructorVfqn)
                  _ <- mg.addGetField[F](
                         JvmIdentifier.encode(fieldDef.name.value),
                         simpleType(fieldDef.parameterType),
                         constructorVfqn
                       )
                  _ <- mg.addCallToApply[F]()
                  _ <- mg.addCastTo[F](systemFunctionValue).whenA(fieldIndex < constructorFields.size - 1)
                } yield ()
              }
            }
          _ <- mg.addGoto[F](endLabel)
          // Else branch: apply elseCase handler to null (Unit)
          _ <- mg.addLabel[F](elseLabel)
          _ <- mg.addLoadVar[F](systemFunctionValue, 2)
          _ <- mg.addConstNull[F]()
          _ <- mg.addCallToApply[F]()
          _ <- mg.addLabel[F](endLabel)
        } yield ()
      }
  }

  /** Generate the Type marker interface class that all type constructor data classes implement. */
  def generateTypeInterface[F[_]: Sync](): F[ClassFile] =
    for {
      interfaceGen <- createInterfaceGenerator[F](ModuleName(defaultSystemPackage, "Type$Type"))
      classFile    <- interfaceGen.generate[F]()
    } yield classFile
}
