package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import cats.effect.Sync
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.{ClassGenerator, JvmIdentifier}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.CommonPatterns.{addDataFieldsAndCtor, simpleType}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.ClassGenerator.createInterfaceGenerator
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType.{
  convertToNestedClassName,
  systemAnyValue,
  systemFunctionValue,
  systemTypeValue
}
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

  /** Single-constructor data: generates a concrete class, factory method, and optional handleCases. */
  def createSingleConstructorData[F[_]: Sync](
      outerClassGenerator: ClassGenerator,
      valueFQN: ValueFQN,
      uncurriedValue: UncurriedValue,
      generateHandleCases: Boolean
  ): F[Seq[ClassFile]] =
    for {
      cs <- createDataClass(
              outerClassGenerator,
              valueFQN.name.name,
              uncurriedValue.parameters,
              Seq.empty,
              if (generateHandleCases) Some((0, 1)) else None
            )
      _  <- createFactoryMethod(outerClassGenerator, valueFQN, uncurriedValue.parameters, valueFQN)
    } yield cs

  /** Multi-constructor (union) data: generates an interface + implementation classes + factory methods. */
  def createMultiConstructorData[F[_]: Sync](
      outerClassGenerator: ClassGenerator,
      typeVFQ: ValueFQN,
      ctors: Seq[(ValueFQN, UncurriedValue)],
      generateHandleCases: Boolean
  ): F[Seq[ClassFile]] =
    for {
      // Create the interface for the data type
      interfaceGen   <- outerClassGenerator.createInnerInterfaceGenerator[F](JvmIdentifier.encode(typeVFQ.name.name))
      // Add abstract handleCases method to the interface if used
      _              <- interfaceGen
                          .createAbstractMethod[F](
                            JvmIdentifier.encode("handleCases"),
                            Seq(systemFunctionValue),
                            systemAnyValue
                          )
                          .whenA(generateHandleCases)
      interfaceClass <- interfaceGen.generate[F]()
      interfaceName   = convertToNestedClassName(typeVFQ)
      // Sort constructors by source position to match definition order (= handler order)
      sortedCtors     = ctors.sortBy { case (_, uv) => (uv.name.range.from.line, uv.name.range.from.col) }
      ctorIndexMap    = sortedCtors.zipWithIndex.map { case ((vfqn, _), idx) => vfqn -> idx }.toMap
      totalCtors      = ctors.size
      // Create implementation classes and factory methods for each constructor
      implClasses    <- ctors.flatTraverse { case (vfqn, uncurriedValue) =>
                          val ctorIndex = ctorIndexMap.getOrElse(vfqn, 0)
                          for {
                            cs <- createDataClass(
                                    outerClassGenerator,
                                    vfqn.name.name,
                                    uncurriedValue.parameters,
                                    Seq(interfaceName),
                                    if (generateHandleCases) Some((ctorIndex, totalCtors)) else None
                                  )
                            _  <- createFactoryMethod(outerClassGenerator, vfqn, uncurriedValue.parameters, typeVFQ)
                          } yield cs
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

  /** Create a data class with fields, constructor, and optionally a handleCases instance method that implements
    * Church-encoded pattern matching.
    *
    * @param handleCasesInfo
    *   If present, (constructorIndex, totalConstructors) for generating handleCases with Church encoding.
    */
  private def createDataClass[F[_]: Sync](
      outerClassGenerator: ClassGenerator,
      innerClassName: String,
      fields: Seq[ParameterDefinition],
      javaInterfaces: Seq[String] = Seq.empty,
      handleCasesInfo: Option[(Int, Int)] = None
  ): F[Seq[ClassFile]] =
    for {
      innerClassWriter <-
        outerClassGenerator.createInnerClassGenerator[F](JvmIdentifier.encode(innerClassName), javaInterfaces)
      _                <- innerClassWriter.addDataFieldsAndCtor[F](fields)
      // Generate handleCases instance method with Church encoding if requested
      selectorClasses  <- handleCasesInfo match {
                            case Some((ctorIndex, totalCtors)) =>
                              val ctorVfqn =
                                ValueFQN(
                                  outerClassGenerator.moduleName,
                                  QualifiedName(innerClassName, Qualifier.Default)
                                )
                              for {
                                lambdaFiles <- generateSelectorLambdas(
                                                 outerClassGenerator,
                                                 innerClassName,
                                                 ctorIndex,
                                                 totalCtors,
                                                 fields
                                               )
                                _           <- innerClassWriter
                                                 .createPublicInstanceMethod[F](
                                                   JvmIdentifier.encode("handleCases"),
                                                   Seq(systemFunctionValue),
                                                   systemAnyValue
                                                 )
                                                 .use { mg =>
                                                   val selector0Name =
                                                     s"handleCases$$${innerClassName}$$0"
                                                   val selector0Vfqn = ValueFQN(
                                                     outerClassGenerator.moduleName,
                                                     QualifiedName(selector0Name, Qualifier.Default)
                                                   )
                                                   for {
                                                     // Load cases (param 1, after 'this')
                                                     _ <- mg.addLoadVar[F](systemFunctionValue, 1)
                                                     // Create Selector_0(this.field1, this.field2, ...)
                                                     _ <- mg.addNew[F](selector0Vfqn)
                                                     _ <- fields.traverse_ { field =>
                                                            for {
                                                              _ <- mg.addLoadThis[F]()
                                                              _ <- mg.addGetField[F](
                                                                     JvmIdentifier.encode(field.name.value),
                                                                     simpleType(field.parameterType),
                                                                     ctorVfqn
                                                                   )
                                                            } yield ()
                                                          }
                                                     _ <- mg.addCallToCtor[F](
                                                            selector0Vfqn,
                                                            fields.map(_.parameterType).map(simpleType)
                                                          )
                                                     // Call cases.apply(selector_0)
                                                     _ <- mg.addCallToApply[F]()
                                                   } yield ()
                                                 }
                              } yield lambdaFiles
                            case None                          =>
                              Seq.empty[ClassFile].pure[F]
                          }
      classFile        <- innerClassWriter.generate[F]()
    } yield classFile +: selectorClasses

  /** Generate the chain of selector lambda inner classes for Church-encoded pattern matching.
    *
    * For constructor at index `ctorIndex` out of `totalConstructors`, generates `totalConstructors` lambda classes.
    * Each lambda in the chain accepts one handler parameter. The chain ultimately applies the handler at position
    * `ctorIndex` to the constructor's fields.
    */
  private def generateSelectorLambdas[F[_]: Sync](
      outerClassGenerator: ClassGenerator,
      ctorName: String,
      ctorIndex: Int,
      totalConstructors: Int,
      ctorFields: Seq[ParameterDefinition]
  ): F[Seq[ClassFile]] = {
    val moduleName       = outerClassGenerator.moduleName
    val n                = totalConstructors
    val ctorFieldEntries = ctorFields.map(f => (f.name.value, simpleType(f.parameterType)))

    (0 until n).toList
      .traverse { (j: Int) =>
        val lambdaName = s"handleCases$$${ctorName}$$$j"
        val lambdaVfqn = ValueFQN(moduleName, QualifiedName(lambdaName, Qualifier.Default))
        val hasHandler = j > ctorIndex
        val isTarget   = j === ctorIndex
        val isLast     = j === n - 1

        val capturedEntries =
          if (hasHandler) ctorFieldEntries :+ ("$handler", systemFunctionValue)
          else ctorFieldEntries

        for {
          lambdaCg  <- outerClassGenerator.createInnerClassGenerator[F](
                         JvmIdentifier.encode(lambdaName),
                         Seq("java/util/function/Function")
                       )
          _         <- addFieldsAndCtor(lambdaCg, capturedEntries)
          _         <- lambdaCg.createApplyMethod[F](Seq(systemAnyValue), systemAnyValue).use { mg =>
                         if (!isLast) {
                           generateSelectorContinuation(
                             mg,
                             outerClassGenerator,
                             ctorName,
                             ctorIndex,
                             j,
                             n,
                             lambdaVfqn,
                             ctorFieldEntries,
                             hasHandler,
                             isTarget
                           )
                         } else {
                           generateSelectorTermination(
                             mg,
                             lambdaVfqn,
                             ctorFieldEntries,
                             isTarget
                           )
                         }
                       }
          classFile <- lambdaCg.generate[F]()
        } yield classFile
      }
      .map(_.toSeq)
  }

  /** Generate the body of a non-terminal selector lambda: creates the next selector in the chain. */
  private def generateSelectorContinuation[F[_]: Sync](
      mg: com.vanillasource.eliot.eliotc.jvm.classgen.asm.MethodGenerator,
      outerClassGenerator: ClassGenerator,
      ctorName: String,
      ctorIndex: Int,
      j: Int,
      n: Int,
      lambdaVfqn: ValueFQN,
      ctorFieldEntries: Seq[(String, ValueFQN)],
      hasHandler: Boolean,
      isTarget: Boolean
  ): F[Unit] = {
    val moduleName       = outerClassGenerator.moduleName
    val nextName         = s"handleCases$$${ctorName}$$${j + 1}"
    val nextVfqn         = ValueFQN(moduleName, QualifiedName(nextName, Qualifier.Default))
    val nextNeedsHandler = (j + 1) > ctorIndex
    val nextEntries      =
      if (nextNeedsHandler) ctorFieldEntries :+ ("$handler", systemFunctionValue)
      else ctorFieldEntries

    for {
      _ <- mg.addNew[F](nextVfqn)
      // Load constructor fields from this
      _ <- ctorFieldEntries.traverse_ { (name, fieldType) =>
             mg.addLoadVar[F](lambdaVfqn, 0) >> // this
               mg.addGetField[F](JvmIdentifier.encode(name), fieldType, lambdaVfqn)
           }
      // Load handler for next selector if needed
      _ <-
        if (isTarget) {
          // Capture the apply argument as handler
          mg.addLoadVar[F](systemAnyValue, 1)
        } else if (hasHandler) {
          // Pass through captured handler
          mg.addLoadVar[F](lambdaVfqn, 0) >>
            mg.addGetField[F](JvmIdentifier.encode("$handler"), systemFunctionValue, lambdaVfqn)
        } else {
          ().pure[F]
        }
      _ <- mg.addCallToCtor[F](nextVfqn, nextEntries.map(_._2))
    } yield ()
  }

  /** Generate the body of the terminal selector lambda: applies the target handler to constructor fields. */
  private def generateSelectorTermination[F[_]: Sync](
      mg: com.vanillasource.eliot.eliotc.jvm.classgen.asm.MethodGenerator,
      lambdaVfqn: ValueFQN,
      ctorFieldEntries: Seq[(String, ValueFQN)],
      isTarget: Boolean
  ): F[Unit] =
    for {
      // Get the handler: either the apply argument (if this is the target) or the captured $handler
      _ <-
        if (isTarget) {
          mg.addLoadVar[F](systemAnyValue, 1) >> mg.addCastTo[F](systemFunctionValue)
        } else {
          mg.addLoadVar[F](lambdaVfqn, 0) >>
            mg.addGetField[F](JvmIdentifier.encode("$handler"), systemFunctionValue, lambdaVfqn)
        }
      // Apply handler to constructor fields
      _ <-
        if (ctorFieldEntries.isEmpty) {
          mg.addConstNull[F]() >> mg.addCallToApply[F]()
        } else {
          ctorFieldEntries.zipWithIndex.traverse_ { case ((name, fieldType), fieldIdx) =>
            for {
              _ <- mg.addLoadVar[F](lambdaVfqn, 0) // this
              _ <- mg.addGetField[F](JvmIdentifier.encode(name), fieldType, lambdaVfqn)
              _ <- mg.addCallToApply[F]()
              _ <- mg.addCastTo[F](systemFunctionValue).whenA(fieldIdx < ctorFieldEntries.size - 1)
            } yield ()
          }
        }
    } yield ()

  /** Add fields and constructor to a class without requiring ParameterDefinition. */
  private def addFieldsAndCtor[F[_]: Sync](
      classGenerator: ClassGenerator,
      fields: Seq[(String, ValueFQN)]
  ): F[Unit] =
    for {
      _ <- fields.traverse_ { (name, fieldType) =>
             classGenerator.createField[F](JvmIdentifier.encode(name), fieldType)
           }
      _ <- classGenerator.createCtor[F](fields.map(_._2)).use { mg =>
             for {
               _ <- mg.addLoadThis[F]()
               _ <- mg.addCallToObjectCtor[F]()
               _ <- fields.zipWithIndex.traverse_ { case ((name, fieldType), index) =>
                      for {
                        _ <- mg.addLoadThis[F]()
                        _ <- mg.addLoadVar[F](fieldType, index + 1)
                        _ <- mg.addPutField[F](JvmIdentifier.encode(name), fieldType)
                      } yield ()
                    }
             } yield ()
           }
    } yield ()

  /** Create a type constructor data class that implements the external Type interface. */
  def createTypeConstructorData[F[_]: Sync](
      outerClassGenerator: ClassGenerator,
      constructorVfqn: ValueFQN,
      constructorUncurried: UncurriedValue
  ): F[Seq[ClassFile]] = {
    val typeInterfaceName = convertToNestedClassName(systemTypeValue)
    for {
      cs <- createDataClass(
              outerClassGenerator,
              "type$" + constructorVfqn.name.name,
              constructorUncurried.parameters,
              Seq(typeInterfaceName),
              None
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
