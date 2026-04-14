package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{Expression => CoreExpression}
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.{ClassGenerator, JvmIdentifier}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.ClassGenerator.createClassGenerator
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.CommonPatterns
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.CommonPatterns.{
  constructorArityValue,
  constructorDataTypeValue,
  mangleSuffix,
  valueType
}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType.{convertToNestedClassName, systemAnyValue, systemFunctionValue, systemUnitValue}
import com.vanillasource.eliot.eliotc.jvm.classgen.fact.{ClassFile, GeneratedModule}
import com.vanillasource.eliot.eliotc.jvm.classgen.processor.DataClassGenerator.{isConstructor, isTypeConstructor}
import com.vanillasource.eliot.eliotc.jvm.classgen.processor.ExpressionCodeGenerator.{createExpressionCode, patternMatchSingletonName}
import com.vanillasource.eliot.eliotc.jvm.classgen.processor.NativeImplementation.implementations
import com.vanillasource.eliot.eliotc.jvm.classgen.processor.TypeState.*
import com.vanillasource.eliot.eliotc.implementation.util.ImplementationMarkerUtils
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, UnifiedModuleNames, ValueFQN}
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.operator.fact.{OperatorResolvedExpression, OperatorResolvedValue}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort
import com.vanillasource.eliot.eliotc.uncurry.fact.*
import com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedMonomorphicExpression.*
import com.vanillasource.eliot.eliotc.used.UsedNames
import com.vanillasource.eliot.eliotc.used.UsedNames.UsageStats

class JvmClassGenerator extends SingleKeyTypeProcessor[GeneratedModule.Key] with Logging {

  override protected def generateFact(key: GeneratedModule.Key): CompilerIO[Unit] =
    for {
      _                      <- debug[CompilerIO](s"Generating ${key.moduleName.show} (from ${key.vfqn.show}).")
      usedNames              <- getFactOrAbort(UsedNames.Key(key.vfqn))
      usedValues              = usedNames.usedNames.filter((vfqn, _) => vfqn.moduleName === key.moduleName)
      mainClassGenerator     <- createClassGenerator[CompilerIO](key.moduleName)
      moduleNames            <- getFactOrAbort(UnifiedModuleNames.Key(key.moduleName))
      // Get all constructor names in this module
      allCtorNames            = moduleNames.names.keys.toSeq.filter(qn => isConstructor(ValueFQN(key.moduleName, qn)))
      // Group ALL constructors by data type using evaluated return types
      allCtorsWithDataType   <- allCtorNames.traverseFilter { qn =>
                                  val vfqn = ValueFQN(key.moduleName, qn)
                                  evaluateConstructorDataType(vfqn).map(_.map(dt => (vfqn, dt)))
                                }
      allCtorGroups           = allCtorsWithDataType.groupBy((_, dt) => dt)
      // Collect used constructors
      usedCtorVfqns           = usedValues.filter((vfqn, _) => isConstructor(vfqn)).keySet
      // Find PatternMatch handleCases impls and map to their data types
      handleCasesByDataType  <- usedValues.toSeq.traverseFilter { (vfqn, stats) =>
                                  vfqn.name.qualifier match {
                                    case Qualifier.AbilityImplementation(abilityName, _)
                                        if abilityName.value === "PatternMatch" && vfqn.name.name === "handleCases" =>
                                      ImplementationMarkerUtils
                                        .firstPatternTypeConstructorName(vfqn, "PatternMatch")
                                        .map {
                                          case Some(typeName) =>
                                            val dataTypeVfqn = allCtorGroups.keys
                                              .find(_.name.name === typeName)
                                              .getOrElse(ValueFQN(key.moduleName, QualifiedName(typeName, Qualifier.Default)))
                                            Some((dataTypeVfqn, (vfqn, stats)))
                                          case None           => Option.empty
                                        }
                                    case _ =>
                                      Option.empty.pure[CompilerIO]
                                  }
                                }
      handleCasesMap          = handleCasesByDataType.toMap
      _                      <- debug[CompilerIO](s"Module ${key.moduleName.show}: handleCasesMap=${handleCasesMap.keys.map(_.show).mkString(", ")}")
      // Collect ALL PatternMatch impl VFQNs to exclude from normal processing
      allPatternMatchVfqns    = usedValues.keys.filter { vfqn =>
                                  vfqn.name.qualifier match {
                                    case Qualifier.AbilityImplementation(name, _) => name.value === "PatternMatch"
                                    case _                                        => false
                                  }
                                }.toSet
      // Collect ALL TypeMatch impl VFQNs to exclude from normal processing
      allTypeMatchVfqns       = usedValues.keys.filter { vfqn =>
                                  vfqn.name.qualifier match {
                                    case Qualifier.AbilityImplementation(name, _) => name.value === "TypeMatch"
                                    case _                                        => false
                                  }
                                }.toSet
      // Map each typeMatch method VFQN to its corresponding type constructor name (via the marker signature)
      typeMatchByConstructor <- allTypeMatchVfqns
                                  .filter(_.name.name === "typeMatch")
                                  .toSeq
                                  .traverseFilter { tmVfqn =>
                                    ImplementationMarkerUtils
                                      .firstPatternTypeConstructorName(tmVfqn, "TypeMatch")
                                      .map(_.map(_ -> tmVfqn))
                                  }
                                  .map(_.toMap)
      // Process each data type: check handleCases usage, merge constructors, generate classes
      dataResults            <- allCtorGroups.toSeq.traverse { (typeVFQ, allTypeCtors) =>
                                  val handleCasesUsed = handleCasesMap.contains(typeVFQ)
                                  val usedFromType    = allTypeCtors.filter((vfqn, _) => usedCtorVfqns.contains(vfqn))
                                  if (!handleCasesUsed && usedFromType.isEmpty) {
                                    (Seq.empty[ClassFile], Seq.empty[ValueFQN]).pure[CompilerIO]
                                  } else {
                                    // When handleCases is used, include ALL constructors for virtual dispatch
                                    val ctorVfqns       = if (handleCasesUsed) allTypeCtors.map(_._1) else usedFromType.map(_._1)
                                    val defaultTypeArgs  = usedFromType
                                      .flatMap((v, _) => usedValues.get(v))
                                      .flatMap(_.monomorphicTypeParameters.headOption)
                                      .headOption
                                      .getOrElse(Seq.empty)
                                    for {
                                      // Fetch UncurriedMonomorphicValue for each constructor
                                      ctorsWithInfo   <- ctorVfqns.traverse { vfqn =>
                                                           val stats    = usedValues.getOrElse(vfqn, UsageStats(Seq(defaultTypeArgs), Map(0 -> 1)))
                                                           val typeArgs = stats.monomorphicTypeParameters.headOption.getOrElse(defaultTypeArgs)
                                                           val arity    = stats.highestArity.getOrElse(0)
                                                           getFactOrAbort(UncurriedMonomorphicValue.Key(vfqn, typeArgs, arity))
                                                             .map(umv => (vfqn, stats, umv))
                                                         }
                                      sortedCtors      = ctorsWithInfo.sortBy { case (_, _, umv) =>
                                                           (umv.name.range.from.line, umv.name.range.from.col)
                                                         }
                                      classes          <- if (ctorsWithInfo.size === 1) {
                                                            val (vfqn, _, umv) = ctorsWithInfo.head
                                                            DataClassGenerator.createSingleConstructorData[CompilerIO](
                                                              mainClassGenerator, vfqn, umv.parameters, handleCasesUsed
                                                            )
                                                          } else {
                                                            DataClassGenerator.createMultiConstructorData[CompilerIO](
                                                              mainClassGenerator, typeVFQ,
                                                              sortedCtors.zipWithIndex.map { case ((v, _, umv), idx) => (v, umv.parameters, idx) },
                                                              handleCasesUsed
                                                            )
                                                          }
                                      singletonClasses <- if (handleCasesUsed) {
                                                            generatePatternMatchSingleton(
                                                              mainClassGenerator, typeVFQ,
                                                              ctorsWithInfo.size === 1
                                                            ).map(Seq(_))
                                                          } else {
                                                            Seq.empty[ClassFile].pure[CompilerIO]
                                                          }
                                      generatedFunctions = ctorsWithInfo.map(_._1)
                                    } yield (classes ++ singletonClasses, generatedFunctions)
                                  }
                                }
      dataClasses             = dataResults.flatMap(_._1)
      dataGeneratedFunctions  = dataResults.flatMap(_._2)
      // Get all type constructor names at arity 0 (for identification and arity computation)
      allTypeCtorNames        = moduleNames.names.keys.toSeq.filter(qn =>
                                  isTypeConstructor(ValueFQN(key.moduleName, qn))
                                )
      allTypeCtorsArityZero  <- allTypeCtorNames.traverseFilter { qn =>
                                  val vfqn     = ValueFQN(key.moduleName, qn)
                                  val stats    = usedValues.get(vfqn)
                                  val typeArgs = stats.flatMap(_.monomorphicTypeParameters.headOption).getOrElse(Seq.empty)
                                  getFact(UncurriedMonomorphicValue.Key(vfqn, typeArgs, 0)).map(_.map(umv => (vfqn, umv)))
                                }
      // Determine which type constructors need data classes (used directly or via typeMatch)
      neededTypeCtors         = allTypeCtorsArityZero.filter { (vfqn, _) =>
                                  usedValues.contains(vfqn) || typeMatchByConstructor.contains(vfqn.name.name)
                                }
      // Get each type constructor at its proper arity
      usedTypeCtorsWithUmv   <- neededTypeCtors.traverse { (vfqn, umvZero) =>
                                  val stats    = usedValues.get(vfqn)
                                  val typeArgs = stats.flatMap(_.monomorphicTypeParameters.headOption).getOrElse(Seq.empty)
                                  val arity    = stats match {
                                    case Some(s) => s.highestArity.getOrElse(constructorArityValue(umvZero.returnType))
                                    case None    => constructorArityValue(umvZero.returnType)
                                  }
                                  getFactOrAbort(UncurriedMonomorphicValue.Key(vfqn, typeArgs, arity)).map((vfqn, _))
                                }
      // Generate type constructor data classes and factory methods
      typeCtorClasses        <- usedTypeCtorsWithUmv.flatTraverse { (vfqn, umv) =>
                                  DataClassGenerator.createTypeConstructorData[CompilerIO](mainClassGenerator, vfqn, umv.parameters)
                                }
      // Generate typeMatch methods for used typeMatch functions
      typeMatchGenerated     <- usedTypeCtorsWithUmv.traverseFilter { (vfqn, umv) =>
                                  typeMatchByConstructor.get(vfqn.name.name) match {
                                    case Some(tmVfqn) =>
                                      val tmStats    = usedValues(tmVfqn)
                                      val tmTypeArgs = tmStats.monomorphicTypeParameters.headOption.getOrElse(Seq.empty)
                                      for {
                                        tmUmv <- getFactOrAbort(
                                                   UncurriedMonomorphicValue.Key(tmVfqn, tmTypeArgs, tmStats.highestArity.getOrElse(0))
                                                 )
                                        _     <- DataClassGenerator.generateTypeMatch[CompilerIO](
                                                   mainClassGenerator, tmUmv.parameters, tmUmv.returnType, vfqn, umv.parameters
                                                 )
                                      } yield Some(tmVfqn)
                                    case None         => Option.empty[ValueFQN].pure[CompilerIO]
                                  }
                                }
      // Generate Type marker interface if any type constructors are used
      typeInterfaceClass     <- if (usedTypeCtorsWithUmv.nonEmpty) {
                                  DataClassGenerator.generateTypeInterface[CompilerIO]().map(Seq(_))
                                } else Seq.empty[ClassFile].pure[CompilerIO]
      typeCtorGeneratedFns    = usedTypeCtorsWithUmv.map(_._1) ++ typeMatchGenerated
      allGeneratedFunctions   = dataGeneratedFunctions ++ typeCtorGeneratedFns ++ allPatternMatchVfqns ++ allTypeMatchVfqns
      functionFiles          <-
        usedValues.view
          .filterKeys(k => !allGeneratedFunctions.contains(k))
          .toSeq
          .flatTraverse { case (vfqn, stats) =>
            createModuleMethod(mainClassGenerator, vfqn, stats)
          }
      mainClass              <- mainClassGenerator.generate[CompilerIO]()
      _                      <- registerFactIfClear(
                                  GeneratedModule(
                                    key.moduleName,
                                    key.vfqn,
                                    functionFiles ++ dataClasses ++ typeCtorClasses ++ typeInterfaceClass ++ Seq(mainClass)
                                  )
                                )
    } yield ()

  private def createModuleMethod(
      mainClassGenerator: ClassGenerator,
      vfqn: ValueFQN,
      stats: UsageStats
  ): CompilerIO[Seq[ClassFile]] = {
    implementations.get(vfqn) match {
      case Some(nativeImplementation) =>
        nativeImplementation.generateMethod(mainClassGenerator).as(Seq.empty)
      case None                       =>
        val distinctTypeArgs = stats.monomorphicTypeParameters.distinct
        val arities          = stats.directCallApplications.keys.toSeq.sorted
        for {
          result <- distinctTypeArgs.foldLeftM((Seq.empty[ClassFile], 0)) { case ((accFiles, lambdaCount), typeArgs) =>
                      arities.foldLeftM((accFiles, lambdaCount)) { case ((innerAccFiles, innerLambdaCount), arity) =>
                        for {
                          uncurriedValue          <- getFactOrAbort(UncurriedMonomorphicValue.Key(vfqn, typeArgs, arity))
                          (files, newLambdaCount) <- createModuleMethod(
                                                       mainClassGenerator, uncurriedValue, typeArgs, innerLambdaCount
                                                     )
                        } yield (innerAccFiles ++ files, newLambdaCount)
                      }
                    }
          (classFiles, _)   = result
          // Check if this is the main function
          highestTypeArgs    = distinctTypeArgs.headOption.getOrElse(Seq.empty)
          highestUncurried  <- getFactOrAbort(
                                 UncurriedMonomorphicValue.Key(vfqn, highestTypeArgs, stats.highestArity.getOrElse(0))
                               )
          _                 <- createApplicationMain(vfqn, mainClassGenerator).whenA(isMain(highestUncurried))
        } yield classFiles
    }
  }

  private def createModuleMethod(
      classGenerator: ClassGenerator,
      uncurriedValue: UncurriedMonomorphicValue,
      typeArgs: Seq[GroundValue],
      initialLambdaCount: Int = 0
  ): CompilerIO[(Seq[ClassFile], Int)] = {
    uncurriedValue.body match {
      case Some(body) =>
        val methodName = uncurriedValue.vfqn.name.name + mangleSuffix(typeArgs)
        classGenerator
          .createMethod[CompilerIO](
            JvmIdentifier.encode(methodName),
            uncurriedValue.parameters.map(p => valueType(p.parameterType)),
            valueType(uncurriedValue.returnType)
          )
          .use { methodGenerator =>
            val bodyExpression = UncurriedMonomorphicExpression(uncurriedValue.returnType, body.value)
            val program        = for {
              _       <- uncurriedValue.parameters.traverse_(addParameterDefinition)
              classes <-
                createExpressionCode(
                  uncurriedValue.vfqn.moduleName,
                  classGenerator,
                  methodGenerator,
                  bodyExpression
                )
              _       <-
                debug[CompilationTypesIO](
                  s"From function ${uncurriedValue.vfqn.show}, created: ${classes.map(_.fileName).mkString(", ")}"
                )
              state   <- StateT.get[CompilerIO, TypeState]
            } yield (classes, state.lambdaCount)

            program
              .run(TypeState(methodName = uncurriedValue.vfqn.name.name, lambdaCount = initialLambdaCount))
              .map(_._2)
          }
      case None       =>
        compilerAbort(uncurriedValue.name.as(s"Function not implemented."))
    }
  }

  private def createApplicationMain(mainVfqn: ValueFQN, generator: ClassGenerator): CompilerIO[Unit] =
    generator.createMainMethod[CompilerIO]().use { methodGenerator =>
      methodGenerator.addCallTo(mainVfqn, Seq.empty, systemUnitValue)
    }

  private def isMain(uncurriedValue: UncurriedMonomorphicValue): Boolean =
    uncurriedValue.vfqn.name.name === "main" && uncurriedValue.parameters.isEmpty

  private def generatePatternMatchSingleton(
      mainClassGenerator: ClassGenerator,
      dataTypeVfqn: ValueFQN,
      isSingleConstructor: Boolean
  ): CompilerIO[ClassFile] = {
    val innerClassName = patternMatchSingletonName(dataTypeVfqn)
    val singletonVfqn = ValueFQN(mainClassGenerator.moduleName, QualifiedName(innerClassName, Qualifier.Default))
    val dataTypeName   = convertToNestedClassName(dataTypeVfqn)

    for {
      singletonCg <- mainClassGenerator.createInnerClassGenerator[CompilerIO](
                        JvmIdentifier.encode(innerClassName), Seq.empty
                      )
      _           <- singletonCg.createStaticFinalField[CompilerIO](JvmIdentifier("INSTANCE"), singletonVfqn)
      _           <- singletonCg.createCtor[CompilerIO](Seq.empty).use { ctor =>
                       ctor.addLoadThis[CompilerIO]() >> ctor.addCallToObjectCtor[CompilerIO]()
                     }
      _           <- singletonCg.createStaticInit[CompilerIO]().use { clinit =>
                       clinit.addNew[CompilerIO](singletonVfqn) >>
                         clinit.addCallToCtor[CompilerIO](singletonVfqn, Seq.empty) >>
                         clinit.addPutStaticField[CompilerIO](JvmIdentifier("INSTANCE"), singletonVfqn)
                     }
      _           <- singletonCg
                       .createPublicInstanceMethod[CompilerIO](
                         JvmIdentifier.encode("handleCases"),
                         Seq(systemAnyValue, systemFunctionValue),
                         systemAnyValue
                       )
                       .use { bridge =>
                         for {
                           _ <- bridge.addLoadVar[CompilerIO](systemAnyValue, 1)
                           _ <- bridge.addCastTo[CompilerIO](dataTypeVfqn)
                           _ <- bridge.addLoadVar[CompilerIO](systemFunctionValue, 2)
                           _ <-
                             if (isSingleConstructor) {
                               bridge.addCallToVirtualMethod[CompilerIO](
                                 dataTypeName,
                                 JvmIdentifier.encode("handleCases"),
                                 Seq(systemFunctionValue),
                                 systemAnyValue
                               )
                             } else {
                               bridge.addCallToAbilityMethod[CompilerIO](
                                 dataTypeName,
                                 JvmIdentifier.encode("handleCases"),
                                 Seq(systemFunctionValue),
                                 systemAnyValue
                               )
                             }
                         } yield ()
                       }
      classFile   <- singletonCg.generate[CompilerIO]()
    } yield classFile
  }

  private def evaluateConstructorDataType(vfqn: ValueFQN): CompilerIO[Option[ValueFQN]] =
    getFact(OperatorResolvedValue.Key(vfqn)).map {
      case None           => Option.empty[ValueFQN]
      case Some(resolved) =>
        extractReturnTypeRef(resolved.typeStack.value.signature).map(CommonPatterns.stripDataTypeSuffix)
    }

  @scala.annotation.tailrec
  private def stripFunctionLiterals(expr: OperatorResolvedExpression): OperatorResolvedExpression =
    expr match {
      case OperatorResolvedExpression.FunctionLiteral(_, _, body) => stripFunctionLiterals(body.value)
      case other                                                  => other
    }

  @scala.annotation.tailrec
  private def stripApplicationTargets(expr: OperatorResolvedExpression): OperatorResolvedExpression =
    expr match {
      case OperatorResolvedExpression.FunctionApplication(target, _) => stripApplicationTargets(target.value)
      case other                                                     => other
    }

  private def extractReturnTypeRef(expr: OperatorResolvedExpression): Option[ValueFQN] =
    stripApplicationTargets(stripCurriedReturnType(stripFunctionLiterals(expr))) match {
      case OperatorResolvedExpression.ValueReference(vfqn, _) => Some(vfqn.value)
      case _                                                  => None
    }

  @scala.annotation.tailrec
  private def stripCurriedReturnType(expr: OperatorResolvedExpression): OperatorResolvedExpression =
    expr match {
      case OperatorResolvedExpression.FunctionApplication(target, argument)
          if isFunctionTypeApplication(target.value) =>
        stripCurriedReturnType(argument.value)
      case _ => expr
    }

  private def isFunctionTypeApplication(expr: OperatorResolvedExpression): Boolean =
    expr match {
      case OperatorResolvedExpression.FunctionApplication(innerTarget, _) =>
        stripApplicationTargets(innerTarget.value) match {
          case OperatorResolvedExpression.ValueReference(vfqn, _) =>
            vfqn.value.name.name === "Function" && vfqn.value.name.qualifier === Qualifier.Type
          case _                                                  => false
        }
      case _                                                              => false
    }

}
