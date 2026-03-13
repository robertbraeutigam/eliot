package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.{ClassGenerator, JvmIdentifier}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.ClassGenerator.createClassGenerator
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.CommonPatterns.{extractSignatureTypes, simpleType}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType.{convertToNestedClassName, systemAnyValue, systemFunctionValue, systemUnitValue}
import com.vanillasource.eliot.eliotc.jvm.classgen.processor.AbilityImplGenerator.{abilityInterfaceVfqn, singletonInnerName}
import com.vanillasource.eliot.eliotc.jvm.classgen.fact.{ClassFile, GeneratedModule}
import com.vanillasource.eliot.eliotc.jvm.classgen.processor.DataClassGenerator.{constructorArity, constructorDataType, isConstructor, isTypeConstructor}
import com.vanillasource.eliot.eliotc.jvm.classgen.processor.ExpressionCodeGenerator.{computeDictParams, createExpressionCode}
import com.vanillasource.eliot.eliotc.jvm.classgen.processor.NativeImplementation.implementations
import com.vanillasource.eliot.eliotc.jvm.classgen.processor.TypeState.*
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, UnifiedModuleNames, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort
import com.vanillasource.eliot.eliotc.symbolic.fact.{SymbolicType, TypeCheckedValue, Qualifier as SymbolicQualifier}
import com.vanillasource.eliot.eliotc.resolve.fact.AbilityFQN
import com.vanillasource.eliot.eliotc.uncurry.fact.*
import com.vanillasource.eliot.eliotc.uncurry.fact.UncurriedExpression.*
import com.vanillasource.eliot.eliotc.used.UsedNames
import com.vanillasource.eliot.eliotc.used.UsedNames.UsageStats

class JvmClassGenerator extends SingleKeyTypeProcessor[GeneratedModule.Key] with Logging {

  override protected def generateFact(key: GeneratedModule.Key): CompilerIO[Unit] =
    for {
      _                      <- debug[CompilerIO](s"Generating ${key.moduleName.show} (from ${key.vfqn.show}).")
      usedNames              <- getFactOrAbort(UsedNames.Key(key.vfqn))
      usedValues              = usedNames.usedNames.filter((vfqn, _) => vfqn.moduleName === key.moduleName)
      mainClassGenerator     <- createClassGenerator[CompilerIO](key.moduleName)
      // Get all module constructor names for per-data-type processing
      moduleNames            <- getFactOrAbort(UnifiedModuleNames.Key(key.moduleName))
      allCtorNames            = moduleNames.names.keys.toSeq.filter(qn => isConstructor(ValueFQN(key.moduleName, qn)))
      allCtorsWithUv         <- allCtorNames.traverseFilter { qn =>
                                  val vfqn = ValueFQN(key.moduleName, qn)
                                  getFact(UncurriedValue.Key(vfqn, 0)).map(_.map(uv => (vfqn, uv)))
                                }
      allCtorGroups           = allCtorsWithUv.groupBy((_, uv) => simpleType(constructorDataType(uv.returnType)))
      // Get used constructors with actual arity stats
      usedCtorEntries         = usedValues.filter((vfqn, _) => isConstructor(vfqn)).toSeq
      usedCtorsInfo          <- usedCtorEntries.traverseFilter { (vfqn, stats) =>
                                  getFact(UncurriedValue.Key(vfqn, stats.highestArity.getOrElse(0)))
                                    .map(_.map((vfqn, stats, _)))
                                }
      // Find PatternMatch handleCases impls in usedValues and map to their data types
      handleCasesImpls       <- usedValues.toSeq.traverseFilter { (vfqn, _) =>
                                  vfqn.name.qualifier match {
                                    case Qualifier.AbilityImplementation(abilityName, _)
                                        if abilityName.value === "PatternMatch" && vfqn.name.name === "handleCases" =>
                                      getFactOrAbort(TypeCheckedValue.Key(vfqn)).map { checked =>
                                        checked.name.value.qualifier match {
                                          case SymbolicQualifier.AbilityImplementation(abilityFQN, typeArgs) =>
                                            Some((simpleType(typeArgs.head), (abilityFQN, typeArgs)))
                                          case _                                                              =>
                                            None
                                        }
                                      }
                                    case _                                                                      =>
                                      Option.empty.pure[CompilerIO]
                                  }
                                }
      handleCasesByDataType   = handleCasesImpls.toMap
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
      // Process each data type: check handleCases usage, merge constructors, generate classes
      dataResults            <- allCtorGroups.toSeq.traverse { (typeVFQ, allTypeCtors) =>
                                  val handleCasesUsed = handleCasesByDataType.contains(typeVFQ)
                                  val usedFromType    = usedCtorsInfo.filter(c => allTypeCtors.exists(_._1 === c._1))
                                  if (!handleCasesUsed && usedFromType.isEmpty) {
                                    (Seq.empty[ClassFile], Seq.empty[ValueFQN]).pure[CompilerIO]
                                  } else {
                                    // When handleCases is used, include ALL constructors for virtual dispatch
                                    val ctors = if (handleCasesUsed) {
                                                  val usedVfqns = usedFromType.map(_._1).toSet
                                                  usedFromType ++ allTypeCtors
                                                    .filterNot((vfqn, _) => usedVfqns.contains(vfqn))
                                                    .map { (vfqn, uv) =>
                                                      val stats = usedValues.getOrElse(vfqn, UsageStats(Seq.empty, Map(0 -> 1)))
                                                      (vfqn, stats, uv)
                                                    }
                                                } else {
                                                  usedFromType
                                                }
                                    for {
                                      classes          <- if (ctors.size === 1) {
                                                            val (vfqn, _, uv) = ctors.head
                                                            DataClassGenerator.createSingleConstructorData[CompilerIO](
                                                              mainClassGenerator, vfqn, uv, handleCasesUsed
                                                            )
                                                          } else {
                                                            DataClassGenerator.createMultiConstructorData[CompilerIO](
                                                              mainClassGenerator, typeVFQ,
                                                              ctors.map((v, _, uv) => (v, uv)),
                                                              handleCasesUsed
                                                            )
                                                          }
                                      singletonClasses <- if (handleCasesUsed) {
                                                            val (abilityFQN, typeArgs) = handleCasesByDataType(typeVFQ)
                                                            generatePatternMatchSingleton(
                                                              mainClassGenerator, abilityFQN, typeArgs,
                                                              typeVFQ, ctors.size === 1
                                                            ).map(Seq(_))
                                                          } else {
                                                            Seq.empty[ClassFile].pure[CompilerIO]
                                                          }
                                      generatedFunctions = ctors.map(_._1)
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
                                  val vfqn = ValueFQN(key.moduleName, qn)
                                  getFact(UncurriedValue.Key(vfqn, 0)).map(_.map(uv => (vfqn, uv)))
                                }
      // Determine which type constructors need data classes (used directly or via typeMatch)
      neededTypeCtors         = allTypeCtorsArityZero.filter { (vfqn, _) =>
                                  usedValues.contains(vfqn) || allTypeMatchVfqns.exists(_.name.name === "typeMatch")
                                }
      // Get each type constructor at its proper arity (from usage stats or computed from signature)
      usedTypeCtorsWithUv    <- neededTypeCtors.traverse { (vfqn, uvZero) =>
                                  val arity = usedValues.get(vfqn) match {
                                    case Some(stats) => stats.highestArity.getOrElse(constructorArity(uvZero.returnType))
                                    case None        => constructorArity(uvZero.returnType)
                                  }
                                  getFactOrAbort(UncurriedValue.Key(vfqn, arity)).map((vfqn, _))
                                }
      // Generate type constructor data classes and factory methods
      typeCtorClasses        <- usedTypeCtorsWithUv.flatTraverse { (vfqn, uv) =>
                                  DataClassGenerator.createTypeConstructorData[CompilerIO](mainClassGenerator, vfqn, uv)
                                }
      // Generate typeMatch methods for used typeMatch functions
      typeMatchGenerated     <- usedTypeCtorsWithUv.traverseFilter { (vfqn, uv) =>
                                  val typeMatchVfqn = allTypeMatchVfqns.find(_.name.name === "typeMatch")
                                  typeMatchVfqn match {
                                    case Some(tmVfqn) =>
                                      val stats = usedValues(tmVfqn)
                                      for {
                                        tmUv <- getFactOrAbort(
                                                  UncurriedValue.Key(tmVfqn, stats.highestArity.getOrElse(0))
                                                )
                                        _    <- DataClassGenerator.generateTypeMatch[CompilerIO](
                                                  mainClassGenerator, tmUv, vfqn, uv.parameters
                                                )
                                      } yield Some(tmVfqn)
                                    case None         => Option.empty[ValueFQN].pure[CompilerIO]
                                  }
                                }
      // Generate Type marker interface if any type constructors are used
      typeInterfaceClass     <- if (usedTypeCtorsWithUv.nonEmpty) {
                                  DataClassGenerator.generateTypeInterface[CompilerIO]().map(Seq(_))
                                } else Seq.empty[ClassFile].pure[CompilerIO]
      typeCtorGeneratedFns    = usedTypeCtorsWithUv.map(_._1) ++ typeMatchGenerated
      allGeneratedFunctions   = dataGeneratedFunctions ++ typeCtorGeneratedFns ++ allPatternMatchVfqns ++ allTypeMatchVfqns
      abilityInterfaces      <- createAbilityInterfaces(mainClassGenerator, key.moduleName)
      abilityImpls           <- createAbilityImplSingletons(mainClassGenerator, key.moduleName)
      functionFiles          <-
        usedValues.view
          .filterKeys(k => !allGeneratedFunctions.contains(k) && !isAbilityMethod(k))
          .toSeq
          .flatTraverse { case (vfqn, stats) =>
            createModuleMethod(mainClassGenerator, vfqn, stats)
          }
      mainClass              <- mainClassGenerator.generate[CompilerIO]()
      _                      <- registerFactIfClear(
                                  GeneratedModule(
                                    key.moduleName,
                                    key.vfqn,
                                    functionFiles ++ dataClasses ++ typeCtorClasses ++ typeInterfaceClass ++
                                      abilityInterfaces ++ abilityImpls ++ Seq(mainClass)
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
        val arities = stats.directCallApplications.keys.toSeq.sorted
        for {
          result           <- arities.foldLeftM((Seq.empty[ClassFile], 0)) { case ((accFiles, lambdaCount), arity) =>
                                for {
                                  uncurriedValue          <- getFactOrAbort(UncurriedValue.Key(vfqn, arity))
                                  (files, newLambdaCount) <- createModuleMethod(mainClassGenerator, uncurriedValue, lambdaCount)
                                } yield (accFiles ++ files, newLambdaCount)
                              }
          (classFiles, _)   = result
          highestUncurried <- getFactOrAbort(UncurriedValue.Key(vfqn, stats.highestArity.getOrElse(0)))
          _                <- createApplicationMain(vfqn, mainClassGenerator).whenA(isMain(highestUncurried))
        } yield classFiles
    }
  }

  private def createModuleMethod(
      classGenerator: ClassGenerator,
      uncurriedValue: UncurriedValue,
      initialLambdaCount: Int = 0
  ): CompilerIO[(Seq[ClassFile], Int)] = {
    uncurriedValue.body match {
      case Some(body) =>
        for {
          dictParams <- computeDictParams(uncurriedValue.vfqn, uncurriedValue.name)
          result     <-
            classGenerator
              .createMethod[CompilerIO](
                JvmIdentifier.encode(uncurriedValue.vfqn.name.name),
                dictParams.map(p => simpleType(p.parameterType)) ++ uncurriedValue.parameters.map(p =>
                  simpleType(p.parameterType)
                ),
                simpleType(uncurriedValue.returnType)
              )
              .use { methodGenerator =>
                val bodyExpression = UncurriedExpression(uncurriedValue.returnType, body.value)
                val program        = for {
                  _       <- dictParams.traverse_(addParameterDefinition)
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
        } yield result
      case None       =>
        compilerAbort(uncurriedValue.name.as(s"Function not implemented."))
    }
  }

  private def createApplicationMain(mainVfqn: ValueFQN, generator: ClassGenerator): CompilerIO[Unit] =
    generator.createMainMethod[CompilerIO]().use { methodGenerator =>
      methodGenerator.addCallTo(mainVfqn, Seq.empty, systemUnitValue)
    }

  private def isMain(uncurriedValue: UncurriedValue): Boolean =
    uncurriedValue.name.value.name === "main" && uncurriedValue.parameters.isEmpty

  private def isAbilityMethod(valueFQN: ValueFQN): Boolean =
    valueFQN.name.qualifier match {
      case Qualifier.Ability(_) => true
      case _                    => false
    }

  private def generatePatternMatchSingleton(
      mainClassGenerator: ClassGenerator,
      abilityFQN: AbilityFQN,
      typeArgs: Seq[SymbolicType],
      dataTypeVfqn: ValueFQN,
      isSingleConstructor: Boolean
  ): CompilerIO[ClassFile] = {
    val innerClassName = singletonInnerName(abilityFQN, typeArgs)
    val singletonVfqn = ValueFQN(mainClassGenerator.moduleName, QualifiedName(innerClassName, Qualifier.Default))
    val dataTypeName  = convertToNestedClassName(dataTypeVfqn)

    for {
      // Create singleton class (no interface - standalone bridge)
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
      // Generate handleCases bridge: delegate to data type's instance method
      _           <- singletonCg
                       .createPublicInstanceMethod[CompilerIO](
                         JvmIdentifier.encode("handleCases"),
                         Seq(systemAnyValue, systemFunctionValue),
                         systemAnyValue
                       )
                       .use { bridge =>
                         for {
                           // Load value (param index 1, after 'this')
                           _ <- bridge.addLoadVar[CompilerIO](systemAnyValue, 1)
                           // Cast to data type
                           _ <- bridge.addCastTo[CompilerIO](dataTypeVfqn)
                           // Load cases (param index 2)
                           _ <- bridge.addLoadVar[CompilerIO](systemFunctionValue, 2)
                           // Call data type's handleCases(cases) instance method
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

  private def createAbilityInterfaces(
      mainClassGenerator: ClassGenerator,
      moduleName: ModuleName
  ): CompilerIO[Seq[ClassFile]] =
    for {
      unifiedModuleNames <- getFactOrAbort(UnifiedModuleNames.Key(moduleName))
      abilityMethodsByAbility = unifiedModuleNames.names.keys.toSeq.collect {
                                  case qn @ QualifiedName(_, Qualifier.Ability(abilityName)) =>
                                    (abilityName, ValueFQN(moduleName, qn))
                                }.groupMap(_._1)(_._2)
      classFiles         <- abilityMethodsByAbility.toSeq.flatTraverse { (abilityName, methodVfqns) =>
                              for {
                                methods   <- methodVfqns.traverse(vfqn => getFactOrAbort(UncurriedValue.Key(vfqn, 0)))
                                classFile <- AbilityInterfaceGenerator.createAbilityInterface[CompilerIO](
                                               mainClassGenerator, abilityName, methods
                                             )
                              } yield Seq(classFile)
                            }
    } yield classFiles

  private def createAbilityImplSingletons(
      mainClassGenerator: ClassGenerator,
      moduleName: ModuleName
  ): CompilerIO[Seq[ClassFile]] =
    for {
      unifiedModuleNames <- getFactOrAbort(UnifiedModuleNames.Key(moduleName))
      implNames           = unifiedModuleNames.names.keys.toSeq.collect {
                              case qn @ QualifiedName(_, Qualifier.AbilityImplementation(_, _)) =>
                                ValueFQN(moduleName, qn)
                            }
      implsWithAbility   <- implNames.flatTraverse { implVfqn =>
                              getFactOrAbort(TypeCheckedValue.Key(implVfqn)).map { checked =>
                                checked.name.value.qualifier match {
                                  case SymbolicQualifier.AbilityImplementation(abilityFQN, typeArgs) =>
                                    Seq((implVfqn, abilityFQN, typeArgs))
                                  case _                                                             =>
                                    Seq.empty
                                }
                              }
                            }
      grouped             = implsWithAbility
                              .filter { case (_, abilityFQN, _) =>
                                abilityFQN.abilityName =!= "PatternMatch" && abilityFQN.abilityName =!= "TypeMatch"
                              }
                              .groupMap { case (_, abilityFQN, typeArgs) => (abilityFQN, typeArgs) } {
                                case (vfqn, _, _) => vfqn
                              }
      classFiles         <- grouped.toSeq.flatTraverse { case ((abilityFQN, typeArgs), implVfqns) =>
                              for {
                                methodPairs <- implVfqns.traverse { implVfqn =>
                                                val abilityMethodVfqn = ValueFQN(
                                                  abilityFQN.moduleName,
                                                  QualifiedName(implVfqn.name.name, Qualifier.Ability(abilityFQN.abilityName))
                                                )
                                                for {
                                                  abilityMethod <- getFactOrAbort(UncurriedValue.Key(abilityMethodVfqn, 0))
                                                  implMethod    <- getFactOrAbort(UncurriedValue.Key(implVfqn, 0))
                                                } yield (abilityMethod, implMethod)
                                              }
                                classFile   <- AbilityImplGenerator.createAbilityImpl[CompilerIO](
                                                 mainClassGenerator, abilityFQN, typeArgs, methodPairs
                                               )
                              } yield Seq(classFile)
                            }
    } yield classFiles
}
