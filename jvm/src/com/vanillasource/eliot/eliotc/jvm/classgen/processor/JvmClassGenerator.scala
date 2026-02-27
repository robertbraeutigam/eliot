package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.eval.fact.ExpressionValue
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.{ClassGenerator, JvmIdentifier}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.ClassGenerator.createClassGenerator
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.CommonPatterns.simpleType
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType.systemUnitValue
import com.vanillasource.eliot.eliotc.jvm.classgen.fact.{ClassFile, GeneratedModule}
import com.vanillasource.eliot.eliotc.jvm.classgen.processor.DataClassGenerator.{constructorDataType, isConstructor}
import com.vanillasource.eliot.eliotc.jvm.classgen.processor.ExpressionCodeGenerator.{computeDictParams, createExpressionCode}
import com.vanillasource.eliot.eliotc.jvm.classgen.processor.NativeImplementation.implementations
import com.vanillasource.eliot.eliotc.jvm.classgen.processor.TypeState.*
import com.vanillasource.eliot.eliotc.module.fact.{ModuleName, UnifiedModuleNames, ValueFQN}
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.source.content.Sourced.compilerAbort
import com.vanillasource.eliot.eliotc.symbolic.fact.{TypeCheckedValue, Qualifier as SymbolicQualifier}
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
      // Process each data type: check per-type eliminator usage, merge constructors, generate classes
      dataResults            <- allCtorGroups.toSeq.traverse { (typeVFQ, allTypeCtors) =>
                                  val typeName       = typeVFQ.name.name
                                  val eliminatorName = s"handle${typeName}With"
                                  val eliminatorVfqn = ValueFQN(key.moduleName, QualifiedName(eliminatorName, Qualifier.Default))
                                  val eliminatorUsed = usedValues.contains(eliminatorVfqn)
                                  val usedFromType   = usedCtorsInfo.filter(c => allTypeCtors.exists(_._1 === c._1))
                                  if (!eliminatorUsed && usedFromType.isEmpty) {
                                    (Seq.empty[ClassFile], Seq.empty[ValueFQN]).pure[CompilerIO]
                                  } else {
                                    for {
                                      eliminatorUncurried <- if (eliminatorUsed) {
                                                               val stats = usedValues(eliminatorVfqn)
                                                               getFact(UncurriedValue.Key(eliminatorVfqn, stats.highestArity.getOrElse(0)))
                                                             } else {
                                                               Option.empty[UncurriedValue].pure[CompilerIO]
                                                             }
                                      // When eliminator is used, include ALL constructors for virtual dispatch
                                      ctors                = if (eliminatorUsed) {
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
                                      classes             <- if (ctors.size === 1) {
                                                               val (vfqn, _, uv) = ctors.head
                                                               DataClassGenerator.createSingleConstructorData[CompilerIO](
                                                                 mainClassGenerator, vfqn, uv, eliminatorUncurried, eliminatorName
                                                               )
                                                             } else {
                                                               DataClassGenerator.createMultiConstructorData[CompilerIO](
                                                                 mainClassGenerator, typeVFQ,
                                                                 ctors.map((v, _, uv) => (v, uv)),
                                                                 eliminatorUncurried, eliminatorName
                                                               )
                                                             }
                                      generatedFunctions   = {
                                                               val ctorFunctions = ctors.map(_._1)
                                                               ctorFunctions ++ (if (eliminatorUsed) Seq(eliminatorVfqn) else Seq.empty)
                                                             }
                                    } yield (classes, generatedFunctions)
                                  }
                                }
      dataClasses             = dataResults.flatMap(_._1)
      dataGeneratedFunctions  = dataResults.flatMap(_._2)
      abilityInterfaces      <- createAbilityInterfaces(mainClassGenerator, key.moduleName)
      abilityImpls           <- createAbilityImplSingletons(mainClassGenerator, key.moduleName)
      functionFiles          <-
        usedValues.view
          .filterKeys(k => !dataGeneratedFunctions.contains(k) && !isAbilityMethod(k))
          .toSeq
          .flatTraverse { case (vfqn, stats) =>
            createModuleMethod(mainClassGenerator, vfqn, stats)
          }
      mainClass              <- mainClassGenerator.generate[CompilerIO]()
      _                      <- registerFactIfClear(
                                  GeneratedModule(
                                    key.moduleName,
                                    key.vfqn,
                                    functionFiles ++ dataClasses ++ abilityInterfaces ++ abilityImpls ++ Seq(mainClass)
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
        for {
          uncurriedValue <- getFactOrAbort(UncurriedValue.Key(vfqn, stats.highestArity.getOrElse(0)))
          classFiles     <- createModuleMethod(mainClassGenerator, uncurriedValue)
          _              <- createApplicationMain(vfqn, mainClassGenerator).whenA(isMain(uncurriedValue))
        } yield classFiles
    }
  }

  private def createModuleMethod(
      classGenerator: ClassGenerator,
      uncurriedValue: UncurriedValue
  ): CompilerIO[Seq[ClassFile]] = {
    uncurriedValue.body match {
      case Some(body) =>
        for {
          dictParams <- computeDictParams(uncurriedValue.vfqn, uncurriedValue.name)
          classFiles <-
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
                } yield classes

                program.runA(TypeState(methodName = uncurriedValue.vfqn.name.name))
              }
        } yield classFiles
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
