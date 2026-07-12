package com.vanillasource.eliot.eliotc.jvm.classgen.processor

import cats.data.StateT
import cats.syntax.all.*
import com.vanillasource.eliot.eliotc.core.fact.{Expression => CoreExpression}
import com.vanillasource.eliot.eliotc.module.fact.{QualifiedName, Qualifier}
import com.vanillasource.eliot.eliotc.feedback.Logging
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.{ClassGenerator, JvmIdentifier}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.ClassGenerator.createClassGenerator
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.CommonPatterns.{
  constructorArityValue,
  constructorDataTypeValue,
  mangledMethodName,
  valueType
}
import com.vanillasource.eliot.eliotc.jvm.classgen.asm.NativeType.{convertToNestedClassName, systemAnyValue, systemFunctionValue, systemUnitValue}
import com.vanillasource.eliot.eliotc.jvm.classgen.fact.{ClassFile, GeneratedModule}
import com.vanillasource.eliot.eliotc.jvm.classgen.processor.DataClassGenerator.isConstructor
import com.vanillasource.eliot.eliotc.jvm.classgen.processor.ExpressionCodeGenerator.{createExpressionCode, patternMatchSingletonName}
import com.vanillasource.eliot.eliotc.jvm.classgen.processor.NativeImplementation.implementations
import com.vanillasource.eliot.eliotc.jvm.classgen.processor.TypeState.*
import com.vanillasource.eliot.eliotc.ability.util.ImplementationMarkerUtils
import com.vanillasource.eliot.eliotc.module.fact.{
  ModuleConstructors,
  ModuleName,
  UnifiedModuleValue,
  ValueFQN,
  WellKnownTypes
}
import com.vanillasource.eliot.eliotc.monomorphize.fact.GroundValue
import com.vanillasource.eliot.eliotc.operator.fact.OperatorResolvedValue
import com.vanillasource.eliot.eliotc.processor.CompilerIO.*
import com.vanillasource.eliot.eliotc.processor.common.SingleKeyTypeProcessor
import com.vanillasource.eliot.eliotc.reconcile.fact.ReconciledMonomorphicValue
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
      // Group ALL value constructors by data type, from the shared module constructor index. The JVM keys a data type
      // by its `Qualifier.Default` FQN (its class-naming identity), and each group entry pairs a constructor with that
      // data-type FQN — the shape the constructor-class generation below consumes.
      moduleConstructors     <- getFactOrAbort(ModuleConstructors.Key(key.moduleName))
      allCtorGroups           = moduleConstructors.byDataType.map { (dataType, ctors) =>
                                  val dataTypeVfqn = ValueFQN(key.moduleName, QualifiedName(dataType.name, Qualifier.Default))
                                  dataTypeVfqn -> ctors.map(ctor => (ctor, dataTypeVfqn))
                                }
      // Collect used constructors
      usedCtorVfqns           = usedValues.filter((vfqn, _) => isConstructor(vfqn)).keySet
      // Find PatternMatch handleCases impls and map to their data types
      handleCasesByDataType  <- usedValues.toSeq.traverseFilter { (vfqn, stats) =>
                                  if (WellKnownTypes.isPatternMatchHandleCases(vfqn)) {
                                    ImplementationMarkerUtils
                                      .firstPatternTypeConstructorName(vfqn, WellKnownTypes.patternMatchAbilityName)
                                      .map {
                                        case Some(typeName) =>
                                          val dataTypeVfqn = allCtorGroups.keys
                                            .find(_.name.name === typeName)
                                            .getOrElse(ValueFQN(key.moduleName, QualifiedName(typeName, Qualifier.Default)))
                                          Some((dataTypeVfqn, (vfqn, stats)))
                                        case None           => Option.empty
                                      }
                                  } else {
                                    Option.empty.pure[CompilerIO]
                                  }
                                }
      handleCasesMap          = handleCasesByDataType.toMap
      _                      <- debug[CompilerIO](s"Module ${key.moduleName.show}: handleCasesMap=${handleCasesMap.keys.map(_.show).mkString(", ")}")
      // Collect ALL PatternMatch impl VFQNs to exclude from normal processing
      allPatternMatchVfqns    = usedValues.keys.filter(WellKnownTypes.isPatternMatchImplementation).toSet
      // Collect ALL TypeMatch impl VFQNs to exclude from normal processing
      allTypeMatchVfqns       = usedValues.keys.filter(WellKnownTypes.isTypeMatchImplementation).toSet
      // Map each typeMatch method VFQN to its corresponding type constructor name (via the marker signature)
      typeMatchByConstructor <- allTypeMatchVfqns
                                  .filter(WellKnownTypes.isTypeMatchTypeMatch)
                                  .toSeq
                                  .traverseFilter { tmVfqn =>
                                    ImplementationMarkerUtils
                                      .firstPatternTypeConstructorName(tmVfqn, WellKnownTypes.typeMatchAbilityName)
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
                                      // Fetch UncurriedMonomorphicValue for each constructor. The constructor is emitted
                                      // once (one class + factory per data type), so bare type-parameter fields are erased
                                      // to the opaque `Any` carrier — matching every monomorphic call site, which erases
                                      // the same way (DataClassGenerator.erasePolymorphicFields). A field whose carrier
                                      // still varies across instantiations after erasure is not representable by the single
                                      // shared descriptor; verify they agree and error rather than silently miscompile.
                                      ctorsWithInfo   <- ctorVfqns.traverse { vfqn =>
                                                           val stats        = usedValues.getOrElse(vfqn, UsageStats(Seq(defaultTypeArgs), Map(0 -> 1)))
                                                           val typeArgsList = stats.monomorphicTypeParameters.distinct match {
                                                             case Seq() => Seq(defaultTypeArgs)
                                                             case xs    => xs
                                                           }
                                                           val arity        = stats.highestArity.getOrElse(0)
                                                           for {
                                                             resolved  <- getFactOrAbort(OperatorResolvedValue.Key(vfqn))
                                                             instances <- typeArgsList.traverse(ta =>
                                                                            getFactOrAbort(UncurriedMonomorphicValue.Key(vfqn, ta, arity))
                                                                          )
                                                             erasedAll  = instances.map(umv =>
                                                                            DataClassGenerator.erasePolymorphicFields(resolved, umv.parameters)
                                                                          )
                                                             _         <- compilerAbort[Unit](
                                                                            resolved.name.as(
                                                                              "Constructor has a field whose machine representation differs across instantiations and cannot be erased to a single representation."
                                                                            )
                                                                          ).whenA(erasedAll.map(_.map(p => valueType(p.parameterType))).distinct.size > 1)
                                                           } yield (vfqn, stats, instances.head.copy(parameters = erasedAll.head))
                                                         }
                                      sortedCtors      = ctorsWithInfo.sortBy { case (_, _, umv) =>
                                                           (umv.name.range.from.line, umv.name.range.from.col)
                                                         }
                                      classes          <- if (ctorsWithInfo.size === 1) {
                                                            val (vfqn, _, umv) = ctorsWithInfo.head
                                                            DataClassGenerator.createSingleConstructorData[CompilerIO](
                                                              mainClassGenerator, vfqn, typeVFQ, umv.parameters, handleCasesUsed
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
      // Get all type constructors at arity 0 (for identification and arity computation), from the shared index
      allTypeCtorsArityZero  <- moduleConstructors.typeConstructors.traverseFilter { vfqn =>
                                  val stats    = usedValues.get(vfqn)
                                  val typeArgs = stats.flatMap(_.monomorphicTypeParameters.headOption).getOrElse(Seq.empty)
                                  getFactIfProduced(UncurriedMonomorphicValue.Key(vfqn, typeArgs, 0)).map(_.map(umv => (vfqn, umv)))
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
          // Intrinsics (`+`/`-`/`*`, `intToString`, `nativeWiden`) are emitted inline at the call site by
          // ExpressionCodeGenerator, so they have no generated method — skip them rather than hitting the body-less
          // "Function not implemented." abort. (Integer literals are not here: `integerLiteral` is rewritten to a plain
          // literal node in `PostDrainQuoter`, so it never reaches codegen as a value at all.)
          .filterKeys(k => !allGeneratedFunctions.contains(k) && !Intrinsics.isIntrinsic(k))
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

  /** Fail-safe enforcement of the I/O boundary: an impure leaf native (e.g. `printLineInternal`) must be declared
    * `private` so no application module can name it and perform untracked I/O. The compiler cannot detect a native's
    * impurity from its bytecode, so the registry is the source of truth and this asserts the resolved def's visibility
    * matches — a forgotten `private` is caught at build time, never a silent pure-typed-impure hole. Pure natives are
    * unconstrained and skip the fact lookup entirely.
    */
  private def verifyNativeVisibility(vfqn: ValueFQN, native: NativeImplementation): CompilerIO[Unit] =
    if (!native.impure) ().pure[CompilerIO]
    else
      getFactOrAbort(UnifiedModuleValue.Key(vfqn)).flatMap { umv =>
        NativeImplementation.visibilityViolation(vfqn, native.impure, umv.namedValue.visibility) match {
          case Some(message) => compilerAbort[Unit](umv.namedValue.qualifiedName.as(message))
          case None          => ().pure[CompilerIO]
        }
      }

  private def createModuleMethod(
      mainClassGenerator: ClassGenerator,
      vfqn: ValueFQN,
      stats: UsageStats
  ): CompilerIO[Seq[ClassFile]] = {
    implementations.get(vfqn) match {
      case Some(nativeImplementation) =>
        verifyNativeVisibility(vfqn, nativeImplementation) >>
          nativeImplementation.generateMethod(mainClassGenerator).as(Seq.empty)
      case None                       =>
        val distinctTypeArgs = stats.monomorphicTypeParameters.distinct
        val arities          = stats.directCallApplications.keys.toSeq.sorted
        for {
          // Distinct ranges that lower to the same representation (e.g. `Int[0,3]` and `Int[0,5]` -> `Byte`) yield the
          // SAME method name + descriptor with byte-identical bodies; generate each such signature only once. Different
          // representations give different descriptors and so legitimately overload the same mangled name.
          result <- distinctTypeArgs.foldLeftM((Seq.empty[ClassFile], 0, Set.empty[String])) {
                      case ((accFiles, lambdaCount, seen), typeArgs) =>
                        arities.foldLeftM((accFiles, lambdaCount, seen)) {
                          case ((innerAccFiles, innerLambdaCount, innerSeen), arity) =>
                            for {
                              uncurriedValue <- getFactOrAbort(UncurriedMonomorphicValue.Key(vfqn, typeArgs, arity))
                              key             = methodSignatureKey(uncurriedValue, typeArgs)
                              next           <-
                                if (innerSeen.contains(key))
                                  (innerAccFiles, innerLambdaCount, innerSeen).pure[CompilerIO]
                                else
                                  createModuleMethod(mainClassGenerator, uncurriedValue, typeArgs, innerLambdaCount)
                                    .map { case (files, newLambdaCount) =>
                                      (innerAccFiles ++ files, newLambdaCount, innerSeen + key)
                                    }
                            } yield next
                        }
                    }
          (classFiles, _, _) = result
          // Check if this is the main function
          highestTypeArgs    = distinctTypeArgs.headOption.getOrElse(Seq.empty)
          highestUncurried  <- getFactOrAbort(
                                 UncurriedMonomorphicValue.Key(vfqn, highestTypeArgs, stats.highestArity.getOrElse(0))
                               )
          _                 <- createApplicationMain(vfqn, mainClassGenerator).whenA(isMain(highestUncurried))
        } yield classFiles
    }
  }

  /** A stable key for a generated method's emitted signature: the mangled name plus the lowered parameter and return
    * representations (i.e. its JVM descriptor). Two monomorphic instantiations sharing this key are byte-identical and
    * must be generated once; instantiations with the same name but a different key are valid overloads.
    */
  private def methodSignatureKey(uncurriedValue: UncurriedMonomorphicValue, typeArgs: Seq[GroundValue]): String = {
    val name       = mangledMethodName(uncurriedValue.vfqn, typeArgs)
    val paramTypes = uncurriedValue.parameters.map(p => valueType(p.parameterType).show).mkString(",")
    s"$name($paramTypes):${valueType(uncurriedValue.returnType).show}"
  }

  private def createModuleMethod(
      classGenerator: ClassGenerator,
      uncurriedValue: UncurriedMonomorphicValue,
      typeArgs: Seq[GroundValue],
      initialLambdaCount: Int = 0
  ): CompilerIO[(Seq[ClassFile], Int)] =
    // The body is generated from the *reconciled* form (per-node refinement meta + explicit `Reconcile` re-encode
    // nodes, `docs/generic-refinement-merges.md`); the value-level structure (descriptors from `parameters`/
    // `returnType`, the method name) stays from the uncurried value. Every uncurried value has a reconciled counterpart
    // (the reconcile pass is a transformation over `UncurriedMonomorphicValue`), so this fact is always producible.
    getFactOrAbort(
      ReconciledMonomorphicValue.Key(uncurriedValue.vfqn, typeArgs, uncurriedValue.arity)
    ).flatMap { reconciledValue =>
      reconciledValue.body match {
        case Some(body) =>
          val methodName = mangledMethodName(uncurriedValue.vfqn, typeArgs)
          classGenerator
            .createMethod[CompilerIO](
              JvmIdentifier.encode(methodName),
              uncurriedValue.parameters.map(p => valueType(p.parameterType)),
              valueType(uncurriedValue.returnType)
            )
            .use { methodGenerator =>
              val bodyExpression = body.value
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

            // The closure-class prefix must be the *mangled* method name, not the bare local name: two
            // implementations of one ability share the local name (`wrap`) but get distinct mangled names
            // (`wrap$Wrap$impl$0…`/`wrap$Wrap$impl$1…`). Using the bare name here collided their lambda classes
            // (`Test$wrap$lambda$1` twice → a duplicate-entry ZipException at JAR time) whenever both were
            // co-located in one module.
            program
              .run(TypeState(methodName = methodName, lambdaCount = initialLambdaCount))
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

}
